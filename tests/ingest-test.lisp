(in-package #:demiurge/tests)

(defun %write-corpus (dir files)
  (ensure-directories-exist dir)
  (dolist (pair files)
    (destructuring-bind (name text) pair
      (with-open-file (out (merge-pathnames name dir)
                           :direction :output
                           :if-exists :supersede)
        (write-string text out))))
  dir)

(defun %chunk-ids (store)
  (mapcar #'rag:rag-chunk-id (list-stored-chunks store)))

(deftest as-string-list-coerces-json-vectors
  "Enumerate journals a vector of hashes; JSON decode also yields a vector."
  (ok (equal '("a" "b") (demiurge/ingest::%as-string-list #("a" "b"))))
  (ok (equal '("a") (demiurge/ingest::%as-string-list "a")))
  (ok (equal '("h1") (demiurge/ingest::%as-string-list '((:hash "h1"))))))

(deftest file-source-enumerates-stable-hashes
  (with-tmp-dir (tmp)
    (let* ((dir (%write-corpus (merge-pathnames "c/" tmp)
                               '(("a.md" "# A~%alpha")
                                 ("b.md" "# B~%beta"))))
           (source (make-file-source :root dir :pattern "*.md" :recursive t))
           (items (enumerate-items source)))
      (ok (= 2 (length items)))
      (ok (every #'ingest-item-hash items))
      (ok (every (lambda (it) (eq :md (ingest-item-format it))) items)
          "Unix path must not be interned as a format keyword")
      (ok (equal (mapcar #'ingest-item-hash items)
                 (mapcar #'ingest-item-hash (enumerate-items source)))))))

(deftest s3-source-enumerates-from-object-store
  (let* ((store (obj:make-in-memory-object-store))
         (bytes (map '(vector (unsigned-byte 8)) #'char-code "hello s3")))
    (obj:put-object store "docs/one.md" bytes)
    (let* ((source (make-s3-source :store store :bucket "demo" :prefix "docs/"))
           (items (enumerate-items source)))
      (ok (= 1 (length items)))
      (ok (equal (content-hash bytes) (ingest-item-hash (first items))))
      (ok (search "hello s3" (ingest-item-content (first items))))
      (ok (getf (ingest-item-metadata (first items)) :etag)
          "head-object etag is recorded as a change signal"))))

(deftest s3-source-pages-list-objects
  (let* ((store (obj:make-in-memory-object-store))
         (a (map '(vector (unsigned-byte 8)) #'char-code "aaa"))
         (b (map '(vector (unsigned-byte 8)) #'char-code "bbb")))
    (obj:put-object store "docs/a.md" a)
    (obj:put-object store "docs/b.md" b)
    (let* ((source (make-s3-source :store store :bucket "demo"
                                   :prefix "docs/" :page-size 1))
           (items (enumerate-items source)))
      (ok (= 2 (length items)))
      (ok (equal (sort (mapcar #'ingest-item-id items) #'string<)
                 '("docs/a.md" "docs/b.md"))))))

(deftest imap-source-enumerates-scripted-mailbox
  (let* ((msg (mail:print-message
               (mail:make-message :from "a@ex.com" :to "b@ex.com"
                                  :subject "Hi" :body "imap body")))
         (client (mail:make-imap-client
                  :io-fn
                  (lambda (cmd)
                    (cond
                      ((null cmd) '("* OK IMAP4rev1 ready"))
                      ((search "LOGIN" cmd) '("A0001 OK LOGIN completed"))
                      ((search "SELECT" cmd)
                       '("* 1 EXISTS" "A0002 OK SELECT completed"))
                      ((search "SEARCH" cmd)
                       '("* SEARCH 1" "A0003 OK SEARCH completed"))
                      ((search "FETCH" cmd)
                       (list (format nil "* 1 FETCH (RFC822 {~d}~%~a)"
                                     (length msg) msg)
                             "A0004 OK FETCH completed"))
                      (t '("A9999 BAD unknown"))))))
         (source (make-imap-source :client client :mailbox "INBOX")))
    (mail:imap-connect client)
    (mail:imap-login client "alice" "secret")
    (let ((items (enumerate-items source)))
      (ok (plusp (length items)))
      (ok (search "imap body" (ingest-item-content (first items))))
      (ok (ingest-item-hash (first items)))
      (ok (search "+" (ingest-item-id (first items)))
          "stable id is Message-ID+UID")
      (ok (getf (ingest-item-metadata (first items)) :uid)))))

(deftest ingest-kill-and-resume-no-duplicates
  (with-tmp-dir (tmp)
    (let* ((dir (%write-corpus (merge-pathnames "corpus/" tmp)
                               '(("a.md" "# A~%alpha chunk")
                                 ("b.md" "# B~%beta chunk")
                                 ("c.md" "# C~%gamma chunk"))))
           (source (make-file-source :root dir :pattern "*.md"))
           (items (enumerate-items source))
           (journal (task:make-in-memory-journal))
           (store (rag:make-mock-vector-store))
           (domain (make-expert-domain :name "ingest-resume"))
           (seen 0)
           (hook (lambda (plist)
                   (declare (ignore plist))
                   (incf seen)
                   (when (= seen 2)
                     (error "killed mid-corpus")))))
      (ok (= 3 (length items)))
      (let ((*ingest-item-hook* hook))
        (handler-case
            (run-ingest domain source
                        :store store
                        :journal journal
                        :task-id "ingest-resume"
                        :embedder (mock-llm))
          (error ())))
      (ok (= 2 seen))
      (let ((ids-after-kill (%chunk-ids store)))
        (ok (plusp (length ids-after-kill)))
        (let ((*ingest-item-hook*
               (lambda (plist)
                 (declare (ignore plist))
                 (incf seen))))
          (let ((result (run-ingest domain source
                                    :store store
                                    :journal journal
                                    :task-id "ingest-resume"
                                    :embedder (mock-llm))))
            (ok (getf result :hashes))
            (ok (= 3 (length (getf result :hashes))))
            (ok (= (length (getf result :chunk-ids))
                   (length (list-stored-chunks store)))
                "resume result keeps chunk ids from replayed steps")
            (let ((ids (%chunk-ids store))
                  (hashes (stored-content-hashes store)))
              (ok (= (length ids) (length (remove-duplicates ids :test #'equal)))
                  "no duplicate chunk ids")
              (ok (every (lambda (item)
                           (member (ingest-item-hash item) hashes :test #'equal))
                         items)
                  "all source hashes present")
              (ok (= (length hashes) 3)))))))))

(deftest ingest-mark-and-sweep-deletes-missing
  (with-tmp-dir (tmp)
    (let* ((dir (%write-corpus (merge-pathnames "sweep/" tmp)
                               '(("keep.md" "# Keep~%stay")
                                 ("gone.md" "# Gone~%drop"))))
           (source (make-file-source :root dir :pattern "*.md"))
           (store (rag:make-mock-vector-store))
           (domain (make-expert-domain :name "ingest-sweep")))
      (run-ingest domain source
                  :store store
                  :journal (task:make-in-memory-journal)
                  :task-id "sweep-1"
                  :embedder (mock-llm))
      (ok (= 2 (length (stored-content-hashes store))))
      (uiop:delete-file-if-exists (merge-pathnames "gone.md" dir))
      (run-ingest domain source
                  :store store
                  :journal (task:make-in-memory-journal)
                  :task-id "sweep-2"
                  :embedder (mock-llm))
      (ok (= 1 (length (stored-content-hashes store))))
      (ok (equal (stored-content-hashes store)
                 (mapcar #'ingest-item-hash (enumerate-items source))))
      (let* ((journal (task:make-in-memory-journal))
             (task-id "sweep-3"))
        (run-ingest domain source
                    :store store
                    :journal journal
                    :task-id task-id
                    :embedder (mock-llm))
        (let* ((task (task:make-durable-task :id task-id :journal journal))
               (events (task:journal-events journal task))
               (sweep (find-if (lambda (ev)
                                 (and (typep ev 'task:step-completed)
                                      (equal "sweep" (task:step-name ev))))
                               events))
               (report (and sweep (task:step-result sweep))))
          (ok sweep)
          (ok (eq :sweep-completed (getf report :event)))
          (ok (numberp (getf report :stale)))
          (ok (numberp (getf report :stored)))
          (ok (numberp (getf report :enumerated))))))))

(defun %chunk-meta (store key)
  (mapcar (lambda (ch) (getf (rag:rag-chunk-metadata ch) key))
          (list-stored-chunks store)))

(defun %write-octets (path bytes)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (write-sequence bytes out))
  path)

(defclass %h4-failing-embedder (llm:llm-backend) ())

(defmethod llm:embed ((backend %h4-failing-embedder) inputs
                      &key &allow-other-keys)
  (declare (ignore inputs))
  (error "h4 embedder failed"))

(defclass %h4-failing-store (rag:rag-vector-store) ())

(defmethod rag:upsert ((store %h4-failing-store) chunks)
  (declare (ignore chunks))
  (error "h4 store failed"))

(defclass %h4-spy-pdf-extractor (doc:doc-extract-backend)
  ((seen :initform nil :accessor %h4-spy-seen)))

(defmethod doc:extract-document ((backend %h4-spy-pdf-extractor) source
                                 &key format)
  (declare (ignore format))
  (setf (%h4-spy-seen backend) source)
  (let ((doc (doc:make-extracted-document
              :blocks (list (doc:make-text-block :text "pdf-ok")))))
    (doc:ensure-ids doc)
    doc))

(defun %extracted (text)
  (let ((doc (doc:make-extracted-document
              :blocks (list (doc:make-text-block :text text)))))
    (doc:ensure-ids doc)
    doc))

(deftest ingest-two-sources-do-not-interfere
  "Sweep of source B must not delete source A's chunks in a shared store."
  (with-tmp-dir (tmp)
    (let* ((dir-a (%write-corpus (merge-pathnames "a/" tmp)
                                 '(("keep-a.md" "# A~%alpha"))))
           (dir-b (%write-corpus (merge-pathnames "b/" tmp)
                                 '(("keep-b.md" "# B~%beta")
                                   ("gone-b.md" "# G~%drop"))))
           (src-a (make-file-source :root dir-a :pattern "*.md"))
           (src-b (make-file-source :root dir-b :pattern "*.md"))
           (store (rag:make-mock-vector-store))
           (domain (make-expert-domain :name "shared-src"))
           (emb (mock-llm)))
      (run-ingest domain src-a
                  :store store
                  :journal (task:make-in-memory-journal)
                  :task-id "src-a-1"
                  :embedder emb)
      (run-ingest domain src-b
                  :store store
                  :journal (task:make-in-memory-journal)
                  :task-id "src-b-1"
                  :embedder emb)
      (ok (= 3 (length (stored-content-hashes store))))
      (ok (equal (sort (remove-duplicates (%chunk-meta store :source)
                                          :test #'equal)
                       #'string<)
                 (sort (list (ingest-source-id src-a) (ingest-source-id src-b))
                       #'string<))
          "chunks record distinct source ownership")
      (uiop:delete-file-if-exists (merge-pathnames "gone-b.md" dir-b))
      (run-ingest domain src-b
                  :store store
                  :journal (task:make-in-memory-journal)
                  :task-id "src-b-2"
                  :embedder emb)
      (let ((hashes (stored-content-hashes store))
            (hash-a (ingest-item-hash (first (enumerate-items src-a))))
            (hash-b (ingest-item-hash (first (enumerate-items src-b)))))
        (ok (= 2 (length hashes)))
        (ok (member hash-a hashes :test #'equal)
            "source A survived source B sweep")
        (ok (member hash-b hashes :test #'equal))))))

(deftest ingest-two-domains-do-not-interfere
  "Sweep of domain A must not delete domain B's chunks in a shared store."
  (with-tmp-dir (tmp)
    (let* ((dir-a (%write-corpus (merge-pathnames "da/" tmp)
                                 '(("keep-a.md" "# A~%alpha")
                                   ("gone-a.md" "# G~%drop"))))
           (dir-b (%write-corpus (merge-pathnames "db/" tmp)
                                 '(("keep-b.md" "# B~%beta"))))
           (src-a (make-file-source :root dir-a :pattern "*.md"))
           (src-b (make-file-source :root dir-b :pattern "*.md"))
           (store (rag:make-mock-vector-store))
           (dom-a (make-expert-domain :name "domain-a"))
           (dom-b (make-expert-domain :name "domain-b"))
           (emb (mock-llm)))
      (run-ingest dom-a src-a
                  :store store
                  :journal (task:make-in-memory-journal)
                  :task-id "dom-a-1"
                  :embedder emb)
      (run-ingest dom-b src-b
                  :store store
                  :journal (task:make-in-memory-journal)
                  :task-id "dom-b-1"
                  :embedder emb)
      (ok (= 3 (length (stored-content-hashes store))))
      (ok (equal (sort (remove-duplicates (%chunk-meta store :domain)
                                          :test #'equal)
                       #'string<)
                 '("domain-a" "domain-b"))
          "chunks record distinct domain ownership")
      (uiop:delete-file-if-exists (merge-pathnames "gone-a.md" dir-a))
      (run-ingest dom-a src-a
                  :store store
                  :journal (task:make-in-memory-journal)
                  :task-id "dom-a-2"
                  :embedder emb)
      (let ((hashes (stored-content-hashes store))
            (hash-b (ingest-item-hash (first (enumerate-items src-b)))))
        (ok (= 2 (length hashes)))
        (ok (member hash-b hashes :test #'equal)
            "domain B survived domain A sweep")
        (ok (every (lambda (ch)
                     (member (getf (rag:rag-chunk-metadata ch) :domain)
                             '("domain-a" "domain-b")
                             :test #'equal))
                   (list-stored-chunks store)))))))

(defun %live-domain-without-store (name)
  "A real deployment profile with no rag-store — not the keyword :personal."
  (make-expert-domain :name name
                      :profile (make-instance 'personal-profile)))

(deftest ingest-requires-store-outside-mock-profile
  (with-tmp-dir (tmp)
    (let* ((dir (%write-corpus (merge-pathnames "c/" tmp)
                               '(("a.md" "# A~%alpha"))))
           (source (make-file-source :root dir :pattern "*.md"))
           (domain (%live-domain-without-store "no-store"))
           (*ingest-profile* :live))
      (ok (signals (run-ingest domain source
                               :journal (task:make-in-memory-journal)
                               :task-id "no-store"
                               :embedder (mock-llm))
                   'ingest-store-required)))))

(deftest ingest-store-required-use-value
  (with-tmp-dir (tmp)
    (let* ((dir (%write-corpus (merge-pathnames "c/" tmp)
                               '(("a.md" "# A~%alpha"))))
           (source (make-file-source :root dir :pattern "*.md"))
           (domain (%live-domain-without-store "store-uv"))
           (store (rag:make-mock-vector-store))
           (*ingest-profile* :live)
           (got (handler-bind ((ingest-store-required
                                (lambda (c)
                                  (use-value store c))))
                  (run-ingest domain source
                              :journal (task:make-in-memory-journal)
                              :task-id "store-uv"
                              :embedder (mock-llm)))))
      (ok (getf got :hashes))
      (ok (= 1 (length (stored-content-hashes store)))))))

(deftest ingest-extractor-error-no-plain-fallback
  (let* ((item (make-ingest-item :id "x.bin" :uri "x.bin"
                                 :content "not-a-pdf"
                                 :hash "deadbeef"
                                 :format :no-such-h4-fmt))
         (store (rag:make-mock-vector-store)))
    (ok (signals (ingest-one-item item
                                  :store store
                                  :embedder (mock-llm))
                 'ingest-extractor-error))
    (ok (null (list-stored-chunks store))
        "failed extract must not upsert a plain-text fallback")))

(deftest ingest-extractor-use-value
  (let* ((item (make-ingest-item :id "x.bin" :uri "x.bin"
                                 :content "not-a-pdf"
                                 :hash "uv-extract"
                                 :format :no-such-h4-fmt))
         (store (rag:make-mock-vector-store))
         (got (handler-bind ((ingest-extractor-error
                              (lambda (c)
                                (use-value (%extracted "recovered") c))))
                (ingest-one-item item
                                 :store store
                                 :embedder (mock-llm)
                                 :domain (make-expert-domain :name "uv")
                                 :source (make-file-source :root "/tmp/uv")))))
    (ok (equal "uv-extract" (getf got :hash)))
    (ok (plusp (length (getf got :chunk-ids))))))

(deftest ingest-embedder-error-outside-mock
  (with-tmp-dir (tmp)
    (let* ((dir (%write-corpus (merge-pathnames "c/" tmp)
                               '(("a.md" "# A~%alpha"))))
           (source (make-file-source :root dir :pattern "*.md"))
           (domain (make-expert-domain :name "emb-fail"))
           (store (rag:make-mock-vector-store))
           (journal (task:make-in-memory-journal))
           (*ingest-profile* :live))
      (ok (signals (run-ingest domain source
                               :store store
                               :journal journal
                               :task-id "emb-fail"
                               :embedder (make-instance '%h4-failing-embedder))
                   'ingest-embedder-error))
      (ok (null (list-stored-chunks store)))
      (let* ((task (task:make-durable-task :id "emb-fail" :journal journal))
             (events (task:journal-events journal task))
             (item-done (find-if (lambda (ev)
                                   (and (typep ev 'task:step-completed)
                                        (search "ingest-item/"
                                                (task:step-name ev))))
                                 events))
             (done (find-if (lambda (ev) (typep ev 'task:task-completed))
                            events)))
        (ok (null item-done) "item step is not journaled on embed failure")
        (ok (null done) "task is not complete on embed failure")))))

(deftest ingest-zero-embeddings-only-in-mock-profile
  (with-tmp-dir (tmp)
    (let* ((dir (%write-corpus (merge-pathnames "c/" tmp)
                               '(("a.md" "# A~%alpha"))))
           (source (make-file-source :root dir :pattern "*.md"))
           (domain (make-expert-domain :name "emb-mock"))
           (store (rag:make-mock-vector-store))
           (*ingest-profile* :mock))
      (run-ingest domain source
                  :store store
                  :journal (task:make-in-memory-journal)
                  :task-id "emb-mock"
                  :embedder (make-instance '%h4-failing-embedder))
      (ok (plusp (length (list-stored-chunks store))))
      (ok (every (lambda (ch)
                   (every #'zerop (rag:rag-chunk-embedding ch)))
                 (list-stored-chunks store))
          "zero vectors are allowed only in the named :mock profile"))))

(deftest ingest-store-error-no-continue
  (with-tmp-dir (tmp)
    (let* ((dir (%write-corpus (merge-pathnames "c/" tmp)
                               '(("a.md" "# A~%alpha"))))
           (source (make-file-source :root dir :pattern "*.md"))
           (domain (make-expert-domain :name "store-fail"))
           (store (rag:make-mock-vector-store :dimension 3))
           (*ingest-profile* :live))
      (ok (signals (run-ingest domain source
                               :store store
                               :journal (task:make-in-memory-journal)
                               :task-id "store-fail"
                               :embedder (mock-llm))
                   'ingest-store-error))
      (ok (null (list-stored-chunks store))
          "dimension mismatch is not auto-continued"))))

(deftest ingest-store-error-retry
  (with-tmp-dir (tmp)
    (let* ((dir (%write-corpus (merge-pathnames "c/" tmp)
                               '(("a.md" "# A~%alpha"))))
           (source (make-file-source :root dir :pattern "*.md"))
           (domain (make-expert-domain :name "store-retry"))
           (store (make-instance '%h4-failing-store))
           (tries 0)
           (retry-name (intern "RETRY" :demiurge/ingest))
           (got (handler-bind ((ingest-store-error
                                (lambda (c)
                                  (incf tries)
                                  (if (= tries 1)
                                      (invoke-restart
                                       (find-restart retry-name c))
                                      (use-value t c)))))
                  (run-ingest domain source
                              :store store
                              :journal (task:make-in-memory-journal)
                              :task-id "store-retry"
                              :embedder (mock-llm)))))
      (ok (>= tries 2))
      (ok (getf got :hashes)))))

(deftest ingest-binary-passes-path-or-octets
  (with-tmp-dir (tmp)
    (let* ((dir (ensure-directories-exist (merge-pathnames "bin/" tmp)))
           (path (%write-octets (merge-pathnames "doc.pdf" dir)
                                #(37 80 68 70 45 49 46 52)))
           (spy (make-instance '%h4-spy-pdf-extractor))
           (doc:*extractors* (list (list :backend spy
                                         :formats '(:pdf)
                                         :priority 100)))
           (source (make-file-source :root dir :pattern "*.pdf"))
           (items (enumerate-items source)))
      (ok (= 1 (length items)))
      (ok (not (stringp (ingest-item-content (first items))))
          "binary file-source must not text-decode content")
      (let ((store (rag:make-mock-vector-store)))
        (ingest-one-item (first items)
                         :store store
                         :embedder (mock-llm)
                         :domain (make-expert-domain :name "bin")
                         :source source)
        (let ((seen (%h4-spy-seen spy)))
          (ok (or (pathnamep seen)
                  (and (vectorp seen) (not (stringp seen))))
              "extractor receives a path or octet vector")
          (ok (not (stringp seen))
              "extractor must not receive decoded text")
          (when (pathnamep seen)
            (ok (equal (pathname-type seen) "pdf")))
          (ok (probe-file path)))))))

(deftest ingest-replay-aggregates-chunk-ids
  (with-tmp-dir (tmp)
    (let* ((dir (%write-corpus (merge-pathnames "corpus/" tmp)
                               '(("a.md" "# A~%alpha chunk")
                                 ("b.md" "# B~%beta chunk")
                                 ("c.md" "# C~%gamma chunk"))))
           (source (make-file-source :root dir :pattern "*.md"))
           (journal (task:make-in-memory-journal))
           (store (rag:make-mock-vector-store))
           (domain (make-expert-domain :name "replay-ids"))
           (seen 0)
           (hook (lambda (plist)
                   (declare (ignore plist))
                   (incf seen)
                   (when (= seen 2)
                     (error "killed mid-corpus")))))
      (let ((*ingest-item-hook* hook))
        (handler-case
            (run-ingest domain source
                        :store store
                        :journal journal
                        :task-id "replay-ids"
                        :embedder (mock-llm))
          (error ())))
      (let ((*ingest-item-hook*
             (lambda (plist)
               (declare (ignore plist))
               (incf seen))))
        (let ((result (run-ingest domain source
                                  :store store
                                  :journal journal
                                  :task-id "replay-ids"
                                  :embedder (mock-llm))))
          (ok (= (length (getf result :chunk-ids))
                 (length (list-stored-chunks store)))
              "replay keeps prior chunk ids")
          (ok (= 3 (length (getf result :hashes)))))))))
