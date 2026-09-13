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
      (ok (search "hello s3" (ingest-item-content (first items)))))))

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
      (ok (ingest-item-hash (first items))))))

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
                 (mapcar #'ingest-item-hash (enumerate-items source)))))))
