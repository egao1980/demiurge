(in-package #:demiurge/tests)

(defun %bundle-profile (&key skill-store journal rag-store)
  (make-instance 'personal-profile
                 :kind :personal
                 :skill-store skill-store
                 :journal journal
                 :rag-store rag-store))

(defun %bundle-domain (name llm &key corpus eval-suites skill-store journal
                       rag-store)
  (let ((domain (make-echo-expert :backend llm :name name)))
    (when corpus
      (setf (expert-corpora domain) (list corpus)))
    (when eval-suites
      (setf (expert-eval-suites domain) eval-suites))
    (setf (expert-profile domain)
          (%bundle-profile :skill-store skill-store
                           :journal journal
                           :rag-store rag-store))
    domain))

(defun %working-skill (store-root name)
  (steer:load-skill (merge-pathnames
                     (format nil "~a/SKILL.md" name)
                     (uiop:ensure-directory-pathname store-root))))

(deftest packed-manifest-blob-is-readable
  "schema:dump :as :plist embeds hash-tables; the OCI blob must PRINT/READ."
  (let* ((m (make-expert-bundle-manifest
             :name "round"
             :version "1.2.3"
             :skill-refs (list (make-bundle-skill-ref
                                :name "s" :version "1" :digest "abc"))
             :corpus-sources
             (list (make-bundle-corpus-source
                    :items (list (make-bundle-corpus-item
                                  :uri "a.md" :digest "def"))))))
         (text (demiurge/bundle::%manifest-text m))
         (parsed (demiurge/bundle::%parse-manifest-plist
                  (demiurge/bundle::%read-sexp text))))
    (ok (not (search "#<" text))
        "manifest text must not contain unreadable #<HASH-TABLE>")
    (ok (equal "round" (expert-bundle-manifest-name parsed)))
    (ok (equal "1.2.3" (expert-bundle-manifest-version parsed)))
    (ok (equal "1" (bundle-skill-ref-version
                    (first (expert-bundle-manifest-skill-refs parsed))))
        "nested skill-ref keys must not be swapped by hash-table conversion")
    (ok (equal "def" (bundle-corpus-item-digest
                      (first (bundle-corpus-source-items
                              (first (expert-bundle-manifest-corpus-sources
                                      parsed)))))))))

(deftest pack-install-round-trip
  "pack→install against a local OCI layout; installed echo-expert is runnable."
  (with-clean-registry
    (clear-bundle-installs)
    (with-tmp-dir (tmp)
      (let* ((corpus (%write-corpus (merge-pathnames "corpus/" tmp)
                                    '(("a.md" "# A~%alpha chunk"))))
             (skills (merge-pathnames "skills/" tmp))
             (store (steer:make-file-skill-store skills))
             (journal (task:make-in-memory-journal))
             (rag (rag:make-mock-vector-store))
             (llm (mock-llm))
             (ds (eval:make-eval-dataset
                  :name "bundle-smoke"
                  :cases (list (eval:make-eval-case
                                :input "hi" :expected "echo: hi"))))
             (domain (%bundle-domain "bundle-echo" llm
                                     :corpus corpus
                                     :eval-suites (list ds)
                                     :skill-store store
                                     :journal journal
                                     :rag-store rag)))
        (steer:save-skill-version
         store (steer:make-steer-skill "bundle-echo" :body "echo skill v1"))
        (let* ((packed (pack-expert domain
                                    :registry (merge-pathnames "oci/" tmp)
                                    :version "1.0.0"))
               (layout (uiop:ensure-directory-pathname (getf packed :layout))))
          (ok (probe-file (merge-pathnames "oci-layout" layout)))
          (ok (probe-file (merge-pathnames "index.json" layout)))
          (ok (uiop:directory-files
               (merge-pathnames "blobs/sha256/" layout)))
          (ok (expert-bundle-manifest-p (getf packed :manifest)))
          (ok (expert-bundle-manifest-p
               (demiurge/bundle::%load-manifest-from-layout layout))
              "OCI manifest blob must PRINT/READ after pack")
          (clear-expert-registry)
          (let ((result (install-expert packed
                                        :journal journal
                                        :task-id "bundle-round-trip"
                                        :store rag
                                        :skill-store store
                                        :llm llm)))
            (ok (equal "bundle-echo" (getf result :name)))
            (let ((installed (find-expert "bundle-echo")))
              (ok (expert-domain-p installed))
              (let ((board (run-expert installed :trigger '(:prompt "hi"))))
                (ok (equal "echo: hi" (bb:read-section board :result)))))))))))

(deftest tampered-blob-signals-verification-error
  "Tampered blob → bundle-verification-error; skip-verification is absent."
  (with-clean-registry
    (clear-bundle-installs)
    (with-tmp-dir (tmp)
      (let* ((llm (mock-llm))
             (journal (task:make-in-memory-journal))
             (domain (%bundle-domain "bundle-tamper" llm :journal journal))
             (packed (pack-expert domain
                                  :registry (merge-pathnames "oci/" tmp)
                                  :version "1.0.0"))
             (layout (uiop:ensure-directory-pathname (getf packed :layout)))
             (blobs (uiop:directory-files
                     (merge-pathnames "blobs/sha256/" layout)))
             (victim (first blobs))
             (signaled nil)
             (skip-restart nil))
        (ok victim)
        (with-open-file (out victim :direction :output :if-exists :supersede
                             :element-type '(unsigned-byte 8))
          (write-sequence (map '(vector (unsigned-byte 8)) #'char-code
                               "TAMPERED-BUNDLE-BLOB")
                          out))
        (ok (signals (install-expert packed
                                     :journal (task:make-in-memory-journal)
                                     :task-id "bundle-tamper-a"
                                     :llm llm)
                     'bundle-verification-error))
        (handler-case
            (handler-bind
                ((bundle-verification-error
                  (lambda (c)
                    (setf signaled t)
                    (setf skip-restart (find-restart 'skip-verification c)))))
              (install-expert packed
                              :journal (task:make-in-memory-journal)
                              :task-id "bundle-tamper-b"
                              :llm llm))
          (bundle-verification-error ()))
        (ok signaled)
        (ok (null skip-restart)
            "skip-verification must not be offered")
        (ok (null (find-expert "bundle-tamper"))
            "install refuses to register on hash mismatch")))))

(deftest rollback-restores-prior-skill-versions
  "rollback-expert restores the A4 file-skill-store to the pinned version."
  (with-clean-registry
    (clear-bundle-installs)
    (with-tmp-dir (tmp)
      (let* ((skills (merge-pathnames "skills/" tmp))
             (store (steer:make-file-skill-store skills))
             (journal (task:make-in-memory-journal))
             (llm (mock-llm))
             (registry (merge-pathnames "oci/" tmp)))
        (flet ((pack-version (body version)
                 (steer:save-skill-version
                  store (steer:make-steer-skill "bundle-rb" :body body))
                 (pack-expert (%bundle-domain "bundle-rb" llm
                                              :skill-store store
                                              :journal journal)
                              :registry registry
                              :version version)))
          (let ((p1 (pack-version "skill body v1" "1.0.0"))
                (p2 (pack-version "skill body v2" "1.0.1")))
            (install-expert p1 :journal journal :task-id "bundle-rb-1"
                            :skill-store store :llm llm)
            (install-expert p2 :journal journal :task-id "bundle-rb-2"
                            :skill-store store :llm llm)
            (ok (search "v2" (steer:steer-directive-body
                              (%working-skill skills "bundle-rb"))))
            (ok (steer:skill-versions store "bundle-rb"))
            (rollback-expert "bundle-rb" "1.0.0"
                             :journal journal
                             :task-id "bundle-rb-roll"
                             :skill-store store
                             :llm llm)
            (ok (search "v1" (steer:steer-directive-body
                              (%working-skill skills "bundle-rb"))))
            (ok (expert-domain-p (find-expert "bundle-rb")))))))))

(deftest install-kill-and-resume-mid-ingest
  "Kill mid-corpus during install; resume replays unique ingest-item/<hash> steps."
  (with-clean-registry
    (clear-bundle-installs)
    (with-tmp-dir (tmp)
      (let* ((corpus (%write-corpus (merge-pathnames "corpus/" tmp)
                                    '(("a.md" "# A~%alpha chunk")
                                      ("b.md" "# B~%beta chunk")
                                      ("c.md" "# C~%gamma chunk"))))
             (journal (task:make-in-memory-journal))
             (rag (rag:make-mock-vector-store))
             (llm (mock-llm))
             (domain (%bundle-domain "bundle-resume" llm
                                     :corpus corpus
                                     :journal journal
                                     :rag-store rag))
             (packed (pack-expert domain
                                  :registry (merge-pathnames "oci/" tmp)
                                  :version "1.0.0"))
             (seen 0)
             (hook (lambda (plist)
                     (declare (ignore plist))
                     (incf seen)
                     (when (= seen 2)
                       (error "killed mid-corpus")))))
        (let ((*ingest-item-hook* hook))
          (handler-case
              (install-expert packed
                              :journal journal
                              :task-id "bundle-resume"
                              :store rag
                              :llm llm)
            (error ())))
        (ok (= 2 seen))
        (let ((ids-after-kill (%chunk-ids rag)))
          (ok (plusp (length ids-after-kill)))
          (let ((*ingest-item-hook*
                 (lambda (plist)
                   (declare (ignore plist))
                   (incf seen))))
            (let ((result (install-expert packed
                                          :journal journal
                                          :task-id "bundle-resume"
                                          :store rag
                                          :llm llm)))
              (ok (equal "bundle-resume" (getf result :name)))
              (let ((ids (%chunk-ids rag))
                    (hashes (stored-content-hashes rag)))
                (ok (= (length ids) (length (remove-duplicates ids :test #'equal)))
                    "no duplicate chunk ids")
                (ok (= (length hashes) 3)
                    "all source hashes present after resume")
                (ok (expert-domain-p (find-expert "bundle-resume")))))))))))
