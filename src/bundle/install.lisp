(in-package #:demiurge/bundle)

(defun %install-key (name)
  (string-downcase (string name)))

(defun record-bundle-install (name version manifest layout)
  (let ((rec (list :name (%install-key name)
                   :version (string version)
                   :manifest (schema:dump manifest :as :plist)
                   :layout (namestring (uiop:ensure-directory-pathname layout))
                   :skill-refs (mapcar (lambda (r)
                                         (list :name (bundle-skill-ref-name r)
                                               :version (bundle-skill-ref-version r)
                                               :digest (bundle-skill-ref-digest r)))
                                       (expert-bundle-manifest-skill-refs
                                        manifest)))))
    (push rec (gethash (%install-key name) *bundle-installs*))
    rec))

(defun find-bundle-install (name version)
  (find (string version)
        (gethash (%install-key name) *bundle-installs*)
        :key (lambda (r) (getf r :version))
        :test #'equal))

(defun %layout-from-ref (ref)
  (cond
    ((and (consp ref) (keywordp (first ref)))
     (uiop:ensure-directory-pathname
      (or (getf ref :layout)
          (and (getf ref :registry)
               (%layout-root (getf ref :registry)
                             (or (getf ref :name) "")
                             (or (getf ref :version) "0.1.0")))
          (error 'bundle-error :message "ref plist needs :layout or :registry"))))
    ((pathnamep ref) (uiop:ensure-directory-pathname ref))
    ((stringp ref) (uiop:ensure-directory-pathname ref))
    (t (error 'bundle-error
              :message (format nil "cannot coerce bundle ref ~s" ref)))))

(defun %blob-files (layout)
  (let ((dir (merge-pathnames "blobs/sha256/"
                              (uiop:ensure-directory-pathname layout))))
    (when (uiop:directory-exists-p dir)
      (uiop:directory-files dir))))

(defun %signal-mismatch (expected actual &optional path message)
  "Hash mismatch. skip-verification is deliberately not offered."
  (error 'bundle-verification-error
         :expected expected
         :actual actual
         :path path
         :message message))

(defun %parse-manifest-plist (plist)
  (schema:parse 'expert-bundle-manifest
                (if (and (consp plist) (keywordp (first plist)))
                    plist
                    (if (and (consp plist) (consp (first plist)))
                        plist
                        plist))))

(defun %try-manifest-from-octets (octets)
  (handler-case
      (let ((sexp (%read-sexp (%octets-string octets))))
        (when (and (consp sexp) (keywordp (first sexp))
                   (or (getf sexp :name) (getf sexp :catalogue-vocab)
                       (getf sexp :ks-definitions) (getf sexp :skill-refs)))
          (%parse-manifest-plist sexp)))
    (error () nil)))

(defun %load-manifest-from-layout (layout)
  (or (loop for path in (%blob-files layout)
            for manifest = (%try-manifest-from-octets (%read-octets path))
            when manifest return manifest)
      (error 'bundle-error
             :message (format nil "no expert-bundle-manifest in ~s" layout))))

(defun %blob-by-digest (layout digest)
  (let ((path (%blob-path layout digest)))
    (unless (probe-file path)
      (error 'bundle-error
             :message (format nil "missing blob sha256:~a" digest)))
    (%read-octets path)))

(defun verify-bundle-layout (layout &optional manifest)
  "Verify every blob filename against portable-digest and every content hash
   recorded on MANIFEST. Signals BUNDLE-VERIFICATION-ERROR on mismatch.
   No skip-verification restart is established."
  (let ((layout (uiop:ensure-directory-pathname layout)))
    (unless (probe-file (merge-pathnames "oci-layout" layout))
      (error 'bundle-error :message "missing oci-layout"))
    (unless (probe-file (merge-pathnames "index.json" layout))
      (error 'bundle-error :message "missing index.json"))
    (dolist (path (%blob-files layout))
      (let* ((expected (pathname-name path))
             (actual (%content-digest (%read-octets path))))
        (unless (equal expected actual)
          (%signal-mismatch expected actual path "tampered blob"))))
    (let ((manifest (or manifest (%load-manifest-from-layout layout))))
      (flet ((check (digest title)
               (when (and digest (plusp (length digest)))
                 (let* ((octets (%blob-by-digest layout digest))
                        (actual (%content-digest octets)))
                   (unless (equal digest actual)
                     (%signal-mismatch digest actual title
                                       (format nil "content hash ~a" title)))))))
        (dolist (ref (expert-bundle-manifest-skill-refs manifest))
          (check (bundle-skill-ref-digest ref)
                 (format nil "skill/~a" (bundle-skill-ref-name ref))))
        (dolist (ref (expert-bundle-manifest-eval-datasets manifest))
          (check (bundle-eval-dataset-ref-digest ref)
                 (format nil "dataset/~a" (bundle-eval-dataset-ref-name ref))))
        (dolist (src (expert-bundle-manifest-corpus-sources manifest))
          (dolist (item (bundle-corpus-source-items src))
            (check (bundle-corpus-item-digest item)
                   (format nil "corpus/~a" (bundle-corpus-item-uri item))))))
      manifest)))

(defun %journal-for (profile journal)
  (or journal
      (and (deployment-profile-p profile) (profile-journal profile))
      (task:make-in-memory-journal)))

(defun %skill-store-for (profile skill-store)
  (or skill-store
      (and (deployment-profile-p profile) (profile-skill-store profile))))

(defun %rag-store-for (profile store)
  (or store
      (and (deployment-profile-p profile) (profile-rag-store profile))
      (rag:make-mock-vector-store)))

(defun %llm-for (profile llm)
  (or llm
      (let ((cat (and (deployment-profile-p profile)
                      (profile-llm-catalog profile)))
            (model (and (deployment-profile-p profile)
                        (profile-default-model profile))))
        (when cat
          (ignore-errors (llm:resolve-backend cat model))))
      (llm:make-mock-llm-backend)))

(defun %keywordize (name)
  (intern (string-upcase (string name)) :keyword))

(defun %ks-from-definition (def llm)
  (let* ((name (intern (string-upcase (bundle-ks-definition-name def))
                       :demiurge))
         (watch (let ((w (%read-sexp (bundle-ks-definition-watch def)
                                     '(:prompt))))
                  (if (listp w) w (list w))))
         (agent (agent:make-ai-agent
                 :name (bundle-ks-definition-name def)
                 :backend llm
                 :instructions (or (bundle-ks-definition-instructions def)
                                   "Echo the user."))))
    (make-agent-ks :name name
                   :agent agent
                   :watch watch
                   :prompt-key (%keywordize
                                (bundle-ks-definition-prompt-key def))
                   :result-key (%keywordize
                                (bundle-ks-definition-result-key def)))))

(defun %catalogue-from-vocab (vocab)
  (let ((sexp (%read-sexp vocab '(:world))))
    (cap:make-catalogue
     (cond
       ((keywordp sexp) sexp)
       ((and (consp sexp) (keywordp (first sexp))) (first sexp))
       (t :world)))))

(defun %datasets-from-manifest (manifest)
  (loop for ref in (expert-bundle-manifest-eval-datasets manifest)
        for payload = (bundle-eval-dataset-ref-payload ref)
        collect (if (and payload (plusp (length payload)))
                    (eval:load-dataset payload :format :sexp)
                    (eval:make-eval-dataset
                     :name (bundle-eval-dataset-ref-name ref)
                     :cases nil))))

(defun %install-skill (store ref text)
  (when (and store text)
    (multiple-value-bind (fm body)
        (steer:parse-skill-markdown text)
      (let ((skill (steer:make-steer-skill
                    (or (getf fm :name) (bundle-skill-ref-name ref))
                    :description (getf fm :description)
                    :body (or body text))))
        (steer:save-skill-version store skill)))))

(defun %write-corpus-snapshot (layout manifest dest)
  "Materialize corpus blobs under DEST. → list of snapshot directories."
  (let ((dirs '()))
    (dolist (src (expert-bundle-manifest-corpus-sources manifest))
      (let ((root (ensure-directories-exist
                   (merge-pathnames
                    (format nil "~a/" (or (bundle-corpus-source-kind src)
                                          "file"))
                    (uiop:ensure-directory-pathname dest)))))
        (dolist (item (bundle-corpus-source-items src))
          (let* ((digest (bundle-corpus-item-digest item))
                 (octets (%blob-by-digest layout digest))
                 (uri (bundle-corpus-item-uri item))
                 (base (file-namestring (pathname uri)))
                 (path (merge-pathnames
                        (if (and base (plusp (length base)))
                            base
                            (format nil "~a.txt" digest))
                        root)))
            (ensure-directories-exist path)
            (%write-octets path octets)))
        (push root dirs)))
    (nreverse dirs)))

(defun %domain-from-manifest (manifest &key profile llm eval-suites corpora
                                         steering)
  (let* ((llm (or llm (%llm-for profile nil)))
         (defs (expert-bundle-manifest-ks-definitions manifest))
         (ks-set (if defs
                     (mapcar (lambda (d) (%ks-from-definition d llm)) defs)
                     (expert-ks-set
                      (make-echo-expert
                       :backend llm
                       :name (expert-bundle-manifest-name manifest))))))
    (make-expert-domain
     :name (expert-bundle-manifest-name manifest)
     :catalogue (%catalogue-from-vocab
                 (expert-bundle-manifest-catalogue-vocab manifest))
     :ks-set ks-set
     :steering steering
     :corpora corpora
     :eval-suites (or eval-suites (%datasets-from-manifest manifest))
     :profile (or profile :personal))))

(defun %ingest-sources (domain dirs &key store journal task-id embedder)
  "Run demiurge/ingest:run-ingest per snapshot dir. Unique ingest task ids."
  (let ((results '()))
    (loop for dir in dirs
          for i from 0
          for source = (ingest:make-file-source :root dir :pattern "*"
                                                :recursive t)
          for id = (format nil "~a/dir-~d" (or task-id "ingest") i)
          do (push (ingest:run-ingest domain source
                                      :store store
                                      :journal journal
                                      :task-id id
                                      :embedder embedder)
                   results))
    (nreverse results)))

(defgeneric install-expert (ref &key profile journal task-id store
                                  skill-store llm embedder)
  (:documentation
   "Durable task: pull local OCI layout, verify every content hash, register
    the domain, then ingest corpus sources. No skip-verification restart."))

(defmethod install-expert (ref &key profile journal task-id store
                           skill-store llm embedder)
  (let* ((layout (%layout-from-ref ref))
         (journal (%journal-for profile journal))
         (task (task:make-durable-task
                :id (or task-id
                        (format nil "bundle/install/~a"
                                (file-namestring
                                 (uiop:ensure-directory-pathname layout))))
                :journal journal))
         (skill-store (%skill-store-for profile skill-store))
         (rag-store (%rag-store-for profile store))
         (llm (%llm-for profile llm))
         (embedder (or embedder llm))
         (result nil))
    (task:with-durable-task (task journal)
      (let* ((pulled (task:with-durable-step
                         ("pull" :idempotency-key "bundle/pull")
                       (unless (probe-file (merge-pathnames "oci-layout" layout))
                         (error 'bundle-error
                                :message (format nil "no oci-layout at ~s"
                                                 layout)))
                       (namestring layout)))
             (manifest (task:with-durable-step
                           ("verify" :idempotency-key "bundle/verify")
                         (let ((m (verify-bundle-layout pulled)))
                           (schema:dump m :as :plist))))
             (manifest (if (expert-bundle-manifest-p manifest)
                           manifest
                           (%parse-manifest-plist manifest)))
             (name (expert-bundle-manifest-name manifest))
             (version (expert-bundle-manifest-version manifest)))
        (task:with-durable-step
            ("skills" :idempotency-key "bundle/skills")
          (dolist (ref (expert-bundle-manifest-skill-refs manifest))
            (let* ((digest (bundle-skill-ref-digest ref))
                   (text (%octets-string (%blob-by-digest layout digest))))
              (%install-skill skill-store ref text)))
          t)
        (let ((domain
               (let ((registered
                      (task:with-durable-step
                          ("register" :idempotency-key "bundle/register")
                        (expert-name
                         (register-expert
                          (%domain-from-manifest
                           manifest :profile profile :llm llm
                           :steering skill-store))))))
                 (or (find-expert registered)
                     (register-expert
                      (%domain-from-manifest
                       manifest :profile profile :llm llm
                       :steering skill-store))))))
          (let* ((snap-root (merge-pathnames
                             (format nil "bundle-snap-~a-~a/" name version)
                             (uiop:temporary-directory)))
                 (dirs (%write-corpus-snapshot layout manifest snap-root)))
            (when dirs
              (setf (expert-corpora domain) dirs)
              ;; Child durable ingest: unique task-id, unique per-item steps
              ;; inside run-ingest (ingest-item/<hash>). Do not wrap the
              ;; whole ingest in one step named "ingest-item".
              (%ingest-sources domain dirs
                               :store rag-store
                               :journal journal
                               :task-id (format nil "ingest/~a/~a" name version)
                               :embedder embedder))
            (let ((rec (task:with-durable-step
                           ("record" :idempotency-key "bundle/record")
                         (record-bundle-install name version manifest layout))))
              (setf result (list :name name
                                 :version version
                                 :layout (namestring layout)
                                 :domain (expert-name domain)
                                 :record rec))
              (task:complete-task task result))))))
    result))

(defun %rollback-skills (store rec manifest)
  (let ((refs (or (and rec (getf rec :skill-refs))
                  (mapcar (lambda (r)
                            (list :name (bundle-skill-ref-name r)
                                  :version (bundle-skill-ref-version r)))
                          (expert-bundle-manifest-skill-refs manifest)))))
    (dolist (ref refs)
      (let ((name (getf ref :name))
            (version (getf ref :version)))
        (when (and store name version (plusp (length (string version))))
          (handler-case
              (steer:rollback-skill store name version)
            (error (c)
              (error 'bundle-error
                     :message (format nil "skill rollback ~s ~s failed: ~a"
                                      name version c)
                     :cause c))))))))

(defgeneric rollback-expert (name version &key profile journal task-id
                                       skill-store llm)
  (:documentation
   "Journaled rollback: re-register the prior manifest and rollback-skill
    on the A4 / steer-protocol file skill store."))

(defmethod rollback-expert (name version &key profile journal task-id
                            skill-store llm)
  (let* ((journal (%journal-for profile journal))
         (task (task:make-durable-task
                :id (or task-id
                        (format nil "bundle/rollback/~a/~a"
                                (%install-key name) version))
                :journal journal))
         (skill-store (%skill-store-for profile skill-store))
         (result nil))
    (task:with-durable-task (task journal)
      (let* ((rec (or (find-bundle-install name version)
                      (error 'bundle-error
                             :message (format nil "no installed bundle ~s ~s"
                                              name version))))
             (manifest (task:with-durable-step
                           ("load-prior" :idempotency-key "bundle/rollback/load")
                         (getf rec :manifest)))
             (manifest (if (expert-bundle-manifest-p manifest)
                           manifest
                           (%parse-manifest-plist manifest))))
        (task:with-durable-step
            ("rollback-skills" :idempotency-key "bundle/rollback/skills")
          (%rollback-skills skill-store rec manifest)
          t)
        (let ((domain
               (register-expert
                (%domain-from-manifest manifest
                                       :profile profile
                                       :llm (%llm-for profile llm)
                                       :steering skill-store))))
          (setf result (list :name (expert-name domain)
                             :version (expert-bundle-manifest-version manifest)
                             :domain (expert-name domain)))
          (task:complete-task task result))))
    result))
