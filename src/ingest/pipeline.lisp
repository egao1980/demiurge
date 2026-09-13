(in-package #:demiurge/ingest)

(defvar *ingest-item-hook* nil
  "Optional (lambda (item-plist)) invoked at the start of a live ingest step.")

(defun %journal-for (domain journal)
  (or journal
      (let ((prof (and (expert-domain-p domain) (expert-profile domain))))
        (and (deployment-profile-p prof) (profile-journal prof)))
      (task:make-in-memory-journal)))

(defun %embedder-for (domain embedder)
  (or embedder
      (let ((prof (and (expert-domain-p domain) (expert-profile domain))))
        (when (deployment-profile-p prof)
          (let ((cat (profile-llm-catalog prof))
                (model (profile-default-model prof)))
            (when cat
              (ignore-errors (llm:resolve-backend cat model))))))
      (llm:make-mock-llm-backend)))

(defun %rag-store-for (domain store)
  (or store
      (let ((prof (and (expert-domain-p domain) (expert-profile domain))))
        (and (deployment-profile-p prof) (profile-rag-store prof)))
      (rag:make-mock-vector-store)))

(defun %object-store-for (store)
  (or store obj:*object-store*))

(defun %store-chunk-table (store)
  "Slot name CHUNKS is interned in the store's home package, not this one."
  (or (let ((acc (find-symbol "MOCK-STORE-TABLE" :rag-protocol)))
        (when (and acc (fboundp acc))
          (ignore-errors (funcall acc store))))
      (loop for pkg-name in '("RAG-PROTOCOL" "RAG-BACKEND-MEMORY" "RAG-BACKEND-SQL")
            for pkg = (find-package pkg-name)
            for slot = (and pkg (find-symbol "CHUNKS" pkg))
            when (and slot (slot-exists-p store slot))
              return (slot-value store slot))))

(defun list-stored-chunks (store)
  (let ((table (%store-chunk-table store)))
    (if (hash-table-p table)
        (loop for ch being the hash-values of table collect ch)
        nil)))

(defun stored-content-hashes (store)
  (let ((seen (make-hash-table :test 'equal)))
    (dolist (ch (list-stored-chunks store))
      (let ((h (or (getf (rag:rag-chunk-metadata ch) :content-hash)
                   (rag:rag-chunk-document-id ch))))
        (when (and h (stringp h))
          (setf (gethash h seen) t))))
    (loop for k being the hash-keys of seen collect k)))

(defun %text-format-p (fmt)
  (member fmt '(:txt :text :md :markdown :rst :plain nil) :test #'eq))

(defun %extract-backend (fmt)
  (or (ignore-errors (doc:find-extractor fmt))
      (when (%text-format-p fmt)
        (make-instance 'plain-text-extractor))
      (make-instance 'plain-text-extractor)))

(defun %extract (item)
  (let* ((fmt (or (ingest-item-format item) :txt))
         (fmt (if (and (or (stringp fmt) (pathnamep fmt))
                       (or (find #\/ (if (pathnamep fmt) (namestring fmt) fmt))
                           (find #\\ (if (pathnamep fmt) (namestring fmt) fmt))))
                  (%infer-format fmt)
                  (or (ignore-errors (doc:canonicalize-format fmt)) fmt)))
         (source (or (ingest-item-content item)
                     (ingest-item-uri item)))
         (backend (%extract-backend fmt)))
    (restart-case
        (handler-bind ((doc:no-extractor-for-format
                        (lambda (c)
                          (let ((r (find-restart 'doc:use-backend c)))
                            (when r
                              (invoke-restart r
                                              (make-instance
                                               'plain-text-extractor)))))))
          (doc:extract-document backend source :format fmt))
      (use-value (value)
        :report "Use a supplied extracted-document"
        value)
      (skip ()
        :report "Skip this item"
        nil))))

(defun %zero-embedding (&optional (dim 8))
  (make-array dim :element-type 'single-float :initial-element 0.0f0))

(defun %embed-and-upsert (store embedder chunks)
  (when chunks
    (let ((embs (handler-case
                    (let* ((texts (mapcar #'rag:rag-chunk-text chunks))
                           (result (llm:embed embedder texts :dimensions 8)))
                      (llm:llm-embed-result-embeddings result))
                  (error () nil))))
      (loop for ch in chunks
            for i from 0
            for emb = (and embs (nth i embs))
            for raw = (cond
                        ((null emb) nil)
                        ((vectorp emb) emb)
                        (t (ignore-errors (llm:llm-embedding-vector emb))))
            do (setf (rag:rag-chunk-embedding ch)
                     (let ((v (%zero-embedding 8)))
                       (when (and raw (arrayp raw))
                         (loop for j from 0 below (min 8 (length raw))
                               do (setf (aref v j)
                                        (float (aref raw j) 0.0f0))))
                       v)))
      (handler-bind ((error
                      (lambda (c)
                        (let ((r (find-restart 'continue c)))
                          (when r (invoke-restart r))))))
        (rag:upsert store chunks))))
  chunks)

(defun %plain-chunks (item)
  (let ((hash (ingest-item-hash item))
        (text (or (ingest-item-content item) "")))
    (list (rag:make-rag-chunk
           :id hash
           :document-id hash
           :text text
           :metadata (list :content-hash hash
                           :uri (ingest-item-uri item))))))

(defun ingest-one-item (item &key store embedder object-store)
  (when *ingest-item-hook*
    (funcall *ingest-item-hook* (item-plist item)))
  (let* ((hash (ingest-item-hash item))
         (doc (handler-case (%extract item) (error () nil)))
         (chunks (handler-case
                     (when doc
                       (rag.text:chunk-extracted-document
                        (rag.text:make-block-tree-chunker :store object-store)
                        doc
                        :document-id hash
                        :base-metadata (list :content-hash hash
                                             :uri (ingest-item-uri item))
                        :store object-store))
                   (error () nil))))
    (unless chunks
      (setf chunks (%plain-chunks item)))
    (%embed-and-upsert store embedder chunks)
    (list :hash hash
          :chunk-ids (mapcar #'rag:rag-chunk-id chunks))))

(defun sweep-deleted-items (store live-hashes)
  "Mark-and-sweep: drop chunks whose content-hash is not in LIVE-HASHES."
  (let* ((live (make-hash-table :test 'equal))
         (stale '()))
    (dolist (h live-hashes)
      (when h (setf (gethash h live) t)))
    (dolist (ch (list-stored-chunks store))
      (let ((h (or (getf (rag:rag-chunk-metadata ch) :content-hash)
                   (rag:rag-chunk-document-id ch))))
        (unless (gethash h live)
          (push (rag:rag-chunk-id ch) stale))))
    (when stale
      (handler-bind ((rag:rag-not-found
                      (lambda (c)
                        (let ((r (find-restart 'continue c)))
                          (when r (invoke-restart r))))))
        (rag:delete-ids store (nreverse stale))))
    stale))

(defun run-ingest (domain source &key store journal task-id embedder
                                   object-store)
  "Durable ingest. Per-item step idempotency key = content hash.
   Sweep deletes stored hashes that the source no longer enumerates."
  (check-type domain expert-domain)
  (let* ((journal (%journal-for domain journal))
         (task (task:make-durable-task
                :id (or task-id
                        (format nil "ingest/~a" (expert-name domain)))
                :journal journal))
         (store (%rag-store-for domain store))
         (embedder (%embedder-for domain embedder))
         (object-store (%object-store-for object-store))
         (result nil))
    (task:with-durable-task (task journal)
      (let* ((plists (task:with-durable-step
                         ("enumerate" :idempotency-key "ingest/enumerate")
                       (mapcar #'item-plist (enumerate-items source))))
             (chunk-ids '())
             (hashes (mapcar (lambda (p) (getf p :hash)) plists)))
        (dolist (plist plists)
          (let* ((hash (getf plist :hash))
                 (one (task:with-durable-step
                          ("ingest-item" :idempotency-key hash)
                        (let ((got (ingest-one-item (item-from-plist plist)
                                                    :store store
                                                    :embedder embedder
                                                    :object-store object-store)))
                          (list :hash (and hash (princ-to-string hash))
                                :chunk-ids (mapcar (lambda (id)
                                                     (if (stringp id)
                                                         id
                                                         (princ-to-string id)))
                                                   (or (getf got :chunk-ids)
                                                       '())))))))
            (when one
              (setf chunk-ids (append chunk-ids (getf one :chunk-ids))))))
        (let ((swept (task:with-durable-step
                         ("sweep" :idempotency-key "ingest/sweep")
                       (sweep-deleted-items store hashes))))
          (setf result (list :hashes hashes
                             :chunk-ids chunk-ids
                             :swept swept))
          (task:complete-task task result))))
    result))
