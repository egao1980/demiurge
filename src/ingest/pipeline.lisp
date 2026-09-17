(in-package #:demiurge/ingest)

(defvar *ingest-item-hook* nil
  "Optional (lambda (item-plist)) invoked at the start of a live ingest step.")

(defvar *ingest-profile* :live
  "Ingest execution profile.
   :LIVE requires a configured store and real embeddings.
   :MOCK may allocate an ephemeral mock store and fill zero-vector
   embeddings when the embedder fails.")

(defun mock-ingest-profile-p ()
  (let ((p *ingest-profile*))
    (or (eq p :mock)
        (and (stringp p) (string-equal p "mock")))))

(defun %journal-for (domain journal)
  (or journal
      (let ((prof (and (expert-domain-p domain) (expert-profile domain))))
        (and (deployment-profile-p prof) (profile-journal prof)))
      (task:make-in-memory-journal)))

(defun %embedder-for (domain embedder)
  (or (resolve-profile-llm (and (expert-domain-p domain) (expert-profile domain))
                          embedder)
      (when (mock-ingest-profile-p)
        (llm:make-mock-llm-backend))
      (tagbody
       :retry
         (restart-case
             (error 'ingest-embedder-error
                    :retryable t
                    :message "no embedder configured")
           (retry ()
             :report "Retry resolving the embedder"
             (go :retry))
           (use-value (value)
             :report "Use a supplied embedder"
             :interactive (lambda ()
                            (format *query-io* "Embedder: ")
                            (force-output *query-io*)
                            (list (read *query-io*)))
             (return-from %embedder-for value))))))

(defun %rag-store-for (domain store)
  (or store
      (let ((prof (and (expert-domain-p domain) (expert-profile domain))))
        (and (deployment-profile-p prof) (profile-rag-store prof)))
      (when (mock-ingest-profile-p)
        (rag:make-mock-vector-store))
      (tagbody
       :retry
         (restart-case
             (error 'ingest-store-required
                    :domain domain
                    :message "ingest requires a configured rag store")
           (retry ()
             :report "Retry resolving the rag store"
             (go :retry))
           (use-value (value)
             :report "Use a supplied rag store"
             :interactive (lambda ()
                            (format *query-io* "Store: ")
                            (force-output *query-io*)
                            (list (read *query-io*)))
             (return-from %rag-store-for value))))))

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

(defun %domain-name (domain)
  (cond
    ((null domain) nil)
    ((expert-domain-p domain) (expert-name domain))
    (t (string-downcase (string domain)))))

(defun %scope-source-id (source)
  (cond
    ((null source) nil)
    ((ingest-source-p source) (ingest-source-id source))
    (t (princ-to-string source))))

(defun %chunk-in-scope-p (chunk domain-name source-id)
  (if (and domain-name source-id)
      (let ((meta (rag:rag-chunk-metadata chunk)))
        (and (equal (getf meta :domain) domain-name)
             (equal (getf meta :source) source-id)))
      t))

(defun stored-content-hashes (store &key domain source)
  (let ((seen (make-hash-table :test 'equal))
        (domain-name (%domain-name domain))
        (source-id (%scope-source-id source)))
    (dolist (ch (list-stored-chunks store))
      (when (%chunk-in-scope-p ch domain-name source-id)
        (let ((h (or (getf (rag:rag-chunk-metadata ch) :content-hash)
                     (rag:rag-chunk-document-id ch))))
          (when (and h (stringp h))
            (setf (gethash h seen) t)))))
    (loop for k being the hash-keys of seen collect k)))

(defun %item-format (item)
  (let ((fmt (or (ingest-item-format item) :txt)))
    (if (and (or (stringp fmt) (pathnamep fmt))
             (or (find #\/ (if (pathnamep fmt) (namestring fmt) fmt))
                 (find #\\ (if (pathnamep fmt) (namestring fmt) fmt))))
        (%infer-format fmt)
        (or (ignore-errors (doc:canonicalize-format fmt)) fmt))))

(defun %extract-backend (fmt)
  (or (handler-case (doc:find-extractor fmt)
        (doc:no-extractor-for-format ()
          nil))
      (when (%text-format-p fmt)
        (make-instance 'plain-text-extractor))))

(defun %extract-source (item)
  "Binary formats pass a pathname or octet vector, never decoded text."
  (let* ((fmt (%item-format item))
         (content (ingest-item-content item))
         (uri (ingest-item-uri item)))
    (cond
      ((%binary-format-p fmt)
       (cond
         ((pathnamep content) content)
         ((and (vectorp content) (not (stringp content))) content)
         ((and uri (probe-file uri)) (pathname uri))
         ((and (stringp content) uri (probe-file uri)) (pathname uri))
         (t content)))
      (t (or content uri)))))

(defun %signal-extractor-error (item fmt cause message)
  (error 'ingest-extractor-error
         :item item
         :format fmt
         :cause cause
         :retryable t
         :message message))

(defun %extract (item)
  (let* ((fmt (%item-format item))
         (source (%extract-source item)))
    (tagbody
     :retry
       (return-from %extract
         (restart-case
             (let ((backend (%extract-backend fmt)))
               (unless backend
                 (%signal-extractor-error
                  item fmt nil
                  (format nil "no extractor for format ~s" fmt)))
               (handler-case
                   (or (doc:extract-document backend source :format fmt)
                       (%signal-extractor-error
                        item fmt nil "extractor returned no document"))
                 (ingest-extractor-error (c)
                   (error c))
                 (error (c)
                   (%signal-extractor-error
                    item fmt c
                    (format nil "extractor failed: ~a" c)))))
           (retry ()
             :report "Retry extract"
             (go :retry))
           (use-value (value)
             :report "Use a supplied extracted-document"
             :interactive (lambda ()
                            (format *query-io* "Extracted document: ")
                            (force-output *query-io*)
                            (list (read *query-io*)))
             value))))))

(defun %zero-embedding (&optional (dim 8))
  (make-array dim :element-type 'single-float :initial-element 0.0f0))

(defun %embedding-vector (emb)
  (cond
    ((null emb) nil)
    ((and (vectorp emb) (not (stringp emb))) emb)
    (t (llm:llm-embedding-vector emb))))

(defun %copy-embedding (raw &optional (dim 8))
  (let ((v (%zero-embedding dim)))
    (when (and raw (arrayp raw))
      (loop for j from 0 below (min dim (length raw))
            do (setf (aref v j) (float (aref raw j) 0.0f0))))
    v))

(defun %apply-embeddings (chunks embeddings)
  (loop for ch in chunks
        for i from 0
        for emb = (and embeddings (nth i embeddings))
        for raw = (%embedding-vector emb)
        do (setf (rag:rag-chunk-embedding ch)
                 (if (and raw (arrayp raw) (plusp (length raw)))
                     (%copy-embedding raw 8)
                     (if (mock-ingest-profile-p)
                         (%zero-embedding 8)
                         (error 'ingest-embedder-error
                                :retryable t
                                :message (format nil
                                                 "missing embedding for chunk ~s"
                                                 (rag:rag-chunk-id ch)))))))
  chunks)

(defun %embed-chunks (embedder chunks)
  (unless chunks
    (return-from %embed-chunks nil))
  (tagbody
   :retry
     (return-from %embed-chunks
       (restart-case
           (handler-case
               (let* ((texts (mapcar #'rag:rag-chunk-text chunks))
                      (result (llm:embed embedder texts :dimensions 8))
                      (embs (and result (llm:llm-embed-result-embeddings result))))
                 (unless (and embs (= (length embs) (length chunks)))
                   (error 'ingest-embedder-error
                          :retryable t
                          :message "embedder returned the wrong number of vectors"))
                 (%apply-embeddings chunks embs))
             (ingest-embedder-error (c)
               (error c))
             (error (c)
               (if (mock-ingest-profile-p)
                   (progn
                     (dolist (ch chunks)
                       (setf (rag:rag-chunk-embedding ch) (%zero-embedding 8)))
                     chunks)
                   (error 'ingest-embedder-error
                          :cause c
                          :retryable t
                          :message (format nil "embedder failed: ~a" c)))))
         (retry ()
           :report "Retry embed"
           (go :retry))
         (use-value (value)
           :report "Use supplied embeddings (list of vectors)"
           :interactive (lambda ()
                          (format *query-io* "Embeddings: ")
                          (force-output *query-io*)
                          (list (read *query-io*)))
           (%apply-embeddings chunks (if (and value (not (consp value)))
                                         (list value)
                                         value)))))))

(defun %upsert-chunks (store chunks)
  (unless chunks
    (return-from %upsert-chunks nil))
  (tagbody
   :retry
     (return-from %upsert-chunks
       (restart-case
           (handler-case
               (progn
                 (rag:upsert store chunks)
                 chunks)
             (error (c)
               (error 'ingest-store-error
                      :cause c
                      :retryable t
                      :message (format nil "store upsert failed: ~a" c))))
         (retry ()
           :report "Retry store upsert"
           (go :retry))
         (use-value (value)
           :report "Treat store upsert as successful with a supplied value"
           :interactive (lambda ()
                          (format *query-io* "Value: ")
                          (force-output *query-io*)
                          (list (read *query-io*)))
           (or value chunks))))))

(defun %ingest-base-metadata (item &key domain source)
  "Chunk metadata. Persist domain + source ownership on every chunk.
   When *TENANT* is bound, also carry tenant + scoped corpus name."
  (append (list :content-hash (ingest-item-hash item)
                :uri (ingest-item-uri item))
          (let ((d (%domain-name domain)))
            (when d (list :domain d)))
          (let ((s (%scope-source-id source)))
            (when s (list :source s)))
          (when (current-tenant)
            (list :tenant (current-tenant)
                  :corpus (tenant-corpus-name
                           (or (ingest-item-uri item)
                               (ingest-item-hash item)))))))

(defun %owned-document-id (item domain source)
  (let ((hash (ingest-item-hash item))
        (d (%domain-name domain))
        (s (%scope-source-id source)))
    (if (and d s)
        (format nil "~a/~a/~a" d s hash)
        hash)))

(defun %chunks-from-document (item doc &key object-store domain source)
  (handler-case
      (rag.text:chunk-extracted-document
       (rag.text:make-block-tree-chunker :store object-store)
       doc
       :document-id (%owned-document-id item domain source)
       :base-metadata (%ingest-base-metadata item :domain domain :source source)
       :store object-store)
    (error (c)
      (error 'ingest-extractor-error
             :item item
             :format (%item-format item)
             :cause c
             :retryable t
             :message (format nil "chunking failed: ~a" c)))))

(defun ingest-one-item (item &key store embedder object-store domain source)
  (when *ingest-item-hook*
    (funcall *ingest-item-hook* (item-plist item)))
  (%ensure-item-content item)
  (let* ((hash (ingest-item-hash item))
         (doc (%extract item))
         (chunks (%chunks-from-document item doc
                                        :object-store object-store
                                        :domain domain
                                        :source source)))
    (unless chunks
      (error 'ingest-extractor-error
             :item item
             :format (%item-format item)
             :retryable t
             :message "extractor produced no chunks"))
    (%embed-chunks embedder chunks)
    (%upsert-chunks store chunks)
    (list :hash hash
          :chunk-ids (mapcar #'rag:rag-chunk-id chunks))))

(defun sweep-deleted-items (store live-hashes &key domain source)
  "Mark-and-sweep within DOMAIN+SOURCE ownership only.
   Returns the deleted chunk ids."
  (let* ((live (make-hash-table :test 'equal))
         (domain-name (%domain-name domain))
         (source-id (%scope-source-id source))
         (stale '()))
    (dolist (h live-hashes)
      (when h (setf (gethash h live) t)))
    (dolist (ch (list-stored-chunks store))
      (when (%chunk-in-scope-p ch domain-name source-id)
        (let ((h (or (getf (rag:rag-chunk-metadata ch) :content-hash)
                     (rag:rag-chunk-document-id ch))))
          (unless (gethash h live)
            (push (rag:rag-chunk-id ch) stale)))))
    (when stale
      (let ((ids (nreverse stale)))
        (rag:delete-ids store ids)
        (return-from sweep-deleted-items ids)))
    stale))

(defun %sweep-completed-report (stored-hashes live-hashes stale-ids)
  "Counts for the journaled sweep-completed event.
   STALE = stored-hashes − enumerated-hashes (set difference, before delete)."
  (let* ((stored (copy-list stored-hashes))
         (enumerated (remove nil live-hashes))
         (stale-hashes (set-difference stored enumerated :test #'equal)))
    (list :event :sweep-completed
          :stale (length stale-hashes)
          :stored (length stored)
          :enumerated (length enumerated)
          :stale-ids (copy-list stale-ids))))

(defun %as-string-list (value)
  "Coerce a journaled enumerate result to hash strings.
   JSON decode turns Lisp lists into vectors; a lone string is one hash."
  (cond
    ((null value) nil)
    ((stringp value) (list value))
    ((and (vectorp value) (not (stringp value)))
     (map 'list (lambda (x)
                  (cond
                    ((stringp x) x)
                    ((null x) nil)
                    (t (princ-to-string x))))
          value))
    ((listp value)
     (loop for x in value
           collect (cond
                     ((stringp x) x)
                     ((null x) nil)
                     ((consp x)
                      (or (getf x :hash) (getf x :HASH)))
                     (t (princ-to-string x)))))
    (t nil)))

(defun %item-step-chunk-ids (result)
  "Replay-safe: journaled ingest-one-item result is a plist with :CHUNK-IDS.
   Older journals stored only the item hash string."
  (cond
    ((and (consp result) (not (keywordp (first result))))
     nil)
    ((consp result)
     (remove nil (%as-string-list (getf result :chunk-ids))))
    (t nil)))

(defun %sanitize-task-token (value)
  (let ((s (substitute #\- #\/ (substitute #\- #\\ (princ-to-string value)))))
    (if (plusp (length s)) s "source")))

(defun default-ingest-task-id (domain source)
  "Unique per run so two concurrent ingests of the same domain do not collide."
  (format nil "ingest/~a/~a/~d-~4,'0d"
          (expert-name domain)
          (%sanitize-task-token (%scope-source-id source))
          (get-universal-time)
          (random 10000)))

(defun run-ingest (domain source &key store journal task-id embedder
                                   object-store)
  "Durable ingest. Per-item step name includes the content hash.
   task-protocol also records each step under (name . nil); sharing the
   name ingest-item makes item 2 replay item 1 (SEEN=1 on kill/resume).
   Enumerate journals a vector of hash strings (JSON-safe; not a plist
   alist). Content is always re-read from the live source.
   Sweep deletes stored hashes that this domain+source no longer enumerates.
   An item step is journaled only after extract, embed, and store succeed."
  (check-type domain expert-domain)
  (let* ((journal (%journal-for domain journal))
         (task (task:make-durable-task
                :id (or task-id (default-ingest-task-id domain source))
                :journal journal))
         (store (%rag-store-for domain store))
         (embedder (%embedder-for domain embedder))
         (object-store (%object-store-for object-store))
         (result nil))
    (task:with-durable-task (task journal)
      (let* ((live (enumerate-items source))
             (hashes (remove nil
                             (%as-string-list
                              (task:with-durable-step
                                  ("enumerate" :idempotency-key
                                   "ingest/enumerate")
                                (map 'vector #'ingest-item-hash live)))))
             (chunk-ids '()))
        (dolist (hash hashes)
          (let ((item (find hash live :key #'ingest-item-hash :test #'equal)))
            (when item
              (restart-case
                  (let ((got (task:with-durable-step
                                 ((format nil "ingest-item/~a" hash)
                                  :idempotency-key hash)
                               (let ((got (ingest-one-item
                                           item
                                           :store store
                                           :embedder embedder
                                           :object-store object-store
                                           :domain domain
                                           :source source)))
                                 (demiurge::%observe-record
                                  "RECORD-INGEST-DOCUMENT"
                                  :count 1
                                  :expert (expert-name domain))
                                 got))))
                    (dolist (id (%item-step-chunk-ids got))
                      (push (if (stringp id)
                                id
                                (princ-to-string id))
                            chunk-ids)))
                (skip ()
                  :report "Skip this item without marking it complete"
                  nil)))))
        (let ((sweep-report
               (task:with-durable-step
                   ("sweep" :idempotency-key "ingest/sweep")
                 (let* ((stored-before (stored-content-hashes
                                        store
                                        :domain domain
                                        :source source))
                        (swept (sweep-deleted-items store hashes
                                                    :domain domain
                                                    :source source)))
                   (%sweep-completed-report stored-before hashes swept)))))
          (setf result (list :hashes hashes
                             :chunk-ids (nreverse chunk-ids)
                             :swept (getf sweep-report :stale-ids)
                             :sweep sweep-report))
          (task:complete-task task result))))
    result))
