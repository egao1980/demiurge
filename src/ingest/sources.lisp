(in-package #:demiurge/ingest)

(defclass ingest-item ()
  ((id :initarg :id :accessor ingest-item-id :initform nil)
   (hash :initarg :hash :accessor ingest-item-hash :initform nil)
   (uri :initarg :uri :accessor ingest-item-uri :initform nil)
   (content :initarg :content :accessor ingest-item-content :initform nil)
   (format :initarg :format :accessor ingest-item-format :initform nil)
   (metadata :initarg :metadata :accessor ingest-item-metadata :initform nil)))

(defun ingest-item-p (x)
  (typep x 'ingest-item))

(defun content-hash (content)
  "Stable SHA-256 hex of CONTENT (string or octets)."
  (doc:portable-digest content))

(defun make-ingest-item (&key id hash uri content format metadata)
  (let* ((content content)
         (hash (or hash (and content (content-hash content)))))
    (make-instance 'ingest-item
                   :id (or id uri hash)
                   :hash hash
                   :uri uri
                   :content content
                   :format format
                   :metadata (copy-list metadata))))

(defun item-plist (item)
  (list :id (ingest-item-id item)
        :hash (ingest-item-hash item)
        :uri (ingest-item-uri item)
        :content (ingest-item-content item)
        :format (ingest-item-format item)
        :metadata (ingest-item-metadata item)))

(defun item-from-plist (plist)
  (make-ingest-item
   :id (getf plist :id)
   :hash (getf plist :hash)
   :uri (getf plist :uri)
   :content (getf plist :content)
   :format (getf plist :format)
   :metadata (getf plist :metadata)))

(defun %infer-format (uri)
  (or (and uri (doc:canonicalize-format uri))
      :txt))

(defun %path-string (p)
  (cond
    ((stringp p) p)
    ((pathnamep p) (namestring p))
    (t (pathlib:as-namestring p))))

(defclass ingest-source ()
  ())

(defun ingest-source-p (x)
  (typep x 'ingest-source))

(defgeneric enumerate-items (source)
  (:documentation "→ list of INGEST-ITEM with stable content hashes."))

(defmethod enumerate-items ((source ingest-source))
  (restart-case
      (error 'ingest-source-error
             :source source
             :message (format nil "~a does not implement enumerate-items"
                              (class-of source)))
    (use-value (value)
      :report "Use a supplied item list"
      value)
    (skip ()
      :report "Treat the source as empty"
      nil)))

(defclass file-source (ingest-source)
  ((root :initarg :root :accessor file-source-root)
   (pattern :initarg :pattern :accessor file-source-pattern :initform "*")
   (recursive :initarg :recursive :accessor file-source-recursive :initform t)))

(defun file-source-p (x)
  (typep x 'file-source))

(defun make-file-source (&key root (pattern "*") (recursive t))
  (make-instance 'file-source :root root :pattern pattern :recursive recursive))

(defmethod enumerate-items ((source file-source))
  (let* ((root (file-source-root source))
         (paths (pathlib:glob root (file-source-pattern source)
                              :recursive (file-source-recursive source))))
    (loop for p in paths
          when (pathlib:file-p p)
            collect (let* ((ns (%path-string p))
                           (text (pathlib:read-text p))
                           (bytes (pathlib:read-bytes p)))
                      (make-ingest-item
                       :id ns
                       :uri ns
                       :content text
                       :hash (content-hash bytes)
                       :format (%infer-format ns))))))

(defclass imap-source (ingest-source)
  ((client :initarg :client :accessor imap-source-client :initform nil)
   (mailbox :initarg :mailbox :accessor imap-source-mailbox :initform "INBOX")
   (search :initarg :search :accessor imap-source-search :initform "ALL")
   (host :initarg :host :accessor imap-source-host :initform nil)
   (port :initarg :port :accessor imap-source-port :initform 143)
   (username :initarg :username :accessor imap-source-username :initform nil)
   (password :initarg :password :accessor imap-source-password :initform nil)))

(defun imap-source-p (x)
  (typep x 'imap-source))

(defun make-imap-source (&key client mailbox host port username password
                           (search "ALL"))
  (make-instance 'imap-source
                 :client client
                 :mailbox (or mailbox "INBOX")
                 :search (or search "ALL")
                 :host host
                 :port (or port 143)
                 :username username
                 :password password))

(defun %ensure-imap-client (source)
  (or (imap-source-client source)
      (let ((client (mail:make-imap-client
                     :host (or (imap-source-host source) "localhost")
                     :port (imap-source-port source))))
        (setf (imap-source-client source) client)
        client)))

(defun %message-text (msg)
  (cond
    ((mail:message-p msg)
     (or (mail:message-body msg)
         (mail:message-subject msg)
         ""))
    ((stringp msg) msg)
    (t (princ-to-string msg))))

(defmethod enumerate-items ((source imap-source))
  (let ((client (%ensure-imap-client source)))
    (when (eq (mail:imap-client-state client) :disconnected)
      (mail:imap-connect client)
      (when (imap-source-username source)
        (mail:imap-login client
                         (imap-source-username source)
                         (imap-source-password source))))
    (mail:imap-select client (imap-source-mailbox source))
    (let ((seqs (or (mail:imap-search client (imap-source-search source))
                    nil)))
      (loop for seq in seqs
            nconc (loop for msg in (mail:imap-fetch client seq)
                        for text = (%message-text msg)
                        for id = (format nil "imap:~a:~a"
                                         (imap-source-mailbox source) seq)
                        collect (make-ingest-item
                                 :id id
                                 :uri id
                                 :content text
                                 :hash (content-hash text)
                                 :format :txt
                                 :metadata (list :mailbox
                                                 (imap-source-mailbox source)
                                                 :seq seq)))))))

(defclass s3-source (ingest-source)
  ((store :initarg :store :accessor s3-source-store)
   (bucket :initarg :bucket :accessor s3-source-bucket :initform nil)
   (prefix :initarg :prefix :accessor s3-source-prefix :initform "")))

(defun s3-source-p (x)
  (typep x 's3-source))

(defun make-s3-source (&key store bucket (prefix ""))
  (make-instance 's3-source :store store :bucket bucket :prefix (or prefix "")))

(defun %object-text (bytes)
  (if (stringp bytes)
      bytes
      (map 'string #'code-char bytes)))

(defmethod enumerate-items ((source s3-source))
  (let* ((store (or (s3-source-store source) obj:*object-store*))
         (prefix (or (s3-source-prefix source) ""))
         (bucket (s3-source-bucket source)))
    (unless store
      (error 'ingest-source-error
             :source source
             :message "s3-source has no object-store"))
    (let ((listing (obj:list-objects store :prefix prefix)))
      (loop for stat in (obj:object-listing-objects listing)
            for key = (obj:object-stat-key stat)
            for bytes = (obj:get-object store key)
            collect (make-ingest-item
                     :id key
                     :uri (format nil "s3://~a/~a" (or bucket "") key)
                     :content (%object-text bytes)
                     :hash (content-hash bytes)
                     :format (%infer-format key)
                     :metadata (list :bucket bucket :key key))))))

;;; Fallback extractor so .md/.txt ingest works without a live office backend.
(defclass plain-text-extractor (doc:doc-extract-backend) ())

(defmethod doc:extract-document ((backend plain-text-extractor) source
                                 &key format)
  (declare (ignore format))
  (let* ((text (etypecase source
                 (string source)
                 (pathname (pathlib:read-text source))
                 ((vector (unsigned-byte 8)) (%object-text source))))
         (doc (doc:make-extracted-document
               :metadata (doc:make-document-metadata
                          :mimetype "text/plain"
                          :content-hash (content-hash text))
               :blocks (list (doc:make-text-block :text text)))))
    (doc:ensure-ids doc)
    doc))

(doc:register-extractor (make-instance 'plain-text-extractor)
                        :formats '(:txt :text :md :markdown :rst)
                        :priority -10)
