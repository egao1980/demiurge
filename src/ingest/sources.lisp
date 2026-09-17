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

(defun %basename (uri)
  "Last path component. canonicalize-format treats #\/ as a mimetype slash,
   so a Unix path like /tmp/foo.md must not be passed through whole."
  (let ((s (etypecase uri
             (pathname (namestring uri))
             (string uri)
             (t (princ-to-string uri)))))
    (subseq s (1+ (max (or (position #\/ s :from-end t) -1)
                       (or (position #\\ s :from-end t) -1))))))

(defun %infer-format (uri)
  (or (and uri (doc:canonicalize-format (%basename uri)))
      :txt))

(defun %path-string (p)
  (cond
    ((stringp p) p)
    ((pathnamep p) (namestring p))
    (t (pathlib:as-namestring p))))

(defun %text-format-p (fmt)
  (member fmt '(:txt :text :md :markdown :rst :plain nil) :test #'eq))

(defun %binary-format-p (fmt)
  (member fmt '(:pdf :docx :xlsx :pptx :odt :ods :odp
                :png :jpg :jpeg :gif :webp :tif :tiff
                :bin :zip :gz :doc :xls :ppt)
          :test #'eq))

(defclass ingest-source ()
  ((source-id :initarg :source-id :accessor ingest-source-assigned-id
              :initform nil)))

(defun ingest-source-p (x)
  (typep x 'ingest-source))

(defgeneric ingest-source-id (source)
  (:documentation "Stable ownership token written onto every chunk."))

(defmethod ingest-source-id ((source ingest-source))
  (or (ingest-source-assigned-id source)
      (format nil "~(~a~)" (class-name (class-of source)))))

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

(defun make-file-source (&key root (pattern "*") (recursive t) source-id)
  (make-instance 'file-source :root root :pattern pattern :recursive recursive
                              :source-id source-id))

(defmethod ingest-source-id ((source file-source))
  (or (ingest-source-assigned-id source)
      (format nil "file:~a"
              (string-right-trim
               "/\\"
               (%path-string (uiop:ensure-directory-pathname
                              (file-source-root source)))))))

(defun %name-matches-pattern-p (namestring pattern)
  (cond
    ((or (null pattern) (string= pattern "*")) t)
    ((and (plusp (length pattern)) (char= (char pattern 0) #\*))
     (let ((suffix (subseq pattern 1)))
       (and (>= (length namestring) (length suffix))
            (string= namestring suffix
                     :start1 (- (length namestring) (length suffix))))))
    (t (string= namestring pattern))))

(defun %uiop-fallback-files (root pattern recursive)
  "UIOP walk when pathlib:glob returns nothing (macOS DIRECTORY + **/ is flaky)."
  (let ((base (uiop:ensure-directory-pathname root))
        (out '()))
    (labels ((walk (dir)
               (dolist (f (ignore-errors (uiop:directory-files dir)))
                 (when (%name-matches-pattern-p (file-namestring f) pattern)
                   (push f out)))
               (when recursive
                 (dolist (sub (ignore-errors (uiop:subdirectories dir)))
                   (walk sub)))))
      (walk base)
      (nreverse out))))

(defmethod enumerate-items ((source file-source))
  (let* ((root (file-source-root source))
         (pattern (file-source-pattern source))
         (recursive (file-source-recursive source))
         (paths (or (pathlib:glob root pattern :recursive recursive)
                    (%uiop-fallback-files root pattern recursive))))
    (loop for p in paths
          when (if (pathnamep p)
                   (uiop:file-exists-p p)
                   (pathlib:file-p p))
            collect (let* ((ns (%path-string p))
                           (fmt (%infer-format ns))
                           (bytes (if (pathnamep p)
                                      (with-open-file (in p :element-type
                                                          '(unsigned-byte 8))
                                        (let ((buf (make-array (file-length in)
                                                               :element-type
                                                               '(unsigned-byte 8))))
                                          (read-sequence buf in)
                                          buf))
                                      (pathlib:read-bytes p)))
                           (content (cond
                                      ((%binary-format-p fmt)
                                       (or (and (pathnamep p) p)
                                           (and (stringp ns) (probe-file ns))
                                           bytes))
                                      ((pathnamep p)
                                       (uiop:read-file-string p))
                                      (t (pathlib:read-text p)))))
                      (make-ingest-item
                       :id ns
                       :uri ns
                       :content content
                       :hash (content-hash bytes)
                       :format fmt)))))

(defclass imap-source (ingest-source)
  ((client :initarg :client :accessor imap-source-client :initform nil)
   (mailbox :initarg :mailbox :accessor imap-source-mailbox :initform "INBOX")
   (search :initarg :search :accessor imap-source-search :initform "ALL")
   (host :initarg :host :accessor imap-source-host :initform nil)
   (port :initarg :port :accessor imap-source-port :initform nil)
   (username :initarg :username :accessor imap-source-username :initform nil)
   (password :initarg :password :accessor imap-source-password :initform nil)
   (tls :initarg :tls :accessor imap-source-tls :initform :starttls)))

(defun imap-source-p (x)
  (typep x 'imap-source))

(defun %imap-tls-mode (tls)
  "→ :TLS | :STARTTLS | NIL (plaintext)."
  (cond
    ((or (eq tls t) (eq tls :tls) (eq tls :imaps)) :tls)
    ((or (eq tls :starttls) (eq tls :start-tls) (eq tls :always)) :starttls)
    ((or (null tls) (eq tls :plain) (eq tls :none)) nil)
    (t :starttls)))

(defun imap-tls-mode (source)
  (%imap-tls-mode (imap-source-tls source)))

(defun imap-secure-p (source)
  (not (null (imap-tls-mode source))))

(defun %default-imap-port (tls-mode)
  (if (eq tls-mode :tls) 993 143))

(defun %assert-imap-not-plaintext (source)
  (unless (imap-secure-p source)
    (error 'imap-plaintext-refused
           :source source
           :message "IMAP login requires TLS or STARTTLS")))

(defun make-imap-source (&key client mailbox host port username password
                           (search "ALL") (tls :starttls) source-id)
  "TLS defaults to STARTTLS (port 143) or implicit TLS on 993 when :TLS T.
   USERNAME/PASSWORD without TLS/STARTTLS is refused."
  (let* ((mode (%imap-tls-mode tls))
         (source (make-instance 'imap-source
                                :client client
                                :mailbox (or mailbox "INBOX")
                                :search (or search "ALL")
                                :host host
                                :port (or port (%default-imap-port mode))
                                :username username
                                :password password
                                :tls tls
                                :source-id source-id)))
    (when (and (or username password) (not (imap-secure-p source)))
      (%assert-imap-not-plaintext source))
    source))

(defmethod ingest-source-id ((source imap-source))
  (or (ingest-source-assigned-id source)
      (format nil "imap:~a:~a"
              (or (imap-source-host source) "local")
              (imap-source-mailbox source))))

(defun %ensure-imap-client (source)
  (or (imap-source-client source)
      (progn
        (%assert-imap-not-plaintext source)
        (let ((client (mail:make-imap-client
                       :host (or (imap-source-host source) "localhost")
                       :port (or (imap-source-port source)
                                 (%default-imap-port (imap-tls-mode source))))))
          (setf (imap-source-client source) client)
          client))))

(defun %message-text (msg)
  (cond
    ((mail:message-p msg)
     (or (mail:message-body msg)
         (mail:message-subject msg)
         ""))
    ((stringp msg) msg)
    (t (princ-to-string msg))))

(defun %parse-fetch-uid (raw)
  (let ((pos (and raw (search "UID " raw :test #'char-equal))))
    (when pos
      (parse-integer raw :start (+ pos 4) :junk-allowed t))))

(defun %message-entity (msg)
  (cond
    ((mail:message-p msg) (mail:message-entity msg))
    ((mime:mime-entity-p msg) msg)
    ((or (stringp msg) (vectorp msg))
     (handler-case (mime:parse-mime msg) (error () nil)))
    (t nil)))

(defun %entity-text (entity)
  (let ((content (and entity (mime:mime-content entity))))
    (cond
      ((stringp content) content)
      ((and (vectorp content) (not (stringp content)))
       (handler-case (%object-text content) (error () "")))
      (t ""))))

(defun %attachment-p (entity)
  (let ((cd (and entity (mime:mime-content-disposition entity))))
    (or (and cd (string-equal (mime:content-disposition-type cd) "attachment"))
        (and cd (mime:disposition-filename cd)))))

(defun %part-format (entity)
  (let* ((cd (mime:mime-content-disposition entity))
         (filename (and cd (mime:disposition-filename cd)))
         (ct (mime:mime-content-type entity)))
    (or (and filename (%infer-format filename))
        (and ct (ignore-errors
                  (doc:canonicalize-format
                   (format nil "~a/~a"
                           (mime:media-type-type ct)
                           (mime:media-type-subtype ct)))))
        :txt)))

(defun %extract-attachment-text (entity)
  (let* ((fmt (%part-format entity))
         (source (or (mime:mime-content entity) #()))
         (backend (or (ignore-errors (doc:find-extractor fmt))
                      (make-instance 'plain-text-extractor))))
    (handler-case
        (let ((doc (doc:extract-document backend source :format fmt)))
          (or (and doc (doc:document-text doc))
              (%entity-text entity)))
      (error () (%entity-text entity)))))

(defun %collect-mime-text (entity)
  "Walk MIME parts: text bodies concatenated; attachments via extractors."
  (let ((parts '()))
    (labels ((walk (e)
               (cond
                 ((null e) nil)
                 ((mime:multipart-p e)
                  (dolist (p (mime:mime-parts e)) (walk p)))
                 ((%attachment-p e)
                  (let ((text (or (%extract-attachment-text e) "")))
                    (when (plusp (length text))
                      (push text parts))))
                 (t
                  (let ((text (%entity-text e)))
                    (when (plusp (length text))
                      (push text parts)))))))
      (walk entity)
      (format nil "~{~a~^~%~}" (nreverse parts)))))

(defun %canonical-mime-bytes (raw entity)
  (cond
    ((and raw (vectorp raw) (not (stringp raw))) raw)
    ((and raw (stringp raw))
     (map '(vector (unsigned-byte 8)) #'char-code raw))
    (entity
     (map '(vector (unsigned-byte 8)) #'char-code (mime:print-mime entity)))
    (t #())))

(defun %message-id-header (entity)
  (or (and entity (mime:header-value entity "message-id"))
      ""))

(defun %imap-item-from-message (source seq msg)
  (let* ((raw (if (stringp msg) msg (ignore-errors (mail:print-message msg))))
         (entity (or (%message-entity msg)
                     (and raw (handler-case (mime:parse-mime raw)
                                (error () nil)))))
         (uid (or (and raw (%parse-fetch-uid raw)) seq))
         (mid (string-trim '(#\Space #\< #\>) (%message-id-header entity)))
         (id (format nil "~a+~a"
                     (if (plusp (length mid)) mid "unknown")
                     uid))
         (text (let ((collected (and entity (%collect-mime-text entity))))
                 (if (and collected (plusp (length collected)))
                     collected
                     (%message-text msg))))
         (bytes (%canonical-mime-bytes raw entity)))
    (make-ingest-item
     :id id
     :uri (format nil "imap:~a:~a" (imap-source-mailbox source) id)
     :content text
     :hash (content-hash (if (plusp (length bytes)) bytes text))
     :format :txt
     :metadata (list :mailbox (imap-source-mailbox source)
                     :seq seq
                     :uid uid
                     :message-id mid))))

(defmethod enumerate-items ((source imap-source))
  (let ((client (%ensure-imap-client source)))
    (when (eq (mail:imap-client-state client) :disconnected)
      (mail:imap-connect client)
      (when (imap-source-username source)
        (%assert-imap-not-plaintext source)
        (mail:imap-login client
                         (imap-source-username source)
                         (imap-source-password source))))
    (mail:imap-select client (imap-source-mailbox source))
    (let ((seqs (or (mail:imap-search client (imap-source-search source))
                    nil)))
      (loop for seq in seqs
            nconc (loop for msg in (mail:imap-fetch client seq)
                        collect (%imap-item-from-message source seq msg))))))

(defclass s3-source (ingest-source)
  ((store :initarg :store :accessor s3-source-store)
   (bucket :initarg :bucket :accessor s3-source-bucket :initform nil)
   (prefix :initarg :prefix :accessor s3-source-prefix :initform "")
   (page-size :initarg :page-size :accessor s3-source-page-size
              :initform 1000)))

(defun s3-source-p (x)
  (typep x 's3-source))

(defun make-s3-source (&key store bucket (prefix "") (page-size 1000) source-id)
  (make-instance 's3-source :store store :bucket bucket :prefix (or prefix "")
                            :page-size (or page-size 1000)
                            :source-id source-id))

(defmethod ingest-source-id ((source s3-source))
  (or (ingest-source-assigned-id source)
      (format nil "s3:~a/~a"
              (or (s3-source-bucket source) "")
              (or (s3-source-prefix source) ""))))

(defun %object-text (bytes)
  (if (stringp bytes)
      bytes
      (map 'string #'code-char bytes)))

(defun %list-objects-paged (store prefix page-size)
  "Drain LIST-OBJECTS via continuation-token until the listing is complete."
  (loop with token = nil
        for listing = (apply #'obj:list-objects store
                             :prefix prefix
                             (append (when page-size
                                       (list :max-keys page-size))
                                     (when token
                                       (list :continuation-token token))))
        append (copy-list (obj:object-listing-objects listing))
        do (setf token (obj:object-listing-continuation-token listing))
        while (obj:object-listing-truncated-p listing)))

(defun %coerce-object-content (bytes fmt)
  (if (%binary-format-p fmt)
      bytes
      (%object-text bytes)))

(defun %ensure-item-content (item)
  "Load octets on demand when CONTENT is empty (S3 get-object).
   Binary formats keep octets; text formats decode."
  (when (and (null (ingest-item-content item))
             (ingest-item-metadata item))
    (let* ((meta (ingest-item-metadata item))
           (store (getf meta :object-store))
           (key (getf meta :key)))
      (when (and store key)
        (let ((bytes (obj:get-object store key)))
          (setf (ingest-item-content item)
                (%coerce-object-content bytes (ingest-item-format item)))
          (unless (ingest-item-hash item)
            (setf (ingest-item-hash item) (content-hash bytes)))))))
  (ingest-item-content item))

(defmethod enumerate-items ((source s3-source))
  (let* ((store (or (s3-source-store source) obj:*object-store*))
         (prefix (or (s3-source-prefix source) ""))
         (bucket (s3-source-bucket source))
         (page-size (s3-source-page-size source)))
    (unless store
      (error 'ingest-source-error
             :source source
             :message "s3-source has no object-store"))
    (loop for stat in (%list-objects-paged store prefix page-size)
          for key = (obj:object-stat-key stat)
          for head = (handler-case (obj:head-object store key)
                       (error () nil))
          for etag = (or (and head (obj:object-stat-etag head))
                         (obj:object-stat-etag stat))
          for bytes = (obj:get-object store key)
          for fmt = (%infer-format key)
          collect (make-ingest-item
                   :id key
                   :uri (format nil "s3://~a/~a" (or bucket "") key)
                   :content (%coerce-object-content bytes fmt)
                   :hash (content-hash bytes)
                   :format fmt
                   :metadata (list :bucket bucket :key key :etag etag
                                   :object-store store)))))

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
