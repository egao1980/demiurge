(in-package #:demiurge/bundle)

(defun %package-symbol (package name)
  (let ((pkg (find-package package)))
    (and pkg (find-symbol name pkg))))

(defun %utf8-octets (string)
  "UTF-8 octets of STRING. SBCL native, else babel, else char-code (ASCII)."
  (let ((s (or string "")))
    (cond
      ((%package-symbol :sb-ext "STRING-TO-OCTETS")
       (funcall (%package-symbol :sb-ext "STRING-TO-OCTETS")
                s :external-format :utf-8))
      ((%package-symbol :babel "STRING-TO-OCTETS")
       (funcall (%package-symbol :babel "STRING-TO-OCTETS")
                s :encoding :utf-8))
      (t
       (map '(simple-array (unsigned-byte 8) (*)) #'char-code s)))))

(defun %octets-string (octets)
  (cond
    ((stringp octets) octets)
    ((%package-symbol :sb-ext "OCTETS-TO-STRING")
     (funcall (%package-symbol :sb-ext "OCTETS-TO-STRING")
              octets :external-format :utf-8))
    ((%package-symbol :babel "OCTETS-TO-STRING")
     (funcall (%package-symbol :babel "OCTETS-TO-STRING")
              octets :encoding :utf-8))
    (t
     (map 'string #'code-char octets))))

(defun %content-digest (content)
  "SHA-256 hex via doc-extract-protocol:portable-digest. Do not invent a hash."
  (doc:portable-digest content))

(defun %write-octets (path octets)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                       :if-does-not-exist :create
                       :element-type '(unsigned-byte 8))
    (write-sequence octets out))
  path)

(defun %read-octets (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((buf (make-array (file-length in)
                           :element-type '(unsigned-byte 8))))
      (read-sequence buf in)
      buf)))

(defun %json-escape (string)
  (with-output-to-string (o)
    (loop for c across (or string "")
          do (case c
               (#\" (write-string "\\\"" o))
               (#\\ (write-string "\\\\" o))
               (#\Newline (write-string "\\n" o))
               (#\Return (write-string "\\r" o))
               (#\Tab (write-string "\\t" o))
               (t (if (and (graphic-char-p c) (< (char-code c) 128))
                      (write-char c o)
                      (format o "\\u~4,'0x" (char-code c))))))))

(defun %json-string (string)
  (format nil "\"~a\"" (%json-escape string)))

(defun %json-object (pairs)
  "PAIRS is an alist of (key . value) where value is already JSON text."
  (with-output-to-string (o)
    (write-char #\{ o)
    (loop for (k . v) in pairs
          for first = t then nil
          do (unless first (write-char #\, o))
             (format o "~a:~a" (%json-string k) v))
    (write-char #\} o)))

(defun %json-array (items)
  (with-output-to-string (o)
    (write-char #\[ o)
    (loop for item in items
          for first = t then nil
          do (unless first (write-char #\, o))
             (write-string item o))
    (write-char #\] o)))

(defun %descriptor-json (digest size media-type &optional annotations)
  (%json-object
   `(("mediaType" . ,(%json-string media-type))
     ("digest" . ,(%json-string (format nil "sha256:~a" digest)))
     ("size" . ,(princ-to-string size))
     ,@(when annotations
         `(("annotations" . ,(%json-object
                              (loop for (k . v) in annotations
                                    collect (cons k (%json-string v))))))))))

(defun %blob-path (layout digest)
  (merge-pathnames (format nil "blobs/sha256/~a" digest)
                   (uiop:ensure-directory-pathname layout)))

(defun %put-blob (layout content &key media-type title)
  "Write CONTENT (string or octets) as a content-addressed blob. → plist."
  (let* ((octets (if (stringp content)
                     (%utf8-octets content)
                     content))
         (digest (%content-digest octets)))
    (%write-octets (%blob-path layout digest) octets)
    (list :digest digest
          :size (length octets)
          :media-type (or media-type "application/octet-stream")
          :title title)))

(defun %skill-store-of (domain)
  (let ((prof (and (expert-domain-p domain) (expert-profile domain))))
    (and (deployment-profile-p prof) (profile-skill-store prof))))

(defun %steering-directives (domain)
  (let ((s (expert-steering domain)))
    (cond
      ((null s) nil)
      ((steer:steering-source-p s)
       (steer:list-directives s))
      ((and (consp s) (every #'steer:steer-directive-p s)) s)
      (t (ignore-errors
           (steer:list-directives (steer:coerce-steering s)))))))

(defun %store-skill-names (store)
  (when (and store (steer:skill-store-p store))
    (let* ((root (steer:skill-store-root store))
           (pattern (merge-pathnames "*/SKILL.md" root)))
      (loop for p in (directory pattern)
            for dirs = (pathname-directory p)
            for name = (and (consp dirs) (car (last dirs)))
            when name collect name))))

(defun %collect-skills (domain)
  "→ list of (skill-ref . markdown-text)."
  (let ((out '())
        (seen (make-hash-table :test 'equal))
        (store (%skill-store-of domain)))
    (flet ((add (name version text)
             (let ((key (string-downcase (string name))))
               (unless (gethash key seen)
                 (setf (gethash key seen) t)
                 (let* ((digest (%content-digest (%utf8-octets text)))
                        (ref (make-bundle-skill-ref
                              :name key
                              :version (or version "")
                              :digest digest)))
                   (push (cons ref text) out))))))
      (when store
        (dolist (name (%store-skill-names store))
          (let ((path (merge-pathnames
                       (format nil "~a/SKILL.md" name)
                       (steer:skill-store-root store))))
            (when (probe-file path)
              (add name
                   (%latest-skill-version store name)
                   (uiop:read-file-string path))))))
      (dolist (d (%steering-directives domain))
        (when (eq (steer:steer-directive-kind d) :skill)
          (add (steer:steer-directive-name d)
               ""
               (steer:serialize-skill-markdown d)))))
    (nreverse out)))

(defun %dataset-json (dataset)
  (let ((cases (mapcar (lambda (c)
                         (%json-object
                          `(("input" . ,(%json-string
                                         (princ-to-string
                                          (eval:eval-case-input c))))
                            ("expected" . ,(%json-string
                                            (princ-to-string
                                             (eval:eval-case-expected c)))))))
                       (eval:eval-dataset-cases dataset))))
    (%json-object
     `(("name" . ,(%json-string (eval:eval-dataset-name dataset)))
       ("version" . ,(%json-string (eval:eval-dataset-version dataset)))
       ("cases" . ,(%json-array cases))
       ("sexp" . ,(%json-string (eval:dump-dataset dataset :format :sexp)))))))

(defun %collect-datasets (domain)
  "→ list of (eval-dataset-ref . json-text)."
  (loop for ds in (expert-eval-suites domain)
        when (eval:eval-dataset-p ds)
          collect (let* ((json (%dataset-json ds))
                         (digest (%content-digest (%utf8-octets json)))
                         (ref (make-bundle-eval-dataset-ref
                               :name (eval:eval-dataset-name ds)
                               :version (eval:eval-dataset-version ds)
                               :digest digest
                               :payload (eval:dump-dataset ds :format :sexp))))
                    (cons ref json))))

(defun %path-as-directory (p)
  (cond
    ((null p) nil)
    ((pathnamep p)
     (if (uiop:directory-pathname-p p)
         p
         (uiop:ensure-directory-pathname p)))
    ((stringp p)
     (uiop:ensure-directory-pathname p))
    (t nil)))

(defun %corpus-roots (domain)
  (loop for c in (expert-corpora domain)
        for dir = (%path-as-directory c)
        when (and dir (uiop:directory-exists-p dir))
          collect dir))

(defun %infer-format-name (uri)
  (let* ((s (if (pathnamep uri) (namestring uri) (string uri)))
         (dot (position #\. s :from-end t)))
    (if dot
        (string-downcase (subseq s (1+ dot)))
        "txt")))

(defun %collect-corpus (domain)
  "→ list of (corpus-source . list of (item . text))."
  (loop for root in (%corpus-roots domain)
        collect (let* ((source (ingest:make-file-source :root root :pattern "*"
                                                       :recursive t))
                       (pairs '()))
                  (dolist (item (ingest:enumerate-items source))
                    (let* ((text (or (ingest:ingest-item-content item) ""))
                           (digest (or (ingest:ingest-item-hash item)
                                       (%content-digest text)))
                           (ref (make-bundle-corpus-item
                                 :uri (or (ingest:ingest-item-uri item)
                                          (ingest:ingest-item-id item)
                                          "")
                                 :digest digest
                                 :format (string-downcase
                                          (string (or (ingest:ingest-item-format item)
                                                      (%infer-format-name
                                                       (ingest:ingest-item-uri item))))))))
                      (push (cons ref text) pairs)))
                  (cons (make-bundle-corpus-source
                         :kind "file"
                         :spec (%prin1-string (list :root (namestring root)
                                                    :pattern "*"
                                                    :recursive t))
                         :items (mapcar #'car (nreverse pairs)))
                        (nreverse pairs)))))

(defun %checksum-annotation (entries)
  (format nil "~{~a~^;~}"
          (loop for e in entries
                collect (format nil "sha256:~a=~a"
                                (getf e :digest)
                                (or (getf e :title) "blob")))))

(defun %write-oci-layout (layout layers &key name version checksums)
  "Write oci-layout + index.json + image manifest around LAYERS (blob plists)."
  (let* ((config (list :media-type "application/vnd.demiurge.bundle.config.v1+json"
                       :title "config"))
         (config-json (%json-object
                       `(("architecture" . "\"generic\"")
                         ("os" . "\"generic\"")
                         ("rootfs" . "{\"type\":\"layers\",\"diff_ids\":[]}")
                         ("config" . ,(%json-object
                                       `(("Env" . "[]")
                                         ("Labels" . ,(%json-object
                                                       `(("org.opencontainers.image.title"
                                                          . ,(%json-string name))
                                                         ("org.opencontainers.image.version"
                                                          . ,(%json-string version)))))))))))
         (config-blob (%put-blob layout config-json
                                 :media-type (getf config :media-type)
                                 :title "config"))
         (annotations `(("org.opencontainers.image.title" . ,name)
                        ("org.opencontainers.image.version" . ,version)
                        (,+checksum-annotation-key+ . ,(or checksums ""))
                        (,+cosign-annotation-key+ . "")
                        ("io.demiurge.bundle.manifest"
                         . ,(format nil "sha256:~a"
                                    (getf (first layers) :digest)))))
         (manifest-json
          (%json-object
           `(("schemaVersion" . "2")
             ("mediaType" . "\"application/vnd.oci.image.manifest.v1+json\"")
             ("config" . ,(%descriptor-json (getf config-blob :digest)
                                            (getf config-blob :size)
                                            (getf config-blob :media-type)
                                            '(("org.opencontainers.image.title"
                                               . "config"))))
             ("layers" . ,(%json-array
                           (loop for layer in layers
                                 collect (%descriptor-json
                                          (getf layer :digest)
                                          (getf layer :size)
                                          (getf layer :media-type)
                                          (when (getf layer :title)
                                            `(("org.opencontainers.image.title"
                                               . ,(getf layer :title))))))))
             ("annotations" . ,(%json-object
                                (loop for (k . v) in annotations
                                      collect (cons k (%json-string v))))))))
         (manifest-blob (%put-blob layout manifest-json
                                   :media-type
                                   "application/vnd.oci.image.manifest.v1+json"
                                   :title "oci-manifest"))
         (index-json
          (%json-object
           `(("schemaVersion" . "2")
             ("mediaType" . "\"application/vnd.oci.image.index.v1+json\"")
             ("manifests" . ,(%json-array
                              (list (%descriptor-json
                                     (getf manifest-blob :digest)
                                     (getf manifest-blob :size)
                                     "application/vnd.oci.image.manifest.v1+json"
                                     annotations))))))))
    (with-open-file (out (merge-pathnames "oci-layout" layout)
                         :direction :output :if-exists :supersede)
      (write-string "{\"imageLayoutVersion\":\"1.0.0\"}" out))
    (with-open-file (out (merge-pathnames "index.json" layout)
                         :direction :output :if-exists :supersede)
      (write-string index-json out))
    (list :layout layout
          :manifest-digest (getf manifest-blob :digest)
          :index-digest (getf (first layers) :digest))))

(defun %layout-root (registry name version)
  (uiop:ensure-directory-pathname
   (merge-pathnames (format nil "~a/~a/" name version)
                    (uiop:ensure-directory-pathname registry))))

(defgeneric pack-expert (domain &key registry version cycle-ids eval-run-ids)
  (:documentation
   "Assemble a manifest + content blobs and write a local OCI layout directory.
    REGISTRY is a pathname (no live GHCR push). → plist (:layout :manifest …)."))

(defmethod pack-expert ((domain expert-domain) &key registry version
                       cycle-ids eval-run-ids)
  (unless registry
    (error 'bundle-error :message "pack-expert requires :registry (pathname)"))
  (let* ((version (or version "0.1.0"))
         (name (expert-name domain))
         (layout (%layout-root registry name version))
         (skills (%collect-skills domain))
         (datasets (%collect-datasets domain))
         (corpus (%collect-corpus domain))
         (manifest (assemble-manifest
                    domain
                    :version version
                    :skill-refs (mapcar #'car skills)
                    :eval-datasets (mapcar #'car datasets)
                    :corpus-sources (mapcar #'car corpus)
                    :cycle-ids cycle-ids
                    :eval-run-ids eval-run-ids))
         (manifest-text (%prin1-string (schema:dump manifest :as :plist)))
         (layers '()))
    (ensure-directories-exist layout)
    (push (%put-blob layout manifest-text
                     :media-type "application/vnd.demiurge.bundle.manifest.v1+lisp"
                     :title "manifest")
          layers)
    (dolist (pair skills)
      (push (%put-blob layout (cdr pair)
                       :media-type "text/markdown"
                       :title (format nil "skill/~a"
                                      (bundle-skill-ref-name (car pair))))
            layers))
    (dolist (pair datasets)
      (push (%put-blob layout (cdr pair)
                       :media-type "application/json"
                       :title (format nil "dataset/~a"
                                      (bundle-eval-dataset-ref-name (car pair))))
            layers))
    (dolist (entry corpus)
      (dolist (pair (cdr entry))
        (push (%put-blob layout (cdr pair)
                         :media-type "text/plain"
                         :title (format nil "corpus/~a"
                                        (bundle-corpus-item-uri (car pair))))
              layers)))
    (setf layers (nreverse layers))
    (let* ((checksums (%checksum-annotation layers))
           (ann (format nil "~a;~a=" checksums +cosign-annotation-key+)))
      (setf (expert-bundle-manifest-annotations manifest) ann)
      (let ((updated (%prin1-string (schema:dump manifest :as :plist))))
        (setf (first layers)
              (%put-blob layout updated
                         :media-type
                         "application/vnd.demiurge.bundle.manifest.v1+lisp"
                         :title "manifest")))
      (%write-oci-layout layout layers
                         :name name :version version
                         :checksums checksums)
      (list :layout (namestring layout)
            :name name
            :version version
            :manifest manifest
            :digest (getf (first layers) :digest)))))
