(defpackage #:demiurge-bootstrap/bootstrap/code-editor-ks
  (:use #:cl)
  (:import-from #:demiurge/src/capabilities/code-editing
                #:code-editing-capability #:read-file #:write-file #:patch-file #:list-files)
  (:export #:file-code-editing-capability #:make-file-code-editing-capability))

(in-package #:demiurge-bootstrap/bootstrap/code-editor-ks)

(defclass file-code-editing-capability (code-editing-capability)
  ((root :initarg :root :reader editing-root :initform nil
         :documentation "Optional root directory for relative paths.")
   (workspace-dir :initarg :workspace-dir :reader editing-workspace-dir :initform nil
                  :documentation "Host dir mapped to /workspace/ inside containers.")))

(defun make-file-code-editing-capability (&key root workspace-dir (version "0.1.0"))
  (make-instance 'file-code-editing-capability
                 :name :code-editing :version version :root root
                 :workspace-dir workspace-dir))

(defun resolve-path (cap path)
  "Resolve PATH: /workspace/... → shared workspace dir, relative → root, else literal."
  (let ((ws-prefix "/workspace/"))
    (cond
      ((and (editing-workspace-dir cap)
            (>= (length path) (length ws-prefix))
            (string= ws-prefix path :end2 (length ws-prefix)))
       (merge-pathnames (subseq path (length ws-prefix)) (editing-workspace-dir cap)))
      ((and (editing-root cap) (not (uiop:absolute-pathname-p path)))
       (merge-pathnames path (editing-root cap)))
      (t (pathname path)))))

(defmethod read-file ((cap file-code-editing-capability) path &key)
  (let ((p (resolve-path cap path)))
    (when (probe-file p)
      (uiop:read-file-string p))))

(defmethod write-file ((cap file-code-editing-capability) path content &key)
  (let ((p (resolve-path cap path)))
    (ensure-directories-exist p)
    (with-open-file (s p :direction :output :if-exists :supersede)
      (write-string content s))
    t))

(defmethod patch-file ((cap file-code-editing-capability) path patch &key)
  "Simple line-based search-and-replace patch. PATCH is a plist (:old \"...\" :new \"...\")."
  (let* ((p (resolve-path cap path))
         (content (uiop:read-file-string p))
         (old (getf patch :old))
         (new (getf patch :new)))
    (when (and old new (search old content))
      (let ((patched (cl-ppcre:regex-replace (cl-ppcre:quote-meta-chars old) content new)))
        (with-open-file (s p :direction :output :if-exists :supersede)
          (write-string patched s))
        t))))

(defmethod list-files ((cap file-code-editing-capability) directory &key)
  (let ((dir (resolve-path cap directory)))
    (mapcar #'namestring (uiop:directory-files dir))))
