(defpackage #:demiurge-bootstrap/bootstrap/compute-ks
  (:use #:cl)
  (:import-from #:demiurge/src/capabilities/compute
                #:compute-capability
                #:run-command #:create-environment #:exec-in-environment #:destroy-environment)
  (:export #:local-compute-capability #:make-local-compute-capability))

(in-package #:demiurge-bootstrap/bootstrap/compute-ks)

(defclass local-compute-capability (compute-capability)
  ((default-cwd :initarg :cwd :reader default-cwd :initform nil)))

(defun make-local-compute-capability (&key cwd (version "0.1.0"))
  (make-instance 'local-compute-capability
                 :name :compute :version version :cwd cwd))

(defmethod run-command ((cap local-compute-capability) command &key)
  (multiple-value-bind (stdout stderr exit-code)
      (uiop:run-program command
                        :directory (default-cwd cap)
                        :output '(:string :stripped t)
                        :error-output '(:string :stripped t)
                        :ignore-error-status t)
    (list exit-code stdout stderr)))

(defmethod create-environment ((cap local-compute-capability) spec &key)
  (let ((dir (or (getf spec :directory)
                 (uiop:ensure-pathname
                  (format nil "/tmp/demiurge-env-~A/" (get-universal-time))
                  :ensure-directory t))))
    (ensure-directories-exist dir)
    (list :type :local :directory dir)))

(defmethod exec-in-environment ((cap local-compute-capability) env command &key)
  (let ((dir (getf env :directory)))
    (multiple-value-bind (stdout stderr exit-code)
        (uiop:run-program command
                          :directory dir
                          :output '(:string :stripped t)
                          :error-output '(:string :stripped t)
                          :ignore-error-status t)
      (list exit-code stdout stderr))))

(defmethod destroy-environment ((cap local-compute-capability) env &key)
  (let ((dir (getf env :directory)))
    (when (and dir (uiop:directory-exists-p dir))
      (uiop:delete-directory-tree (pathname dir) :validate t :if-does-not-exist :ignore)
      t)))
