(defpackage #:demiurge/src/utils/config
  (:use #:cl)
  (:export #:get-config #:load-config #:*config*))

(in-package #:demiurge/src/utils/config)

(defvar *config* (make-hash-table :test 'equal))

(defun get-config (key &optional default)
  "Get config value. Checks env var DEMIURGE_<KEY> first, then config hash."
  (let ((env-key (format nil "DEMIURGE_~A" (string-upcase (substitute #\_ #\- (string key))))))
    (or (uiop:getenv env-key)
        (gethash (string key) *config* default))))

(defun load-config (path)
  "Load JSON config file into *config*."
  (when (probe-file path)
    (let ((content (uiop:read-file-string path)))
      (setf *config* (yason:parse content :object-as :hash-table
                                          :object-key-fn #'identity))))
  *config*)
