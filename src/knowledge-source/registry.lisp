(defpackage #:demiurge/src/knowledge-source/registry
  (:use #:cl)
  (:import-from #:demiurge/src/blackboard/core
                #:blackboard #:blackboard-ks-registry #:blackboard-lock)
  (:import-from #:demiurge/src/knowledge-source/protocol
                #:knowledge-source #:ks-name)
  (:export #:register-ks #:unregister-ks #:find-ks #:list-ks))

(in-package #:demiurge/src/knowledge-source/registry)

(defun register-ks (bb ks)
  (bt2:with-lock-held ((blackboard-lock bb))
    (setf (gethash (ks-name ks) (blackboard-ks-registry bb)) ks))
  ks)

(defun unregister-ks (bb name)
  (bt2:with-lock-held ((blackboard-lock bb))
    (remhash name (blackboard-ks-registry bb))))

(defun find-ks (bb name)
  (bt2:with-lock-held ((blackboard-lock bb))
    (gethash name (blackboard-ks-registry bb))))

(defun list-ks (bb)
  (bt2:with-lock-held ((blackboard-lock bb))
    (let ((result nil))
      (maphash (lambda (k v) (declare (ignore k)) (push v result))
               (blackboard-ks-registry bb))
      result)))
