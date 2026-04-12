(defpackage #:demiurge/src/capabilities/registry
  (:use #:cl)
  (:import-from #:alexandria #:when-let)
  (:import-from #:demiurge/src/blackboard/core
                #:blackboard #:blackboard-capabilities #:blackboard-lock)
  (:import-from #:demiurge/src/capabilities/protocol
                #:capability #:capability-name #:capability-version
                #:capability-description #:capability-operations
                #:capability-operation-name #:capability-operation-params
                #:capability-operation-returns #:capability-operation-doc)
  (:export #:register-capability #:unregister-capability
           #:get-capability #:list-capabilities #:capability-schema))

(in-package #:demiurge/src/capabilities/registry)

(defun register-capability (bb cap)
  (bt2:with-lock-held ((blackboard-lock bb))
    (setf (gethash (capability-name cap) (blackboard-capabilities bb)) cap))
  cap)

(defun unregister-capability (bb name)
  (bt2:with-lock-held ((blackboard-lock bb))
    (remhash name (blackboard-capabilities bb))))

(defun get-capability (bb name)
  (bt2:with-lock-held ((blackboard-lock bb))
    (gethash name (blackboard-capabilities bb))))

(defun list-capabilities (bb)
  (bt2:with-lock-held ((blackboard-lock bb))
    (let ((result nil))
      (maphash (lambda (name cap)
                 (push (list :name name
                             :version (capability-version cap)
                             :operations (mapcar #'capability-operation-name
                                                 (capability-operations cap)))
                       result))
               (blackboard-capabilities bb))
      result)))

(defun capability-schema (cap)
  (list :name (capability-name cap)
        :description (capability-description cap)
        :version (capability-version cap)
        :operations (mapcar (lambda (op)
                              (list :name (capability-operation-name op)
                                    :params (capability-operation-params op)
                                    :returns (capability-operation-returns op)
                                    :doc (capability-operation-doc op)))
                            (capability-operations cap))))
