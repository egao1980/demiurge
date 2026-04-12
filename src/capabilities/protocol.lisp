(defpackage #:demiurge/src/capabilities/protocol
  (:use #:cl)
  (:export #:capability #:capability-name #:capability-version
           #:capability-description #:capability-operations
           #:capability-operation #:make-capability-operation
           #:capability-operation-name #:capability-operation-params
           #:capability-operation-returns #:capability-operation-doc
           #:invoke-operation))

(in-package #:demiurge/src/capabilities/protocol)

(defclass capability ()
  ((name :initarg :name :reader capability-name)
   (version :initarg :version :reader capability-version :initform "0.1.0")
   (description :initarg :description :reader capability-description :initform "")))

(defgeneric capability-operations (cap)
  (:documentation "Return list of operation descriptors for this capability.")
  (:method ((cap capability)) nil))

(defstruct capability-operation
  (name nil :type symbol)
  (params nil :type list)
  (returns nil)
  (doc "" :type string))

(defgeneric invoke-operation (cap op-name &rest args)
  (:documentation "Dynamically invoke an operation on a capability."))

(defmethod invoke-operation ((cap capability) op-name &rest args)
  (let ((method (find-method #'invoke-operation nil
                             (list (class-of cap) (eql-specializer op-name))
                             nil)))
    (if method
        (apply method cap op-name args)
        (error "Operation ~A not found on capability ~A" op-name (capability-name cap)))))

(defun eql-specializer (value)
  #+sbcl (sb-mop:intern-eql-specializer value)
  #-sbcl (error "eql-specializer not implemented for this CL"))
