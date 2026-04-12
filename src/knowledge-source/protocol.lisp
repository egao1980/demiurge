(defpackage #:demiurge/src/knowledge-source/protocol
  (:use #:cl)
  (:export #:knowledge-source #:ks-name #:ks-version #:ks-priority
           #:ks-precondition #:ks-execute #:ks-postcondition))

(in-package #:demiurge/src/knowledge-source/protocol)

(defclass knowledge-source ()
  ((name :initarg :name :reader ks-name)
   (version :initarg :version :reader ks-version :initform "0.1.0")
   (priority :initarg :priority :accessor ks-priority :initform 0)))

(defgeneric ks-precondition (ks blackboard)
  (:documentation "Return T if this KS should activate given current blackboard state.")
  (:method ((ks knowledge-source) bb)
    (declare (ignore bb))
    t))

(defgeneric ks-execute (ks blackboard)
  (:documentation "Run the KS, reading/writing blackboard sections. Returns result."))

(defgeneric ks-postcondition (ks blackboard result)
  (:documentation "Validate and write results back to blackboard.")
  (:method ((ks knowledge-source) bb result)
    (declare (ignore bb result))
    t))
