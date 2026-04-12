(defpackage #:demiurge/src/blackboard/core
  (:use #:cl)
  (:import-from #:alexandria #:when-let)
  (:export #:blackboard #:make-blackboard
           #:read-section #:write-section #:remove-section
           #:list-sections #:blackboard-lock #:blackboard-sections
           #:blackboard-notify-fn
           #:blackboard-capabilities #:blackboard-workspaces
           #:blackboard-ks-registry))

(in-package #:demiurge/src/blackboard/core)

(defclass blackboard ()
  ((sections :initform (make-hash-table :test 'eq) :reader blackboard-sections)
   (lock :initform (bt2:make-lock :name "blackboard") :reader blackboard-lock)
   (notify-fn :initarg :notify-fn :accessor blackboard-notify-fn :initform nil
              :documentation "Called with (key old-value new-value) on section change.")
   (capabilities :initform (make-hash-table :test 'eq) :accessor blackboard-capabilities)
   (workspaces :initform (make-hash-table :test 'equal) :accessor blackboard-workspaces)
   (ks-registry :initform (make-hash-table :test 'equal) :accessor blackboard-ks-registry)))

(defun make-blackboard (&key notify-fn)
  (make-instance 'blackboard :notify-fn notify-fn))

(defgeneric read-section (bb key &key default)
  (:documentation "Read a section from the blackboard."))

(defgeneric write-section (bb key value &key merge-fn)
  (:documentation "Write a section to the blackboard. Fires section-changed event."))

(defgeneric remove-section (bb key)
  (:documentation "Remove a section from the blackboard."))

(defmethod read-section ((bb blackboard) key &key default)
  (bt2:with-lock-held ((blackboard-lock bb))
    (gethash key (blackboard-sections bb) default)))

(defmethod write-section ((bb blackboard) key value &key merge-fn)
  (bt2:with-lock-held ((blackboard-lock bb))
    (let ((old (gethash key (blackboard-sections bb))))
      (setf (gethash key (blackboard-sections bb))
            (if (and merge-fn old)
                (funcall merge-fn old value)
                value))
      (when-let (fn (blackboard-notify-fn bb))
        (funcall fn key old value))
      value)))

(defmethod remove-section ((bb blackboard) key)
  (bt2:with-lock-held ((blackboard-lock bb))
    (remhash key (blackboard-sections bb))))

(defgeneric list-sections (bb)
  (:documentation "List all section keys."))

(defmethod list-sections ((bb blackboard))
  (bt2:with-lock-held ((blackboard-lock bb))
    (let ((keys nil))
      (maphash (lambda (k v) (declare (ignore v)) (push k keys))
               (blackboard-sections bb))
      keys)))
