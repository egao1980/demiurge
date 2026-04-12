(defpackage #:demiurge/src/introspection/object-registry
  (:use #:cl)
  (:export #:object-registry #:make-object-registry
           #:register-object #:lookup-object #:inspectable-p
           #:registry-capacity))

(in-package #:demiurge/src/introspection/object-registry)

(defclass object-registry ()
  ((storage :initform (make-hash-table :test 'eql) :reader registry-storage)
   (history :reader registry-history)
   (capacity :initarg :capacity :reader registry-capacity :initform 1000)
   (next-id :initform 1 :accessor registry-next-id)
   (position :initform 0 :accessor registry-position)
   (lock :initform (bt2:make-lock :name "object-registry") :reader registry-lock)))

(defmethod initialize-instance :after ((reg object-registry) &key)
  (setf (slot-value reg 'history)
        (make-array (registry-capacity reg) :initial-element nil)))

(defun make-object-registry (&key (capacity 1000))
  (make-instance 'object-registry :capacity capacity))

(defun inspectable-p (object)
  "NIL for primitives (number, string, symbol, character) -- inline them."
  (not (typep object '(or number string symbol character null))))

(defun register-object (registry object)
  "Register object, return its integer ID. Evicts oldest if full."
  (bt2:with-lock-held ((registry-lock registry))
    (let ((id (registry-next-id registry))
          (pos (registry-position registry)))
      ;; Evict old entry at this ring position
      (let ((old-id (aref (registry-history registry) pos)))
        (when old-id
          (remhash old-id (registry-storage registry))))
      ;; Store new
      (setf (gethash id (registry-storage registry)) object
            (aref (registry-history registry) pos) id
            (registry-next-id registry) (1+ id)
            (registry-position registry) (mod (1+ pos) (registry-capacity registry)))
      id)))

(defun lookup-object (registry id)
  "Look up object by ID. Returns (values object found-p)."
  (bt2:with-lock-held ((registry-lock registry))
    (gethash id (registry-storage registry))))
