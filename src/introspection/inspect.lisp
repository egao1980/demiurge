(defpackage #:demiurge/src/introspection/inspect
  (:use #:cl)
  (:import-from #:demiurge/src/introspection/object-registry
                #:object-registry #:register-object #:lookup-object #:inspectable-p)
  (:export #:inspect-object #:value-repr))

(in-package #:demiurge/src/introspection/inspect)

(defun value-repr (registry value &key seen)
  "Represent a value for JSON output. Primitives inline, complex as object-ref."
  (cond
    ((null value) (make-repr :null nil))
    ((not (inspectable-p value))
     (make-repr :inline value))
    ((and seen (gethash value seen))
     (make-repr :circular-ref (gethash value seen)))
    (t
     (let ((id (register-object registry value)))
       (when seen (setf (gethash value seen) id))
       (make-repr :object-ref id
                  :summary (format nil "~A" (type-of value))
                  :type (format nil "~A" (type-of value)))))))

(defun make-repr (kind value &key summary type)
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash "kind" h) (string-downcase (symbol-name kind)))
    (case kind
      (:inline (setf (gethash "value" h) (if (symbolp value) (symbol-name value) value)
                     (gethash "type" h) (format nil "~A" (type-of value))))
      (:null (setf (gethash "value" h) :null))
      (:object-ref (setf (gethash "id" h) value
                         (gethash "summary" h) summary
                         (gethash "type" h) type))
      (:circular-ref (setf (gethash "ref_id" h) value
                           (gethash "summary" h) "circular reference")))
    h))

(defun inspect-object (registry id &key (max-depth 1) (max-elements 50))
  "Inspect object by ID. Returns hash-table (JSON-serializable)."
  (multiple-value-bind (obj found) (lookup-object registry id)
    (unless found
      (return-from inspect-object nil))
    (let ((seen (make-hash-table :test 'eq)))
      (setf (gethash obj seen) id)
      (%inspect-impl registry obj max-depth max-elements seen))))

(defun %inspect-impl (registry obj depth max-elements seen)
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash "type" h) (format nil "~A" (type-of obj)))
    (cond
      ;; List
      ((consp obj)
       (setf (gethash "kind" h) "list")
       (let ((elements nil) (count 0) (tail obj))
         (loop while (and (consp tail) (< count max-elements)) do
           (push (if (> depth 0)
                     (sub-inspect registry (car tail) (1- depth) max-elements seen)
                     (value-repr registry (car tail) :seen seen))
                 elements)
           (incf count)
           (setf tail (cdr tail)))
         (setf (gethash "elements" h) (nreverse elements)
               (gethash "length" h) (if (null tail) count (format nil "~A+" count)))
         (when tail
           (setf (gethash "dotted_tail" h) (value-repr registry tail :seen seen)))))

      ;; Vector/array
      ((arrayp obj)
       (setf (gethash "kind" h) "array"
             (gethash "dimensions" h) (array-dimensions obj)
             (gethash "element_type" h) (format nil "~A" (array-element-type obj)))
       (let ((elts nil))
         (dotimes (i (min (array-total-size obj) max-elements))
           (push (if (> depth 0)
                     (sub-inspect registry (row-major-aref obj i) (1- depth) max-elements seen)
                     (value-repr registry (row-major-aref obj i) :seen seen))
                 elts))
         (setf (gethash "elements" h) (nreverse elts))))

      ;; Hash-table
      ((hash-table-p obj)
       (setf (gethash "kind" h) "hash-table"
             (gethash "test" h) (format nil "~A" (hash-table-test obj))
             (gethash "count" h) (hash-table-count obj))
       (let ((entries nil) (count 0))
         (block done
           (maphash (lambda (k v)
                      (when (>= count max-elements) (return-from done))
                      (push (list (if (> depth 0)
                                      (sub-inspect registry k (1- depth) max-elements seen)
                                      (value-repr registry k :seen seen))
                                  (if (> depth 0)
                                      (sub-inspect registry v (1- depth) max-elements seen)
                                      (value-repr registry v :seen seen)))
                            entries)
                      (incf count))
                    obj))
         (setf (gethash "entries" h) (nreverse entries))))

      ;; Function
      ((functionp obj)
       (setf (gethash "kind" h) "function"
             (gethash "name" h) (format nil "~A"
                                        (or #+sbcl (sb-kernel:%fun-name obj)
                                            "anonymous")))
       #+sbcl
       (handler-case
           (progn
             (require :sb-introspect)
             (setf (gethash "lambda_list" h)
                   (format nil "~A" (funcall (find-symbol "FUNCTION-LAMBDA-LIST" "SB-INTROSPECT") obj))))
         (error () nil)))

      ;; CLOS instance
      ((typep obj 'standard-object)
       (setf (gethash "kind" h) "instance"
             (gethash "class" h) (format nil "~A" (class-name (class-of obj))))
       (let ((slots nil))
         (dolist (slot-def (closer-mop:class-slots (class-of obj)))
           (let* ((slot-name (closer-mop:slot-definition-name slot-def))
                  (bound (slot-boundp obj slot-name)))
             (push (list (symbol-name slot-name)
                         (if bound
                             (if (> depth 0)
                                 (sub-inspect registry (slot-value obj slot-name)
                                              (1- depth) max-elements seen)
                                 (value-repr registry (slot-value obj slot-name) :seen seen))
                             (make-repr :inline "UNBOUND")))
                   slots)))
         (setf (gethash "slots" h) (nreverse slots))))

      ;; Structure
      ((typep obj 'structure-object)
       (setf (gethash "kind" h) "structure"
             (gethash "class" h) (format nil "~A" (type-of obj)))
       #+sbcl
       (let ((slots nil))
         (dolist (slot-def (closer-mop:class-slots (class-of obj)))
           (let ((slot-name (closer-mop:slot-definition-name slot-def)))
             (push (list (symbol-name slot-name)
                         (if (> depth 0)
                             (sub-inspect registry (slot-value obj slot-name)
                                          (1- depth) max-elements seen)
                             (value-repr registry (slot-value obj slot-name) :seen seen)))
                   slots)))
         (setf (gethash "slots" h) (nreverse slots))))

      ;; Fallback
      (t
       (setf (gethash "kind" h) "other"
             (gethash "summary" h) (format nil "~A" obj))))
    h))

(defun sub-inspect (registry obj depth max-elements seen)
  "Inspect sub-object: primitives inline, complex recurse/ref."
  (if (inspectable-p obj)
      (if (gethash obj seen)
          (make-repr :circular-ref (gethash obj seen))
          (progn
            (let ((id (register-object registry obj)))
              (setf (gethash obj seen) id))
            (%inspect-impl registry obj depth max-elements seen)))
      (value-repr registry obj :seen seen)))
