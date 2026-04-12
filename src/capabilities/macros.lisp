(defpackage #:demiurge/src/capabilities/macros
  (:use #:cl)
  (:import-from #:demiurge/src/capabilities/protocol
                #:capability #:capability-operations #:make-capability-operation)
  (:export #:defcapability))

(in-package #:demiurge/src/capabilities/macros)

(defun parse-operation (op-form)
  "Parse (:operation name (params...) &key returns doc) into components."
  (destructuring-bind (op-tag op-name params &key returns (doc "")) op-form
    (declare (ignore op-tag))
    ;; Strip &key from params for the descriptor
    (let ((clean-params (remove-if (lambda (p) (and (symbolp p) (string= "&KEY" (symbol-name p))))
                                   params)))
      (values op-name clean-params returns doc))))

(defmacro defcapability (name doc &body operations)
  "Define a capability type. Generates a class and generic functions.
NAME is a keyword. DOC is a string.
OPERATIONS: (:operation op-name ((param type) ...) :returns type :doc \"...\")."
  (let* ((class-name (intern (format nil "~A-CAPABILITY"
                                     (string-upcase (symbol-name name)))))
         (gen-forms nil)
         (desc-forms nil))
    (dolist (op operations)
      (multiple-value-bind (op-name params returns op-doc) (parse-operation op)
        (let ((param-names (mapcar (lambda (p) (if (listp p) (car p) p)) params)))
          (push `(defgeneric ,op-name (cap ,@param-names &key)
                   (:documentation ,op-doc))
                gen-forms)
          (push `(make-capability-operation
                  :name ',op-name
                  :params ',params
                  :returns ',returns
                  :doc ,op-doc)
                desc-forms))))
    `(progn
       (defclass ,class-name (capability)
         ()
         (:default-initargs :name ,name :description ,doc))
       (defmethod capability-operations ((cap ,class-name))
         (list ,@(nreverse desc-forms)))
       ,@(nreverse gen-forms)
       ',class-name)))
