(in-package #:demiurge)

(define-condition demiurge-error (error)
  ((message :initarg :message :reader demiurge-error-message :initform nil)
   (cause :initarg :cause :reader demiurge-error-cause :initform nil))
  (:report (lambda (c s)
             (format s "demiurge error~@[: ~A~]~@[: ~A~]"
                     (demiurge-error-message c)
                     (demiurge-error-cause c)))))

(define-condition unknown-expert (demiurge-error)
  ((name :initarg :name :reader unknown-expert-name))
  (:report (lambda (c s)
             (format s "unknown expert ~S~@[: ~A~]"
                     (unknown-expert-name c)
                     (demiurge-error-message c)))))

(define-condition invalid-expert (demiurge-error)
  ()
  (:report (lambda (c s)
             (format s "invalid expert-domain~@[: ~A~]"
                     (demiurge-error-message c)))))

(define-condition missing-event-backend (demiurge-error)
  ()
  (:report (lambda (c s)
             (format s "no event-protocol backend for run-ai-agent~@[: ~A~]"
                     (demiurge-error-message c)))))

(define-condition compute-denied (demiurge-error)
  ()
  (:report (lambda (c s)
             (format s "operation requires a granted :compute capability~@[: ~A~]"
                     (demiurge-error-message c)))))

(define-condition persistence-error (demiurge-error)
  ()
  (:report (lambda (c s)
             (format s "demiurge persistence error~@[: ~A~]"
                     (demiurge-error-message c)))))

(defun call-with-demiurge-restarts (thunk)
  "Establish RETRY / USE-VALUE around THUNK."
  (tagbody
   :retry
     (return-from call-with-demiurge-restarts
       (restart-case (funcall thunk)
         (retry ()
           :report "Retry the demiurge operation"
           (go :retry))
         (use-value (value)
           :report "Use a supplied value instead"
           :interactive (lambda ()
                          (format *query-io* "Value to use: ")
                          (force-output *query-io*)
                          (list (read *query-io*)))
           value)))))

(defmacro with-demiurge-restarts (&body body)
  `(call-with-demiurge-restarts (lambda () ,@body)))

(defun invoke-retry (&optional condition)
  (let ((r (find-restart 'retry condition)))
    (when r (invoke-restart r))))

(defun invoke-use-value (value &optional condition)
  (let ((r (find-restart 'use-value condition)))
    (when r (invoke-restart r value))))

(defun invoke-skip (&optional condition)
  (let ((r (find-restart 'skip condition)))
    (when r (invoke-restart r))))
