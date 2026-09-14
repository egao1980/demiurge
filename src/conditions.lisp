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

(define-condition expert-config-error (invalid-expert)
  ((path :initarg :path :reader expert-config-error-path :initform nil)
   (issues :initarg :issues :reader expert-config-error-issues :initform nil))
  (:report (lambda (c s)
             (format s "expert.toml error~@[ (~A)~]~@[: ~A~]"
                     (expert-config-error-path c)
                     (demiurge-error-message c)))))

(define-condition unknown-expert-config-key (expert-config-error)
  ((key :initarg :key :reader unknown-expert-config-key-name :initform nil)
   (valid-keys :initarg :valid-keys :reader unknown-expert-config-valid-keys
               :initform nil)
   (section :initarg :section :reader unknown-expert-config-section
            :initform nil))
  (:report (lambda (c s)
             (format s "unknown expert.toml key ~S~@[ in ~A~]; valid keys: ~{~A~^, ~}~@[: ~A~]"
                     (unknown-expert-config-key-name c)
                     (unknown-expert-config-section c)
                     (unknown-expert-config-valid-keys c)
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

(define-condition capability-denied (demiurge-error)
  ((capability :initarg :capability :reader capability-denied-capability
               :initform nil)
   (operation :initarg :operation :reader capability-denied-operation
              :initform nil)
   (principal :initarg :principal :reader capability-denied-principal
              :initform nil)
   (tenant :initarg :tenant :reader capability-denied-tenant
           :initform nil))
  (:report (lambda (c s)
             (format s "capability denied~@[ for ~S~]~@[ on ~S~]~@[ (principal ~S)~]~@[: ~A~]"
                     (capability-denied-operation c)
                     (let ((cap (capability-denied-capability c)))
                       (and cap (ignore-errors (cap:capability-name cap))))
                     (capability-denied-principal c)
                     (demiurge-error-message c)))))

(define-condition tenant-isolation-error (demiurge-error)
  ((expected :initarg :expected :reader tenant-isolation-expected
             :initform nil)
   (actual :initarg :actual :reader tenant-isolation-actual
           :initform nil)
   (reference :initarg :reference :reader tenant-isolation-reference
              :initform nil))
  (:report (lambda (c s)
             (format s "tenant isolation: reference ~S is tenant ~S, expected ~S~@[: ~A~]"
                     (tenant-isolation-reference c)
                     (tenant-isolation-actual c)
                     (tenant-isolation-expected c)
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
