(in-package #:demiurge/observe)

;;; Structured log-protocol only. Never FORMAT/PRINT for product logs.
;;; Correlate with telemetry-protocol via :trace-id / :span-id.

(defmacro with-observe-log ((&rest fields) &body body)
  "Bind log-protocol context with the current span ids plus FIELDS."
  `(log:with-context (:trace-id (tel:current-trace-id)
                      :span-id (tel:current-span-id)
                      ,@fields)
     ,@body))

(defun %call-with-named-span (name attributes thunk)
  (tel:with-span (name :attributes attributes)
    (with-observe-log ()
      (funcall thunk))))

(defun call-with-task-step-observe (step-name thunk)
  "Span +SPAN-TASK-STEP+ around THUNK. STEP-NAME is a string/symbol attribute."
  (%call-with-named-span +span-task-step+
                         (list "demiurge.step" (string step-name))
                         thunk))

(defun call-with-ingest-observe (document-id thunk)
  "Span +SPAN-INGEST-DOCUMENT+ around THUNK."
  (%call-with-named-span +span-ingest-document+
                         (list "demiurge.document" (string document-id))
                         thunk))

(defun call-with-improve-cycle-observe (cycle-id thunk)
  "Span +SPAN-IMPROVE-CYCLE+ around THUNK."
  (%call-with-named-span +span-improve-cycle+
                         (list "demiurge.cycle-id" (string cycle-id))
                         thunk))

(defun log-section-write (key)
  "Board section write at debug with the section key."
  (when log:*log-backend*
    (with-observe-log (:section key)
      (log:debug "board section write"))))

(defun log-agenda-decision (message &rest fields)
  "KSAR agenda decision at info."
  (when log:*log-backend*
    (apply #'log:info message fields)))

(defun log-improve-verdict (verdict &key eval-run-id cycle-id)
  "Improvement-cycle verdict at info with eval-run id."
  (when log:*log-backend*
    (with-observe-log (:verdict verdict
                       :eval-run-id eval-run-id
                       :cycle-id cycle-id)
      (log:info "improvement-cycle verdict"))))
