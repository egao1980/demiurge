(in-package #:demiurge/observe)

;;; Personal = stream-log + in-memory recording tracer + DUMP-OBSERVABILITY.
;;; Corporate = log4cl + OTLP when those systems load, else the same stub
;;; stack (tests always assert against the recording tracer).

(defvar *observability-profile* nil
  "Keyword :personal or :corporate after APPLY-*-OBSERVABILITY.")

(defun %try-load-system (name)
  (or (asdf:component-loaded-p name)
      (ignore-errors (asdf:load-system name :verbose nil) t)))

(defun %apply-redaction (provider)
  (let ((policy (tel:make-default-redaction-policy)))
    (setf tel:*redaction-policy* policy)
    (when (and provider (tel:tracer-provider-p provider))
      (setf (tel:tracer-provider-redaction-policy provider) policy))
    policy))

(defun apply-personal-observability (&key (stream *standard-output*))
  "stream-log-backend + recording tracer. Registers taxonomy instruments."
  (let ((backend (tel:use-recording-telemetry)))
    (%apply-redaction backend)
    (register-taxonomy-instruments backend)
    (log:configure :backend (log:make-stream-log-backend :stream stream)
                   :level :info
                   :layout :text)
    (setf *observability-profile* :personal)
    backend))

(defun %try-log4cl-backend (&key stream)
  (when (%try-load-system "log-backend-log4cl")
    (let ((fn (find-symbol "MAKE-LOG4CL-BACKEND" :log-backend-log4cl)))
      (when (and fn (fboundp fn))
        (if stream
            (funcall fn :stream stream)
            (funcall fn))))))

(defun %try-otlp-backend (&key endpoint service-name)
  (when (%try-load-system "telemetry-backend-otlp")
    (let ((fn (find-symbol "MAKE-OTLP-TELEMETRY-BACKEND"
                          :telemetry-backend-otlp)))
      (when (and fn (fboundp fn))
        (funcall fn :endpoint (or endpoint "http://127.0.0.1:4318")
                    :service-name (or service-name "demiurge"))))))

(defun apply-corporate-observability (&key endpoint stream
                                        (service-name "demiurge")
                                        force-recording)
  "log4cl + OTLP when loadable. FORCE-RECORDING keeps the recording tracer
   (CI / tests). Redaction is applied before export when the API exists."
  (let* ((log-backend (unless force-recording
                        (%try-log4cl-backend :stream stream)))
         (otlp (unless force-recording
                 (%try-otlp-backend :endpoint endpoint
                                    :service-name service-name)))
         (backend (or otlp (tel:use-recording-telemetry))))
    (setf tel:*telemetry-backend* backend
          tel:*tracer-provider* (and otlp backend))
    (%apply-redaction backend)
    (register-taxonomy-instruments backend)
    (setf log:*log-backend*
          (or log-backend
              (log:make-stream-log-backend :stream (or stream *standard-output*))))
    (setf *observability-profile* :corporate)
    backend))

(defun %span-sexp (span)
  (list :name (tel:telemetry-span-name span)
        :trace-id (tel:telemetry-span-trace-id span)
        :span-id (tel:telemetry-span-id span)
        :parent-id (tel:telemetry-span-parent-id span)
        :status (tel:telemetry-span-status span)
        :attributes (copy-list (tel:telemetry-span-attributes span))))

(defun %metric-sexp (metric)
  (list :name (tel:telemetry-metric-name metric)
        :value (tel:telemetry-metric-value metric)
        :unit (tel:telemetry-metric-unit metric)
        :kind (tel:telemetry-metric-kind metric)
        :attributes (copy-list (tel:telemetry-metric-attributes metric))))

(defun dump-observability (&key since)
  "REPL helper: spans + metrics snapshot as sexp.
   SINCE is a universal-time; spans started before it are omitted."
  (let* ((backend tel:*telemetry-backend*)
         (recording (typep backend 'tel:recording-telemetry-backend))
         (spans (and recording (copy-list (tel:recorded-spans backend))))
         (metrics (and recording (copy-list (tel:recorded-metrics backend))))
         (since-ns (and since
                        (* (- since 2208988800) 1000000000))))
    (when (and since-ns spans)
      (setf spans
            (remove-if (lambda (span)
                         (let ((t0 (tel:telemetry-span-start-unix-ns span)))
                           (and t0 (< t0 since-ns))))
                       spans)))
    (list :profile *observability-profile*
          :since since
          :spans (mapcar #'%span-sexp spans)
          :metrics (mapcar #'%metric-sexp metrics))))
