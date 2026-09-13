(in-package #:demiurge)

;;; Spans live on telemetry-protocol. Logs carry :trace-id / :span-id
;;; via log-protocol:with-context. Never emit spans through the logger.
;;; Names match demiurge/observe taxonomy constants.

(defun %observe-span-name (which fallback)
  (let* ((pkg (find-package '#:demiurge/observe))
         (sym (and pkg (find-symbol which pkg))))
    (if (and sym (boundp sym))
        (symbol-value sym)
        fallback)))

(defun %observe-record (name &rest args)
  (let* ((pkg (find-package '#:demiurge/observe))
         (fn (and pkg (find-symbol name pkg))))
    (when (and fn (fboundp fn))
      (apply fn args))))

(defun call-with-ksar-observe (ks thunk)
  "Span demiurge.ksar.execute around THUNK. Log with correlated ids."
  (let ((start (get-internal-real-time)))
    (tel:with-span ((%observe-span-name "+SPAN-KSAR-EXECUTE+"
                                        "demiurge.ksar.execute")
                    :attributes (list "demiurge.ks"
                                      (string (bb:ks-name ks))))
      (log:with-context (:trace-id (tel:current-trace-id)
                         :span-id (tel:current-span-id)
                         :ks (bb:ks-name ks))
        (when log:*log-backend*
          (log:info "ksar execute"))
        (prog1 (funcall thunk)
          (%observe-record "RECORD-KSAR-DURATION" ks start))))))

(defun call-with-agent-observe (agent thunk)
  "Span demiurge.agent.run around THUNK. Log with correlated ids."
  (tel:with-span ((%observe-span-name "+SPAN-AGENT-RUN+"
                                      "demiurge.agent.run")
                  :attributes (list "demiurge.agent"
                                    (if (and agent (agent:ai-agent-p agent))
                                        (agent:ai-agent-name agent)
                                        "agent")))
    (log:with-context (:trace-id (tel:current-trace-id)
                       :span-id (tel:current-span-id)
                       :agent (and agent (agent:ai-agent-name agent)))
      (when log:*log-backend*
        (log:info "agent run"))
      (funcall thunk))))
