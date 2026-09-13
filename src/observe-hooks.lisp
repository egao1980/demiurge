(in-package #:demiurge)

;;; Spans live on telemetry-protocol. Logs carry :trace-id / :span-id
;;; via log-protocol:with-context. Never emit spans through the logger.

(defun call-with-ksar-observe (ks thunk)
  "Span demiurge.ksar.execute around THUNK. Log with correlated ids."
  (tel:with-span ("demiurge.ksar.execute"
                  :attributes (list "demiurge.ks"
                                    (string (bb:ks-name ks))))
    (log:with-context (:trace-id (tel:current-trace-id)
                       :span-id (tel:current-span-id)
                       :ks (bb:ks-name ks))
      (when log:*log-backend*
        (log:info "ksar execute"))
      (funcall thunk))))

(defun call-with-agent-observe (agent thunk)
  "Span demiurge.agent.run around THUNK. Log with correlated ids."
  (tel:with-span ("demiurge.agent.run"
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
