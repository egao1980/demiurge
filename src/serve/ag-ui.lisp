(in-package #:demiurge/serve)

(defun run-expert-as-ag-ui-events (domain prompt &key blackboard
                                                     thread-id run-id
                                                     (watch '(:result :feedback-id))
                                                     timeout)
  "Collect AG-UI events (camel key-style) for a board run of DOMAIN."
  (check-type domain expert-domain)
  (let ((thread (or thread-id "thread-expert")))
    (with-request-session (nil :transport (or *request-transport* :http)
                               :conversation-id thread)
      (let ((board (or blackboard (bb:make-blackboard))))
        (unless (bb:list-watchers board)
          (register-expert-ks board domain))
        (wire.ag-ui:run-board-as-ag-ui-events
         board
         (lambda (b)
           (bb:write-section b :prompt prompt)
           (bb:run-scheduler (bb:find-root-bb b)
                             :until-empty t
                             :timeout (or timeout 10))
           (unless (bb:section-bound-p b :feedback-id)
             (bb:write-section b :feedback-id (make-feedback-id))))
         :watch watch
         :thread-id thread
         :run-id (or run-id (format nil "run-~a" (random 100000000))))))))

(defun make-expert-ag-ui-agent (domain)
  "AG-UI agent whose handler runs DOMAIN via blackboard-wire/ag-ui."
  (ag-ui:make-ag-ui-agent
   :name (expert-name domain)
   :handler (lambda (input)
              (run-expert-as-ag-ui-events
               domain
               (ag-ui:last-user-text input)
               :thread-id (or (ag-ui:run-agent-input-thread-id input)
                              "thread-expert")
               :run-id (or (ag-ui:run-agent-input-run-id input)
                           (format nil "run-~a" (random 100000000)))))))
