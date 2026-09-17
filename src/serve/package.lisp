(defpackage #:demiurge/serve
  (:use #:cl #:demiurge)
  (:nicknames #:stack-demiurge.serve)
  (:local-nicknames (#:bb #:blackboard-protocol)
                    (#:bbj #:blackboard-journal)
                    (#:cap #:capability-protocol)
                    (#:steer #:steer-protocol)
                    (#:eval #:eval-protocol)
                    (#:mcp #:mcp-protocol)
                    (#:a2a #:a2a-protocol)
                    (#:ag-ui #:ag-ui-protocol)
                    (#:wire #:blackboard-wire)
                    (#:wire.mcp #:blackboard-wire/mcp)
                    (#:wire.a2a #:blackboard-wire/a2a)
                    (#:wire.ag-ui #:blackboard-wire/ag-ui)
                    (#:mcp.stdio #:mcp-backend-stdio)
                    (#:mcp.http #:mcp-backend-streamable-http)
                    (#:a2a.rpc #:a2a-backend-jsonrpc)
                    (#:ag-ui.sse #:ag-ui-backend-sse))
  (:export
   #:serve-error
   #:invalid-feedback
   #:request-too-large
   #:*max-request-bytes*
   #:loopback-address-p
   #:check-serve-security
   #:*readyz-fn*
   #:domain-ready-p
   #:readyz-ok-p

   #:make-feedback-id
   #:record-feedback
   #:handle-feedback-event
   #:make-record-feedback-tool

   #:ask-expert
   #:make-expert-mcp-server
   #:expert-agent-card
   #:make-expert-a2a-agent
   #:run-expert-as-a2a-task
   #:make-expert-ag-ui-agent
   #:run-expert-as-ag-ui-events

   #:render-expert-transcript

   #:make-expert-app
   #:serve-expert
   #:serve-session
   #:serve-session-p
   #:serve-session-domain
   #:serve-session-app
   #:serve-session-mcp
   #:serve-session-transports)
  (:documentation
   "Clack + wire adapters: MCP / A2A / AG-UI over an expert-domain."))

(in-package #:demiurge/serve)

(define-condition serve-error (demiurge-error)
  ()
  (:report (lambda (c s)
             (format s "demiurge serve error~@[: ~A~]"
                     (demiurge-error-message c)))))

(defparameter *max-request-bytes* (* 256 1024)
  "Hard cap on served HTTP request bodies (feedback and other POST routes).")

(define-condition invalid-feedback (serve-error)
  ()
  (:report (lambda (c s)
             (format s "invalid feedback~@[: ~A~]"
                     (demiurge-error-message c)))))

(define-condition request-too-large (serve-error)
  ((limit :initarg :limit :reader request-too-large-limit :initform nil)
   (size :initarg :size :reader request-too-large-size :initform nil))
  (:report (lambda (c s)
             (format s "request body exceeds ~A bytes~@[: ~A~]"
                     (or (request-too-large-limit c) *max-request-bytes*)
                     (demiurge-error-message c)))))
