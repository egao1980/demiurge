(defpackage #:demiurge/tests
  (:use #:cl #:rove #:demiurge)
  (:shadowing-import-from #:demiurge #:run-tests)
  (:local-nicknames (#:bb #:blackboard-protocol)
                    (#:cap #:capability-protocol)
                    (#:agent #:ai-agent-protocol)
                    (#:llm #:llm-protocol)
                    (#:conv #:conversation-protocol)
                    (#:steer #:steer-protocol)
                    (#:eval #:eval-protocol)
                    (#:task #:task-protocol)
                    (#:tel #:telemetry-protocol)))

(in-package #:demiurge/tests)
