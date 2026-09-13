(defsystem "demiurge"
  :version "0.2.0"
  :description "Self-improving expert-system core for cl-stack (defexpert + agent-ks + controller)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("blackboard-protocol"
               "blackboard-journal"
               "capability-protocol"
               "ai-agent-protocol"
               "ai-agent-protocol/mcp"
               "conversation-protocol"
               "conversation-backend-sql"
               "steer-protocol"
               "eval-protocol"
               "event-protocol"
               "log-protocol"
               "rag-protocol"
               "rag-backend-text"
               "rag-backend-memory"
               "rag-backend-sql"
               "llm-protocol"
               "llm-protocol/router"
               "task-protocol"
               "task-backend-sql"
               "sql-protocol"
               "telemetry-protocol"
               "cl-stack-config")
  :properties (:cl-repo
               (:ci (:with ("event-backend-libuv"
                            "sql-backend-sqlite3"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "config")
               (:file "observe-hooks")
               (:file "persistence")
               (:file "profile")
               (:file "domain")
               (:file "tools")
               (:file "agent-ks")
               (:file "controller")
               (:file "echo-expert" :pathname "../examples/echo-expert")
               (:file "cl-dev-expert" :pathname "../examples/cl-dev-expert"))
  :in-order-to ((test-op (test-op "demiurge/tests"))))

(defsystem "demiurge/tests"
  :depends-on ("demiurge" "llm-protocol" "event-backend-libuv"
               "sql-backend-sqlite3" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "helpers")
               (:file "protocol-test")
               (:file "restarts-test")
               (:file "config-test")
               (:file "persistence-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
