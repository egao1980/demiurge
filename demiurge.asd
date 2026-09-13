(defsystem "demiurge"
  :version "0.3.0"
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
                            "sql-backend-sqlite3"
                            "log-backend-log4cl"
                            "telemetry-backend-otlp"))))
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

(defsystem "demiurge/improve"
  :version "0.3.0"
  :description "Self-improvement cycle for demiurge (versioned-ks + eval gates)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("demiurge" "schema-protocol")
  :serial t
  :pathname "src/improve"
  :components ((:file "package")
               (:file "versioned-ks")
               (:file "sandbox")
               (:file "promotion")
               (:file "cycle")))

(defsystem "demiurge/observe"
  :version "0.3.0"
  :description "Observability subsystem: span/metric taxonomy, health, profiles"
  :author "egao1980"
  :license "MIT"
  :depends-on ("demiurge" "telemetry-protocol" "log-protocol"
               "llm-protocol" "rag-protocol" "task-protocol")
  :serial t
  :pathname "src/observe"
  :components ((:file "package")
               (:file "taxonomy")
               (:file "logging")
               (:file "health")
               (:file "profiles")))

(defsystem "demiurge/tests"
  :depends-on ("demiurge" "demiurge/improve" "demiurge/observe" "llm-protocol"
               "event-backend-libuv" "sql-backend-sqlite3" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "helpers")
               (:file "protocol-test")
               (:file "restarts-test")
               (:file "config-test")
               (:file "persistence-test")
               (:file "improve-test")
               (:file "observe-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
