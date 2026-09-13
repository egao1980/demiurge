(defsystem "demiurge"
  :version "0.3.1"
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
               (:provides ("demiurge"
                           "demiurge/improve"
                           "demiurge/observe"
                           "demiurge/serve"
                           "demiurge/ingest")
                :ci (:with ("event-backend-libuv"
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
  :version "0.3.1"
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
  :version "0.3.1"
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

(defsystem "demiurge/serve"
  :version "0.3.1"
  :description "Serve an expert-domain over MCP / A2A / AG-UI"
  :author "egao1980"
  :license "MIT"
  :depends-on ("demiurge"
               "blackboard-wire"
               "blackboard-wire/mcp"
               "blackboard-wire/a2a"
               "blackboard-wire/ag-ui"
               "mcp-backend-stdio"
               "mcp-backend-streamable-http"
               "a2a-backend-jsonrpc"
               "ag-ui-backend-sse")
  :serial t
  :pathname "src/serve"
  :components ((:file "package")
               (:file "feedback")
               (:file "mcp")
               (:file "a2a")
               (:file "ag-ui")
               (:file "tui")
               (:file "app")))

(defsystem "demiurge/ingest"
  :version "0.3.1"
  :description "Durable corpus ingest for demiurge (file / IMAP / object-store)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("demiurge"
               "doc-extract-protocol"
               "object-store-protocol"
               "mail-protocol"
               "cl-stack-pathlib"
               "rag-protocol"
               "rag-backend-text")
  :serial t
  :pathname "src/ingest"
  :components ((:file "package")
               (:file "sources")
               (:file "pipeline")))

(defsystem "demiurge/tests"
  :depends-on ("demiurge" "demiurge/improve" "demiurge/observe"
               "demiurge/serve" "demiurge/ingest"
               "llm-protocol" "event-backend-libuv"
               "sql-backend-sqlite3" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "helpers")
               (:file "protocol-test")
               (:file "restarts-test")
               (:file "config-test")
               (:file "persistence-test")
               (:file "improve-test")
               (:file "observe-test")
               (:file "serve-test")
               (:file "ingest-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
