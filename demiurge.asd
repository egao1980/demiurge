(defsystem "demiurge"
  :version "0.3.10"
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
               (:version "task-protocol" "0.2.0")
               "task-backend-sql"
               "sql-protocol"
               "telemetry-protocol"
               "cl-stack-config"
               "cl-stack-oauth2"
               "cl-stack-jwt"
               "ldap-protocol")
  :properties (:cl-repo
               (:provides ("demiurge"
                           "demiurge/improve"
                           "demiurge/observe"
                           "demiurge/serve"
                           "demiurge/ingest"
                           "demiurge/workflows"
                           "demiurge/bundle"
                           "demiurge/cli")
                :ci (:with ("event-backend-libuv"
                            "sql-backend-sqlite3"
                            "log-backend-log4cl"
                            "telemetry-backend-otlp"
                            "crypto-backend-ironclad"
                            "json-backend-jzon"
                            "toml-backend-tomlet"
                            "cli-protocol"
                            "cli-backend-clingon"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "config")
               (:file "observe-hooks")
               (:file "persistence")
               (:file "profile")
               (:file "profile-corporate")
               (:file "domain")
               (:file "tools")
               (:file "agent-ks")
               (:file "controller")
               (:file "echo-expert" :pathname "../examples/echo-expert")
               (:file "cl-dev-expert" :pathname "../examples/cl-dev-expert"))
  :in-order-to ((test-op (test-op "demiurge/tests"))))

(defsystem "demiurge/improve"
  :version "0.3.10"
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
  :version "0.3.10"
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
  :version "0.3.10"
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
  :version "0.3.10"
  :description "Durable corpus ingest for demiurge (file / IMAP / object-store)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("demiurge"
               "doc-extract-protocol"
               "object-store-protocol"
               "mail-protocol"
               "mime-protocol"
               "cl-stack-pathlib"
               "rag-protocol"
               "rag-backend-text")
  :serial t
  :pathname "src/ingest"
  :components ((:file "package")
               (:file "conditions")
               (:file "sources")
               (:file "pipeline")))

(defsystem "demiurge/workflows"
  :version "0.3.10"
  :description "Durable project workflows and deep-research fan-out"
  :author "egao1980"
  :license "MIT"
  :depends-on ("demiurge"
               "websearch-protocol"
               "doc-extract-protocol"
               "schema-protocol"
               "eval-protocol"
               "rag-protocol"
               "llm-protocol"
               "llm-protocol/schema"
               "llm-protocol/router"
               "task-protocol"
               "mcp-protocol"
               "cl-stack-pathlib")
  :serial t
  :pathname "src/workflows"
  :components ((:file "package")
               (:file "reporting")
               (:file "project")
               (:file "workspace")
               (:file "tree")
               (:file "index")
               (:file "deep-research")))

(defsystem "demiurge/bundle"
  :version "0.3.10"
  :description "Expert-bundle packaging and distribution (OCI layout + install)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("demiurge"
               "demiurge/ingest"
               "cl-stack-pathlib"
               "schema-protocol"
               "toml-protocol"
               "steer-protocol"
               "eval-protocol"
               "task-protocol"
               "doc-extract-protocol"
               "llm-protocol"
               "websearch-protocol"
               "rag-protocol"
               "capability-protocol"
               "blackboard-protocol"
               "ai-agent-protocol")
  :serial t
  :pathname "src/bundle"
  :components ((:file "package")
               (:file "manifest")
               (:file "pack")
               (:file "install")
               (:file "expert-config")))

(defsystem "demiurge/cli"
  :version "0.3.10"
  :description "demiurge command-line entrypoint (cli-protocol + clingon)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("demiurge"
               "demiurge/serve"
               "demiurge/ingest"
               "demiurge/improve"
               "demiurge/workflows"
               "demiurge/bundle"
               "cli-protocol"
               "cli-backend-clingon"
               "websearch-protocol")
  :build-operation asdf:program-op
  :build-pathname "demiurge"
  :entry-point "demiurge/cli:main"
  :serial t
  :pathname "src/cli"
  :components ((:file "package")
               (:file "command")))

(defsystem "demiurge/tests"
  :depends-on ("demiurge" "demiurge/improve" "demiurge/observe"
               "demiurge/serve" "demiurge/ingest" "demiurge/workflows"
               "demiurge/bundle" "demiurge/cli"
               "llm-protocol" "llm-protocol/schema" "llm-protocol-openai"
               "http-backend-dexador"
               "http-backend-async"
               "event-backend-libuv"
               "sql-backend-sqlite3"
               "crypto-backend-ironclad"
               "json-backend-jzon"
               "toml-backend-tomlet"
               "rag-backend-hybrid"
               "rove")
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
               (:file "ingest-test")
               (:file "workflows-test")
               (:file "bundle-test")
               (:file "expert-config-test")
               (:file "cli-test")
               (:file "corporate-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
