(defpackage #:demiurge-bootstrap/bootstrap/main
  (:nicknames #:demiurge-bootstrap)
  (:use #:cl)
  ;; LLM
  (:import-from #:demiurge-bootstrap/bootstrap/llm-ks
                #:openai-llm-capability #:make-openai-llm-capability)
  ;; Code editing
  (:import-from #:demiurge-bootstrap/bootstrap/code-editor-ks
                #:file-code-editing-capability #:make-file-code-editing-capability)
  ;; Git
  (:import-from #:demiurge-bootstrap/bootstrap/git-ks
                #:git-vcs-capability #:make-git-vcs-capability)
  ;; Forge
  (:import-from #:demiurge-bootstrap/bootstrap/forge-github-ks
                #:github-forge-capability #:make-github-forge-capability)
  (:import-from #:demiurge-bootstrap/bootstrap/forge-forgejo-ks
                #:forgejo-forge-capability #:make-forgejo-forge-capability)
  ;; Compute
  (:import-from #:demiurge-bootstrap/bootstrap/compute-ks
                #:local-compute-capability #:make-local-compute-capability)
  (:import-from #:demiurge-bootstrap/bootstrap/podman-compute-ks
                #:podman-compute-capability #:make-podman-compute-capability)
  ;; Communication
  (:import-from #:demiurge-bootstrap/bootstrap/mcp-bridge-ks
                #:mcp-bridge-capability #:make-mcp-bridge-capability)
  ;; Agent loop (tool-calling)
  (:import-from #:demiurge-bootstrap/bootstrap/agent-tools
                #:build-tool-definitions #:execute-tool-call)
  (:import-from #:demiurge-bootstrap/bootstrap/agent-loop
                #:start-agent-task)
  ;; Supervisor
  (:import-from #:demiurge-bootstrap/bootstrap/supervisor
                #:run-supervisor-step #:dispatch-action #:parse-action
                #:register-supervisor-watchers #:discover-models)
  ;; Loader
  (:import-from #:demiurge-bootstrap/bootstrap/loader
                #:load-bootstrap-capabilities #:make-bootstrap-context*
                #:bootstrap-context #:seed-bb-state
                #:ctx-bb #:ctx-mem)
  ;; REPL
  (:import-from #:demiurge-bootstrap/bootstrap/repl
                #:start #:stop-demiurge-repl #:submit-task #:status)
  ;; Swank
  (:import-from #:demiurge-bootstrap/bootstrap/swank-server
                #:start-swank #:stop-swank)
  ;; MCP server
  (:import-from #:demiurge-bootstrap/bootstrap/mcp-server
                #:start-mcp-server #:stop-mcp-server)
  ;; A2A server
  (:import-from #:demiurge-bootstrap/bootstrap/a2a-server
                #:start-a2a-server #:stop-a2a-server)
  ;; Re-export
  (:export ;; Capabilities
           #:openai-llm-capability #:make-openai-llm-capability
           #:file-code-editing-capability #:make-file-code-editing-capability
           #:git-vcs-capability #:make-git-vcs-capability
           #:github-forge-capability #:make-github-forge-capability
           #:forgejo-forge-capability #:make-forgejo-forge-capability
           #:local-compute-capability #:make-local-compute-capability
           #:podman-compute-capability #:make-podman-compute-capability
           #:mcp-bridge-capability #:make-mcp-bridge-capability
           ;; Agent loop
           #:build-tool-definitions #:execute-tool-call
           #:start-agent-task
           ;; Supervisor
           #:run-supervisor-step #:dispatch-action #:parse-action
           #:register-supervisor-watchers #:discover-models
           ;; Loader
           #:load-bootstrap-capabilities #:make-bootstrap-context*
           #:bootstrap-context #:seed-bb-state
           #:ctx-bb #:ctx-mem
           ;; REPL
           #:start #:stop-demiurge-repl #:submit-task #:status
           ;; Swank
           #:start-swank #:stop-swank
           ;; MCP server
           #:start-mcp-server #:stop-mcp-server
           ;; A2A server
           #:start-a2a-server #:stop-a2a-server))

(in-package #:demiurge-bootstrap/bootstrap/main)
