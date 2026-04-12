(defpackage #:demiurge-bootstrap/main
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
  ;; Communication
  (:import-from #:demiurge-bootstrap/bootstrap/mcp-bridge-ks
                #:mcp-bridge-capability #:make-mcp-bridge-capability)
  ;; Loader
  (:import-from #:demiurge-bootstrap/bootstrap/loader
                #:load-bootstrap-capabilities)
  ;; Re-export
  (:export #:openai-llm-capability #:make-openai-llm-capability
           #:file-code-editing-capability #:make-file-code-editing-capability
           #:git-vcs-capability #:make-git-vcs-capability
           #:github-forge-capability #:make-github-forge-capability
           #:forgejo-forge-capability #:make-forgejo-forge-capability
           #:local-compute-capability #:make-local-compute-capability
           #:mcp-bridge-capability #:make-mcp-bridge-capability
           #:load-bootstrap-capabilities))

(in-package #:demiurge-bootstrap/main)
