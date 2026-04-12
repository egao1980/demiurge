(defpackage #:demiurge-bootstrap/bootstrap/loader
  (:use #:cl)
  (:import-from #:demiurge/src/blackboard/core #:blackboard)
  (:import-from #:demiurge/src/capabilities/registry #:register-capability)
  (:import-from #:demiurge-bootstrap/bootstrap/llm-ks
                #:make-openai-llm-capability)
  (:import-from #:demiurge-bootstrap/bootstrap/code-editor-ks
                #:make-file-code-editing-capability)
  (:import-from #:demiurge-bootstrap/bootstrap/git-ks
                #:make-git-vcs-capability)
  (:import-from #:demiurge-bootstrap/bootstrap/compute-ks
                #:make-local-compute-capability)
  (:import-from #:demiurge-bootstrap/bootstrap/mcp-bridge-ks
                #:make-mcp-bridge-capability)
  (:export #:load-bootstrap-capabilities))

(in-package #:demiurge-bootstrap/bootstrap/loader)

(defun load-bootstrap-capabilities (bb &key llm-provider project-root)
  "Register all bootstrap capability implementations with the blackboard."
  ;; LLM (optional -- needs a provider)
  (when llm-provider
    (register-capability bb (make-openai-llm-capability llm-provider)))
  ;; Code editing
  (register-capability bb (make-file-code-editing-capability :root project-root))
  ;; Version control
  (register-capability bb (make-git-vcs-capability))
  ;; Compute
  (register-capability bb (make-local-compute-capability :cwd project-root))
  ;; Communication (MCP bridge)
  (register-capability bb (make-mcp-bridge-capability bb))
  bb)
