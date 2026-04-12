(defpackage #:demiurge-bootstrap/bootstrap/loader
  (:use #:cl)
  (:import-from #:demiurge/src/blackboard/core
                #:blackboard #:read-section #:write-section)
  (:import-from #:demiurge/src/capabilities/registry #:register-capability)
  (:import-from #:demiurge/src/persistence/memory
                #:persistent-memory #:make-persistent-memory
                #:mem-get #:mem-set)
  (:import-from #:demiurge/src/utils/config
                #:load-config)
  (:import-from #:demiurge-bootstrap/bootstrap/llm-ks
                #:make-openai-llm-capability)
  (:import-from #:demiurge-bootstrap/bootstrap/code-editor-ks
                #:make-file-code-editing-capability)
  (:import-from #:demiurge-bootstrap/bootstrap/git-ks
                #:make-git-vcs-capability)
  (:import-from #:demiurge-bootstrap/bootstrap/compute-ks
                #:make-local-compute-capability)
  (:import-from #:demiurge-bootstrap/bootstrap/podman-compute-ks
                #:make-podman-compute-capability #:shared-workspace)
  (:import-from #:demiurge-bootstrap/bootstrap/mcp-bridge-ks
                #:make-mcp-bridge-capability)
  (:import-from #:demiurge-bootstrap/bootstrap/forge-github-ks
                #:make-github-forge-capability)
  (:import-from #:demiurge-bootstrap/bootstrap/forge-forgejo-ks
                #:make-forgejo-forge-capability)
  (:import-from #:demiurge-bootstrap/bootstrap/searxng-ks
                #:make-searxng-web-search-capability)
  (:import-from #:demiurge-bootstrap/bootstrap/supervisor
                #:register-supervisor-watchers #:discover-models)
  (:export #:load-bootstrap-capabilities
           #:bootstrap-context #:make-bootstrap-context*
           #:seed-bb-state
           #:ctx-bb #:ctx-mem))

(in-package #:demiurge-bootstrap/bootstrap/loader)

;;; --- Bootstrap context struct ---

(defstruct (bootstrap-context (:constructor %make-bootstrap-context))
  (bb nil)
  (mem nil))

(defun ctx-bb (ctx) (bootstrap-context-bb ctx))
(defun ctx-mem (ctx) (bootstrap-context-mem ctx))

;;; --- BB state seeding ---

(defun seed-bb-state (bb mem config)
  "Seed blackboard sections from CONFIG, preferring persisted memory values."
  (flet ((seed (key value)
           (unless (read-section bb key)
             (write-section bb key value))))
    (let ((url (or (and config (gethash "lm_studio_url" config))
                   "http://192.168.86.67:1234/v1"))
          (api-key (and config (gethash "lm_studio_api_key" config)))
          (read-timeout (or (and config (gethash "llm_read_timeout_seconds" config)) 300))
          (connect-timeout (or (and config (gethash "llm_connect_timeout_seconds" config)) 30)))
      (seed :llm-config (append (list :base-url url
                                      :read-timeout read-timeout
                                      :connect-timeout connect-timeout)
                                (when api-key (list :api-key api-key)))))
    (let ((saved-roles (when mem (mem-get mem "pref:model-roles"))))
      (seed :model-roles
            (or saved-roles
                (let ((defaults (and config (gethash "default_model_roles" config))))
                  (if defaults
                      (let ((roles nil))
                        (maphash (lambda (k v)
                                   (push (cons (intern (string-upcase k) :keyword) v) roles))
                                 defaults)
                        (nreverse roles))
                      '((:supervisor . "nvidia/nemotron-3-nano-4b")
                        (:coder . "qwen/qwen3-coder-next")
                        (:fast . "nvidia/nemotron-3-nano-4b")
                        (:embeddings . "text-embedding-nomic-embed-text-v1.5")))))))
    (seed :container-config
          (list :runtime (intern (string-upcase
                                  (or (and config (gethash "container_runtime" config)) "podman"))
                                 :keyword)
                :image (or (and config (gethash "container_image" config))
                           "docker.io/library/ubuntu:24.04")))
    (seed :project-root
          (or (and config (gethash "project_root" config)) "~/Projects/lisp/"))))

;;; --- Capability registration ---

(defun load-bootstrap-capabilities (bb &key config mem project-root)
  "Register all bootstrap capabilities with the blackboard."
  (seed-bb-state bb mem config)
  (let ((root (or project-root (read-section bb :project-root))))
    (register-capability bb (make-openai-llm-capability nil :bb bb))
    (register-capability bb (make-git-vcs-capability))
    (let ((container-cfg (read-section bb :container-config)))
      (if (eq :podman (getf container-cfg :runtime))
          (let ((cap (make-podman-compute-capability
                      :image (getf container-cfg :image))))
            (register-capability bb cap)
            (let ((ws-dir (namestring (shared-workspace cap))))
              (write-section bb :shared-workspace ws-dir)
              (register-capability bb (make-file-code-editing-capability
                                       :root root :workspace-dir ws-dir))))
          (progn
            (register-capability bb (make-local-compute-capability :cwd root))
            (register-capability bb (make-file-code-editing-capability :root root)))))
    (register-capability bb (make-github-forge-capability))
    (when (and config (gethash "forgejo_url" config))
      (register-capability bb (make-forgejo-forge-capability
                                (gethash "forgejo_url" config))))
    (register-capability bb (make-mcp-bridge-capability bb))
    ;; Web search via SearXNG (configurable URL from BB or default localhost:8888)
    (let ((searxng-url (or (let ((cfg (read-section bb :searxng-config)))
                             (when (listp cfg) (getf cfg :url)))
                           "http://localhost:8888")))
      (register-capability bb (make-searxng-web-search-capability :base-url searxng-url)))
    (handler-case (discover-models bb)
      (error (e)
        (format *error-output* "Initial model discovery failed: ~A~%" e))))
  bb)

;;; --- Full bootstrap ---

(defun make-bootstrap-context* (bb &key config-path memory-path project-root)
  "Create a fully bootstrapped context: load config, create memory, register capabilities, wire watchers."
  (let* ((config (when config-path (load-config config-path)))
         (mem-path (or memory-path
                       (and config (gethash "memory_path" config))
                       "~/.config/demiurge/memory.json"))
         (mem (make-persistent-memory :path (merge-pathnames mem-path (user-homedir-pathname)))))
    (load-bootstrap-capabilities bb :config config :mem mem :project-root project-root)
    (register-supervisor-watchers bb mem)
    (%make-bootstrap-context :bb bb :mem mem)))
