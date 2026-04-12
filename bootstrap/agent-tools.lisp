(defpackage #:demiurge-bootstrap/bootstrap/agent-tools
  (:use #:cl)
  (:import-from #:demiurge/src/blackboard/core
                #:blackboard #:blackboard-capabilities #:blackboard-lock
                #:read-section #:write-section)
  (:import-from #:demiurge/src/capabilities/protocol
                #:capability #:capability-name #:capability-operations
                #:capability-operation-name #:capability-operation-params
                #:capability-operation-doc)
  (:import-from #:demiurge/src/capabilities/registry
                #:get-capability)
  (:import-from #:demiurge/src/capabilities/code-editing
                #:read-file #:write-file #:patch-file #:list-files)
  (:import-from #:demiurge/src/capabilities/compute
                #:run-command #:create-environment #:exec-in-environment #:destroy-environment)
  (:import-from #:demiurge/src/capabilities/web-search
                #:web-search #:fetch-page)
  (:import-from #:cl-openai
                #:make-tool-definition #:make-function-parameters)
  (:export #:build-tool-definitions #:execute-tool-call))

(in-package #:demiurge-bootstrap/bootstrap/agent-tools)

;;; ---------------------------------------------------------------------------
;;; Tool definition builders — one per exposed operation
;;; ---------------------------------------------------------------------------

(defun tool-def (name description params &optional required)
  "Shorthand: build an OpenAI tool definition."
  (make-tool-definition name description
                        (make-function-parameters params :required (or required
                                                                       (mapcar #'car params)))))

(defun build-tool-definitions (bb)
  "Build a list of OpenAI tool definitions from registered capabilities + BB tools.
Only includes operations that the agent loop knows how to dispatch."
  (declare (ignore bb))
  (list
   ;; --- :code-editing ---
   (tool-def "read_file" "Read a file and return its contents."
             '(("path" . (:type "string" :description "Absolute file path to read")))
             '("path"))
   (tool-def "write_file" "Write content to a file, creating directories as needed."
             '(("path" . (:type "string" :description "Absolute file path (use /workspace/ for container-visible files)"))
               ("content" . (:type "string" :description "File content to write")))
             '("path" "content"))
   (tool-def "patch_file" "Apply a patch to a file (old/new text replacement)."
             '(("path" . (:type "string" :description "Absolute file path"))
               ("old" . (:type "string" :description "Text to find"))
               ("new" . (:type "string" :description "Replacement text")))
             '("path" "old" "new"))
   (tool-def "list_files" "List files in a directory."
             '(("directory" . (:type "string" :description "Directory path")))
             '("directory"))
   ;; --- :compute ---
   (tool-def "run_command"
             "Run a shell command in a FRESH ephemeral container. Each call starts from a clean image — installed packages and files outside /workspace/ are LOST between calls. Chain related commands with && in one call (e.g. \"apt-get update && apt-get install -y sbcl && sbcl --script /workspace/fib.lisp\"). /workspace/ is mounted and persists. No sudo needed (already root)."
             '(("command" . (:type "string" :description "Shell command (chain with && to keep state within one call)")))
             '("command"))
   (tool-def "create_environment"
             "Create a persistent named container. State (installed packages, files) survives across exec_in_environment calls. Use for multi-step work: install tools once, then run commands repeatedly."
             '(("name" . (:type "string" :description "Environment name"))
               ("image" . (:type "string" :description "Docker image (e.g. ubuntu:24.04)")))
             '("name"))
   (tool-def "exec_in_environment"
             "Execute a command in a persistent environment. State from previous exec_in_environment calls is preserved (packages, files, etc.)."
             '(("env" . (:type "string" :description "Environment name from create_environment"))
               ("command" . (:type "string" :description "Shell command")))
             '("env" "command"))
   (tool-def "destroy_environment" "Destroy a persistent environment and free resources."
             '(("env" . (:type "string" :description "Environment name")))
             '("env"))
   ;; --- :web-search ---
   (tool-def "web_search" "Search the web via SearXNG. Returns results with title, url, snippet."
             '(("query" . (:type "string" :description "Search query")))
             '("query"))
   (tool-def "fetch_page" "Fetch a URL and return its text content."
             '(("url" . (:type "string" :description "URL to fetch")))
             '("url"))
   ;; --- blackboard ---
   (tool-def "bb_read" "Read a blackboard section by key."
             '(("key" . (:type "string" :description "Section key (e.g. model-roles, shared-workspace)")))
             '("key"))
   (tool-def "bb_write" "Write a value to a blackboard section."
             '(("key" . (:type "string" :description "Section key"))
               ("value" . (:type "string" :description "Value to write")))
             '("key" "value"))))

;;; ---------------------------------------------------------------------------
;;; Known tools — required args registry for validation
;;; ---------------------------------------------------------------------------

(defvar *tool-required-args*
  '(("read_file" . ("path"))
    ("write_file" . ("path" "content"))
    ("patch_file" . ("path" "old" "new"))
    ("list_files" . ("directory"))
    ("run_command" . ("command"))
    ("create_environment" . ("name"))
    ("exec_in_environment" . ("env" "command"))
    ("destroy_environment" . ("env"))
    ("web_search" . ("query"))
    ("fetch_page" . ("url"))
    ("bb_read" . ("key"))
    ("bb_write" . ("key" "value")))
  "Alist of tool-name -> list of required argument names.")

;;; ---------------------------------------------------------------------------
;;; Tool call extraction and parsing
;;; ---------------------------------------------------------------------------

(defun tool-call-name (tc)
  "Extract tool name from an OpenAI tool_call object (hash-table)."
  (let ((fn (gethash "function" tc)))
    (when fn (gethash "name" fn))))

(defun tool-call-id (tc)
  "Extract tool call ID."
  (gethash "id" tc))

(defun tool-call-args (tc)
  "Parse and return (values args-hash parse-error-p).
On JSON parse failure, returns empty hash and T as second value."
  (let* ((fn (gethash "function" tc))
         (args-str (when fn (gethash "arguments" fn))))
    (if (and args-str (stringp args-str) (plusp (length args-str)))
        (handler-case
            (values (yason:parse args-str :object-as :hash-table :object-key-fn #'identity)
                    nil)
          (error (e)
            (declare (ignore e))
            (values (make-hash-table :test 'equal) t)))
        (values (make-hash-table :test 'equal) nil))))

(defun arg (args key &optional default)
  (or (gethash key args) default))

;;; ---------------------------------------------------------------------------
;;; Tool call execution with validation
;;; ---------------------------------------------------------------------------

(defun execute-tool-call (bb ws-bb tc)
  "Execute a single tool call with validation. Returns a result string.
Validates: tool name exists, JSON args parsed, required args present,
capability is registered. All errors return clear messages the LLM can act on."
  (let ((name (tool-call-name tc)))
    ;; 1. Validate tool name
    (unless (and name (stringp name))
      (return-from execute-tool-call
        "Error: malformed tool call — missing or invalid function name."))
    (let ((required (assoc name *tool-required-args* :test #'string=)))
      (unless required
        (return-from execute-tool-call
          (format nil "Error: unknown tool '~A'. Available tools: ~{~A~^, ~}"
                  name (mapcar #'car *tool-required-args*)))))
    ;; 2. Parse arguments
    (multiple-value-bind (args parse-error-p) (tool-call-args tc)
      (when parse-error-p
        (let* ((fn (gethash "function" tc))
               (raw (when fn (gethash "arguments" fn))))
          (return-from execute-tool-call
            (format nil "Error: invalid JSON in arguments for '~A'. Raw: ~A"
                    name (if (> (length raw) 200) (subseq raw 0 200) raw)))))
      ;; 3. Check required args
      (let ((missing (loop for req in (cdr (assoc name *tool-required-args* :test #'string=))
                           unless (nth-value 1 (gethash req args))
                           collect req)))
        (when missing
          (return-from execute-tool-call
            (format nil "Error: missing required argument~P for '~A': ~{~A~^, ~}"
                    (length missing) name missing))))
      ;; 4. Dispatch with capability check and error handling
      (handler-case
          (dispatch-tool bb ws-bb name args)
        (error (e)
          (format nil "Error executing '~A': ~A" name e))))))

(defun format-result (value)
  "Convert a tool result to a string for the LLM."
  (typecase value
    (string value)
    (hash-table (with-output-to-string (s) (yason:encode value s)))
    (null "OK")
    (cons (if (every #'hash-table-p value)
              (with-output-to-string (s) (yason:encode value s))
              (format nil "~S" value)))
    (t (format nil "~A" value))))

(defun require-capability (bb cap-name tool-name)
  "Get a capability or signal a clear error."
  (or (get-capability bb cap-name)
      (error "Capability '~A' not available (needed by tool '~A')" cap-name tool-name)))

(defun format-command-result (res)
  "Format a (exit-code stdout stderr) list from run-command."
  (if (listp res)
      (format nil "exit=~A~%~A~@[~%STDERR: ~A~]"
              (first res) (or (second res) "") (third res))
      (format nil "~A" res)))

(defun dispatch-tool (bb ws-bb name args)
  "Route a tool call to the appropriate capability and format the result."
  (let ((result
          (cond
            ;; --- code-editing ---
            ((string= name "read_file")
             (let ((content (read-file (require-capability bb :code-editing name)
                                       (arg args "path"))))
               (or content
                   (format nil "Error: file not found: ~A" (arg args "path")))))
            ((string= name "write_file")
             (let ((content (arg args "content")))
               (write-file (require-capability bb :code-editing name)
                           (arg args "path") content)
               (format nil "Wrote ~A (~D bytes)" (arg args "path") (length content))))
            ((string= name "patch_file")
             (let ((patch (list :old (arg args "old") :new (arg args "new"))))
               (if (patch-file (require-capability bb :code-editing name)
                               (arg args "path") patch)
                   (format nil "Patched ~A" (arg args "path"))
                   (format nil "Error: old text not found in ~A" (arg args "path")))))
            ((string= name "list_files")
             (let ((files (list-files (require-capability bb :code-editing name)
                                      (arg args "directory"))))
               (if files
                   (format nil "~{~A~%~}" files)
                   (format nil "Error: directory empty or not found: ~A"
                           (arg args "directory")))))
            ;; --- compute ---
            ((string= name "run_command")
             (format-command-result
              (run-command (require-capability bb :compute name)
                           (arg args "command"))))
            ((string= name "create_environment")
             (let ((cap (require-capability bb :compute name))
                   (spec (list :name (arg args "name")
                               :image (arg args "image" "docker.io/library/ubuntu:24.04"))))
               (create-environment cap spec)
               (let ((envs (or (read-section bb :active-environments) nil)))
                 (write-section bb :active-environments (cons spec envs)))
               (format nil "Environment ~A created." (arg args "name"))))
            ((string= name "exec_in_environment")
             (let* ((env-name (arg args "env"))
                    (envs (read-section bb :active-environments))
                    (env (find env-name envs :key (lambda (e) (getf e :name))
                                             :test #'string=)))
               (if env
                   (format-command-result
                    (exec-in-environment (require-capability bb :compute name)
                                         env (arg args "command")))
                   (format nil "Error: environment '~A' not found. Active: ~{~A~^, ~}"
                           env-name (mapcar (lambda (e) (getf e :name)) envs)))))
            ((string= name "destroy_environment")
             (let* ((env-name (arg args "env"))
                    (envs (read-section bb :active-environments))
                    (env (find env-name envs :key (lambda (e) (getf e :name))
                                             :test #'string=)))
               (if env
                   (progn
                     (destroy-environment (require-capability bb :compute name) env)
                     (write-section bb :active-environments (remove env envs))
                     (format nil "Environment ~A destroyed." env-name))
                   (format nil "Error: environment '~A' not found" env-name))))
            ;; --- web-search ---
            ((string= name "web_search")
             (let ((results (web-search (require-capability bb :web-search name)
                                        (arg args "query"))))
               (if results
                   (with-output-to-string (s)
                     (dolist (r results)
                       (format s "~A~%  ~A~%  ~A~%~%"
                               (getf r :title) (getf r :url) (getf r :snippet))))
                   "No results found.")))
            ((string= name "fetch_page")
             (or (fetch-page (require-capability bb :web-search name)
                             (arg args "url"))
                 (format nil "Error: could not fetch ~A" (arg args "url"))))
            ;; --- blackboard ---
            ((string= name "bb_read")
             (let* ((key (intern (string-upcase (arg args "key")) :keyword))
                    (val (or (read-section ws-bb key) (read-section bb key))))
               (if val
                   (format nil "~A" val)
                   (format nil "Section ~A not found." key))))
            ((string= name "bb_write")
             (let ((key (intern (string-upcase (arg args "key")) :keyword)))
               (write-section ws-bb key (arg args "value"))
               (format nil "Written: ~A" key)))
            ;; --- unknown --- (shouldn't reach here due to validation above)
            (t (format nil "Error: unknown tool '~A'" name)))))
    (format-result result)))
