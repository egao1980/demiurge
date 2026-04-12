(defpackage #:demiurge-bootstrap/bootstrap/mcp-server
  (:use #:cl)
  (:import-from #:demiurge/src/blackboard/core
                #:blackboard #:read-section #:write-section #:list-sections
                #:watch #:list-watchers #:agenda-size #:bb-active-count)
  (:import-from #:demiurge/src/blackboard/workspace
                #:list-workspaces #:get-workspace #:workspace-name
                #:workspace-status #:workspace-blackboard
                #:merge-workspace #:discard-workspace)
  (:import-from #:demiurge/src/capabilities/registry
                #:list-capabilities #:get-capability)
  (:import-from #:demiurge/src/capabilities/llm
                #:list-models)
  (:import-from #:demiurge/src/persistence/memory
                #:persistent-memory #:mem-get)
  (:import-from #:demiurge/src/persistence/memory-keys
                #:recent-tasks)
  (:import-from #:demiurge/src/introspection/render
                #:render-bb-summary #:render-workspace #:render-capabilities)
  (:import-from #:cl-mcp-sdk
                #:make-mcp-server #:server-registry #:handle-message
                #:define-tool #:define-resource #:define-resource-template)
  (:import-from #:alexandria #:when-let)
  (:export #:start-mcp-server #:stop-mcp-server #:*mcp-acceptor*))

(in-package #:demiurge-bootstrap/bootstrap/mcp-server)

;;; --- MCP server backed by Hunchentoot HTTP ---

(defvar *mcp-acceptor* nil)
(defvar *mcp-server* nil)

(defun value-to-string (val)
  "Render a value to a human-readable string."
  (typecase val
    (hash-table (with-output-to-string (s) (yason:encode val s)))
    (cons (if (every #'hash-table-p val)
              (with-output-to-string (s) (yason:encode val s))
              (format nil "~S" val)))
    (null "NIL")
    (t (format nil "~A" val))))

;;; --- Resource & Tool registration ---

(defun register-resources (server bb mem)
  (let ((reg (server-registry server)))
    (cl-mcp-sdk:register-resource
     reg "demiurge://blackboard" "Blackboard"
     "Current blackboard state summary" "text/plain"
     (lambda () (render-bb-summary bb)))

    (cl-mcp-sdk:register-resource
     reg "demiurge://workspaces" "Workspaces"
     "Active workspaces list" "text/plain"
     (lambda ()
       (format nil "~{~A~%~}"
               (mapcar (lambda (ws)
                         (format nil "~A [~A]" (workspace-name ws) (workspace-status ws)))
                       (list-workspaces bb)))))

    (cl-mcp-sdk:register-resource
     reg "demiurge://models" "Models"
     "Available models and role assignments" "text/plain"
     (lambda ()
       (format nil "Available: ~{~A~^, ~}~%~%Roles:~%~{  ~A -> ~A~%~}"
               (or (read-section bb :available-models) '("(unknown)"))
               (loop for (role . model) in (or (read-section bb :model-roles) nil)
                     collect (string-downcase (symbol-name role))
                     collect model))))

    (cl-mcp-sdk:register-resource
     reg "demiurge://tasks/recent" "Recent Tasks"
     "Recent task history from memory" "text/plain"
     (lambda ()
       (if mem
           (let ((tasks (recent-tasks mem)))
             (if tasks
                 (format nil "~{~A~%~}" tasks)
                 "No recent tasks."))
           "No persistent memory available.")))

    (cl-mcp-sdk:register-resource-template
     reg "demiurge://workspaces/{name}" "Workspace Detail"
     "Detailed workspace state" "text/plain"
     (lambda (bindings)
       (let* ((name (cdr (assoc "name" bindings :test #'string=)))
              (ws (get-workspace bb name)))
         (if ws
             (render-workspace ws)
             (format nil "Workspace ~A not found." name)))))

    (cl-mcp-sdk:register-resource-template
     reg "demiurge://memory/{prefix}" "Memory Query"
     "Query persistent memory by prefix" "text/plain"
     (lambda (bindings)
       (let ((prefix (cdr (assoc "prefix" bindings :test #'string=))))
         (if mem
             (let ((val (mem-get mem prefix)))
               (if val
                   (format nil "~A" val)
                   (format nil "No value for key: ~A" prefix)))
             "No persistent memory available."))))))

(defun register-tools (server bb mem)
  (let ((reg (server-registry server)))
    ;; submit-task — writes :pending-task token to BB
    (cl-mcp-sdk:register-tool
     reg "submit-task" "Submit a task to Demiurge"
     (yason:parse "{\"type\":\"object\",\"properties\":{\"description\":{\"type\":\"string\"}},\"required\":[\"description\"]}"
                  :object-as :hash-table :object-key-fn #'identity)
     (lambda (args)
       (let ((desc (gethash "description" args)))
         (write-section bb :pending-task
                        (list :id (format nil "task-~A" (get-universal-time))
                              :description desc
                              :source :mcp))
         (format nil "Task submitted: ~A" desc))))

    ;; bb-read
    (cl-mcp-sdk:register-tool
     reg "bb-read" "Read a blackboard section"
     (yason:parse "{\"type\":\"object\",\"properties\":{\"key\":{\"type\":\"string\"}},\"required\":[\"key\"]}"
                  :object-as :hash-table :object-key-fn #'identity)
     (lambda (args)
       (let* ((key (intern (string-upcase (gethash "key" args)) :keyword))
              (val (read-section bb key)))
         (value-to-string val))))

    ;; bb-write
    (cl-mcp-sdk:register-tool
     reg "bb-write" "Write a blackboard section"
     (yason:parse "{\"type\":\"object\",\"properties\":{\"key\":{\"type\":\"string\"},\"value\":{\"type\":\"string\"}},\"required\":[\"key\",\"value\"]}"
                  :object-as :hash-table :object-key-fn #'identity)
     (lambda (args)
       (let ((key (intern (string-upcase (gethash "key" args)) :keyword))
             (val (gethash "value" args)))
         (write-section bb key val)
         (format nil "Written: ~A" key))))

    ;; configure-model
    (cl-mcp-sdk:register-tool
     reg "configure-model" "Reassign a model to a role"
     (yason:parse "{\"type\":\"object\",\"properties\":{\"role\":{\"type\":\"string\"},\"model\":{\"type\":\"string\"}},\"required\":[\"role\",\"model\"]}"
                  :object-as :hash-table :object-key-fn #'identity)
     (lambda (args)
       (let* ((role (intern (string-upcase (gethash "role" args)) :keyword))
              (model (gethash "model" args))
              (roles (or (read-section bb :model-roles) nil))
              (updated (cons (cons role model)
                             (remove role roles :key #'car))))
         (write-section bb :model-roles updated)
         (format nil "Role ~A -> ~A" role model))))

    ;; list-models
    (cl-mcp-sdk:register-tool
     reg "list-models" "List available LM Studio models"
     (yason:parse "{\"type\":\"object\",\"properties\":{}}" :object-as :hash-table :object-key-fn #'identity)
     (lambda (args)
       (declare (ignore args))
       (let ((llm (get-capability bb :llm-generation)))
         (if llm
             (format nil "~{~A~^~%~}" (or (list-models llm) '("No models found")))
             "No LLM capability registered."))))

    ;; workspace-status
    (cl-mcp-sdk:register-tool
     reg "workspace-status" "Get workspace detail"
     (yason:parse "{\"type\":\"object\",\"properties\":{\"name\":{\"type\":\"string\"}},\"required\":[\"name\"]}"
                  :object-as :hash-table :object-key-fn #'identity)
     (lambda (args)
       (let ((ws (get-workspace bb (gethash "name" args))))
         (if ws (render-workspace ws) "Workspace not found."))))

    ;; workspace-read
    (cl-mcp-sdk:register-tool
     reg "workspace-read" "Read a section from a workspace blackboard"
     (yason:parse "{\"type\":\"object\",\"properties\":{\"name\":{\"type\":\"string\"},\"key\":{\"type\":\"string\"}},\"required\":[\"name\",\"key\"]}"
                  :object-as :hash-table :object-key-fn #'identity)
     (lambda (args)
       (let ((ws (get-workspace bb (gethash "name" args))))
         (if ws
             (let* ((key (intern (string-upcase (gethash "key" args)) :keyword))
                    (val (read-section (workspace-blackboard ws) key)))
               (value-to-string val))
             "Workspace not found."))))

    ;; workspace-merge
    (cl-mcp-sdk:register-tool
     reg "workspace-merge" "Merge a workspace back to main BB"
     (yason:parse "{\"type\":\"object\",\"properties\":{\"name\":{\"type\":\"string\"}},\"required\":[\"name\"]}"
                  :object-as :hash-table :object-key-fn #'identity)
     (lambda (args)
       (let ((ws (get-workspace bb (gethash "name" args))))
         (if ws (progn (merge-workspace ws) "Merged.") "Workspace not found."))))

    ;; workspace-discard
    (cl-mcp-sdk:register-tool
     reg "workspace-discard" "Discard a workspace"
     (yason:parse "{\"type\":\"object\",\"properties\":{\"name\":{\"type\":\"string\"}},\"required\":[\"name\"]}"
                  :object-as :hash-table :object-key-fn #'identity)
     (lambda (args)
       (let ((ws (get-workspace bb (gethash "name" args))))
         (if ws (progn (discard-workspace ws) "Discarded.") "Workspace not found."))))

    ;; cancel-task — signal a running agent workspace to stop
    (cl-mcp-sdk:register-tool
     reg "cancel-task" "Cancel a running agent task by workspace name"
     (yason:parse "{\"type\":\"object\",\"properties\":{\"name\":{\"type\":\"string\"}},\"required\":[\"name\"]}"
                  :object-as :hash-table :object-key-fn #'identity)
     (lambda (args)
       (let ((ws (get-workspace bb (gethash "name" args))))
         (if ws
             (progn
               (write-section (workspace-blackboard ws) :agent-cancelled t)
               (format nil "Cancellation signalled for ~A. Will stop at next iteration."
                       (gethash "name" args)))
             (format nil "Workspace '~A' not found." (gethash "name" args))))))

    ;; memory-query
    (cl-mcp-sdk:register-tool
     reg "memory-query" "Query persistent memory"
     (yason:parse "{\"type\":\"object\",\"properties\":{\"key\":{\"type\":\"string\"}},\"required\":[\"key\"]}"
                  :object-as :hash-table :object-key-fn #'identity)
     (lambda (args)
       (if mem
           (format nil "~A" (mem-get mem (gethash "key" args)))
           "No persistent memory available.")))))

;;; --- HTTP transport via Hunchentoot ---

(defun make-json-rpc-handler (server)
  (lambda ()
    (setf (hunchentoot:content-type*) "application/json")
    (let* ((body (hunchentoot:raw-post-data :force-text t))
           (request (handler-case (yason:parse body :object-as :hash-table
                                                    :object-key-fn #'identity)
                      (error () nil))))
      (if request
          (let* ((method (gethash "method" request))
                 (params (or (gethash "params" request)
                             (make-hash-table :test 'equal)))
                 (id (gethash "id" request)))
            (multiple-value-bind (result err)
                (handle-message server method params)
              (with-output-to-string (s)
                (yason:encode
                 (if err
                     (alexandria:plist-hash-table
                      (list "jsonrpc" "2.0" "id" id "error" err) :test 'equal)
                     (alexandria:plist-hash-table
                      (list "jsonrpc" "2.0" "id" id "result" result) :test 'equal))
                 s))))
          (with-output-to-string (s)
            (yason:encode
             (alexandria:plist-hash-table
              (list "jsonrpc" "2.0" "id" nil
                    "error" (alexandria:plist-hash-table
                             (list "code" -32700 "message" "Parse error") :test 'equal))
              :test 'equal)
             s))))))

(defun start-mcp-server (bb mem &key (port 8080))
  "Start the MCP HTTP server on PORT."
  (when *mcp-acceptor*
    (format t "~&MCP server already running.~%")
    (return-from start-mcp-server *mcp-acceptor*))
  (let ((server (make-mcp-server :name "demiurge" :version "0.1.0")))
    (register-resources server bb mem)
    (register-tools server bb mem)
    (setf *mcp-server* server)
    (let ((acceptor (make-instance 'hunchentoot:easy-acceptor
                                    :port port :name "demiurge-mcp")))
      (push (hunchentoot:create-prefix-dispatcher
             "/mcp" (make-json-rpc-handler server))
            hunchentoot:*dispatch-table*)
      (hunchentoot:start acceptor)
      (setf *mcp-acceptor* acceptor)
      (format t "~&MCP server started on port ~D~%" port)
      acceptor)))

(defun stop-mcp-server ()
  (when *mcp-acceptor*
    (hunchentoot:stop *mcp-acceptor*)
    (setf *mcp-acceptor* nil *mcp-server* nil)
    (format t "~&MCP server stopped.~%")))
