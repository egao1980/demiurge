(defpackage #:demiurge-bootstrap/bootstrap/a2a-server
  (:use #:cl)
  (:import-from #:demiurge/src/blackboard/core
                #:blackboard #:read-section #:write-section)
  (:import-from #:demiurge/src/blackboard/workspace
                #:get-workspace #:list-workspaces
                #:workspace-name #:workspace-status)
  (:import-from #:cl-a2a
                #:make-agent-card #:make-a2a-server #:make-a2a-handler
                #:handle-json-rpc-request)
  (:import-from #:alexandria #:when-let)
  (:export #:start-a2a-server #:stop-a2a-server #:*a2a-acceptor*))

(in-package #:demiurge-bootstrap/bootstrap/a2a-server)

(defvar *a2a-acceptor* nil)
(defvar *a2a-server* nil)

(defun make-demiurge-agent-card (&key (port 8081))
  (make-agent-card
   "Demiurge"
   :description "Self-improving blackboard coding agent"
   :url (format nil "http://localhost:~D/a2a" port)
   :version "0.1.0"
   :capabilities (list "streaming" "pushNotifications")
   :skills (list (alexandria:plist-hash-table
                  '("id" "code-editing" "name" "Code Editing"
                    "description" "Edit code files with structural awareness")
                  :test 'equal)
                 (alexandria:plist-hash-table
                  '("id" "issue-resolution" "name" "Issue Resolution"
                    "description" "Resolve GitHub/Forgejo issues end-to-end")
                  :test 'equal)
                 (alexandria:plist-hash-table
                  '("id" "self-improvement" "name" "Self Improvement"
                    "description" "Improve own capabilities and KS implementations")
                  :test 'equal)
                 (alexandria:plist-hash-table
                  '("id" "code-review" "name" "Code Review"
                    "description" "Review code changes and provide feedback")
                  :test 'equal))))

(defun make-task-handler (bb)
  "Create a task handler that writes :pending-task token to BB."
  (lambda (task)
    (let* ((task-ht task)
           (messages (when (hash-table-p task-ht) (gethash "messages" task-ht)))
           (first-msg (when (and messages (> (length messages) 0))
                        (if (listp messages) (first messages) (aref messages 0))))
           (parts (when (hash-table-p first-msg) (gethash "parts" first-msg)))
           (first-part (when (and parts (> (length parts) 0))
                         (if (listp parts) (first parts) (aref parts 0))))
           (text (when (hash-table-p first-part) (gethash "text" first-part)))
           (task-id (when (hash-table-p task-ht) (gethash "id" task-ht))))
      (when text
        (write-section bb :pending-task
                       (list :id (or task-id (format nil "a2a-~A" (get-universal-time)))
                             :description text
                             :source :a2a))))
    task))

(defun make-a2a-rpc-handler (server)
  (lambda ()
    (setf (hunchentoot:content-type*) "application/json")
    (let ((body (hunchentoot:raw-post-data :force-text t)))
      (multiple-value-bind (result error-ht)
          (handle-json-rpc-request server body)
        (declare (ignore error-ht))
        (with-output-to-string (s)
          (yason:encode result s))))))

(defun make-agent-card-handler (server)
  (lambda ()
    (setf (hunchentoot:content-type*) "application/json")
    (with-output-to-string (s)
      (yason:encode (cl-a2a:agent-card-to-hash
                      (cl-a2a:server-agent-card server))
                    s))))

(defun start-a2a-server (bb &key (port 8081))
  "Start the A2A HTTP server."
  (when *a2a-acceptor*
    (format t "~&A2A server already running.~%")
    (return-from start-a2a-server *a2a-acceptor*))
  (let* ((card (make-demiurge-agent-card :port port))
         (handler (make-a2a-handler :task-fn (make-task-handler bb)))
         (server (make-a2a-server :agent-card card :handler handler)))
    (setf *a2a-server* server)
    (let ((acceptor (make-instance 'hunchentoot:easy-acceptor
                                    :port port :name "demiurge-a2a")))
      (push (hunchentoot:create-prefix-dispatcher
             "/.well-known/agent.json" (make-agent-card-handler server))
            hunchentoot:*dispatch-table*)
      (push (hunchentoot:create-prefix-dispatcher
             "/a2a" (make-a2a-rpc-handler server))
            hunchentoot:*dispatch-table*)
      (hunchentoot:start acceptor)
      (setf *a2a-acceptor* acceptor)
      (format t "~&A2A server started on port ~D~%" port)
      acceptor)))

(defun stop-a2a-server ()
  (when *a2a-acceptor*
    (hunchentoot:stop *a2a-acceptor*)
    (setf *a2a-acceptor* nil *a2a-server* nil)
    (format t "~&A2A server stopped.~%")))
