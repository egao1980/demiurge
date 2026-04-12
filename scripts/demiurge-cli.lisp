;;; Demiurge CLI — MCP client that connects to the running MCP server.
;;; Usage: sbcl --load demiurge-cli.lisp -- <command> [args...]

(require :asdf)
(asdf:load-system :dexador :verbose nil)
(asdf:load-system :yason :verbose nil)
(asdf:load-system :alexandria :verbose nil)

(defpackage #:demiurge-cli
  (:use #:cl))

(in-package #:demiurge-cli)

(defvar *base-url* "http://localhost:8080")

(defun rpc-call (method &optional params)
  (let* ((id (random 100000))
         (request (alexandria:plist-hash-table
                   (append (list "jsonrpc" "2.0" "method" method "id" id)
                           (when params (list "params" params)))
                   :test 'equal))
         (body (with-output-to-string (s) (yason:encode request s))))
    (handler-case
        (let ((response-body (dex:post (format nil "~A/mcp" *base-url*)
                                       :content body
                                       :headers '(("Content-Type" . "application/json"))
                                       :want-stream nil)))
          (let* ((response (yason:parse response-body :object-as :hash-table
                                                      :object-key-fn #'identity))
                 (result (gethash "result" response))
                 (err (gethash "error" response)))
            (if err
                (progn (format *error-output* "Error: ~A~%" (gethash "message" err)) nil)
                result)))
      (error (e)
        (format *error-output* "~&Connection failed: ~A~%Is Demiurge running?~%" e)
        nil))))

(defun call-tool (name args)
  (rpc-call "tools/call"
            (alexandria:plist-hash-table
             (list "name" name "arguments" args) :test 'equal)))

(defun read-resource (uri)
  "Read an MCP resource. Extracts text from the contents array."
  (let ((result (rpc-call "resources/read"
                          (alexandria:plist-hash-table
                           (list "uri" uri) :test 'equal))))
    (when (hash-table-p result)
      (let ((contents (gethash "contents" result)))
        (when (and contents (listp contents))
          (let ((first-item (first contents)))
            (when (hash-table-p first-item)
              (or (gethash "text" first-item)
                  (gethash "uri" first-item)))))))))

(defun extract-tool-text (result)
  "Extract text from a tool call result."
  (when (hash-table-p result)
    (let ((content (gethash "content" result)))
      (when (and content (listp content))
        (let ((first-item (first content)))
          (when (hash-table-p first-item)
            (gethash "text" first-item)))))))

(defun cmd-submit (desc)
  (let ((r (call-tool "submit-task"
                      (alexandria:plist-hash-table (list "description" desc) :test 'equal))))
    (when r (format t "~A~%" (or (extract-tool-text r) r)))))

(defun cmd-status (&optional name)
  (if name
      (let ((r (call-tool "workspace-status"
                          (alexandria:plist-hash-table (list "name" name) :test 'equal))))
        (when r (format t "~A~%" (or (extract-tool-text r) r))))
      (let ((r (read-resource "demiurge://blackboard")))
        (when r (format t "~A~%" r)))))

(defun cmd-tasks ()
  (let ((r (read-resource "demiurge://tasks/recent")))
    (when r (format t "~A~%" r))))

(defun cmd-models ()
  (let ((r (read-resource "demiurge://models")))
    (when r (format t "~A~%" r))))

(defun cmd-configure (role model)
  (let ((r (call-tool "configure-model"
                      (alexandria:plist-hash-table (list "role" role "model" model) :test 'equal))))
    (when r (format t "~A~%" (or (extract-tool-text r) r)))))

(defun cmd-bb (&optional key)
  (if key
      (let ((r (call-tool "bb-read"
                          (alexandria:plist-hash-table (list "key" key) :test 'equal))))
        (when r (format t "~A~%" (or (extract-tool-text r) r))))
      (let ((r (read-resource "demiurge://blackboard")))
        (when r (format t "~A~%" r)))))

(defun cmd-memory (prefix)
  (let ((r (read-resource (format nil "demiurge://memory/~A" prefix))))
    (when r (format t "~A~%" r))))

(defun cmd-workspace-read (name key)
  (let ((r (call-tool "workspace-read"
                      (alexandria:plist-hash-table (list "name" name "key" key) :test 'equal))))
    (when r (format t "~A~%" (or (extract-tool-text r) r)))))

(defun cmd-workspaces ()
  (let ((r (read-resource "demiurge://workspaces")))
    (when r (format t "~A~%" r))))

(defun cmd-help ()
  (format t "~&Demiurge CLI~%~%")
  (format t "Usage: sbcl --load demiurge-cli.lisp -- <command> [args...]~%~%")
  (format t "Commands:~%")
  (format t "  submit <description>        Submit a task~%")
  (format t "  status [workspace-name]     Show status~%")
  (format t "  tasks                       Recent tasks~%")
  (format t "  models                      Available models~%")
  (format t "  configure <role> <model>    Reassign model role~%")
  (format t "  bb [section-key]            Read blackboard~%")
  (format t "  memory <prefix>             Query memory~%")
  (format t "  workspaces                  List workspaces~%")
  (format t "  help                        This help~%")
  (format t "~%Options:~%")
  (format t "  --url <base-url>            Server URL (default: http://localhost:8080)~%"))

(defun run ()
  (let ((args (uiop:command-line-arguments)))
    ;; Skip past "--"
    (when (member "--" args :test #'string=)
      (setf args (rest (member "--" args :test #'string=))))
    ;; Parse --url
    (loop while (and args (string= "--url" (first args)))
          do (pop args) (when args (setf *base-url* (pop args))))
    (let ((cmd (or (first args) "help"))
          (rest (rest args)))
      (cond
        ((string-equal cmd "submit")  (cmd-submit (format nil "~{~A~^ ~}" rest)))
        ((string-equal cmd "status")  (cmd-status (first rest)))
        ((string-equal cmd "tasks")   (cmd-tasks))
        ((string-equal cmd "models")  (cmd-models))
        ((string-equal cmd "configure") (cmd-configure (first rest) (second rest)))
        ((string-equal cmd "bb")      (cmd-bb (first rest)))
        ((string-equal cmd "memory")  (cmd-memory (or (first rest) "")))
        ((string-equal cmd "workspaces") (cmd-workspaces))
        ((string-equal cmd "ws-read") (cmd-workspace-read (first rest) (second rest)))
        (t (cmd-help))))))

(run)
(sb-ext:exit)
