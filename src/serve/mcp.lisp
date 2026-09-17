(in-package #:demiurge/serve)

(defun ask-expert (domain prompt &key board timeout)
  "Run DOMAIN on PROMPT. → (values result-text feedback-id board)."
  (check-type domain expert-domain)
  (let* ((board (or board (bb:make-blackboard)))
         (fid (make-feedback-id))
         (ran (run-expert domain
                          :board board
                          :trigger (list :prompt prompt)
                          :timeout timeout))
         (text (bb:read-section ran :result :default nil)))
    (bb:write-section ran :feedback-id fid)
    (values text fid ran)))

(defun make-ask-expert-tool (domain)
  (mcp:make-mcp-tool
   "ask_expert"
   :description "Run the expert on a prompt and return the result"
   :input-schema (mcp:json-object
                  "type" "object"
                  "additionalProperties" nil
                  "required" (vector "prompt")
                  "properties"
                  (mcp:json-object
                   "prompt" (mcp:json-object "type" "string")))
   :handler (lambda (args)
              (let ((prompt (or (%ht-get args "prompt" :prompt) "")))
                (multiple-value-bind (text fid)
                    (ask-expert domain prompt)
                  (mcp:tool-result
                   (list (mcp:make-text-content
                          (format nil "~a~%feedback-id: ~a"
                                  (or text "") fid)))))))))

(defun %attach-workspace-mcp (server domain)
  "When demiurge/workflows is loaded and DOMAIN has a checkout root,
   mount workspace:// + search/read tools. Soft dep — serve stays loadable alone."
  (let ((fn (find-symbol "ATTACH-WORKSPACE-TO-EXPERT-MCP" :demiurge/workflows)))
    (when (and fn (fboundp fn))
      (funcall fn server domain))))

(defun make-expert-mcp-server (domain &key blackboard name)
  "Catalogue tools + ask_expert + record_feedback + workspace:// when available."
  (check-type domain expert-domain)
  (let ((server (wire.mcp:make-mcp-server-from-catalogue
                 (expert-catalogue domain)
                 :blackboard blackboard
                 :name (or name (expert-name domain))
                 :version "0.3.1"
                 :instructions (format nil "Demiurge expert ~a" (expert-name domain)))))
    (mcp:register-tool server (make-ask-expert-tool domain))
    (mcp:register-tool server (make-record-feedback-tool domain))
    (%attach-workspace-mcp server domain)
    server))
