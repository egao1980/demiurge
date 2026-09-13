(in-package #:demiurge)

(defun %param-name (param)
  (let ((name (if (consp param) (car param) param)))
    (string-downcase (symbol-name name))))

(defun %arg-get (arguments key)
  (cond
    ((null arguments) nil)
    ((hash-table-p arguments)
     (or (gethash key arguments)
         (gethash (string-downcase key) arguments)
         (gethash (intern (string-upcase key) :keyword) arguments)))
    ((and (consp arguments) (keywordp (car arguments)))
     (getf arguments (intern (string-upcase key) :keyword)))
    ((listp arguments)
     (cdr (assoc key arguments :test #'equal)))
    (t nil)))

(defun %arguments-for-op (op arguments)
  (cond
    ((or (null arguments)
         (equal arguments "")
         (equal arguments "{}"))
     nil)
    ((stringp arguments)
     nil)
    (t
     (loop for param in (cap:capability-operation-params op)
           collect (%arg-get arguments (%param-name param))))))

(defun %tool-name-for (cap-name op-name)
  (format nil "~A/~A"
          (string-downcase (symbol-name cap-name))
          (string-downcase (symbol-name op-name))))

(defvar *operation-catalogue* nil
  "Catalogue bound while a capability tool handler runs.")

(defun %op-function-tool (capability op &optional catalogue)
  (let ((op-name (cap:capability-operation-name op)))
    (agent:make-function-tool
     :name (%tool-name-for (cap:capability-name capability) op-name)
     :description (or (cap:capability-operation-doc op) "")
     :handler (lambda (args)
                (let* ((*operation-catalogue* catalogue)
                       (out (apply #'cap:invoke-operation
                                   capability op-name
                                   (%arguments-for-op op args))))
                  (if (stringp out) out (princ-to-string out)))))))

(defun catalogue-function-tools (catalogue)
  "FUNCTION-TOOL list for every operation on every capability registered on CATALOGUE."
  (when catalogue
    (loop for row in (cap:list-capabilities catalogue)
          for capability = (cap:get-capability catalogue (getf row :name))
          when capability
            nconc (loop for op in (cap:capability-operations capability)
                        collect (%op-function-tool capability op catalogue)))))

(defun %skill-tool-sources (steering)
  "Steer-protocol directives that expose :SKILL-TOOLS extra → MAKE-SKILL-TOOL-SOURCE."
  (when steering
    (loop for directive in (steer:list-directives steering)
          for extra = (ignore-errors (steer:steer-directive-extra directive))
          when (getf extra :skill-tools)
            collect (agent:make-skill-tool-source directive))))

(defun %maybe-mcp-source (peer)
  "MAKE-MCP-TOOL-SOURCE when PEER is supplied."
  (when peer
    (agent.mcp:make-mcp-tool-source peer)))
