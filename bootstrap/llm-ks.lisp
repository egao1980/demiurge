(defpackage #:demiurge-bootstrap/bootstrap/llm-ks
  (:use #:cl)
  (:import-from #:demiurge/src/capabilities/llm
                #:llm-generation-capability #:generate-text #:generate-with-tools
                #:generate-embedding #:list-models)
  (:import-from #:demiurge/src/blackboard/core
                #:blackboard #:read-section)
  (:import-from #:cl-openai
                #:make-provider #:create-chat-completion #:create-embedding
                #:extract-content #:extract-tool-calls #:extract-model-ids
                #:make-tool-call-message)
  (:export #:openai-llm-capability #:make-openai-llm-capability))

(in-package #:demiurge-bootstrap/bootstrap/llm-ks)

(defclass openai-llm-capability (llm-generation-capability)
  ((bb       :initarg :bb       :reader llm-bb       :initform nil)
   (provider :initarg :provider :reader llm-provider  :initform nil
             :documentation "Fallback provider when BB has no :llm-config.")))

(defun make-openai-llm-capability (provider &key bb (version "0.1.0"))
  "Create an LLM capability. PROVIDER is the legacy fallback; BB (a blackboard)
enables dynamic model-role resolution via :model-roles and :llm-config sections."
  (make-instance 'openai-llm-capability
                 :name :llm-generation :version version
                 :provider provider :bb bb))

;;; --- provider resolution ---------------------------------------------------

(defun resolve-provider (cap role)
  "Build a provider for ROLE from BB state, falling back to the stored provider."
  (let ((bb (llm-bb cap)))
    (if (null bb)
        (llm-provider cap)
        (let ((config (read-section bb :llm-config))
              (roles  (read-section bb :model-roles)))
          (if (null config)
              (llm-provider cap)
              (let ((model (cdr (assoc role roles))))
                (apply #'make-provider
                       :model model
                       config)))))))

;;; --- message conversion -----------------------------------------------------

(defun message-to-hash (m)
  "Convert a plist message to an OpenAI-format hash-table.
Handles :role, :content, :tool-calls, :tool-call-id, and :name."
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash "role" h) (or (getf m :role) "user"))
    (when (getf m :content)
      (setf (gethash "content" h) (getf m :content)))
    (when (getf m :tool-calls)
      (setf (gethash "tool_calls" h) (coerce (getf m :tool-calls) 'vector)))
    (when (getf m :tool-call-id)
      (setf (gethash "tool_call_id" h) (getf m :tool-call-id)))
    (when (getf m :name)
      (setf (gethash "name" h) (getf m :name)))
    h))

;;; --- capability methods ----------------------------------------------------

(defmethod generate-text ((cap openai-llm-capability) messages &key (role :supervisor))
  (let* ((provider (resolve-provider cap role))
         (msg-list (mapcar #'message-to-hash messages))
         (response (create-chat-completion provider msg-list)))
    (extract-content response)))

(defmethod generate-with-tools ((cap openai-llm-capability) messages tools &key (role :supervisor))
  "Call chat completion with tool definitions. Returns (values content tool-calls raw-response).
TOOLS is a list of OpenAI tool definition hash-tables.
TOOL-CALLS is a vector of tool-call objects from the response (or NIL)."
  (let* ((provider (resolve-provider cap role))
         (msg-list (mapcar #'message-to-hash messages))
         (response (create-chat-completion provider msg-list :tools tools)))
    (values (extract-content response)
            (extract-tool-calls response)
            response)))

(defmethod generate-embedding ((cap openai-llm-capability) text &key)
  (let ((provider (resolve-provider cap :embeddings)))
    (let* ((response (create-embedding provider text))
           (data (gethash "data" response))
           (first-entry (when data (aref data 0))))
      (when first-entry
        (gethash "embedding" first-entry)))))

(defmethod list-models ((cap openai-llm-capability) &key)
  (handler-case
      (let* ((provider (resolve-provider cap :supervisor))
             (response (cl-openai:list-models provider)))
        (extract-model-ids response))
    (error () nil)))
