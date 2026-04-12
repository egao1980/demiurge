(defpackage #:demiurge-bootstrap/bootstrap/llm-ks
  (:use #:cl)
  (:import-from #:demiurge/src/capabilities/llm
                #:llm-generation-capability #:generate-text #:generate-embedding #:list-models)
  (:import-from #:demiurge/src/blackboard/core
                #:blackboard #:read-section)
  (:import-from #:cl-openai
                #:make-provider #:create-chat-completion #:create-embedding
                #:extract-content #:extract-model-ids)
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

;;; --- capability methods ----------------------------------------------------

(defmethod generate-text ((cap openai-llm-capability) messages &key (role :supervisor))
  (let* ((provider (resolve-provider cap role))
         (msg-list (mapcar (lambda (m)
                             (let ((h (make-hash-table :test 'equal)))
                               (setf (gethash "role" h) (or (getf m :role) "user")
                                     (gethash "content" h) (or (getf m :content) ""))
                               h))
                           messages))
         (response (create-chat-completion provider msg-list)))
    (extract-content response)))

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
