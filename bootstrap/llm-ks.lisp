(defpackage #:demiurge-bootstrap/bootstrap/llm-ks
  (:use #:cl)
  (:import-from #:demiurge/src/capabilities/llm
                #:llm-generation-capability #:generate-text #:generate-embedding #:list-models)
  (:import-from #:cl-openai
                #:make-provider #:create-chat-completion #:create-embedding)
  (:export #:openai-llm-capability #:make-openai-llm-capability))

(in-package #:demiurge-bootstrap/bootstrap/llm-ks)

(defclass openai-llm-capability (llm-generation-capability)
  ((provider :initarg :provider :reader llm-provider)))

(defun make-openai-llm-capability (provider &key (version "0.1.0"))
  (make-instance 'openai-llm-capability
                 :name :llm-generation :version version :provider provider))

(defmethod generate-text ((cap openai-llm-capability) messages &key)
  (let* ((msg-list (mapcar (lambda (m)
                             (let ((h (make-hash-table :test 'equal)))
                               (setf (gethash "role" h) (or (getf m :role) "user")
                                     (gethash "content" h) (or (getf m :content) ""))
                               h))
                           messages))
         (response (create-chat-completion (llm-provider cap) msg-list)))
    ;; Extract text from response
    (let* ((choices (gethash "choices" response))
           (first-choice (when choices (aref choices 0)))
           (message (when first-choice (gethash "message" first-choice))))
      (when message
        (or (gethash "content" message) "")))))

(defmethod generate-embedding ((cap openai-llm-capability) text &key)
  (let ((response (create-embedding (llm-provider cap) text)))
    (let* ((data (gethash "data" response))
           (first-entry (when data (aref data 0))))
      (when first-entry
        (gethash "embedding" first-entry)))))

(defmethod list-models ((cap openai-llm-capability) &key)
  (handler-case
      (let ((response (cl-openai:list-models (llm-provider cap))))
        (when response
          (mapcar (lambda (m) (gethash "id" m))
                  (coerce (gethash "data" response) 'list))))
    (error () nil)))
