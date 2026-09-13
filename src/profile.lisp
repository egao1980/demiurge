(in-package #:demiurge)

(defclass deployment-profile ()
  ((kind :initarg :kind :accessor profile-kind :initform :personal)
   (data-dir :initarg :data-dir :accessor profile-data-dir :initform nil)
   (config :initarg :config :accessor profile-config :initform nil)
   (session-store :initarg :session-store :accessor profile-session-store
                  :initform nil)
   (journal :initarg :journal :accessor profile-journal :initform nil)
   (chunker :initarg :chunker :accessor profile-chunker :initform nil)
   (rag-store :initarg :rag-store :accessor profile-rag-store :initform nil)
   (llm-catalog :initarg :llm-catalog :accessor profile-llm-catalog :initform nil)
   (default-model :initarg :default-model :accessor profile-default-model
                  :initform nil)))

(defun deployment-profile-p (x)
  (typep x 'deployment-profile))

(defclass personal-profile (deployment-profile)
  ()
  (:default-initargs :kind :personal))

(defun personal-profile-p (x)
  (typep x 'personal-profile))

(defun %default-data-dir (config)
  (or (and config (demiurge-config-paths-data-dir config))
      (namestring (merge-pathnames "demiurge/" (uiop:xdg-data-home)))))

(defun %try-load (system)
  (ignore-errors (asdf:load-system system :verbose nil)))

(defun %catalog-kind (entry)
  (let ((kind (or (getf entry :kind) (getf entry :type) (getf entry :backend))))
    (when kind
      (intern (string-upcase (string kind)) :keyword))))

(defun %make-catalog-backend (entry)
  "Build an llm-protocol backend from a catalog entry. Soft-loads natives."
  (let ((kind (%catalog-kind entry))
        (name (string-downcase (string (or (getf entry :name) "default")))))
    (cond
      ((member kind '(:mock :echo) :test #'eq)
       (values name (llm:make-mock-llm-backend
                     :prefix (or (getf entry :prefix) "echo: "))))
      ((member kind '(:lmstudio :lm-studio :openai :openai-compat) :test #'eq)
       (if (%try-load "llm-protocol-openai")
           (let ((fn (find-symbol "MAKE-OPENAI-COMPAT-BACKEND"
                                 :llm-protocol-openai)))
             (if (and fn (fboundp fn))
                 (values name
                         (funcall fn
                                  :base-url (or (getf entry :base-url)
                                                (getf entry :endpoint)
                                                "http://127.0.0.1:1234/v1")
                                  :default-model (or (getf entry :model)
                                                     (getf entry :default-model)
                                                     "local")
                                  :api-key (getf entry :api-key)))
                 (values nil nil)))
           (values nil nil)))
      ((member kind '(:llama-cpp :llamacpp :gguf) :test #'eq)
       (if (%try-load "llm-backend-llama-cpp")
           (let ((fn (find-symbol "MAKE-LLAMA-CPP-BACKEND"
                                 :llm-backend-llama-cpp)))
             (if (and fn (fboundp fn))
                 (values name
                         (funcall fn
                                  :model-path (or (getf entry :model-path)
                                                  (getf entry :model)
                                                  (uiop:getenv "LLAMA_MODEL_PATH"))))
                 (values nil nil)))
           (values nil nil)))
      (t (values nil nil)))))

(defun %build-llm-catalog (config)
  (let ((cat (llm:make-in-memory-provider-catalog))
        (entries (or (and config (demiurge-config-llm-catalog config)) nil)))
    (dolist (entry entries)
      (multiple-value-bind (name backend)
          (%make-catalog-backend entry)
        (when (and name backend)
          (llm:register-provider cat name backend
                                 :models (let ((m (getf entry :model)))
                                           (and m (list m)))))))
    (when (zerop (length (llm:list-providers cat)))
      (llm:register-provider cat "mock" (llm:make-mock-llm-backend)))
    cat))

(defun %open-session-store (path)
  (unless (%ensure-sqlite-backend)
    (return-from %open-session-store
      (conv:make-in-memory-conversation-store)))
  (csql:make-sql-session-store :driver :sqlite3
                               :database-name (namestring path)))

(defun %open-rag-store (path)
  (if (and path (%ensure-sqlite-backend))
      (rag-backend-sql:make-sql-vector-store
       :driver :sqlite3
       :database-name (namestring path))
      (rag-backend-memory:make-memory-vector-store)))

(defun make-personal-profile (&key data-dir config journal session-store
                                chunker rag-store llm-catalog default-model)
  "SQLite sessions + journal, file corpora (text splitter + memory/sql store),
   LLM catalog from CONFIG (llama-cpp or LM Studio). Zero external services."
  (let* ((cfg (or config (current-demiurge-config)))
         (root (uiop:ensure-directory-pathname
                (or data-dir (%default-data-dir cfg)))))
    (ensure-directories-exist root)
    (make-instance 'personal-profile
                   :kind :personal
                   :data-dir (namestring root)
                   :config cfg
                   :session-store (or session-store
                                      (%open-session-store
                                       (merge-pathnames "sessions.sqlite" root)))
                   :journal (or journal
                                (if (%ensure-sqlite-backend)
                                    (%open-sql-journal
                                     (merge-pathnames "journal.sqlite" root))
                                    (task:make-in-memory-journal)))
                   :chunker (or chunker
                                (rag-backend-text:make-recursive-character-chunker
                                 :size 1000 :overlap 200))
                   :rag-store (or rag-store
                                  (%open-rag-store
                                   (merge-pathnames "rag.sqlite" root)))
                   :llm-catalog (or llm-catalog (%build-llm-catalog cfg))
                   :default-model (or default-model
                                      (demiurge-config-llm-default-model cfg)))))
