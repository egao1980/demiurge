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
                  :initform nil)
   (skill-store :initarg :skill-store :accessor profile-skill-store
                :initform nil)
   (require-hitl-p :initarg :require-hitl-p :accessor profile-require-hitl-p
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
  (or (asdf:component-loaded-p system)
      (handler-case (progn (asdf:load-system system :verbose nil) t)
        (error (e)
          (warn "could not load ~a: ~a" system e)
          (and (asdf:component-loaded-p system) t)))))

(defun %nonempty (value)
  (and value (plusp (length (string value))) value))

(defun %catalog-kind (entry)
  (let ((kind (or (%nonempty (getf entry :kind))
                  (%nonempty (getf entry :type))
                  (%nonempty (getf entry :backend)))))
    (when kind
      (intern (string-upcase (string kind)) :keyword))))

(defun %ensure-http-backend ()
  "Soft-bind http-backend-dexador so openai-compat / SearXNG can SEND."
  (let* ((http (find-package '#:http-protocol))
         (star (and http (find-symbol "*HTTP-BACKEND*" http))))
    (when (and star (symbol-value star))
      (return-from %ensure-http-backend t)))
  (when (%try-load "http-backend-dexador")
    (let ((fn (find-symbol "MAKE-DEXADOR-BACKEND" :http-backend-dexador))
          (http (find-package '#:http-protocol)))
      (when (and fn (fboundp fn) http)
        (let ((star (find-symbol "*HTTP-BACKEND*" http))
              (client (find-symbol "*HTTP-CLIENT*" http))
              (make-c (find-symbol "MAKE-HTTP-CLIENT" http))
              (backend (funcall fn)))
          (when star (setf (symbol-value star) backend))
          (when (and client make-c)
            (setf (symbol-value client)
                  (funcall make-c backend :timeout 300)))
          t)))))

(defun %require-catalog-symbol (system package-name symbol-name)
  (unless (%try-load system)
    (error 'expert-config-error
           :message (format nil "catalog entry needs system ~a loaded" system)))
  (let ((fn (find-symbol symbol-name package-name)))
    (unless (and fn (fboundp fn))
      (error 'expert-config-error
             :message (format nil "~a:~a missing after loading ~a"
                              package-name symbol-name system)))
    fn))

(defun %make-catalog-backend (entry)
  "Build an llm-protocol backend from a catalog entry. Live kinds error if
   their system cannot be loaded — no silent mock fallback."
  (let ((kind (%catalog-kind entry))
        (name (string-downcase (string (or (%nonempty (getf entry :name))
                                           "default")))))
    (cond
      ((or (null kind) (member kind '(:mock :echo) :test #'eq))
       (values name (llm:make-mock-llm-backend
                     :prefix (or (%nonempty (getf entry :prefix)) "echo: "))))
      ((member kind '(:lmstudio :lm-studio :openai :openai-compat) :test #'eq)
       (unless (%ensure-http-backend)
         (error 'expert-config-error
                :message "openai-compat catalog entry needs http-backend-dexador"))
       (let ((fn (%require-catalog-symbol "llm-protocol-openai"
                                          :llm-protocol-openai
                                          "MAKE-OPENAI-COMPAT-BACKEND")))
         (values name
                 (funcall fn
                          :base-url (or (%nonempty (getf entry :base-url))
                                        (%nonempty (getf entry :endpoint))
                                        "http://127.0.0.1:1234/v1")
                          :default-model (or (%nonempty (getf entry :model))
                                             (%nonempty (getf entry :default-model))
                                             "local")
                          :api-key (or (%nonempty (getf entry :api-key))
                                       (let ((env (getf entry :api-key-env)))
                                         (and env (plusp (length env))
                                              (uiop:getenv env))))))))
      ((member kind '(:llama-cpp :llamacpp :gguf) :test #'eq)
       (let ((fn (%require-catalog-symbol "llm-backend-llama-cpp"
                                          :llm-backend-llama-cpp
                                          "MAKE-LLAMA-CPP-BACKEND")))
         (values name
                 (funcall fn
                          :model-path (or (%nonempty (getf entry :model-path))
                                          (%nonempty (getf entry :model))
                                          (uiop:getenv "LLAMA_MODEL_PATH"))))))
      (t
       (error 'expert-config-error
              :message (format nil "unknown llm catalog kind ~s for ~s"
                               kind name))))))

(defun %build-llm-catalog (config)
  (let ((cat (llm:make-in-memory-provider-catalog))
        (entries (or (and config (demiurge-config-llm-catalog config)) nil)))
    (dolist (entry entries)
      (multiple-value-bind (name backend)
          (%make-catalog-backend entry)
        (when (and name backend)
          (llm:register-provider
           cat name
           (wrap-llm-observe backend :expert name :scope "profile")
           :models (let ((m (getf entry :model)))
                     (and m (list m)))))))
    (when (zerop (length (llm:list-providers cat)))
      (llm:register-provider
       cat "mock"
       (wrap-llm-observe (llm:make-mock-llm-backend)
                         :expert "mock" :scope "profile")))
    cat))

(defun resolve-profile-llm (profile &optional llm)
  "LLM from PROFILE's [[llm.catalog]]. Supplied LLM wins.
   No catalog → NIL (caller may mock). Catalog but unresolvable → error."
  (when llm
    (return-from resolve-profile-llm llm))
  (unless (deployment-profile-p profile)
    (return-from resolve-profile-llm nil))
  (let ((cat (profile-llm-catalog profile))
        (model (profile-default-model profile)))
    (unless cat
      (return-from resolve-profile-llm nil))
    (handler-case (nth-value 0 (llm:resolve-backend cat model))
      (error (e)
        (error 'expert-config-error
               :message (format nil
                                "cannot resolve llm ~s from catalog (~{~a~^, ~}): ~a"
                                model
                                (mapcar #'llm:llm-provider-name
                                        (or (ignore-errors (llm:list-providers cat))
                                            '()))
                                e))))))

(defun profile-backend-summary (profile)
  "Plist describing the catalog backend bound from expert.toml."
  (when (deployment-profile-p profile)
    (let* ((llm (ignore-errors (resolve-profile-llm profile)))
           (bare (and llm (bare-llm-backend llm))))
      (list :model (profile-default-model profile)
            :providers (mapcar #'llm:llm-provider-name
                               (or (ignore-errors
                                     (llm:list-providers (profile-llm-catalog profile)))
                                   '()))
            :llm-class (and bare (type-of bare))))))

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
                                chunker rag-store llm-catalog default-model
                                skill-store (require-hitl-p nil))
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
                                      (demiurge-config-llm-default-model cfg))
                   :skill-store skill-store
                   :require-hitl-p require-hitl-p)))
