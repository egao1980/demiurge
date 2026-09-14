(in-package #:demiurge/bundle)

;;; Canonical expert.toml schema. Packed bundle manifests are the same
;;; construction path: parse → bundle-ks-definition / corpus / skill / eval
;;; refs → INSTANTIATE-EXPERT-DOMAIN.

(schema:defschema expert-config-agenda ()
  (max-concurrency integer :optional t :accessor expert-config-agenda-max-concurrency)
  (:key-style :kebab)
  (:extra :forbid))

(schema:defschema expert-config-ksar ()
  (timeout-seconds integer :optional t :accessor expert-config-ksar-timeout-seconds)
  (:key-style :kebab)
  (:extra :forbid))

(schema:defschema expert-config-session ()
  (window-turns integer :optional t :accessor expert-config-session-window-turns)
  (:key-style :kebab)
  (:extra :forbid))

(schema:defschema expert-config-paths ()
  (data-dir string :optional t :accessor expert-config-paths-data-dir)
  (:key-style :kebab)
  (:extra :forbid))

(schema:defschema expert-config-improve-overrides ()
  (enabled boolean :optional t :accessor expert-config-improve-overrides-enabled)
  (:key-style :kebab)
  (:extra :forbid))

(schema:defschema expert-config-oidc ()
  (issuer string :optional t :accessor expert-config-oidc-issuer)
  (client-id string :optional t :accessor expert-config-oidc-client-id)
  (:key-style :kebab)
  (:extra :forbid))

(schema:defschema expert-config-ldap ()
  (url string :optional t :accessor expert-config-ldap-url)
  (base-dn string :optional t :accessor expert-config-ldap-base-dn)
  (group-role-map hash-table :optional t
                  :accessor expert-config-ldap-group-role-map)
  (:key-style :kebab)
  (:extra :forbid))

(schema:defschema expert-config-dsn ()
  (dsn string :optional t :accessor expert-config-dsn-dsn)
  (:key-style :kebab)
  (:extra :forbid))

(schema:defschema expert-config-endpoint ()
  (endpoint string :optional t :accessor expert-config-endpoint-endpoint)
  (:key-style :kebab)
  (:extra :forbid))

(schema:defschema expert-config-tenant ()
  (id string :optional t :accessor expert-config-tenant-id)
  (:key-style :kebab)
  (:extra :forbid))

(schema:defschema expert-config-corporate ()
  (oidc expert-config-oidc :optional t :default nil
        :accessor expert-config-corporate-oidc)
  (ldap expert-config-ldap :optional t :default nil
        :accessor expert-config-corporate-ldap)
  (postgres expert-config-dsn :optional t :default nil
            :accessor expert-config-corporate-postgres)
  (otlp expert-config-endpoint :optional t :default nil
        :accessor expert-config-corporate-otlp)
  (tenant expert-config-tenant :optional t :default nil
          :accessor expert-config-corporate-tenant)
  (role-grants hash-table :optional t :default nil
               :accessor expert-config-corporate-role-grants)
  (:key-style :kebab)
  (:extra :forbid))

(schema:defschema expert-config-profile ()
  "personal|corporate plus cl-stack-config key overrides (src/config.lisp)."
  (kind string :optional t :default "personal"
        :accessor expert-config-profile-kind)
  (agenda expert-config-agenda :optional t :default nil
          :accessor expert-config-profile-agenda)
  (ksar expert-config-ksar :optional t :default nil
        :accessor expert-config-profile-ksar)
  (session expert-config-session :optional t :default nil
           :accessor expert-config-profile-session)
  (paths expert-config-paths :optional t :default nil
         :accessor expert-config-profile-paths)
  (improve expert-config-improve-overrides :optional t :default nil
           :accessor expert-config-profile-improve)
  (corporate expert-config-corporate :optional t :default nil
             :accessor expert-config-profile-corporate)
  (:key-style :kebab)
  (:extra :forbid))

(schema:defschema expert-config-llm-entry ()
  (name string :optional t :default "" :accessor expert-config-llm-entry-name)
  (kind string :optional t :default "" :accessor expert-config-llm-entry-kind)
  (type string :optional t :default "" :accessor expert-config-llm-entry-type)
  (backend string :optional t :default ""
           :accessor expert-config-llm-entry-backend)
  (model string :optional t :default "" :accessor expert-config-llm-entry-model)
  (default-model string :optional t :default ""
                 :accessor expert-config-llm-entry-default-model)
  (prefix string :optional t :default ""
          :accessor expert-config-llm-entry-prefix)
  (base-url string :optional t :default ""
            :accessor expert-config-llm-entry-base-url)
  (endpoint string :optional t :default ""
            :accessor expert-config-llm-entry-endpoint)
  (api-key string :optional t :default ""
           :accessor expert-config-llm-entry-api-key)
  (api-key-env string :optional t :default ""
               :accessor expert-config-llm-entry-api-key-env)
  (model-path string :optional t :default ""
              :accessor expert-config-llm-entry-model-path)
  (:key-style :kebab)
  (:extra :forbid))

(schema:defschema expert-config-llm-budget ()
  (tokens integer :optional t :accessor expert-config-budget-tokens)
  (cost number :optional t :accessor expert-config-budget-cost)
  (:key-style :kebab)
  (:extra :forbid))

(schema:defschema expert-config-llm ()
  (default-model string :optional t :default "mock"
                 :accessor expert-config-llm-default-model)
  (catalog (list expert-config-llm-entry) :optional t :default nil
           :accessor expert-config-llm-catalog)
  (catalogue (list expert-config-llm-entry) :optional t :default nil
             :accessor expert-config-llm-catalogue)
  (budget-tokens integer :optional t :default nil
                 :accessor expert-config-llm-budget-tokens)
  (budget-cost number :optional t :default nil
               :accessor expert-config-llm-budget-cost)
  (budget expert-config-llm-budget :optional t :default nil
          :accessor expert-config-llm-budget)
  (:key-style :kebab)
  (:extra :forbid))

(schema:defschema expert-config-expert ()
  (name string :min-length 1 :accessor expert-config-expert-name)
  (description string :optional t :default ""
               :accessor expert-config-expert-description)
  (catalogue string :optional t :default "world"
             :accessor expert-config-expert-catalogue)
  (catalog string :optional t :default ""
           :accessor expert-config-expert-catalog)
  (:key-style :kebab)
  (:extra :forbid))

(schema:defschema expert-config-corpus ()
  (kind string :optional t :default "file" :accessor expert-config-corpus-kind)
  (spec string :optional t :default "" :accessor expert-config-corpus-spec)
  (root string :optional t :default "" :accessor expert-config-corpus-root)
  (path string :optional t :default "" :accessor expert-config-corpus-path)
  (uri string :optional t :default "" :accessor expert-config-corpus-uri)
  (pattern string :optional t :default "*"
           :accessor expert-config-corpus-pattern)
  (recursive boolean :optional t :default t
             :accessor expert-config-corpus-recursive)
  (store string :optional t :default "" :accessor expert-config-corpus-store)
  (host string :optional t :default "" :accessor expert-config-corpus-host)
  (port integer :optional t :accessor expert-config-corpus-port)
  (mailbox string :optional t :default ""
           :accessor expert-config-corpus-mailbox)
  (search string :optional t :default "" :accessor expert-config-corpus-search)
  (username string :optional t :default ""
            :accessor expert-config-corpus-username)
  (bucket string :optional t :default "" :accessor expert-config-corpus-bucket)
  (prefix string :optional t :default "" :accessor expert-config-corpus-prefix)
  (:key-style :kebab)
  (:extra :forbid))

(schema:defschema expert-config-skill ()
  (name string :optional t :default "" :accessor expert-config-skill-name)
  (path string :optional t :default "" :accessor expert-config-skill-path)
  (version string :optional t :default ""
           :accessor expert-config-skill-version)
  (digest string :optional t :default "" :accessor expert-config-skill-digest)
  (store string :optional t :default "" :accessor expert-config-skill-store)
  (:key-style :kebab)
  (:extra :forbid))

(schema:defschema expert-config-tool-grant ()
  (op string :accessor expert-config-tool-grant-op)
  (world boolean :optional t :default t
         :accessor expert-config-tool-grant-world)
  (compute boolean :optional t :default nil
           :accessor expert-config-tool-grant-compute)
  (:key-style :kebab)
  (:extra :forbid))

(schema:defschema expert-config-ks ()
  (name string :min-length 1 :accessor expert-config-ks-name)
  (kind string :optional t :default "agent-ks"
        :accessor expert-config-ks-kind)
  (watch string :optional t :default "(:prompt)"
         :accessor expert-config-ks-watch)
  (prompt-key string :optional t :default "prompt"
              :accessor expert-config-ks-prompt-key)
  (result-key string :optional t :default "result"
              :accessor expert-config-ks-result-key)
  (instructions string :optional t :default ""
                :accessor expert-config-ks-instructions)
  (prompt string :optional t :default "" :accessor expert-config-ks-prompt)
  (skill string :optional t :default "" :accessor expert-config-ks-skill)
  (tool-grants (list expert-config-tool-grant) :optional t :default nil
               :accessor expert-config-ks-tool-grants)
  (world boolean :optional t :default nil :accessor expert-config-ks-world)
  (compute boolean :optional t :default nil :accessor expert-config-ks-compute)
  (mcp-url string :optional t :default "" :accessor expert-config-ks-mcp-url)
  (split-ratio number :optional t :default nil
               :accessor expert-config-ks-split-ratio)
  (:key-style :kebab)
  (:extra :forbid))

(schema:defschema expert-config-eval ()
  (name string :min-length 1 :accessor expert-config-eval-name)
  (path string :optional t :default "" :accessor expert-config-eval-path)
  (dataset string :optional t :default ""
           :accessor expert-config-eval-dataset)
  (gate string :optional t :default "" :accessor expert-config-eval-gate)
  (version string :optional t :default ""
           :accessor expert-config-eval-version)
  (:key-style :kebab)
  (:extra :forbid))

(schema:defschema expert-config-serve ()
  (transports (list string) :optional t :default nil
              :accessor expert-config-serve-transports)
  (host string :optional t :default "127.0.0.1"
        :accessor expert-config-serve-host)
  (port integer :optional t :default 8080
        :accessor expert-config-serve-port)
  (:key-style :kebab)
  (:extra :forbid))

(schema:defschema expert-config-improve ()
  (enabled boolean :optional t :default nil
           :accessor expert-config-improve-enabled)
  (schedule string :optional t :default ""
            :accessor expert-config-improve-schedule)
  (hitl boolean :optional t :default nil
        :accessor expert-config-improve-hitl)
  (:key-style :kebab)
  (:extra :forbid))

(schema:defschema expert-config-websearch ()
  "Websearch backend referenced by research (mock fixtures or SearXNG)."
  (kind string :optional t :default "mock"
        :accessor expert-config-websearch-kind)
  (base-url string :optional t :default ""
            :accessor expert-config-websearch-base-url)
  (fixtures string :optional t :default ""
            :accessor expert-config-websearch-fixtures)
  (:key-style :kebab)
  (:extra :forbid))

(schema:defschema expert-config ()
  "Declarative expert.toml document. Extra keys are forbidden."
  (expert expert-config-expert :accessor expert-config-expert)
  (profile expert-config-profile :optional t :default nil
           :accessor expert-config-profile)
  (llm expert-config-llm :optional t :default nil :accessor expert-config-llm)
  (websearch expert-config-websearch :optional t :default nil
             :accessor expert-config-websearch)
  (corpus (list expert-config-corpus) :optional t :default nil
          :accessor expert-config-corpus)
  (skill (list expert-config-skill) :optional t :default nil
         :accessor expert-config-skill)
  (ks (list expert-config-ks) :optional t :default nil
      :accessor expert-config-ks)
  (eval (list expert-config-eval) :optional t :default nil
        :accessor expert-config-eval)
  (serve expert-config-serve :optional t :default nil
         :accessor expert-config-serve)
  (improve expert-config-improve :optional t :default nil
           :accessor expert-config-improve)
  (:key-style :kebab)
  (:extra :forbid))

(defun expert-config-p (x)
  (typep x 'expert-config))

(defun %ensure-toml-backend ()
  (or toml:*toml-backend*
      (let ((fn (find-symbol "USE-TOMLET-BACKEND" :toml-backend-tomlet)))
        (if (and fn (fboundp fn))
            (funcall fn)
            (error 'expert-config-error
                   :message "load toml-backend-tomlet to decode expert.toml")))))

(defun %schema-valid-keys (schema)
  (let ((class (schema:schema-of schema)))
    (loop for slot in (schema:schema-slots class)
          when (schema:slot-wire-p slot)
            collect (schema:slot-wire-key slot class))))

(defun %schema-class-name-p (name)
  (let ((class (and (symbolp name) (find-class name nil))))
    (and class (schema:schema-class-p class))))

(defun %slot-nested-schema (slot)
  (let ((elt (ignore-errors (schema:slot-element-type slot))))
    (cond
      ((and elt (symbolp elt) (%schema-class-name-p elt)) elt)
      (t
       (let* ((mop (find-symbol "SLOT-DEFINITION-TYPE" "CLOSER-MOP"))
              (spec (and mop (funcall mop slot))))
         (cond
           ((and spec (symbolp spec) (%schema-class-name-p spec)) spec)
           ((and (consp spec)
                 (member (first spec) '(list vector sequence))
                 (symbolp (second spec))
                 (%schema-class-name-p (second spec)))
            (second spec))
           (t nil)))))))

(defun %table-get (table key)
  (when (hash-table-p table)
    (or (gethash key table)
        (gethash (string-downcase (string key)) table))))

(defun %as-sequence (value)
  (cond
    ((null value) nil)
    ((and (vectorp value) (not (stringp value))) value)
    ((listp value) value)
    (t (list value))))

(defun %strip-unknown-keys (schema table &optional section)
  "Signal UNKNOWN-EXPERT-CONFIG-KEY for extras. CONTINUE drops the key."
  (unless (hash-table-p table)
    (return-from %strip-unknown-keys table))
  (let ((valid (%schema-valid-keys schema))
        (class (schema:schema-of schema)))
    (maphash
     (lambda (k v)
       (declare (ignore v))
       (let ((key (string-downcase (string k))))
         (unless (member key valid :test #'equal)
           (restart-case
               (error 'unknown-expert-config-key
                      :key key
                      :valid-keys valid
                      :section section
                      :message (format nil "unknown key ~S; valid keys: ~{~A~^, ~}"
                                       key valid))
             (continue ()
               :report (lambda (s)
                         (format s "Ignore unknown key ~S. Valid keys: ~{~A~^, ~}"
                                 key valid))
               (remhash k table))))))
     table)
    (dolist (slot (schema:schema-slots class))
      (when (schema:slot-wire-p slot)
        (let* ((key (schema:slot-wire-key slot class))
               (nested (%slot-nested-schema slot))
               (val (%table-get table key)))
          (when (and nested val)
            (if (member (ignore-errors
                          (schema:type-kind
                           (let ((mop (find-symbol "SLOT-DEFINITION-TYPE"
                                                   "CLOSER-MOP")))
                             (and mop (funcall mop slot)))))
                        '(:list :vector :sequence))
                (map nil (lambda (item)
                           (when (hash-table-p item)
                             (%strip-unknown-keys nested item key)))
                     (%as-sequence val))
                (when (hash-table-p val)
                  (%strip-unknown-keys nested val key)))))))
    table))

(defun %ht (pairs)
  (let ((ht (make-hash-table :test #'equal)))
    (loop for (k v) on pairs by #'cddr
          do (setf (gethash k ht) v))
    ht))

(defun %normalize-llm-catalog (table)
  (let ((llm (%table-get table "llm")))
    (when (hash-table-p llm)
      (let ((cat (or (%table-get llm "catalog")
                     (%table-get llm "catalogue"))))
        (when (hash-table-p cat)
          (let ((entries (make-array 0 :adjustable t :fill-pointer 0)))
            (maphash (lambda (name spec)
                       (let ((ht (if (hash-table-p spec)
                                     spec
                                     (make-hash-table :test #'equal))))
                         (unless (%table-get ht "name")
                           (setf (gethash "name" ht) (string name)))
                         (vector-push-extend ht entries)))
                     cat)
            (setf (gethash "catalog" llm) entries)
            (remhash "catalogue" llm)))))))

(defun %normalize-watch (ks)
  (let ((w (%table-get ks "watch")))
    (when (and w (not (stringp w)))
      (setf (gethash "watch" ks)
            (%prin1-string
             (mapcar (lambda (x)
                       (intern (string-upcase (string x)) :keyword))
                     (coerce (%as-sequence w) 'list)))))))

(defun %normalize-tool-grants (ks)
  (let ((g (%table-get ks "tool-grants")))
    (when g
      (setf (gethash "tool-grants" ks)
            (map 'vector
                 (lambda (x)
                   (cond
                     ((hash-table-p x) x)
                     ((stringp x) (%ht (list "op" x)))
                     ((symbolp x) (%ht (list "op" (string-downcase
                                                   (symbol-name x)))))
                     (t x)))
                 (%as-sequence g))))))

(defun %normalize-transports (table)
  (let ((serve (%table-get table "serve")))
    (when (hash-table-p serve)
      (let ((tr (%table-get serve "transports")))
        (when (stringp tr)
          (setf (gethash "transports" serve) (vector tr)))))))

(defun %normalize-config-table (table)
  (unless (hash-table-p table)
    (error 'expert-config-error
           :message "expert.toml root must be a table"))
  (%normalize-llm-catalog table)
  (%normalize-transports table)
  (let ((profile (%table-get table "profile")))
    (when (stringp profile)
      (setf (gethash "profile" table)
            (%ht (list "kind" profile)))))
  (map nil (lambda (ks)
             (when (hash-table-p ks)
               (%normalize-watch ks)
               (%normalize-tool-grants ks)))
       (%as-sequence (%table-get table "ks")))
  table)

(defun %wrap-schema-error (err path)
  (let* ((issues (ignore-errors (schema:schema-validation-error-issues err)))
         (unknown (find-if (lambda (i)
                             (search "unexpected field"
                                     (or (schema:schema-issue-message i) "")
                                     :test #'char-equal))
                           issues)))
    (if unknown
        (error 'unknown-expert-config-key
               :key (first (schema:schema-issue-path unknown))
               :valid-keys nil
               :path path
               :issues issues
               :message (format nil "~a" err))
        (error 'expert-config-error
               :path path
               :issues issues
               :message (format nil "~a" err)
               :cause err))))

(defun parse-expert-config (source &key path)
  "Parse SOURCE (hash-table, pathname, or TOML string) → EXPERT-CONFIG.
   Unknown keys signal UNKNOWN-EXPERT-CONFIG-KEY with a CONTINUE restart
   that lists valid keys and proceeds."
  (%ensure-toml-backend)
  (let* ((path (or path
                   (and (or (pathnamep source) (stringp source))
                        (probe-file source)
                        (pathname source))))
         (table (cond
                  ((hash-table-p source) source)
                  ((or (pathnamep source) (and (stringp source)
                                               (probe-file source)))
                   (toml:decode (pathname source)))
                  ((stringp source) (toml:decode source))
                  (t (error 'expert-config-error
                            :path path
                            :message (format nil "cannot decode expert.toml from ~s"
                                             (type-of source)))))))
    (%normalize-config-table table)
    (%strip-unknown-keys 'expert-config table)
    (handler-case
        (schema:parse 'expert-config table :coerce t)
      (schema:schema-validation-error (err)
        (%wrap-schema-error err path)))))

(defun %catalogue-name (config)
  (let* ((ex (expert-config-expert config))
         (raw (or (and ex (plusp (length (expert-config-expert-catalogue ex)))
                       (expert-config-expert-catalogue ex))
                  (and ex (plusp (length (expert-config-expert-catalog ex)))
                       (expert-config-expert-catalog ex))
                  "world")))
    (string-downcase (string-trim '(#\: #\Space) raw))))

(defun %profile-kind (config)
  (let* ((p (expert-config-profile config))
         (kind (and p (expert-config-profile-kind p))))
    (cond
      ((null kind) :personal)
      ((member kind '("corporate" :corporate) :test #'equal)
       :corporate)
      ((eq kind :corporate) :corporate)
      (t :personal))))

(defun %maybe (object reader)
  (and object (ignore-errors (funcall reader object))))

(defun %profile-has-overrides-p (p)
  (and p (or (%maybe p #'expert-config-profile-agenda)
             (%maybe p #'expert-config-profile-ksar)
             (%maybe p #'expert-config-profile-session)
             (%maybe p #'expert-config-profile-paths)
             (%maybe p #'expert-config-profile-improve)
             (%maybe p #'expert-config-profile-corporate))))

(defun %env-or (name)
  (let ((v (and name (plusp (length name)) (uiop:getenv name))))
    (and v (plusp (length v)) v)))

(defun %llm-entry-plist (e)
  (list :name (or (expert-config-llm-entry-name e) "")
        :kind (or (let ((k (expert-config-llm-entry-kind e)))
                    (and k (plusp (length k)) k))
                  (let ((k (expert-config-llm-entry-type e)))
                    (and k (plusp (length k)) k))
                  (expert-config-llm-entry-backend e))
        :model (or (let ((m (expert-config-llm-entry-model e)))
                     (and m (plusp (length m)) m))
                   (expert-config-llm-entry-default-model e))
        :default-model (expert-config-llm-entry-default-model e)
        :prefix (expert-config-llm-entry-prefix e)
        :base-url (or (let ((u (expert-config-llm-entry-base-url e)))
                        (and u (plusp (length u)) u))
                      (expert-config-llm-entry-endpoint e))
        :endpoint (expert-config-llm-entry-endpoint e)
        :api-key (or (let ((k (expert-config-llm-entry-api-key e)))
                       (and k (plusp (length k)) k))
                     (%env-or (expert-config-llm-entry-api-key-env e)))
        :api-key-env (expert-config-llm-entry-api-key-env e)
        :model-path (expert-config-llm-entry-model-path e)))

(defun %llm-catalog-entries (config)
  (let ((section (%maybe config #'expert-config-llm)))
    (when section
      (or (expert-config-llm-catalog section)
          (expert-config-llm-catalogue section)))))

(defun %apply-llm-section (cfg config)
  (let ((section (%maybe config #'expert-config-llm)))
    (when section
      (let ((default (expert-config-llm-default-model section)))
        (when (and default (plusp (length default)))
          (setf (demiurge-config-llm-default-model cfg) default)))
      (let ((entries (%llm-catalog-entries config)))
        (when entries
          (setf (demiurge-config-llm-catalog cfg)
                (mapcar #'%llm-entry-plist entries))))))
  cfg)

(defun %bind-websearch-from-config (config)
  "Bind WEB:*WEBSEARCH-BACKEND* from [websearch] (searxng | mock)."
  (let ((ws (%maybe config #'expert-config-websearch)))
    (when ws
      (let* ((kind (string-downcase (or (expert-config-websearch-kind ws) "mock")))
             (url (expert-config-websearch-base-url ws)))
        (setf web:*websearch-backend*
              (if (member kind '("searxng" "searx" "live") :test #'equal)
                  (progn
                    (unless (demiurge::%ensure-http-backend)
                      (error 'expert-config-error
                             :message "[websearch] kind=searxng needs http-backend-dexador"))
                    (web:make-searxng-backend
                     :base-url (if (and url (plusp (length url)))
                                   url
                                   "http://127.0.0.1:8888")))
                  (web:make-mock-websearch-backend))))
      web:*websearch-backend*)))

(defun %config-profile (config &key hitl)
  (let ((kind (%profile-kind config))
        (p (expert-config-profile config)))
    (if (or hitl (%profile-has-overrides-p p) (eq kind :corporate)
            (%llm-catalog-entries config))
        (let ((cfg (make-instance 'demiurge-config)))
          (%apply-llm-section cfg config)
          (when p
            (let ((ag (expert-config-profile-agenda p))
                  (ks (expert-config-profile-ksar p))
                  (se (expert-config-profile-session p))
                  (pa (expert-config-profile-paths p))
                  (im (expert-config-profile-improve p)))
              (when (and ag (expert-config-agenda-max-concurrency ag))
                (setf (demiurge-config-agenda-max-concurrency cfg)
                      (expert-config-agenda-max-concurrency ag)))
              (when (and ks (expert-config-ksar-timeout-seconds ks))
                (setf (demiurge-config-ksar-timeout-seconds cfg)
                      (expert-config-ksar-timeout-seconds ks)))
              (when (and se (expert-config-session-window-turns se))
                (setf (demiurge-config-session-window-turns cfg)
                      (expert-config-session-window-turns se)))
              (when (and pa (expert-config-paths-data-dir pa)
                         (plusp (length (expert-config-paths-data-dir pa))))
                (setf (demiurge-config-paths-data-dir cfg)
                      (expert-config-paths-data-dir pa)))
              (when (and im (expert-config-improve-overrides-enabled im))
                (setf (demiurge-config-improve-enabled cfg)
                      (expert-config-improve-overrides-enabled im)))))
          (if (eq kind :corporate)
              (make-corporate-profile :config cfg :require-hitl-p (and hitl t))
              (make-personal-profile :config cfg :require-hitl-p (and hitl t))))
        kind)))

(defun %grants-string (ks)
  (let ((grants (expert-config-ks-tool-grants ks))
        (world (expert-config-ks-world ks))
        (compute (expert-config-ks-compute ks)))
    (if (and (null grants) (null world) (null compute))
        "()"
        (%prin1-string
         (append (mapcar (lambda (g)
                           (list :op (expert-config-tool-grant-op g)
                                 :world (expert-config-tool-grant-world g)
                                 :compute (expert-config-tool-grant-compute g)))
                         grants)
                 (when world (list (list :op "world" :world t)))
                 (when compute (list (list :op "compute" :compute t))))))))

(defun %ks-defs-from-config (config)
  (mapcar (lambda (ks)
            (make-bundle-ks-definition
             :name (expert-config-ks-name ks)
             :kind (or (expert-config-ks-kind ks) "agent-ks")
             :watch (or (expert-config-ks-watch ks) "(:prompt)")
             :prompt-key (or (expert-config-ks-prompt-key ks) "prompt")
             :result-key (or (expert-config-ks-result-key ks) "result")
             :instructions (let ((ins (expert-config-ks-instructions ks))
                                 (prompt (expert-config-ks-prompt ks)))
                             (if (and ins (plusp (length ins)))
                                 ins
                                 (or prompt "")))
             :skill-ref (or (expert-config-ks-skill ks) "")
             :tool-grants (%grants-string ks)
             :mcp-url (or (expert-config-ks-mcp-url ks) "")
             :split-ratio (let ((r (expert-config-ks-split-ratio ks)))
                            (cond
                              ((null r) 0)
                              ((integerp r) r)
                              ((realp r) (max 0 (round r)))
                              (t 0)))))
          (or (expert-config-ks config) '())))

(defun %resolve-path (spec base-dir)
  (cond
    ((or (null spec) (zerop (length spec))) nil)
    ((uiop:absolute-pathname-p spec) (pathname spec))
    (t
     (or (and base-dir (probe-file (merge-pathnames spec base-dir)))
         (probe-file (asdf:system-relative-pathname "demiurge" spec))
         (and base-dir (merge-pathnames spec base-dir))
         (pathname spec)))))

(defun %corpus-location (c)
  (or (let ((s (expert-config-corpus-spec c)))
        (and s (plusp (length s)) s))
      (let ((s (expert-config-corpus-root c)))
        (and s (plusp (length s)) s))
      (let ((s (expert-config-corpus-path c)))
        (and s (plusp (length s)) s))
      (let ((s (expert-config-corpus-uri c)))
        (and s (plusp (length s)) s))
      ""))

(defun %corpus-refs-from-config (config base-dir)
  "Resolve [[corpus]] file specs to pathnames (dirs preferred for pack)."
  (loop for c in (or (expert-config-corpus config) '())
        for kind = (string-downcase (or (expert-config-corpus-kind c) "file"))
        for loc = (%corpus-location c)
        for resolved = (%resolve-path loc base-dir)
        when (and resolved (or (equal kind "file") (equal kind "snapshot")))
          collect (if (uiop:file-exists-p resolved)
                      (namestring resolved)
                      (namestring (uiop:ensure-directory-pathname resolved)))
        when (equal kind "imap")
          collect (format nil "imap://~a/~a"
                          (or (expert-config-corpus-host c) "localhost")
                          (or (expert-config-corpus-mailbox c) "INBOX"))
        when (equal kind "s3")
          collect (format nil "s3://~a/~a"
                          (or (expert-config-corpus-bucket c) "")
                          (or (expert-config-corpus-prefix c) ""))))

(defun %corpus-sources-from-config (config)
  (mapcar (lambda (c)
            (make-bundle-corpus-source
             :kind (or (expert-config-corpus-kind c) "file")
             :spec (%corpus-location c)
             :items nil))
          (or (expert-config-corpus config) '())))

(defun %load-skill-from-path (path name)
  (cond
    ((and path (uiop:file-exists-p path))
     (steer:load-skill path))
    ((and path (uiop:directory-exists-p path))
     (let ((md (merge-pathnames "SKILL.md"
                                (uiop:ensure-directory-pathname path))))
       (if (probe-file md)
           (steer:load-skill md)
           (steer:make-steer-skill (or name (file-namestring path))))))
    (t (steer:make-steer-skill (or name "skill")))))

(defun %skills-from-config (config base-dir)
  (loop for s in (or (expert-config-skill config) '())
        for name = (expert-config-skill-name s)
        for path = (%resolve-path (expert-config-skill-path s) base-dir)
        collect (if (and path (or (uiop:file-exists-p path)
                                  (uiop:directory-exists-p path)))
                    (%load-skill-from-path path name)
                    (steer:make-steer-skill
                     (if (and name (plusp (length name))) name "skill")))))

(defun %skill-names-from-config (config)
  (mapcar #'expert-config-skill-name
          (remove-if (lambda (s)
                       (or (null (expert-config-skill-name s))
                           (zerop (length (expert-config-skill-name s)))))
                     (or (expert-config-skill config) '()))))

(defun %eval-suites-from-config (config base-dir)
  (loop for e in (or (expert-config-eval config) '())
        for loc = (or (let ((p (expert-config-eval-path e)))
                        (and p (plusp (length p)) p))
                      (let ((p (expert-config-eval-dataset e)))
                        (and p (plusp (length p)) p)))
        for resolved = (and loc (%resolve-path loc base-dir))
        collect (cond
                  ((and resolved (probe-file resolved))
                   (let ((text (uiop:read-file-string resolved)))
                     (handler-case
                         (eval:load-dataset resolved :format :json)
                       (error ()
                         (handler-case
                             (eval:load-dataset text :format :sexp)
                           (error ()
                             (eval:make-eval-dataset
                              :name (expert-config-eval-name e)
                              :cases nil)))))))
                  (t (eval:make-eval-dataset
                      :name (expert-config-eval-name e)
                      :cases nil)))))

(defun %eval-refs-from-config (config)
  (mapcar (lambda (e)
            (make-bundle-eval-dataset-ref
             :name (expert-config-eval-name e)
             :version (or (expert-config-eval-version e) "")
             :payload ""))
          (or (expert-config-eval config) '())))

(defun %config-to-manifest (config)
  (let ((ex (expert-config-expert config)))
    (make-expert-bundle-manifest
     :name (expert-config-expert-name ex)
     :catalogue-vocab (%prin1-string
                       (list (intern (string-upcase (%catalogue-name config))
                                     :keyword)))
     :ks-definitions (%ks-defs-from-config config)
     :skill-refs (mapcar (lambda (s)
                           (make-bundle-skill-ref
                            :name (or (expert-config-skill-name s) "")
                            :version (or (expert-config-skill-version s) "")
                            :digest (or (expert-config-skill-digest s) "")))
                         (or (expert-config-skill config) '()))
     :corpus-sources (%corpus-sources-from-config config)
     :eval-datasets (%eval-refs-from-config config)
     :profile-defaults (%prin1-string
                        (list :kind (%profile-kind config)))
     :annotations (or (expert-config-expert-description ex) ""))))

(defun load-expert-config (path &key profile llm (register nil) base-dir)
  "Load expert.toml at PATH → the same EXPERT-DOMAIN DEFEXPERT builds.
   Unknown keys signal UNKNOWN-EXPERT-CONFIG-KEY; CONTINUE ignores them
   and lists valid keys. Config may only reference declared ops/tools."
  (let* ((pathname (cond
                     ((pathnamep path) path)
                     ((stringp path) (or (probe-file path) (pathname path)))
                     (t (error 'expert-config-error
                               :message (format nil "path must be a pathname or string, got ~s"
                                                (type-of path))))))
         (base (or base-dir
                   (uiop:pathname-directory-pathname pathname)))
         (config (if (probe-file pathname)
                     (parse-expert-config pathname :path pathname)
                     (error 'expert-config-error
                            :path pathname
                            :message (format nil "expert.toml not found: ~a"
                                             pathname))))
         (improve (%maybe config #'expert-config-improve))
         (hitl (and improve (%maybe improve #'expert-config-improve-hitl)))
         (profile (or profile (%config-profile config :hitl hitl)))
         (llm (or llm (%llm-for profile nil) (llm:make-mock-llm-backend)))
         (skills (%skills-from-config config base))
         (steering (and skills (steer:coerce-steering skills)))
         (manifest (%config-to-manifest config))
         (domain (%domain-from-manifest
                  manifest
                  :profile profile
                  :llm llm
                  :steering steering
                  :skill-names (%skill-names-from-config config)
                  :eval-suites (%eval-suites-from-config config base)
                  :corpora (%corpus-refs-from-config config base))))
    (%bind-websearch-from-config config)
    (when register
      (register-expert domain))
    domain))

(setf (fdefinition 'demiurge:load-expert-config)
      (fdefinition 'load-expert-config))
(setf (documentation 'demiurge:load-expert-config 'function)
      (documentation 'load-expert-config 'function))
