(in-package #:demiurge)

(defparameter +default-config-toml+
  "[agenda]
max-concurrency = 4

[ksar]
timeout-seconds = 10

[session]
window-turns = 8

[llm]
default-model = \"mock\"

[paths]
data-dir = \"\"

[improve]
enabled = false

[corporate]
postgres.dsn = \"\"
otlp.endpoint = \"\"
tenant.id = \"\"

[corporate.oidc]
issuer = \"\"
client-id = \"\"

[corporate.ldap]
url = \"\"
base-dn = \"\"

[corporate.session]
secret = \"\"
kid = \"\"
previous-secret = \"\"
previous-kid = \"\"
issuer = \"\"
audience = \"\"
")

(defclass demiurge-config ()
  ((agenda-max-concurrency
    :initarg :agenda-max-concurrency
    :accessor demiurge-config-agenda-max-concurrency
    :initform 4)
   (ksar-timeout-seconds
    :initarg :ksar-timeout-seconds
    :accessor demiurge-config-ksar-timeout-seconds
    :initform 10)
   (session-window-turns
    :initarg :session-window-turns
    :accessor demiurge-config-session-window-turns
    :initform 8)
   (llm-default-model
    :initarg :llm-default-model
    :accessor demiurge-config-llm-default-model
    :initform "mock")
   (llm-catalog
    :initarg :llm-catalog
    :accessor demiurge-config-llm-catalog
    :initform nil)
   (paths-data-dir
    :initarg :paths-data-dir
    :accessor demiurge-config-paths-data-dir
    :initform nil)
   (workspace-root
    :initarg :workspace-root
    :accessor demiurge-config-workspace-root
    :initform nil)
   (workspace-seed
    :initarg :workspace-seed
    :accessor demiurge-config-workspace-seed
    :initform nil)
   (improve-enabled
    :initarg :improve-enabled
    :accessor demiurge-config-improve-enabled
    :initform nil)
   (corporate-oidc-issuer
    :initarg :corporate-oidc-issuer
    :accessor demiurge-config-corporate-oidc-issuer
    :initform nil)
   (corporate-oidc-client-id
    :initarg :corporate-oidc-client-id
    :accessor demiurge-config-corporate-oidc-client-id
    :initform nil)
   (corporate-ldap-url
    :initarg :corporate-ldap-url
    :accessor demiurge-config-corporate-ldap-url
    :initform nil)
   (corporate-ldap-base-dn
    :initarg :corporate-ldap-base-dn
    :accessor demiurge-config-corporate-ldap-base-dn
    :initform nil)
   (corporate-ldap-group-role-map
    :initarg :corporate-ldap-group-role-map
    :accessor demiurge-config-corporate-ldap-group-role-map
    :initform nil)
   (corporate-postgres-dsn
    :initarg :corporate-postgres-dsn
    :accessor demiurge-config-corporate-postgres-dsn
    :initform nil)
   (corporate-otlp-endpoint
    :initarg :corporate-otlp-endpoint
    :accessor demiurge-config-corporate-otlp-endpoint
    :initform nil)
   (corporate-tenant-id
    :initarg :corporate-tenant-id
    :accessor demiurge-config-corporate-tenant-id
    :initform nil)
   (corporate-role-grants
    :initarg :corporate-role-grants
    :accessor demiurge-config-corporate-role-grants
    :initform nil)
   (corporate-session-secret
    :initarg :corporate-session-secret
    :accessor demiurge-config-corporate-session-secret
    :initform nil)
   (corporate-session-kid
    :initarg :corporate-session-kid
    :accessor demiurge-config-corporate-session-kid
    :initform nil)
   (corporate-session-previous-secret
    :initarg :corporate-session-previous-secret
    :accessor demiurge-config-corporate-session-previous-secret
    :initform nil)
   (corporate-session-previous-kid
    :initarg :corporate-session-previous-kid
    :accessor demiurge-config-corporate-session-previous-kid
    :initform nil)
   (corporate-session-issuer
    :initarg :corporate-session-issuer
    :accessor demiurge-config-corporate-session-issuer
    :initform nil)
   (corporate-session-audience
    :initarg :corporate-session-audience
    :accessor demiurge-config-corporate-session-audience
    :initform nil)
   (corporate-insecure-local
    :initarg :corporate-insecure-local
    :accessor demiurge-config-corporate-insecure-local
    :initform nil)
   (raw
    :initarg :raw
    :accessor demiurge-config-raw
    :initform nil)))

(defun demiurge-config-p (x)
  (typep x 'demiurge-config))

(defvar *demiurge-config* nil
  "Last config returned by LOAD-DEMIURGE-CONFIG.")

(defun %cfg-get (stack path &optional default)
  (let ((alt (substitute #\_ #\- (string path))))
    (cond
      ((null stack) default)
      ((not (eq alt path))
       (or (ignore-errors (cfg:get stack path))
           (ignore-errors (cfg:get stack alt))
           default))
      (t
       (or (ignore-errors (cfg:get stack path))
           default)))))

(defun %cfg-int (stack path default)
  (let ((v (%cfg-get stack path default)))
    (cond
      ((integerp v) v)
      ((stringp v) (parse-integer v :junk-allowed nil))
      (t default))))

(defun %cfg-bool (stack path default)
  (handler-case (cfg:get-boolean stack path)
    (error ()
      (let ((alt (substitute #\_ #\- (string path))))
        (handler-case (cfg:get-boolean stack alt)
          (error () default))))))

(defun %cfg-string (stack path default)
  (let ((v (%cfg-get stack path default)))
    (cond
      ((null v) default)
      ((stringp v) (if (zerop (length v)) default v))
      (t (princ-to-string v)))))

(defun %table-to-plist (table)
  (cond
    ((null table) nil)
    ((hash-table-p table)
     (let ((out nil))
       (maphash (lambda (k v)
                  (setf out (list* (intern (string-upcase (string k)) :keyword)
                                   (if (hash-table-p v) (%table-to-plist v) v)
                                   out)))
                table)
       out))
    ((listp table) table)
    (t table)))

(defun %cfg-catalog (stack)
  (let ((raw (or (%cfg-get stack "llm.catalog")
                 (%cfg-get stack "llm.catalogue"))))
    (cond
      ((null raw) nil)
      ((hash-table-p raw)
       (let ((out nil))
         (maphash (lambda (name spec)
                    (push (list* :name (string name)
                                 (%table-to-plist spec))
                          out))
                  raw)
         (nreverse out)))
      ((or (vectorp raw) (listp raw))
       (map 'list (lambda (spec)
                    (if (hash-table-p spec)
                        (%table-to-plist spec)
                        spec))
            raw))
      (t nil))))

(defun %as-string-list (value)
  (cond
    ((null value) nil)
    ((stringp value) (list value))
    ((and (vectorp value) (not (stringp value)))
     (map 'list (lambda (x) (if (stringp x) x (princ-to-string x))) value))
    ((listp value)
     (mapcar (lambda (x) (if (stringp x) x (princ-to-string x))) value))
    (t (list (princ-to-string value)))))

(defun %cfg-string-map (stack path)
  "Table at PATH → alist of (string . string)."
  (let ((raw (%cfg-get stack path)))
    (cond
      ((null raw) nil)
      ((hash-table-p raw)
       (let ((out nil))
         (maphash (lambda (k v)
                    (push (cons (string k)
                                (if (or (stringp v) (null v))
                                    v
                                    (princ-to-string v)))
                          out))
                  raw)
         (nreverse out)))
      ((and (listp raw) (keywordp (car raw)))
       (loop for (k v) on raw by #'cddr
             collect (cons (string-downcase (string k))
                           (if (or (stringp v) (null v))
                               v
                               (princ-to-string v)))))
      ((listp raw) raw)
      (t nil))))

(defun %cfg-role-grants (stack)
  "corporate.role-grants → alist of (role-string . op-string-list)."
  (let ((raw (%cfg-get stack "corporate.role-grants")))
    (cond
      ((null raw) nil)
      ((hash-table-p raw)
       (let ((out nil))
         (maphash (lambda (role ops)
                    (push (cons (string role) (%as-string-list ops)) out))
                  raw)
         (nreverse out)))
      ((and (listp raw) (keywordp (car raw)))
       (loop for (k v) on raw by #'cddr
             collect (cons (string-downcase (string k))
                           (%as-string-list v))))
      ((listp raw) raw)
      (t nil))))

(defun %config-from-stack (stack)
  (make-instance 'demiurge-config
                 :agenda-max-concurrency (%cfg-int stack "agenda.max-concurrency" 4)
                 :ksar-timeout-seconds (%cfg-int stack "ksar.timeout-seconds" 10)
                 :session-window-turns (%cfg-int stack "session.window-turns" 8)
                 :llm-default-model (%cfg-string stack "llm.default-model" "mock")
                 :llm-catalog (%cfg-catalog stack)
                 :paths-data-dir (%cfg-string stack "paths.data-dir" nil)
                 :workspace-root (%cfg-string stack "workspace.root" nil)
                 :workspace-seed (%cfg-string stack "workspace.seed" nil)
                 :improve-enabled (%cfg-bool stack "improve.enabled" nil)
                 :corporate-oidc-issuer (%cfg-string stack "corporate.oidc.issuer" nil)
                 :corporate-oidc-client-id (%cfg-string stack "corporate.oidc.client-id" nil)
                 :corporate-ldap-url (%cfg-string stack "corporate.ldap.url" nil)
                 :corporate-ldap-base-dn (%cfg-string stack "corporate.ldap.base-dn" nil)
                 :corporate-ldap-group-role-map
                 (%cfg-string-map stack "corporate.ldap.group-role-map")
                 :corporate-postgres-dsn (%cfg-string stack "corporate.postgres.dsn" nil)
                 :corporate-otlp-endpoint (%cfg-string stack "corporate.otlp.endpoint" nil)
                 :corporate-tenant-id (%cfg-string stack "corporate.tenant.id" nil)
                 :corporate-role-grants (%cfg-role-grants stack)
                 :corporate-session-secret
                 (%cfg-string stack "corporate.session.secret" nil)
                 :corporate-session-kid
                 (%cfg-string stack "corporate.session.kid" nil)
                 :corporate-session-previous-secret
                 (%cfg-string stack "corporate.session.previous-secret" nil)
                 :corporate-session-previous-kid
                 (%cfg-string stack "corporate.session.previous-kid" nil)
                 :corporate-session-issuer
                 (%cfg-string stack "corporate.session.issuer" nil)
                 :corporate-session-audience
                 (%cfg-string stack "corporate.session.audience" nil)
                 :corporate-insecure-local
                 (%cfg-bool stack "corporate.insecure-local" nil)
                 :raw stack))

(defun %write-default-toml (path)
  (with-open-file (out path :direction :output :if-exists :supersede
                       :if-does-not-exist :create)
    (write-string +default-config-toml+ out))
  path)

(defun load-demiurge-config (&key path (prefix "DEMIURGE") (env t) environ)
  "Load TOML at PATH (or built-in defaults) via cl-stack-config.
   Precedence: defaults < file < env (PREFIX_TABLE__KEY). → DEMIURGE-CONFIG."
  (let* ((source (or (and path (probe-file path) path)
                     (and (null path)
                          (let ((from-env (uiop:getenv "DEMIURGE_CONFIG")))
                            (and from-env (probe-file from-env))))))
         (stack
          (if source
              (cfg:load-config source :prefix prefix :env env :environ environ)
              (uiop:with-temporary-file (:pathname tmp :prefix "demiurge-cfg-"
                                        :type "toml")
                (%write-default-toml tmp)
                (cfg:load-config tmp :prefix prefix :env env :environ environ)))))
    (let ((obj (%config-from-stack stack)))
      (setf *demiurge-config* obj)
      obj)))

(defun current-demiurge-config ()
  (or *demiurge-config* (load-demiurge-config :env nil)))
