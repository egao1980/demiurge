(in-package #:demiurge)

;;; Corporate deployment profile: OIDC + LDAP authz, tenant scope, Postgres
;;; when a DSN is present, OTLP observability. Tests run without Postgres
;;; (sqlite / memory fallbacks) and without a live IdP (canned discovery).

(defvar *tenant* nil
  "Dynamic tenant scope key bound per request (corporate profile).")

(defvar *principal* nil
  "Authenticated subject (OIDC sub) bound per request.")

(defvar *principal-roles* nil
  "Role name strings for *PRINCIPAL*.")

(defvar *principal-catalogue* nil
  "Filtered capability-catalogue for the current principal, or NIL.")

(defvar *capability-denial-audit* nil
  "Newest-first plist records of CAPABILITY-DENIED signals.")

(defvar *oidc-http-fn* nil
  "Optional (lambda (url) json) for OIDC discovery / JWKS (tests).")

(defvar *oidc-token-exchange-fn* nil
  "Optional (lambda (code profile) id-token-string) (tests).")

(defclass corporate-profile (deployment-profile)
  ((tenant :initarg :tenant :accessor profile-tenant :initform "default")
   (oidc-discovery :initarg :oidc-discovery
                   :accessor corporate-profile-oidc-discovery
                   :initform nil)
   (oidc-jwks :initarg :oidc-jwks
              :accessor corporate-profile-oidc-jwks
              :initform nil)
   (oidc-http :initarg :oidc-http
              :accessor corporate-profile-oidc-http
              :initform nil)
   (oidc-key :initarg :oidc-key
             :accessor corporate-profile-oidc-key
             :initform nil)
   (oidc-algorithms :initarg :oidc-algorithms
                    :accessor corporate-profile-oidc-algorithms
                    :initform '("RS256"))
   (token-exchange :initarg :token-exchange
                   :accessor corporate-profile-token-exchange
                   :initform nil)
   (session-secret :initarg :session-secret
                   :accessor corporate-profile-session-secret
                   :initform nil)
   (session-kid :initarg :session-kid
                :accessor corporate-profile-session-kid
                :initform "k1")
   (session-keys :initarg :session-keys
                 :accessor corporate-profile-session-keys
                 :initform nil)
   (session-issuer :initarg :session-issuer
                   :accessor corporate-profile-session-issuer
                   :initform "demiurge")
   (session-audience :initarg :session-audience
                     :accessor corporate-profile-session-audience
                     :initform "demiurge-session")
   (insecure-local-p :initarg :insecure-local-p
                     :accessor corporate-profile-insecure-local-p
                     :initform nil)
   (ldap-directory :initarg :ldap-directory
                   :accessor corporate-profile-ldap-directory
                   :initform nil)
   (group-role-map :initarg :group-role-map
                   :accessor corporate-profile-group-role-map
                   :initform nil)
   (role-grants :initarg :role-grants
                :accessor corporate-profile-role-grants
                :initform nil)
   (pending :initarg :pending
            :accessor corporate-profile-pending
            :initform (make-hash-table :test #'equal))
   (migrate-directory :initarg :migrate-directory
                      :accessor corporate-profile-migrate-directory
                      :initform nil)
   (claim-sql :initarg :claim-sql
              :accessor corporate-profile-claim-sql
              :initform nil))
  (:default-initargs :kind :corporate))

(defun corporate-profile-p (x)
  (typep x 'corporate-profile))

;;; ---------------------------------------------------------------------------
;;; Tenancy
;;; ---------------------------------------------------------------------------

(defun current-tenant (&optional default)
  (or *tenant* default))

(defmacro with-tenant (tenant &body body)
  `(let ((*tenant* ,tenant))
     ,@body))

(defun tenant-scope (kind name &optional (tenant (current-tenant)))
  "KIND/NAME scoped by TENANT when bound: tenant/<tenant>/<kind>/<name>."
  (let ((kind (string-downcase (string kind)))
        (name (string name)))
    (if tenant
        (format nil "tenant/~a/~a/~a" tenant kind name)
        (format nil "~a/~a" kind name))))

(defun tenant-of-reference (id)
  "Tenant segment of a tenant/<tenant>/… id, or NIL."
  (let ((s (string id)))
    (when (and (>= (length s) 8)
               (string= s "tenant/" :end1 7))
      (let* ((rest (subseq s 7))
             (slash (position #\/ rest)))
        (if slash (subseq rest 0 slash) rest)))))

(defun assert-tenant-scope (id &optional (tenant (current-tenant)))
  "Hard error when ID names a different tenant than TENANT."
  (let ((got (tenant-of-reference id)))
    (when (and tenant got (not (string= (string tenant) (string got))))
      (error 'tenant-isolation-error
             :expected tenant
             :actual got
             :reference id
             :message (format nil "cross-tenant reference ~S" id)))
    id))

(defun tenant-session-id (name &optional (tenant (current-tenant)))
  (if tenant
      (tenant-scope "session" name tenant)
      (string name)))

(defun tenant-task-id (name &optional (tenant (current-tenant)))
  (if tenant
      (tenant-scope "domain" name tenant)
      (format nil "domain/~a" name)))

(defun tenant-corpus-name (name &optional (tenant (current-tenant)))
  (if tenant
      (tenant-scope "corpus" name tenant)
      (string name)))

(defun tenant-budget-scope (&rest extra)
  (list* :tenant (or (current-tenant) "default") extra))

;;; ---------------------------------------------------------------------------
;;; AuthZ — filtered catalogue (never mutate the root)
;;; ---------------------------------------------------------------------------

(defun operation-granted-p (op-name allowed-ops &optional cap-name)
  (cond
    ((or (eq allowed-ops t) (eq allowed-ops :all)) t)
    ((null allowed-ops) nil)
    (t
     (let ((op (string op-name))
           (cap (and cap-name (string cap-name))))
       (some (lambda (item)
               (cond
                 ((and (consp item) (not (keywordp (car item))))
                  (and cap
                       (string-equal cap (string (car item)))
                       (string-equal op (string (cdr item)))))
                 ((and (stringp item) (find #\/ item))
                  (let ((slash (position #\/ item)))
                    (and cap
                         (string-equal cap (subseq item 0 slash))
                         (string-equal op (subseq item (1+ slash))))))
                 (t (string-equal op (string item)))))
             allowed-ops)))))

(defun role-allowed-ops (roles grants)
  "Union of ops granted to ROLES from GRANTS alist/hash. :all short-circuits."
  (let ((roles (if (listp roles) roles (list roles)))
        (ops nil))
    (flet ((granted (role)
             (cond
               ((hash-table-p grants)
                (or (gethash role grants)
                    (gethash (string role) grants)
                    (gethash (string-downcase (string role)) grants)))
               ((listp grants)
                (or (cdr (assoc role grants :test #'string-equal))
                    (getf grants (intern (string-upcase (string role)) :keyword)))))))
      (dolist (role roles ops)
        (let ((g (granted role)))
          (when (or (eq g t) (eq g :all))
            (return-from role-allowed-ops :all))
          (setf ops (union ops (%as-string-list g) :test #'string-equal)))))))

(defun record-capability-denial (&key capability operation principal tenant)
  (let ((entry (list :capability (and capability
                                      (ignore-errors (cap:capability-name capability)))
                     :op operation
                     :principal (or principal *principal*)
                     :tenant (or tenant *tenant*)
                     :time (get-universal-time))))
    (push entry *capability-denial-audit*)
    (when log:*log-backend*
      (ignore-errors
        (log:warn "capability denied"
                  :op operation
                  :principal (or principal *principal*)
                  :tenant (or tenant *tenant*))))
    entry))

(defclass granted-capability (cap:capability)
  ((inner :initarg :inner :reader granted-capability-inner)
   (allowed-ops :initarg :allowed-ops :reader granted-capability-allowed-ops
                :initform nil)
   (principal :initarg :principal :reader granted-capability-principal
              :initform nil)
   (roles :initarg :roles :reader granted-capability-roles :initform nil)
   (tenant :initarg :tenant :reader granted-capability-tenant :initform nil)
   (audit :initarg :audit :accessor granted-capability-audit :initform nil)))

(defmethod cap:capability-operations ((cap granted-capability))
  (remove-if-not
   (lambda (op)
     (operation-granted-p (cap:capability-operation-name op)
                          (granted-capability-allowed-ops cap)
                          (cap:capability-name cap)))
   (copy-list (cap:capability-operations (granted-capability-inner cap)))))

(defmethod cap:invoke-operation ((cap granted-capability) op-name &rest args)
  (unless (operation-granted-p op-name
                               (granted-capability-allowed-ops cap)
                               (cap:capability-name cap))
    (record-capability-denial :capability cap
                              :operation op-name
                              :principal (granted-capability-principal cap)
                              :tenant (granted-capability-tenant cap))
    (let ((cell (granted-capability-audit cap)))
      (when (consp cell)
        (push (list :capability (cap:capability-name cap)
                    :op op-name
                    :principal (granted-capability-principal cap)
                    :tenant (granted-capability-tenant cap))
              (cdr cell))))
    (restart-case
        (error 'capability-denied
               :capability cap
               :operation op-name
               :principal (or (granted-capability-principal cap) *principal*)
               :tenant (or (granted-capability-tenant cap) *tenant*)
               :message (format nil "operation ~S is not granted" op-name))
      (use-value (value)
        :report "Use a supplied result instead"
        (return-from cap:invoke-operation value))))
  (apply #'cap:invoke-operation (granted-capability-inner cap) op-name args))

(defun make-principal-catalogue (source &key allowed-ops principal roles tenant)
  "Fresh catalogue copy whose ops are filtered by ALLOWED-OPS.
   Never mutates SOURCE (same gotcha as MAKE-RESTRICTED-CATALOGUE)."
  (check-type source cap:capability-catalogue)
  (let* ((copy (cap:make-capability-catalogue
                :name (cap:catalogue-name source)
                :description (cap:catalogue-description source)
                :defined-names (copy-list (cap:catalogue-defined-names source))))
         (audit (cons :denials nil)))
    (dolist (row (cap:list-capabilities source))
      (let ((inner (cap:get-capability source (getf row :name))))
        (when inner
          (cap:register-capability
           copy
           (make-instance 'granted-capability
                          :name (cap:capability-name inner)
                          :version (cap:capability-version inner)
                          :description (cap:capability-description inner)
                          :inner inner
                          :allowed-ops allowed-ops
                          :principal principal
                          :roles roles
                          :tenant (or tenant *tenant*)
                          :audit audit)))))
    copy))

(defun filter-catalogue-for-roles (source roles grants &key principal tenant)
  (make-principal-catalogue source
                            :allowed-ops (role-allowed-ops roles grants)
                            :principal principal
                            :roles roles
                            :tenant tenant))

(defun catalogue-for-request (&optional domain)
  (or *principal-catalogue*
      (and domain *principal*
           (let* ((profile (and (expert-domain-p domain)
                                (expert-profile domain)))
                  (grants (or (and (corporate-profile-p profile)
                                   (corporate-profile-role-grants profile))
                              (and (deployment-profile-p profile)
                                   (profile-config profile)
                                   (demiurge-config-corporate-role-grants
                                    (profile-config profile))))))
             (when (and grants (expert-domain-p domain))
               (filter-catalogue-for-roles (expert-catalogue domain)
                                           *principal-roles*
                                           grants
                                           :principal *principal*
                                           :tenant *tenant*))))
      (and (expert-domain-p domain) (expert-catalogue domain))))

;;; ---------------------------------------------------------------------------
;;; LDAP group → role
;;; ---------------------------------------------------------------------------

(defun %ldap-attr (entry name)
  (let ((attrs (and entry (ldap:ldap-entry-attributes entry))))
    (cdr (or (assoc name attrs :test #'string-equal)
             (assoc (string-downcase (string name)) attrs :test #'string-equal)))))

(defun %cn-of-dn (dn)
  (let* ((s (string dn))
         (comma (position #\, s))
         (head (if comma (subseq s 0 comma) s))
         (eq-pos (position #\= head)))
    (if eq-pos (subseq head (1+ eq-pos)) head)))

(defun %map-get (map key)
  (cond
    ((null map) nil)
    ((hash-table-p map)
     (or (gethash key map)
         (gethash (string-downcase (string key)) map)))
    ((listp map)
     (or (cdr (assoc key map :test #'string-equal))
         (getf map (intern (string-upcase (string key)) :keyword))))))

(defun ldap-groups-for-dn (directory dn &key base-dn)
  "memberOf on the entry plus groups under BASE-DN that list DN as member."
  (let ((groups nil))
    (when (and directory dn)
      (let ((hits (ignore-errors
                    (ldap:ldap-search directory :base dn :scope :base))))
        (dolist (val (%ldap-attr (first hits) "memberOf"))
          (push val groups)))
      (when (and base-dn (plusp (length (string base-dn))))
        (dolist (hit (or (ignore-errors
                           (ldap:ldap-search
                            directory
                            :base base-dn
                            :scope :sub
                            :filter `(:or (= "member" ,dn)
                                          (= "uniqueMember" ,dn))))
                         nil))
          (push (ldap:ldap-entry-dn hit) groups))))
    (remove-duplicates groups :test #'string-equal)))

(defun map-groups-to-roles (groups group-role-map)
  (let ((roles nil))
    (dolist (g groups)
      (let ((role (or (%map-get group-role-map g)
                      (%map-get group-role-map (%cn-of-dn g)))))
        (when role
          (push (string role) roles))))
    (remove-duplicates roles :test #'string-equal)))

(defun %principal-dn (profile subject)
  (let* ((dir (corporate-profile-ldap-directory profile))
         (base (and (profile-config profile)
                    (demiurge-config-corporate-ldap-base-dn
                     (profile-config profile))))
         (hits (and dir
                    (ignore-errors
                      (ldap:ldap-search
                       dir
                       :base (or base "")
                       :scope :sub
                       :filter `(:or (= "uid" ,subject)
                                     (= "cn" ,subject)
                                     (= "mail" ,subject)))))))
    (if hits
        (ldap:ldap-entry-dn (first hits))
        (if (and base (plusp (length (string base))))
            (format nil "cn=~a,~a" subject base)
            (format nil "cn=~a" subject)))))

(defun resolve-principal-roles (profile subject &optional claims)
  (or (let ((dir (corporate-profile-ldap-directory profile)))
        (when dir
          (let* ((dn (%principal-dn profile subject))
                 (base (and (profile-config profile)
                            (demiurge-config-corporate-ldap-base-dn
                             (profile-config profile))))
                 (groups (ldap-groups-for-dn dir dn :base-dn base)))
            (map-groups-to-roles groups
                                 (or (corporate-profile-group-role-map profile)
                                     (and (profile-config profile)
                                          (demiurge-config-corporate-ldap-group-role-map
                                           (profile-config profile))))))))
      (let ((raw (and claims
                      (or (cdr (assoc "roles" claims :test #'string=))
                          (cdr (assoc "groups" claims :test #'string=))))))
        (%as-string-list raw))))

;;; ---------------------------------------------------------------------------
;;; Postgres DSN + SKIP LOCKED claim
;;; ---------------------------------------------------------------------------

(defun parse-postgres-dsn (dsn)
  "Parse postgres://user:pass@host:port/db → sql-protocol connect keys."
  (let ((s (string-trim '(#\Space) (or (and dsn (string dsn)) ""))))
    (when (plusp (length s))
      (let* ((rest (cond
                     ((and (>= (length s) 11)
                           (string-equal s "postgres://" :end1 11))
                      (subseq s 11))
                     ((and (>= (length s) 13)
                           (string-equal s "postgresql://" :end1 13))
                      (subseq s 13))
                     (t s)))
             (qpos-rest (position #\? rest))
             (rest (if qpos-rest (subseq rest 0 qpos-rest) rest))
             (at (position #\@ rest))
             (userinfo (and at (subseq rest 0 at)))
             (hostpart (if at (subseq rest (1+ at)) rest))
             (slash (position #\/ hostpart))
             (auth (if slash (subseq hostpart 0 slash) hostpart))
             (path (and slash (subseq hostpart (1+ slash))))
             (colon-h (position #\: auth :from-end t))
             (ipv6 (and (plusp (length auth)) (char= (char auth 0) #\[)))
             (host (cond
                     (ipv6
                      (let ((rb (position #\] auth)))
                        (if rb (subseq auth 1 rb) auth)))
                     (colon-h (subseq auth 0 colon-h))
                     (t auth)))
             (port (cond
                     (ipv6
                      (let ((rb (position #\] auth)))
                        (if (and rb (< (1+ rb) (length auth))
                                 (char= (char auth (1+ rb)) #\:))
                            (parse-integer (subseq auth (+ rb 2)) :junk-allowed t)
                            5432)))
                     (colon-h
                      (or (parse-integer (subseq auth (1+ colon-h))
                                         :junk-allowed t)
                          5432))
                     (t 5432)))
             (colon-u (and userinfo (position #\: userinfo))))
        (list :host (if (plusp (length host)) host "127.0.0.1")
              :port port
              :database-name (if (and path (plusp (length path))) path "postgres")
              :username (and userinfo
                             (if colon-u (subseq userinfo 0 colon-u) userinfo))
              :password (and userinfo colon-u (subseq userinfo (1+ colon-u))))))))

(defun postgres-claimable-lease-sql (&optional (lease-table "task_lease"))
  "Postgres worker-lease SELECT … FOR UPDATE SKIP LOCKED.
   task-backend-sql CLAIM-TASK stays SQLite-friendly; this is local."
  (format nil "SELECT task_id, worker_id, lease_until, heartbeat_at, status
 FROM ~a
 WHERE (lease_until IS NULL OR lease_until < ?)
   AND (status IS NULL OR status IN ('new', 'running', 'waiting'))
 ORDER BY task_id
 LIMIT 1
 FOR UPDATE SKIP LOCKED"
          lease-table))

(defun claim-task-postgres (journal worker-id &key (now (get-universal-time))
                                               (lease-seconds 30)
                                               (lease-table nil))
  "Claim one task using SKIP LOCKED. JOURNAL must have a sql-protocol connection."
  (check-type worker-id string)
  (let* ((table (or lease-table
                    (ignore-errors (tbsql:sql-journal-lease-table journal))
                    "task_lease"))
         (sql (postgres-claimable-lease-sql table))
         (conn (tbsql:sql-journal-connection journal))
         (row (sql-protocol:fetch
               (sql-protocol:execute conn sql (list now)))))
    (when row
      (let ((task-id (or (getf row :task_id) (getf row :|TASK_ID|)))
            (until (+ now lease-seconds)))
        (sql-protocol:execute
         conn
         (format nil "UPDATE ~a SET worker_id = ?, lease_until = ?, heartbeat_at = ?
 WHERE task_id = ?"
                 table)
         (list worker-id until now task-id))
        task-id))))

;;; ---------------------------------------------------------------------------
;;; Storage helpers
;;; ---------------------------------------------------------------------------

(defun %ensure-postgres-backend ()
  (or (find-package '#:sql-backend-postgres)
      (and (%try-load "sql-backend-postgres")
           (find-package '#:sql-backend-postgres))))

(defun %open-postgres-connection (dsn)
  (let ((keys (parse-postgres-dsn dsn)))
    (unless (and keys (%ensure-postgres-backend))
      (return-from %open-postgres-connection nil))
    (ignore-errors
      (apply #'sql-protocol:connect :driver :postgres keys))))

(defun %tenant-ident (tenant)
  (let ((s (substitute #\_ #\- (string-downcase (string (or tenant "default"))))))
    (unless (and (plusp (length s))
                 (alpha-char-p (char s 0))
                 (every (lambda (c) (or (alphanumericp c) (char= c #\_))) s))
      (error 'tenant-isolation-error
             :expected tenant
             :actual s
             :reference tenant
             :message (format nil "invalid tenant identifier ~S" tenant)))
    s))

(defun %maybe-tenant-migrations (conn tenant)
  (unless (and tenant (%try-load "sql-migrate") (%try-load "sql-orm"))
    (return-from %maybe-tenant-migrations nil))
  (let* ((make-dir (find-symbol "MAKE-SCRIPT-DIRECTORY" :sql-migrate))
         (reg (find-symbol "REGISTER-REVISION" :sql-migrate))
         (mig-class (find-symbol "SCHEMA-MIGRATION" :sql-orm))
         (ident (%tenant-ident tenant))
         (dir (and make-dir
                   (funcall make-dir
                            :version-table
                            (format nil "~a_sql_migrate_version" ident)))))
    (when (and dir reg mig-class)
      (funcall reg dir
               (make-instance mig-class
                              :name "tenant-schema"
                              :revision "0001"
                              :down-revision nil
                              :ops nil))
      (let ((sess (find-symbol "MAKE-SESSION-REVISION" :conversation-backend-sql))
            (jour (find-symbol "MAKE-JOURNAL-REVISION" :task-backend-sql)))
        (when (and sess (fboundp sess)) (funcall sess dir))
        (when (and jour (fboundp jour)) (funcall jour dir))))
    (when (and conn dir)
      (ignore-errors
        (sql-protocol:execute
         conn (format nil "CREATE SCHEMA IF NOT EXISTS ~a" ident)))
      (ignore-errors
        (sql-protocol:execute
         conn (format nil "SET search_path TO ~a, public" ident))))
    dir))

(defun %open-corporate-session-store (cfg root)
  (let ((dsn (and cfg (demiurge-config-corporate-postgres-dsn cfg))))
    (if dsn
        (let ((conn (%open-postgres-connection dsn)))
          (if conn
              (csql:make-sql-session-store :connection conn)
              (if (%ensure-sqlite-backend)
                  (csql:make-sql-session-store
                   :driver :sqlite3
                   :database-name
                   (namestring (merge-pathnames "sessions.sqlite" root)))
                  (conv:make-in-memory-conversation-store))))
        (if (%ensure-sqlite-backend)
            (csql:make-sql-session-store
             :driver :sqlite3
             :database-name
             (namestring (merge-pathnames "sessions.sqlite" root)))
            (conv:make-in-memory-conversation-store)))))

(defun %open-corporate-journal (cfg root)
  (let ((dsn (and cfg (demiurge-config-corporate-postgres-dsn cfg))))
    (if dsn
        (let ((conn (%open-postgres-connection dsn)))
          (if conn
              (tbsql:make-sql-task-journal :connection conn :ensure-schema t)
              (if (%ensure-sqlite-backend)
                  (%open-sql-journal (merge-pathnames "journal.sqlite" root))
                  (task:make-in-memory-journal))))
        (if (%ensure-sqlite-backend)
            (%open-sql-journal (merge-pathnames "journal.sqlite" root))
            (task:make-in-memory-journal)))))

(defun %open-corporate-rag (cfg root)
  (let ((dsn (and cfg (demiurge-config-corporate-postgres-dsn cfg))))
    (when (and dsn (%try-load "rag-backend-pgvector"))
      (let ((make (find-symbol "MAKE-PGVECTOR-STORE" :rag-backend-pgvector))
            (conn (%open-postgres-connection dsn)))
        (when (and make (fboundp make) conn)
          (let ((pg (ignore-errors (funcall make :connection conn))))
            (when pg
              (if (%try-load "rag-backend-hybrid")
                  (let ((hy (find-symbol "MAKE-HYBRID-STORE" :rag-backend-hybrid)))
                    (if (and hy (fboundp hy))
                        (funcall hy :vector-store pg)
                        pg))
                  (return-from %open-corporate-rag pg)))))))
    (%open-rag-store (merge-pathnames "rag.sqlite" root))))

(defun %apply-corporate-observe (cfg &key force-recording)
  (let ((pkg (find-package :demiurge/observe)))
    (when pkg
      (let ((fn (find-symbol "APPLY-CORPORATE-OBSERVABILITY" pkg)))
        (when (and fn (fboundp fn))
          (funcall fn
                   :endpoint (and cfg (demiurge-config-corporate-otlp-endpoint cfg))
                   :force-recording force-recording))))))

(defconstant +min-session-secret-length+ 32
  "HS256 session secrets must be at least 256 bits (32 octets).")

(defparameter +forbidden-session-secrets+
  '("demiurge-corporate-dev" "changeme" "secret" "password")
  "Known default / placeholder secrets that must never ship.")

(defparameter +default-session-issuer+ "demiurge")
(defparameter +default-session-audience+ "demiurge-session")
(defparameter +default-session-kid+ "k1")

(defvar *session-secret-environ* :process
  "Where to read session-secret env vars.
   :PROCESS → UIOP:GETENV; an alist → those pairs; NIL → no env.")

(defun %env-lookup (name)
  (cond
    ((eq *session-secret-environ* :process)
     (uiop:getenv name))
    ((listp *session-secret-environ*)
     (cdr (assoc name *session-secret-environ* :test #'string=)))
    (t nil)))

(defun %nonempty-secret (value)
  (and value (plusp (length (string value))) (string value)))

(defun session-secret-weakness (secret)
  "Keyword classifying SECRET, or NIL when it is acceptable."
  (let ((s (and secret (string secret))))
    (cond
      ((or (null s) (zerop (length (string-trim '(#\Space #\Tab #\Newline) s))))
       :missing)
      ((find s +forbidden-session-secrets+ :test #'string=) :default)
      ((< (length s) +min-session-secret-length+) :short)
      (t nil))))

(defun strong-session-secret-p (secret)
  (null (session-secret-weakness secret)))

(defun assert-strong-session-secret (secret)
  "Signal WEAK-SESSION-SECRET unless SECRET is strong. USE-VALUE to supply one."
  (let ((why (session-secret-weakness secret)))
    (if (null why)
        (string secret)
        (restart-case
            (error 'weak-session-secret
                   :provided why
                   :message (format nil "refusing corporate startup (~A secret)" why))
          (use-value (value)
            :report "Use a supplied session secret"
            (assert-strong-session-secret value))))))

(defun resolve-session-secret (cfg &optional explicit)
  "EXPLICIT > CFG > *DEMIURGE-CONFIG* > env. Never falls back to a default."
  (or (%nonempty-secret explicit)
      (%nonempty-secret (and cfg (demiurge-config-corporate-session-secret cfg)))
      (%nonempty-secret (and *demiurge-config*
                             (not (eq cfg *demiurge-config*))
                             (demiurge-config-corporate-session-secret
                              *demiurge-config*)))
      (%nonempty-secret (%env-lookup "DEMIURGE_CORPORATE__SESSION__SECRET"))
      (%nonempty-secret (%env-lookup "DEMIURGE_SESSION_SECRET"))))

(defun %session-keys-from (kid secret prev-kid prev-secret extra)
  (let ((keys (copy-list extra)))
    (when (and prev-kid prev-secret (plusp (length (string prev-secret))))
      (push (cons (string prev-kid) (string prev-secret)) keys))
    (push (cons (or kid +default-session-kid+) secret) keys)
    (remove-duplicates keys :key #'car :test #'equal)))

(defmethod initialize-instance :after ((profile corporate-profile) &key)
  (setf (corporate-profile-session-secret profile)
        (assert-strong-session-secret (corporate-profile-session-secret profile)))
  (unless (corporate-profile-session-kid profile)
    (setf (corporate-profile-session-kid profile) +default-session-kid+))
  (unless (corporate-profile-session-issuer profile)
    (setf (corporate-profile-session-issuer profile) +default-session-issuer+))
  (unless (corporate-profile-session-audience profile)
    (setf (corporate-profile-session-audience profile) +default-session-audience+))
  (unless (corporate-profile-session-keys profile)
    (setf (corporate-profile-session-keys profile)
          (list (cons (corporate-profile-session-kid profile)
                      (corporate-profile-session-secret profile))))))

(defun make-corporate-profile (&key data-dir config journal session-store
                                 chunker rag-store llm-catalog default-model
                                 skill-store (require-hitl-p nil)
                                 tenant
                                 oidc-discovery oidc-jwks oidc-http oidc-key
                                 oidc-algorithms token-exchange session-secret
                                 session-kid session-keys session-issuer
                                 session-audience previous-secret previous-kid
                                 insecure-local-p
                                 ldap-directory group-role-map role-grants
                                 (force-recording nil))
  "Postgres sessions/journal/pgvector when DSN present; else sqlite/memory.
   Applies corporate observability (FORCE-RECORDING for tests).
   SESSION-SECRET must be a strong external value (arg / config / env)."
  (let* ((cfg (or config (current-demiurge-config)))
         (root (uiop:ensure-directory-pathname
                (or data-dir (%default-data-dir cfg))))
         (tenant (or tenant
                     (demiurge-config-corporate-tenant-id cfg)
                     "default"))
         (*tenant* tenant)
         (secret (assert-strong-session-secret
                  (resolve-session-secret cfg session-secret)))
         (kid (or session-kid
                  (and cfg (demiurge-config-corporate-session-kid cfg))
                  +default-session-kid+))
         (prev-secret (or previous-secret
                          (and cfg (demiurge-config-corporate-session-previous-secret cfg))))
         (prev-kid (or previous-kid
                       (and cfg (demiurge-config-corporate-session-previous-kid cfg))
                       "k0"))
         (keys (or session-keys
                   (%session-keys-from kid secret prev-kid prev-secret nil)))
         (issuer (or session-issuer
                     (and cfg (demiurge-config-corporate-session-issuer cfg))
                     +default-session-issuer+))
         (audience (or session-audience
                       (and cfg (demiurge-config-corporate-session-audience cfg))
                       +default-session-audience+))
         (insecure (if insecure-local-p
                       t
                       (and cfg (demiurge-config-corporate-insecure-local cfg)))))
    (ensure-directories-exist root)
    (%apply-corporate-observe cfg :force-recording force-recording)
    (let* ((dsn (demiurge-config-corporate-postgres-dsn cfg))
           (conn (and dsn (%open-postgres-connection dsn)))
           (migrate (%maybe-tenant-migrations conn tenant))
           (sessions (or session-store (%open-corporate-session-store cfg root)))
           (journal (or journal (%open-corporate-journal cfg root)))
           (rag (or rag-store (%open-corporate-rag cfg root))))
      (make-instance 'corporate-profile
                     :kind :corporate
                     :data-dir (namestring root)
                     :config cfg
                     :tenant tenant
                     :session-store sessions
                     :journal journal
                     :chunker (or chunker
                                  (rag-backend-text:make-recursive-character-chunker
                                   :size 1000 :overlap 200))
                     :rag-store rag
                     :llm-catalog (or llm-catalog (%build-llm-catalog cfg))
                     :default-model (or default-model
                                        (demiurge-config-llm-default-model cfg))
                     :skill-store skill-store
                     :require-hitl-p require-hitl-p
                     :oidc-discovery oidc-discovery
                     :oidc-jwks oidc-jwks
                     :oidc-http oidc-http
                     :oidc-key oidc-key
                     :oidc-algorithms (or oidc-algorithms '("RS256"))
                     :token-exchange token-exchange
                     :session-secret secret
                     :session-kid kid
                     :session-keys keys
                     :session-issuer issuer
                     :session-audience audience
                     :insecure-local-p (and insecure t)
                     :ldap-directory ldap-directory
                     :group-role-map (or group-role-map
                                         (demiurge-config-corporate-ldap-group-role-map cfg))
                     :role-grants (or role-grants
                                      (demiurge-config-corporate-role-grants cfg))
                     :migrate-directory migrate
                     :claim-sql (postgres-claimable-lease-sql "task_lease")
                     :pending (make-hash-table :test #'equal)))))

;;; ---------------------------------------------------------------------------
;;; OIDC middleware (Clack)
;;; ---------------------------------------------------------------------------

(defun %header (env name)
  (let ((headers (getf env :headers)))
    (cond
      ((hash-table-p headers)
       (or (gethash name headers)
           (gethash (string-downcase name) headers)))
      ((listp headers)
       (or (getf headers (intern (string-upcase (substitute #\- #\_ name))
                                 :keyword))
           (cdr (assoc name headers :test #'string-equal))))
      (t nil))))

(defun %url-decode (s)
  (with-output-to-string (o)
    (loop with i = 0
          while (< i (length s))
          for c = (char s i)
          do (cond
               ((char= c #\+)
                (write-char #\Space o)
                (incf i))
               ((and (char= c #\%) (>= (length s) (+ i 3)))
                (write-char (code-char
                             (parse-integer s :start (1+ i) :end (+ i 3)
                                            :radix 16))
                            o)
                (incf i 3))
               (t
                (write-char c o)
                (incf i))))))

(defun %parse-query (qs)
  (when (and qs (plusp (length qs)))
    (mapcar (lambda (pair)
              (let ((eq-pos (position #\= pair)))
                (if eq-pos
                    (cons (subseq pair 0 eq-pos)
                          (%url-decode (subseq pair (1+ eq-pos))))
                    (cons pair ""))))
            (uiop:split-string qs :separator '(#\&)))))

(defun %parse-cookies (header)
  (when (and header (plusp (length header)))
    (mapcar (lambda (pair)
              (let* ((s (string-trim '(#\Space) pair))
                     (eq-pos (position #\= s)))
                (if eq-pos
                    (cons (subseq s 0 eq-pos) (subseq s (1+ eq-pos)))
                    (cons s ""))))
            (uiop:split-string header :separator '(#\;)))))

(defun %cookie (env name)
  (or (getf (getf env :cookies) (intern (string-upcase name) :keyword))
      (cdr (assoc name (%parse-cookies (%header env "cookie"))
                  :test #'string-equal))))

(defun %request-host (env)
  (or (%header env "host")
      (let ((server (getf env :server-name))
            (port (getf env :server-port)))
        (if (and server port)
            (format nil "~a:~a" server port)
            (or server "127.0.0.1")))))

(defun %callback-uri (env)
  (let ((scheme (or (getf env :url-scheme) :http)))
    (format nil "~a://~a/callback"
            (string-downcase (string scheme))
            (%request-host env))))

(defun %ensure-oidc-discovery (profile)
  (or (corporate-profile-oidc-discovery profile)
      (let* ((cfg (profile-config profile))
             (issuer (and cfg (demiurge-config-corporate-oidc-issuer cfg)))
             (http (or (corporate-profile-oidc-http profile) *oidc-http-fn*)))
        (when issuer
          (setf (corporate-profile-oidc-discovery profile)
                (if http
                    (oauth2:fetch-oidc-discovery issuer :http http)
                    (oauth2:fetch-oidc-discovery issuer)))))))

(defun %jwt-claim (claims key)
  (or (cdr (assoc key claims :test #'string=))
      (cdr (assoc key claims :test #'equalp))))

(defun %claim-unix (claims key)
  (let ((v (%jwt-claim claims key)))
    (cond
      ((integerp v) v)
      ((and (realp v) (not (complexp v))) (truncate v))
      ((stringp v) (parse-integer v :junk-allowed t))
      (t nil))))

(defun %aud-matches-p (expected aud)
  (let ((want (string expected)))
    (cond
      ((null aud) nil)
      ((stringp aud) (string= want aud))
      ((or (vectorp aud) (listp aud))
       (some (lambda (x) (string= want (string x))) (coerce aud 'list)))
      (t nil))))

(defun %session-key-for-kid (profile kid)
  (let* ((keys (corporate-profile-session-keys profile))
         (from-keys (and kid (cdr (assoc kid keys :test #'equal)))))
    (or from-keys
        (and (or (null kid)
                 (equal kid (corporate-profile-session-kid profile)))
             (corporate-profile-session-secret profile)))))

(defun %session-claims-valid-p (profile claims &key (now (jwt:unix-time)))
  "Require exp/iat/nbf + matching iss/aud. Missing or stale → NIL."
  (let ((exp (%claim-unix claims "exp"))
        (iat (%claim-unix claims "iat"))
        (nbf (%claim-unix claims "nbf"))
        (iss (%jwt-claim claims "iss"))
        (aud (%jwt-claim claims "aud"))
        (want-iss (or (corporate-profile-session-issuer profile)
                      +default-session-issuer+))
        (want-aud (or (corporate-profile-session-audience profile)
                      +default-session-audience+)))
    (and (numberp exp) (< now exp)
         (numberp iat) (<= iat now)
         (numberp nbf) (<= nbf now)
         (stringp iss) (string= (string iss) (string want-iss))
         (%aud-matches-p want-aud aud))))

(defun encode-session-cookie (profile subject &key tenant roles
                                         (now (jwt:unix-time))
                                         (ttl 86400)
                                         exp iat nbf iss aud)
  "Compact JWT for the session cookie. Signed with the current kid."
  (let ((kid (or (corporate-profile-session-kid profile) +default-session-kid+))
        (secret (corporate-profile-session-secret profile)))
    (jwt:encode
     :hs256 secret
     `(("sub" . ,subject)
       ("tenant" . ,(or tenant (profile-tenant profile) "default"))
       ("roles" . ,(coerce (or roles #()) 'vector))
       ("iat" . ,(or iat now))
       ("nbf" . ,(or nbf now))
       ("exp" . ,(or exp (+ now ttl)))
       ("iss" . ,(or iss
                     (corporate-profile-session-issuer profile)
                     +default-session-issuer+))
       ("aud" . ,(or aud
                     (corporate-profile-session-audience profile)
                     +default-session-audience+)))
     :headers `(("kid" . ,kid)))))

(defun decode-session-cookie (profile token)
  "Verify signature (current or previous kid) and claims. NIL if rejected."
  (when (and token (plusp (length (string token))))
    (handler-case
        (multiple-value-bind (unverified-claims header)
            (jwt:inspect-token token)
          (declare (ignore unverified-claims))
          (let* ((kid (%jwt-claim header "kid"))
                 (key (%session-key-for-kid profile kid)))
            (unless key
              (return-from decode-session-cookie nil))
            (multiple-value-bind (claims hdr)
                (jwt:decode :hs256 key token)
              (declare (ignore hdr))
              (when (%session-claims-valid-p profile claims)
                (list :subject (%jwt-claim claims "sub")
                      :tenant (or (%jwt-claim claims "tenant")
                                  (profile-tenant profile))
                      :roles (%as-string-list (%jwt-claim claims "roles"))
                      :kid kid)))))
      (error () nil))))

(defun %encode-session-cookie (profile subject tenant roles)
  (encode-session-cookie profile subject :tenant tenant :roles roles))

(defun %decode-session-cookie (profile token)
  (decode-session-cookie profile token))

(defun %session-from-request (env profile)
  (let ((raw (%cookie env "demiurge_session")))
    (when raw
      (let ((sess (%decode-session-cookie profile raw)))
        (when sess
          (assert-tenant-scope
           (format nil "tenant/~a/session/~a"
                   (getf sess :tenant)
                   (or (getf sess :subject) "anon"))
           (or (getf sess :tenant) (profile-tenant profile)))
          sess)))))

(defun session-cookie-header (token &key (secure t))
  "HttpOnly + SameSite=Lax. SECURE T (production) adds the Secure attribute."
  (format nil "demiurge_session=~a; Path=/; HttpOnly; SameSite=Lax~:[~;; Secure~]"
          token secure))

(defun %set-cookie-header (token &optional profile)
  (session-cookie-header
   token
   :secure (not (and profile (corporate-profile-insecure-local-p profile)))))

(defun %redirect (location &optional set-cookie)
  (if set-cookie
      `(302 (:location ,location
             :set-cookie ,set-cookie
             :content-type "text/plain; charset=utf-8")
            ("redirect"))
      `(302 (:location ,location :content-type "text/plain; charset=utf-8")
            ("redirect"))))

(defun %auth-challenge (profile env)
  (declare (ignore env))
  (let* ((cfg (profile-config profile))
         (issuer (and cfg (demiurge-config-corporate-oidc-issuer cfg))))
    (if issuer
        (%redirect "/login")
        '(401 (:content-type "text/plain; charset=utf-8") ("unauthorized")))))

(defun %oidc-login (profile env)
  (let* ((cfg (profile-config profile))
         (discovery (%ensure-oidc-discovery profile))
         (issuer (and cfg (demiurge-config-corporate-oidc-issuer cfg)))
         (client (and cfg (demiurge-config-corporate-oidc-client-id cfg))))
    (unless (and issuer client discovery)
      (return-from %oidc-login
        '(503 (:content-type "text/plain; charset=utf-8")
          ("oidc not configured"))))
    (let* ((auth (oauth2:make-oidc-auth
                  :client-id client
                  :redirect-uri (%callback-uri env)
                  :authorize-url (oauth2:oidc-authorization-endpoint discovery)
                  :token-url (oauth2:oidc-token-endpoint discovery))))
      (oauth2:apply-oidc-discovery! auth discovery)
      (let ((url (oauth2:oidc-authorization-url auth :pkce t)))
        (setf (gethash (oauth2:oauth2-state auth)
                       (corporate-profile-pending profile))
              (list :nonce (oauth2:oauth2-nonce auth)
                    :verifier (oauth2:oauth2-code-verifier auth)
                    :auth auth))
        (%redirect url)))))

(defun %exchange-id-token (profile code)
  (let ((fn (or (corporate-profile-token-exchange profile)
                *oidc-token-exchange-fn*)))
    (when (and fn code)
      (funcall fn code profile))))

(defun %oidc-callback (profile env)
  (let* ((qs (%parse-query (or (getf env :query-string) "")))
         (code (cdr (assoc "code" qs :test #'string=)))
         (state (cdr (assoc "state" qs :test #'string=)))
         (id-token (or (cdr (assoc "id_token" qs :test #'string=))
                       (%exchange-id-token profile code)))
         (pending (and state
                       (gethash state (corporate-profile-pending profile))))
         (cfg (profile-config profile))
         (issuer (and cfg (demiurge-config-corporate-oidc-issuer cfg)))
         (audience (and cfg (demiurge-config-corporate-oidc-client-id cfg))))
    (unless id-token
      (return-from %oidc-callback
        '(400 (:content-type "text/plain; charset=utf-8")
          ("missing id_token"))))
    (when state
      (remhash state (corporate-profile-pending profile)))
    (multiple-value-bind (claims header)
        (oauth2:validate-id-token
         id-token
         :algorithms (corporate-profile-oidc-algorithms profile)
         :jwks (corporate-profile-oidc-jwks profile)
         :key (corporate-profile-oidc-key profile)
         :issuer issuer
         :audience audience
         :nonce (getf pending :nonce)
         :http (or (corporate-profile-oidc-http profile) *oidc-http-fn*))
      (declare (ignore header))
      (let* ((sub (cdr (assoc "sub" claims :test #'string=)))
             (tenant (or (cdr (assoc "tenant" claims :test #'string=))
                         (profile-tenant profile)
                         "default"))
             (roles (resolve-principal-roles profile sub claims))
             (cookie (%encode-session-cookie profile sub tenant roles)))
        (%redirect "/" (%set-cookie-header cookie profile))))))

(defun wrap-corporate-auth (app profile)
  "OIDC login middleware. /healthz and /readyz stay unauthenticated.
   Session cookie holds subject + tenant. Refuses a weak session secret."
  (assert-strong-session-secret (corporate-profile-session-secret profile))
  (lambda (env)
    (let ((path (or (getf env :path-info) "/")))
      (cond
        ((string= path "/healthz")
         (funcall app env))
        ((string= path "/readyz")
         (funcall app env))
        ((string= path "/login")
         (%oidc-login profile env))
        ((string= path "/callback")
         (%oidc-callback profile env))
        (t
         (let ((sess (%session-from-request env profile)))
           (if sess
               (let ((*tenant* (or (getf sess :tenant) (profile-tenant profile)))
                     (*principal* (getf sess :subject))
                     (*principal-roles* (getf sess :roles)))
                 (funcall app env))
               (%auth-challenge profile env))))))))
