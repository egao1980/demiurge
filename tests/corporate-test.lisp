(in-package #:demiurge/tests)

(defparameter *corporate-oidc-discovery-json*
  "{
  \"issuer\": \"https://idp.example\",
  \"authorization_endpoint\": \"https://idp.example/authorize\",
  \"token_endpoint\": \"https://idp.example/token\",
  \"jwks_uri\": \"https://idp.example/jwks\",
  \"userinfo_endpoint\": \"https://idp.example/userinfo\",
  \"id_token_signing_alg_values_supported\": [\"HS256\"]
}")

(defun %corporate-hs-key ()
  "secret-key-123456789012345678901234")

(defun %corporate-id-token (&key (iss "https://idp.example")
                                 (aud "app")
                                 (nonce "n1")
                                 (sub "alice")
                                 (tenant "acme")
                                 (exp (+ (jwt:unix-time) 3600))
                                 (kid "k1"))
  (jwt:encode
   :hs256 (%corporate-hs-key)
   `(("iss" . ,iss)
     ("aud" . ,aud)
     ("nonce" . ,nonce)
     ("sub" . ,sub)
     ("tenant" . ,tenant)
     ("exp" . ,exp))
   :headers `(("kid" . ,kid))))

(defun %corporate-cfg (&key (issuer "https://idp.example")
                            (client "app")
                            (dsn "postgres://demiurge:demiurge@127.0.0.1:5432/demiurge")
                            (tenant "acme"))
  (let* ((path (%write-tmp-toml
                (format nil "
[corporate]
postgres.dsn = ~S
otlp.endpoint = \"http://127.0.0.1:4318\"
tenant.id = ~S

[corporate.oidc]
issuer = ~S
client-id = ~S

[corporate.ldap]
url = \"ldap://127.0.0.1:389\"
base-dn = \"dc=example,dc=com\"

[corporate.ldap.group-role-map]
\"cn=readers,ou=groups,dc=example,dc=com\" = \"reader\"
\"cn=admins,ou=groups,dc=example,dc=com\" = \"admin\"

[corporate.role-grants]
reader = [\"lookup-symbol\", \"search-corpus\"]
admin = [\"lookup-symbol\", \"search-corpus\", \"run-tests\"]
"
                        dsn tenant issuer client)))
         (demiurge::*demiurge-config* nil))
    (load-demiurge-config :path path :env nil)))

(defun %corporate-ldap ()
  (ldap:make-mock-ldap-directory
   :entries
   (list (cons "dc=example,dc=com"
               '(("objectClass" "domain") ("dc" "example")))
         (cons "ou=people,dc=example,dc=com"
               '(("objectClass" "organizationalUnit") ("ou" "people")))
         (cons "ou=groups,dc=example,dc=com"
               '(("objectClass" "organizationalUnit") ("ou" "groups")))
         (cons "cn=alice,ou=people,dc=example,dc=com"
               '(("objectClass" "person" "inetOrgPerson")
                 ("cn" "alice")
                 ("uid" "alice")
                 ("memberOf" "cn=readers,ou=groups,dc=example,dc=com")
                 ("userPassword" "s3cret")))
         (cons "cn=readers,ou=groups,dc=example,dc=com"
               '(("objectClass" "groupOfNames")
                 ("cn" "readers")
                 ("member" "cn=alice,ou=people,dc=example,dc=com"))))))

(defun %memory-corporate (tmp &key config tenant ldap-directory
                              oidc-discovery oidc-jwks oidc-key
                              oidc-algorithms token-exchange
                              group-role-map role-grants)
  (make-corporate-profile
   :data-dir tmp
   :config (or config (current-demiurge-config))
   :tenant (or tenant "acme")
   :force-recording t
   :journal (task-protocol:make-in-memory-journal)
   :session-store (conv:make-in-memory-conversation-store)
   :rag-store (rag-backend-memory:make-memory-vector-store)
   :chunker (rag-backend-text:make-recursive-character-chunker
             :size 200 :overlap 20)
   :ldap-directory ldap-directory
   :oidc-discovery oidc-discovery
   :oidc-jwks oidc-jwks
   :oidc-key oidc-key
   :oidc-algorithms (or oidc-algorithms '("HS256"))
   :token-exchange token-exchange
   :group-role-map group-role-map
   :role-grants role-grants
   :session-secret (%corporate-hs-key)))

(deftest corporate-config-parses-issuer-dsn-tenant
  (let ((cfg (%corporate-cfg)))
    (ok (equal "https://idp.example" (demiurge-config-corporate-oidc-issuer cfg)))
    (ok (equal "app" (demiurge-config-corporate-oidc-client-id cfg)))
    (ok (equal "postgres://demiurge:demiurge@127.0.0.1:5432/demiurge"
               (demiurge-config-corporate-postgres-dsn cfg)))
    (ok (equal "acme" (demiurge-config-corporate-tenant-id cfg)))
    (ok (equal "http://127.0.0.1:4318"
               (demiurge-config-corporate-otlp-endpoint cfg)))
    (ok (equal "dc=example,dc=com" (demiurge-config-corporate-ldap-base-dn cfg)))
    (ok (find "lookup-symbol"
              (cdr (assoc "reader" (demiurge-config-corporate-role-grants cfg)
                          :test #'string-equal))
              :test #'string-equal))))

(deftest corporate-config-env-override
  (let* ((path (%write-tmp-toml "
[corporate]
tenant.id = \"file-tenant\"

[corporate.oidc]
issuer = \"https://file.example\"
"))
         (demiurge::*demiurge-config* nil)
         (cfg (load-demiurge-config
               :path path
               :prefix "DEMIURGE"
               :env t
               :environ '(("DEMIURGE_CORPORATE__OIDC__ISSUER" . "https://env.example")
                          ("DEMIURGE_CORPORATE__TENANT__ID" . "env-tenant")
                          ("DEMIURGE_CORPORATE__POSTGRES__DSN"
                           . "postgres://env@localhost/db")))))
    (ok (equal "https://env.example" (demiurge-config-corporate-oidc-issuer cfg)))
    (ok (equal "env-tenant" (demiurge-config-corporate-tenant-id cfg)))
    (ok (equal "postgres://env@localhost/db"
               (demiurge-config-corporate-postgres-dsn cfg)))))

(deftest corporate-profile-factory-without-postgres
  (with-tmp-dir (tmp)
    (let ((profile (%memory-corporate tmp :config (%corporate-cfg))))
      (ok (corporate-profile-p profile))
      (ok (eq :corporate (profile-kind profile)))
      (ok (equal "acme" (profile-tenant profile)))
      (ok (search "FOR UPDATE SKIP LOCKED"
                  (postgres-claimable-lease-sql))))))

(deftest corporate-authz-denial-is-audited
  (let ((demiurge::*capability-denial-audit* nil)
        (root (cap:make-catalogue :cl-dev)))
    (cap:register-capability root (make-instance 'lisp-dev-capability))
    (let* ((filtered (filter-catalogue-for-roles
                      root '("reader")
                      '(("reader" . ("lookup-symbol" "search-corpus")))
                      :principal "alice"
                      :tenant "acme"))
           (root-cap (cap:get-capability root :lisp-dev))
           (prin-cap (cap:get-capability filtered :lisp-dev)))
      (ok (not (eq root filtered)))
      (ok (find 'run-tests (cap:capability-operations root-cap)
                :key #'cap:capability-operation-name))
      (ok (null (find 'run-tests (cap:capability-operations prin-cap)
                      :key #'cap:capability-operation-name)))
      (ok (signals (cap:invoke-operation prin-cap 'run-tests "demiurge")
                   'capability-denied))
      (ok (plusp (length demiurge::*capability-denial-audit*)))
      (ok (equal "alice" (getf (first demiurge::*capability-denial-audit*)
                               :principal)))
      (ok (equal "lookup-symbol"
                 (ignore-errors
                   (princ-to-string
                    (cap:invoke-operation prin-cap 'lookup-symbol "car"))))))))

(deftest corporate-ldap-group-role-map
  (let* ((dir (%corporate-ldap))
         (groups (ldap-groups-for-dn
                  dir "cn=alice,ou=people,dc=example,dc=com"
                  :base-dn "ou=groups,dc=example,dc=com"))
         (roles (map-groups-to-roles
                 groups
                 '(("cn=readers,ou=groups,dc=example,dc=com" . "reader")))))
    (ok (find "cn=readers,ou=groups,dc=example,dc=com" groups
              :test #'string-equal))
    (ok (find "reader" roles :test #'string-equal))))

(deftest corporate-oidc-flow-against-mock-issuer
  (with-tmp-dir (tmp)
    (let* ((cfg (%corporate-cfg))
           (discovery (oauth2:parse-oidc-discovery *corporate-oidc-discovery-json*))
           (jwks (oauth2:make-jwks-cache))
           (tok nil)
           (profile (%memory-corporate
                     tmp
                     :config cfg
                     :oidc-discovery discovery
                     :oidc-jwks jwks
                     :oidc-key (%corporate-hs-key)
                     :token-exchange
                     (lambda (code prof)
                       (declare (ignore code prof))
                       tok)))
           (domain (make-echo-expert :backend (mock-llm) :name "echo-oidc"
                                     :profile profile))
           (app (make-expert-app domain profile)))
      (oauth2:jwks-cache-put jwks "k1" (%corporate-hs-key))
      (ok (oauth2:oidc-discovery-p discovery))
      (ok (equal "https://idp.example" (oauth2:oidc-issuer discovery)))
      (let ((hz (funcall app '(:request-method :get :path-info "/healthz")))
            (rz (funcall app '(:request-method :get :path-info "/readyz"))))
        (ok (= 200 (first hz)))
        (ok (= 200 (first rz))))
      (let ((denied (funcall app '(:request-method :get :path-info "/"))))
        (ok (= 302 (first denied)))
        (ok (equal "/login" (getf (second denied) :location))))
      (let* ((login (funcall app '(:request-method :get
                                   :path-info "/login"
                                   :url-scheme :http
                                   :server-name "app.example"
                                   :server-port 80)))
             (loc (getf (second login) :location))
             (uri (quri:uri loc))
             (q (quri:uri-query-params uri))
             (state (cdr (assoc "state" q :test #'string=)))
             (pending (gethash state (demiurge::corporate-profile-pending profile)))
             (nonce (getf pending :nonce)))
        (ok (= 302 (first login)))
        (ok (search "https://idp.example/authorize" loc))
        (ok (equal "openid" (cdr (assoc "scope" q :test #'string=))))
        (ok (stringp (cdr (assoc "code_challenge" q :test #'string=))))
        (ok (stringp (getf pending :verifier)))
        (setf tok (%corporate-id-token :nonce nonce :aud "app"))
        (multiple-value-bind (claims header)
            (oauth2:validate-id-token tok
                                      :algorithms '("HS256")
                                      :key (%corporate-hs-key)
                                      :jwks jwks
                                      :issuer "https://idp.example"
                                      :audience "app"
                                      :nonce nonce)
          (ok (equal "alice" (cdr (assoc "sub" claims :test #'string=))))
          (ok (equal "HS256" (cdr (assoc "alg" header :test #'string=)))))
        (let* ((cb (funcall app
                            (list :request-method :get
                                  :path-info "/callback"
                                  :query-string
                                  (format nil "code=abc&state=~a&id_token=~a"
                                          (quri:url-encode state)
                                          (quri:url-encode tok))))))
               (headers (second cb))
               (cookie (getf headers :set-cookie)))
          (ok (= 302 (first cb)))
          (ok (search "demiurge_session=" cookie))
          (let* ((raw (subseq cookie (length "demiurge_session=")
                              (position #\; cookie)))
                 (authed
                  (funcall app
                           (list :request-method :get
                                 :path-info "/feedback"
                                 :headers
                                 (let ((ht (make-hash-table :test #'equal)))
                                   (setf (gethash "cookie" ht)
                                         (format nil "demiurge_session=~a" raw))
                                   ht)))))
            (ok (= 404 (first authed))
                "authenticated GET /feedback is 404, not a login redirect")))))))

(deftest corporate-tenant-isolation-error
  (with-tenant "acme"
    (ok (signals (assert-tenant-scope "tenant/other/domain/x")
                 'tenant-isolation-error))
    (ok (equal "tenant/acme/domain/echo" (assert-tenant-scope
                                          (tenant-task-id "echo"))))
    (ok (equal "acme" (tenant-of-reference "tenant/acme/session/alice"))))
  (ok (null (tenant-of-reference "domain/echo"))))

(deftest corporate-skip-locked-sql-text
  (let ((sql (postgres-claimable-lease-sql "task_lease")))
    (ok (search "FOR UPDATE SKIP LOCKED" sql))
    (ok (search "task_lease" sql))
    (ok (search "lease_until" sql))))

(deftest corporate-parse-postgres-dsn
  (let ((keys (parse-postgres-dsn
               "postgres://demiurge:s3cret@db.example:5433/app")))
    (ok (equal "db.example" (getf keys :host)))
    (ok (eql 5433 (getf keys :port)))
    (ok (equal "app" (getf keys :database-name)))
    (ok (equal "demiurge" (getf keys :username)))
    (ok (equal "s3cret" (getf keys :password)))))

(deftest corporate-compose-readyz-skipped-without-docker
  (skip "docker compose corporate profile is a manual/CI-compose check; default CI does not require Docker"))
