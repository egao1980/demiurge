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

[corporate.session]
secret = ~S
kid = \"k1\"
issuer = \"demiurge\"
audience = \"demiurge-session\"
"
                        dsn tenant issuer client (%corporate-hs-key))))
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
              :test #'string-equal))
    (ok (equal (%corporate-hs-key) (demiurge-config-corporate-session-secret cfg)))
    (ok (equal "k1" (demiurge-config-corporate-session-kid cfg)))))

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
        (root (make-cl-dev-catalogue :grant-compute t)))
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
      (ok (stringp (cap:invoke-operation prin-cap 'lookup-symbol "car"))
          "granted op still invokes on the inner capability"))))

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
           (inner (lambda (env)
                    (let ((path (or (getf env :path-info) "/")))
                      (cond
                        ((member path '("/healthz" "/readyz") :test #'string=)
                         '(200 (:content-type "text/plain; charset=utf-8")
                           ("ok")))
                        (t
                         '(404 (:content-type "text/plain; charset=utf-8")
                           ("not found")))))))
           (app (wrap-corporate-auth inner profile)))
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
                                          (quri:url-encode tok)))))
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
                "authenticated GET /feedback is 404, not a login redirect")
            (ok (search "Secure" cookie)
                "production session cookie is Secure")))))))

(deftest corporate-tenant-isolation-error
  (with-tenant "acme"
    (ok (signals (assert-tenant-scope "tenant/other/domain/x")
                 'tenant-isolation-error))
    (ok (equal "tenant/acme/domain/echo" (assert-tenant-scope
                                          (tenant-task-id "echo"))))
    (ok (equal "acme" (tenant-of-reference "tenant/acme/session/alice")))
    (ok (equal "tenant/acme/corpus/docs" (tenant-corpus-name "docs")))
    (ok (equal '(:tenant "acme" :improve "c1")
               (tenant-budget-scope :improve "c1"))))
  (ok (null (tenant-of-reference "domain/echo")))
  (ok (equal "docs" (tenant-corpus-name "docs")))
  (ok (equal '(:tenant "default" :improve "c1")
             (tenant-budget-scope :improve "c1"))))

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
  (ok t "docker compose corporate profile is a manual/CI-compose check; default CI does not require Docker"))

(defun %bare-corporate-keys (&key session-secret)
  (append
   (list :data-dir "/tmp/demiurge-h3-unused"
         :config (make-instance 'demiurge-config)
         :journal (task-protocol:make-in-memory-journal)
         :session-store (conv:make-in-memory-conversation-store)
         :rag-store (rag-backend-memory:make-memory-vector-store)
         :chunker (rag-backend-text:make-recursive-character-chunker
                   :size 200 :overlap 20))
   (when session-secret (list :session-secret session-secret))))

(deftest corporate-rejects-default-missing-weak-secret
  "H7 gate 5: default/missing/weak secrets are rejected at startup."
  (let ((*session-secret-environ* nil)
        (demiurge::*demiurge-config* nil))
    (ok (signals (apply #'make-corporate-profile (%bare-corporate-keys))
                 'weak-session-secret)
        "missing secret")
    (ok (signals (apply #'make-corporate-profile
                        (%bare-corporate-keys
                         :session-secret "demiurge-corporate-dev"))
                 'weak-session-secret)
        "default secret")
    (ok (signals (apply #'make-corporate-profile
                        (%bare-corporate-keys :session-secret ""))
                 'weak-session-secret)
        "empty secret")
    (ok (signals (apply #'make-corporate-profile
                        (%bare-corporate-keys :session-secret "short"))
                 'weak-session-secret)
        "short secret")
    (ok (signals (make-instance 'corporate-profile)
                 'weak-session-secret)
        "make-instance without secret")))

(deftest corporate-accepts-config-and-env-secret
  (with-tmp-dir (tmp)
    (let* ((cfg (%corporate-cfg))
           (profile (make-corporate-profile
                     :data-dir tmp
                     :config cfg
                     :force-recording t
                     :journal (task-protocol:make-in-memory-journal)
                     :session-store (conv:make-in-memory-conversation-store)
                     :rag-store (rag-backend-memory:make-memory-vector-store)
                     :chunker (rag-backend-text:make-recursive-character-chunker
                               :size 200 :overlap 20))))
      (ok (corporate-profile-p profile))
      (ok (equal (%corporate-hs-key)
                 (corporate-profile-session-secret profile))))
    (let ((*session-secret-environ*
           `(("DEMIURGE_CORPORATE__SESSION__SECRET" . ,(%corporate-hs-key))))
          (demiurge::*demiurge-config* nil))
      (with-tmp-dir (tmp)
        (let ((profile (make-corporate-profile
                        :data-dir tmp
                        :config (make-instance 'demiurge-config)
                        :force-recording t
                        :journal (task-protocol:make-in-memory-journal)
                        :session-store (conv:make-in-memory-conversation-store)
                        :rag-store (rag-backend-memory:make-memory-vector-store)
                        :chunker (rag-backend-text:make-recursive-character-chunker
                                  :size 200 :overlap 20))))
          (ok (equal (%corporate-hs-key)
                     (corporate-profile-session-secret profile))))))))

(deftest corporate-session-rejects-expired-tampered-and-incomplete
  "H7 gate 5: expired/tampered/incomplete tokens are not sessions."
  (with-tmp-dir (tmp)
    (let* ((profile (%memory-corporate tmp :config (%corporate-cfg)))
           (now (jwt:unix-time))
           (good (encode-session-cookie profile "alice"
                                        :tenant "acme" :roles '("reader")
                                        :now now)))
      (ok (equal "alice" (getf (decode-session-cookie profile good) :subject)))
      (let ((expired (encode-session-cookie profile "alice"
                                            :tenant "acme"
                                            :now (- now 120)
                                            :exp (- now 10))))
        (ok (null (decode-session-cookie profile expired))
            "expired exp rejected"))
      (let ((future-nbf (encode-session-cookie profile "alice"
                                               :tenant "acme"
                                               :now now
                                               :nbf (+ now 3600))))
        (ok (null (decode-session-cookie profile future-nbf))
            "future nbf rejected"))
      (let ((bad-iss (encode-session-cookie profile "alice"
                                            :tenant "acme"
                                            :iss "other-issuer")))
        (ok (null (decode-session-cookie profile bad-iss))
            "iss mismatch rejected"))
      (let ((bad-aud (encode-session-cookie profile "alice"
                                            :tenant "acme"
                                            :aud "other-aud")))
        (ok (null (decode-session-cookie profile bad-aud))
            "aud mismatch rejected"))
      (let ((no-iat (jwt:encode
                     :hs256 (%corporate-hs-key)
                     `(("sub" . "alice")
                       ("tenant" . "acme")
                       ("roles" . #())
                       ("nbf" . ,now)
                       ("exp" . ,(+ now 3600))
                       ("iss" . "demiurge")
                       ("aud" . "demiurge-session"))
                     :headers '(("kid" . "k1")))))
        (ok (null (decode-session-cookie profile no-iat))
            "missing iat rejected"))
      (let* ((tampered (copy-seq good))
             (dot (position #\. tampered :from-end t)))
        (setf (char tampered (1- (length tampered)))
              (if (char= (char tampered (1- (length tampered))) #\A)
                  #\B #\A))
        (ok (null (decode-session-cookie profile tampered))
            "tampered signature rejected")
        (ok (numberp dot))))))

(deftest corporate-session-kid-rotation-and-secure-cookie
  "H7 gate 5: previous kid accepted; Secure policy is explicit."
  (with-tmp-dir (tmp)
    (let* ((prev "previous-secret-12345678901234567")
           (profile (%memory-corporate tmp :config (%corporate-cfg)))
           (now (jwt:unix-time)))
      (setf (corporate-profile-session-keys profile)
            (list (cons "k1" (%corporate-hs-key))
                  (cons "k0" prev)))
      (let ((old (jwt:encode
                  :hs256 prev
                  `(("sub" . "alice")
                    ("tenant" . "acme")
                    ("roles" . #())
                    ("iat" . ,now)
                    ("nbf" . ,now)
                    ("exp" . ,(+ now 3600))
                    ("iss" . "demiurge")
                    ("aud" . "demiurge-session"))
                  :headers '(("kid" . "k0")))))
        (ok (equal "alice" (getf (decode-session-cookie profile old) :subject))
            "previous kid still verifies"))
      (let ((unknown (jwt:encode
                      :hs256 "unknown-secret-123456789012345678"
                      `(("sub" . "eve")
                        ("tenant" . "acme")
                        ("roles" . #())
                        ("iat" . ,now)
                        ("nbf" . ,now)
                        ("exp" . ,(+ now 3600))
                        ("iss" . "demiurge")
                        ("aud" . "demiurge-session"))
                      :headers '(("kid" . "k99")))))
        (ok (null (decode-session-cookie profile unknown))
            "unknown kid rejected"))
      (ok (search "Secure" (session-cookie-header "tok" :secure t)))
      (ok (not (search "Secure" (session-cookie-header "tok" :secure nil))))
      (ok (search "Secure" (demiurge::%set-cookie-header "tok" profile))
          "corporate default cookie is Secure")
      (setf (corporate-profile-insecure-local-p profile) t)
      (ok (not (search "Secure" (demiurge::%set-cookie-header "tok" profile)))
          "insecure-local omits Secure"))))

(deftest corporate-observability-compose-is-loopback
  (let ((text (uiop:read-file-string
               (asdf:system-relative-pathname
                "demiurge" "ops/docker-compose.observability.yml"))))
    (ok (search "127.0.0.1:3000:3000" text))
    (ok (search "127.0.0.1:4318:4318" text))
    (ok (search "GF_AUTH_ANONYMOUS_ENABLED: \"false\"" text))
    (ok (search "GRAFANA_ADMIN_PASSWORD" text))
    (ng (search "GF_AUTH_ANONYMOUS_ORG_ROLE: Admin" text))))
