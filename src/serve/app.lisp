(in-package #:demiurge/serve)

(defvar *readyz-fn* nil
  "Optional (lambda (domain)) → generalized boolean. B5 replaces the stub.")

(defgeneric domain-ready-p (domain)
  (:documentation "T when DOMAIN can serve traffic. B5 overrides this.")
  (:method (domain)
    (declare (ignore domain))
    t))

(defun readyz-ok-p (domain)
  (if *readyz-fn*
      (funcall *readyz-fn* domain)
      (domain-ready-p domain)))

(defun loopback-address-p (host)
  (let ((s (string-downcase (string (or host "")))))
    (or (string= s "127.0.0.1")
        (string= s "localhost")
        (string= s "::1")
        (string= s "[::1]")
        (string= s "0:0:0:0:0:0:0:1"))))

(defun check-serve-security (host profile &key insecure-local)
  "Non-loopback HTTP needs corporate authn+authz or explicit insecure-local.
   Corporate profiles must already hold a strong session secret."
  (when (corporate-profile-p profile)
    (assert-strong-session-secret (corporate-profile-session-secret profile)))
  (unless (or (loopback-address-p host)
              insecure-local
              (and (corporate-profile-p profile)
                   (corporate-profile-insecure-local-p profile))
              (corporate-profile-p profile))
    (error 'serve-error
           :message
           "non-loopback HTTP requires corporate auth or --insecure-local"))
  t)

(defun %env-header (env name)
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

(defun %request-content-type (env)
  (or (getf env :content-type)
      (%env-header env "content-type")))

(defun %json-content-type-p (ct)
  (let ((s (string-downcase (string-trim '(#\Space) (or ct "")))))
    (or (string= s "application/json")
        (and (>= (length s) 16)
             (string= s "application/json" :end1 16)
             (or (= (length s) 16)
                 (find (char s 16) '(#\Space #\;)))))))

(defun %content-length (env)
  (let ((raw (or (getf env :content-length)
                 (%env-header env "content-length"))))
    (cond
      ((integerp raw) raw)
      ((stringp raw) (parse-integer raw :junk-allowed t))
      (t nil))))

(defun %request-too-large-p (env &optional (limit *max-request-bytes*))
  (let ((n (%content-length env)))
    (and n (> n limit))))

(defun %slurp-body (env &key (limit *max-request-bytes*))
  (let ((raw (getf env :raw-body)))
    (flet ((bounded (seq)
             (when (and limit (> (length seq) limit))
               (error 'request-too-large :limit limit :size (length seq)))
             seq))
      (cond
        ((null raw) "")
        ((stringp raw) (bounded raw))
        ((and (vectorp raw) (not (stringp raw)))
         (bounded (map 'string #'code-char raw)))
        ((streamp raw)
         (with-output-to-string (o)
           (loop for n from 1
                 for c = (read-char raw nil nil)
                 while c
                 do (when (and limit (> n limit))
                      (error 'request-too-large :limit limit :size n))
                    (write-char c o))))
        (t (bounded (princ-to-string raw)))))))

(defun %observe-symbol (name)
  (let ((pkg (find-package :demiurge/observe)))
    (and pkg (find-symbol name pkg))))

(defun %profile-probes (domain profile)
  (let ((prof (cond
                ((deployment-profile-p profile) profile)
                ((and (expert-domain-p domain)
                      (deployment-profile-p (expert-profile domain)))
                 (expert-profile domain))
                (t nil))))
    (when (deployment-profile-p prof)
      (list :journal (profile-journal prof)
            :rag-store (profile-rag-store prof)
            :llm-backend
            (let ((cat (profile-llm-catalog prof))
                  (model (profile-default-model prof)))
              (when cat
                (ignore-errors
                  (funcall (or (find-symbol "RESOLVE-BACKEND" :llm-protocol)
                               (constantly nil))
                           cat model))))))))

(defun %healthz-response ()
  (let ((fn (%observe-symbol "HEALTHZ-RESPONSE")))
    (if (and fn (fboundp fn))
        (funcall fn)
        '(200 (:content-type "text/plain; charset=utf-8") ("ok")))))

(defun %readyz-response (domain profile)
  (when *readyz-fn*
    (return-from %readyz-response
      (if (funcall *readyz-fn* domain)
          '(200 (:content-type "text/plain; charset=utf-8") ("ready"))
          '(503 (:content-type "text/plain; charset=utf-8") ("not ready")))))
  (let ((fn (%observe-symbol "READYZ-RESPONSE"))
        (probes (%profile-probes domain profile)))
    (cond
      ((and fn (fboundp fn) probes)
       (apply fn probes))
      ((domain-ready-p domain)
       '(200 (:content-type "text/plain; charset=utf-8") ("ready")))
      (t
       '(503 (:content-type "text/plain; charset=utf-8") ("not ready"))))))

(defun %feedback-response (domain env)
  (handler-case
      (progn
        (unless (%json-content-type-p (%request-content-type env))
          (return-from %feedback-response
            '(415 (:content-type "text/plain; charset=utf-8")
              ("content-type must be application/json"))))
        (when (%request-too-large-p env)
          (return-from %feedback-response
            '(413 (:content-type "text/plain; charset=utf-8")
              ("payload too large"))))
        (let* ((body (%slurp-body env :limit *max-request-bytes*))
               (parsed (cond
                         ((zerop (length body))
                          (error 'invalid-feedback :message "empty body"))
                         (t (ag-ui:decode-json body))))
               (event (cond
                        ((and (hash-table-p parsed)
                              (equal (gethash "type" parsed) "CUSTOM"))
                         (ag-ui:decode-ag-ui-event parsed))
                        (t parsed))))
          (handle-feedback-event domain event)
          '(200 (:content-type "application/json; charset=utf-8")
            ("{\"ok\":true}"))))
    (request-too-large ()
      '(413 (:content-type "text/plain; charset=utf-8")
        ("payload too large")))
    (invalid-feedback (c)
      `(400 (:content-type "text/plain; charset=utf-8")
            (,(or (demiurge-error-message c) "malformed feedback"))))
    (error (c)
      `(400 (:content-type "text/plain; charset=utf-8")
            (,(format nil "malformed feedback: ~A" c))))))

(defun %rewrite-path (env path)
  (let ((copy (copy-list env)))
    (setf (getf copy :path-info) path)
    copy))

(defun %query-param (query-string name)
  (when (and query-string (plusp (length query-string)))
    (loop for part in (uiop:split-string query-string :separator "&")
          for eq = (position #\= part)
          when (and eq (string-equal name (subseq part 0 eq)))
            do (return (subseq part (1+ eq))))))

(defun %conversation-id-from-env (env)
  (let ((id (or (%env-header env "x-conversation-id")
                (%env-header env "x-thread-id")
                (%query-param (getf env :query-string) "conversation_id")
                (%query-param (getf env :query-string) "threadId"))))
    (and id (plusp (length (string id))) (string id))))

(defclass serve-session ()
  ((domain :initarg :domain :accessor serve-session-domain)
   (app :initarg :app :accessor serve-session-app)
   (mcp :initarg :mcp :accessor serve-session-mcp :initform nil)
   (transports :initarg :transports :accessor serve-session-transports
               :initform nil)))

(defun serve-session-p (x)
  (typep x 'serve-session))

(defun make-expert-app (domain profile)
  "Clack dispatcher: AG-UI POST→SSE, feedback, /healthz, /readyz.
   /readyz uses demiurge/observe when that system is loaded and PROFILE
   carries stores; otherwise the B3 stub (DOMAIN-READY-P / *READYZ-FN*).
   A CORPORATE-PROFILE wraps the app in OIDC middleware; /healthz and
   /readyz stay unauthenticated."
  (check-type domain expert-domain)
  (let* ((mcp-server (make-expert-mcp-server domain))
         (ag-ui-app (ag-ui:make-ag-ui-app
                     (make-expert-ag-ui-agent domain)
                     :path "/"))
         (mcp-app (mcp.http:make-mcp-app mcp-server :path "/mcp"))
         (a2a-app (a2a.rpc:make-a2a-app
                   (make-expert-a2a-agent domain)
                   :path "/a2a"
                   :card (expert-agent-card domain)))
         (app
          (lambda (env)
            (with-request-session (nil :transport :http
                                       :conversation-id
                                       (%conversation-id-from-env env))
              (let ((path (or (getf env :path-info) "/"))
                    (method (getf env :request-method)))
                (cond
                  ((string= path "/healthz")
                   (%healthz-response))
                  ((string= path "/readyz")
                   (%readyz-response domain profile))
                  ((and (member method '(:post :put :patch) :test #'eq)
                        (%request-too-large-p env))
                   '(413 (:content-type "text/plain; charset=utf-8")
                     ("payload too large")))
                  ((and (string= path "/feedback") (eq method :post))
                   (%feedback-response domain env))
                  ((string= path "/mcp")
                   (funcall mcp-app env))
                  ((or (string= path "/a2a")
                       (a2a.rpc:well-known-card-path-p path))
                   (funcall a2a-app env))
                  ((or (string= path "/") (string= path "/ag-ui"))
                   (funcall ag-ui-app
                            (if (string= path "/ag-ui")
                                (%rewrite-path env "/")
                                env)))
                  (t
                   '(404 (:content-type "text/plain; charset=utf-8")
                     ("not found")))))))))
    (if (corporate-profile-p profile)
        (wrap-corporate-auth app profile)
        app)))

(defvar *start-http-hook* nil
  "Optional (lambda (app host port)). Tests bind this instead of opening a socket.")

(defvar *start-stdio-hook* nil
  "Optional (lambda (mcp-server)). Tests bind this instead of blocking on stdin.")

(defun %start-http (app host port)
  (if *start-http-hook*
      (funcall *start-http-hook* app host port)
      (let ((serve (find-symbol "SERVE" :http-server-protocol)))
        (unless serve
          (error 'serve-error
                 :message "http-server-protocol:serve is not available"))
        (funcall serve app :host host :port port))))

(defun %start-stdio (mcp-server)
  (if *start-stdio-hook*
      (funcall *start-stdio-hook* mcp-server)
      (with-request-session (nil :transport :stdio)
        (mcp:backend-mcp-serve
         (mcp.stdio:make-stdio-mcp-backend)
         mcp-server))))

(defun normalize-serve-transports (transports)
  "Return the unique requested transports. :http and :stdio may both be
   present — the combination is never silently dropped."
  (let ((wanted (remove-duplicates
                 (if (listp transports)
                     (copy-list transports)
                     (list transports))
                 :test #'eq)))
    (unless wanted
      (error 'serve-error
             :message "serve-expert requires at least one transport"))
    (dolist (tr wanted)
      (unless (member tr '(:http :stdio) :test #'eq)
        (error 'serve-error
               :message (format nil "unknown serve transport ~S (expected :http and/or :stdio)"
                                tr))))
    wanted))

(defun serve-expert (domain &key (transports '(:stdio :http))
                              (host "127.0.0.1") (port 8080)
                              profile (start t) insecure-local)
  "Assemble the expert app. START T begins every requested transport
   (:http is background; :stdio then blocks). The :http+:stdio pair is
   started together — stdio is never dropped. Non-loopback HTTP requires
   a corporate profile or INSECURE-LOCAL."
  (check-type domain expert-domain)
  (let* ((profile (or profile (expert-profile domain)))
         (wanted (normalize-serve-transports transports)))
    (when (member :http wanted :test #'eq)
      (check-serve-security host profile :insecure-local insecure-local))
    (let* ((app (make-expert-app domain profile))
           (mcp (make-expert-mcp-server domain))
           (started '()))
      (when start
        (when (member :http wanted :test #'eq)
          (%start-http app host port)
          (push :http started))
        (when (member :stdio wanted :test #'eq)
          (%start-stdio mcp)
          (push :stdio started)))
      (make-instance 'serve-session
                     :domain domain
                     :app app
                     :mcp mcp
                     :transports (if start (nreverse started) wanted)))))
