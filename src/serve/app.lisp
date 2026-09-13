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

(defun %slurp-body (env)
  (let ((raw (getf env :raw-body)))
    (cond
      ((null raw) "")
      ((stringp raw) raw)
      ((and (vectorp raw) (not (stringp raw)))
       (map 'string #'code-char raw))
      ((streamp raw)
       (with-output-to-string (o)
         (loop for c = (read-char raw nil nil)
               while c
               do (write-char c o))))
      (t (princ-to-string raw)))))

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
  (let* ((body (%slurp-body env))
         (parsed (if (plusp (length body))
                     (handler-case (ag-ui:decode-json body)
                       (error () body))
                     nil))
         (event (cond
                  ((and (hash-table-p parsed)
                        (equal (gethash "type" parsed) "CUSTOM"))
                   (handler-case (ag-ui:decode-ag-ui-event parsed)
                     (error () parsed)))
                  (t parsed))))
    (handle-feedback-event domain (or event parsed))
    '(200 (:content-type "application/json; charset=utf-8")
      ("{\"ok\":true}"))))

(defun %rewrite-path (env path)
  (let ((copy (copy-list env)))
    (setf (getf copy :path-info) path)
    copy))

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
   carries stores; otherwise the B3 stub (DOMAIN-READY-P / *READYZ-FN*)."
  (check-type domain expert-domain)
  (let* ((mcp-server (make-expert-mcp-server domain))
         (ag-ui-app (ag-ui:make-ag-ui-app
                     (make-expert-ag-ui-agent domain)
                     :path "/"))
         (mcp-app (mcp.http:make-mcp-app mcp-server :path "/mcp"))
         (a2a-app (a2a.rpc:make-a2a-app
                   (make-expert-a2a-agent domain)
                   :path "/a2a"
                   :card (expert-agent-card domain))))
    (lambda (env)
      (let ((path (or (getf env :path-info) "/"))
            (method (getf env :request-method)))
        (cond
          ((string= path "/healthz")
           (%healthz-response))
          ((string= path "/readyz")
           (%readyz-response domain profile))
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
           '(404 (:content-type "text/plain; charset=utf-8") ("not found"))))))))

(defun %start-http (app host port)
  (let ((serve (find-symbol "SERVE" :http-server-protocol)))
    (unless serve
      (error 'serve-error
             :message "http-server-protocol:serve is not available"))
    (funcall serve app :host host :port port)))

(defun %start-stdio (mcp-server)
  (mcp:backend-mcp-serve
   (mcp.stdio:make-stdio-mcp-backend)
   mcp-server))

(defun serve-expert (domain &key (transports '(:stdio :http))
                              (host "127.0.0.1") (port 8080)
                              profile (start t))
  "Assemble the expert app. START T begins stdio-MCP and/or HTTP when
   the transport backends are loaded."
  (check-type domain expert-domain)
  (let* ((profile (or profile (expert-profile domain)))
         (app (make-expert-app domain profile))
         (mcp (make-expert-mcp-server domain))
         (wanted (if (listp transports)
                     transports
                     (list transports)))
         (started '()))
    (when start
      (when (member :http wanted :test #'eq)
        (%start-http app host port)
        (push :http started))
      (when (and (member :stdio wanted :test #'eq)
                 (not (member :http started)))
        (%start-stdio mcp)
        (push :stdio started)))
    (make-instance 'serve-session
                   :domain domain
                   :app app
                   :mcp mcp
                   :transports (or started wanted))))
