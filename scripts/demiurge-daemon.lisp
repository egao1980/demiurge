;;; Demiurge Daemon — starts the full agent with Swank, MCP, and A2A servers.
;;; Usage: sbcl --load demiurge-daemon.lisp [-- --no-swank --mcp-port 8080 ...]

(require :asdf)
(asdf:load-system :demiurge-bootstrap)
(asdf:load-system :hunchentoot)

(defpackage #:demiurge-daemon
  (:use #:cl))

(in-package #:demiurge-daemon)

(defvar *ctx* nil)

(defun load-config-file ()
  (let ((paths (list (merge-pathnames ".config/demiurge/config.json"
                                       (user-homedir-pathname))
                     (merge-pathnames "config/default-config.json"
                                      (asdf:system-source-directory :demiurge)))))
    (dolist (path paths)
      (when (probe-file path)
        (return (funcall (find-symbol "LOAD-CONFIG" :demiurge/src/utils/config) path))))))

(defun parse-args (args)
  (let ((swank-port 4005) (mcp-port 8080) (a2a-port 8081)
        (tick-interval 10) (workers 4))
    (loop while args do
      (let ((arg (pop args)))
        (cond
          ((string= arg "--swank-port") (setf swank-port (parse-integer (pop args))))
          ((string= arg "--mcp-port") (setf mcp-port (parse-integer (pop args))))
          ((string= arg "--a2a-port") (setf a2a-port (parse-integer (pop args))))
          ((string= arg "--workers") (setf workers (parse-integer (pop args))))
          ((string= arg "--no-swank") (setf swank-port nil)))))
    (values swank-port mcp-port a2a-port tick-interval workers)))

(defun run ()
  (let ((config (load-config-file))
        (cli-args (uiop:command-line-arguments)))
    (when (member "--" cli-args :test #'string=)
      (setf cli-args (rest (member "--" cli-args :test #'string=))))
    (multiple-value-bind (swank-port mcp-port a2a-port tick-interval workers)
        (parse-args cli-args)
      (when config
        (alexandria:when-let (p (gethash "mcp_port" config))
          (setf mcp-port p))
        (alexandria:when-let (p (gethash "a2a_port" config))
          (setf a2a-port p))
        (alexandria:when-let (i (gethash "tick_interval_seconds" config))
          (setf tick-interval i)))
      (format t "~&Starting Demiurge daemon...~%")
      (format t "  Workers: ~D  Tick: ~Ds~%" workers tick-interval)
      (format t "  Swank: ~:[disabled~;port ~:*~D~]~%" swank-port)
      (format t "  MCP:   port ~D~%" mcp-port)
      (format t "  A2A:   port ~D~%" a2a-port)
      ;; Bootstrap
      (let* ((start-fn (find-symbol "START" :demiurge-bootstrap/bootstrap/repl))
             (api-key (when config (gethash "lm_studio_api_key" config)))
             (lm-url (when config (gethash "lm_studio_url" config)))
             (ctx (funcall start-fn
                           :lm-studio-url (or lm-url "http://192.168.86.67:1234/v1")
                           :lm-studio-api-key api-key
                           :config-path (merge-pathnames ".config/demiurge/config.json"
                                                          (user-homedir-pathname))
                           :tick-interval tick-interval
                           :workers workers
                           :swank-port swank-port)))
        (setf *ctx* ctx)
        ;; Start MCP + A2A servers (no bus param)
        (let ((ctx-bb-fn (find-symbol "CTX-BB" :demiurge-bootstrap/bootstrap/loader))
              (ctx-mem-fn (find-symbol "CTX-MEM" :demiurge-bootstrap/bootstrap/loader)))
          (let ((bb (funcall ctx-bb-fn ctx))
                (mem (funcall ctx-mem-fn ctx)))
            (funcall (find-symbol "START-MCP-SERVER" :demiurge-bootstrap/bootstrap/mcp-server)
                     bb mem :port mcp-port)
            (funcall (find-symbol "START-A2A-SERVER" :demiurge-bootstrap/bootstrap/a2a-server)
                     bb :port a2a-port)))
        (format t "~&Demiurge daemon ready.~%")
        (handler-case
            (loop (sleep 60))
          (#+sbcl sb-sys:interactive-interrupt
           #-sbcl condition ()
            (format t "~&Shutting down...~%")
            (funcall (find-symbol "STOP-MCP-SERVER" :demiurge-bootstrap/bootstrap/mcp-server))
            (funcall (find-symbol "STOP-A2A-SERVER" :demiurge-bootstrap/bootstrap/a2a-server))
            (funcall (find-symbol "STOP-DEMIURGE-REPL" :demiurge-bootstrap/bootstrap/repl))
            (format t "~&Goodbye.~%")))))))

(run)
