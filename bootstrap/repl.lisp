(defpackage #:demiurge-bootstrap/bootstrap/repl
  (:use #:cl)
  (:import-from #:demiurge/src/blackboard/core
                #:blackboard #:read-section #:write-section
                #:run-scheduler #:stop-scheduler
                #:bb-scheduler-thread #:bb-scheduler-running-p)
  (:import-from #:demiurge/src/controller/main-loop
                #:make-demiurge-instance #:demiurge-ctx-bb
                #:stop-demiurge)
  (:import-from #:demiurge/src/controller/timers
                #:start-timer)
  (:import-from #:demiurge/src/controller/handlers
                #:init-kernel #:shutdown-kernel)
  (:import-from #:demiurge-bootstrap/bootstrap/loader
                #:bootstrap-context #:make-bootstrap-context*
                #:load-bootstrap-capabilities
                #:ctx-bb #:ctx-mem)
  (:import-from #:demiurge/src/utils/config
                #:load-config)
  (:import-from #:demiurge/src/introspection/render
                #:render-bb-summary)
  (:export #:start #:stop-demiurge-repl
           #:submit-task #:status
           #:*ctx*))

(in-package #:demiurge-bootstrap/bootstrap/repl)

(defvar *ctx* nil "Current bootstrap context for REPL interaction.")
(defvar *scheduler-thread* nil)
(defvar *timer* nil)

(defun start (&key (lm-studio-url "http://192.168.86.67:1234/v1")
                   (lm-studio-api-key nil)
                   (project-root #P"~/Projects/lisp/")
                   (memory-path "~/.config/demiurge/memory.json")
                   (config-path nil)
                   (tick-interval 10)
                   (workers 4)
                   (swank-port 4005)
                   (read-timeout 300)
                   (connect-timeout 30))
  "Start the Demiurge agent. Returns the bootstrap context."
  (when *ctx*
    (format t "~&Demiurge already running. Call STOP-DEMIURGE-REPL first.~%")
    (return-from start *ctx*))
  ;; Initialize thread pool
  (init-kernel :workers workers)
  ;; Create core instance (BB with agenda)
  (let* ((dm (make-demiurge-instance :workers workers))
         (bb (demiurge-ctx-bb dm)))
    ;; Pre-seed LLM config before bootstrap
    (write-section bb :llm-config
                   (append (list :base-url lm-studio-url
                                 :read-timeout read-timeout
                                 :connect-timeout connect-timeout)
                           (when lm-studio-api-key
                             (list :api-key lm-studio-api-key))))
    ;; Full bootstrap: load config, create memory, register caps, wire watchers
    (let ((ctx (make-bootstrap-context* bb
                                        :config-path config-path
                                        :memory-path memory-path
                                        :project-root (namestring project-root))))
      ;; Start tick timer (writes :tick token to BB)
      (setf *timer* (start-timer bb :tick tick-interval))
      ;; Start Swank if requested
      (when swank-port
        (start-swank-if-available swank-port))
      ;; Start scheduler thread (replaces event loop)
      (setf *scheduler-thread*
            (bt2:make-thread (lambda () (run-scheduler bb))
                             :name "demiurge-scheduler"))
      (setf (bb-scheduler-thread bb) *scheduler-thread*)
      (setf *ctx* ctx)
      (format t "~&Demiurge started. Scheduler running in background.~%")
      (format t "  BB sections: ~A~%" (length (demiurge/src/blackboard/core:list-sections bb)))
      (format t "  Capabilities: ~A~%" (length (demiurge/src/capabilities/registry:list-capabilities bb)))
      ctx)))

(defun stop-demiurge-repl ()
  "Stop the running Demiurge instance."
  (when *ctx*
    (let ((bb (ctx-bb *ctx*)))
      ;; Stop scheduler
      (stop-scheduler bb)
      (setf *scheduler-thread* nil)
      ;; Stop timer
      (when *timer*
        (demiurge/src/controller/timers:stop-timer *timer*)
        (setf *timer* nil))
      ;; Shutdown kernel
      (shutdown-kernel)
      ;; Save memory
      (when (ctx-mem *ctx*)
        (demiurge/src/persistence/memory:mem-save (ctx-mem *ctx*)))
      (format t "~&Demiurge stopped.~%")
      (setf *ctx* nil))))

(defun submit-task (description &key (source :repl))
  "Submit a task to the running Demiurge by writing :pending-task token."
  (unless *ctx*
    (error "Demiurge not running. Call START first."))
  (write-section (ctx-bb *ctx*) :pending-task
                 (list :id (format nil "task-~A" (get-universal-time))
                       :description description
                       :source source))
  (format t "~&Task submitted: ~A~%" description))

(defun status ()
  "Print current Demiurge status."
  (unless *ctx*
    (format t "~&Demiurge not running.~%")
    (return-from status))
  (format t "~&~A~%" (render-bb-summary (ctx-bb *ctx*))))

;;; --- Optional Swank ---

(defun start-swank-if-available (port)
  (handler-case
      (progn
        (unless (find-package :swank)
          (asdf:load-system :swank))
        (let ((create-server (find-symbol "CREATE-SERVER" :swank)))
          (when create-server
            (funcall create-server :port port :dont-close t)
            (format t "  Swank server on port ~D~%" port))))
    (error (e)
      (format *error-output* "  Swank not available: ~A~%" e))))
