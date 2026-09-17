(in-package #:demiurge)

(defclass agent-ks (bb:knowledge-source)
  ((agent :initarg :agent :accessor agent-ks-agent)
   (watch :initarg :watch :accessor agent-ks-watch :initform '(:prompt))
   (prompt-key :initarg :prompt-key :accessor agent-ks-prompt-key
               :initform :prompt)
   (result-key :initarg :result-key :accessor agent-ks-result-key
               :initform :result)
   (memory :initarg :memory :accessor agent-ks-memory :initform nil)
   (steering :initarg :steering :accessor agent-ks-steering :initform nil)
   (catalogue :initarg :catalogue :accessor agent-ks-catalogue :initform nil)
   (mcp-peer :initarg :mcp-peer :accessor agent-ks-mcp-peer :initform nil)
   (durability :initarg :durability :accessor agent-ks-durability :initform nil)))

(defun agent-ks-p (x)
  (typep x 'agent-ks))

(defun make-agent-ks (&key name agent (watch '(:prompt))
                        (prompt-key :prompt) (result-key :result)
                        memory steering catalogue mcp-peer durability
                        (priority 0) (version "0.1.0"))
  (check-type agent agent:ai-agent)
  (make-instance 'agent-ks
                 :name name
                 :agent agent
                 :watch (copy-list watch)
                 :prompt-key prompt-key
                 :result-key result-key
                 :memory memory
                 :steering (and steering (steer:coerce-steering steering))
                 :catalogue catalogue
                 :mcp-peer mcp-peer
                 :durability durability
                 :priority priority
                 :version version))

(defgeneric ks-watch-keys (ks)
  (:documentation "Section keys the KS watcher requires. Default none.")
  (:method (ks)
    (declare (ignore ks))
    nil)
  (:method ((ks agent-ks))
    (copy-list (agent-ks-watch ks))))

(defvar *trial-restricted-catalogue* nil
  "Restricted catalogue bound for an improvement trial. Preferred over the
   expert/principal catalogue when constructing candidate tools.")

(defun collect-agent-ks-tools (ks &key catalogue steering mcp-peer)
  "Capability ops + skill-tool sources + optional MCP source.
   A bound *TRIAL-RESTRICTED-CATALOGUE* wins so sandboxed trials do not
   inherit side-effecting ops from the expert catalogue."
  (append (catalogue-function-tools
           (or catalogue
               *trial-restricted-catalogue*
               (agent-ks-catalogue ks)))
          (%skill-tool-sources
           (or steering (agent-ks-steering ks)))
          (let ((src (%maybe-mcp-source (or mcp-peer (agent-ks-mcp-peer ks)))))
            (and src (list src)))))

(defmethod bb:ks-precondition ((ks agent-ks) blackboard)
  (every (lambda (key) (bb:section-bound-p blackboard key))
         (agent-ks-watch ks)))

(defvar *event-backend-maker* :auto
  "Event-backend constructor. :AUTO looks up MAKE-LIBUV-BACKEND; NIL forces missing.")

(defun %event-backend-maker ()
  (cond
    ((eq *event-backend-maker* :auto)
     (let ((sym (find-symbol "MAKE-LIBUV-BACKEND" :event-backend-libuv)))
       (and sym (fboundp sym) (symbol-function sym))))
    ((eq *event-backend-maker* nil) nil)
    ((functionp *event-backend-maker*) *event-backend-maker*)
    (t (lambda () *event-backend-maker*))))

(defun %durability-supported-p ()
  "A6b hook: only forward :durability when the durability subsystem is loaded."
  (flet ((bound (name &optional (package :ai-agent-protocol))
           (let ((s (find-symbol name package)))
             (and s (fboundp s)))))
    (or (bound "MAKE-AGENT-DURABILITY")
        (bound "WITH-AGENT-DURABILITY")
        (bound "AGENT-RUN-DURABILITY")
        (let ((pkg (find-package :ai-agent-protocol/durability)))
          (and pkg (bound "COERCE-AGENT-DURABILITY" pkg))))))

(defun call-with-event-loop (thunk)
  "Bind a fresh event loop on THIS thread. KSAR workers do not inherit specials."
  (let ((maker (%event-backend-maker)))
    (unless maker
      (setf maker
            (restart-case
                (error 'missing-event-backend
                       :message "bind an event backend or load event-backend-libuv")
              (use-value (fn)
                :report "Use a supplied event-backend constructor"
                (cond
                  ((functionp fn) fn)
                  (t (lambda () fn)))))))
    (let* ((eb (funcall maker))
           (el (event:make-event-loop eb)))
      (event:with-event-backend (eb)
        (event:with-event-loop-var (el)
          (funcall thunk))))))

(defun %session-window-turns ()
  (demiurge-config-session-window-turns (current-demiurge-config)))

(defun %ensure-agent-memory (ks agent)
  "Attach a memory object if the agent has none. Session identity is
   passed per run — never key shared domain memory by KS name."
  (unless (agent:ai-agent-memory agent)
    (setf (agent:ai-agent-memory agent)
          (or (agent-ks-memory ks)
              (conv:make-window-memory
               :window-size (%session-window-turns)))))
  (agent:ai-agent-memory agent))

(defun %ensure-agent-steering (ks agent)
  (let ((steering (or (agent:ai-agent-steering agent)
                      (agent-ks-steering ks))))
    (when steering
      (setf (agent:ai-agent-steering agent)
            (steer:coerce-steering steering)))
    (agent:ai-agent-steering agent)))

(defun %run-ai-agent (agent prompt &key tools session durability)
  (let ((keys (append (when tools (list :tools tools))
                      (when session (list :session session))
                      (when (and durability (%durability-supported-p))
                        (list :durability durability)))))
    (call-with-agent-observe agent
      (lambda ()
        (apply #'agent:run-ai-agent agent prompt keys)))))

(defun %turn-mentions-p (turns needle)
  (let ((needle (and needle (princ-to-string needle))))
    (and needle
         (plusp (length needle))
         (some (lambda (tr)
                 (let ((text (or (ignore-errors (llm:turn-text tr))
                                 (ignore-errors (princ-to-string tr)))))
                   (and text (search needle text))))
               (if (listp turns) turns (and turns (list turns)))))))

(defun %record-session-exchange (ks prompt run session)
  "Persist prompt+result on SESSION when the agent loop remembered nothing
   usable. run-ai-agent :replace t with empty AGENT-RUN-TURNS would otherwise
   leave an isolated-but-empty store (H6 CI)."
  (when (and session (plusp (length (string session))))
    (let* ((agent (agent-ks-agent ks))
           (mem (%ensure-agent-memory ks agent))
           (store (and mem (conv:memory-store mem)))
           (existing (and store (ignore-errors (conv:load-session store session))))
           (needle (let ((s (princ-to-string (or prompt ""))))
                     (subseq s 0 (min (length s) 32))))
           (text (cond
                   ((and run (agent:agent-run-p run))
                    (or (agent:agent-run-text run) ""))
                   ((stringp run) run)
                   (t ""))))
      (when (and mem store (plusp (length needle))
                 (not (%turn-mentions-p existing needle)))
        (conv:remember mem
                       (list (llm:user-turn (princ-to-string prompt))
                             (llm:assistant-turn (princ-to-string text)))
                       :session session)))))

(defun %request-session-for-board (blackboard)
  "Session captured on the request thread (board section or special).
   Workers do not inherit *REQUEST-SESSION*."
  (let ((stored (and blackboard
                     (bb:section-bound-p blackboard :%demiurge-request-session)
                     (bb:read-section blackboard :%demiurge-request-session))))
    (cond
      ((request-session-p stored) stored)
      ((request-session-p *request-session*) *request-session*)
      (t (make-request-session)))))

(defun %execute-agent-ks (ks blackboard)
  (call-with-request-session (%request-session-for-board blackboard)
    (lambda ()
      (let* ((agent (agent-ks-agent ks))
             (prompt (bb:read-section blackboard (agent-ks-prompt-key ks)))
             (steering (%ensure-agent-steering ks agent))
             (domain (%domain-for-board blackboard))
             (catalogue (or *trial-restricted-catalogue*
                            (catalogue-for-request domain)
                            (agent-ks-catalogue ks)))
             (tools (collect-agent-ks-tools
                     ks
                     :catalogue catalogue
                     :steering steering
                     :mcp-peer (agent-ks-mcp-peer ks))))
        (%ensure-agent-memory ks agent)
        (let* ((session (current-request-session-key))
               (run (call-with-event-loop
                     (lambda ()
                       (%run-ai-agent agent prompt
                                      :tools tools
                                      :session session
                                      :durability (agent-ks-durability ks))))))
          (%record-session-exchange ks prompt run session)
          (bb:write-section blackboard
                           (agent-ks-result-key ks)
                           (or (agent:agent-run-text run) run))
          run)))))

(defmethod bb:ks-execute ((ks agent-ks) blackboard)
  (call-with-ksar-observe ks
    (lambda ()
      (call-with-durable-ksar blackboard ks
                              (lambda ()
                                (%execute-agent-ks ks blackboard))))))

(defmethod bb:ks-postcondition ((ks agent-ks) blackboard result)
  (declare (ignore result))
  (bb:section-bound-p blackboard (agent-ks-result-key ks)))
