(defpackage #:demiurge-bootstrap/bootstrap/agent-loop
  (:use #:cl)
  (:import-from #:demiurge/src/blackboard/core
                #:blackboard #:read-section #:write-section #:remove-section
                #:record-bb-error #:watch #:unwatch)
  (:import-from #:demiurge/src/blackboard/workspace
                #:fork-workspace #:merge-workspace #:discard-workspace
                #:workspace-blackboard #:workspace-name #:find-root-bb)
  (:import-from #:demiurge/src/capabilities/registry
                #:get-capability)
  (:import-from #:demiurge/src/capabilities/llm
                #:generate-text #:generate-with-tools)
  (:import-from #:demiurge/src/persistence/memory
                #:persistent-memory #:mem-get #:mem-set)
  (:import-from #:demiurge/src/persistence/memory-keys
                #:record-task-result)
  (:import-from #:demiurge-bootstrap/bootstrap/agent-tools
                #:build-tool-definitions #:execute-tool-call)
  (:import-from #:demiurge/src/controller/prompts
                #:build-agent-prompt)
  (:export #:start-agent-task))

(in-package #:demiurge-bootstrap/bootstrap/agent-loop)

(defvar *max-agent-iterations* 50
  "Maximum number of LLM round-trips in a single agent task.")

(defvar *max-llm-retries* 3
  "Maximum consecutive LLM API errors before aborting.")

(defvar *error-loop-threshold* 3
  "Consecutive tool failures with the same tool before supervisor intervenes.")

(defvar *max-supervisor-interventions* 3
  "Cap on supervisor diagnosis injections per task.")

;;; ---------------------------------------------------------------------------
;;; Naming helpers — unique IDs per workspace
;;; ---------------------------------------------------------------------------

(defun watcher-id-for (ws-name)
  "Unique watcher ID for this workspace's agent step."
  (intern (format nil "AGENT-STEP/~A" ws-name) :keyword))

(defun trigger-key-for (ws-name)
  "Unique BB section key that triggers this workspace's agent step."
  (intern (format nil "AGENT-TRIGGER/~A" ws-name) :keyword))

;;; ---------------------------------------------------------------------------
;;; Message section helpers — workspace IS the conversation
;;; ---------------------------------------------------------------------------

(defun msg-key (index)
  "Return a keyword like :MSG-0, :MSG-1, etc."
  (intern (format nil "MSG-~D" index) :keyword))

(defun collect-messages (ws-bb)
  "Reconstruct ordered message list from :MSG-0 .. :MSG-(count-1)."
  (let ((count (or (read-section ws-bb :msg-count) 0)))
    (loop for i from 0 below count
          for msg = (read-section ws-bb (msg-key i))
          when msg collect msg)))

(defun append-message (ws-bb msg)
  "Append a message to the workspace conversation.
Writes :MSG-N and increments :MSG-COUNT. Returns the index."
  (let ((idx (or (read-section ws-bb :msg-count) 0)))
    (write-section ws-bb (msg-key idx) msg)
    (write-section ws-bb :msg-count (1+ idx))
    idx))

;;; ---------------------------------------------------------------------------
;;; Message constructors
;;; ---------------------------------------------------------------------------

(defun make-assistant-message (content tool-calls)
  "Build an assistant message plist. TOOL-CALLS is a vector of raw tool-call hash-tables."
  (let ((msg (list :role "assistant")))
    (when content
      (setf msg (append msg (list :content content))))
    (when (and tool-calls (plusp (length tool-calls)))
      (setf msg (append msg (list :tool-calls (coerce tool-calls 'list)))))
    msg))

(defun make-tool-result-message (tc result-string)
  "Build a tool result message plist. TC is the raw tool-call hash-table."
  (list :role "tool"
        :tool-call-id (gethash "id" tc)
        :content result-string))

;;; ---------------------------------------------------------------------------
;;; Error loop detection and supervisor intervention
;;; ---------------------------------------------------------------------------

(defun tool-result-error-p (msg)
  "Return T if MSG is a tool result indicating failure."
  (and (string= (getf msg :role) "tool")
       (let ((content (or (getf msg :content) "")))
         (or (search "Error:" content)
             (and (>= (length content) 6)
                  (string= "exit=" (subseq content 0 5))
                  (not (string= "exit=0" (subseq content 0 6))))))))

(defun extract-tool-name-from-assistant (msg)
  "Extract the first tool name from an assistant message's tool-calls."
  (let ((tcs (getf msg :tool-calls)))
    (when tcs
      (let* ((tc (first tcs))
             (fn (when (hash-table-p tc) (gethash "function" tc))))
        (when fn (gethash "name" fn))))))

(defun detect-error-loop (ws-bb)
  "Examine recent messages for error loop patterns.
Returns NIL or a diagnostic summary string.
Checks the last several assistant+tool pairs for:
 - Pattern 1: N+ consecutive failing tool results from the same tool
 - Pattern 2: N+ consecutive calls to the same tool (even mixed success/failure)"
  (let* ((count (or (read-section ws-bb :msg-count) 0))
         (window (* 2 *error-loop-threshold*))
         (start (max 0 (- count window)))
         (recent (loop for i from start below count
                       for msg = (read-section ws-bb (msg-key i))
                       when msg collect msg))
         (tool-results (remove-if-not (lambda (m) (string= (getf m :role) "tool")) recent))
         (assistant-msgs (remove-if-not (lambda (m) (string= (getf m :role) "assistant")) recent)))
    ;; Pattern 1: consecutive tool errors
    (when (>= (length tool-results) *error-loop-threshold*)
      (let ((tail (last tool-results *error-loop-threshold*)))
        (when (every #'tool-result-error-p tail)
          (let ((snippets (mapcar (lambda (m)
                                    (let ((c (or (getf m :content) "")))
                                      (if (> (length c) 120) (subseq c 0 120) c)))
                                  tail)))
            (return-from detect-error-loop
              (format nil "~D consecutive tool failures. Recent errors:~%~{  - ~A~%~}"
                      *error-loop-threshold* snippets))))))
    ;; Pattern 2: same tool called repeatedly
    (when (>= (length assistant-msgs) *error-loop-threshold*)
      (let* ((tail (last assistant-msgs *error-loop-threshold*))
             (names (mapcar #'extract-tool-name-from-assistant tail)))
        (when (and (first names)
                   (every (lambda (n) (and n (string= n (first names)))) names))
          (return-from detect-error-loop
            (format nil "Tool '~A' called ~D times in a row."
                    (first names) *error-loop-threshold*)))))
    nil))

(defun build-diagnosis-prompt (ws-bb detection-summary)
  "Build a compact prompt for the supervisor to diagnose an error loop."
  (let* ((count (or (read-section ws-bb :msg-count) 0))
         (start (max 0 (- count 8)))
         (recent-tool-msgs
           (loop for i from start below count
                 for msg = (read-section ws-bb (msg-key i))
                 when (and msg (string= (getf msg :role) "tool"))
                 collect (let ((c (or (getf msg :content) "")))
                           (if (> (length c) 200) (subseq c 0 200) c)))))
    (format nil "You are a supervisor monitoring an agent that is stuck in an error loop.

Detected pattern: ~A

Recent tool results:
~{- ~A~%~}
IMPORTANT context about available tools:
- run_command: spawns a FRESH ephemeral container each call. Installed packages are LOST between calls. Must chain with &&.
- create_environment + exec_in_environment: persistent container, state survives across calls.
- write_file: writes to /workspace/ which is shared and persists everywhere.

Give the agent ONE specific corrective instruction (2-3 sentences max).
Do not explain the architecture. Just tell the agent exactly what to do differently on its next attempt."
            detection-summary recent-tool-msgs)))

(defun maybe-supervisor-intervene (bb ws-bb ws-name)
  "Check for error loops and inject a supervisor diagnosis if needed.
Returns T if intervention was injected, NIL otherwise."
  (let ((interventions (or (read-section ws-bb :agent-supervisor-interventions) 0)))
    (when (>= interventions *max-supervisor-interventions*)
      (return-from maybe-supervisor-intervene nil))
    (let ((detection (detect-error-loop ws-bb)))
      (when detection
        (let ((llm-cap (get-capability bb :llm-generation)))
          (when llm-cap
            (format t "~&[supervisor] ~A error loop detected: ~A~%" ws-name detection)
            (handler-case
                (let* ((prompt (build-diagnosis-prompt ws-bb detection))
                       (diagnosis (generate-text llm-cap
                                                 (list (list :role "user" :content prompt))
                                                 :role :supervisor)))
                  (when (and diagnosis (plusp (length diagnosis)))
                    (format t "~&[supervisor] ~A injecting corrective guidance~%" ws-name)
                    (append-message ws-bb
                                    (list :role "user"
                                          :content (format nil "[Supervisor intervention] ~A" diagnosis)))
                    (write-section ws-bb :agent-supervisor-interventions (1+ interventions))
                    t))
              (error (e)
                (format *error-output* "~&[supervisor] ~A diagnosis failed: ~A~%" ws-name e)
                nil))))))))

;;; ---------------------------------------------------------------------------
;;; Entry point — called from supervisor watcher, returns immediately
;;; ---------------------------------------------------------------------------

(defun start-agent-task (bb mem task-description)
  "Initialize an agent task workspace and register the reactive step watcher.
Forks a workspace, seeds the conversation, registers a watcher on the ROOT BB
(so the scheduler can look it up), then writes the trigger key to fire the
first step. Returns the workspace name immediately."
  (let* ((ws-name (format nil "agent-~A" (get-universal-time)))
         (ws (fork-workspace bb ws-name))
         (ws-bb (workspace-blackboard ws))
         (tools (build-tool-definitions bb))
         (system-prompt (build-agent-prompt bb :mem mem :task task-description))
         (root-bb (find-root-bb bb))
         (watcher-id (watcher-id-for ws-name))
         (trigger-key (trigger-key-for ws-name)))
    ;; Seed conversation on workspace
    (write-section ws-bb :msg-count 0)
    (append-message ws-bb (list :role "system" :content system-prompt))
    (append-message ws-bb (list :role "user" :content task-description))
    ;; Metadata
    (write-section ws-bb :task-description task-description)
    (write-section ws-bb :agent-tools tools)
    (write-section ws-bb :agent-iteration 0)
    (write-section ws-bb :agent-retry-count 0)
    (write-section ws-bb :agent-supervisor-interventions 0)
    (format t "~&[agent] Starting task in workspace ~A (~D tools)~%"
            ws-name (length tools))
    ;; Register watcher on ROOT BB — scheduler looks up watchers there
    (watch root-bb
           :id watcher-id
           :requires (list trigger-key)
           :handler (make-step-handler root-bb ws-bb mem ws-name)
           :priority 90
           :one-shot nil)
    ;; Write trigger key on root BB to fire the first step
    (write-section root-bb trigger-key t)
    ws-name))

;;; ---------------------------------------------------------------------------
;;; Step handler — one LLM round-trip per scheduler invocation
;;; ---------------------------------------------------------------------------

(defun make-step-handler (bb ws-bb mem ws-name)
  "Return a closure for the agent step watcher handler.
BB is the root blackboard. WS-BB is the workspace COW-BB."
  (lambda (triggering-bb ksar)
    (declare (ignore triggering-bb ksar))
    (handle-agent-step bb ws-bb mem ws-name)))

(defun finish-agent (bb ws-bb ws-name mem status result-text description)
  "Cleanup: write result to workspace, remove trigger, unwatch, record to memory."
  (write-section ws-bb :agent-result result-text)
  (remove-section bb (trigger-key-for ws-name))
  (unwatch bb (watcher-id-for ws-name))
  (when mem
    (record-task-result mem
                        :workspace ws-name
                        :status status
                        :description description)))

(defun retrigger-agent (bb ws-name)
  "Re-trigger the next agent step by toggling the trigger key on root BB."
  (let ((trigger-key (trigger-key-for ws-name)))
    (remove-section bb trigger-key)
    (write-section bb trigger-key t)))

(defun handle-agent-step (bb ws-bb mem ws-name)
  "Execute one agent step: collect messages, call LLM, process tool calls or finish.
BB is the root blackboard. WS-BB is the workspace COW-BB (the conversation)."
  (let ((iteration (or (read-section ws-bb :agent-iteration) 0)))
    ;; Cancellation check
    (when (read-section ws-bb :agent-cancelled)
      (format t "~&[agent] ~A cancelled at iteration ~D~%" ws-name iteration)
      (finish-agent bb ws-bb ws-name mem :cancelled
                    "Task cancelled by user."
                    (format nil "Cancelled at iteration ~D" iteration))
      (return-from handle-agent-step nil))
    ;; Max iterations guard
    (when (>= iteration *max-agent-iterations*)
      (format *error-output* "~&[agent] ~A hit max iterations (~D)~%"
              ws-name *max-agent-iterations*)
      (finish-agent bb ws-bb ws-name mem :partial
                    "Max iterations reached — task incomplete."
                    (format nil "Hit max iterations (~D)" *max-agent-iterations*))
      (return-from handle-agent-step nil))
    (let ((messages (collect-messages ws-bb))
          (tools (read-section ws-bb :agent-tools))
          (llm-cap (get-capability bb :llm-generation)))
      (unless llm-cap
        (format *error-output* "~&[agent] No :llm-generation capability~%")
        (finish-agent bb ws-bb ws-name mem :failed
                      "Error: no LLM capability available."
                      "No LLM capability")
        (return-from handle-agent-step nil))
      ;; Increment iteration
      (write-section ws-bb :agent-iteration (1+ iteration))
      (format t "~&[agent] ~A iteration ~D (~D messages)~%"
              ws-name (1+ iteration) (length messages))
      (handler-case
          (multiple-value-bind (content tool-calls raw-response)
              (generate-with-tools llm-cap messages tools :role :supervisor)
            (declare (ignore raw-response))
            (write-section ws-bb :agent-retry-count 0)
            ;; Append assistant message to conversation
            (append-message ws-bb (make-assistant-message content tool-calls))
            (cond
              ;; No tool calls — model is done
              ((or (null tool-calls) (zerop (length tool-calls)))
               (format t "~&[agent] ~A finished after ~D iterations~%"
                       ws-name (1+ iteration))
               (finish-agent bb ws-bb ws-name mem :success
                             (or content "")
                             (format nil "Completed in ~D iterations"
                                     (1+ iteration))))
              ;; Tool calls — execute each, check for error loops, then re-trigger
              (t
               (loop for i from 0 below (length tool-calls)
                     for tc = (elt tool-calls i)
                     for tc-name = (let ((fn (gethash "function" tc)))
                                     (when fn (gethash "name" fn)))
                     do (format t "~&[agent]   tool[~D]: ~A~%" i tc-name)
                        (let ((result (execute-tool-call bb ws-bb tc)))
                          (append-message ws-bb
                                          (make-tool-result-message tc result))))
               (maybe-supervisor-intervene bb ws-bb ws-name)
               (retrigger-agent bb ws-name))))
        (error (e)
          (let ((retries (or (read-section ws-bb :agent-retry-count) 0)))
            (format *error-output* "~&[agent] ~A LLM error (retry ~D/~D): ~A~%"
                    ws-name (1+ retries) *max-llm-retries* e)
            (if (< retries *max-llm-retries*)
                (progn
                  (write-section ws-bb :agent-retry-count (1+ retries))
                  (retrigger-agent bb ws-name))
                (finish-agent bb ws-bb ws-name mem :failed
                              (format nil "LLM error after ~D retries: ~A"
                                      *max-llm-retries* e)
                              (format nil "Failed: ~A" e)))))))))
