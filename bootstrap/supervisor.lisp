(defpackage #:demiurge-bootstrap/bootstrap/supervisor
  (:use #:cl)
  (:import-from #:demiurge/src/blackboard/core
                #:blackboard #:read-section #:write-section #:list-sections
                #:watch #:unwatch #:ksar #:ksar-context #:ksar-triggered-key
                #:record-bb-error)
  (:import-from #:demiurge/src/blackboard/workspace
                #:fork-workspace #:merge-workspace #:discard-workspace
                #:workspace-blackboard #:workspace-name
                #:get-workspace #:list-workspaces)
  (:import-from #:demiurge/src/capabilities/registry
                #:get-capability)
  (:import-from #:demiurge/src/capabilities/llm
                #:generate-text #:list-models)
  (:import-from #:demiurge/src/capabilities/code-editing
                #:read-file #:write-file #:patch-file #:list-files)
  (:import-from #:demiurge/src/capabilities/compute
                #:run-command #:create-environment #:exec-in-environment #:destroy-environment)
  (:import-from #:demiurge/src/capabilities/web-search
                #:web-search #:fetch-page)
  (:import-from #:demiurge/src/controller/prompts
                #:build-supervisor-prompt #:build-task-prompt)
  (:import-from #:demiurge/src/persistence/memory
                #:persistent-memory #:mem-get #:mem-set)
  (:import-from #:demiurge/src/persistence/memory-keys
                #:record-task-result #:remember #:recall
                #:log-interaction #:set-preference #:get-preference)
  (:import-from #:demiurge-bootstrap/bootstrap/agent-loop
                #:start-agent-task)
  (:import-from #:alexandria #:when-let)
  (:export #:run-supervisor-step #:dispatch-action #:parse-action
           #:register-supervisor-watchers #:discover-models
           #:execute-steps))

(in-package #:demiurge-bootstrap/bootstrap/supervisor)

;;; ---------------------------------------------------------------------------
;;; JSON response parsing
;;; ---------------------------------------------------------------------------

(defun strip-code-fences (text)
  "Extract content from markdown code fences if present.
If text contains ```...```, returns the content between the first pair.
Otherwise returns the text with leading/trailing whitespace stripped."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
    (multiple-value-bind (start end)
        (cl-ppcre:scan "(?s)```[^\\n]*\\n(.*?)```" trimmed)
      (if start
          (let ((inner (cl-ppcre:register-groups-bind (body)
                           ("(?s)```[^\\n]*\\n(.*?)```" trimmed)
                         body)))
            (if inner
                (string-trim '(#\Space #\Tab #\Newline #\Return) inner)
                trimmed))
          trimmed))))

(defun hash-table-to-plist (ht)
  "Convert a string-keyed hash-table into a plist with string keys."
  (let ((result nil))
    (maphash (lambda (k v) (push v result) (push k result)) ht)
    result))

(defun parse-action (json-string)
  "Parse LLM JSON response into an action plist."
  (handler-case
      (let* ((cleaned (strip-code-fences json-string))
             (parsed  (yason:parse cleaned :object-as :hash-table
                                           :object-key-fn #'identity)))
        (etypecase parsed
          (hash-table (hash-table-to-plist parsed))
          (t (list "action" "idle" "reasoning" "Unexpected JSON shape"))))
    (error ()
      (list "action" "idle" "reasoning" "Failed to parse response"))))

;;; ---------------------------------------------------------------------------
;;; Action dispatch
;;; ---------------------------------------------------------------------------

(defun dispatch-action (action bb mem)
  "Dispatch a parsed action plist against blackboard and memory.
Never signals — all errors are caught and recorded to BB."
  (handler-case
      (let ((act (getf-string action "action")))
        (cond
          ((string-equal act "execute")
           (dispatch-execute action bb mem))
          ((string-equal act "schedule-ks")
           (dispatch-schedule-ks action bb))
          ((string-equal act "workspace")
           (dispatch-workspace action bb))
          ((string-equal act "learn")
           (dispatch-learn action mem))
          ((string-equal act "configure")
           (dispatch-configure action bb))
          ((string-equal act "query")
           (dispatch-query action bb))
          ((string-equal act "idle")
           (when mem
             (when-let (reason (getf-string action "reasoning"))
               (log-interaction mem :role :supervisor :content reason))))
          (t
           (format *error-output* "Unknown supervisor action: ~A~%" act))))
    (serious-condition (e)
      (format *error-output* "~&[dispatch-action] Error: ~A~%" e)
      (record-bb-error bb nil e))))

;;; --- plist helpers ---

(defun getf-string (plist key)
  "Like GETF but compares keys with STRING-EQUAL."
  (loop for (k v) on plist by #'cddr
        when (and (stringp k) (string-equal k key))
          return v))

;;; ---------------------------------------------------------------------------
;;; Step execution engine (synchronous within workspace)
;;; ---------------------------------------------------------------------------

(defun step-result-ok-p (result)
  (and (listp result) (eq :ok (getf result :status))))

(defun step-result-failed-p (result)
  "True if a step has :status :error (set by execute-single-step for non-zero exit codes too)."
  (and (listp result) (eq :error (getf result :status))))

(defvar *max-decomposition-depth* 3)
(defvar *max-recovery-attempts* 2
  "Maximum number of supervisor-driven error recovery cycles per task.")

(defun execute-steps (steps bb ws-bb &key (depth 0) (prefix ""))
  "Execute a list of steps sequentially. Writes each result as a token on WS-BB."
  (let ((results nil)
        (last-output nil))
    (loop for step in (if (listp steps) steps nil)
          for i from 0
          do (let ((result (handler-case
                               (execute-single-step step bb last-output depth)
                             (error (e)
                               (list :status :error :message (format nil "~A" e))))))
               (let ((key (intern (format nil "~ASTEP-~D-RESULT" prefix i) :keyword)))
                 (write-section ws-bb key result))
               (let ((sub-steps (getf result :sub-steps)))
                 (when (and sub-steps (< depth *max-decomposition-depth*))
                   (let ((sub-results (execute-steps sub-steps bb ws-bb
                                                     :depth (1+ depth)
                                                     :prefix (format nil "~A~D." prefix i))))
                     (setf (getf result :sub-results) sub-results)
                     (let ((last-sub (find-if #'step-result-ok-p (reverse sub-results))))
                       (when last-sub
                         (setf (getf result :output) (getf last-sub :output)))))))
               (when (eq :ok (getf result :status))
                 (setf last-output (getf result :output)))
               (push result results)))
    (nreverse results)))

(defun get-step-field (step key)
  (etypecase step
    (hash-table (gethash key step))
    (cons (getf-string step key))))

(defun substitute-prev-output (value prev-output)
  (if (and (stringp value) prev-output (stringp prev-output)
           (search "{{prev_output}}" value))
      (cl-ppcre:regex-replace-all "\\{\\{prev_output\\}\\}" value prev-output)
      value))

(defun execute-single-step (step bb prev-output depth)
  (let* ((cap-name (or (get-step-field step "capability") ""))
         (op-name  (or (get-step-field step "operation") ""))
         (params   (get-step-field step "params"))
         (cap-key  (intern (string-upcase (string-left-trim ":" cap-name)) :keyword))
         (cap      (get-capability bb cap-key)))
    (unless cap
      (return-from execute-single-step
        (list :status :error :message (format nil "Capability ~A not found" cap-name))))
    (let ((result (dispatch-operation cap op-name params prev-output bb)))
      (let* ((op-lc (string-downcase op-name))
             (exit-failed (and (stringp result)
                               (member op-lc '("run-command" "exec-in-env" "exec-in-environment") :test #'string=)
                               (>= (length result) 6)
                               (string= "exit=" result :end2 5)
                               (not (char= #\0 (char result 5)))))
             (status (if exit-failed :error :ok))
             (sub-steps (and (eq status :ok)
                             (stringp result)
                             (< depth *max-decomposition-depth*)
                             (try-parse-sub-steps result))))
        (if sub-steps
            (list :status :ok :output result :operation op-name
                  :capability cap-name :sub-steps sub-steps)
            (list :status status :output result :operation op-name
                  :capability cap-name))))))

(defun try-parse-sub-steps (text)
  (let ((trimmed (string-trim '(#\Space #\Newline #\Return #\Tab) text)))
    (when (and (> (length trimmed) 1)
               (char= (char trimmed 0) #\[))
      (handler-case
          (let ((parsed (yason:parse trimmed :object-as :hash-table
                                              :object-key-fn #'identity)))
            (when (and (listp parsed)
                       (every (lambda (item)
                                (and (hash-table-p item)
                                     (or (gethash "capability" item)
                                         (gethash "operation" item))))
                              parsed))
              parsed))
        (error () nil)))))

(defun get-param (params key &optional default)
  (etypecase params
    (hash-table (or (gethash key params) default))
    (cons (or (getf-string params key) default))
    (null default)))

(defun dispatch-operation (cap op-name params prev-output bb)
  (let ((op (string-downcase op-name)))
    (cond
      ((string= op "generate-text")
       (let* ((prompt (substitute-prev-output
                       (or (get-param params "prompt") (get-param params "content") "")
                       prev-output))
              (role-str (get-param params "role" "coder"))
              (role (intern (string-upcase role-str) :keyword))
              (messages (list (list :role "user" :content prompt)))
              (raw (generate-text cap messages :role role))
              (result (if raw (strip-code-fences raw) nil)))
         (or result (error "generate-text returned NIL for role ~A, prompt length ~D" role (length prompt)))))
      ((string= op "write-file")
       (let ((path (get-param params "path"))
             (content (substitute-prev-output
                       (or (get-param params "content") "") prev-output)))
         (write-file cap path content)
         (format nil "Wrote ~A (~D bytes)" path (length content))))
      ((string= op "read-file")
       (read-file cap (get-param params "path")))
      ((string= op "patch-file")
       (patch-file cap (get-param params "path")
                   (list :old (get-param params "old")
                         :new (substitute-prev-output
                               (or (get-param params "new") "") prev-output)))
       "Patched.")
      ((string= op "list-files")
       (format nil "~{~A~%~}" (list-files cap (get-param params "directory"))))
      ((string= op "run-command")
       (let ((result (run-command cap (substitute-prev-output
                                       (get-param params "command") prev-output))))
         (format nil "exit=~A~%~A~@[~%STDERR: ~A~]" (first result) (second result) (third result))))
      ((or (string= op "create-env") (string= op "create-environment"))
       (let* ((image (get-param params "image"))
              (name  (get-param params "name"))
              (spec  (append (when image (list :image image))
                             (when name  (list :name name))))
              (env   (create-environment cap spec)))
         (let ((envs (or (read-section bb :active-environments) nil)))
           (write-section bb :active-environments (cons env envs)))
         (format nil "Environment created: ~A (image: ~A, workspace: ~A)"
                 (getf env :name) (getf env :image) (getf env :directory))))
      ((or (string= op "exec-in-env") (string= op "exec-in-environment"))
       (let* ((env-name (get-param params "env"))
              (command  (substitute-prev-output (get-param params "command") prev-output))
              (envs     (or (read-section bb :active-environments) nil))
              (env      (find env-name envs :key (lambda (e) (getf e :name)) :test #'string-equal)))
         (unless env
           (error "Environment ~A not found. Active: ~{~A~^, ~}"
                  env-name (mapcar (lambda (e) (getf e :name)) envs)))
         (let ((result (exec-in-environment cap env command)))
           (format nil "exit=~A~%~A~@[~%STDERR: ~A~]" (first result) (second result) (third result)))))
      ((or (string= op "destroy-env") (string= op "destroy-environment"))
       (let* ((env-name (get-param params "env"))
              (envs     (or (read-section bb :active-environments) nil))
              (env      (find env-name envs :key (lambda (e) (getf e :name)) :test #'string-equal)))
         (unless env
           (error "Environment ~A not found" env-name))
         (destroy-environment cap env)
         (write-section bb :active-environments (remove env envs :test #'equal))
         (format nil "Environment ~A destroyed" env-name)))
      ((or (string= op "web-search") (string= op "search"))
       (let* ((query (substitute-prev-output
                      (or (get-param params "query") (get-param params "q") "")
                      prev-output))
              (results (web-search cap query)))
         (with-output-to-string (s)
           (loop for r in results for i from 0
                 do (format s "~D. ~A~%   ~A~%   ~A~%~%"
                            (1+ i) (getf r :title) (getf r :url) (getf r :snippet))))))
      ((string= op "fetch-page")
       (let ((url (substitute-prev-output
                   (or (get-param params "url") "") prev-output)))
         (fetch-page cap url)))
      (t
       (llm-fallback-execute op-name params prev-output bb)))))

(defun llm-fallback-execute (op-name params prev-output bb)
  (let ((llm-cap (get-capability bb :llm-generation)))
    (unless llm-cap
      (error "Unknown operation ~A and no LLM available for fallback" op-name))
    (let* ((params-desc (with-output-to-string (s)
                          (etypecase params
                            (hash-table (yason:encode params s))
                            (cons (format s "~S" params))
                            (null (write-string "{}" s)))))
           (prompt (format nil "You are a step executor. Decompose this operation into sub-steps.~%~
For compute commands: delegate to coder via generate-text (role: coder), then {{prev_output}}.~%~%~
Available: :llm-generation (generate-text), :code-editing (write-file, read-file, etc.), ~
:compute (run-command, create-env, exec-in-env, destroy-env)~%~%~
Sub-step format: [{\"capability\":\"...\",\"operation\":\"...\",\"params\":{...}}, ...]~%~
Operation: ~A~%Parameters: ~A~:[~;~%Previous output: ~:*~A~]~%~%~
Respond with ONLY a JSON array."
                           op-name params-desc prev-output))
           (messages (list (list :role "user" :content prompt))))
      (generate-text llm-cap messages :role :supervisor))))

;;; ---------------------------------------------------------------------------
;;; Individual dispatchers
;;; ---------------------------------------------------------------------------

(defun build-recovery-prompt (original-task failed-results all-results)
  "Build a prompt asking the supervisor to analyze step failures and produce corrective steps."
  (let ((failures (loop for r in all-results for i from 0
                        when (step-result-failed-p r)
                          collect (format nil "Step ~D (~A/~A): ~A"
                                          i
                                          (or (getf r :capability) "?")
                                          (or (getf r :operation) "?")
                                          (or (getf r :output) (getf r :message) "unknown error")))))
    (format nil "You are a SUPERVISOR in a blackboard agent system. A task's execution had failures.~%~%~
ORIGINAL TASK: ~A~%~%~
STEP FAILURES:~%~{  - ~A~%~}~%~
SUCCESSFUL STEPS SO FAR:~%~{  - ~A~%~}~%~%~
ERROR RECOVERY: Produce corrective steps. For any compute commands (shell, apt-get, etc.), ~
delegate to the CODER model — ask via generate-text (role: coder) including the error message, ~
then use {{prev_output}} in the run-command/exec-in-env step.~%~%~
Available capabilities:~%~
  :llm-generation  - generate-text (params: prompt, role) — roles: coder, supervisor, fast~%~
  :code-editing    - write-file, read-file, patch-file, list-files~%~
  :compute (EPHEMERAL): run-command (params: command) — fresh container each call~%~
  :compute (PERSISTENT): create-env / exec-in-env / destroy-env — state persists~%~
  :web-search      - web-search (params: query), fetch-page (params: url)~%~%~
RULES:~%~
  - NEVER put shell commands directly in compute params — ask the coder, then {{prev_output}}~%~
  - Containers run as ROOT — NEVER use sudo~%~
  - /workspace/ is SHARED between host and containers — write-file to /workspace/foo → accessible at /workspace/foo in containers~%~
  - Do NOT use exec-in-env unless you first create-env — use run-command for single shots~%~
  - Use web-search to look up docs/examples if the error is unfamiliar~%~
  - Tell the coder to output RAW content (no markdown fences)~%~
  - Only use real Docker Hub images (ubuntu:24.04, etc.)~%~%~
Respond with ONLY valid JSON (no text before/after): {\"action\":\"execute\",\"reasoning\":\"...\",\"steps\":[...]}~%~
Each step: {\"capability\":\"...\",\"operation\":\"...\",\"params\":{...}}~%~%~
Example recovery — fix a missing sbcl error by asking the coder for correct command:~%~
{\"action\":\"execute\",\"reasoning\":\"sbcl not found, ask coder for install+run command\",\"steps\":[~
{\"capability\":\":llm-generation\",\"operation\":\"generate-text\",~
\"params\":{\"prompt\":\"Write a shell command to install sbcl via apt-get (no sudo, root) and run /workspace/file.lisp with sbcl --script. Chain with &&. Output ONLY the command.\",\"role\":\"coder\"}},~
{\"capability\":\":compute\",\"operation\":\"run-command\",~
\"params\":{\"command\":\"{{prev_output}}\"}}]}"
            original-task
            failures
            (loop for r in all-results for i from 0
                  when (step-result-ok-p r)
                    collect (format nil "Step ~D (~A/~A): ~A"
                                    i
                                    (or (getf r :capability) "?")
                                    (or (getf r :operation) "?")
                                    (let ((out (getf r :output)))
                                      (if (and (stringp out) (> (length out) 120))
                                          (concatenate 'string (subseq out 0 120) "...")
                                          out)))))))

(defun attempt-recovery (bb ws-bb original-task results attempt mem)
  "Ask the supervisor LLM to produce corrective steps for failed results.
Returns new results or NIL if recovery is not possible."
  (let ((llm-cap (get-capability bb :llm-generation)))
    (unless llm-cap (return-from attempt-recovery nil))
    (let* ((prompt (build-recovery-prompt original-task results results))
           (messages (list (list :role "user" :content prompt)))
           (response (generate-text llm-cap messages :role :supervisor)))
      (when mem
        (log-interaction mem :role :supervisor
                         :content (format nil "Recovery attempt ~D: ~A" attempt response)))
      (write-section ws-bb (intern (format nil "RECOVERY-~D-RAW" attempt) :keyword)
                     (if (> (length response) 2000) (subseq response 0 2000) response))
      (let ((action (parse-action response)))
        (unless action
          (format *error-output* "~&[recovery ~D] Could not parse action from LLM response (~D chars)~%" attempt (length response))
          (return-from attempt-recovery nil))
        (unless (string-equal "execute" (getf-string action "action"))
          (format *error-output* "~&[recovery ~D] LLM returned action=~A (not execute)~%" attempt (getf-string action "action"))
          (return-from attempt-recovery nil))
        (let ((new-steps (getf-string action "steps")))
          (unless new-steps
            (format *error-output* "~&[recovery ~D] LLM action has no steps~%" attempt)
            (return-from attempt-recovery nil))
          (write-section ws-bb :recovery-reasoning
                         (getf-string action "reasoning"))
          (write-section ws-bb :recovery-steps new-steps)
              (execute-steps new-steps bb ws-bb
                         :prefix (format nil "RECOVERY-~D-" attempt)))))))

(defun dispatch-execute (action bb mem)
  (let* ((ws-name (or (getf-string action "workspace")
                      (format nil "task-~A" (get-universal-time))))
         (steps   (getf-string action "steps"))
         (ws      (fork-workspace bb ws-name))
         (ws-bb   (workspace-blackboard ws))
         (task-desc (getf-string action "reasoning")))
    (write-section ws-bb :task-steps steps)
    (write-section ws-bb :task-reasoning task-desc)
    (when mem
      (log-interaction mem :role :supervisor
                       :content (format nil "Executing ~A step(s) in workspace ~A"
                                        (length steps) ws-name)))
    (let ((results (execute-steps steps bb ws-bb)))
      ;; Supervisor error recovery loop
      (loop for attempt from 1 to *max-recovery-attempts*
            while (some #'step-result-failed-p results)
            do (format t "~&[supervisor] Recovery attempt ~D for ~A (~D failed steps)~%"
                       attempt ws-name (count-if #'step-result-failed-p results))
               (let ((recovery-results (attempt-recovery bb ws-bb task-desc results attempt mem)))
                 (if (and recovery-results (every #'step-result-ok-p recovery-results))
                     (progn
                       (setf results (append results recovery-results))
                       (return))
                     (when recovery-results
                       (setf results (append results recovery-results))))))
      (write-section ws-bb :step-results results)
      (when mem
        (let ((ok-count (count-if #'step-result-ok-p results))
              (fail-count (count-if #'step-result-failed-p results)))
          (record-task-result mem
                              :workspace ws-name
                              :status (cond ((zerop fail-count) :success)
                                            ((zerop ok-count) :failed)
                                            (t :partial))
                              :description (format nil "~D/~D steps succeeded~@[, ~D recovered~]"
                                                   ok-count (length results)
                                                   (when (> (length results) (length steps))
                                                     (- (length results) (length steps))))))))))

(defun dispatch-schedule-ks (action bb)
  (let* ((ks-name (getf-string action "ks"))
         (ws-name (getf-string action "workspace"))
         (params  (getf-string action "params"))
         (ks      (demiurge/src/knowledge-source/registry:find-ks bb ks-name)))
    (if ks
        (let ((ws (or (when ws-name (get-workspace bb ws-name))
                      (fork-workspace bb (format nil "ks-~A-~A" ks-name (get-universal-time))))))
          (write-section (workspace-blackboard ws) :scheduled-ks ks-name)
          (write-section (workspace-blackboard ws) :ks-params params)
          ;; Write a new pending-task token to trigger supervisor
          (write-section bb :pending-task
                         (list :id (format nil "ks-~A" (get-universal-time))
                               :description (format nil "Execute KS ~A" ks-name)
                               :source :supervisor
                               :ks ks-name :workspace (workspace-name ws) :params params)))
        (format *error-output* "KS not found: ~A~%" ks-name))))

(defun dispatch-workspace (action bb)
  (let ((op   (getf-string action "operation"))
        (name (getf-string action "name")))
    (cond
      ((string-equal op "fork") (fork-workspace bb name))
      ((string-equal op "merge")
       (when-let (ws (get-workspace bb name)) (merge-workspace ws)))
      ((string-equal op "discard")
       (when-let (ws (get-workspace bb name)) (discard-workspace ws)))
      (t (format *error-output* "Unknown workspace operation: ~A~%" op)))))

(defun dispatch-learn (action mem)
  (when mem
    (remember mem
              (or (getf-string action "topic") "unnamed")
              (or (getf-string action "content") "")
              :confidence (or (getf-string action "confidence") 1.0)
              :source     (getf-string action "source"))))

(defun dispatch-configure (action bb)
  (when-let (section (getf-string action "section"))
    (let ((key (intern (string-upcase section) :keyword)))
      (write-section bb key (getf-string action "value")))))

(defun dispatch-query (action bb)
  (write-section bb :pending-queries
                 (list :questions (getf-string action "questions")
                       :capabilities-needed (getf-string action "capabilities_needed")
                       :timestamp (get-universal-time))))

;;; ---------------------------------------------------------------------------
;;; Core supervisor step
;;; ---------------------------------------------------------------------------

(defun run-supervisor-step (bb mem &key task-context)
  "Execute one supervisor reasoning cycle. Returns the parsed action plist."
  (let* ((prompt   (build-supervisor-prompt bb :mem mem :task-context task-context))
         (llm-cap  (get-capability bb :llm-generation)))
    (unless llm-cap
      (format *error-output* "No :llm-generation capability registered~%")
      (return-from run-supervisor-step
        (list "action" "idle" "reasoning" "No LLM capability available")))
    (let* ((messages (list (list :role "system" :content prompt)))
           (response (generate-text llm-cap messages :role :supervisor)))
      (when mem
        (log-interaction mem :role :supervisor :content response))
      (let ((action (parse-action response)))
        (dispatch-action action bb mem)
        action))))

;;; ---------------------------------------------------------------------------
;;; Watcher registration (replaces register-supervisor-handlers)
;;; ---------------------------------------------------------------------------

(defun register-supervisor-watchers (bb mem)
  "Wire the supervisor into the blackboard via watchers.
Replaces the old event-bus subscription model."
  ;; New task — watches :pending-task token (persistent, high priority)
  ;; Dispatches to either tool-calling agent loop or plan-based supervisor
  ;; based on :agent-mode BB section (:tool-calling or :plan, default :plan)
  (watch bb :id :supervisor-task
         :requires '(:pending-task)
         :handler (lambda (bb ksar)
                    (let* ((ctx-val (cdr (assoc :pending-task (ksar-context ksar))))
                           (desc (if (listp ctx-val) (getf ctx-val :description) ctx-val))
                           (source (if (listp ctx-val) (getf ctx-val :source) :unknown))
                           (raw-mode (read-section bb :agent-mode))
                           (mode (cond
                                   ((null raw-mode) :plan)
                                   ((keywordp raw-mode) raw-mode)
                                   ((and (stringp raw-mode)
                                         (string-equal raw-mode "TOOL-CALLING"))
                                    :tool-calling)
                                   (t :plan))))
                      (handler-case
                          (ecase mode
                            (:tool-calling
                             (format t "~&[supervisor] Starting agent task: ~A~%" desc)
                             (start-agent-task bb mem desc))
                            (:plan
                             (let ((task-ctx (list :type "task" :description desc :source source)))
                               (run-supervisor-step bb mem :task-context task-ctx))))
                        (serious-condition (e)
                          (format *error-output* "~&[supervisor] Task error: ~A~%" e)
                          (record-bb-error bb ksar e)))))
         :priority 100
         :one-shot nil)

  ;; Periodic model discovery — watches :tick token (persistent, low priority)
  (watch bb :id :model-discovery
         :requires '(:tick)
         :handler (lambda (bb ksar)
                    (declare (ignore ksar))
                    (handler-case
                        (let* ((last-discovery (or (read-section bb :last-model-discovery) 0))
                               (now (get-universal-time)))
                          (when (> (- now last-discovery) 60)
                            (discover-models bb)))
                      (serious-condition (e)
                        (format *error-output* "~&[supervisor] Model discovery error: ~A~%" e)
                        (record-bb-error bb ksar e))))
         :priority 10
         :one-shot nil))

;;; ---------------------------------------------------------------------------
;;; Model discovery
;;; ---------------------------------------------------------------------------

(defun discover-models (bb)
  "Probe the LLM provider for available models and cache on the blackboard."
  (when-let (llm-cap (get-capability bb :llm-generation))
    (let ((models (list-models llm-cap)))
      (write-section bb :available-models models)
      (write-section bb :last-model-discovery (get-universal-time))
      models)))
