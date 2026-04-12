(defpackage #:demiurge/src/controller/prompts
  (:use #:cl)
  (:import-from #:alexandria #:when-let)
  (:import-from #:demiurge/src/blackboard/core
                #:blackboard #:read-section #:list-sections)
  (:import-from #:demiurge/src/capabilities/registry
                #:list-capabilities #:capability-schema)
  (:import-from #:demiurge/src/knowledge-source/registry
                #:list-ks)
  (:import-from #:demiurge/src/knowledge-source/protocol
                #:ks-name #:ks-version)
  (:import-from #:demiurge/src/blackboard/workspace
                #:list-workspaces #:workspace-name #:workspace-status)
  (:import-from #:demiurge/src/persistence/memory
                #:persistent-memory #:mem-get #:mem-keys)
  (:import-from #:demiurge/src/persistence/memory-keys
                #:recent-tasks #:ks-success-rate #:recent-interactions
                #:get-preference)
  (:export #:build-supervisor-prompt #:build-task-prompt #:build-review-prompt
           #:build-self-improve-prompt #:build-merge-review-prompt
           #:build-agent-prompt
           #:*identity-preamble* #:*architecture-section*
           #:format-capability-catalog #:format-memory-context))

(in-package #:demiurge/src/controller/prompts)

;;; --- Static sections ---

(defvar *identity-preamble*
  "You are Demiurge, an autonomous self-improving software agent built as a blackboard system in Common Lisp.

Your purpose is to:
1. Work on issues and tasks in Git repositories (GitHub and Forgejo)
2. Continuously improve your own components when idle
3. Maintain high quality through A/B testing of component versions
4. Learn from every execution to improve future performance

You are NOT a chatbot. You are an autonomous agent. Your responses are structured
commands and decisions that drive the blackboard controller.")

(defvar *architecture-section*
  "## Architecture

You operate as the **supervisor** of a blackboard system:

- **Blackboard**: Shared state store with named sections. All data flows through here.
- **Workspaces**: Isolated copy-on-write branches of the blackboard for parallel task execution.
  Each task runs in its own workspace. Workspaces can be forked, merged, or discarded.
- **Capabilities**: Abstract operation interfaces (protocol-only). Implementations are pluggable.
  You interact with the world ONLY through capability invocations.
- **Knowledge Sources (KS)**: Components that read from and write to the blackboard.
  They can be versioned for A/B testing. You decide which KS to schedule.
- **Event Bus**: Reactive event stream. You respond to events, not poll.
- **Persistent Memory**: Durable store for learned patterns, KS metrics, task history,
  and preferences. Survives restarts.

### Decision Flow

1. An event arrives (new task, timer tick, KS completion, etc.)
2. You see the current blackboard state + relevant memory
3. You decide: which KS to run, in which workspace, with what parameters
4. The controller executes your decision
5. Results are written to the blackboard
6. You review and decide next action")

(defun format-capability-catalog (bb)
  "Render available capabilities as prompt text."
  (with-output-to-string (s)
    (format s "## Available Capabilities~%~%")
    (let ((caps (list-capabilities bb)))
      (if (null caps)
          (format s "_No capabilities registered._~%")
          (dolist (cap caps)
            (format s "### :~(~A~)~%" (getf cap :name))
            (format s "~A~%" (or (getf cap :description) ""))
            (when-let (ops (getf cap :operations))
              (format s "Operations:~%")
              (dolist (op ops)
                (format s "  - `~(~A~)`: ~A~%" op op)))
            (format s "~%"))))))

(defun format-workspace-state (bb)
  "Render active workspace list."
  (with-output-to-string (s)
    (let ((ws-list (list-workspaces bb)))
      (format s "## Active Workspaces (~A)~%~%" (length ws-list))
      (if (null ws-list)
          (format s "_No active workspaces._~%")
          (dolist (ws ws-list)
            (format s "- **~A** [~A]~%" (workspace-name ws) (workspace-status ws)))))))

(defun format-bb-sections (bb &key (max-value-len 200))
  "Render key blackboard sections."
  (with-output-to-string (s)
    (let ((sections (list-sections bb)))
      (format s "## Blackboard Sections (~A)~%~%" (length sections))
      (dolist (key sections)
        (let* ((val (read-section bb key))
               (repr (format nil "~A" val)))
          (format s "- **~A**: ~A~%" key
                  (if (> (length repr) max-value-len)
                      (concatenate 'string (subseq repr 0 (- max-value-len 3)) "...")
                      repr)))))))

(defun format-ks-list (bb &optional mem)
  "Render available KS with optional performance data."
  (with-output-to-string (s)
    (let ((ks-list (list-ks bb)))
      (format s "## Knowledge Sources (~A)~%~%" (length ks-list))
      (if (null ks-list)
          (format s "_No KS registered._~%")
          (dolist (ks ks-list)
            (let ((name (ks-name ks)))
              (format s "- **~A** v~A" name (ks-version ks))
              (when mem
                (when-let (rate (ks-success-rate mem name))
                  (format s " (success: ~,1F%)" (* 100 rate))))
              (format s "~%")))))))

(defun format-memory-context (mem &key (recent-tasks-n 5) (recent-interactions-n 5))
  "Render relevant persistent memory as prompt context."
  (unless mem (return-from format-memory-context ""))
  (with-output-to-string (s)
    (format s "## Memory Context~%~%")
    ;; Preferences
    (let ((pref-keys (mem-keys mem "pref:")))
      (when pref-keys
        (format s "### Preferences~%")
        (dolist (k pref-keys)
          (format s "- ~A: ~A~%" (subseq k 5) (mem-get mem k)))
        (format s "~%")))
    ;; Learned patterns
    (let ((learn-keys (mem-keys mem "learn:")))
      (when learn-keys
        (format s "### Learned Patterns~%")
        (dolist (k learn-keys)
          (let ((entry (mem-get mem k)))
            (when entry
              (format s "- **~A**: ~A~%" (subseq k 6) (getf entry :content)))))
        (format s "~%")))
    ;; Recent tasks
    (let ((tasks (recent-tasks mem recent-tasks-n)))
      (when tasks
        (format s "### Recent Tasks (~A)~%" (length tasks))
        (dolist (task tasks)
          (format s "- [~A] ~A: ~A~%"
                  (or (getf task :status) "?")
                  (or (getf task :task-id) "?")
                  (or (getf task :description) "")))
        (format s "~%")))
    ;; Recent interactions (abbreviated)
    (let ((interactions (recent-interactions mem recent-interactions-n)))
      (when interactions
        (format s "### Recent LLM Interactions (~A)~%" (length interactions))
        (dolist (i interactions)
          (let ((content (or (getf i :content) "")))
            (format s "- [~A/~A] ~A~%"
                    (or (getf i :role) "?")
                    (or (getf i :model) "?")
                    (if (> (length content) 100)
                        (concatenate 'string (subseq content 0 97) "...")
                        content))))
        (format s "~%")))))

;;; --- Prompt builders ---

(defun build-supervisor-prompt (bb &key mem task-context)
  "Build the full system prompt for the supervisor model.
   BB is the current blackboard. MEM is optional persistent memory.
   TASK-CONTEXT is an optional plist (:type :description :issue :repo etc.)."
  (with-output-to-string (s)
    ;; Identity
    (format s "~A~%~%" *identity-preamble*)
    ;; Architecture
    (format s "~A~%~%" *architecture-section*)
    ;; Dynamic state
    (format s "~A" (format-capability-catalog bb))
    (format s "~A" (format-ks-list bb mem))
    (format s "~A" (format-workspace-state bb))
    (format s "~A" (format-bb-sections bb))
    ;; Memory
    (when mem
      (format s "~A" (format-memory-context mem)))
    ;; Task-specific context
    (when task-context
      (format s "## Current Task~%~%")
      (loop for (k v) on task-context by #'cddr
            do (format s "- **~A**: ~A~%" k v))
      (format s "~%"))
    ;; Response format
    (format s "~A" (response-format-section))))

(defun response-format-section ()
  "Instructions for structured output."
  "## Response Format

Respond with a JSON object. The controller parses your response to drive execution.

### For task execution:

You are the SUPERVISOR — you plan and orchestrate COMPLETE task execution.

CRITICAL: When a task says \"write X and run it\", you MUST produce ALL FOUR steps:
  1. generate-text (coder) → produce source code
  2. write-file → save to /workspace/
  3. generate-text (coder) → produce the shell run command
  4. run-command → execute in container
A plan with only step 1 is INCOMPLETE. You must always include write-file AND run-command.

For CODE GENERATION: use generate-text (role: coder) to produce source code, then write-file.
For COMPUTE COMMANDS: ALWAYS delegate to the CODER model. Never write shell commands yourself.
  Ask the coder via generate-text (role: coder) for the exact command, then use {{prev_output}}.

Example — write and run ANY program (adapt the prompts to the actual task):
```json
{
  \"action\": \"execute\",
  \"reasoning\": \"Generate code, write to /workspace/, ask coder for run command, execute\",
  \"steps\": [
    {\"capability\": \":llm-generation\", \"operation\": \"generate-text\",
     \"params\": {\"prompt\": \"<describe what code to write>. Output ONLY source code, no markdown.\", \"role\": \"coder\"}},
    {\"capability\": \":code-editing\", \"operation\": \"write-file\",
     \"params\": {\"path\": \"/workspace/<filename>\", \"content\": \"{{prev_output}}\"}},
    {\"capability\": \":llm-generation\", \"operation\": \"generate-text\",
     \"params\": {\"prompt\": \"Write a shell command to install <runtime> (apt-get, no sudo) and run /workspace/<filename>. Chain with &&. Output ONLY the command.\", \"role\": \"coder\"}},
    {\"capability\": \":compute\", \"operation\": \"run-command\",
     \"params\": {\"command\": \"{{prev_output}}\"}}
  ]
}
```

IMPORTANT: Files under /workspace/ are shared between host and ALL containers (ephemeral and persistent).
  write-file to /workspace/foo.lisp → visible inside containers at /workspace/foo.lisp.
RULE: Never put shell commands directly in compute step params — ask the coder first, then {{prev_output}}.
RULE: ALWAYS include all 4 steps (generate, write, generate-command, run) for write-and-run tasks.

### For scheduling a KS:
```json
{
  \"action\": \"schedule-ks\",
  \"ks\": \"<ks-name>\",
  \"workspace\": \"<workspace-name>\",
  \"params\": {},
  \"reasoning\": \"Why this KS now\"
}
```

### For workspace management:
```json
{
  \"action\": \"workspace\",
  \"operation\": \"fork|merge|discard\",
  \"name\": \"<workspace-name>\",
  \"parent\": \"<parent-workspace for fork>\",
  \"reasoning\": \"Why\"
}
```

### For self-improvement:
```json
{
  \"action\": \"improve\",
  \"target\": \"<ks-name>\",
  \"analysis\": \"What to improve and why\",
  \"approach\": \"How to improve it\"
}
```

### For learning (store to persistent memory):
```json
{
  \"action\": \"learn\",
  \"topic\": \"<topic-key>\",
  \"content\": \"What was learned\",
  \"confidence\": 0.8,
  \"source\": \"observation|feedback|experiment\"
}
```

### For web research:
```json
{
  \"action\": \"execute\",
  \"reasoning\": \"Need to look up the API docs for library X\",
  \"steps\": [
    {\"capability\": \":web-search\", \"operation\": \"web-search\",
     \"params\": {\"query\": \"library X API documentation\"}},
    {\"capability\": \":web-search\", \"operation\": \"fetch-page\",
     \"params\": {\"url\": \"{{prev_output}}\"}}
  ]
}
```

### For requesting more information:
```json
{
  \"action\": \"query\",
  \"questions\": [\"What is the project structure?\", \"Which test framework is used?\"],
  \"capabilities_needed\": [\":code-intelligence\", \":code-editing\"]
}
```

### For idle / no action needed:
```json
{
  \"action\": \"idle\",
  \"reasoning\": \"Why no action is needed right now\"
}
```

IMPORTANT RULES:
- NEVER put shell commands directly in compute params — delegate to coder via generate-text, then {{prev_output}}.
- Always include \"reasoning\" to explain your decision.
- Prefer small, incremental steps over large monolithic changes.
- Always test changes before committing.
- Use workspaces for isolation — never modify the root blackboard directly for tasks.
- /workspace/ is a SHARED directory between host and all containers.
  write-file to /workspace/foo.lisp → visible inside containers at /workspace/foo.lisp.
  Always write generated code to /workspace/ paths.
- :compute has TWO modes:
  * EPHEMERAL: run-command — each call is a FRESH container (podman run --rm), /workspace/ mounted.
    Installed packages and files outside /workspace/ are LOST between calls.
    Chain related commands with && in ONE call: \"apt-get update && apt-get install -y sbcl && sbcl --script /workspace/fib.lisp\"
  * PERSISTENT: create-env (params: name, image) -> exec-in-env (params: env, command) -> destroy-env (params: env).
    State persists across exec-in-env calls. Use for multi-step work: install tools, write files, run code.
    Only use real Docker Hub images (ubuntu:24.04, debian:bookworm, etc.).
  * BOTH modes run as root — NEVER use sudo. /workspace/ is always available.
- :web-search — search the web or fetch pages:
  * web-search (params: query) — returns search results with titles, URLs, and snippets.
  * fetch-page (params: url) — fetches a URL and returns text content.
  Use web-search when you need to find documentation, examples, APIs, or any info not in memory.
- If a step fails, the supervisor will attempt automatic recovery with corrected steps.
")

(defun build-task-prompt (bb task-description &key mem issue repo)
  "Build prompt for a specific task."
  (build-supervisor-prompt bb
    :mem mem
    :task-context (append
                   (list :type "task" :description task-description)
                   (when issue (list :issue issue))
                   (when repo (list :repo repo)))))

(defun build-review-prompt (bb workspace-name &key mem diff)
  "Build prompt for reviewing a workspace before merge."
  (with-output-to-string (s)
    (format s "~A~%~%" *identity-preamble*)
    (format s "## Review Request~%~%")
    (format s "Workspace **~A** is ready for review before merging to the root blackboard.~%~%" workspace-name)
    (format s "~A" (format-bb-sections bb))
    (when diff
      (format s "### Changes~%```~%~A~%```~%~%" diff))
    (when mem
      (format s "~A" (format-memory-context mem)))
    (format s "~%Respond with:~%")
    (format s "```json~%{\"action\": \"merge\", \"approved\": true|false, \"reasoning\": \"...\", \"suggestions\": [...]}~%```~%")))

(defun build-self-improve-prompt (bb target-ks &key mem metrics)
  "Build prompt for self-improvement analysis."
  (with-output-to-string (s)
    (format s "~A~%~%" *identity-preamble*)
    (format s "## Self-Improvement Cycle~%~%")
    (format s "Target KS: **~A** v~A~%~%" (ks-name target-ks) (ks-version target-ks))
    (when metrics
      (format s "### Current Metrics~%")
      (loop for (k v) on metrics by #'cddr
            do (format s "- ~A: ~A~%" k v))
      (format s "~%"))
    (format s "~A" (format-capability-catalog bb))
    (when mem
      (format s "~A" (format-memory-context mem)))
    (format s "~%Analyze this KS and propose a concrete improvement. Respond with:~%")
    (format s "```json~%")
    (format s "{\"action\": \"improve\", \"target\": \"<ks>\", \"analysis\": \"...\", ")
    (format s "\"changes\": [{\"file\": \"...\", \"description\": \"...\"}], ")
    (format s "\"expected_improvement\": \"...\", \"risk\": \"low|medium|high\"}~%")
    (format s "```~%")))

(defun build-merge-review-prompt (parent-bb workspace-bb workspace-name &key mem)
  "Build prompt for reviewing workspace merge conflicts."
  (with-output-to-string (s)
    (format s "~A~%~%" *identity-preamble*)
    (format s "## Workspace Merge Review~%~%")
    (format s "Workspace **~A** needs to be merged into the parent blackboard.~%~%" workspace-name)
    (format s "### Parent State~%")
    (format s "~A" (format-bb-sections parent-bb))
    (format s "### Workspace State~%")
    (format s "~A" (format-bb-sections workspace-bb))
    (when mem
      (format s "~A" (format-memory-context mem)))
    (format s "~%Decide how to merge. For each conflicting section, choose parent or workspace value.~%")
    (format s "```json~%")
    (format s "{\"action\": \"merge-resolve\", \"resolutions\": {\"<section>\": \"parent|workspace\"}, ")
    (format s "\"reasoning\": \"...\"}~%")
    (format s "```~%")))

;;; --- Agent (tool-calling) prompt ---

(defvar *agent-identity*
  "You are Demiurge, an autonomous software agent with direct tool access.

You complete tasks by calling tools — reading/writing files, running commands in
containers, searching the web, and inspecting the blackboard. You have full
autonomy: decide what to do, call the tools, observe results, iterate.

You are NOT generating a plan for someone else to execute. You ARE the executor.")

(defvar *agent-rules*
  "## Rules

### File system
- /workspace/ is a SHARED directory mounted into every container.
  write_file to /workspace/foo.lisp → visible at /workspace/foo.lisp inside any container.

### Compute — CRITICAL
- **run_command** spawns a FRESH ephemeral container every call.
  Anything installed or written outside /workspace/ is LOST between calls.
  ALWAYS chain related commands with && in ONE call:
    GOOD: run_command(\"apt-get update && apt-get install -y sbcl && sbcl --script /workspace/fib.lisp\")
    BAD:  run_command(\"apt-get install -y sbcl\") then run_command(\"sbcl --script /workspace/fib.lisp\")  ← sbcl is gone!
- For multi-step work needing persistent state (install tools, compile, run, debug),
  use **create_environment** + **exec_in_environment** instead.
  State (installed packages, files) persists across exec_in_environment calls.
- All containers run as root. NEVER use sudo.
- Use real Docker Hub images (ubuntu:24.04, debian:bookworm, etc.).

### Strategy
- For simple tasks (write file + run once): write_file then ONE run_command with chained commands.
- For complex tasks (install tools, iterate): create_environment, then exec_in_environment repeatedly.
- When a tool returns an error, analyze it and try a different approach.
- When you're done, respond with a text summary of what you accomplished.
  Do NOT call any more tools after you're finished.")

(defun build-agent-prompt (bb &key mem task)
  "Build the system prompt for the tool-calling agent mode.
Simpler than the supervisor prompt — no JSON plan format, just tool descriptions and rules."
  (declare (ignore task))
  (with-output-to-string (s)
    (format s "~A~%~%" *agent-identity*)
    (format s "~A~%~%" *agent-rules*)
    (format s "~A" (format-capability-catalog bb))
    (format s "~A" (format-bb-sections bb))
    (when mem
      (format s "~A" (format-memory-context mem)))))
