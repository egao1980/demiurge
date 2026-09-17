(in-package #:demiurge/tests)

(defparameter *research-questions*
  '("What is KSAR?" "What is a blackboard?" "What is a journal?"))

(defun %turns-text (turns)
  (cond
    ((stringp turns) turns)
    ((listp turns)
     (with-output-to-string (s)
       (dolist (tn turns)
         (write-string (if (stringp tn) tn (llm:turn-text tn)) s))))
    (t (princ-to-string turns))))

(defun %research-llm (&key (questions *research-questions*)
                           gap-once
                           (include-answers-in-synthesis t)
                           synthesis-text)
  "Scripted research LLM.
   GAP-ONCE (string) is emitted as a new subquestion on the first gap call.
   INCLUDE-ANSWERS-IN-SYNTHESIS nil omits ANSWER: lines so the A1 gate fails."
  (let ((gap-remaining (if gap-once 1 0))
        (gap-q (if (stringp gap-once) gap-once "What is a restart?"))
        (all-qs (if (and gap-once (stringp gap-once))
                    (append questions (list gap-once))
                    questions)))
    (llm:make-mock-llm-backend
     :handler
     (lambda (backend turns &key &allow-other-keys)
       (declare (ignore backend))
       (let* ((text (%turns-text turns))
              (sub-pos (search "Subquestion: " text))
              (sub (when sub-pos
                     (let* ((start (+ sub-pos (length "Subquestion: ")))
                            (end (or (position #\Newline text :start start)
                                     (length text))))
                       (string-trim '(#\Space #\Tab #\Return)
                                    (subseq text start end)))))
              (q (or (find sub all-qs :test #'string-equal)
                     (find-if (lambda (q) (search q text)) all-qs))))
         (cond
           ((search "Gap analysis" text)
            (if (plusp gap-remaining)
                (progn
                  (decf gap-remaining)
                  (llm:make-llm-response
                   :parts (list (llm:make-llm-text-part :text "gap"))
                   :output (make-research-plan
                            :question "q"
                            :subquestions
                            (list (make-research-subquestion
                                   :id "gap-1"
                                   :question gap-q)))))
                (llm:make-llm-response
                 :parts (list (llm:make-llm-text-part :text "none"))
                 :output (make-research-plan :question "q" :subquestions nil))))
           ((search "Decompose" text)
            (llm:make-llm-response
             :parts (list (llm:make-llm-text-part :text "plan"))
             :output (make-research-plan
                      :question "CL expert systems"
                      :subquestions
                      (loop for q in questions
                            for i from 1
                            collect (make-research-subquestion
                                     :id (format nil "q~d" i)
                                     :question q)))))
           ((or (search "ONE subquestion" text)
                (search "research child" text)
                (search "Subquestion:" text))
            (llm:make-llm-response
             :parts (list (llm:make-llm-text-part
                           :text (format nil "ANSWER:~a [~a]"
                                         (or q "unknown")
                                         (if q
                                             (format nil "src-~a"
                                                     (substitute #\- #\Space q))
                                             "src-1"))))))
           (t
            (llm:make-llm-response
             :parts (list (llm:make-llm-text-part
                           :text (or synthesis-text
                                     (if include-answers-in-synthesis
                                         (format nil "Cited briefing.~%~{~a~%~}"
                                                 (mapcar (lambda (qq)
                                                           (format nil "ANSWER:~a" qq))
                                                         all-qs))
                                         "I omit the expected findings."))))))))))))

(defun %hit-url (query)
  (format nil "https://ex.test/~a" (substitute #\- #\Space (string query))))

(defun %research-websearch ()
  (web:make-mock-websearch-backend
   :pages (loop for q in *research-questions*
                collect (cons (%hit-url q)
                              (format nil "<p>ANSWER:~a page body for the workspace.</p>" q)))
   :handler
   (lambda (backend query &key &allow-other-keys)
     (declare (ignore backend))
     (list (web:make-search-hit
            :url (%hit-url query)
            :title (string query)
            :snippet (format nil "ANSWER:~a" query)
            :rank 1
            :source "mock")))))

(defun %research-domain (&key name)
  (make-expert-domain :name (or name "research-demo")
                      :catalogue (cap:make-catalogue :world)
                      :profile :personal))

(defun %count-events (journal type &optional task-id)
  (loop for id in (if task-id
                      (list task-id)
                      (task:journal-task-ids journal))
        sum (count-if (lambda (e) (typep e type))
                      (task:journal-events
                       journal (task:make-durable-task :id id)))))

(defun %run-research (&key journal task-id llm websearch budget blackboard
                        tree-root
                        (max-rounds 1) (question "CL expert systems")
                        require-hitl browser domain)
  (run-deep-research (or domain (%research-domain))
                     question
                     :max-rounds max-rounds
                     :budget budget
                     :llm (or llm (%research-llm))
                     :websearch (or websearch (%research-websearch))
                     :journal (or journal (task:make-in-memory-journal))
                     :task-id (or task-id "research-e2e")
                     :blackboard blackboard
                     :tree-root tree-root
                     :require-hitl require-hitl
                     :browser browser))

(deftest make-research-plan-accepts-jzon-vector
  "jzon decodes JSON arrays as vectors; plan construction must not MAPCAR them."
  (let* ((row (make-hash-table :test 'equal))
         (rows nil)
         (plan nil))
    (setf (gethash "id" row) "q2"
          (gethash "question" row) "blackboard")
    (setf rows (vector (make-research-subquestion :id "q1" :question "KSAR")
                       row))
    (setf plan (make-research-plan :question "CL" :subquestions rows))
    (ok (= 2 (length (research-plan-subquestions plan))))
    (ok (equal "KSAR"
               (research-subquestion-question
                (first (research-plan-subquestions plan)))))
    (ok (equal "blackboard"
               (research-subquestion-question
                (second (research-plan-subquestions plan)))))))

(deftest research-plan-emits-json-schema
  "Live openai-compat needs llm-protocol/schema for :output research-plan."
  (let ((schema (llm:structured-output-json-schema 'research-plan)))
    (ok (hash-table-p schema))))

(deftest generate-research-step-retries-then-dies-on-empty
  "Empty structured-output completion retries, then RESEARCH-ERROR."
  (let* ((*research-output-attempts* 2)
         (llm:*structured-output-repair* :signal)
         (n 0)
         (bare (llm:make-mock-llm-backend
                :handler (lambda (backend turns &key &allow-other-keys)
                           (declare (ignore backend turns))
                           (incf n)
                           (llm:make-llm-response
                            :parts (list (llm:make-llm-text-part :text ""))))))
         (llm (wrap-llm-observe bare :expert "t" :scope "t")))
    (handler-case
        (progn
          (generate-research-step llm :plan "CL expert systems"
                                  :output 'research-plan)
          (ok nil "expected research-error"))
      (research-error (e)
        (let ((msg (string-downcase (or (demiurge-error-message e) ""))))
          (ok (or (search "failed" msg) (search "empty" msg))))))
    (ok (plusp n))))

(deftest generate-research-step-invalid-json-falls-back
  "Invalid JSON with a nonempty completion is returned for COERCE-RESEARCH-PLAN."
  (let* ((*research-output-attempts* 2)
         (llm:*structured-output-repair* :signal)
         (n 0)
         (bare (llm:make-mock-llm-backend
                :handler (lambda (backend turns &key &allow-other-keys)
                           (declare (ignore backend turns))
                           (incf n)
                           (llm:make-llm-response
                            :parts (list (llm:make-llm-text-part :text "not-json"))))))
         (llm (wrap-llm-observe bare :expert "t" :scope "t"))
         (r (generate-research-step llm :plan "CL expert systems"
                                    :output 'research-plan)))
    (ok (= 2 n))
    (ok (llm:llm-response-p r))
    (ok (null (llm:llm-response-output r)))
    (ok (search "not-json" (or (llm:llm-response-text r) "")))
    (ok (research-plan-p (coerce-research-plan
                          (llm:llm-response-text r)
                          :question "CL expert systems")))))

(deftest generate-research-step-traces-llm
  "Live demo needs these lines flushed before GENERATE blocks on HTTP."
  (let* ((out (make-string-output-stream))
         (*research-trace-stream* out)
         (llm (llm:make-mock-llm-backend
               :handler (lambda (backend turns &key &allow-other-keys)
                          (declare (ignore backend turns))
                          (llm:make-llm-response
                           :parts (list (llm:make-llm-text-part :text "ok")))))))
    (generate-research-step llm :child "hello")
    (let ((s (get-output-stream-string out)))
      (ok (search "LLM GENERATE :CHILD" (string-upcase s)))
      (ok (search "DONE" (string-upcase s))))))

(deftest generate-research-step-retry-succeeds
  (let* ((*research-output-attempts* 3)
         (n 0)
         (bare (llm:make-mock-llm-backend
                :handler (lambda (backend turns &key &allow-other-keys)
                           (declare (ignore backend turns))
                           (incf n)
                           (llm:make-llm-response
                            :parts (list (llm:make-llm-text-part
                                          :text (if (= n 1)
                                                    "not-json"
                                                    "{\"question\":\"q\",\"subquestions\":[{\"id\":\"q1\",\"question\":\"KSAR\",\"rationale\":\"\"}]}")))))))
         (llm (wrap-llm-observe bare :expert "t" :scope "t"))
         (r (generate-research-step llm :plan "q" :output 'research-plan)))
    (ok (= 2 n))
    (ok (llm:llm-response-p r))
    (ok (research-plan-p (llm:llm-response-output r)))))

(deftest deep-research-e2e-mock-llm-websearch
  (let* ((board (bb:make-blackboard))
         (result (%run-research :task-id "research-e2e"
                                :blackboard board)))
    (ok (eq :pass (getf result :verdict)))
    (ok (stringp (getf result :markdown)))
    (ok (= 3 (length (getf result :children))))
    (ok (bb:section-bound-p board :round-summary))
    (ok (search "https://ex.test/" (getf result :markdown))
        "rendered report includes a source URL")
    (ok (search "[" (getf result :markdown))
        "rendered report includes a block-id / source id")
    (ok (search "Budget scope:" (getf result :markdown))
        "rendered report includes the budget footer")
    (ok (search "Sources" (getf result :markdown)))
    (dolist (q *research-questions*)
      (ok (search (format nil "ANSWER:~a" q) (getf result :markdown))
          (format nil "report cites ~a" q)))))

(deftest deep-research-kill-and-resume-replays-finished-children
  (let* ((journal (task:make-in-memory-journal))
         (task-id "research-resume")
         (exec 0)
         (llm (%research-llm))
         (web (%research-websearch)))
    (let ((*research-child-exec-hook*
           (lambda (in)
             (declare (ignore in))
             (incf exec)))
          (*research-child-hook*
           (lambda (child input)
             (declare (ignore input))
             (when (and (eq :completed (task:durable-task-status child))
                        (= exec 2))
               (error "killed after 2 of 3 children")))))
      (handler-case
          (%run-research :journal journal :task-id task-id
                         :llm llm :websearch web)
        (error ())))
    (ok (= 2 exec) "two children executed before kill")
    (ok (= 2 (%count-events journal 'task:child-spawned task-id))
        "two child-spawned events after kill")
    (let ((completed-after-kill
           (%count-events journal 'task:task-completed)))
      (let ((*research-child-exec-hook*
             (lambda (in)
               (declare (ignore in))
               (incf exec)))
            (*research-child-hook* nil))
        (let ((result (%run-research :journal journal :task-id task-id
                                     :llm llm :websearch web)))
          (ok (= 3 exec) "finished children replay, only the rest execute")
          (ok (= 3 (%count-events journal 'task:child-spawned task-id))
              "child-spawned count is 3, not re-appended")
          (ok (>= (%count-events journal 'task:task-completed)
                  completed-after-kill)
              "replay does not drop finished-child completions")
          (ok (member (getf result :verdict) '(:pass :fail)))
          (dolist (q *research-questions*)
            (ok (search (format nil "ANSWER:~a" q) (getf result :markdown))
                (format nil "resumed report cites ~a" q))))))))

(deftest project-milestone-wait-input-resume
  (let* ((journal (task:make-in-memory-journal))
         (task-id "project-ms")
         (domain (%research-domain :name "project-demo"))
         (spec (make-project-spec
                :name "compliance"
                :milestones '("review")
                :schedule (lambda (last) (+ last 86400))))
         (waited nil))
    (handler-case
        (start-project domain spec :journal journal :task-id task-id)
      (approval-required (c)
        (setf waited t)
        (ok (approval-required-milestone c))))
    (ok waited "first run waits at the milestone")
    (ok (plusp (%count-events journal 'task:wait-input task-id))
        "wait-input is journaled")
    (ok (plusp (%count-events journal 'task:step-completed task-id))
        "milestone-reached checkpoint is journaled")
    (let ((wf (handler-bind ((approval-required
                              (lambda (c)
                                (invoke-approve c))))
                (start-project domain spec :journal journal :task-id task-id))))
      (ok (project-workflow-p wf))
      (ok (eq :completed (project-workflow-status wf)))
      (ok (eq :completed (task:durable-task-status
                          (project-workflow-task wf))))
      (ok (find-if (lambda (e)
                     (and (typep e 'task:timer-set)
                          (task:timer-recurring-p e)))
                   (task:journal-events journal
                                        (project-workflow-task wf)))
          "schedule-recurring armed a recurring timer"))))

(deftest deep-research-budget-exhaustion-incomplete
  (let ((result (%run-research
                 :task-id "research-budget"
                 :budget (llm:make-llm-budget :max-tokens 0))))
    (ok (eq :incomplete (getf result :verdict)))
    (ok (stringp (getf result :markdown)))
    (ok (search "incomplete" (string-downcase (getf result :markdown))))
    (ok (search "Budget scope:" (getf result :markdown))
        "partial report still surfaces the budget footer")))

(deftest deep-research-workspace-rag-and-mcp
  "Fetched pages land on the board, RAG retrieve, and MCP resources."
  (let* ((board (bb:make-blackboard))
         (result (%run-research :task-id "research-workspace"
                                :blackboard board))
         (ws (getf result :workspace))
         (sources (bb:read-section board :sources :default nil)))
    (ok (research-workspace-p ws))
    (ok (bb:section-bound-p board :sources))
    (ok (bb:section-bound-p board :source-index))
    (ok (bb:section-bound-p board :research-instructions))
    (ok (>= (length sources) 3) "one fetched page per child")
    (ok (every (lambda (s)
                 (and (getf s :id) (getf s :uri)
                      (plusp (or (getf s :chars) 0))))
               sources))
    (ok (search "planning KS" (research-instruction ws :plan)))
    (ok (search "research child" (research-instruction ws :child)))
    (ok (search "gap-analysis" (research-instruction ws :gap)))
    (ok (search "synthesis KS" (research-instruction ws :synthesize)))
    (let ((hits (retrieve-research-sources ws "KSAR" :top-k 2)))
      (ok (plusp (length hits)))
      (ok (getf (first hits) :id)))
    (ok (research-workspace-mcp ws))
    (let* ((listed (list-research-resources ws))
           (uris (mapcar #'mcp:mcp-resource-uri listed)))
      (ok (find "research://catalog" uris :test #'equal))
      (ok (find "research://instructions/plan" uris :test #'equal))
      (ok (find "research://instructions/expert" uris :test #'equal))
      (ok (find-if (lambda (u) (eql (search "research://source/" u) 0)) uris)))
    (let* ((first (first sources))
           (uri (getf first :resource-uri))
           (body (%mcp-resource-text-for-test (read-research-resource ws uri))))
      (ok (search "ANSWER:" body)))
    (dolist (child (getf result :children))
      (ok (< (length (getf child :answer)) 400)
          "child answer is a summary, not a page dump"))))

(defun %mcp-resource-text-for-test (contents)
  (cond
    ((stringp contents) contents)
    ((hash-table-p contents)
     (let ((vec (gethash "contents" contents)))
       (if (and vec (plusp (length vec)))
           (gethash "text" (elt vec 0))
           "")))
    (t (princ-to-string contents))))

(deftest research-instructions-seed-local-workspace
  (ok (search "workspace://" (research-instruction nil :plan)))
  (ok (search "workspace://" (research-instruction nil :child)))
  (ok (search "workspace://" (research-instruction nil :gap)))
  (ok (search "workspace://" (research-instruction nil :expert))))

(deftest deep-research-instruction-override
  (let* ((custom "You are a test planning KS override. Decompose this question.")
         (ws (make-research-workspace
              :name "override"
              :instructions (list :plan custom))))
    (ok (equal custom (research-instruction ws :plan)))
    (ok (search "research child" (research-instruction ws :child)))))

(deftest deep-research-expert-instructions-from-domain
  (let* ((domain (make-cl-dev-expert :backend (mock-llm) :name "ws-expert"
                                     :ingest nil))
         (board (bb:make-blackboard))
         (result (run-deep-research domain "CL expert systems"
                                    :max-rounds 1
                                    :llm (%research-llm)
                                    :websearch (%research-websearch)
                                    :journal (task:make-in-memory-journal)
                                    :task-id "research-expert"
                                    :blackboard board))
         (ws (getf result :workspace)))
    (ok (search "Common Lisp" (research-instruction ws :expert)))
    (ok (search "Common Lisp"
                (getf (bb:read-section board :research-instructions) :expert)))))

(deftest research-tree-jail-rejects-dotdot
  "pathlib:under + relative-to-p: lexical .. and absolute paths stay outside."
  (with-tmp-dir (root)
    (%write-tree-file root "ok.md" "inside")
    (ok (search "inside" (read-research-tree-file root "ok.md")))
    (ok (signals (read-research-tree-file root "../ok.md") 'research-error))
    (ok (signals (read-research-tree-file root "/etc/passwd") 'research-error))
    (ok (signals (read-research-tree-file root "foo/../../etc/passwd")
                 'research-error))))

(deftest research-workspace-mcp-over-tree
  "workspace:// is jailed, listed, readable, and ingested onto the board."
  (with-tmp-dir (root)
    (%write-tree-file root "src/ksar.lisp"
                      "(defun ksar () \"KSAR: Knowledge-Source Activation Record\")")
    (%write-tree-file root "docs/blackboard.md"
                      "The blackboard is shared working memory for KSAR control.")
    (%write-tree-file root "secret.bin" "not-listed")
    (let* ((ws (make-research-workspace :name "tree-mcp" :tree-root root))
           (files (list-research-tree-files root)))
      (ok (research-workspace-tree-root ws))
      (ok (find "src/ksar.lisp" files :test #'equal))
      (ok (find "docs/blackboard.md" files :test #'equal))
      (ng (find "secret.bin" files :test #'equal))
      (ok (signals (read-research-resource ws "workspace://../etc/passwd")
                   'research-error))
      (ok (search "KSAR" (read-research-tree-file root "src/ksar.lisp")))
      (let* ((listed (list-research-resources ws))
             (uris (mapcar (lambda (r)
                             (or (ignore-errors (mcp:mcp-resource-uri r))
                                 (and (consp r) (getf r :uri))))
                           listed)))
        (ok (find "workspace://" uris :test #'equal))
        (ok (find (workspace-resource-uri "src/ksar.lisp") uris :test #'equal)))
      (ok (search "KSAR"
                  (%mcp-resource-text-for-test
                   (read-research-resource ws "workspace://src/ksar.lisp"))))
      (let ((hits (ingest-workspace-hits ws "KSAR blackboard" :top-k 2)))
        (ok (plusp (length hits)))
        (ok (every (lambda (s) (eq (getf s :kind) :workspace)) hits))
        (ok (every (lambda (s) (workspace-resource-uri-p (getf s :uri))) hits)))
      (let ((board (research-workspace-board ws)))
        (ok (find :workspace
                  (bb:read-section board :sources :default nil)
                  :key (lambda (s) (getf s :kind))))))))

(deftest deep-research-ingests-workspace-tree
  (with-tmp-dir (root)
    (%write-tree-file root "improve.md"
                      "The improve cycle uses no-critical-regression-gate.")
    (let* ((board (bb:make-blackboard))
           (result (%run-research :task-id "research-tree"
                                  :blackboard board
                                  :tree-root root
                                  :question "no-critical-regression-gate"))
           (sources (bb:read-section board :sources :default nil)))
      (ok (research-workspace-tree-root (getf result :workspace)))
      (ok (find :workspace sources :key (lambda (s) (getf s :kind)))
          "child ingest recorded a workspace:// source"))))

(deftest seed-research-workspace-ingests-seed-terms
  (with-tmp-dir (root)
    (%write-tree-file root "src/ksar.lisp"
                      "(defun ksar () \"KSAR: Knowledge-Source Activation Record\")")
    (let* ((ws (make-research-workspace :name "seed" :tree-root root))
           (hits (seed-research-workspace ws :seed "KSAR blackboard"
                                          :query "no-critical-regression-gate")))
      (ok (plusp (length hits)))
      (ok (find :workspace (research-workspace-sources ws)
                :key (lambda (s) (getf s :kind)))))))

(deftest tokenize-keeps-hyphenated-identifiers
  (let ((toks (wf::%tokenize "uses no-critical-regression-gate.")))
    (ok (member "no-critical-regression-gate" toks :test #'string=))
    (ok (member "regression" toks :test #'string=))))

(deftest workspace-local-query-p-smoke
  (ok (workspace-local-query-p "workspace://src/improve/cycle.lisp"))
  (ok (workspace-local-query-p "search workspace:// for no-critical-regression-gate"))
  (ng (workspace-local-query-p "What is KSAR in the literature?")))

(deftest search-research-tree-ranks-identifier-hits
  "Exact gate token beats a file that only mentions workspace/files."
  (with-tmp-dir (root)
    (%write-tree-file root "improve.md"
                      "The improve cycle uses no-critical-regression-gate.")
    (%write-tree-file root "readme.md"
                      "what files exist in the workspace checkout tree")
    (let ((hits (search-research-tree
                 root "workspace:// no-critical-regression-gate")))
      (ok (plusp (length hits)))
      (ok (equal "improve.md" (getf (first hits) :rel))))))

(deftest search-research-tree-ignores-prompt-boilerplate
  (with-tmp-dir (root)
    (%write-tree-file root "src/improve/cycle.lisp"
                      "(defun no-critical-regression-gate () t)")
    (%write-tree-file root "queries.md"
                      (concatenate
                       'string
                       "Search workspace:// for no-critical-regression-gate. "
                       "Quote the defining file and function; do not expand acronyms."))
    (let ((hits (search-research-tree
                 root
                 "Search workspace:// for no-critical-regression-gate. Quote the defining file.")))
      (ok (plusp (length hits)))
      (ok (equal "src/improve/cycle.lisp" (getf (first hits) :rel))))))

(deftest retrieve-research-sources-ranks-gate-identifier
  (let ((ws (make-research-workspace :name "rank")))
    (record-research-source
     ws :id "gate" :uri "workspace://improve.md" :title "improve.md"
     :text "The improve cycle uses no-critical-regression-gate."
     :kind :workspace)
    (record-research-source
     ws :id "noise" :uri "workspace://readme.md" :title "readme.md"
     :text "what files exist in the workspace checkout tree"
     :kind :workspace)
    (let ((hits (retrieve-research-sources ws "no-critical-regression-gate" :top-k 2)))
      (ok (plusp (length hits)))
      (ok (equal "gate" (getf (first hits) :id))))))

(deftest retrieve-research-sources-ignores-prompt-boilerplate
  "seed-ws prompt text must not let queries.md beat cycle.lisp / cl-stack.md."
  (let ((ws (make-research-workspace :name "rank-prompt"))
        (q (concatenate
            'string
            "Search workspace:// for cl-stack, self-reflection, "
            "no-critical-regression-gate, gap-analysis. "
            "Quote the defining file and function; do not expand acronyms.")))
    (record-research-source
     ws :id "cycle" :uri "workspace://src/improve/cycle.lisp" :title "cycle.lisp"
     :text "(defun no-critical-regression-gate () (eval:make-default-promotion-gate))"
     :kind :workspace)
    (record-research-source
     ws :id "corpus" :uri "workspace://examples/corpus/cl-stack.md" :title "cl-stack.md"
     :text "The promotion gate is no-critical-regression-gate composed with mean-improvement-gate."
     :kind :workspace)
    (record-research-source
     ws :id "queries" :uri "workspace://demos/deep-research/queries.md" :title "queries.md"
     :text q
     :kind :workspace)
    (let* ((hits (retrieve-research-sources ws q :top-k 3))
           (ids (mapcar (lambda (h) (getf h :id)) hits)))
      (ok (plusp (length hits)))
      (ok (find "cycle" ids :test #'equal))
      (ok (not (equal "queries" (getf (first hits) :id)))
          "boilerplate echo must not rank first"))))

(deftest workspace-symbol-map-extracts-gate
  (with-tmp-dir (root)
    (%write-tree-file root "src/improve/cycle.lisp"
                      (format nil "(defun default-improve-gate ()~%  (eval:make-default-promotion-gate))~%;; no-critical-regression-gate composed with mean-improvement-gate~%"))
    (let ((map (workspace-symbol-map
                :root root :focus '("src/improve/cycle.lisp"))))
      (ok (search "no-critical-regression-gate" map))
      (ok (search "default-improve-gate" map)))))

(deftest seed-research-workspace-records-symbol-map
  (with-tmp-dir (root)
    (%write-tree-file root "examples/corpus/cl-stack.md"
                      "The self-improvement promotion gate is no-critical-regression-gate.")
    (let* ((ws (make-research-workspace :name "map-seed" :tree-root root))
           (hits (seed-research-workspace
                  ws :seed "no-critical-regression-gate")))
      (ok (plusp (length hits)))
      (ok (find "workspace://.symbol-map" (research-workspace-sources ws)
                :key (lambda (s) (getf s :uri)) :test #'equal))
      (ok (search "no-critical-regression-gate"
                  (getf (find "workspace://.symbol-map"
                              (research-workspace-sources ws)
                              :key (lambda (s) (getf s :uri)) :test #'equal)
                        :text))))))

(deftest research-one-subquestion-skips-workspace-websearch
  (with-tmp-dir (root)
    (%write-tree-file root "improve.md"
                      "The improve cycle uses no-critical-regression-gate.")
    (let* ((ws (make-research-workspace :name "skip-web" :tree-root root))
           (out (wf::research-one-subquestion
                 (list :id "s1"
                       :question "workspace:// no-critical-regression-gate")
                 :llm (%research-llm)
                 :websearch (%research-websearch)
                 :workspace ws)))
      (ok (null (getf out :web-hits)))
      (ok (find :workspace (research-workspace-sources ws)
                :key (lambda (s) (getf s :kind)))))))

(deftest ensure-research-tree-index-reuses-mtime
  (with-tmp-dir (root)
    (%write-tree-file root "a.md" "alpha KSAR")
    (let ((ws (make-research-workspace :name "idx" :tree-root root)))
      (ensure-research-tree-index ws)
      (let ((first (research-workspace-tree-index ws)))
        (ok (plusp (hash-table-count first)))
        (ensure-research-tree-index ws)
        (ok (eq (gethash "a.md" first)
                (gethash "a.md" (research-workspace-tree-index ws))))))))

(deftest ensure-research-plan-seed-subquestion-injects-gate
  (let* ((plan (make-research-plan
                :question "self-reflection"
                :subquestions (list (make-research-subquestion
                                     :id "s1"
                                     :question "What file implements self-reflection in workspace://?"))))
         (out (ensure-research-plan-seed-subquestion
               plan
               :question "improve cycle with no-critical-regression-gate"
               :seed "KSAR no-critical-regression-gate gap-analysis"))
         (first (first (research-plan-subquestions out))))
    (ok (search "no-critical-regression-gate"
                (research-subquestion-question first)))
    (ok (search "gap-analysis" (research-subquestion-question first)))
    (ok (workspace-local-query-p (research-subquestion-question first)))
    (ok (equal "s1" (research-subquestion-id
                     (second (research-plan-subquestions out)))))))

(deftest ensure-research-plan-seed-subquestion-skips-when-covered
  (let* ((q "Search workspace:// for no-critical-regression-gate. Quote the file.")
         (plan (make-research-plan
                :question "gate"
                :subquestions (list (make-research-subquestion :id "s1" :question q))))
         (out (ensure-research-plan-seed-subquestion
               plan :question "no-critical-regression-gate")))
    (ok (= 1 (length (research-plan-subquestions out))))
    (ok (equal "s1" (research-subquestion-id
                     (first (research-plan-subquestions out)))))))

(deftest deep-research-forces-seed-workspace-subquestion
  (with-tmp-dir (root)
    (%write-tree-file root "improve.md"
                      "The improve cycle uses no-critical-regression-gate.")
    (let* ((board (bb:make-blackboard))
           (result (%run-research :task-id "research-seed-q"
                                  :blackboard board
                                  :tree-root root
                                  :question "no-critical-regression-gate"))
           (qs (mapcar (lambda (c) (getf c :question))
                       (getf result :children)))
           (sources (bb:read-section board :sources :default nil)))
      (ok (find-if (lambda (q)
                     (and (workspace-local-query-p q)
                          (search "no-critical-regression-gate" q
                                  :test #'char-equal)))
                   qs)
          "plan includes a forced workspace:// identifier lookup")
      (ok (find :workspace sources :key (lambda (s) (getf s :kind))))
      (ok (find-if (lambda (s)
                     (search "improve.md" (or (getf s :uri) "")))
                   sources)
          "identifier lookup ingested the gate file"))))
(deftest deep-research-gap-respawns-second-child
  "A gap subquestion actually executes a second child (bounded by max-rounds)."
  (let* ((exec '())
         (gap-q "What is a restart?")
         (*research-child-exec-hook*
          (lambda (in)
            (push (or (getf in :question) (getf in :id)) exec))))
    (let ((result (%run-research
                   :task-id "research-gap"
                   :max-rounds 2
                   :llm (%research-llm :questions '("What is KSAR?")
                                       :gap-once gap-q))))
      (ok (= 2 (length exec)) "plan child + gap child both executed")
      (ok (find "What is KSAR?" exec :test #'equal))
      (ok (find gap-q exec :test #'equal))
      (ok (= 2 (length (getf result :children))))
      (ok (eq :pass (getf result :verdict))))))

(deftest deep-research-citations-not-search-dependent
  "Delivered markdown lists block-ids and URLs even when the LLM omits them."
  (let* ((result (%run-research
                  :task-id "research-cites"
                  :llm (%research-llm
                        :questions '("What is KSAR?")
                        :synthesis-text "Briefing with no inline cites."
                        :include-answers-in-synthesis nil)))
         (md (getf result :markdown))
         (cites (collect-research-citations (getf result :children)
                                            :workspace (getf result :workspace))))
    (ok (plusp (length cites)))
    (ok (find :block-id cites :key (lambda (c) (getf c :kind))))
    (ok (find :link cites :key (lambda (c) (getf c :kind))))
    (ok (search "https://ex.test/" md))
    (ok (search "[" md) "block-id / source id appears in Sources")
    (ok (search "Sources" md))
    (ok (not (find-if (lambda (c)
                        (search "https://ex.test/" (or (getf c :answer) "")))
                      (getf result :children)))
        "child answers need not echo the URL — the Sources section does")))

(deftest deep-research-quality-gate-fails-when-synthesis-omits
  "Scripted LLM that omits expected child text must not report :pass."
  (let ((result (%run-research
                 :task-id "research-gate-fail"
                 :llm (%research-llm
                       :questions '("What is KSAR?")
                       :include-answers-in-synthesis nil
                       :synthesis-text "I omit the expected findings."))))
    (ok (eq :fail (getf result :verdict)))
    (ok (not (eq :pass (getf result :verdict))))
    (ok (stringp (getf result :markdown)))
    (ok (search "ANSWER:What is KSAR?" (getf result :markdown))
        "assembled child sections still land in the report")))

(defun %ensure-browser-fallback-stub (dom)
  "Real browser-protocol mock when loadable; otherwise a NAVIGATE/DOM-SNAPSHOT stub."
  (or (ignore-errors
        (asdf:load-system "browser-protocol" :verbose nil)
        (let ((fn (find-symbol "MAKE-MOCK-BROWSER-BACKEND" :browser-protocol)))
          (and fn (fboundp fn) (funcall fn :dom dom))))
      (let* ((pkg (or (find-package '#:browser-protocol)
                      (make-package '#:browser-protocol :use 'nil)))
             (nav (intern "NAVIGATE" pkg))
             (snap (intern "DOM-SNAPSHOT" pkg)))
        (setf (symbol-function nav)
              (lambda (browser url)
                (declare (ignore browser))
                url))
        (setf (symbol-function snap)
              (lambda (browser)
                (declare (ignore browser))
                dom))
        (export (list nav snap) pkg)
        :stub-browser)))

(deftest deep-research-browser-fetch-page-fallback
  "JS-heavy sources fall back to browser navigate + dom-snapshot when fetch-page fails."
  (let* ((dom "BROWSER-DOM-FALLBACK body for JS-heavy source")
         (browser (%ensure-browser-fallback-stub dom))
         (web (web:make-mock-websearch-backend
               :pages nil
               :handler
               (lambda (backend query &key &allow-other-keys)
                 (declare (ignore backend query))
                 (list (web:make-search-hit
                        :url "https://ex.test/js-heavy"
                        :title "JS-heavy"
                        :snippet "snippet only"
                        :rank 1
                        :source "mock")))))
         (result (%run-research
                  :task-id "research-browser"
                  :llm (%research-llm :questions '("What is KSAR?"))
                  :websearch web
                  :browser browser))
         (ws (getf result :workspace))
         (sources (and (research-workspace-p ws)
                       (research-workspace-sources ws))))
    (ok (find-if (lambda (s)
                   (search "BROWSER-DOM-FALLBACK" (or (getf s :text) "")))
                 sources)
        "browser DOM text is ingested when fetch-page fails")
    (ok (find-if (lambda (s) (eq :fetch (getf s :kind))) sources)
        "ingested page is recorded as a fetch, not a snippet")))

(deftest deep-research-hitl-between-rounds-approve
  "HITL checkpoint between rounds continues after invoke-approve."
  (let* ((exec 0)
         (*research-child-exec-hook*
          (lambda (in)
            (declare (ignore in))
            (incf exec)))
         (result
          (handler-bind ((approval-required
                          (lambda (c)
                            (invoke-approve c))))
            (%run-research
             :task-id "research-hitl-ok"
             :max-rounds 2
             :require-hitl t
             :llm (%research-llm :questions '("What is KSAR?")
                                 :gap-once "What is a restart?")))))
    (ok (eq :pass (getf result :verdict)))
    (ok (= 2 exec) "second child ran after approval")
    (ok (= 2 (length (getf result :children))))))

(deftest deep-research-hitl-between-rounds-signals
  "Without approve, HITL between rounds signals approval-required."
  (ok (signals
       (%run-research
        :task-id "research-hitl-wait"
        :max-rounds 2
        :require-hitl t
        :llm (%research-llm :questions '("What is KSAR?")
                            :gap-once "What is a restart?"))
       'approval-required)))

(deftest deep-research-hitl-honors-profile-flag
  "PROFILE-REQUIRE-HITL-P is enough to arm the between-round checkpoint."
  (let ((domain (make-expert-domain
                 :name "hitl-research"
                 :catalogue (cap:make-catalogue :world)
                 :profile (make-instance 'personal-profile
                                         :require-hitl-p t))))
    (ok (signals
         (%run-research
          :domain domain
          :task-id "research-hitl-profile"
          :max-rounds 2
          :llm (%research-llm :questions '("What is KSAR?")
                              :gap-once "What is a restart?"))
         'approval-required))))
