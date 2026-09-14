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

(defun %research-llm (&key (questions *research-questions*))
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
                     (string-trim '(#\Space #\Tab #\Return) (subseq text start end)))))
            (q (or (find sub questions :test #'string-equal)
                   (find-if (lambda (q) (search q text)) questions))))
       (cond
         ((search "Gap analysis" text)
          (llm:make-llm-response
           :parts (list (llm:make-llm-text-part :text "none"))
           :output (make-research-plan :question "q" :subquestions nil)))
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
                         :text "Cited briefing over KSAR, the blackboard, and the journal.")))))))))

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
                        (max-rounds 1) (question "CL expert systems"))
  (run-deep-research (%research-domain)
                     question
                     :max-rounds max-rounds
                     :budget budget
                     :llm (or llm (%research-llm))
                     :websearch (or websearch (%research-websearch))
                     :journal (or journal (task:make-in-memory-journal))
                     :task-id (or task-id "research-e2e")
                     :blackboard blackboard
                     :tree-root tree-root))

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

(deftest generate-research-step-retries-then-dies-with-completion
  "Invalid JSON retries, then RESEARCH-ERROR includes the raw completion."
  (let* ((*research-output-attempts* 2)
         (n 0)
         (bare (llm:make-mock-llm-backend
                :handler (lambda (backend turns &key &allow-other-keys)
                           (declare (ignore backend turns))
                           (incf n)
                           (llm:make-llm-response
                            :parts (list (llm:make-llm-text-part :text "not-json"))))))
         (llm (wrap-llm-observe bare :expert "t" :scope "t")))
    (handler-case
        (progn
          (generate-research-step llm :plan "CL expert systems"
                                  :output 'research-plan)
          (ok nil "expected research-error"))
      (research-error (e)
        (ok (search "not-json" (or (demiurge-error-message e) "")))
        (ok (search "2" (or (demiurge-error-message e) "")))))
    (ok (= 2 n))))

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
    (ok (member (getf result :verdict) '(:pass :fail)))
    (ok (stringp (getf result :markdown)))
    (ok (= 3 (length (getf result :children))))
    (ok (bb:section-bound-p board :round-summary))
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
    (ok (search "incomplete" (string-downcase (getf result :markdown))))))

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

(defun %write-tree-file (root rel text)
  (let ((path (merge-pathnames rel (uiop:ensure-directory-pathname root))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string text out))
    path))

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
