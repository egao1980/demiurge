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
    (let* ((result (%run-research
                    :task-id "research-browser"
                    :llm (%research-llm :questions '("What is KSAR?"))
                    :websearch web
                    :browser browser))
           (ws (getf result :workspace))
           (sources (and (research-workspace-p ws)
                         (research-workspace-sources ws))))
      (ok (find-if (lambda (s) (search "BROWSER-DOM-FALLBACK" (or (getf s :text) "")))
                   sources)
          "browser DOM text is ingested when fetch-page fails")
      (ok (find-if (lambda (s) (eq :fetch (getf s :kind))) sources)
          "ingested page is recorded as a fetch, not a snippet"))))

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
