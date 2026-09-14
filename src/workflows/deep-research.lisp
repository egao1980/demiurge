(in-package #:demiurge/workflows)

(schema:defschema research-subquestion ()
  "One planned sub-question for deep research."
  (id string :optional t :default "" :accessor research-subquestion-id)
  (question string :optional t :default "" :accessor research-subquestion-question)
  (rationale string :optional t :default "" :accessor research-subquestion-rationale)
  (:key-style :kebab)
  (:extra :allow))

(schema:defschema research-plan ()
  "Schema-typed plan produced by the plan KS."
  (question string :optional t :default "" :accessor research-plan-question)
  (subquestions (list research-subquestion) :optional t :default nil
                :accessor research-plan-subquestions)
  (:key-style :kebab)
  (:extra :allow))

(defun research-subquestion-p (x)
  (typep x 'research-subquestion))

(defun research-plan-p (x)
  (typep x 'research-plan))

(defun make-research-subquestion (&key id question rationale)
  (make-instance 'research-subquestion
                 :id (or id "")
                 :question (or question "")
                 :rationale (or rationale "")))

(defun %as-plain-list (x)
  "jzon decodes JSON arrays as vectors; MAPCAR needs a list."
  (cond
    ((null x) nil)
    ((listp x) x)
    ((and (vectorp x) (not (stringp x))) (coerce x 'list))
    (t (list x))))

(defun make-research-plan (&key question subquestions)
  (make-instance 'research-plan
                 :question (or question "")
                 :subquestions (mapcar #'coerce-subquestion
                                       (%as-plain-list subquestions))))

(defun %ht-get (table key)
  (or (gethash key table)
      (gethash (string-downcase (string key)) table)
      (gethash (intern (string-upcase (string key)) :keyword) table)))

(defun coerce-subquestion (value)
  (cond
    ((research-subquestion-p value) value)
    ((stringp value) (make-research-subquestion :question value))
    ((hash-table-p value)
     (make-research-subquestion :id (or (%ht-get value :id) "")
                                :question (or (%ht-get value :question) "")
                                :rationale (or (%ht-get value :rationale) "")))
    ((and (consp value) (keywordp (first value)))
     (make-research-subquestion :id (or (getf value :id) "")
                                :question (or (getf value :question) "")
                                :rationale (or (getf value :rationale) "")))
    (t (make-research-subquestion :question (princ-to-string value)))))

(defun coerce-research-plan (value &key question)
  (cond
    ((research-plan-p value) value)
    ((null value)
     (make-research-plan :question (or question "")
                         :subquestions (when question
                                         (list (make-research-subquestion
                                                :id "q1"
                                                :question question)))))
    ((hash-table-p value)
     (make-research-plan :question (or (%ht-get value :question) question "")
                         :subquestions (%ht-get value :subquestions)))
    ((and (consp value) (keywordp (first value)))
     (make-research-plan :question (or (getf value :question) question "")
                         :subquestions (getf value :subquestions)))
    ((stringp value)
     (make-research-plan :question value
                         :subquestions (list (make-research-subquestion
                                              :id "q1" :question value))))
    (t
     (restart-case
         (error 'research-error
                :message (format nil "cannot coerce ~s to research-plan" value))
       (use-value (plan)
         :report "Use a supplied research-plan"
         (coerce-research-plan plan :question question))))))

(defun research-plan-plist (plan)
  (let ((p (coerce-research-plan plan)))
    (list :question (or (research-plan-question p) "")
          :subquestions
          (loop for q in (research-plan-subquestions p)
                for i from 1
                collect (list :id (let ((id (research-subquestion-id q)))
                                    (if (and id (plusp (length id)))
                                        id
                                        (format nil "q~d" i)))
                              :question (or (research-subquestion-question q) "")
                              :rationale (or (research-subquestion-rationale q)
                                             ""))))))

(defvar *research-child-hook* nil
  "Optional (lambda (child input)) invoked after spawn-child-task returns.")

(defvar *research-child-exec-hook* nil
  "Optional (lambda (input)) invoked only when a child body actually executes.")

(defvar *research-phase-hook* nil
  "Optional (lambda (phase-name)) invoked at the start of a live durable phase.")

(defun %phase (name thunk)
  (when *research-phase-hook*
    (funcall *research-phase-hook* name))
  (funcall thunk))

(defun research-budget-scope (run-id)
  "A2 budget-policy scope for a deep-research run.
   When *TENANT* is bound, the scope is tenant-prefixed (C4)."
  (if (current-tenant)
      (tenant-budget-scope :research run-id)
      (list :research run-id)))

(defun wrap-research-llm (llm run-id &key budget)
  "Wrap LLM in an A2 budget-policy scoped to (:RESEARCH RUN-ID)."
  (if (null budget)
      llm
      (llm:make-llm-router-backend
       :policy (llm:make-budget-policy
                :inner (llm:make-fallback-chain-policy :candidates (list llm))
                :budget budget)
       :candidates (list llm)
       :scope (research-budget-scope run-id))))

(defun %llm-for (domain llm)
  (or llm
      (let ((prof (and (expert-domain-p domain) (expert-profile domain))))
        (when (deployment-profile-p prof)
          (let ((cat (profile-llm-catalog prof))
                (model (profile-default-model prof)))
            (when cat
              (ignore-errors (llm:resolve-backend cat model))))))
      (llm:make-mock-llm-backend)))

(defun %websearch-for (websearch)
  (or websearch web:*websearch-backend* (web:make-mock-websearch-backend)))

(defun %hit-plist (hit)
  (list :url (web:search-hit-url hit)
        :title (web:search-hit-title hit)
        :snippet (web:search-hit-snippet hit)
        :rank (web:search-hit-rank hit)
        :source (web:search-hit-source hit)))

(defun %rag-hit-plist (hit)
  (let* ((chunk (and (rag:rag-hit-p hit) (rag:rag-hit-chunk hit)))
         (id (and chunk (rag:rag-chunk-id chunk)))
         (text (and chunk (rag:rag-chunk-text chunk))))
    (list :id id
          :text (or text "")
          :score (and (rag:rag-hit-p hit) (rag:rag-hit-score hit)))))

(defun %retrieve-one (corpus query &key llm (top-k 5))
  (cond
    ((typep corpus 'rag:rag-pipeline)
     (mapcar #'%rag-hit-plist
             (rag:retrieve corpus query :top-k top-k)))
    ((typep corpus 'rag:rag-vector-store)
     (let ((pipe (ignore-errors
                   (rag:make-rag-pipeline :store corpus :embedder llm))))
       (when pipe
         (mapcar #'%rag-hit-plist
                 (ignore-errors (rag:retrieve pipe query :top-k top-k))))))
    (t nil)))

(defun %retrieve-corpus (domain query &key llm (top-k 5))
  (loop for corpus in (and (expert-domain-p domain) (expert-corpora domain))
        append (or (ignore-errors
                     (%retrieve-one corpus query :llm llm :top-k top-k))
                   nil)))

(defun %browser-fetch-page (browser url)
  "Soft-dep: navigate + dom-snapshot when browser-protocol is loaded."
  (when (and browser url)
    (let* ((pkg (find-package '#:browser-protocol))
           (nav (and pkg (find-symbol "NAVIGATE" pkg)))
           (snap (and pkg (find-symbol "DOM-SNAPSHOT" pkg))))
      (when (and nav snap (fboundp nav) (fboundp snap))
        (funcall nav browser url)
        (let ((shot (funcall snap browser)))
          (if (stringp shot) shot (princ-to-string shot)))))))

(defun %fetch-source (url &key websearch browser)
  (or (ignore-errors (and websearch url (web:fetch-page websearch url)))
      (%browser-fetch-page browser url)))

(defun %format-source-block (rec &key clip-chars)
  (let ((id (getf rec :id))
        (title (or (getf rec :title) ""))
        (uri (or (getf rec :uri) ""))
        (text (clip-research-text (or (getf rec :chunk-text) (getf rec :text) "")
                                  clip-chars)))
    (format nil "[~a] ~a~%  ~a~%  ~a~%" id title uri text)))

(defun %child-user-prompt (question retrieved &key clip-chars)
  (with-output-to-string (s)
    (format s "Subquestion: ~a~%~%" question)
    (format s "Retrieved workspace sources (cite as [id]; full text at research://source/<id>):~%~%")
    (if retrieved
        (dolist (rec retrieved)
          (write-string (%format-source-block rec :clip-chars clip-chars) s)
          (terpri s))
        (format s "(no workspace sources retrieved)~%"))))

(defun %ingest-web-hits (workspace web-hits &key websearch browser subquestion)
  (loop for h in web-hits
        for n from 1
        for url = (getf h :url)
        for page = (and url (%fetch-source url :websearch websearch :browser browser))
        for text = (or page (getf h :snippet) "")
        for rec = (record-research-source
                   workspace
                   :id (format nil "~a-~d" (or subquestion "src") n)
                   :uri url
                   :title (getf h :title)
                   :text text
                   :subquestion subquestion
                   :kind (if page :fetch :snippet))
        collect rec))

(defun research-one-subquestion (input &key domain llm websearch browser workspace
                                        (top-k 5))
  "Search + fetch onto the research workspace; generate a short cited answer."
  (when *research-child-exec-hook*
    (funcall *research-child-exec-hook* input))
  (let* ((question (or (getf input :question) ""))
         (id (or (getf input :id) question))
         (ws (or workspace
                 (make-research-workspace :name id :domain domain)))
         (clip (research-workspace-clip-chars ws))
         (corpus-hits (%retrieve-corpus domain question :llm llm :top-k top-k))
         (hits (ignore-errors (web:search-web websearch question :count 5)))
         (web-hits (mapcar #'%hit-plist (or hits nil)))
         (recorded (%ingest-web-hits ws web-hits
                                     :websearch websearch
                                     :browser browser
                                     :subquestion id))
         (retrieved (retrieve-research-sources ws question :top-k top-k))
         (user (%child-user-prompt question retrieved :clip-chars clip))
         (response (generate-research-step llm :child user :workspace ws))
         (answer (string-trim '(#\Space #\Tab #\Newline #\Return)
                              (or (and response (llm:llm-response-text response)) "")))
         (citations (append
                     (loop for rec in retrieved
                           for sid = (getf rec :id)
                           when sid collect (list :kind :block-id :target sid))
                     (loop for rec in retrieved
                           for url = (getf rec :uri)
                           when url collect (list :kind :link :target url))
                     (loop for h in corpus-hits
                           for cid = (getf h :id)
                           when cid collect (list :kind :block-id :target cid)))))
    (list :id id
          :question question
          :answer (if (plusp (length answer))
                      answer
                      (format nil "No grounded answer for ~a." question))
          :citations citations
          :rag-hits (append corpus-hits
                            (mapcar (lambda (r)
                                      (list :id (getf r :id)
                                            :text (clip-research-text
                                                   (or (getf r :chunk-text)
                                                       (getf r :text) "")
                                                   clip)
                                            :score (getf r :score)))
                                    retrieved))
          :web-hits web-hits
          :source-ids (mapcar (lambda (r) (getf r :id)) recorded))))

(defun %spawn-research-child (parent input &key domain llm websearch browser
                             workspace (top-k 5))
  (let ((child (task:spawn-child-task
                parent
                (lambda (in)
                  (research-one-subquestion in
                                            :domain domain
                                            :llm llm
                                            :websearch websearch
                                            :browser browser
                                            :workspace workspace
                                            :top-k top-k))
                :input input)))
    (when *research-child-hook*
      (funcall *research-child-hook* child input))
    child))

(defun %plan-from-llm (llm question &key workspace)
  (let* ((prompt (format nil
                         "Decompose this question into a schema-typed research plan with subquestions: ~a"
                         question))
         (response (generate-research-step llm :plan prompt
                                           :output 'research-plan
                                           :workspace workspace))
         (out (or (and response (llm:llm-response-output response))
                  (and response (llm:llm-response-text response)))))
    (coerce-research-plan out :question question)))

(defun %gap-from-llm (llm question children &key workspace)
  (let* ((clip (if (research-workspace-p workspace)
                   (research-workspace-clip-chars workspace)
                   *default-research-clip-chars*))
         (blob (with-output-to-string (s)
                 (dolist (c children)
                   (format s "~a => ~a~%"
                           (getf c :question)
                           (clip-research-text (or (getf c :answer) "") clip)))))
         (prompt (format nil
                         "Gap analysis for ~a. Existing answers:~%~a~%Return new subquestions or none."
                         question blob))
         (response (generate-research-step llm :gap prompt
                                           :output 'research-plan
                                           :workspace workspace))
         (out (or (and response (llm:llm-response-output response))
                  (and response (llm:llm-response-text response)))))
    (coerce-research-plan out :question question)))

(defun %synth-user-prompt (question children workspace)
  (let ((clip (if (research-workspace-p workspace)
                  (research-workspace-clip-chars workspace)
                  *default-research-clip-chars*)))
    (with-output-to-string (s)
      (format s "Synthesize a cited report for ~a from ~d sub-answers.~%~%"
              question (length children))
      (format s "Child answers:~%")
      (dolist (c children)
        (format s "~%### ~a (~a)~%~a~%"
                (or (getf c :question) "")
                (or (getf c :id) "")
                (clip-research-text (or (getf c :answer) "") clip)))
      (format s "~%Source catalog:~%")
      (if (research-workspace-p workspace)
          (dolist (e (research-source-catalog workspace))
            (format s "[~a] ~a — ~a (~d chars)~%"
                    (getf e :id)
                    (or (getf e :title) "")
                    (or (getf e :uri) "")
                    (or (getf e :chars) 0)))
          (format s "(none)~%")))))

(defun %annotate-text (text citations)
  "Build :link annotation-spans for CITATION targets found in TEXT."
  (let ((anns '()))
    (dolist (cite citations)
      (let* ((target (getf cite :target))
             (kind (or (getf cite :kind) :link)))
        (when (and target (stringp target) (plusp (length target)))
          (let ((start (or (search target text :test #'char-equal)
                           (let ((ans-start (search (format nil "~a" target)
                                                    text)))
                             ans-start))))
            (when start
              (push (doc:make-annotation-span
                     :start start
                     :end (+ start (length target))
                     :kind (if (eq kind :block-id) :ref-bib :link)
                     :target target)
                    anns))))))
    (nreverse anns)))

(defun %synthesize-document (question children &key partial synthesis-text)
  (let* ((title (format nil "Research: ~a" question))
         (intro-text (or synthesis-text
                         (if partial
                             (format nil "Partial report for ~a (incomplete)." question)
                             (format nil "Cited report for ~a." question))))
         (intro (doc:make-text-block :kind :heading :text title))
         (lede (doc:make-text-block :kind :para :text intro-text))
         (sections
          (loop for child in children
                for q = (or (getf child :question) "")
                for ans = (or (getf child :answer) "")
                for cites = (getf child :citations)
                for anns = (%annotate-text ans cites)
                collect (doc:make-section-block
                         :title q
                         :level 2
                         :children (list (doc:make-text-block
                                          :kind :para
                                          :text ans
                                          :annotations anns))))))
    (let ((doc (doc:make-extracted-document
                :metadata (doc:make-document-metadata :title title)
                :blocks (cons intro (cons lede sections)))))
      (doc:ensure-ids doc)
      doc)))

(defun %render-c3e (doc format &key stream profile)
  (let ((fn (%find-sym '#:doc-extract-render "RENDER-DOCUMENT")))
    (when (and fn (fboundp fn))
      (funcall fn doc format :stream stream :profile (or profile :llm)))))

(defun %render-pdf (doc &key stream)
  (or (%funcall-if '#:doc-extract-render-pdf "RENDER-DOCUMENT"
                   doc :pdf :stream stream)
      (let ((fn (%find-sym '#:doc-extract-render "RENDER-DOCUMENT")))
        (when (and fn (fboundp fn))
          (ignore-errors (funcall fn doc :pdf :stream stream))))))

(defun render-research-document (doc &key (format :markdown) stream (profile :llm))
  "C3e render. Markdown is required (dump-markup fallback). PDF is soft-dep."
  (check-type doc doc:extracted-document)
  (ecase format
    ((:markdown :md)
     (or (%render-c3e doc :markdown :stream stream :profile profile)
         (doc:dump-markup doc :profile profile :stream stream)))
    ((:pdf)
     (or (%render-pdf doc :stream stream)
         (restart-case
             (error 'research-error
                    :message "pdf render is unavailable (doc-extract-render-pdf / browser not loaded)")
           (use-value (value)
             :report "Use a supplied PDF payload"
             value)
           (continue ()
             :report "Fall back to markdown"
             (render-research-document doc :format :markdown
                                       :stream stream :profile profile)))))))

(defun %quality-gate (question children markdown)
  (if (null children)
      (eval:make-eval-run
       :dataset (eval:make-eval-dataset :name "research-quality" :cases nil)
       :results nil)
      (let* ((cases (mapcar (lambda (c)
                              (eval:make-eval-case
                               :input question
                               :expected (or (getf c :answer) "")))
                            children))
             (dataset (eval:make-eval-dataset
                       :name "research-quality"
                       :cases cases)))
        (eval:run-eval dataset
                       (lambda (in)
                         (declare (ignore in))
                         markdown)
                       :scorers (list (eval:make-contains-scorer))))))

(defun %verdict-from-run (run)
  (if (and run (plusp (eval:eval-run-n run))
           (= (eval:eval-run-pass-count run) (eval:eval-run-n run)))
      :pass
      :fail))

(defun %result-extras (workspace)
  (list :workspace workspace
        :sources (and (research-workspace-p workspace)
                      (research-source-catalog workspace))
        :mcp-server (and (research-workspace-p workspace)
                         (research-workspace-mcp workspace))))

(defun %merge-research-workspace (workspace)
  (when (and (research-workspace-p workspace)
             (research-workspace-bb workspace))
    (ignore-errors
      (bb:merge-workspace (research-workspace-bb workspace)
                          :strategy :overwrite))))

(defun run-deep-research (domain question &key max-rounds budget
                                        llm websearch browser
                                        journal task-id blackboard
                                        workspace store instructions
                                        (top-k 5))
  "Plan → spawn-child-task per sub-question → join :all → gap rounds →
   C3d extracted-document → A1 eval gate → C3e markdown (PDF if loaded).
   Fetched pages land on a blackboard research workspace (RAG + MCP resources).
   Whole run is under an A2 budget scope."
  (check-type domain expert-domain)
  (check-type question string)
  (let* ((max-rounds (or max-rounds 2))
         (run-id (or task-id (format nil "research/~a" (expert-name domain))))
         (journal (%journal-for domain journal))
         (task (task:make-durable-task :id run-id :journal journal))
         (llm (wrap-research-llm (%llm-for domain llm) run-id :budget budget))
         (websearch (%websearch-for websearch))
         (root-board (or blackboard (bb:make-blackboard)))
         (bb-ws (ignore-errors (bb:fork-workspace root-board run-id)))
         (cow (or (and bb-ws (bb:workspace-blackboard bb-ws)) root-board))
         (workspace (or workspace
                        (make-research-workspace
                         :name run-id
                         :board cow
                         :store store
                         :instructions instructions
                         :domain domain)))
         (board (research-workspace-board workspace))
         (wf (make-project-workflow :name run-id :domain domain
                                    :board board :task task))
         (children '()))
    (when bb-ws
      (setf (research-workspace-bb workspace) bb-ws))
    (labels ((finish (plist)
               (%merge-research-workspace workspace)
               (ignore-errors (task:complete-task task plist))
               (setf (project-workflow-status wf)
                     (if (eq (getf plist :verdict) :incomplete)
                         :incomplete
                         :completed))
               plist)
             (partial (reason kids)
               (let* ((doc (%synthesize-document question kids :partial t))
                      (md (render-research-document doc :format :markdown)))
                 (report-workflow-progress
                  wf :board board :status :failed
                  :summary (format nil "incomplete: ~a" reason))
                 (finish (append (list :verdict :incomplete
                                       :question question
                                       :children kids
                                       :markdown md
                                       :document-text md
                                       :reason reason)
                                 (%result-extras workspace)))))
             (run-rounds (pending)
               (loop for round from 1 to max-rounds
                     while pending
                     do (dolist (q pending)
                          (%spawn-research-child
                           task q
                           :domain domain :llm llm
                           :websearch websearch :browser browser
                           :workspace workspace
                           :top-k top-k))
                        (let ((joined
                               (task:with-durable-step
                                   ((format nil "join-~d" round)
                                    :idempotency-key
                                    (format nil "research/join/~d" round))
                                 (%phase (format nil "join-~d" round)
                                         (lambda ()
                                           (task:join-children
                                            task :policy :all))))))
                          (setf children (or joined children)))
                        (report-workflow-progress
                         wf :board board :round round :status :working
                         :summary (format nil "round ~d joined ~d"
                                          round (length children)))
                        (setf pending
                              (unless (>= round max-rounds)
                                (let ((gap-plist
                                       (task:with-durable-step
                                           ((format nil "gap-~d" round)
                                            :idempotency-key
                                            (format nil "research/gap/~d" round))
                                         (%phase (format nil "gap-~d" round)
                                                 (lambda ()
                                                   (research-plan-plist
                                                    (%gap-from-llm
                                                     llm question children
                                                     :workspace workspace)))))))
                                  (remove-if
                                   (lambda (q)
                                     (or (null (getf q :question))
                                         (zerop (length (getf q :question)))))
                                   (getf gap-plist :subquestions)))))))
             (deliver ()
               (let* ((synth-plist
                       (task:with-durable-step
                           ("synthesize" :idempotency-key "research/synthesize")
                         (%phase "synthesize"
                                 (lambda ()
                                   (let* ((prompt (%synth-user-prompt
                                                   question children workspace))
                                          (resp (generate-research-step
                                                 llm :synthesize prompt
                                                 :workspace workspace))
                                          (text (or (and resp
                                                         (llm:llm-response-text resp))
                                                    "")))
                                     (list :text text))))))
                      (doc (%synthesize-document
                            question children
                            :synthesis-text (getf synth-plist :text)))
                      (md (task:with-durable-step
                              ("render" :idempotency-key "research/render")
                            (%phase "render"
                                    (lambda ()
                                      (render-research-document
                                       doc :format :markdown)))))
                      (run (task:with-durable-step
                               ("gate" :idempotency-key "research/gate")
                             (%phase "gate"
                                     (lambda ()
                                       (let ((ev (%quality-gate
                                                  question children md)))
                                         (list :mean (eval:eval-run-mean ev)
                                               :n (eval:eval-run-n ev)
                                               :pass-count
                                               (eval:eval-run-pass-count ev)
                                               :verdict (%verdict-from-run ev)))))))
                      (verdict (or (getf run :verdict) :pass)))
                 (report-workflow-progress
                  wf :board board :status :completed
                  :summary (format nil "delivered ~a" verdict))
                 (finish (append (list :verdict verdict
                                       :question question
                                       :children children
                                       :markdown md
                                       :document-text md
                                       :eval-mean (getf run :mean)
                                       :eval-n (getf run :n))
                                 (%result-extras workspace))))))
      (task:with-durable-task (task journal)
        (when (eq (task:durable-task-status task) :completed)
          (return-from run-deep-research
            (or (task:durable-task-result task)
                (list :verdict :pass :question question))))
        (restart-case
            (handler-bind ((llm:llm-budget-exceeded
                            (lambda (c)
                              (declare (ignore c))
                              (invoke-restart 'use-partial))))
              (let ((plan-plist
                     (task:with-durable-step
                         ("plan" :idempotency-key "research/plan")
                       (%phase "plan"
                               (lambda ()
                                 (research-plan-plist
                                  (%plan-from-llm llm question
                                                  :workspace workspace)))))))
                (report-workflow-progress
                 wf :board board :round 0 :status :working
                 :summary (format nil "plan ~a subquestions"
                                  (length (getf plan-plist :subquestions))))
                (run-rounds (copy-list (getf plan-plist :subquestions)))
                (deliver)))
          (use-partial ()
            :report "Deliver a graceful partial report"
            (partial :budget-exceeded children)))))))
