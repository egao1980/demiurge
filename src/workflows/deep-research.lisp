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

(defun %named-identifier-terms (&rest texts)
  "Hyphen/`_`/`/` identifiers from TEXTS. Empty / NIL texts are ignored."
  (remove-duplicates
   (loop for text in texts
         nconc (remove-if-not #'%identifier-token-p
                              (%tokenize (or text ""))))
   :test #'string=))

(defun %subquestion-covers-terms-p (subq terms)
  (let ((q (cond
             ((research-subquestion-p subq) (research-subquestion-question subq))
             ((and (consp subq) (keywordp (first subq))) (getf subq :question))
             (t ""))))
    (and (workspace-local-query-p q)
         (plusp (length terms))
         (every (lambda (tok) (search tok q :test #'char-equal)) terms))))

(defun make-workspace-seed-subquestion (&key question seed (id "seed-ws"))
  "One workspace:// lookup that names every identifier in QUESTION and SEED."
  (let ((terms (%named-identifier-terms question seed)))
    (when terms
      (make-research-subquestion
       :id id
       :question (format nil
                         "Search workspace:// for ~{~a~^, ~}. Quote the defining file and function; do not expand acronyms."
                         terms)
       :rationale "Forced local lookup of named identifiers."))))

(defun ensure-research-plan-seed-subquestion (plan &key question seed)
  "Prepend a workspace:// identifier search unless the plan already has one."
  (let* ((plan (coerce-research-plan plan :question question))
         (q (or question (research-plan-question plan) ""))
         (terms (%named-identifier-terms q seed))
         (required (%named-identifier-terms q)))
    (cond
      ((null terms) plan)
      ((find-if (lambda (sq)
                  (%subquestion-covers-terms-p sq (or required terms)))
                (research-plan-subquestions plan))
       plan)
      (t
       (let ((seed-q (make-workspace-seed-subquestion :question q :seed seed)))
         (when seed-q
           (research-trace "plan seed-subquestion ~s"
                           (research-subquestion-question seed-q))
           (setf (research-plan-subquestions plan)
                 (cons seed-q (research-plan-subquestions plan))))
         plan)))))

(defvar *research-child-hook* nil
  "Optional (lambda (child input)) invoked after spawn-child-task returns.")

(defvar *research-child-exec-hook* nil
  "Optional (lambda (input)) invoked only when a child body actually executes.")

(defvar *research-phase-hook* nil
  "Optional (lambda (phase-name)) invoked at the start of a live durable phase.")

(defun %phase (name thunk)
  (research-trace "phase ~a" name)
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

(defun %research-mock-turns-text (turns)
  (cond
    ((stringp turns) turns)
    ((listp turns)
     (with-output-to-string (s)
       (dolist (tn turns)
         (write-string (if (stringp tn) tn (or (llm:turn-text tn) "")) s))))
    (t (princ-to-string turns))))

(defun %research-mock-topic (text)
  (flet ((after (marker)
           (let ((pos (search marker text :test #'char-equal)))
             (when pos
               (let* ((start (+ pos (length marker)))
                      (end (or (position-if (lambda (c)
                                              (member c '(#\. #\Newline #\Return)))
                                            text :start start)
                               (length text))))
                 (string-trim '(#\Space #\Tab) (subseq text start end)))))))
    (or (after "subquestions: ")
        (after "Gap analysis for ")
        (after "cited report for ")
        "")))

(defun %research-mock-subquestion-line (text)
  (let ((pos (search "Subquestion: " text)))
    (when pos
      (let* ((start (+ pos (length "Subquestion: ")))
             (end (or (position #\Newline text :start start)
                      (length text))))
        (string-trim '(#\Space #\Tab #\Return) (subseq text start end))))))

(defun make-research-mock-llm (&key questions
                                    gap-once
                                    (include-answers-in-synthesis t)
                                    synthesis-text)
  "Mock LLM that satisfies :OUTPUT RESEARCH-PLAN (demo + %LLM-FOR).
   Decompose → plan with subquestions; gap analysis → empty or one gap;
   child → short ANSWER text; else synthesis. QUESTIONS defaults to the
   topic extracted from the decompose prompt."
  (let ((gap-remaining (if gap-once 1 0))
        (gap-q (if (stringp gap-once) gap-once "What is a restart?")))
    (llm:make-mock-llm-backend
     :handler
     (lambda (backend turns &key &allow-other-keys)
       (declare (ignore backend))
       (let* ((text (%research-mock-turns-text turns))
              (qs (or questions
                      (let ((topic (%research-mock-topic text)))
                        (if (plusp (length topic))
                            (list topic)
                            '("What is the core question?")))))
              (all-qs (if (and gap-once (stringp gap-once)
                               (not (member gap-once qs :test #'string-equal)))
                          (append qs (list gap-once))
                          qs))
              (sub (%research-mock-subquestion-line text))
              (q (or (and sub (find sub all-qs :test #'string-equal))
                     (find-if (lambda (item) (search item text)) all-qs))))
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
                                   :id "gap-1" :question gap-q)))))
                (llm:make-llm-response
                 :parts (list (llm:make-llm-text-part :text "none"))
                 :output (make-research-plan :question "q"
                                            :subquestions nil))))
           ((search "Decompose" text)
            (llm:make-llm-response
             :parts (list (llm:make-llm-text-part :text "plan"))
             :output (make-research-plan
                      :question (or (first qs) "q")
                      :subquestions
                      (loop for item in qs
                            for i from 1
                            collect (make-research-subquestion
                                     :id (format nil "q~d" i)
                                     :question item)))))
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
                                                 (mapcar (lambda (item)
                                                           (format nil "ANSWER:~a" item))
                                                         all-qs))
                                         "I omit the expected findings."))))))))))))

(defun %bare-mock-llm-p (llm)
  (let ((bare (or (ignore-errors (bare-llm-backend llm)) llm)))
    (and (typep bare 'llm:mock-llm-backend)
         (null (llm:mock-llm-handler bare)))))

(defun %llm-for (domain llm)
  "Profile LLM, or MAKE-RESEARCH-MOCK-LLM. A bare mock (no handler) cannot
   satisfy :OUTPUT RESEARCH-PLAN — replace it so demo / cmd-research work."
  (let ((resolved (resolve-profile-llm
                   (and (expert-domain-p domain) (expert-profile domain))
                   llm)))
    (if (or (null resolved) (%bare-mock-llm-p resolved))
        (make-research-mock-llm)
        resolved)))

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
         (corpus-hits (%retrieve-corpus domain question :llm llm :top-k top-k)))
    (research-trace "child ~a ~s" id question)
    (research-trace "workspace ingest ~a" id)
    (let* ((workspace-hits (ignore-errors
                             (ingest-workspace-hits ws question
                                                    :top-k top-k
                                                    :subquestion id)))
           (hits (progn
                   (research-trace "workspace ingest ~a hits=~d"
                                   id (length (or workspace-hits '())))
                   (if (workspace-local-query-p question)
                       (progn
                         (research-trace "websearch skip ~s (workspace-local)"
                                         question)
                         nil)
                       (progn
                         (research-trace "websearch ~s" question)
                         (ignore-errors
                           (web:search-web websearch question :count 5))))))
           (web-hits (mapcar #'%hit-plist (or hits nil)))
           (recorded (progn
                       (research-trace "websearch ~s hits=~d"
                                       question (length web-hits))
                       (append (or workspace-hits '())
                               (%ingest-web-hits ws web-hits
                                                 :websearch websearch
                                                 :browser browser
                                                 :subquestion id))))
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
            :source-ids (mapcar (lambda (r) (getf r :id)) recorded)))))

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

(defun %plan-from-llm (llm question &key workspace seed)
  (let* ((prompt (format nil
                         "Decompose this question into a schema-typed research plan with subquestions: ~a"
                         question))
         (response (generate-research-step llm :plan prompt
                                           :output 'research-plan
                                           :workspace workspace))
         (out (or (and response (llm:llm-response-output response))
                  (and response (llm:llm-response-text response)))))
    (ensure-research-plan-seed-subquestion
     (coerce-research-plan out :question question)
     :question question
     :seed seed)))

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
  "Build annotation-spans for CITATION targets found in TEXT.
   Inline hits are optional; the Sources section always lists every citation."
  (let ((anns '()))
    (dolist (cite citations)
      (let* ((target (getf cite :target))
             (kind (or (getf cite :kind) :link)))
        (when (and target (stringp target) (plusp (length target)))
          (let ((start (or (search target text :test #'char-equal)
                           (search (format nil "~a" target) text))))
            (when start
              (push (doc:make-annotation-span
                     :start start
                     :end (+ start (length target))
                     :kind (if (eq kind :block-id) :ref-bib :link)
                     :target target)
                    anns))))))
    (nreverse anns)))

(defun collect-research-citations (children &key workspace)
  "Unique citation plists (:kind :block-id|:link :target) from children + catalog.
   Always includes both workspace/corpus block-ids and source URLs."
  (let ((seen (make-hash-table :test 'equal))
        (out '()))
    (flet ((add (kind target)
             (when (and target (or (stringp target) (symbolp target)))
               (let* ((s (if (stringp target) target (princ-to-string target)))
                      (key (cons kind s)))
                 (when (and (plusp (length s)) (not (gethash key seen)))
                   (setf (gethash key seen) t)
                   (push (list :kind kind :target s) out))))))
      (dolist (child children)
        (dolist (cite (getf child :citations))
          (add (or (getf cite :kind) :link) (getf cite :target)))
        (dolist (hit (getf child :rag-hits))
          (add :block-id (getf hit :id)))
        (dolist (hit (getf child :web-hits))
          (add :link (getf hit :url)))
        (dolist (sid (getf child :source-ids))
          (add :block-id sid)))
      (when (research-workspace-p workspace)
        (dolist (e (research-source-catalog workspace))
          (add :block-id (getf e :id))
          (add :link (getf e :uri)))))
    (nreverse out)))

(defun format-research-budget-footer (scope)
  "Plain-text footer line naming the A2 budget scope for this run."
  (format nil "Budget scope: ~s" (or scope '(:research :unknown))))

(defun %sources-section-text (citations workspace)
  "Markdown-ish sources listing. Does not depend on the LLM echoing cites."
  (let ((catalog (and (research-workspace-p workspace)
                      (research-source-catalog workspace))))
    (if (null citations)
        "(no citations recorded)"
        (with-output-to-string (s)
          (dolist (cite citations)
            (let* ((kind (getf cite :kind))
                   (target (getf cite :target))
                   (entry (find target catalog
                                :key (lambda (e) (getf e :id))
                                :test #'equal)))
              (if (eq kind :block-id)
                  (format s "[~a]~@[ ~a~]~@[ — ~a~]~%"
                          target
                          (and entry (getf entry :title))
                          (or (and entry (getf entry :uri))
                              (getf cite :uri)))
                  (format s "~a~%" target))))))))

(defun %synthesize-document (question children &key partial synthesis-text
                             workspace budget-scope)
  (let* ((title (format nil "Research: ~a" question))
         (intro-text (or synthesis-text
                         (if partial
                             (format nil "Partial report for ~a (incomplete)." question)
                             (format nil "Cited report for ~a." question))))
         (citations (collect-research-citations children :workspace workspace))
         (sources-text (%sources-section-text citations workspace))
         (footer (format-research-budget-footer budget-scope))
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
                                          :annotations anns)))))
         (sources-sec
          (doc:make-section-block
           :title "Sources"
           :level 2
           :children (list (doc:make-text-block
                            :kind :para
                            :text sources-text
                            :annotations (%annotate-text sources-text citations)))))
         (footer-sec
          (doc:make-section-block
           :title "Budget"
           :level 2
           :children (list (doc:make-text-block :kind :para :text footer)))))
    (let ((doc (doc:make-extracted-document
                :metadata (doc:make-document-metadata :title title)
                :blocks (append (list intro lede)
                                sections
                                (list sources-sec footer-sec)))))
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

(defun %gate-expected (child)
  "Short expected needle: ANSWER:<question> when present, else the answer."
  (let ((ans (or (getf child :answer) "")))
    (cond
      ((zerop (length ans)) (or (getf child :question) ""))
      ((search "ANSWER:" ans)
       (string-trim '(#\Space #\Tab)
                    (subseq ans 0 (or (position #\[ ans) (length ans)))))
      (t ans))))

(defun %quality-gate (question children scored-text)
  "A1 contains-scorer over SCORED-TEXT (the synthesis briefing, not the
   auto-assembled child sections). A scripted LLM that omits expected
   child text fails the gate."
  (if (null children)
      (eval:make-eval-run
       :dataset (eval:make-eval-dataset :name "research-quality" :cases nil)
       :results nil)
      (let* ((cases (mapcar (lambda (c)
                              (eval:make-eval-case
                               :input question
                               :expected (%gate-expected c)))
                            children))
             (dataset (eval:make-eval-dataset
                       :name "research-quality"
                       :cases cases)))
        (eval:run-eval dataset
                       (lambda (in)
                         (declare (ignore in))
                         (or scored-text ""))
                       :scorers (list (eval:make-contains-scorer))))))

(defun %verdict-from-run (run)
  (if (and run (plusp (eval:eval-run-n run))
           (= (eval:eval-run-pass-count run) (eval:eval-run-n run)))
      :pass
      :fail))

(defun %research-require-hitl-p (domain require-hitl)
  (or require-hitl
      (let ((prof (and (expert-domain-p domain) (expert-profile domain))))
        (and (deployment-profile-p prof) (profile-require-hitl-p prof)))))

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
                                        tree-root
                                        require-hitl
                                        (top-k 5))
  "Plan → spawn-child-task per sub-question → join :all → gap rounds →
   C3d extracted-document → A1 eval gate → C3e markdown (PDF if loaded).
   Fetched pages and workspace:// files land on a blackboard research
   workspace (RAG + MCP resources). Whole run is under an A2 budget scope.
   REQUIRE-HITL (or PROFILE-REQUIRE-HITL-P) checkpoints between rounds."
  (check-type domain expert-domain)
  (check-type question string)
  (let* ((max-rounds (or max-rounds 2))
         (run-id (or task-id (format nil "research/~a" (expert-name domain))))
         (hitl-p (%research-require-hitl-p domain require-hitl))
         (budget-scope (research-budget-scope run-id))
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
                         :domain domain
                         :tree-root tree-root)))
         (board (research-workspace-board workspace))
         (wf (make-project-workflow :name run-id :domain domain
                                    :board board :task task))
         (children '()))
    (when bb-ws
      (setf (research-workspace-bb workspace) bb-ws))
    (seed-research-workspace workspace
                             :query question
                             :seed (workspace-seed-from-domain domain))
    (labels ((finish (plist)
               (%merge-research-workspace workspace)
               (ignore-errors (task:complete-task task plist))
               (setf (project-workflow-status wf)
                     (if (eq (getf plist :verdict) :incomplete)
                         :incomplete
                         :completed))
               plist)
             (partial (reason kids)
               (let* ((doc (%synthesize-document
                            question kids
                            :partial t
                            :workspace workspace
                            :budget-scope budget-scope))
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
                                   (getf gap-plist :subquestions)))))
                        (when (and pending hitl-p)
                          (await-approval
                           (make-project-milestone
                            :name (format nil "research-round-~d" round)
                            :prompt (format nil "approve research round ~d" round))
                           :task task))))
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
                      (synth-text (or (getf synth-plist :text) ""))
                      (doc (%synthesize-document
                            question children
                            :synthesis-text synth-text
                            :workspace workspace
                            :budget-scope budget-scope))
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
                                                  question children
                                                  synth-text)))
                                         (list :mean (eval:eval-run-mean ev)
                                               :n (eval:eval-run-n ev)
                                               :pass-count
                                               (eval:eval-run-pass-count ev)
                                               :verdict (%verdict-from-run ev)))))))
                      (verdict (if (eq (getf run :verdict) :pass)
                                   :pass
                                   :fail)))
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
                                                  :workspace workspace
                                                  :seed (workspace-seed-from-domain domain))))))))
                (report-workflow-progress
                 wf :board board :round 0 :status :working
                 :summary (format nil "plan ~a subquestions"
                                  (length (getf plan-plist :subquestions))))
                (run-rounds (copy-list (getf plan-plist :subquestions)))
                (deliver)))
          (use-partial ()
            :report "Deliver a graceful partial report"
            (partial :budget-exceeded children)))))))
