(in-package #:demiurge/improve)

(defvar *ks-eval-history* (make-hash-table :test 'equal)
  "KS name string → list of recent eval-run means (newest first).")

(defvar *improve-phase-hook* nil
  "Optional (lambda (phase-name)) invoked at the start of a live durable phase.")

(defun record-ks-eval (ks-id mean &optional (history *ks-eval-history*))
  (let ((key (string-downcase (string ks-id))))
    (push mean (gethash key history))
    (when mean
      (demiurge::%observe-record "RECORD-EVAL-SCORE" ks-id mean))
    (gethash key history)))

(defun %rolling-mean (scores window)
  (let* ((kept (subseq scores 0 (min window (length scores))))
         (n (length kept)))
    (if (zerop n)
        0
        (/ (reduce #'+ kept) n))))

(defun select-improvement-target (domain &key (activity-floor 1) (window 5)
                                           (history *ks-eval-history*)
                                           target)
  "Worst rolling aggregate over HISTORY. Skip KS below ACTIVITY-FLOOR.
   TARGET (KS, name, or NIL) short-circuits selection."
  (when target
    (return-from select-improvement-target
      (cond
        ((typep target 'bb:knowledge-source) target)
        ((or (stringp target) (symbolp target))
         (find (string-downcase (string target))
               (expert-ks-set domain)
               :key (lambda (ks) (string-downcase (string (bb:ks-name ks))))
               :test #'equal))
        (t target))))
  (let ((best nil)
        (best-score nil))
    (dolist (ks (expert-ks-set domain))
      (let* ((id (string-downcase (string (bb:ks-name ks))))
             (scores (gethash id history)))
        (when (>= (length scores) activity-floor)
          (let ((agg (%rolling-mean scores window)))
            (when (or (null best) (< agg best-score))
              (setf best ks best-score agg))))))
    best))

(defun %respond (variant input)
  "Run VARIANT on INPUT → actual output."
  (cond
    ((functionp variant)
     (funcall variant input))
    ((revised-ks-p variant)
     (let* ((rev (revised-ks-revision variant))
            (skill (and rev (ks-revision-skill-text rev))))
       (if (and skill (plusp (length skill)))
           (if (stringp input)
               (concatenate 'string skill input)
               skill)
           (%respond (revised-ks-base variant) input))))
    ((agent-ks-p variant)
     (let ((bb (bb:make-blackboard)))
       (bb:write-section bb (agent-ks-prompt-key variant) input)
       (bb:ks-execute variant bb)
       (bb:read-section bb (agent-ks-result-key variant) :default nil)))
    ((typep variant 'bb:knowledge-source)
     (let ((board (bb:make-blackboard)))
       (bb:write-section board :prompt input)
       (let ((out (bb:ks-execute variant board)))
         (or (and (bb:section-bound-p board :result)
                  (bb:read-section board :result))
             out))))
    (t input)))

(defun default-improve-gate ()
  (eval:make-default-promotion-gate))

(defun %phase (name thunk)
  (when *improve-phase-hook*
    (funcall *improve-phase-hook* name))
  (funcall thunk))

(defun %dataset-of (domain)
  (first (expert-eval-suites domain)))

(defun %suite-by-role (domain role)
  (find role (expert-eval-suites domain)
        :key (lambda (ds)
               (and (eval:eval-dataset-p ds) (eval:eval-dataset-role ds)))
        :test #'eq))

(defun %split-role (dataset role)
  (when (and dataset (eval:eval-dataset-p dataset) role)
    (let ((split (ignore-errors (eval:dataset-split dataset :role role))))
      (when (and split (plusp (length (eval:eval-dataset-cases split))))
        split))))

(defun %search-dataset (domain dataset)
  "Train (then dev) used for search. Never the promotion holdout."
  (or (%suite-by-role domain :train)
      (%split-role dataset :train)
      (%suite-by-role domain :dev)
      (%split-role dataset :dev)))

(defun %promotion-dataset (domain dataset)
  "Holdout used for gating, or NIL when the suite has no holdout role."
  (or (%suite-by-role domain :holdout)
      (%split-role dataset :holdout)))

(defun %assert-search-holdout-disjoint (search holdout)
  "Invariant: search/train data and the promotion holdout never overlap."
  (when (and search holdout
             (eval:eval-dataset-p search)
             (eval:eval-dataset-p holdout)
             (not (eq search holdout)))
    (eval:assert-no-holdout-overlap search holdout))
  t)

(defun %eval-run-plist (run)
  (list :mean (if run (eval:eval-run-mean run) 0)
        :n (if run (eval:eval-run-n run) 0)
        :pass-count (if run (eval:eval-run-pass-count run) 0)
        :results
        (when run
          (mapcar (lambda (r)
                    (let ((case (eval:eval-case-result-case r))
                          (score (eval:eval-case-result-score r)))
                      (list :input (and case (eval:eval-case-input case))
                            :expected (and case (eval:eval-case-expected case))
                            :metadata (and case (eval:eval-case-metadata case))
                            :role (and case (eval:eval-case-role case))
                            :source (and case (eval:eval-case-source case))
                            :parent-version (and case (eval:eval-case-parent-version case))
                            :actual (eval:eval-case-result-actual r)
                            :verdict (and score (eval:eval-score-verdict score))
                            :value (and score (eval:eval-score-value score)))))
                  (eval:eval-run-results run)))))

(defun %run-from-plist (dataset plist)
  (eval:make-eval-run
   :dataset dataset
   :results
   (mapcar (lambda (row)
             (eval:make-eval-case-result
              :case (eval:make-eval-case
                     :input (getf row :input)
                     :expected (getf row :expected)
                     :metadata (copy-list (getf row :metadata))
                     :role (getf row :role)
                     :source (getf row :source)
                     :parent-version (getf row :parent-version))
              :actual (getf row :actual)
              :score (eval:make-eval-score
                      :value (or (getf row :value) 0)
                      :verdict (or (getf row :verdict) :fail))))
           (getf plist :results))))

(defun %merge-eval-runs (dataset runs)
  (eval:make-eval-run
   :dataset dataset
   :results (mapcan (lambda (run)
                      (copy-list (eval:eval-run-results run)))
                    runs)))

(defun %paired-from-runs (baseline-runs candidate-runs)
  (let ((n (length baseline-runs))
        (wins 0)
        (ties 0)
        (losses 0)
        (delta-sum 0))
    (mapc (lambda (b c)
            (let ((d (- (eval:eval-run-mean c) (eval:eval-run-mean b))))
              (incf delta-sum d)
              (cond
                ((> d 0) (incf wins))
                ((< d 0) (incf losses))
                (t (incf ties)))))
          baseline-runs
          candidate-runs)
    (eval:make-paired-trial-result
     :n n
     :baseline-runs baseline-runs
     :candidate-runs candidate-runs
     :wins wins
     :ties ties
     :losses losses
     :delta (if (zerop n) 0 (/ delta-sum n)))))

(defun %paired-plist (paired)
  (when paired
    (list :n (eval:paired-trial-result-n paired)
          :wins (eval:paired-trial-result-wins paired)
          :ties (eval:paired-trial-result-ties paired)
          :losses (eval:paired-trial-result-losses paired)
          :delta (eval:paired-trial-result-delta paired)
          :confidence (eval:paired-trial-result-confidence paired))))

(defun %promotion-record-plist (record)
  (when record
    (list :stage (eval:promotion-record-stage record)
          :rollback-p (eval:promotion-record-rollback-p record)
          :reason (eval:promotion-record-reason record)
          :from-stage (eval:promotion-record-from-stage record))))

(defun %stage-promotion (pass &key (stage :shadow))
  "Walk shadow → canary → promote, or emit a rollback marker."
  (let ((start (or stage :shadow)))
    (if pass
        (list (eval:make-promotion-record :shadow)
              (eval:make-promotion-record :canary)
              (eval:make-promotion-record :promote))
        (list (eval:make-promotion-record start)
              (eval:make-rollback-marker
               :from start :reason "gate failed")))))

(defun %propose-revision (llm domain ks)
  (declare (ignore domain))
  (let* ((prompt (concatenate
                  'string
                  "Propose a ks-revision for knowledge source "
                  (string (bb:ks-name ks))
                  ". Return skill-text, prompt, chunk-config."))
         (response (llm:generate llm prompt :output 'ks-revision))
         (out (or (and response (llm:llm-response-output response))
                  (and response (llm:llm-response-text response)))))
    (coerce-ks-revision out)))

(defun %require-hitl-p (domain require-hitl)
  (or require-hitl
      (let ((prof (and (expert-domain-p domain) (expert-profile domain))))
        (and (deployment-profile-p prof) (profile-require-hitl-p prof)))))

(defun %hitl-approve (cycle-id)
  (restart-case
      (error 'promotion-approval-required :cycle-id cycle-id)
    (approve ()
      :report "Approve the promotion"
      t)
    (deny ()
      :report "Deny the promotion"
      nil)))

(defun %journal-for (domain journal)
  (or journal
      (let ((prof (and (expert-domain-p domain) (expert-profile domain))))
        (cond
          ((and (deployment-profile-p prof) (profile-journal prof))
           (profile-journal prof))
          (t (task:make-in-memory-journal))))))

(defun %llm-for (domain llm)
  (or (resolve-profile-llm (and (expert-domain-p domain) (expert-profile domain))
                          llm)
      (llm:make-mock-llm-backend)))

(defun %coerce-restricted-catalogue (catalogue)
  (cond
    ((null catalogue) nil)
    ((restricted-catalogue-p catalogue) catalogue)
    (t (make-restricted-catalogue catalogue))))

(defun %root-fingerprint (bb)
  (let ((root (bb:find-root-bb bb)))
    (list :sections (mapcar (lambda (k)
                              (cons k (bb:read-section root k :default :absent)))
                            (sort (copy-list (bb:list-sections root))
                                  #'string< :key #'string))
          :watchers (sort (mapcar #'bb:watcher-id (bb:list-watchers root))
                          #'string< :key #'string)
          :ks (sort (mapcar #'bb:ks-name (bb:list-ks root))
                    #'string< :key #'string))))

(defun %assert-root-unchanged (root before &key cycle-id)
  (let ((after (%root-fingerprint root)))
    (unless (equal before after)
      (restart-case
          (error 'trial-isolation-error
                 :message (format nil "root board changed during trial ~s"
                                  cycle-id)
                 :root root
                 :before before
                 :after after)
        (continue ()
          :report "Ignore the root-board leak and keep the trial scores"
          after)))
    after))

(defun %prompt-key-for (ks)
  (cond
    ((agent-ks-p ks) (agent-ks-prompt-key ks))
    ((and (revised-ks-p ks) (agent-ks-p (revised-ks-base ks)))
     (agent-ks-prompt-key (revised-ks-base ks)))
    (t :prompt)))

(defun %result-key-for (ks)
  (cond
    ((agent-ks-p ks) (agent-ks-result-key ks))
    ((and (revised-ks-p ks) (agent-ks-p (revised-ks-base ks)))
     (agent-ks-result-key (revised-ks-base ks)))
    (t :result)))

(defun %install-trial-ks (cow vks restricted)
  "Watch VKS on the fork only. REGISTER-KS would mutate the root registry.
   The handler rebinds trial specials on the KSAR worker thread."
  (bb:watch cow
            :id (bb:ks-name vks)
            :requires (or (ks-watch-keys vks) '(:prompt))
            :priority (bb:ks-priority vks)
            :handler (lambda (board ksar)
                       (let ((*current-ksar* ksar)
                             (*trial-restricted-catalogue* restricted)
                             (*trial-force-variant*
                              (versioned-ks-force-variant vks)))
                         (when (bb:ks-precondition vks board)
                           (bb:ks-postcondition
                            vks board (bb:ks-execute vks board)))))))

(defun %scheduler-respond (cow vks input &key (variant :current))
  "Run the installed versioned KS through the fork's scheduler.
   FORCE-VARIANT is stored on VKS so the worker thread sees it."
  (let ((prompt-key (%prompt-key-for (versioned-ks-current vks)))
        (result-key (%result-key-for (versioned-ks-current vks))))
    (setf (versioned-ks-force-variant vks) variant)
    (bb:remove-section cow result-key)
    (bb:remove-section cow prompt-key)
    (when (bb:section-bound-p cow :chunk-config)
      (bb:remove-section cow :chunk-config))
    (bb:write-section cow prompt-key input)
    (bb:run-scheduler cow :until-empty t)
    (or (and (bb:section-bound-p cow result-key)
             (bb:read-section cow result-key))
        (and (bb:section-bound-p cow :result)
             (bb:read-section cow :result)))))

(defun %run-trials (domain current candidate dataset &key trials wall-clock
                     cycle-id catalogue blackboard)
  "N isolated forks. Each installs VERSIONED-KS on the COW board and
   scores both variants through the fork scheduler. All repetitions
   are aggregated — the last run does not replace the earlier ones."
  (declare (ignore domain))
  (let* ((n (max 1 (or trials 1)))
         (root (or blackboard (bb:make-blackboard)))
         (restricted (%coerce-restricted-catalogue catalogue))
         (baseline-runs nil)
         (candidate-runs nil)
         (before (%root-fingerprint root)))
    (dotimes (i n)
      (let* ((ws (bb:fork-workspace root (format nil "trial-~a-~a" cycle-id i)))
             (cow (bb:workspace-blackboard ws)))
        (unwind-protect
             (let* ((*trial-restricted-catalogue* restricted)
                    (vks (make-versioned-ks
                          :name (bb:ks-name current)
                          :current current
                          :candidate candidate
                          :cycle-id cycle-id)))
               (%install-trial-ks cow vks restricted)
               (flet ((once ()
                        (cons
                         (eval:run-eval
                          dataset
                          (lambda (in)
                            (%scheduler-respond cow vks in :variant :current)))
                         (eval:run-eval
                          dataset
                          (lambda (in)
                            (%scheduler-respond cow vks in
                                                :variant :candidate))))))
                 (let* ((pair (call-with-wall-clock wall-clock #'once))
                        (b (car pair))
                        (c (cdr pair))
                        (ks-tag (string (bb:ks-name current))))
                   (when b
                     (demiurge::%observe-record "RECORD-EVAL-SCORE" ks-tag
                                                (eval:eval-run-mean b))
                     (push b baseline-runs))
                   (when c
                     (demiurge::%observe-record "RECORD-EVAL-SCORE" ks-tag
                                                (eval:eval-run-mean c))
                     (push c candidate-runs))
                   (%assert-root-unchanged root before :cycle-id cycle-id))))
          (ignore-errors (bb:unwatch cow (bb:ks-name current)))
          (ignore-errors (bb:discard-workspace ws)))))
    (%assert-root-unchanged root before :cycle-id cycle-id)
    (let* ((baseline-runs (nreverse baseline-runs))
           (candidate-runs (nreverse candidate-runs))
           (paired (%paired-from-runs baseline-runs candidate-runs)))
      (values (%merge-eval-runs dataset baseline-runs)
              (%merge-eval-runs dataset candidate-runs)
              paired))))

(defun %decide-verdict (pass cycle-id)
  (let ((default (if pass :promote :demote)))
    (restart-case
        (progn
          (signal 'improvement-decision
                  :verdict default
                  :cycle-id cycle-id)
          default)
      (promote ()
        :report "Promote the candidate"
        :promote)
      (demote ()
        :report "Demote the candidate"
        :demote)
      (defer ()
        :report "Defer the decision"
        :defer))))

(defun %paired-from-plist (plist)
  (when plist
    (eval:make-paired-trial-result
     :n (or (getf plist :n) 0)
     :wins (or (getf plist :wins) 0)
     :ties (or (getf plist :ties) 0)
     :losses (or (getf plist :losses) 0)
     :delta (or (getf plist :delta) 0)
     :confidence (getf plist :confidence))))

(defun run-improvement-cycle (domain &key target llm journal task-id cycle-id
                                       (trials 1) wall-clock budget
                                       (activity-floor 1) (window 5)
                                       skill-store blackboard
                                       require-hitl
                                       (gate (default-improve-gate))
                                       (min-sample 1)
                                       (confidence-threshold 0)
                                       (promotion-stage :shadow)
                                       history)
  "Durable select → propose → trial → gate → promote/demote.
   Trials run the installed versioned KS through a COW-fork scheduler.
   Promotion uses the holdout split; search/train never overlaps holdout.
   Restarts PROMOTE / DEMOTE / DEFER. Corporate HITL is off by default."
  (check-type domain expert-domain)
  (let* ((cycle-id (or cycle-id
                       (fresh-durable-id "improve" (expert-name domain))))
         (journal (%journal-for domain journal))
         (task (task:make-durable-task
                :id (or task-id cycle-id)
                :journal journal))
         (llm (wrap-llm-budget (%llm-for domain llm) cycle-id :budget budget))
         (hist (or history *ks-eval-history*))
         (board (or blackboard (bb:make-blackboard)))
         (restricted (%coerce-restricted-catalogue (expert-catalogue domain)))
         (result nil))
    (flet ((finish (verdict &optional extra)
             (setf result (append (list :verdict verdict
                                        :cycle-id cycle-id)
                                  extra))
             result))
      (task:with-durable-task (task journal)
        (restart-case
            (handler-bind ((llm:llm-budget-exceeded
                            (lambda (c)
                              (let ((r (find-restart 'defer)))
                                (if r
                                    (invoke-restart r)
                                    (error c))))))
              (let ((selected
                     (task:with-durable-step
                         ("select" :idempotency-key "improve/select")
                       (%phase "select"
                               (lambda ()
                                 (let ((ks (select-improvement-target
                                            domain
                                            :activity-floor activity-floor
                                            :window window
                                            :history hist
                                            :target target)))
                                   (if ks
                                       (list :ks-name (string (bb:ks-name ks)))
                                       nil)))))))
                (unless selected
                  (finish :skipped)
                  (task:complete-task task result)
                  (return-from run-improvement-cycle result))
                (let* ((ks-name (getf selected :ks-name))
                       (current (or (and target
                                         (typep target 'bb:knowledge-source)
                                         target)
                                    (find (string-downcase ks-name)
                                          (expert-ks-set domain)
                                          :key (lambda (ks)
                                                 (string-downcase
                                                  (string (bb:ks-name ks))))
                                          :test #'equal)))
                       (revision-plist
                        (task:with-durable-step
                            ("propose" :idempotency-key "improve/propose")
                          (%phase "propose"
                                  (lambda ()
                                    (ks-revision-plist
                                     (%propose-revision llm domain current))))))
                       (revision (coerce-ks-revision revision-plist))
                       (*trial-restricted-catalogue* restricted)
                       (candidate (apply-ks-revision current revision
                                                     :catalogue restricted))
                       (raw-dataset (%dataset-of domain))
                       (search (%search-dataset domain raw-dataset))
                       (holdout (%promotion-dataset domain raw-dataset))
                       (dataset (or holdout raw-dataset)))
                  (unless dataset
                    (finish :skipped (list :reason :no-dataset))
                    (task:complete-task task result)
                    (return-from run-improvement-cycle result))
                  (when holdout
                    (%assert-search-holdout-disjoint search holdout))
                  (let ((trial-plist
                         (task:with-durable-step
                             ("trial" :idempotency-key "improve/trial")
                           (%phase "trial"
                                   (lambda ()
                                     (multiple-value-bind (base cand paired)
                                         (%run-trials
                                          domain current candidate dataset
                                          :trials trials
                                          :wall-clock wall-clock
                                          :cycle-id cycle-id
                                          :catalogue restricted
                                          :blackboard board)
                                       (list :baseline (%eval-run-plist base)
                                             :candidate (%eval-run-plist cand)
                                             :paired (%paired-plist paired)
                                             :eval-run-id
                                             (format nil "~a/eval" cycle-id))))))))
                    (let* ((baseline-run (%run-from-plist
                                          dataset (getf trial-plist :baseline)))
                           (candidate-run (%run-from-plist
                                           dataset (getf trial-plist :candidate)))
                           (paired (or (%paired-from-plist
                                        (getf trial-plist :paired))
                                       (eval:make-paired-trial-result :n 0)))
                           (score-pass (eval:gate-passes-p gate baseline-run
                                                           candidate-run))
                           (paired-pass (eval:paired-trial-gate-passes-p
                                         paired
                                         :min-sample min-sample
                                         :confidence-threshold
                                         confidence-threshold))
                           (pass (and score-pass paired-pass))
                           (records (%stage-promotion
                                     pass :stage promotion-stage))
                           (final-record (car (last records)))
                           (gate-plist
                            (task:with-durable-step
                                ("gate" :idempotency-key "improve/gate")
                              (%phase "gate"
                                      (lambda ()
                                        (list :pass (if pass t nil)
                                              :score-pass (if score-pass t nil)
                                              :paired-pass (if paired-pass t nil)
                                              :paired (%paired-plist paired)
                                              :promotion
                                              (mapcar #'%promotion-record-plist
                                                      records)
                                              :baseline-mean
                                              (eval:eval-run-mean baseline-run)
                                              :candidate-mean
                                              (eval:eval-run-mean candidate-run))))))
                           (verdict
                            (task:with-durable-step
                                ("promote" :idempotency-key "improve/promote")
                              (%phase "promote"
                                      (lambda ()
                                        (let ((v (%decide-verdict
                                                  (getf gate-plist :pass)
                                                  cycle-id)))
                                          (when (and (eq v :promote)
                                                     (%require-hitl-p
                                                      domain require-hitl))
                                            (unless (%hitl-approve cycle-id)
                                              (setf v :demote)))
                                          (when (and (eq v :promote)
                                                     final-record
                                                     (eval:rollback-marker-p
                                                      final-record))
                                            (setf v :demote))
                                          (save-promoted-skill
                                           domain revision
                                           :cycle-id cycle-id
                                           :eval-run-id
                                           (getf trial-plist :eval-run-id)
                                           :baseline-score
                                           (getf gate-plist :baseline-mean)
                                           :candidate-score
                                           (getf gate-plist :candidate-mean)
                                           :skill-store skill-store
                                           :blackboard board
                                           :verdict v)
                                          (when current
                                            (record-ks-eval
                                             (bb:ks-name current)
                                             (getf gate-plist :candidate-mean)
                                             hist))
                                          (string-downcase (symbol-name v))))))))
                      (finish (intern (string-upcase verdict) :keyword)
                              (list :eval-run-id (getf trial-plist :eval-run-id)
                                    :baseline-score
                                    (getf gate-plist :baseline-mean)
                                    :candidate-score
                                    (getf gate-plist :candidate-mean)
                                    :n-trials (eval:paired-trial-result-n paired)
                                    :paired-confidence
                                    (eval:paired-trial-result-confidence paired)
                                    :promotion-stage
                                    (and final-record
                                         (eval:promotion-record-stage
                                          final-record))
                                    :rollback-p
                                    (and final-record
                                         (eval:promotion-record-rollback-p
                                          final-record)
                                         t)))
                      (task:complete-task task result)
                      result)))))
          (defer ()
            :report "Defer this improvement cycle"
            (finish :defer)
            (ignore-errors (task:complete-task task result))
            result))))))
