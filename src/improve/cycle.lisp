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

(defclass revised-ks (bb:knowledge-source)
  ((base :initarg :base :accessor revised-ks-base :initform nil)
   (revision :initarg :revision :accessor revised-ks-revision :initform nil)))

(defun revised-ks-p (x)
  (typep x 'revised-ks))

(defun apply-ks-revision (ks revision)
  "Install REVISION as a candidate wrapper around KS."
  (let ((rev (coerce-ks-revision revision)))
    (make-instance 'revised-ks
                   :name (if (typep ks 'bb:knowledge-source)
                             (bb:ks-name ks)
                             'revised)
                   :base ks
                   :revision rev
                   :priority (if (typep ks 'bb:knowledge-source)
                                 (bb:ks-priority ks)
                                 0))))

(defmethod ks-watch-keys ((ks revised-ks))
  (let ((base (revised-ks-base ks)))
    (if (typep base 'bb:knowledge-source)
        (ks-watch-keys base)
        '(:prompt))))

(defmethod bb:ks-precondition ((ks revised-ks) blackboard)
  (let ((base (revised-ks-base ks)))
    (if (typep base 'bb:knowledge-source)
        (bb:ks-precondition base blackboard)
        (bb:section-bound-p blackboard :prompt))))

(defmethod bb:ks-execute ((ks revised-ks) blackboard)
  (let* ((rev (revised-ks-revision ks))
         (skill (and rev (ks-revision-skill-text rev)))
         (input (cond
                  ((bb:section-bound-p blackboard :prompt)
                   (bb:read-section blackboard :prompt))
                  (t nil))))
    (if (and skill (plusp (length skill)))
        (let ((out (if (stringp input)
                       (concatenate 'string skill input)
                       skill)))
          (bb:write-section blackboard :result out)
          out)
        (let ((base (revised-ks-base ks)))
          (when (and (agent-ks-p base) rev
                     (plusp (length (or (ks-revision-prompt rev) ""))))
            (setf (agent:ai-agent-instructions (agent-ks-agent base))
                  (ks-revision-prompt rev)))
          (if (typep base 'bb:knowledge-source)
              (bb:ks-execute base blackboard)
              nil)))))

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
                     :metadata (copy-list (getf row :metadata)))
              :actual (getf row :actual)
              :score (eval:make-eval-score
                      :value (or (getf row :value) 0)
                      :verdict (or (getf row :verdict) :fail))))
           (getf plist :results))))

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

(defvar *trial-restricted-catalogue* nil
  "Restricted catalogue copy bound for the duration of a trial.")

(defun %run-trials (domain current candidate dataset &key trials wall-clock
                     cycle-id catalogue)
  (let ((n (max 1 (or trials 1)))
        (baseline nil)
        (cand-run nil)
        (restricted (and catalogue (make-restricted-catalogue catalogue))))
    (dotimes (i n)
      (let* ((board (bb:make-blackboard))
             (ws (bb:fork-workspace board (format nil "trial-~a" i)))
             (cow (bb:workspace-blackboard ws)))
        (declare (ignore cow))
        (unwind-protect
             (let* ((*trial-restricted-catalogue* restricted)
                    (vks (make-versioned-ks
                          :name (bb:ks-name current)
                          :current current
                          :candidate candidate
                          :cycle-id cycle-id)))
               (declare (ignore vks))
               (flet ((once ()
                        (values
                         (eval:run-eval dataset
                                        (lambda (in) (%respond current in)))
                         (eval:run-eval dataset
                                        (lambda (in) (%respond candidate in))))))
                 (multiple-value-bind (b c)
                     (call-with-wall-clock wall-clock #'once)
                   (let ((ks-tag (string (bb:ks-name current))))
                     (when b
                       (demiurge::%observe-record "RECORD-EVAL-SCORE" ks-tag
                                                  (eval:eval-run-mean b)))
                     (when c
                       (demiurge::%observe-record "RECORD-EVAL-SCORE" ks-tag
                                                  (eval:eval-run-mean c))))
                   (setf baseline b cand-run c))))
          (ignore-errors (bb:discard-workspace ws)))))
    (values baseline cand-run)))

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

(defun run-improvement-cycle (domain &key target llm journal task-id cycle-id
                                       (trials 1) wall-clock budget
                                       (activity-floor 1) (window 5)
                                       skill-store blackboard
                                       require-hitl
                                       (gate (default-improve-gate))
                                       history)
  "Durable select → propose → trial → gate → promote/demote.
   Restarts PROMOTE / DEMOTE / DEFER. Corporate HITL is off by default."
  (check-type domain expert-domain)
  (let* ((cycle-id (or cycle-id
                       (format nil "improve/~a" (expert-name domain))))
         (journal (%journal-for domain journal))
         (task (task:make-durable-task
                :id (or task-id cycle-id)
                :journal journal))
         (llm (wrap-llm-budget (%llm-for domain llm) cycle-id :budget budget))
         (hist (or history *ks-eval-history*))
         (board (or blackboard (bb:make-blackboard)))
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
                       (candidate (apply-ks-revision current revision))
                       (dataset (%dataset-of domain)))
                  (unless dataset
                    (finish :skipped (list :reason :no-dataset))
                    (task:complete-task task result)
                    (return-from run-improvement-cycle result))
                  (let ((trial-plist
                         (task:with-durable-step
                             ("trial" :idempotency-key "improve/trial")
                           (%phase "trial"
                                   (lambda ()
                                     (multiple-value-bind (base cand)
                                         (%run-trials
                                          domain current candidate dataset
                                          :trials trials
                                          :wall-clock wall-clock
                                          :cycle-id cycle-id
                                          :catalogue (expert-catalogue domain))
                                       (list :baseline (%eval-run-plist base)
                                             :candidate (%eval-run-plist cand)
                                             :eval-run-id
                                             (format nil "~a/eval" cycle-id))))))))
                    (let* ((baseline-run (%run-from-plist
                                          dataset (getf trial-plist :baseline)))
                           (candidate-run (%run-from-plist
                                           dataset (getf trial-plist :candidate)))
                           (pass (eval:gate-passes-p gate baseline-run
                                                     candidate-run))
                           (gate-plist
                            (task:with-durable-step
                                ("gate" :idempotency-key "improve/gate")
                              (%phase "gate"
                                      (lambda ()
                                        (list :pass (if pass t nil)
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
                                    (getf gate-plist :candidate-mean)))
                      (task:complete-task task result)
                      result)))))
          (defer ()
            :report "Defer this improvement cycle"
            (finish :defer)
            (ignore-errors (task:complete-task task result))
            result))))))
