(in-package #:demiurge/tests)

(defclass script-ks (bb:knowledge-source)
  ((fn :initarg :fn :accessor script-ks-fn)))

(defmethod bb:ks-precondition ((ks script-ks) board)
  (bb:section-bound-p board :prompt))

(defmethod bb:ks-execute ((ks script-ks) board)
  (let ((out (funcall (script-ks-fn ks) (bb:read-section board :prompt))))
    (bb:write-section board :result out)
    out))

(defun %script-ks (name fn)
  (make-instance 'script-ks :name name :fn fn))

(defun %revision-llm (skill-text)
  (llm:make-mock-llm-backend
   :handler (lambda (backend turns &key &allow-other-keys)
              (declare (ignore backend turns))
              (llm:make-llm-response
               :parts (list (llm:make-llm-text-part :text "ok"))
               :output (make-ks-revision :skill-text skill-text)))))

(defun %improve-domain (&key name cases ks llm profile)
  (make-expert-domain
   :name (or name "improve-demo")
   :ks-set (list (or ks (%script-ks 'echo
                                    (lambda (in)
                                      (format nil "old: ~a" in)))))
   :eval-suites (list (eval:make-eval-dataset
                       :name "improve"
                       :cases cases))
   :profile (or profile :personal)
   :catalogue (cap:make-catalogue :world)))

(deftest select-variant-is-deterministic
  (let* ((vks (make-versioned-ks
               :name 'ab
               :current (%script-ks 'cur (lambda (in) in))
               :candidate (%script-ks 'cand (lambda (in) in))
               :split-ratio 2))
         (even (bb:make-ksar :id 2))
         (odd (bb:make-ksar :id 1)))
    (ok (eq :candidate (select-variant vks even)))
    (ok (eq :current (select-variant vks odd)))
    (ok (eq (select-variant vks even) (select-variant vks even)))
    (ok (eq (select-variant vks odd) (select-variant vks odd)))))

(deftest versioned-ks-records-observation
  (let* ((cur (%script-ks 'cur (lambda (in) (format nil "cur:~a" in))))
         (cand (%script-ks 'cand (lambda (in) (format nil "cand:~a" in))))
         (vks (make-versioned-ks :name 'ab :current cur :candidate cand
                                 :split-ratio 2 :cycle-id "c1"))
         (board (bb:make-blackboard))
         (ksar (bb:make-ksar :id 1)))
    (bb:write-section board :prompt "hi")
    (let* ((demiurge/improve::*current-ksar* ksar)
           (out (bb:ks-execute vks board)))
      (ok (equal "cur:hi" out))
      (ok (= 1 (length (versioned-ks-observations vks))))
      (let* ((obs (first (versioned-ks-observations vks)))
             (case (eval:eval-case-result-case obs))
             (tags (getf (eval:eval-case-metadata case) :tags)))
        (ok (equal "hi" (eval:eval-case-input case)))
        (ok (equal "cur:hi" (eval:eval-case-result-actual obs)))
        (ok (member :current tags))
        (ok (member 'ab tags))))))

(deftest versioned-ks-precondition-delegates
  (let* ((cur (%script-ks 'cur (lambda (in) in)))
         (vks (make-versioned-ks :current cur :candidate cur))
         (board (bb:make-blackboard)))
    (ok (null (bb:ks-precondition vks board)))
    (bb:write-section board :prompt "x")
    (ok (bb:ks-precondition vks board))))

(deftest improve-cycle-promotes-higher-score
  (with-tmp-dir (tmp)
    (let* ((store (steer:make-file-skill-store tmp))
           (cases (list (eval:make-eval-case :input "hi" :expected "echo: hi")))
           (domain (%improve-domain :name "promote-e2e" :cases cases))
           (result (run-improvement-cycle
                    domain
                    :target (first (expert-ks-set domain))
                    :llm (%revision-llm "echo: ")
                    :skill-store store
                    :journal (task:make-in-memory-journal)
                    :cycle-id "promote-e2e"
                    :activity-floor 0)))
      (ok (eq :promote (getf result :verdict)))
      (ok (= 0 (getf result :baseline-score)))
      (ok (= 1 (getf result :candidate-score)))
      (ok (steer:skill-versions store "promote-e2e")))))

(deftest improve-cycle-demotes-critical-regression
  (let* ((cases (list (eval:make-eval-case :input "x" :expected "echo: x")
                      (eval:make-eval-case :input "y" :expected "echo: y")
                      (eval:make-eval-case
                       :input "crit" :expected "keep"
                       :metadata '(:tags (:critical)))))
         (ks (%script-ks 'echo
                         (lambda (in)
                           (if (equal in "crit")
                               "keep"
                               (format nil "old: ~a" in)))))
         (domain (%improve-domain :name "demote-e2e" :cases cases :ks ks))
         (result (run-improvement-cycle
                  domain
                  :target ks
                  :llm (%revision-llm "echo: ")
                  :journal (task:make-in-memory-journal)
                  :cycle-id "demote-e2e"
                  :activity-floor 0)))
    (ok (eq :demote (getf result :verdict)))
    (ok (> (getf result :candidate-score)
           (getf result :baseline-score)))))

(deftest restricted-catalogue-signals-on-removed-op
  (let ((cat (cap:make-catalogue :world)))
    (cap:register-capability cat (make-instance 'cap:communication-capability))
    (let* ((restricted (make-restricted-catalogue cat))
           (orig (cap:get-capability cat :communication))
           (stub (cap:get-capability restricted :communication)))
      (ok (not (eq cat restricted)))
      (ok (find 'cap:send-message (cap:capability-operations orig)
                :key #'cap:capability-operation-name))
      (ok (null (find 'cap:send-message (cap:capability-operations stub)
                      :key #'cap:capability-operation-name)))
      (ok (signals (cap:invoke-operation stub 'cap:send-message "a" "b")
                   'cap:unknown-operation))
      (ok (plusp (length (restricted-catalogue-recordings restricted)))))))

(deftest improve-budget-exceeded-defers
  (let* ((cases (list (eval:make-eval-case :input "hi" :expected "echo: hi")))
         (domain (%improve-domain :name "budget-e2e" :cases cases))
         (result (run-improvement-cycle
                  domain
                  :target (first (expert-ks-set domain))
                  :llm (%revision-llm "echo: ")
                  :budget (llm:make-llm-budget :max-tokens 0)
                  :journal (task:make-in-memory-journal)
                  :cycle-id "budget-e2e"
                  :activity-floor 0)))
    (ok (eq :defer (getf result :verdict)))))

(deftest improve-promotion-upserts-by-cycle-and-eval-id
  (with-tmp-dir (tmp)
    (let* ((store (steer:make-file-skill-store tmp))
           (domain (%improve-domain :name "upsert-promo"))
           (rev (make-ks-revision :skill-text "echo: ")))
      (save-promoted-skill domain rev
                           :cycle-id "c1" :eval-run-id "e1"
                           :skill-store store :verdict :promote)
      (save-promoted-skill domain rev
                           :cycle-id "c1" :eval-run-id "e1"
                           :skill-store store :verdict :promote)
      (ok (= 1 (length (steer:skill-versions store "upsert-promo"))))
      (ok (find-promoted-skill-version store "upsert-promo" "c1" "e1"))
      (save-promoted-skill domain rev
                           :cycle-id "c1" :eval-run-id "e2"
                           :skill-store store :verdict :promote)
      (ok (= 2 (length (steer:skill-versions store "upsert-promo")))))))

(deftest improve-cycle-mints-fresh-cycle-id
  (let* ((cases (list (eval:make-eval-case :input "hi" :expected "echo: hi")))
         (domain (%improve-domain :name "fresh-cycle" :cases cases))
         (r1 (run-improvement-cycle
              domain
              :target (first (expert-ks-set domain))
              :llm (%revision-llm "echo: ")
              :journal (task:make-in-memory-journal)
              :activity-floor 0))
         (r2 (run-improvement-cycle
              domain
              :target (first (expert-ks-set domain))
              :llm (%revision-llm "echo: ")
              :journal (task:make-in-memory-journal)
              :activity-floor 0)))
    (ok (not (equal (getf r1 :cycle-id) "improve/fresh-cycle")))
    (ok (not (equal (getf r1 :cycle-id) (getf r2 :cycle-id)))
        "two invocations without cycle-id do not share an id")))

(deftest improve-kill-and-resume-mid-cycle
  (let* ((cases (list (eval:make-eval-case :input "hi" :expected "echo: hi")))
         (domain (%improve-domain :name "resume-e2e" :cases cases))
         (journal (task:make-in-memory-journal))
         (counts (list :select 0 :propose 0 :trial 0 :gate 0 :promote 0))
         (hook (lambda (phase)
                 (let ((key (intern (string-upcase phase) :keyword)))
                   (incf (getf counts key 0))
                   (when (equal phase "propose")
                     (error "killed mid-cycle"))))))
    (let ((demiurge/improve::*improve-phase-hook* hook))
      (handler-case
          (run-improvement-cycle
           domain
           :target (first (expert-ks-set domain))
           :llm (%revision-llm "echo: ")
           :journal journal
           :task-id "resume-e2e"
           :cycle-id "resume-e2e"
           :activity-floor 0)
        (error ())))
    (ok (= 1 (getf counts :select)))
    (ok (= 1 (getf counts :propose)))
    (ok (= 0 (getf counts :trial)))
    (let ((demiurge/improve::*improve-phase-hook*
           (lambda (phase)
             (incf (getf counts (intern (string-upcase phase) :keyword) 0)))))
      (let ((result (run-improvement-cycle
                     domain
                     :target (first (expert-ks-set domain))
                     :llm (%revision-llm "echo: ")
                     :journal journal
                     :task-id "resume-e2e"
                     :cycle-id "resume-e2e"
                     :activity-floor 0)))
        (ok (eq :promote (getf result :verdict)))
        (ok (= 1 (getf counts :select)))
        (ok (= 2 (getf counts :propose)))
        (ok (= 1 (getf counts :trial)))))))

(deftest apply-ks-revision-materializes-prompt-and-chunk-config
  (let* ((backend (llm:make-mock-llm-backend :prefix "kept: "))
         (agent (agent:make-ai-agent :name "echo"
                                     :backend backend
                                     :instructions "OLD"))
         (ks (make-agent-ks :name 'echo :agent agent))
         (rev (make-ks-revision :prompt "NEW"
                                :chunk-config '(:size 32 :overlap 4
                                                :prefix "echo: ")))
         (cand (apply-ks-revision ks rev)))
    (ok (equal "OLD" (agent:ai-agent-instructions agent))
        "original agent is not mutated")
    (ok (equal "NEW" (agent:ai-agent-instructions
                      (agent-ks-agent (revised-ks-base cand)))))
    (ok (plusp (length (ks-revision-chunk-config (revised-ks-revision cand)))))
    (ok (revised-ks-chunker cand)))
  (let* ((ks (%script-ks 'echo (lambda (in) (format nil "old: ~a" in))))
         (rev (make-ks-revision :prompt "pre:"
                                :chunk-config '(:prefix "echo: ")))
         (cand (apply-ks-revision ks rev))
         (board (bb:make-blackboard)))
    (bb:write-section board :prompt "hi")
    (ok (equal "echo: hi" (bb:ks-execute cand board)))
    (ok (equal '(:prefix "echo: ")
               (parse-chunk-config (bb:read-section board :chunk-config))))))

(deftest restricted-catalogue-is-installed-on-candidate-tools
  (let ((cat (cap:make-catalogue :world)))
    (cap:register-capability cat (make-instance 'cap:communication-capability))
    (let* ((restricted (make-restricted-catalogue cat))
           (agent (agent:make-ai-agent
                   :name "tools"
                   :backend (llm:make-mock-llm-backend :prefix "ok: ")
                   :instructions "base"))
           (ks (make-agent-ks :name 'tools :agent agent :catalogue cat))
           (cand (apply-ks-revision ks (make-ks-revision :prompt "rev")
                                    :catalogue restricted))
           (tools (collect-agent-ks-tools
                   (revised-ks-base cand)
                   :catalogue (revised-ks-catalogue cand)))
           (names (mapcar #'llm:llm-tool-name tools)))
      (ok (restricted-catalogue-p (revised-ks-catalogue cand)))
      (ok (restricted-catalogue-p (agent-ks-catalogue (revised-ks-base cand))))
      (ok (not (find "communication/send-message" names :test #'equal)))
      (let ((*trial-restricted-catalogue* restricted)
            (via-special (collect-agent-ks-tools ks)))
        (ok (not (find "communication/send-message"
                       (mapcar #'llm:llm-tool-name via-special)
                       :test #'equal)))))))

(deftest trial-does-not-leak-onto-root-board
  (let* ((hits (list 0))
         (ks (%script-ks 'echo
                         (lambda (in)
                           (let ((ksar *current-ksar*))
                             (when ksar
                               (incf (car hits))
                               (let ((target (bb:ksar-blackboard ksar)))
                                 (when target
                                   (bb:write-section target :leaked t)))))
                           (format nil "old: ~a" in))))
         (cases (list (eval:make-eval-case :input "hi" :expected "echo: hi")))
         (root (bb:make-blackboard))
         (domain (%improve-domain :name "iso-root" :cases cases :ks ks)))
    (bb:write-section root :sentinel "keep")
    (let ((result (run-improvement-cycle
                   domain
                   :target ks
                   :llm (%revision-llm "echo: ")
                   :journal (task:make-in-memory-journal)
                   :cycle-id "iso-root"
                   :blackboard root
                   :activity-floor 0)))
      (ok (eq :promote (getf result :verdict)))
      (ok (plusp (car hits))
          "versioned KS ran through the fork scheduler (KSAR bound)")
      (ok (equal "keep" (bb:read-section root :sentinel)))
      (ok (not (bb:section-bound-p root :leaked)))
      (ok (not (bb:section-bound-p root :prompt)))
      (ok (not (bb:section-bound-p root :result)))
      (ok (null (bb:list-watchers root)))
      (ok (null (bb:list-ks root))))))

(deftest trials-aggregate-all-repetitions
  (let* ((n (list 0))
         (ks (%script-ks 'echo
                         (lambda (in)
                           (if (eq *trial-force-variant* :candidate)
                               (progn
                                 (incf (car n))
                                 (if (evenp (car n))
                                     (format nil "echo: ~a" in)
                                     "nope"))
                               (format nil "old: ~a" in)))))
         (cases (list (eval:make-eval-case :input "hi" :expected "echo: hi")))
         (domain (%improve-domain :name "agg" :cases cases :ks ks))
         (result (run-improvement-cycle
                  domain
                  :target ks
                  :llm (%revision-llm "")
                  :journal (task:make-in-memory-journal)
                  :cycle-id "agg"
                  :trials 3
                  :activity-floor 0)))
    (ok (= 3 (car n)) "candidate executed once per repetition")
    (ok (= 3 (getf result :n-trials)))
    (ok (= 1/3 (getf result :candidate-score))
        "mean includes every repetition, not only the last (which failed)")
    (ok (eq :promote (getf result :verdict)))
    (ok (eq :promote (getf result :promotion-stage)))
    (ok (null (getf result :rollback-p)))))

(deftest paired-confidence-gate-rolls-back
  (let* ((cases (list (eval:make-eval-case :input "hi" :expected "echo: hi")))
         (domain (%improve-domain :name "conf" :cases cases))
         (result (run-improvement-cycle
                  domain
                  :target (first (expert-ks-set domain))
                  :llm (%revision-llm "echo: ")
                  :journal (task:make-in-memory-journal)
                  :cycle-id "conf"
                  :trials 1
                  :min-sample 1
                  :confidence-threshold 95/100
                  :activity-floor 0)))
    (ok (eq :demote (getf result :verdict)))
    (ok (getf result :rollback-p))
    (ok (eq :shadow (getf result :promotion-stage)))
    (ok (= 1 (getf result :candidate-score)))))

(deftest search-and-holdout-never-overlap
  (let* ((train (eval:make-eval-dataset
                 :name "train" :role :train
                 :cases (list (eval:make-eval-case :input "h" :expected "echo: h"
                                                   :role :train))))
         (holdout (eval:make-eval-dataset
                   :name "hold" :role :holdout
                   :cases (list (eval:make-eval-case :input "h" :expected "echo: h"
                                                     :role :holdout))))
         (domain (make-expert-domain
                  :name "overlap"
                  :ks-set (list (%script-ks 'echo
                                            (lambda (in)
                                              (format nil "old: ~a" in))))
                  :eval-suites (list train holdout)
                  :profile :personal)))
    (ok (signals (run-improvement-cycle
                  domain
                  :target (first (expert-ks-set domain))
                  :llm (%revision-llm "echo: ")
                  :journal (task:make-in-memory-journal)
                  :cycle-id "overlap"
                  :activity-floor 0)
                 'eval:holdout-overlap-error))))

(deftest production-feedback-lands-on-train-not-holdout
  (let* ((holdout (eval:make-eval-dataset
                   :name "hold" :role :holdout
                   :cases (list (eval:make-eval-case :input "gold" :expected "gold"
                                                     :role :holdout))))
         (domain (make-echo-expert :backend (mock-llm) :name "fb-hold"))
         (hold-n (length (eval:eval-dataset-cases holdout))))
    (setf (expert-eval-suites domain) (list holdout))
    (ok (signals (eval:add-case holdout
                                (eval:make-eval-case :input "fb" :expected "fb")
                                :source :human-feedback)
                 'eval:holdout-admission-error))
    (let* ((new (record-feedback domain
                                 :answer "echo: hi"
                                 :correction "better"
                                 :feedback-id "fb-hold-1"
                                 :ks-id "echo"))
           (train (find :train (expert-eval-suites domain)
                        :key #'eval:eval-dataset-role)))
      (ok (eq :train (eval:eval-dataset-role new)))
      (ok (eq :train (eval:eval-dataset-role train)))
      (ok (= hold-n (length (eval:eval-dataset-cases
                             (find :holdout (expert-eval-suites domain)
                                   :key #'eval:eval-dataset-role)))))
      (let ((case (car (last (eval:eval-dataset-cases train)))))
        (ok (eq :train (eval:eval-case-role case)))
        (ok (eq :human-feedback (eval:eval-case-source case)))))))

(deftest disjoint-holdout-is-used-for-promotion
  (let* ((train (eval:make-eval-dataset
                 :name "train" :role :train
                 :cases (list (eval:make-eval-case
                               :input "search" :expected "echo: search"
                               :role :train))))
         (holdout (eval:make-eval-dataset
                   :name "hold" :role :holdout
                   :cases (list (eval:make-eval-case
                                 :input "hi" :expected "echo: hi"
                                 :role :holdout))))
         (domain (make-expert-domain
                  :name "roles"
                  :ks-set (list (%script-ks 'echo
                                            (lambda (in)
                                              (format nil "old: ~a" in))))
                  :eval-suites (list train holdout)
                  :profile :personal))
         (result (run-improvement-cycle
                  domain
                  :target (first (expert-ks-set domain))
                  :llm (%revision-llm "echo: ")
                  :journal (task:make-in-memory-journal)
                  :cycle-id "roles"
                  :activity-floor 0)))
    (ok (eq :promote (getf result :verdict)))
    (ok (= 1 (getf result :candidate-score)))
    (ok (= 0 (getf result :baseline-score)))))
