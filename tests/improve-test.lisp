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
