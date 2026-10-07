(in-package #:demiurge/tests)

(defun %risk-question ()
  (dec:make-choice-question
   :id :risk
   :instructions "Allow this effect?"
   :criteria '((:allow . "proceed") (:deny . "stop"))))

(defun %decision-backend ()
  (dec:make-mock-decision-backend
   :answers '((:risk . ((:allow . 4/5) (:deny . 1/5))))))

(deftest decision-ks-writes-full-mass
  (let* ((backend (%decision-backend))
         (ks (make-decision-ks :name 'risk
                               :backend backend
                               :watch '(:state)
                               :questions (list (%risk-question))
                               :model :kev-latest))
         (bb (bb:make-blackboard)))
    (bb:register-ks bb ks :requires (ks-watch-keys ks))
    (bb:write-section bb :state "tenant=acme")
    (drain bb)
    (let* ((rec (bb:read-section bb :decision/risk))
           (bundle (bb:read-section bb :decision))
           (mass (decision-record-mass rec)))
      (ok (equal "kev-4b" (decision-record-model rec)))
      (ok (equal "kev-4b" (getf bundle :model)))
      (ok (= 4/5 (cdr (assoc :allow mass))))
      (ok (= 1/5 (cdr (assoc :deny mass))))
      (ok (eq :allow (getf rec :winner)))
      (ng (getf rec :confidence))
      (ng (getf rec :concentration)))))

(deftest decision-ks-does-not-run-ai-agent
  (let* ((backend (%decision-backend))
         (ks (make-decision-ks :name 'risk
                               :backend backend
                               :questions (list (%risk-question))))
         (bb (bb:make-blackboard))
         (result (progn
                   (bb:write-section bb :state "x")
                   (bb:ks-execute ks bb))))
    (ok (dec:decision-result-p result))
    (ok (bb:section-bound-p bb :decision))
    (ok (eq :absent (bb:read-section bb :result :default :absent)))))

(deftest decision-ks-lookup-feeds-interceptor
  (let* ((backend (%decision-backend))
         (ks (make-decision-ks :name 'risk
                               :backend backend
                               :questions (list (%risk-question))))
         (bb (bb:make-blackboard)))
    (bb:register-ks bb ks :requires (ks-watch-keys ks))
    (bb:write-section bb :state "tenant=acme")
    (drain bb)
    (let ((lookup (make-decision-section-lookup bb :decision/risk)))
      (multiple-value-bind (mass model) (funcall lookup nil)
        (ok (equal "kev-4b" model))
        (ok (= 4/5 (cdr (assoc :allow mass)))))
      (let* ((pkg (find-package :capability-protocol))
             (make (and pkg (find-symbol "MAKE-DECISION-INTERCEPTOR" pkg)))
             (pre (and pkg (find-symbol "INTERCEPTOR-PRE" pkg)))
             (kind (and pkg (find-symbol "DECISION-KIND" pkg))))
        (when (and make pre kind (fboundp make) (fboundp pre) (fboundp kind))
          (let* ((journal nil)
                 (i (funcall make
                             :question-id :risk
                             :outcome-key :allow
                             :deny-at 0.9
                             :ask-at 0.5
                             :lookup lookup
                             :on-decide (lambda (&rest args) (push args journal))))
                 (decision (funcall pre i nil)))
            (ok (eq :ask (funcall kind decision)))
            (ok (eq :ask (getf (first journal) :kind)))
            (ok (equal "kev-4b" (getf (first journal) :model)))
            (ng (find :concentration (first journal)))))))))
