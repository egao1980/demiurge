(in-package #:demiurge)

;;; Reference expert: mock (or HTTP) decision backend → full-mass board sections.
;;; Not an LLM agent. No tools, no decode loop.

(defun make-decision-expert (&key backend name (profile :personal)
                               (watch '(:state))
                               questions
                               (model :kev-latest))
  "Tiny decision-plane stub. BACKEND defaults to MAKE-MOCK-DECISION-BACKEND."
  (let ((backend (or backend
                     (dec:make-mock-decision-backend
                      :answers '((:ok . ((:true . 9/10) (:false . 1/10)))
                                 (:risk . ((:allow . 4/5) (:deny . 1/5))))))))
    (make-expert-domain
     :name (or name "decision")
     :catalogue :world
     :ks-set (list (make-decision-ks
                    :name 'decide
                    :backend backend
                    :watch watch
                    :questions (or questions
                                   (list (dec:make-binary-question
                                          :id :ok
                                          :instructions "Proceed?")
                                         (dec:make-choice-question
                                          :id :risk
                                          :instructions "Allow this effect?"
                                          :criteria '((:allow . "proceed")
                                                      (:deny . "stop")))))
                    :model model))
     :profile profile)))

(defun decision-expert (&rest args &key &allow-other-keys)
  (apply #'make-decision-expert args))

(defun run-decision-demo (&optional (stream *standard-output*))
  "Drain a decision-expert board. Returns (values board record)."
  (let* ((domain (make-decision-expert))
         (board (run-expert domain :trigger '(:state "tenant=acme ticket=chargeback")))
         (rec (bb:read-section board :decision/risk))
         (mass (decision-record-mass rec))
         (model (decision-record-model rec)))
    (format stream "~&; decision-ks model=~s mass=~s winner=~s~%"
            model mass (getf rec :winner))
    (assert (equal "kev-4b" model))
    (assert (= 4/5 (cdr (assoc :allow mass))))
    (assert (null (getf rec :confidence)))
    (assert (null (getf rec :concentration)))
    (assert (eq :absent (bb:read-section board :result :default :absent)))
    (let ((pkg (find-package :capability-protocol)))
      (when pkg
        (let ((make (find-symbol "MAKE-DECISION-INTERCEPTOR" pkg))
              (pre (find-symbol "INTERCEPTOR-PRE" pkg))
              (kind (find-symbol "DECISION-KIND" pkg)))
          (when (and make pre kind (fboundp make) (fboundp pre) (fboundp kind))
            (let ((decision (funcall pre
                                     (funcall make
                                              :question-id :risk
                                              :outcome-key :allow
                                              :deny-at 0.9
                                              :ask-at 0.5
                                              :lookup (make-decision-section-lookup
                                                       board :decision/risk))
                                     nil)))
              (format stream "~&; interceptor ~s on p(allow) (not concentration)~%"
                      (funcall kind decision))
              (assert (eq :ask (funcall kind decision))))))))
    (values board rec)))
