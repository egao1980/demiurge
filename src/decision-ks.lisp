(in-package #:demiurge)

;;; Sibling of agent-ks. One shared STATE from watched sections, one
;;; declared question batch, full-mass answers written back. Does not
;;; call run-ai-agent. Concentration is never stored as P(correct).

(defclass decision-ks (bb:knowledge-source)
  ((backend :initarg :backend :accessor decision-ks-backend :initform nil)
   (watch :initarg :watch :accessor decision-ks-watch :initform '(:state))
   (questions :initarg :questions :accessor decision-ks-questions :initform nil)
   (questions-fn :initarg :questions-fn :accessor decision-ks-questions-fn
                 :initform nil)
   (model :initarg :model :accessor decision-ks-model :initform :kev-latest)
   (result-key :initarg :result-key :accessor decision-ks-result-key
               :initform :decision)
   (answer-key-fn :initarg :answer-key-fn :accessor decision-ks-answer-key-fn
                  :initform nil)))

(defun decision-ks-p (object)
  (typep object 'decision-ks))

(defun make-decision-ks (&key name backend (watch '(:state)) questions
                           questions-fn (model :kev-latest)
                           (result-key :decision) answer-key-fn
                           (priority 0) (version "0.1.0"))
  (make-instance 'decision-ks
                 :name name
                 :backend backend
                 :watch (copy-list watch)
                 :questions questions
                 :questions-fn questions-fn
                 :model model
                 :result-key result-key
                 :answer-key-fn answer-key-fn
                 :priority priority
                 :version version))

(defmethod ks-watch-keys ((ks decision-ks))
  (copy-list (decision-ks-watch ks)))

(defmethod bb:ks-precondition ((ks decision-ks) blackboard)
  (every (lambda (key) (bb:section-bound-p blackboard key))
         (decision-ks-watch ks)))

(defun serialize-decision-state (blackboard watch)
  "Serialize WATCHED sections once. A lone string section is used as-is."
  (let ((keys watch)
        (values (mapcar (lambda (k)
                          (and (bb:section-bound-p blackboard k)
                               (bb:read-section blackboard k)))
                        watch)))
    (cond
      ((and (= 1 (length keys))
            (stringp (first values)))
       (first values))
      (t
       (with-output-to-string (s)
         (loop for key in keys
               for value in values
               do (format s "~a:~a~%" key value)))))))

(defun decision-answer-section-key (question-id)
  (intern (format nil "DECISION/~A" question-id) :keyword))

(defun make-decision-record (result answer)
  "Plist journaled to the board. Full mass; no concentration-as-accuracy."
  (let* ((dist (dec:decision-answer-distribution answer))
         (usage (dec:decision-result-usage result)))
    (list :question-id (dec:question-id (dec:decision-answer-question answer))
          :mass (dec:distribution-mass dist)
          :winner (dec:distribution-winner dist)
          :score (dec:decision-answer-score answer)
          :model (dec:decision-result-model result)
          :usage (list :input-tokens (and usage (dec:decision-usage-input-tokens usage))
                       :output-tokens (and usage (dec:decision-usage-output-tokens usage))))))

(defun decision-record-mass (record)
  (getf record :mass))

(defun decision-record-model (record)
  (getf record :model))

(defun make-decision-section-lookup (blackboard section-key)
  "LOOKUP for capability decision-interceptor: mass + resolved model."
  (lambda (invocation)
    (declare (ignore invocation))
    (let ((rec (bb:read-section blackboard section-key)))
      (values (decision-record-mass rec)
              (decision-record-model rec)))))

(defun %decision-questions (ks blackboard)
  (cond
    ((functionp (decision-ks-questions-fn ks))
     (funcall (decision-ks-questions-fn ks) blackboard))
    ((decision-ks-questions ks)
     (decision-ks-questions ks))
    (t
     (error 'demiurge-error
            :message (format nil "decision-ks ~s has no questions"
                             (bb:ks-name ks))))))

(defun %decision-backend (ks)
  (or (decision-ks-backend ks)
      dec:*decision-backend*
      (error 'demiurge-error
             :message (format nil "decision-ks ~s has no backend"
                              (bb:ks-name ks)))))

(defun %execute-decision-ks (ks blackboard)
  (let* ((backend (%decision-backend ks))
         (state (serialize-decision-state blackboard (decision-ks-watch ks)))
         (questions (%decision-questions ks blackboard))
         (request (dec:make-decision-request
                   :state state
                   :questions questions
                   :model (decision-ks-model ks)))
         (result (dec:decide backend request))
         (records (mapcar (lambda (answer) (make-decision-record result answer))
                          (dec:decision-result-answers result)))
         (key-fn (or (decision-ks-answer-key-fn ks)
                     (lambda (rec)
                       (decision-answer-section-key (getf rec :question-id))))))
    (bb:write-section blackboard
                     (decision-ks-result-key ks)
                     (list :model (dec:decision-result-model result)
                           :answers records
                           :usage (list :input-tokens
                                        (dec:decision-usage-input-tokens
                                         (dec:decision-result-usage result))
                                        :output-tokens
                                        (dec:decision-usage-output-tokens
                                         (dec:decision-result-usage result)))))
    (dolist (rec records)
      (bb:write-section blackboard (funcall key-fn rec) rec))
    result))

(defmethod bb:ks-execute ((ks decision-ks) blackboard)
  (call-with-ksar-observe ks
    (lambda ()
      (call-with-durable-ksar blackboard ks
                              (lambda ()
                                (%execute-decision-ks ks blackboard))))))

(defmethod bb:ks-postcondition ((ks decision-ks) blackboard result)
  (declare (ignore result))
  (bb:section-bound-p blackboard (decision-ks-result-key ks)))
