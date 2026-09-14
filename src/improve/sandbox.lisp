(in-package #:demiurge/improve)

(defparameter *side-effecting-operations*
  '(write-file run-command send-message)
  "Default operation names treated as :SIDE-EFFECTING when untagged.")

(defvar *operation-tags* (make-hash-table :test 'equal)
  "Keys (CAP-NAME . OP-NAME), values a list of keyword tags.")

(defun %op-key (cap-name op-name)
  (cons (intern (string-upcase (string cap-name)) :keyword)
        (intern (string-upcase (string op-name)) :keyword)))

(defun tag-operation (cap-name op-name &rest tags)
  "Associate TAGS (e.g. :SIDE-EFFECTING) with CAP-NAME / OP-NAME."
  (let ((key (%op-key cap-name op-name)))
    (setf (gethash key *operation-tags*)
          (union tags (gethash key *operation-tags*) :test #'eq))
    (gethash key *operation-tags*)))

(defun operation-tags (cap-name op-name)
  (copy-list (gethash (%op-key cap-name op-name) *operation-tags*)))

(defun side-effecting-operation-p (cap-name op-name)
  (or (member :side-effecting (operation-tags cap-name op-name) :test #'eq)
      (member op-name *side-effecting-operations*
              :test (lambda (a b) (string-equal (string a) (string b))))))

(defclass restricted-capability (cap:capability)
  ((inner :initarg :inner :reader restricted-capability-inner)
   (removed :initarg :removed :reader restricted-capability-removed
            :initform nil)
   (record :initarg :record :accessor restricted-capability-record
           :initform nil)))

(defmethod cap:capability-operations ((cap restricted-capability))
  (remove-if (lambda (op)
               (member (cap:capability-operation-name op)
                       (restricted-capability-removed cap)
                       :test (lambda (a b)
                               (string-equal (string a) (string b)))))
             (copy-list (cap:capability-operations
                         (restricted-capability-inner cap)))))

(defmethod cap:invoke-operation ((cap restricted-capability) op-name &rest args)
  (let ((removed (member op-name (restricted-capability-removed cap)
                         :test (lambda (a b)
                                 (string-equal (string a) (string b))))))
    (when removed
      (let ((rec (restricted-capability-record cap)))
        (when rec
          (push (list :capability (cap:capability-name cap)
                      :op op-name
                      :args args)
                (cdr rec))))
      (restart-case
          (error 'cap:unknown-operation :capability cap :name op-name)
        (use-value (value)
          :report "Use a supplied stub result"
          (return-from cap:invoke-operation value))
        (skip ()
          :report "Skip the removed operation"
          (return-from cap:invoke-operation nil))))
    (apply #'cap:invoke-operation
           (restricted-capability-inner cap) op-name args)))

(defclass restricted-catalogue (cap:capability-catalogue)
  ((recordings :initform (cons :recordings nil)
               :accessor restricted-catalogue-record-cell)
   (source :initarg :source :reader restricted-catalogue-source
           :initform nil)))

(defun restricted-catalogue-p (x)
  (typep x 'restricted-catalogue))

(defun restricted-catalogue-recordings (catalogue)
  (copy-list (cdr (restricted-catalogue-record-cell catalogue))))

(defun %wrap-restricted (inner record)
  (let ((cap-name (cap:capability-name inner))
        (removed nil))
    (dolist (op (cap:capability-operations inner))
      (when (side-effecting-operation-p cap-name
                                        (cap:capability-operation-name op))
        (push (cap:capability-operation-name op) removed)))
    (make-instance 'restricted-capability
                   :name cap-name
                   :version (cap:capability-version inner)
                   :description (cap:capability-description inner)
                   :inner inner
                   :removed removed
                   :record record)))

(defun make-restricted-catalogue (catalogue)
  "Fresh catalogue copy minus :SIDE-EFFECTING ops, plus recording stubs.
   Never mutates CATALOGUE (root-global caps gotcha)."
  (check-type catalogue cap:capability-catalogue)
  (let* ((copy (make-instance 'restricted-catalogue
                              :name (cap:catalogue-name catalogue)
                              :description (cap:catalogue-description catalogue)
                              :defined-names (copy-list
                                              (cap:catalogue-defined-names
                                               catalogue))
                              :source catalogue))
         (record (restricted-catalogue-record-cell copy)))
    (dolist (row (cap:list-capabilities catalogue))
      (let ((inner (cap:get-capability catalogue (getf row :name))))
        (when inner
          (cap:register-capability copy (%wrap-restricted inner record)))))
    copy))

(defun call-with-wall-clock (seconds thunk)
  "Run THUNK; interrupt the worker if SECONDS elapses. NIL seconds = no limit."
  (if (or (null seconds) (not (plusp seconds)))
      (funcall thunk)
      (let ((out nil)
            (err nil)
            (done nil)
            (lock (bt2:make-lock)))
        (let ((th (bt2:make-thread
                   (lambda ()
                     (handler-case (setf out (funcall thunk))
                       (error (c) (setf err c)))
                     (bt2:with-lock-held (lock)
                       (setf done t)))
                   :name "demiurge-improve-trial")))
          (loop repeat (max 1 (ceiling (* seconds 10)))
                until (bt2:with-lock-held (lock) done)
                do (sleep 0.1))
          (unless (bt2:with-lock-held (lock) done)
            (when (bt2:thread-alive-p th)
              (bt2:interrupt-thread
               th
               (lambda () (error 'trial-timeout :seconds seconds))))
            (ignore-errors (bt2:join-thread th))
            (error 'trial-timeout :seconds seconds))
          (ignore-errors (bt2:join-thread th))
          (if err (error err) out)))))

(defun compute-granted-p (domain)
  "T when DOMAIN's catalogue has a registered :COMPUTE capability."
  (let ((cat (and (expert-domain-p domain) (expert-catalogue domain))))
    (and cat (cap:get-capability cat :compute) t)))

(defun run-sandboxed-candidate (domain spec)
  "Run SPEC through compute-protocol RUN-SANDBOXED, or refuse.
   Profiles without :COMPUTE signal COMPUTE-DENIED."
  (unless (compute-granted-p domain)
    (error 'compute-denied
           :message "profile has not granted :compute"))
  (let* ((pkg (find-package '#:compute-protocol))
         (run (and pkg (find-symbol "RUN-SANDBOXED" pkg)))
         (star (and pkg (find-symbol "*COMPUTE-BACKEND*" pkg)))
         (backend (and star (boundp star) (symbol-value star))))
    (unless (and run (fboundp run) backend)
      (error 'compute-denied
             :message "compute-protocol is not loaded or has no backend"))
    (funcall run backend spec)))

(defun improve-budget-scope (cycle-id)
  "A2 budget-policy scope for an improvement cycle.
   When *TENANT* is bound, the scope is tenant-prefixed (C4)."
  (if (current-tenant)
      (tenant-budget-scope :improve cycle-id)
      (list :improve cycle-id)))

(defun wrap-llm-budget (llm cycle-id &key budget)
  "Wrap LLM in a router budget-policy scoped to (:IMPROVE CYCLE-ID).
   BUDGET NIL leaves LLM unchanged."
  (if (null budget)
      llm
      (llm:make-llm-router-backend
       :policy (llm:make-budget-policy
                :inner (llm:make-fallback-chain-policy :candidates (list llm))
                :budget budget)
       :candidates (list llm)
       :scope (improve-budget-scope cycle-id))))
