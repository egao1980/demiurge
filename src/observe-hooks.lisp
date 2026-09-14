(in-package #:demiurge)

;;; Spans live on telemetry-protocol. Logs carry :trace-id / :span-id
;;; via log-protocol:with-context. Never emit spans through the logger.
;;; Names match demiurge/observe taxonomy constants.

(defun %observe-span-name (which fallback)
  (let* ((pkg (find-package '#:demiurge/observe))
         (sym (and pkg (find-symbol which pkg))))
    (if (and sym (boundp sym))
        (symbol-value sym)
        fallback)))

(defun %observe-record (name &rest args)
  (let* ((pkg (find-package '#:demiurge/observe))
         (fn (and pkg (find-symbol name pkg))))
    (when (and fn (fboundp fn))
      (apply fn args))))

(defun record-agenda-depth (bb)
  "Gauge demiurge.task.queue-depth from BLACKBOARD's agenda size."
  (when bb
    (%observe-record "RECORD-QUEUE-DEPTH" (bb:agenda-size bb))))

(defun %usage-tokens (usage)
  (or (and usage (llm:llm-usage-total-tokens usage))
      (and usage
           (+ (or (llm:llm-usage-input-tokens usage) 0)
              (or (llm:llm-usage-output-tokens usage) 0)))
      0))

(defun %observe-llm-usage (usage &key (expert "unknown") (scope "default")
                                  (cost 0) latency)
  (%observe-record "RECORD-LLM-USAGE" (%usage-tokens usage)
                   :cost (or cost 0)
                   :latency latency
                   :expert expert
                   :scope (if (stringp scope)
                              scope
                              (princ-to-string scope))))

(defclass observe-accounting-policy (llm:routing-policy)
  ((inner :initarg :inner :accessor observe-accounting-policy-inner
          :initform nil)
   (expert :initarg :expert :accessor observe-accounting-policy-expert
           :initform "unknown")
   (on-usage :initarg :on-usage :accessor observe-accounting-policy-on-usage
             :initform nil))
  (:documentation
   "Forwards SELECT-BACKEND / RECORD-USAGE / RECORD-LATENCY to INNER.
    RECORD-USAGE is the A2 accounting hook that invokes observe."))

(defun observe-accounting-policy-p (x)
  (typep x 'observe-accounting-policy))

(defun make-observe-accounting-policy (&key inner (expert "unknown") on-usage)
  (make-instance 'observe-accounting-policy
                 :inner inner :expert expert :on-usage on-usage))

(defmethod llm:select-backend ((policy observe-accounting-policy) request
                               candidates)
  (llm:select-backend (or (observe-accounting-policy-inner policy) policy)
                      request candidates))

(defmethod llm:record-latency ((policy observe-accounting-policy) backend
                               seconds)
  (llm:record-latency (observe-accounting-policy-inner policy) backend seconds))

(defmethod llm:record-usage ((policy observe-accounting-policy) scope usage
                             &key backend model)
  (llm:record-usage (observe-accounting-policy-inner policy) scope usage
                    :backend backend :model model)
  (let ((cb (observe-accounting-policy-on-usage policy)))
    (when cb
      (funcall cb usage :scope scope :backend backend :model model
               :expert (observe-accounting-policy-expert policy))))
  (%observe-llm-usage usage
                      :expert (observe-accounting-policy-expert policy)
                      :scope (or scope "default"))
  usage)

(defun bare-llm-backend (llm)
  "Unwrap one llm-router-backend layer so wrap-llm-observe does not nest."
  (if (llm:llm-router-backend-p llm)
      (or (first (llm:llm-router-candidates llm)) llm)
      llm))

(defun wrap-llm-observe (llm &key budget scope (expert "unknown") on-usage)
  "Router whose budget-policy inner records llm-usage via observe.
   BUDGET NIL still wraps (unlimited ledger) so generate is metered once."
  (let* ((bare (bare-llm-backend llm))
         (fallback (llm:make-fallback-chain-policy :candidates (list bare)))
         (acct (make-observe-accounting-policy
                :inner fallback
                :expert expert
                :on-usage on-usage)))
    (llm:make-llm-router-backend
     :policy (llm:make-budget-policy :inner acct :budget budget)
     :candidates (list bare)
     :scope (or scope "default"))))

(defun call-with-ksar-observe (ks thunk)
  "Span demiurge.ksar.execute around THUNK. Log with correlated ids."
  (let ((start (get-internal-real-time)))
    (tel:with-span ((%observe-span-name "+SPAN-KSAR-EXECUTE+"
                                        "demiurge.ksar.execute")
                    :attributes (list "demiurge.ks"
                                      (string (bb:ks-name ks))))
      (log:with-context (:trace-id (tel:current-trace-id)
                         :span-id (tel:current-span-id)
                         :ks (bb:ks-name ks))
        (when log:*log-backend*
          (log:info "ksar execute"))
        (prog1 (funcall thunk)
          (%observe-record "RECORD-KSAR-DURATION" ks start))))))

(defun call-with-agent-observe (agent thunk)
  "Span demiurge.agent.run around THUNK. Log with correlated ids."
  (tel:with-span ((%observe-span-name "+SPAN-AGENT-RUN+"
                                      "demiurge.agent.run")
                  :attributes (list "demiurge.agent"
                                    (if (and agent (agent:ai-agent-p agent))
                                        (agent:ai-agent-name agent)
                                        "agent")))
    (log:with-context (:trace-id (tel:current-trace-id)
                       :span-id (tel:current-span-id)
                       :agent (and agent (agent:ai-agent-name agent)))
      (when log:*log-backend*
        (log:info "agent run"))
      (funcall thunk))))
