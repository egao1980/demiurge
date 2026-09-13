(defpackage #:demiurge/observe
  (:use #:cl #:demiurge)
  (:nicknames #:stack-demiurge.observe)
  (:local-nicknames (#:bb #:blackboard-protocol)
                    (#:log #:log-protocol)
                    (#:rag #:rag-protocol)
                    (#:llm #:llm-protocol)
                    (#:task #:task-protocol)
                    (#:tel #:telemetry-protocol))
  (:export
   #:observe-error
   #:observe-component-unready
   #:observe-unready-component

   #:+span-ksar-execute+
   #:+span-agent-run+
   #:+span-task-step+
   #:+span-ingest-document+
   #:+span-improve-cycle+
   #:+taxonomy-span-names+

   #:+metric-llm-tokens+
   #:+metric-llm-cost+
   #:+metric-ksar-duration+
   #:+metric-llm-latency+
   #:+metric-eval-score+
   #:+metric-task-queue-depth+
   #:+metric-ingest-documents+
   #:+metric-improve-promotions+
   #:+metric-improve-demotions+
   #:+taxonomy-metric-specs+

   #:register-taxonomy-instruments
   #:record-llm-usage
   #:record-ksar-duration
   #:record-eval-score
   #:record-queue-depth
   #:record-ingest-document
   #:record-promotion
   #:record-demotion
   #:observe-generate

   #:with-observe-log
   #:call-with-task-step-observe
   #:call-with-ingest-observe
   #:call-with-improve-cycle-observe
   #:log-section-write
   #:log-agenda-decision
   #:log-improve-verdict

   #:healthz-status
   #:readyz-status
   #:healthz-response
   #:readyz-response
   #:close-journal-store
   #:journal-store-closed-p
   #:ping-journal
   #:ping-rag-store
   #:probe-llm

   #:*observability-profile*
   #:apply-personal-observability
   #:apply-corporate-observability
   #:dump-observability)
  (:documentation
   "Demiurge observability: versioned span/metric taxonomy, health, profiles."))

(in-package #:demiurge/observe)

(define-condition observe-error (demiurge-error)
  ()
  (:report (lambda (c s)
             (format s "demiurge observe error~@[: ~A~]"
                     (demiurge-error-message c)))))

(define-condition observe-component-unready (observe-error)
  ((component :initarg :component :reader observe-unready-component
              :initform nil))
  (:report (lambda (c s)
             (format s "observe component ~S is not ready~@[: ~A~]"
                     (observe-unready-component c)
                     (demiurge-error-message c)))))
