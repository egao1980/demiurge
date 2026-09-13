(in-package #:demiurge/observe)

;;; Versioned like an API. Names are the contract; instruments are registered
;;; on the telemetry-protocol 0.2 provider at profile init.

(defparameter +span-ksar-execute+ "demiurge.ksar.execute"
  "Span around one knowledge-source activation record (KSAR) execute.")

(defparameter +span-agent-run+ "demiurge.agent.run"
  "Span around one ai-agent-protocol RUN-AI-AGENT invocation.")

(defparameter +span-task-step+ "demiurge.task.step"
  "Span around one durable task-protocol step.")

(defparameter +span-ingest-document+ "demiurge.ingest.document"
  "Span around ingest of a single document (extract → chunk → embed → store).")

(defparameter +span-improve-cycle+ "demiurge.improve.cycle"
  "Span around one self-improvement cycle (select → trial → gate → verdict).")

(defparameter +taxonomy-span-names+
  (list +span-ksar-execute+
        +span-agent-run+
        +span-task-step+
        +span-ingest-document+
        +span-improve-cycle+)
  "All versioned demiurge span names.")

(defparameter +metric-llm-tokens+ "demiurge.llm.tokens"
  "Counter of LLM tokens. Attributes: demiurge.expert, demiurge.scope.")

(defparameter +metric-llm-cost+ "demiurge.llm.cost"
  "Counter of LLM spend (currency units). Attributes: demiurge.expert, demiurge.scope.")

(defparameter +metric-ksar-duration+ "demiurge.ksar.duration"
  "Histogram of KSAR execute wall time in seconds. Attribute: demiurge.ks.")

(defparameter +metric-llm-latency+ "demiurge.llm.latency"
  "Histogram of LLM generate latency in seconds. Attributes: demiurge.expert, demiurge.scope.")

(defparameter +metric-eval-score+ "demiurge.eval.score"
  "Gauge of the latest eval-protocol mean score per knowledge source. Attribute: demiurge.ks.")

(defparameter +metric-task-queue-depth+ "demiurge.task.queue-depth"
  "Gauge of durable-task queue depth (pending + running).")

(defparameter +metric-ingest-documents+ "demiurge.ingest.documents"
  "Counter of ingested documents. Attribute: demiurge.expert.")

(defparameter +metric-improve-promotions+ "demiurge.improve.promotions"
  "Counter of improvement-cycle promotions.")

(defparameter +metric-improve-demotions+ "demiurge.improve.demotions"
  "Counter of improvement-cycle demotions.")

(defparameter +latency-boundaries+
  '(0.005d0 0.01d0 0.025d0 0.05d0 0.1d0 0.25d0 0.5d0 1d0 2.5d0 5d0 10d0)
  "Histogram bucket upper bounds in seconds.")

(defparameter +taxonomy-metric-specs+
  (list (list +metric-llm-tokens+ :counter
              :unit "token"
              :description "LLM tokens consumed (A2 budget accounting).")
        (list +metric-llm-cost+ :counter
              :unit "1"
              :description "LLM cost in catalog currency units.")
        (list +metric-ksar-duration+ :histogram
              :unit "s"
              :description "KSAR execute duration."
              :boundaries +latency-boundaries+)
        (list +metric-llm-latency+ :histogram
              :unit "s"
              :description "LLM generate latency."
              :boundaries +latency-boundaries+)
        (list +metric-eval-score+ :gauge
              :unit "1"
              :description "Latest eval mean score per KS.")
        (list +metric-task-queue-depth+ :gauge
              :unit "1"
              :description "Durable task queue depth.")
        (list +metric-ingest-documents+ :counter
              :unit "1"
              :description "Documents ingested.")
        (list +metric-improve-promotions+ :counter
              :unit "1"
              :description "Improvement-cycle promotions.")
        (list +metric-improve-demotions+ :counter
              :unit "1"
              :description "Improvement-cycle demotions."))
  "Instrument registry specs: (NAME KIND &key UNIT DESCRIPTION BOUNDARIES).")

(defun register-taxonomy-instruments
    (&optional (provider (tel:current-tracer-provider)))
  "Create-or-return every taxonomy instrument on PROVIDER."
  (dolist (spec +taxonomy-metric-specs+ provider)
    (destructuring-bind (name kind &key unit description boundaries) spec
      (tel:get-instrument provider name kind
                          :unit unit
                          :description description
                          :boundaries boundaries))))

(defun %instrument (name kind &key unit description boundaries)
  (tel:get-instrument (tel:current-tracer-provider) name kind
                      :unit unit :description description
                      :boundaries boundaries))

(defun %elapsed-seconds (start-internal)
  (max 0d0
       (float (/ (- (get-internal-real-time) start-internal)
                 internal-time-units-per-second)
              1d0)))

(defun record-llm-usage (tokens &key (cost 0) latency attributes
                                  (expert "unknown") (scope "default"))
  "Increment token/cost counters and optionally the latency histogram."
  (let ((attrs (or attributes
                   (list "demiurge.expert" (string expert)
                         "demiurge.scope" (string scope)))))
    (tel:record (%instrument +metric-llm-tokens+ :counter :unit "token")
                (or tokens 0) :attributes attrs)
    (tel:record (%instrument +metric-llm-cost+ :counter :unit "1")
                (or cost 0) :attributes attrs)
    (when latency
      (tel:record (%instrument +metric-llm-latency+ :histogram
                               :unit "s" :boundaries +latency-boundaries+)
                  latency :attributes attrs))
    tokens))

(defun record-ksar-duration (ks start-internal)
  "Record KSAR wall time since START-INTERNAL on the duration histogram."
  (let ((attrs (list "demiurge.ks"
                     (if (and ks (typep ks 'bb:knowledge-source))
                         (string (bb:ks-name ks))
                         (string ks)))))
    (tel:record (%instrument +metric-ksar-duration+ :histogram
                             :unit "s" :boundaries +latency-boundaries+)
                (%elapsed-seconds start-internal)
                :attributes attrs)))

(defun record-eval-score (ks score)
  (tel:record (%instrument +metric-eval-score+ :gauge :unit "1")
              score
              :attributes (list "demiurge.ks" (string ks))))

(defun record-queue-depth (depth)
  (tel:record (%instrument +metric-task-queue-depth+ :gauge :unit "1")
              depth))

(defun record-ingest-document (&key (count 1) (expert "unknown"))
  (tel:record (%instrument +metric-ingest-documents+ :counter :unit "1")
              count
              :attributes (list "demiurge.expert" (string expert))))

(defun record-promotion (&key cycle-id eval-run-id)
  (tel:record (%instrument +metric-improve-promotions+ :counter :unit "1")
              1
              :attributes (list "demiurge.cycle-id" cycle-id
                                "demiurge.eval-run-id" eval-run-id)))

(defun record-demotion (&key cycle-id eval-run-id)
  (tel:record (%instrument +metric-improve-demotions+ :counter :unit "1")
              1
              :attributes (list "demiurge.cycle-id" cycle-id
                                "demiurge.eval-run-id" eval-run-id)))

(defun observe-generate (backend turns &rest args
                         &key expert scope &allow-other-keys)
  "LLM:GENERATE + token/cost/latency taxonomy metrics."
  (let* ((pass (loop for (k v) on args by #'cddr
                     unless (member k '(:expert :scope) :test #'eq)
                       collect k and collect v))
         (start (get-internal-real-time))
         (response (apply #'llm:generate backend turns pass))
         (usage (and (llm:llm-response-p response)
                     (llm:llm-response-usage response)))
         (tokens (or (and usage (llm:llm-usage-total-tokens usage))
                     (and usage
                          (+ (or (llm:llm-usage-input-tokens usage) 0)
                             (or (llm:llm-usage-output-tokens usage) 0)))
                     0)))
    (record-llm-usage tokens
                      :latency (%elapsed-seconds start)
                      :expert (or expert "unknown")
                      :scope (or scope "default"))
    response))
