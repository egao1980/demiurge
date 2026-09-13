(in-package #:demiurge/tests)

(defun %span-named (spans name)
  (find name spans :key #'tel:telemetry-span-name :test #'equal))

(defun %metrics-named (metrics name)
  (remove name metrics :key #'tel:telemetry-metric-name :test-not #'equal))

(deftest echo-expert-emits-taxonomy-spans
  (let* ((log-stream (make-string-output-stream))
         (backend (apply-personal-observability :stream log-stream))
         (domain (make-echo-expert :backend (llm:make-mock-llm-backend)
                                   :name "echo-obs"))
         (board (run-expert domain :trigger '(:prompt "hi")))
         (spans (tel:recorded-spans backend))
         (ksar (%span-named spans +span-ksar-execute+))
         (agent (%span-named spans +span-agent-run+))
         (log (get-output-stream-string log-stream)))
    (ok (equal "echo: hi" (bb:read-section board :result)))
    (ok ksar)
    (ok agent)
    (ok (equal (tel:telemetry-span-trace-id ksar)
               (tel:telemetry-span-trace-id agent)))
    (ok (search (tel:telemetry-span-trace-id ksar) log))
    (ok (search "trace-id=" log))))

(deftest readyz-503-when-journal-closed
  (let* ((journal (task:make-in-memory-journal))
         (rag (rag:make-mock-vector-store))
         (llm (llm:make-mock-llm-backend))
         (ok-st (readyz-status :journal journal
                               :rag-store rag
                               :llm-backend llm)))
    (ok (= 200 (getf (healthz-status) :http-status)))
    (ok (= 200 (first (healthz-response))))
    (ok (= 200 (getf ok-st :http-status)))
    (ok (eq :ok (getf ok-st :status)))
    (close-journal-store journal)
    (ok (journal-store-closed-p journal))
    (let ((st (readyz-status :journal journal
                             :rag-store rag
                             :llm-backend llm))
          (resp (readyz-response :journal journal
                                 :rag-store rag
                                 :llm-backend llm)))
      (ok (= 503 (getf st :http-status)))
      (ok (eq :degraded (getf st :status)))
      (ok (member :journal (getf st :components)))
      (ok (= 503 (first resp)))
      (ok (search "journal" (first (third resp)))))))

(deftest metric-counter-increments-on-mock-generate
  (let* ((backend (apply-personal-observability
                   :stream (make-broadcast-stream)))
         (before (length (%metrics-named (tel:recorded-metrics backend)
                                         +metric-llm-tokens+)))
         (response (observe-generate (llm:make-mock-llm-backend)
                                     "hi"
                                     :expert "echo"
                                     :scope "test"))
         (tokens (%metrics-named (tel:recorded-metrics backend)
                                 +metric-llm-tokens+))
         (latency (%metrics-named (tel:recorded-metrics backend)
                                  +metric-llm-latency+)))
    (ok (llm:llm-response-p response))
    (ok (> (length tokens) before))
    (ok (plusp (tel:telemetry-metric-value (first tokens))))
    (ok (plusp (length latency)))))
