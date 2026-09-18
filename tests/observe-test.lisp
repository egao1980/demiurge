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

(deftest echo-expert-records-ksar-duration-and-queue-depth
  (let* ((backend (apply-personal-observability
                   :stream (make-broadcast-stream)))
         (domain (make-echo-expert :backend (llm:make-mock-llm-backend)
                                   :name "echo-metrics"))
         (board (run-expert domain :trigger '(:prompt "hi")))
         (duration (%metrics-named (tel:recorded-metrics backend)
                                   +metric-ksar-duration+))
         (depth (%metrics-named (tel:recorded-metrics backend)
                                +metric-task-queue-depth+)))
    (ok (equal "echo: hi" (bb:read-section board :result)))
    (ok (plusp (length duration))
        "demiurge.ksar.duration recorded for the echo KSAR")
    (ok (plusp (length depth))
        "demiurge.task.queue-depth recorded on enqueue/dequeue")
    (ok (numberp (tel:telemetry-metric-value (first duration))))
    (ok (>= (tel:telemetry-metric-value (first duration)) 0))
    (ok (find-if (lambda (m) (plusp (tel:telemetry-metric-value m)))
                 depth)
        "queue-depth snapshot includes a non-zero sample")
    (ok (find-if (lambda (m)
                   (equal "ok"
                          (loop for (k v) on (tel:telemetry-metric-attributes m)
                                by #'cddr
                                when (equal k "demiurge.outcome") return v)))
                 duration)
        "successful KSAR records outcome=ok")))

(deftest ingest-and-improve-record-taxonomy-counters
  (let* ((backend (apply-personal-observability
                   :stream (make-broadcast-stream))))
    (with-tmp-dir (tmp)
      (let* ((dir (ensure-directories-exist (merge-pathnames "obs/" tmp)))
             (source (progn
                       (with-open-file (out (merge-pathnames "a.md" dir)
                                            :direction :output
                                            :if-exists :supersede)
                         (write-string "# A" out)
                         (terpri out)
                         (write-string "alpha" out))
                       (make-file-source :root dir :pattern "*.md")))
             (domain (make-expert-domain :name "obs-ingest")))
        (run-ingest domain source
                    :store (rag:make-mock-vector-store)
                    :journal (task:make-in-memory-journal)
                    :task-id "obs-ingest"
                    :embedder (mock-llm))
        (ok (plusp (length (%metrics-named (tel:recorded-metrics backend)
                                           +metric-ingest-documents+))))))
    (let* ((cases (list (eval:make-eval-case :input "hi" :expected "echo: hi")))
           (domain (make-expert-domain
                    :name "obs-improve"
                    :ks-set (list (make-instance
                                   'script-ks :name 'echo
                                   :fn (lambda (in)
                                         (format nil "old: ~a" in))))
                    :eval-suites (list (eval:make-eval-dataset
                                        :name "obs"
                                        :cases cases))))
           (result (run-improvement-cycle
                    domain
                    :target (first (expert-ks-set domain))
                    :llm (llm:make-mock-llm-backend
                          :handler (lambda (b turns &key &allow-other-keys)
                                     (declare (ignore b turns))
                                     (llm:make-llm-response
                                      :parts (list (llm:make-llm-text-part
                                                    :text "ok"))
                                      :output (make-ks-revision
                                               :skill-text "echo: "))))
                    :journal (task:make-in-memory-journal)
                    :cycle-id "obs-improve"
                    :activity-floor 0)))
      (ok (eq :promote (getf result :verdict)))
      (ok (plusp (length (%metrics-named (tel:recorded-metrics backend)
                                         +metric-eval-score+))))
      (ok (plusp (length (%metrics-named (tel:recorded-metrics backend)
                                         +metric-improve-promotions+)))))))
