(in-package #:demiurge/observe)

;;; /healthz = process up. /readyz = journal + RAG + 1-token LLM probe.
;;; B3 mounts HEALTHZ-RESPONSE / READYZ-RESPONSE on the Clack app.

(defvar *closed-journal-stores* (make-hash-table :test 'eq)
  "Journals marked unreachable by CLOSE-JOURNAL-STORE.")

(defun journal-store-closed-p (journal)
  (or (null journal)
      (and (hash-table-p *closed-journal-stores*)
           (gethash journal *closed-journal-stores*))))

(defun close-journal-store (journal)
  "Mark JOURNAL unreachable for readiness probes. Disconnects an SQL journal."
  (when journal
    (setf (gethash journal *closed-journal-stores*) t)
    (let* ((pkg (find-package '#:task-backend-sql))
           (pred (and pkg (find-symbol "SQL-TASK-JOURNAL-P" pkg)))
           (acc (and pkg (find-symbol "SQL-JOURNAL-CONNECTION" pkg))))
      (when (and pred acc (fboundp pred) (fboundp acc) (funcall pred journal))
        (let ((conn (funcall acc journal)))
          (when conn
            (ignore-errors (sql-protocol:disconnect conn)))))))
  journal)

(defun ping-journal (journal)
  "Reachability probe: JOURNAL-TASK-IDS, or unready if closed/missing."
  (when (journal-store-closed-p journal)
    (error 'observe-component-unready
           :component :journal
           :message "journal store is closed"))
  (task:journal-task-ids journal)
  t)

(defun ping-rag-store (store)
  "Reachability probe: QUERY-STORE with a dummy vector.
   Dimension mismatch still means the store answered."
  (unless store
    (error 'observe-component-unready
           :component :rag
           :message "rag store is missing"))
  (handler-case
      (rag:query-store store #(0.0f0) :top-k 1)
    (rag:rag-dimension-mismatch ()
      t))
  t)

(defun probe-llm (backend)
  "1-token GENERATE probe. Dedicated small budget via :max-tokens 1."
  (unless backend
    (error 'observe-component-unready
           :component :llm
           :message "llm backend is missing"))
  (llm:generate backend "ping"
                :settings (llm:make-llm-settings :max-tokens 1)))

(defun %probe-result (component thunk)
  (restart-case
      (handler-case
          (progn
            (funcall thunk)
            (list :name component :ok t))
        (error (c)
          (list :name component :ok nil :reason (princ-to-string c))))
    (continue ()
      :report (lambda (s) (format s "Skip the ~A readiness probe" component))
      (list :name component :ok nil :reason "skipped"))
    (use-value (value)
      :report "Use a supplied probe result"
      (cond
        ((and (consp value) (getf value :name)) value)
        (t (list :name component :ok (and value t)))))))

(defun healthz-status ()
  "Process is up. Always HTTP 200."
  (list :status :ok :http-status 200 :components '()))

(defun readyz-status (&key journal rag-store llm-backend)
  "Journal ping + RAG ping + 1-token LLM probe.
   Degraded responses list failing components and return HTTP 503."
  (let* ((results (list (%probe-result :journal (lambda () (ping-journal journal)))
                        (%probe-result :rag (lambda () (ping-rag-store rag-store)))
                        (%probe-result :llm (lambda () (probe-llm llm-backend)))))
         (failed (mapcar (lambda (r) (getf r :name))
                         (remove-if (lambda (r) (getf r :ok)) results))))
    (if failed
        (list :status :degraded
              :http-status 503
              :components failed
              :details results)
        (list :status :ok
              :http-status 200
              :components '()
              :details results))))

(defun %json-escape (string)
  (with-output-to-string (out)
    (loop for ch across (string string)
          do (case ch
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (#\Newline (write-string "\\n" out))
               (t (write-char ch out))))))

(defun %status-json (status)
  (let ((st (string-downcase (string (getf status :status))))
        (comps (getf status :components)))
    (format nil "{\"status\":~s,\"components\":[~{~s~^,~}]}"
            (%json-escape st)
            (mapcar (lambda (c) (%json-escape (string-downcase (string c))))
                    comps))))

(defun healthz-response (&rest _ignored)
  "Clack-style (STATUS HEADERS BODY) for B3 to mount at /healthz."
  (declare (ignore _ignored))
  (let ((st (healthz-status)))
    (list (getf st :http-status)
          '(:content-type "application/json")
          (list (%status-json st)))))

(defun readyz-response (&key journal rag-store llm-backend)
  "Clack-style (STATUS HEADERS BODY) for B3 to mount at /readyz."
  (let ((st (readyz-status :journal journal
                           :rag-store rag-store
                           :llm-backend llm-backend)))
    (list (getf st :http-status)
          '(:content-type "application/json")
          (list (%status-json st)))))
