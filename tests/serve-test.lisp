(in-package #:demiurge/tests)

(defun %mcp-text (result)
  (let ((content (and (hash-table-p result) (gethash "content" result))))
    (cond
      ((and (vectorp content) (plusp (length content)))
       (or (gethash "text" (aref content 0)) ""))
      ((stringp result) result)
      (t (princ-to-string result)))))

(defun %event-types (events)
  (mapcar #'ag-ui:ag-ui-event-type events))

(deftest echo-expert-mcp-roundtrip
  (let* ((domain (make-echo-expert :backend (mock-llm) :name "echo-mcp"))
         (server (make-expert-mcp-server domain))
         (names (mapcar #'mcp:mcp-tool-name (mcp:list-tools server))))
    (ok (find "ask_expert" names :test #'equal))
    (ok (find "record_feedback" names :test #'equal))
    (let* ((result (mcp:call-tool server "ask_expert"
                                  (mcp:json-object "prompt" "hi")))
           (text (%mcp-text result)))
      (ok (hash-table-p result))
      (ok (search "echo: hi" text))
      (ok (search "feedback-id:" text)))))

(deftest echo-expert-a2a-roundtrip
  (let* ((domain (make-echo-expert :backend (mock-llm) :name "echo-a2a"))
         (board (bb:make-blackboard))
         (task (run-expert-as-a2a-task domain
                                       :blackboard board
                                       :prompt "hi")))
    (ok (eq :completed (a2a:a2a-task-state task)))
    (ok (plusp (length (a2a:a2a-task-artifacts task))))
    (ok (equal "echo: hi" (bb:read-section board :result)))
    (let* ((art (find "result" (a2a:a2a-task-artifacts task)
                      :key #'a2a:a2a-artifact-name :test #'equal))
           (part (and art (first (a2a:a2a-artifact-parts art)))))
      (ok art)
      (ok (equal "echo: hi" (a2a:a2a-part-text part))))))

(deftest echo-expert-ag-ui-roundtrip
  (let* ((domain (make-echo-expert :backend (mock-llm) :name "echo-ag"))
         (events (run-expert-as-ag-ui-events domain "hi"
                                             :thread-id "t1"
                                             :run-id "r1"))
         (types (%event-types events)))
    (ok (equal "RUN_STARTED" (first types)))
    (ok (equal "RUN_FINISHED" (car (last types))))
    (ok (find "STEP_STARTED" types :test #'equal))
    (ok (find "STEP_FINISHED" types :test #'equal))
    (ok (find "STATE_DELTA" types :test #'equal))
    (let ((delta (find-if (lambda (ev)
                            (equal "STATE_DELTA" (ag-ui:ag-ui-event-type ev)))
                          events)))
      (ok delta)
      (let* ((patch (aref (ag-ui:state-delta-patch delta) 0)))
        (ok (equal "add" (ag-ui:param patch "op")))
        (ok (equal "/result" (ag-ui:param patch "path")))
        (ok (equal "echo: hi" (ag-ui:param patch "value")))))
    (let ((json (ag-ui:decode-json
                 (ag-ui:encode-ag-ui-event (first events)))))
      (ok (equal "t1" (gethash "threadId" json))
          "RUN_STARTED encodes camelCase keys"))))

(deftest feedback-event-becomes-eval-case
  (let* ((ds (eval:make-eval-dataset :name "fb" :cases nil))
         (domain (make-echo-expert :backend (mock-llm) :name "echo-fb")))
    (setf (expert-eval-suites domain) (list ds))
    (let ((new (handle-feedback-event
                domain
                (ag-ui:make-custom-event
                 :name "demiurge.feedback"
                 :value (ag-ui:json-object "rating" 5
                                           "correction" "better"
                                           "feedbackId" "fb-1"
                                           "ksId" "echo"
                                           "answer" "echo: hi")))))
      (ok (eval:eval-dataset-p new))
      (ok (member :human-feedback (eval:eval-dataset-provenance new)))
      (let* ((case (car (last (eval:eval-dataset-cases new))))
             (tags (getf (eval:eval-case-metadata case) :tags)))
        (ok (equal "echo: hi" (eval:eval-case-input case)))
        (ok (equal "better" (eval:eval-case-expected case)))
        (ok (member :human-feedback tags))
        (ok (find "fb-1" tags :test #'equal))
        (ok (find "echo" tags :test #'equal)))))
  (let* ((domain (make-echo-expert :backend (mock-llm) :name "echo-fb-mcp"))
         (server (make-expert-mcp-server domain)))
    (setf (expert-eval-suites domain)
          (list (eval:make-eval-dataset :name "fb-mcp" :cases nil)))
    (mcp:call-tool server "record_feedback"
                   (mcp:json-object "feedback_id" "fb-mcp-1"
                                    "rating" 1
                                    "correction" "fix"
                                    "ks_id" "echo"
                                    "answer" "old"))
    (let* ((ds (first (expert-eval-suites domain)))
           (case (car (last (eval:eval-dataset-cases ds))))
           (tags (getf (eval:eval-case-metadata case) :tags)))
      (ok (member :human-feedback (eval:eval-dataset-provenance ds)))
      (ok (find "fb-mcp-1" tags :test #'equal)))))

(deftest expert-app-healthz-readyz
  (let* ((domain (make-echo-expert :backend (mock-llm) :name "echo-hz"))
         (app (make-expert-app domain :personal)))
    (ok (= 200 (first (funcall app '(:request-method :get
                                     :path-info "/healthz")))))
    (ok (= 200 (first (funcall app '(:request-method :get
                                     :path-info "/readyz")))))
    (let ((demiurge/serve:*readyz-fn*
           (lambda (d) (declare (ignore d)) nil)))
      (ok (= 503 (first (funcall app '(:request-method :get
                                       :path-info "/readyz"))))))))
