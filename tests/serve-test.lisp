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
        (ok (eq :train (eval:eval-case-role case)))
        (ok (eq :human-feedback (eval:eval-case-source case)))
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

(defun %workspace-echo (root)
  (let* ((cfg (make-instance 'demiurge-config
                             :workspace-root
                             (namestring (uiop:ensure-directory-pathname root))))
         (profile (make-instance 'personal-profile :config cfg
                                 :data-dir (namestring root))))
    (make-echo-expert :backend (mock-llm) :name "echo-ws" :profile profile)))

(deftest expert-mcp-exposes-workspace-tree
  "demiurge serve MCP mounts workspace:// + search/read when [workspace] root is set."
  (with-tmp-dir (root)
    (%write-tree-file root "src/improve/cycle.lisp"
                      "(defun no-critical-regression-gate () t)")
    (%write-tree-file root "demos/queries.md"
                      "Search workspace:// for no-critical-regression-gate.")
    (let* ((domain (%workspace-echo root))
           (server (make-expert-mcp-server domain))
           (names (mapcar #'mcp:mcp-tool-name (mcp:list-tools server)))
           (listed (mcp:list-resources server))
           (uris (mapcar (lambda (r)
                           (or (ignore-errors (mcp:mcp-resource-uri r))
                               (and (consp r) (getf r :uri))))
                         listed)))
      (ok (find "ask_expert" names :test #'equal))
      (ok (find "search_workspace" names :test #'equal))
      (ok (find "read_workspace" names :test #'equal))
      (ok (find "workspace://" uris :test #'equal))
      (ok (find (workspace-resource-uri "src/improve/cycle.lisp") uris :test #'equal))
      (let* ((hits (%mcp-text
                    (mcp:call-tool server "search_workspace"
                                   (mcp:json-object "query" "no-critical-regression-gate"))))
             (cycle-pos (search "cycle.lisp" hits))
             (query-pos (search "queries.md" hits)))
        (ok cycle-pos)
        (ok (or (null query-pos) (< cycle-pos query-pos))))
      (let ((text (%mcp-text
                   (mcp:call-tool server "read_workspace"
                                  (mcp:json-object
                                   "uri" "workspace://src/improve/cycle.lisp")))))
        (ok (search "no-critical-regression-gate" text)))
      (let ((bad (mcp:call-tool server "read_workspace"
                                (mcp:json-object "path" "../etc/passwd"))))
        (ok (and (hash-table-p bad) (gethash "isError" bad)))))))

(defun %feedback-env (body &key (content-type "application/json")
                             content-length)
  (list :request-method :post
        :path-info "/feedback"
        :content-type content-type
        :content-length (or content-length (length body))
        :raw-body body))

(deftest feedback-http-rejects-malformed-without-mutation
  (let* ((ds (eval:make-eval-dataset :name "fb-http" :cases nil))
         (domain (make-echo-expert :backend (mock-llm) :name "echo-fb-http"))
         (app (make-expert-app domain :personal)))
    (setf (expert-eval-suites domain) (list ds))
    (ok (signals (handle-feedback-event domain "not-json")
                 'invalid-feedback))
    (ok (zerop (length (eval:eval-dataset-cases
                        (first (expert-eval-suites domain))))))
    (let ((res (funcall app (%feedback-env "not-json"))))
      (ok (= 400 (first res))))
    (ok (zerop (length (eval:eval-dataset-cases
                        (first (expert-eval-suites domain)))))
        "malformed JSON does not add a case")
    (let ((res (funcall app (%feedback-env "{\"unknown\":true,\"rating\":1}"))))
      (ok (= 400 (first res))))
    (let ((res (funcall app (%feedback-env "{\"feedback_id\":\"x\"}"))))
      (ok (= 400 (first res)) "missing rating"))
    (let ((res (funcall app (%feedback-env "{\"feedback_id\":\"x\",\"rating\":1}"
                                           :content-type "text/plain"))))
      (ok (= 415 (first res))))
    (let ((res (funcall app (%feedback-env "{\"feedback_id\":\"x\",\"rating\":1}"
                                           :content-length
                                           (1+ *max-request-bytes*)))))
      (ok (= 413 (first res))))
    (ok (zerop (length (eval:eval-dataset-cases
                        (first (expert-eval-suites domain)))))
        "4xx paths leave the dataset untouched")
    (let ((res (funcall app (%feedback-env
                            "{\"feedback_id\":\"fb-ok\",\"rating\":5,\"answer\":\"hi\"}"))))
      (ok (= 200 (first res)))
      (ok (plusp (length (eval:eval-dataset-cases
                          (first (expert-eval-suites domain)))))))))

(deftest record-feedback-mcp-strict-schema
  (let* ((domain (make-echo-expert :backend (mock-llm) :name "echo-fb-strict"))
         (server (make-expert-mcp-server domain)))
    (setf (expert-eval-suites domain)
          (list (eval:make-eval-dataset :name "fb-strict" :cases nil)))
    (ok (signals (mcp:call-tool server "record_feedback"
                                (mcp:json-object "rating" 1))
                 'mcp:mcp-error)
        "missing feedback_id")
    (ok (signals (mcp:call-tool server "record_feedback"
                                (mcp:json-object "feedback_id" "x"
                                                 "rating" 1
                                                 "evil" t))
                 'mcp:mcp-error)
        "unknown field")
    (ok (zerop (length (eval:eval-dataset-cases
                        (first (expert-eval-suites domain))))))))

(deftest serve-http-bind-requires-loopback-or-insecure-local
  (let* ((domain (make-echo-expert :backend (mock-llm) :name "echo-bind")))
    (ok (check-serve-security "127.0.0.1" :personal))
    (ok (check-serve-security "localhost" :personal))
    (ok (signals (check-serve-security "0.0.0.0" :personal)
                 'serve-error))
    (ok (check-serve-security "0.0.0.0" :personal :insecure-local t))
    (ok (signals (serve-expert domain
                               :transports '(:http)
                               :host "0.0.0.0"
                               :start nil)
                 'serve-error))
    (ok (serve-session-p
         (serve-expert domain
                       :transports '(:http)
                       :host "0.0.0.0"
                       :insecure-local t
                       :start nil)))))
