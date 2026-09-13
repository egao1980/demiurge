(in-package #:demiurge/serve)

(defun make-feedback-id (&optional seed)
  "Stable-enough answer span id for a served reply."
  (format nil "fb-~a-~a"
          (or seed (get-universal-time))
          (random (expt 36 6))))

(defun %ensure-feedback-dataset (domain)
  (or (find-if #'eval:eval-dataset-p (expert-eval-suites domain))
      (let ((ds (eval:make-eval-dataset :name "feedback" :cases nil)))
        (setf (expert-eval-suites domain) (list ds))
        ds)))

(defun record-feedback (domain &key feedback-id rating correction text
                                 answer ks-id)
  "ADD-CASE on DOMAIN's dataset with :SOURCE :HUMAN-FEEDBACK.
   Tags carry the answering KS id and FEEDBACK-ID."
  (check-type domain expert-domain)
  (let* ((ds (%ensure-feedback-dataset domain))
         (fid (or feedback-id (make-feedback-id)))
         (ks (or ks-id
                 (let ((first (first (expert-ks-set domain))))
                   (and first (bb:ks-name first)))))
         (tags (remove nil (list :human-feedback ks fid)))
         (case (eval:make-eval-case
                :input (or answer text "")
                :expected (or correction text answer "")
                :metadata (list :tags tags
                                :rating rating
                                :feedback-id fid
                                :ks-id ks
                                :source :human-feedback)))
         (new (eval:add-case ds case :source :human-feedback)))
    (setf (expert-eval-suites domain)
          (cons new (remove ds (copy-list (expert-eval-suites domain)))))
    new))

(defun %ht-get (table &rest keys)
  (cond
    ((null table) nil)
    ((hash-table-p table)
     (dolist (key keys)
       (let ((v (or (gethash key table)
                    (and (stringp key)
                         (gethash (string-downcase key) table))
                    (and (keywordp key)
                         (gethash (string-downcase (symbol-name key)) table)))))
         (when v (return v)))))
    ((listp table)
     (dolist (key keys)
       (let ((v (or (and (keywordp key) (getf table key))
                    (cdr (assoc key table :test #'equal))
                    (and (stringp key)
                         (getf table (intern (string-upcase key) :keyword))))))
         (when v (return v)))))
    (t nil)))

(defun handle-feedback-event (domain event)
  "AG-UI CUSTOM event `demiurge.feedback` (or a value plist/table) → ADD-CASE."
  (let ((value (cond
                 ((and (typep event 'ag-ui:custom-event)
                       (equal (ag-ui:custom-event-name event)
                              "demiurge.feedback"))
                  (ag-ui:custom-event-value event))
                 ((typep event 'ag-ui:custom-event)
                  (ag-ui:custom-event-value event))
                 ((hash-table-p event)
                  (or (gethash "value" event) event))
                 ((listp event) event)
                 (t event))))
    (record-feedback domain
                     :feedback-id (%ht-get value "feedbackId" :feedback-id
                                           "feedback-id" :feedbackId)
                     :rating (%ht-get value "rating" :rating)
                     :correction (%ht-get value "correction" :correction)
                     :text (%ht-get value "text" :text)
                     :answer (%ht-get value "answer" :answer)
                     :ks-id (%ht-get value "ksId" :ks-id "ks-id" :ksId))))

(defun make-record-feedback-tool (domain)
  "MCP tool `record_feedback`."
  (mcp:make-mcp-tool
   "record_feedback"
   :description "Record human feedback as an eval-protocol case"
   :input-schema (mcp:json-object
                  "type" "object"
                  "additionalProperties" t
                  "properties"
                  (mcp:json-object
                   "feedback_id" (mcp:json-object "type" "string")
                   "rating" (mcp:json-object "type" "number")
                   "correction" (mcp:json-object "type" "string")
                   "text" (mcp:json-object "type" "string")
                   "answer" (mcp:json-object "type" "string")
                   "ks_id" (mcp:json-object "type" "string")))
   :handler (lambda (args)
              (let ((ds (record-feedback
                         domain
                         :feedback-id (%ht-get args "feedback_id" :feedback-id
                                               "feedbackId")
                         :rating (%ht-get args "rating" :rating)
                         :correction (%ht-get args "correction" :correction)
                         :text (%ht-get args "text" :text)
                         :answer (%ht-get args "answer" :answer)
                         :ks-id (%ht-get args "ks_id" :ks-id "ksId"))))
                (mcp:tool-result
                 (list (mcp:make-text-content
                        (format nil "recorded feedback on ~a (~a cases)"
                                (eval:eval-dataset-name ds)
                                (length (eval:eval-dataset-cases ds))))))))))
