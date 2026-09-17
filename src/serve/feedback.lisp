(in-package #:demiurge/serve)

(defun make-feedback-id (&optional seed)
  "Stable-enough answer span id for a served reply."
  (format nil "fb-~a-~a"
          (or seed (get-universal-time))
          (random (expt 36 6))))

(defun %ensure-feedback-dataset (domain)
  "Train-role suite for production feedback. Never the promotion holdout."
  (or (find :train (expert-eval-suites domain)
            :key (lambda (ds)
                   (and (eval:eval-dataset-p ds)
                        (eval:eval-dataset-role ds)))
            :test #'eq)
      (find-if (lambda (ds)
                 (and (eval:eval-dataset-p ds)
                      (not (eq (eval:eval-dataset-role ds) :holdout))))
               (expert-eval-suites domain))
      (let ((ds (eval:make-eval-dataset :name "feedback" :role :train
                                        :cases nil)))
        (setf (expert-eval-suites domain)
              (append (expert-eval-suites domain) (list ds)))
        ds)))

(defun record-feedback (domain &key feedback-id rating correction text
                                 answer ks-id)
  "ADD-CASE on DOMAIN's train suite with :SOURCE :HUMAN-FEEDBACK :ROLE :TRAIN.
   Production feedback cannot enter the promotion holdout."
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
                :role :train
                :source :human-feedback
                :metadata (list :tags tags
                                :rating rating
                                :feedback-id fid
                                :ks-id ks
                                :source :human-feedback)))
         (new (eval:add-case ds case :source :human-feedback :role :train)))
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

(defparameter *feedback-known-keys*
  '("feedback_id" "feedback-id" "feedbackId"
    "rating" "correction" "text" "answer"
    "ks_id" "ks-id" "ksId"))

(defun %object-keys (value)
  (cond
    ((hash-table-p value)
     (loop for k being the hash-keys of value collect (string k)))
    ((and (consp value) (keywordp (car value)))
     (loop for (k nil) on value by #'cddr collect (string-downcase (string k))))
    ((and (consp value) (consp (car value)))
     (mapcar (lambda (p) (string (car p))) value))
    (t nil)))

(defun validate-feedback-value (value)
  "Require feedback_id + rating; reject unknown fields. Signals INVALID-FEEDBACK."
  (when (or (null value) (stringp value) (numberp value))
    (error 'invalid-feedback :message "malformed feedback value"))
  (let ((fid (%ht-get value "feedbackId" :feedback-id "feedback-id"
                      :feedbackId "feedback_id"))
        (rating (%ht-get value "rating" :rating))
        (unknown (remove-if (lambda (k)
                              (member k *feedback-known-keys* :test #'string-equal))
                            (%object-keys value))))
    (unless fid
      (error 'invalid-feedback :message "missing feedback_id"))
    (unless rating
      (error 'invalid-feedback :message "missing rating"))
    (when unknown
      (error 'invalid-feedback
             :message (format nil "unknown feedback fields ~S" unknown)))
    value))

(defun handle-feedback-event (domain event)
  "AG-UI CUSTOM event `demiurge.feedback` (or a value plist/table) → ADD-CASE.
   Malformed input signals INVALID-FEEDBACK (no dataset mutation)."
  (when (or (null event) (stringp event) (numberp event))
    (error 'invalid-feedback :message "malformed feedback event"))
  (when (and (typep event 'ag-ui:custom-event)
             (not (equal (ag-ui:custom-event-name event) "demiurge.feedback")))
    (error 'invalid-feedback
           :message (format nil "unsupported event ~S"
                            (ag-ui:custom-event-name event))))
  (let ((value (cond
                 ((typep event 'ag-ui:custom-event)
                  (ag-ui:custom-event-value event))
                 ((hash-table-p event)
                  (if (equal (gethash "type" event) "CUSTOM")
                      (or (gethash "value" event)
                          (error 'invalid-feedback :message "CUSTOM event missing value"))
                      event))
                 ((listp event)
                  (or (getf event :value) event))
                 (t
                  (error 'invalid-feedback :message "malformed feedback event")))))
    (validate-feedback-value value)
    (record-feedback domain
                     :feedback-id (%ht-get value "feedbackId" :feedback-id
                                           "feedback-id" :feedbackId
                                           "feedback_id")
                     :rating (%ht-get value "rating" :rating)
                     :correction (%ht-get value "correction" :correction)
                     :text (%ht-get value "text" :text)
                     :answer (%ht-get value "answer" :answer)
                     :ks-id (%ht-get value "ksId" :ks-id "ks-id" :ksId))))

(defun make-record-feedback-tool (domain)
  "MCP tool `record_feedback` with a closed input schema."
  (mcp:make-mcp-tool
   "record_feedback"
   :description "Record human feedback as an eval-protocol case"
   :input-schema (mcp:json-object
                  "type" "object"
                  "additionalProperties" nil
                  "required" (vector "feedback_id" "rating")
                  "properties"
                  (mcp:json-object
                   "feedback_id" (mcp:json-object "type" "string")
                   "rating" (mcp:json-object "type" "number")
                   "correction" (mcp:json-object "type" "string")
                   "text" (mcp:json-object "type" "string")
                   "answer" (mcp:json-object "type" "string")
                   "ks_id" (mcp:json-object "type" "string")))
   :handler (lambda (args)
              (validate-feedback-value args)
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
