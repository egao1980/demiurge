(in-package #:demiurge/improve)

(defun %skill-store-of (domain &optional store)
  (or store
      (let ((prof (and (expert-domain-p domain) (expert-profile domain))))
        (and (deployment-profile-p prof) (profile-skill-store prof)))))

(defun %skill-from-revision (domain revision)
  (let ((text (and revision (ks-revision-skill-text revision))))
    (when (and text (plusp (length text)))
      (steer:make-steer-skill
       (if (expert-domain-p domain)
           (expert-name domain)
           "ks")
       :body text))))

(defun emit-promotion-metric (&key cycle-id eval-run-id verdict)
  (let* ((pkg (find-package '#:demiurge/observe))
         (name (cond
                 ((and pkg (eq verdict :demote)
                       (find-symbol "+METRIC-IMPROVE-DEMOTIONS+" pkg)
                       (boundp (find-symbol "+METRIC-IMPROVE-DEMOTIONS+" pkg)))
                  (symbol-value (find-symbol "+METRIC-IMPROVE-DEMOTIONS+" pkg)))
                 ((and pkg (find-symbol "+METRIC-IMPROVE-PROMOTIONS+" pkg)
                       (boundp (find-symbol "+METRIC-IMPROVE-PROMOTIONS+" pkg)))
                  (symbol-value (find-symbol "+METRIC-IMPROVE-PROMOTIONS+" pkg)))
                 ((eq verdict :demote) "demiurge.improve.demotions")
                 (t "demiurge.improve.promotions"))))
    (ignore-errors
      (tel:record-metric tel:*telemetry-backend*
                         name
                         1
                         :attributes (list :cycle-id cycle-id
                                           :eval-run-id eval-run-id
                                           :verdict verdict)
                         :unit "1")))
  t)

(defun record-improve-decision (blackboard provenance)
  "Write the decision record to a board section. PROVENANCE is a plist."
  (when blackboard
    (bb:write-section blackboard :improve-decision (copy-list provenance)))
  provenance)

(defun save-promoted-skill (domain revision &key cycle-id eval-run-id
                                              baseline-score candidate-score
                                              skill-store blackboard
                                              (verdict :promote))
  "Promote = save-skill-version when an A4 store is present (soft),
   write a board decision, emit demiurge.improve.promotion."
  (let* ((prov (list :cycle-id cycle-id
                     :eval-run-id eval-run-id
                     :baseline-score baseline-score
                     :candidate-score candidate-score
                     :verdict verdict))
         (store (%skill-store-of domain skill-store))
         (skill (and (eq verdict :promote)
                     (%skill-from-revision domain revision)))
         (saved nil))
    (when (and store skill)
      (setf saved (steer:save-skill-version store skill :provenance prov)))
    (record-improve-decision blackboard prov)
    (when (eq verdict :promote)
      (emit-promotion-metric :cycle-id cycle-id
                             :eval-run-id eval-run-id
                             :verdict verdict)
      (when log:*log-backend*
        (log:with-context (:eval-run-id eval-run-id
                           :cycle-id cycle-id)
          (log:info "improvement promotion"))))
    saved))
