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
  (let ((name (if (eq verdict :demote)
                  "RECORD-DEMOTION"
                  "RECORD-PROMOTION")))
    (demiurge::%observe-record name
                               :cycle-id cycle-id
                               :eval-run-id eval-run-id))
  t)

(defun record-improve-decision (blackboard provenance)
  "Write the decision record to a board section. PROVENANCE is a plist.
   Emits record-promotion / record-demotion from the verdict."
  (when blackboard
    (bb:write-section blackboard :improve-decision (copy-list provenance)))
  (let ((verdict (getf provenance :verdict)))
    (when (member verdict '(:promote :demote) :test #'eq)
      (emit-promotion-metric :cycle-id (getf provenance :cycle-id)
                             :eval-run-id (getf provenance :eval-run-id)
                             :verdict verdict)))
  provenance)

(defun %promotion-skill-name (domain)
  (if (expert-domain-p domain)
      (expert-name domain)
      "ks"))

(defun %promotion-key-match-p (version cycle-id eval-run-id)
  (let ((p (and version (steer:skill-version-provenance version))))
    (and p
         (equal (getf p :cycle-id) cycle-id)
         (equal (getf p :eval-run-id) eval-run-id))))

(defun find-promoted-skill-version (store name cycle-id eval-run-id)
  "Existing skill-version for CYCLE-ID + EVAL-RUN-ID, or NIL."
  (when (and store name cycle-id eval-run-id)
    (find-if (lambda (v)
               (%promotion-key-match-p v cycle-id eval-run-id))
             (steer:skill-versions store name))))

(defun save-promoted-skill (domain revision &key cycle-id eval-run-id
                                              baseline-score candidate-score
                                              skill-store blackboard
                                              (verdict :promote))
  "Promote = save-skill-version when an A4 store is present (soft),
   write a board decision, emit demiurge.improve.promotion.
   Upserts by cycle-id + eval-run-id so a crash-window retry does not duplicate."
  (let* ((prov (list :cycle-id cycle-id
                     :eval-run-id eval-run-id
                     :baseline-score baseline-score
                     :candidate-score candidate-score
                     :verdict verdict))
         (store (%skill-store-of domain skill-store))
         (skill-name (%promotion-skill-name domain))
         (skill (and (eq verdict :promote)
                     (%skill-from-revision domain revision)))
         (saved nil))
    (when (and store skill)
      (setf saved (or (find-promoted-skill-version
                       store skill-name cycle-id eval-run-id)
                      (steer:save-skill-version store skill :provenance prov))))
    (record-improve-decision blackboard prov)
    (when (and (eq verdict :promote) log:*log-backend*)
      (log:with-context (:eval-run-id eval-run-id
                         :cycle-id cycle-id)
        (log:info "improvement promotion")))
    saved))
