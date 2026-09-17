(defpackage #:demiurge/improve
  (:use #:cl #:demiurge)
  (:nicknames #:stack-demiurge.improve)
  (:local-nicknames (#:bb #:blackboard-protocol)
                    (#:bbj #:blackboard-journal)
                    (#:cap #:capability-protocol)
                    (#:agent #:ai-agent-protocol)
                    (#:steer #:steer-protocol)
                    (#:eval #:eval-protocol)
                    (#:llm #:llm-protocol)
                    (#:task #:task-protocol)
                    (#:tel #:telemetry-protocol)
                    (#:log #:log-protocol)
                    (#:schema #:schema-protocol)
                    (#:bt2 #:bordeaux-threads))
  (:export
   #:improve-error
   #:no-improvement-target
   #:trial-timeout
   #:promotion-approval-required
   #:improvement-decision
   #:improvement-decision-verdict
   #:improvement-decision-cycle-id
   #:invoke-promote
   #:invoke-demote
   #:invoke-defer

   #:ks-revision
   #:ks-revision-p
   #:make-ks-revision
   #:ks-revision-skill-text
   #:ks-revision-prompt
   #:ks-revision-chunk-config
   #:coerce-ks-revision

   #:versioned-ks
   #:versioned-ks-p
   #:make-versioned-ks
   #:versioned-ks-current
   #:versioned-ks-candidate
   #:versioned-ks-split-ratio
   #:versioned-ks-cycle-id
   #:versioned-ks-observations
   #:select-variant
   #:*current-ksar*
   #:record-variant-observation

   #:tag-operation
   #:operation-tags
   #:side-effecting-operation-p
   #:*side-effecting-operations*
   #:restricted-capability
   #:restricted-catalogue
   #:restricted-catalogue-p
   #:restricted-catalogue-recordings
   #:make-restricted-catalogue
   #:call-with-wall-clock
   #:compute-granted-p
   #:run-sandboxed-candidate
   #:improve-budget-scope
   #:wrap-llm-budget

   #:save-promoted-skill
   #:find-promoted-skill-version
   #:record-improve-decision
   #:emit-promotion-metric

   #:*ks-eval-history*
   #:record-ks-eval
   #:select-improvement-target
   #:apply-ks-revision
   #:revised-ks
   #:revised-ks-p
   #:*improve-phase-hook*
   #:run-improvement-cycle
   #:default-improve-gate)
  (:documentation
   "Self-improvement loop: versioned-ks A/B, sandboxed trials, eval gates."))

(in-package #:demiurge/improve)

(define-condition improve-error (demiurge-error)
  ()
  (:report (lambda (c s)
             (format s "demiurge improve error~@[: ~A~]"
                     (demiurge-error-message c)))))

(define-condition no-improvement-target (improve-error)
  ()
  (:report (lambda (c s)
             (format s "no improvement target~@[: ~A~]"
                     (demiurge-error-message c)))))

(define-condition trial-timeout (improve-error)
  ((seconds :initarg :seconds :reader trial-timeout-seconds :initform nil))
  (:report (lambda (c s)
             (format s "improvement trial exceeded wall-clock~@[ (~As)~]"
                     (trial-timeout-seconds c)))))

(define-condition promotion-approval-required (improve-error)
  ((cycle-id :initarg :cycle-id :reader promotion-approval-cycle-id
             :initform nil))
  (:report (lambda (c s)
             (format s "HITL approval required for cycle ~S"
                     (promotion-approval-cycle-id c)))))

(define-condition improvement-decision (condition)
  ((verdict :initarg :verdict :reader improvement-decision-verdict)
   (cycle-id :initarg :cycle-id :reader improvement-decision-cycle-id
             :initform nil))
  (:report (lambda (c s)
             (format s "improvement decision ~S for cycle ~S"
                     (improvement-decision-verdict c)
                     (improvement-decision-cycle-id c)))))

(defun invoke-promote (&optional condition)
  (let ((r (find-restart 'promote condition)))
    (when r (invoke-restart r))))

(defun invoke-demote (&optional condition)
  (let ((r (find-restart 'demote condition)))
    (when r (invoke-restart r))))

(defun invoke-defer (&optional condition)
  (let ((r (find-restart 'defer condition)))
    (when r (invoke-restart r))))
