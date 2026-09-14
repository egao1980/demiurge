(defpackage #:demiurge/workflows
  (:use #:cl #:demiurge)
  (:nicknames #:stack-demiurge.workflows)
  (:local-nicknames (#:bb #:blackboard-protocol)
                    (#:eval #:eval-protocol)
                    (#:llm #:llm-protocol)
                    (#:task #:task-protocol)
                    (#:rag #:rag-protocol)
                    (#:web #:websearch-protocol)
                    (#:doc #:doc-extract-protocol)
                    (#:schema #:schema-protocol)
                    (#:log #:log-protocol)
                    (#:mcp #:mcp-protocol)
                    (#:agent #:ai-agent-protocol))
  (:export
   #:workflows-error
   #:approval-required
   #:approval-required-milestone
   #:approval-required-prompt
   #:research-error
   #:research-incomplete
   #:invoke-approve
   #:invoke-use-partial

   #:project-spec
   #:project-spec-p
   #:make-project-spec
   #:project-spec-name
   #:project-spec-milestones
   #:project-spec-schedule
   #:project-spec-board
   #:coerce-project-spec

   #:project-milestone
   #:project-milestone-p
   #:make-project-milestone
   #:milestone-name
   #:milestone-prompt
   #:coerce-milestone

   #:project-workflow
   #:project-workflow-p
   #:make-project-workflow
   #:project-workflow-name
   #:project-workflow-domain
   #:project-workflow-board
   #:project-workflow-task
   #:project-workflow-spec
   #:project-workflow-status
   #:start-project
   #:record-milestone
   #:await-approval
   #:schedule-project

   #:research-subquestion
   #:research-subquestion-p
   #:make-research-subquestion
   #:research-subquestion-id
   #:research-subquestion-question
   #:research-subquestion-rationale
   #:research-plan
   #:research-plan-p
   #:make-research-plan
   #:research-plan-question
   #:research-plan-subquestions
   #:coerce-research-plan
   #:research-plan-plist

   #:*research-child-hook*
   #:*research-child-exec-hook*
   #:*research-phase-hook*
   #:research-budget-scope
   #:wrap-research-llm
   #:collect-research-citations
   #:format-research-budget-footer
   #:run-deep-research
   #:render-research-document

   #:*default-research-instructions*
   #:*default-research-clip-chars*
   #:merge-research-instructions
   #:research-instruction
   #:research-workspace
   #:research-workspace-p
   #:make-research-workspace
   #:research-workspace-name
   #:research-workspace-board
   #:research-workspace-store
   #:research-workspace-sources
   #:research-workspace-mcp
   #:research-workspace-instructions
   #:record-research-source
   #:retrieve-research-sources
   #:research-source-catalog
   #:research-source-uri
   #:clip-research-text
   #:generate-research-step
   #:ensure-research-mcp-server
   #:list-research-resources
   #:read-research-resource

   #:report-workflow-progress
   #:sync-workflow-wire
   #:workflow-a2a-state
   #:workflow-state-delta)
  (:documentation
   "Durable project workflows and deep-research fan-out on task-protocol."))

(in-package #:demiurge/workflows)

(define-condition workflows-error (demiurge-error)
  ()
  (:report (lambda (c s)
             (format s "demiurge workflows error~@[: ~A~]"
                     (demiurge-error-message c)))))

(define-condition approval-required (workflows-error)
  ((milestone :initarg :milestone :reader approval-required-milestone
              :initform nil)
   (prompt :initarg :prompt :reader approval-required-prompt :initform nil))
  (:report (lambda (c s)
             (format s "approval required for milestone ~S~@[: ~A~]"
                     (approval-required-milestone c)
                     (or (approval-required-prompt c)
                         (demiurge-error-message c))))))

(define-condition research-error (workflows-error)
  ()
  (:report (lambda (c s)
             (format s "deep-research error~@[: ~A~]"
                     (demiurge-error-message c)))))

(define-condition research-incomplete (research-error)
  ((reason :initarg :reason :reader research-incomplete-reason :initform nil))
  (:report (lambda (c s)
             (format s "deep-research incomplete~@[: ~A~]"
                     (or (research-incomplete-reason c)
                         (demiurge-error-message c))))))

(defun invoke-approve (&optional condition)
  (let ((r (find-restart 'approve condition)))
    (when r (invoke-restart r))))

(defun invoke-use-partial (&optional condition)
  (let ((r (find-restart 'use-partial condition)))
    (when r (invoke-restart r))))

(defun %find-sym (package name)
  (let ((pkg (find-package package)))
    (and pkg (find-symbol name pkg))))

(defun %funcall-if (package name &rest args)
  (let ((s (%find-sym package name)))
    (when (and s (fboundp s))
      (apply (fdefinition s) args))))
