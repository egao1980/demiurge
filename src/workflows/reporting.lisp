(in-package #:demiurge/workflows)

(defun %status-keyword (status)
  (case status
    ((:working :running) :working)
    ((:completed :done) :completed)
    ((:failed :error) :failed)
    ((:waiting :input-required :input_required) :input-required)
    ((:canceled :cancelled) :canceled)
    (t (if (keywordp status) status :working))))

(defun workflow-a2a-state (status)
  "Map a workflow/task status to an A2A task state via the wire adapter
   when blackboard-wire/a2a is loaded."
  (let* ((mapped (%funcall-if '#:blackboard-wire/a2a "KSAR-STATUS-TO-A2A"
                              (%status-keyword status)))
         (make (%find-sym '#:a2a-protocol "MAKE-A2A-TASK"))
         (set-state (%find-sym '#:a2a-protocol "(SETF A2A-TASK-STATE)"))
         (a2a-state (or mapped (%status-keyword status))))
    (cond
      ((and make (fboundp make))
       (let ((task (funcall make :state a2a-state)))
         (when (and set-state (fboundp set-state))
           (funcall set-state a2a-state task))
         task))
      (t a2a-state))))

(defun workflow-state-delta (board key value)
  "AG-UI STATE_DELTA for a board section write, when the A5 adapter is loaded."
  (let* ((patch-fn (%find-sym '#:blackboard-wire/ag-ui "MAKE-SECTION-PATCH"))
         (event-fn (%find-sym '#:ag-ui-protocol "MAKE-STATE-DELTA-EVENT")))
    (when (and patch-fn event-fn (fboundp patch-fn) (fboundp event-fn))
      (funcall event-fn
               :delta (list (funcall patch-fn key value))))))

(defun sync-workflow-wire (workflow &key board status summary)
  "Push task-tree progress to A2A + AG-UI when serve/wire systems are loaded."
  (let ((board (or board (and (project-workflow-p workflow)
                              (project-workflow-board workflow))))
        (a2a (workflow-a2a-state status))
        (delta (and board (workflow-state-delta board :round-summary summary))))
    (list :a2a a2a :delta delta)))

(defun report-workflow-progress (workflow &key board round summary status)
  "Append a per-round summary on BOARD and sync wire adapters when loaded."
  (let* ((board (or board
                    (and (project-workflow-p workflow)
                         (project-workflow-board workflow))))
         (status (or status :working))
         (entry (list :round round
                      :summary summary
                      :status status)))
    (when board
      (when summary
        (bb:write-section board :round-summary summary))
      (let ((prev (bb:read-section board :workflow-progress :default nil)))
        (bb:write-section board :workflow-progress
                          (append (if (listp prev) prev nil)
                                  (list entry)))))
    (let ((wire (sync-workflow-wire workflow
                                    :board board
                                    :status status
                                    :summary summary)))
      (append entry wire))))
