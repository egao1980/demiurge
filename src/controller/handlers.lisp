(defpackage #:demiurge/src/controller/handlers
  (:use #:cl)
  (:import-from #:alexandria #:when-let)
  (:import-from #:demiurge/src/blackboard/core
                #:blackboard #:write-section #:read-section)
  (:import-from #:demiurge/src/blackboard/events
                #:task-received #:task-received-source #:task-received-payload
                #:ks-completed #:ks-completed-ks #:ks-completed-result
                #:workspace-transitioned #:ws-transition-ws #:ws-transition-to
                #:idle-detected #:capability-registered #:section-changed)
  (:import-from #:demiurge/src/blackboard/workspace
                #:workspace #:workspace-blackboard #:workspace-status
                #:fork-workspace #:merge-workspace #:discard-workspace)
  (:import-from #:demiurge/src/knowledge-source/protocol
                #:ks-execute #:ks-postcondition)
  (:import-from #:demiurge/src/controller/scheduler
                #:schedule-next-ks)
  (:export #:handle-new-task #:handle-ks-done #:handle-ws-transition
           #:handle-idle #:handle-new-capability #:execute-ks-in-workspace))

(in-package #:demiurge/src/controller/handlers)

(defun handle-new-task (event bb)
  (let* ((name (format nil "task-~A" (get-universal-time)))
         (ws (fork-workspace bb name)))
    (write-section (workspace-blackboard ws) :task (task-received-payload event))
    (write-section (workspace-blackboard ws) :source (task-received-source event))
    (when-let (ks (schedule-next-ks (workspace-blackboard ws)))
      (execute-ks-in-workspace ws ks))
    ws))

(defun execute-ks-in-workspace (ws ks)
  (let* ((ws-bb (workspace-blackboard ws))
         (start (get-internal-real-time))
         (result (handler-case (ks-execute ks ws-bb)
                   (error (e)
                     (format *error-output* "KS error: ~A~%" e)
                     nil)))
         (duration (/ (- (get-internal-real-time) start)
                      internal-time-units-per-second)))
    (when result
      (ks-postcondition ks ws-bb result))
    (values result duration)))

(defun handle-ks-done (event bb)
  (declare (ignore event bb)))

(defun handle-ws-transition (event bb)
  (declare (ignore bb))
  (let ((ws (ws-transition-ws event))
        (to (ws-transition-to event)))
    (case to
      (:completed (merge-workspace ws))
      (:failed (discard-workspace ws)))))

(defun handle-idle (event bb)
  (declare (ignore event bb)))

(defun handle-new-capability (event bb)
  (declare (ignore event bb)))
