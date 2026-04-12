(defpackage #:demiurge/src/controller/handlers
  (:use #:cl)
  (:import-from #:alexandria #:when-let)
  (:import-from #:demiurge/src/blackboard/core
                #:blackboard #:write-section #:read-section)
  (:import-from #:demiurge/src/blackboard/workspace
                #:workspace #:workspace-blackboard #:workspace-status #:workspace-name
                #:fork-workspace #:merge-workspace #:discard-workspace
                #:get-workspace)
  (:import-from #:demiurge/src/knowledge-source/protocol
                #:knowledge-source #:ks-name #:ks-execute #:ks-postcondition)
  (:import-from #:demiurge/src/controller/scheduler
                #:schedule-next-ks)
  (:export #:handle-new-task #:handle-ks-done
           #:execute-ks-in-workspace
           #:init-kernel #:shutdown-kernel))

(in-package #:demiurge/src/controller/handlers)

;;; --- lparallel kernel management ---

(defvar *kernel-size* 4)

(defun init-kernel (&key (workers *kernel-size*))
  "Initialize the lparallel thread pool."
  (unless lparallel:*kernel*
    (setf lparallel:*kernel* (lparallel:make-kernel workers :name "demiurge-workers"))))

(defun shutdown-kernel ()
  "Shut down the lparallel thread pool."
  (when lparallel:*kernel*
    (lparallel:end-kernel :wait t)
    (setf lparallel:*kernel* nil)))

;;; --- Synchronous KS execution (for tests / direct calls) ---

(defun execute-ks-in-workspace (ws ks)
  "Execute KS synchronously in workspace. Returns (values result duration).
On completion, writes :ks-result token to workspace BB."
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
    ;; Write result as token (triggers downstream watchers)
    (write-section ws-bb :ks-result
                   (list :ks (ks-name ks) :result result :duration duration
                         :status (if result :ok :error)))
    (values result duration)))

;;; --- Token-based task handling ---

(defun handle-new-task (bb task-plist)
  "Handle a new task from :pending-task token: fork workspace, schedule first KS."
  (let* ((desc (if (listp task-plist) (getf task-plist :description) task-plist))
         (name (format nil "task-~A" (get-universal-time)))
         (ws (fork-workspace bb name)))
    (write-section (workspace-blackboard ws) :task desc)
    (when-let (ks (schedule-next-ks (workspace-blackboard ws)))
      (execute-ks-in-workspace ws ks))
    ws))

(defun handle-ks-done (bb ws-name result-plist)
  "Handle KS completion: chain next KS or finalize workspace."
  (when ws-name
    (when-let (ws (get-workspace bb ws-name))
      (let ((status (getf result-plist :status)))
        (if (eq status :error)
            (progn
              (write-section (workspace-blackboard ws) :error
                             (getf result-plist :result))
              (write-section (workspace-blackboard ws) :ws-status :failed))
            (let ((ws-bb (workspace-blackboard ws)))
              (when-let (next-ks (schedule-next-ks ws-bb))
                (execute-ks-in-workspace ws next-ks))))))))
