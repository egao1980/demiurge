(defpackage #:demiurge/src/controller/main-loop
  (:use #:cl)
  (:import-from #:demiurge/src/blackboard/core
                #:blackboard #:make-blackboard #:blackboard-notify-fn)
  (:import-from #:demiurge/src/blackboard/events
                #:event-bus #:make-event-bus
                #:subscribe #:emit-event #:run-event-loop #:stop-event-loop
                #:task-received #:section-changed #:ks-completed
                #:workspace-transitioned #:capability-registered
                #:idle-detected #:timer-tick
                #:make-section-changed)
  (:import-from #:demiurge/src/controller/handlers
                #:handle-new-task #:handle-ks-done #:handle-ws-transition
                #:handle-idle #:handle-new-capability)
  (:import-from #:demiurge/src/controller/timers
                #:start-timer #:stop-timer)
  (:export #:run-demiurge #:make-demiurge-instance #:demiurge-ctx
           #:demiurge-ctx-bb #:demiurge-ctx-bus
           #:stop-demiurge))

(in-package #:demiurge/src/controller/main-loop)

(defstruct demiurge-ctx
  (bb nil)
  (bus nil)
  (timers nil :type list))

(defun make-demiurge-instance ()
  (let* ((bus (make-event-bus))
         (bb (make-blackboard
              :notify-fn (lambda (key old new)
                           (emit-event bus (make-section-changed
                                           :key key :old-value old :new-value new))))))
    (make-demiurge-ctx :bb bb :bus bus)))

(defun register-handlers (dm)
  (let ((bus (demiurge-ctx-bus dm))
        (bb (demiurge-ctx-bb dm)))
    (subscribe bus 'task-received
               (lambda (event) (handle-new-task event bb)))
    (subscribe bus 'ks-completed
               (lambda (event) (handle-ks-done event bb)))
    (subscribe bus 'workspace-transitioned
               (lambda (event) (handle-ws-transition event bb)))
    (subscribe bus 'idle-detected
               (lambda (event) (handle-idle event bb)))
    (subscribe bus 'capability-registered
               (lambda (event) (handle-new-capability event bb)))))

(defun run-demiurge (&key (idle-interval 30) (metrics-interval 60))
  "Start the demiurge daemon. Blocks until stopped."
  (let ((dm (make-demiurge-instance)))
    (register-handlers dm)
    (push (start-timer (demiurge-ctx-bus dm) :idle-check idle-interval)
          (demiurge-ctx-timers dm))
    (push (start-timer (demiurge-ctx-bus dm) :metrics metrics-interval)
          (demiurge-ctx-timers dm))
    (unwind-protect
         (run-event-loop (demiurge-ctx-bus dm))
      (stop-demiurge dm))
    dm))

(defun stop-demiurge (dm)
  (stop-event-loop (demiurge-ctx-bus dm))
  (dolist (timer (demiurge-ctx-timers dm))
    (stop-timer timer)))
