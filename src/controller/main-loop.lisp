(defpackage #:demiurge/src/controller/main-loop
  (:use #:cl)
  (:import-from #:demiurge/src/blackboard/core
                #:blackboard #:make-blackboard
                #:run-scheduler #:stop-scheduler
                #:bb-scheduler-thread #:bb-scheduler-running-p)
  (:import-from #:demiurge/src/controller/handlers
                #:init-kernel #:shutdown-kernel)
  (:import-from #:demiurge/src/controller/timers
                #:start-timer #:stop-timer)
  (:export #:run-demiurge #:make-demiurge-instance #:demiurge-ctx
           #:demiurge-ctx-bb
           #:stop-demiurge))

(in-package #:demiurge/src/controller/main-loop)

(defstruct demiurge-ctx
  (bb nil)
  (timers nil :type list))

(defun make-demiurge-instance (&key (workers 4) (max-concurrency 4))
  (init-kernel :workers workers)
  (let ((bb (make-blackboard :max-concurrency max-concurrency)))
    (make-demiurge-ctx :bb bb)))

(defun start-scheduler-thread (bb)
  "Start the scheduler in a background thread."
  (setf (bb-scheduler-thread bb)
        (bt2:make-thread (lambda () (run-scheduler bb))
                         :name "demiurge-scheduler")))

(defun run-demiurge (&key (tick-interval 10) (workers 4))
  "Start the demiurge daemon. Blocks until stopped."
  (let ((dm (make-demiurge-instance :workers workers)))
    (push (start-timer (demiurge-ctx-bb dm) :tick tick-interval)
          (demiurge-ctx-timers dm))
    (start-scheduler-thread (demiurge-ctx-bb dm))
    ;; Block until scheduler stops
    (unwind-protect
         (loop while (bb-scheduler-running-p (demiurge-ctx-bb dm))
               do (sleep 1))
      (stop-demiurge dm))
    dm))

(defun stop-demiurge (dm)
  (stop-scheduler (demiurge-ctx-bb dm))
  (dolist (timer (demiurge-ctx-timers dm))
    (stop-timer timer))
  (shutdown-kernel))
