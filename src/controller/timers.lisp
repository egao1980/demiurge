(defpackage #:demiurge/src/controller/timers
  (:use #:cl)
  (:import-from #:demiurge/src/blackboard/events
                #:event-bus #:emit-event #:make-timer-tick #:make-idle-detected)
  (:export #:start-timer #:stop-timer #:timer-thread))

(in-package #:demiurge/src/controller/timers)

(defstruct timer-thread
  (name "timer")
  (interval 30)
  (thread nil)
  (running nil :type boolean))

(defun start-timer (bus name interval &key idle-after)
  "Start a timer that emits tick events. IDLE-AFTER seconds of no activity triggers idle."
  (let ((timer (make-timer-thread :name (string name) :interval interval)))
    (setf (timer-thread-running timer) t
          (timer-thread-thread timer)
          (bt2:make-thread
           (lambda ()
             (loop while (timer-thread-running timer) do
               (sleep interval)
               (when (timer-thread-running timer)
                 (emit-event bus (make-timer-tick :name name :interval interval)))))
           :name (format nil "timer-~A" name)))
    timer))

(defun stop-timer (timer)
  (setf (timer-thread-running timer) nil))
