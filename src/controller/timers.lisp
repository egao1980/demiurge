(defpackage #:demiurge/src/controller/timers
  (:use #:cl)
  (:import-from #:demiurge/src/blackboard/core
                #:blackboard #:write-section)
  (:export #:start-timer #:stop-timer #:timer-thread))

(in-package #:demiurge/src/controller/timers)

(defstruct timer-thread
  (name "timer")
  (interval 30)
  (thread nil)
  (running nil :type boolean))

(defun start-timer (bb name interval)
  "Start a timer that writes :tick token to BB periodically."
  (let ((timer (make-timer-thread :name (string name) :interval interval)))
    (setf (timer-thread-running timer) t
          (timer-thread-thread timer)
          (bt2:make-thread
           (lambda ()
             (loop while (timer-thread-running timer) do
               (sleep interval)
               (when (timer-thread-running timer)
                 (write-section bb :tick (get-universal-time)))))
           :name (format nil "timer-~A" name)))
    timer))

(defun stop-timer (timer)
  (setf (timer-thread-running timer) nil))
