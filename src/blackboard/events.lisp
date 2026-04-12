(defpackage #:demiurge/src/blackboard/events
  (:use #:cl)
  (:import-from #:alexandria #:when-let)
  (:export #:event-bus #:make-event-bus
           #:bb-event #:event-timestamp #:event-workspace
           #:section-changed #:make-section-changed
           #:section-changed-key #:section-changed-old-value #:section-changed-new-value
           #:task-received #:make-task-received
           #:task-received-source #:task-received-payload
           #:workspace-transitioned #:make-workspace-transitioned
           #:ws-transition-ws #:ws-transition-from #:ws-transition-to
           #:capability-registered #:make-capability-registered
           #:cap-registered-capability
           #:timer-tick #:make-timer-tick
           #:tick-name #:tick-interval
           #:ks-completed #:make-ks-completed
           #:ks-completed-ks #:ks-completed-result #:ks-completed-duration
           #:idle-detected #:make-idle-detected
           #:subscribe #:unsubscribe #:emit-event
           #:run-event-loop #:stop-event-loop
           #:event-bus-running-p))

(in-package #:demiurge/src/blackboard/events)

;;; Event base
(defclass bb-event ()
  ((timestamp :initform (get-universal-time) :reader event-timestamp)
   (workspace :initarg :workspace :reader event-workspace :initform nil)))

;;; Event types
(defclass section-changed (bb-event)
  ((key :initarg :key :reader section-changed-key)
   (old-value :initarg :old-value :reader section-changed-old-value :initform nil)
   (new-value :initarg :new-value :reader section-changed-new-value)))

(defclass task-received (bb-event)
  ((source :initarg :source :reader task-received-source :initform :unknown)
   (payload :initarg :payload :reader task-received-payload)))

(defclass workspace-transitioned (bb-event)
  ((ws :initarg :ws :reader ws-transition-ws)
   (from :initarg :from :reader ws-transition-from)
   (to :initarg :to :reader ws-transition-to)))

(defclass capability-registered (bb-event)
  ((capability :initarg :capability :reader cap-registered-capability)))

(defclass timer-tick (bb-event)
  ((name :initarg :name :reader tick-name)
   (interval :initarg :interval :reader tick-interval :initform 0)))

(defclass ks-completed (bb-event)
  ((ks :initarg :ks :reader ks-completed-ks)
   (result :initarg :result :reader ks-completed-result :initform nil)
   (duration :initarg :duration :reader ks-completed-duration :initform 0)))

(defclass idle-detected (bb-event) ())

;;; Constructors
(defun make-section-changed (&key key old-value new-value workspace)
  (make-instance 'section-changed :key key :old-value old-value
                                  :new-value new-value :workspace workspace))

(defun make-task-received (&key source payload workspace)
  (make-instance 'task-received :source source :payload payload :workspace workspace))

(defun make-workspace-transitioned (&key ws from to)
  (make-instance 'workspace-transitioned :ws ws :from from :to to))

(defun make-capability-registered (&key capability workspace)
  (make-instance 'capability-registered :capability capability :workspace workspace))

(defun make-timer-tick (&key name interval)
  (make-instance 'timer-tick :name name :interval interval))

(defun make-ks-completed (&key ks result duration workspace)
  (make-instance 'ks-completed :ks ks :result result :duration duration :workspace workspace))

(defun make-idle-detected ()
  (make-instance 'idle-detected))

;;; Event bus
(defclass event-bus ()
  ((handlers :initform (make-hash-table :test 'eq) :reader bus-handlers)
   (queue :initform nil :accessor bus-queue)
   (queue-lock :initform (bt2:make-lock :name "event-bus-queue") :reader bus-queue-lock)
   (queue-cv :initform (bt2:make-condition-variable :name "event-bus-cv") :reader bus-queue-cv)
   (running :initform nil :accessor event-bus-running-p)
   (history :initform nil :accessor bus-history)
   (history-limit :initarg :history-limit :initform 200 :reader bus-history-limit)))

(defun make-event-bus (&key (history-limit 200))
  (make-instance 'event-bus :history-limit history-limit))

(defun subscribe (bus event-type handler)
  "Subscribe HANDLER to events of EVENT-TYPE (a class name symbol)."
  (bt2:with-lock-held ((bus-queue-lock bus))
    (push handler (gethash event-type (bus-handlers bus)))))

(defun unsubscribe (bus event-type handler)
  (bt2:with-lock-held ((bus-queue-lock bus))
    (setf (gethash event-type (bus-handlers bus))
          (remove handler (gethash event-type (bus-handlers bus))))))

(defun emit-event (bus event)
  "Push an event into the bus queue."
  (bt2:with-lock-held ((bus-queue-lock bus))
    (setf (bus-queue bus) (append (bus-queue bus) (list event)))
    (bt2:condition-notify (bus-queue-cv bus))))

(defun dispatch-event (bus event)
  "Call all matching handlers for EVENT."
  ;; Record in history
  (push event (bus-history bus))
  (when (> (length (bus-history bus)) (bus-history-limit bus))
    (setf (bus-history bus) (subseq (bus-history bus) 0 (bus-history-limit bus))))
  ;; Find handlers for this event's class and all superclasses
  (let ((event-class (class-of event)))
    (dolist (class (closer-mop:class-precedence-list event-class))
      (let ((class-name (class-name class)))
        (when-let (handlers (gethash class-name (bus-handlers bus)))
          (dolist (handler handlers)
            (handler-case (funcall handler event)
              (error (e)
                (format *error-output* "Event handler error: ~A~%" e)))))))))

(defun pop-event (bus &key (timeout 1.0))
  "Pop next event from queue, blocking up to TIMEOUT seconds."
  (bt2:with-lock-held ((bus-queue-lock bus))
    (loop
      (when (bus-queue bus)
        (return (pop (bus-queue bus))))
      (unless (event-bus-running-p bus)
        (return nil))
      (bt2:condition-wait (bus-queue-cv bus) (bus-queue-lock bus) :timeout timeout))))

(defun run-event-loop (bus)
  "Process events until stopped. Blocks."
  (setf (event-bus-running-p bus) t)
  (unwind-protect
       (loop while (event-bus-running-p bus) do
         (when-let (event (pop-event bus))
           (dispatch-event bus event)))
    (setf (event-bus-running-p bus) nil)))

(defun stop-event-loop (bus)
  "Signal the event loop to stop."
  (setf (event-bus-running-p bus) nil)
  (bt2:with-lock-held ((bus-queue-lock bus))
    (bt2:condition-notify (bus-queue-cv bus))))
