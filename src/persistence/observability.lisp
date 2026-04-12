(defpackage #:demiurge/src/persistence/observability
  (:use #:cl)
  (:import-from #:demiurge/src/blackboard/core
                #:blackboard #:list-sections #:read-section)
  (:import-from #:demiurge/src/blackboard/events
                #:event-bus #:bb-event #:event-timestamp
                #:subscribe)
  (:import-from #:demiurge/src/blackboard/workspace
                #:list-workspaces #:workspace-name #:workspace-status)
  (:import-from #:demiurge/src/capabilities/registry #:list-capabilities)
  (:import-from #:demiurge/src/knowledge-source/registry #:list-ks)
  (:import-from #:demiurge/src/knowledge-source/protocol #:ks-name #:ks-version)
  (:export #:make-event-logger #:bb-stats #:health-check))

(in-package #:demiurge/src/persistence/observability)

(defstruct (event-logger (:constructor %make-event-logger))
  (log-file nil)
  (event-count 0 :type fixnum)
  (start-time (get-universal-time)))

(defun log-event (logger event)
  (incf (event-logger-event-count logger))
  (when (event-logger-log-file logger)
    (with-open-file (s (event-logger-log-file logger)
                       :direction :output :if-exists :append :if-does-not-exist :create)
      (format s "~A ~A ~A~%"
              (event-timestamp event)
              (type-of event)
              (event-logger-event-count logger)))))

(defun make-event-logger (&key log-file bus)
  "Create an event logger, optionally subscribing to an event bus."
  (let ((logger (%make-event-logger :log-file log-file)))
    (when bus
      (subscribe bus 'bb-event (lambda (e) (log-event logger e))))
    logger))

(defun bb-stats (bb)
  "Return a plist of blackboard statistics."
  (list :sections (length (list-sections bb))
        :workspaces (length (list-workspaces bb))
        :active-workspaces (length (list-workspaces bb :status :active))
        :capabilities (length (list-capabilities bb))
        :knowledge-sources (length (list-ks bb))))

(defun health-check (bb)
  "Return health status of the system."
  (let ((stats (bb-stats bb)))
    (list :status :healthy
          :stats stats
          :timestamp (get-universal-time))))
