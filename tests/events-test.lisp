(defpackage #:demiurge/tests/events-test
  (:use #:cl #:rove)
  (:import-from #:demiurge/src/blackboard/events
                #:event-bus #:make-event-bus
                #:subscribe #:emit-event #:run-event-loop #:stop-event-loop
                #:section-changed #:make-section-changed
                #:section-changed-key #:section-changed-new-value
                #:task-received #:make-task-received))

(in-package #:demiurge/tests/events-test)

(deftest test-subscribe-and-emit
  (let ((bus (make-event-bus))
        (received nil))
    (subscribe bus 'section-changed
               (lambda (e) (push (section-changed-key e) received)))
    ;; Manually dispatch
    (demiurge/src/blackboard/events::dispatch-event
     bus (make-section-changed :key :tasks :new-value "test"))
    (ok (= 1 (length received)))
    (ok (eq :tasks (first received)))))

(deftest test-multiple-subscribers
  (let ((bus (make-event-bus))
        (count 0))
    (subscribe bus 'section-changed (lambda (e) (declare (ignore e)) (incf count)))
    (subscribe bus 'section-changed (lambda (e) (declare (ignore e)) (incf count)))
    (demiurge/src/blackboard/events::dispatch-event
     bus (make-section-changed :key :x :new-value 1))
    (ok (= 2 count))))

(deftest test-event-loop
  (let ((bus (make-event-bus))
        (received nil)
        (done (bt2:make-condition-variable :name "done"))
        (lock (bt2:make-lock :name "test")))
    (subscribe bus 'task-received
               (lambda (e)
                 (bt2:with-lock-held (lock)
                   (push (demiurge/src/blackboard/events:task-received-payload e) received)
                   (bt2:condition-notify done))))
    ;; Run event loop in background
    (let ((thread (bt2:make-thread
                   (lambda () (run-event-loop bus))
                   :name "test-event-loop")))
      (declare (ignore thread))
      ;; Emit event
      (emit-event bus (make-task-received :source :test :payload "hello"))
      ;; Wait for delivery
      (bt2:with-lock-held (lock)
        (bt2:condition-wait done lock :timeout 3))
      (stop-event-loop bus)
      (sleep 0.2)
      (ok (= 1 (length received)))
      (ok (string= "hello" (first received))))))

(deftest test-event-history
  (let ((bus (make-event-bus :history-limit 5)))
    (dotimes (i 10)
      (demiurge/src/blackboard/events::dispatch-event
       bus (make-section-changed :key (intern (format nil "K~A" i) :keyword)
                                 :new-value i)))
    (ok (<= (length (demiurge/src/blackboard/events::bus-history bus)) 5))))
