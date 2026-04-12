;;; events-test.lisp — now tests the watcher/KSAR/agenda/scheduler system (replaces event bus tests)

(defpackage #:demiurge/tests/events-test
  (:use #:cl #:rove)
  (:import-from #:demiurge/src/blackboard/core
                #:blackboard #:make-blackboard #:read-section #:write-section
                #:watch #:unwatch #:list-watchers #:get-watcher
                #:ksar #:ksar-id #:ksar-watcher-id #:ksar-triggered-key
                #:ksar-priority #:ksar-context #:ksar-status
                #:enqueue-ksar #:pop-agenda #:agenda-contents #:agenda-size
                #:bb-active-count #:bb-max-concurrency
                #:run-scheduler #:stop-scheduler #:bb-scheduler-running-p
                #:bb-scheduler-thread #:make-ksar))

(in-package #:demiurge/tests/events-test)

;;; --- Watcher registration ---

(deftest test-watch-and-list
  (let ((bb (make-blackboard)))
    (watch bb :id :test-w :requires '(:foo) :handler (lambda (bb ksar) (declare (ignore bb ksar)))
           :priority 50)
    (ok (= 1 (length (list-watchers bb))))
    (ok (get-watcher bb :test-w))
    (unwatch bb :test-w)
    (ok (= 0 (length (list-watchers bb))))))

;;; --- write-section triggers watcher ---

(deftest test-write-triggers-ksar
  (let ((bb (make-blackboard)))
    (watch bb :id :w1 :requires '(:token-a)
           :handler (lambda (bb ksar) (declare (ignore bb ksar)))
           :priority 10)
    ;; Writing the required key should enqueue a KSAR
    (write-section bb :token-a "value-a")
    (ok (= 1 (agenda-size bb)))
    (let ((ksar (pop-agenda bb :timeout 0)))
      (ok ksar "Should have a KSAR")
      (ok (eq :w1 (ksar-watcher-id ksar)))
      (ok (eq :token-a (ksar-triggered-key ksar)))
      (ok (= 10 (ksar-priority ksar)))
      (ok (equal '((:token-a . "value-a")) (ksar-context ksar))))))

;;; --- Multi-requirement watcher ---

(deftest test-multi-requires-waits-for-all
  (let ((bb (make-blackboard)))
    (watch bb :id :w2 :requires '(:a :b)
           :handler (lambda (bb ksar) (declare (ignore bb ksar)))
           :priority 5)
    ;; Writing just :a shouldn't fire
    (write-section bb :a 1)
    (ok (= 0 (agenda-size bb)) "Should not fire with only :a")
    ;; Now write :b — both present
    (write-section bb :b 2)
    (ok (= 1 (agenda-size bb)) "Should fire now with both :a and :b")))

;;; --- One-shot watcher ---

(deftest test-one-shot-watcher
  (let ((bb (make-blackboard)))
    (watch bb :id :once :requires '(:x)
           :handler (lambda (bb ksar) (declare (ignore bb ksar)))
           :priority 1 :one-shot t)
    (write-section bb :x "first")
    (ok (= 1 (agenda-size bb)))
    ;; Watcher should be gone
    (ok (null (get-watcher bb :once)) "One-shot should be removed")
    ;; Writing again shouldn't create more KSARs
    (pop-agenda bb :timeout 0)
    (write-section bb :x "second")
    (ok (= 0 (agenda-size bb)) "One-shot shouldn't fire again")))

;;; --- Priority ordering ---

(deftest test-priority-ordering
  (let ((bb (make-blackboard)))
    ;; Enqueue KSARs with different priorities
    (enqueue-ksar bb (make-ksar :watcher-id :low :priority 1))
    (enqueue-ksar bb (make-ksar :watcher-id :high :priority 100))
    (enqueue-ksar bb (make-ksar :watcher-id :mid :priority 50))
    ;; Pop should return highest priority first
    (let ((first (pop-agenda bb :timeout 0)))
      (ok (eq :high (ksar-watcher-id first))))
    (let ((second (pop-agenda bb :timeout 0)))
      (ok (eq :mid (ksar-watcher-id second))))
    (let ((third (pop-agenda bb :timeout 0)))
      (ok (eq :low (ksar-watcher-id third))))))

;;; --- Scheduler integration ---

(deftest test-scheduler-runs-handler
  (let ((bb (make-blackboard))
        (result nil)
        (done (bt2:make-condition-variable :name "done"))
        (lock (bt2:make-lock :name "test-sched")))
    ;; Set up lparallel kernel
    (let ((lparallel:*kernel* (lparallel:make-kernel 2 :name "test")))
      (unwind-protect
           (progn
             (watch bb :id :sched-test :requires '(:trigger)
                    :handler (lambda (bb ksar)
                               (declare (ignore bb))
                               (bt2:with-lock-held (lock)
                                 (setf result (ksar-context ksar))
                                 (bt2:condition-notify done)))
                    :priority 50)
             ;; Start scheduler
             (setf (bb-scheduler-thread bb)
                   (bt2:make-thread (lambda () (run-scheduler bb))
                                    :name "test-scheduler"))
             ;; Trigger the watcher
             (write-section bb :trigger "hello")
             ;; Wait for handler
             (bt2:with-lock-held (lock)
               (bt2:condition-wait done lock :timeout 5))
             (stop-scheduler bb))
        (lparallel:end-kernel :wait t)))
    (ok result "Handler should have been called")
    (ok (equal "hello" (cdr (assoc :trigger result))))))

;;; --- On-change semantics ---

(deftest test-no-ksar-on-same-value
  (let ((bb (make-blackboard)))
    (watch bb :id :dup :requires '(:v)
           :handler (lambda (bb ksar) (declare (ignore bb ksar)))
           :priority 1)
    (write-section bb :v "same")
    (ok (= 1 (agenda-size bb)))
    (pop-agenda bb :timeout 0)
    ;; Write same value again — should NOT fire (on-change semantics)
    (write-section bb :v "same")
    (ok (= 0 (agenda-size bb)) "Same value should not re-trigger")))
