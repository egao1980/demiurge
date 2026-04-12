(defpackage #:demiurge/tests/blackboard-test
  (:use #:cl #:rove)
  (:import-from #:demiurge/src/blackboard/core
                #:make-blackboard #:read-section #:write-section
                #:remove-section #:list-sections))

(in-package #:demiurge/tests/blackboard-test)

(deftest test-read-write-section
  (let ((bb (make-blackboard)))
    (ok (null (read-section bb :tasks)))
    (write-section bb :tasks '(:task-1 :task-2))
    (ok (equal '(:task-1 :task-2) (read-section bb :tasks)))))

(deftest test-default-value
  (let ((bb (make-blackboard)))
    (ok (eq :none (read-section bb :missing :default :none)))))

(deftest test-merge-fn
  (let ((bb (make-blackboard)))
    (write-section bb :tasks '(1 2 3))
    (write-section bb :tasks '(4 5) :merge-fn #'append)
    (ok (equal '(1 2 3 4 5) (read-section bb :tasks)))))

(deftest test-remove-section
  (let ((bb (make-blackboard)))
    (write-section bb :tasks "data")
    (remove-section bb :tasks)
    (ok (null (read-section bb :tasks)))))

(deftest test-list-sections
  (let ((bb (make-blackboard)))
    (write-section bb :tasks "t")
    (write-section bb :code "c")
    (let ((sections (list-sections bb)))
      (ok (= 2 (length sections)))
      (ok (member :tasks sections))
      (ok (member :code sections)))))

(deftest test-thread-safety
  (let ((bb (make-blackboard))
        (done (bt2:make-condition-variable :name "done"))
        (lock (bt2:make-lock :name "test"))
        (count 0))
    (write-section bb :counter 0)
    (dotimes (i 10)
      (bt2:make-thread
       (lambda ()
         (dotimes (j 100)
           (write-section bb :counter (1+ (or (read-section bb :counter) 0))))
         (bt2:with-lock-held (lock)
           (incf count)
           (when (= count 10)
             (bt2:condition-notify done))))))
    (bt2:with-lock-held (lock)
      (loop while (< count 10) do
        (bt2:condition-wait done lock :timeout 5)))
    (ok (numberp (read-section bb :counter)))))
