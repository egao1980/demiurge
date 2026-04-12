(defpackage #:demiurge/tests/memory-test
  (:use #:cl #:rove)
  (:import-from #:demiurge/src/persistence/memory
                #:make-persistent-memory #:mem-get #:mem-set #:mem-delete
                #:mem-keys #:mem-has-p #:mem-append #:mem-get-list
                #:mem-get-list-last #:mem-increment #:mem-get-number
                #:mem-save #:mem-load #:with-memory-transaction)
  (:import-from #:demiurge/src/persistence/memory-keys
                #:record-ks-execution #:ks-success-rate #:ks-avg-duration
                #:record-task-result #:recent-tasks
                #:remember #:recall #:forget
                #:set-preference #:get-preference
                #:log-interaction #:recent-interactions))

(in-package #:demiurge/tests/memory-test)

(deftest key-value-basics
  (let ((mem (make-persistent-memory :auto-save nil)))
    (ok (null (mem-get mem "foo")))
    (mem-set mem "foo" "bar")
    (ok (equal "bar" (mem-get mem "foo")))
    (ok (mem-has-p mem "foo"))
    (mem-delete mem "foo")
    (ok (null (mem-get mem "foo")))))

(deftest list-operations
  (let ((mem (make-persistent-memory :auto-save nil)))
    (mem-append mem "log" "a")
    (mem-append mem "log" "b")
    (mem-append mem "log" "c")
    (ok (equal '("a" "b" "c") (mem-get-list mem "log")))
    (ok (equal '("c") (mem-get-list-last mem "log" 1)))
    (ok (equal '("b" "c") (mem-get-list-last mem "log" 2)))))

(deftest list-max-entries
  (let ((mem (make-persistent-memory :auto-save nil)))
    (dotimes (i 5) (mem-append mem "capped" i :max-entries 3))
    (ok (= 3 (length (mem-get-list mem "capped"))))
    (ok (equal '(2 3 4) (mem-get-list mem "capped")))))

(deftest counters
  (let ((mem (make-persistent-memory :auto-save nil)))
    (ok (= 0 (mem-get-number mem "cnt")))
    (mem-increment mem "cnt")
    (mem-increment mem "cnt" 5)
    (ok (= 6 (mem-get-number mem "cnt")))))

(deftest key-prefix-filter
  (let ((mem (make-persistent-memory :auto-save nil)))
    (mem-set mem "ks:foo:total" 1)
    (mem-set mem "ks:foo:success" 1)
    (mem-set mem "ks:bar:total" 2)
    (mem-set mem "task:x" "y")
    (ok (= 2 (length (mem-keys mem "ks:foo"))))
    (ok (= 3 (length (mem-keys mem "ks:"))))))

(deftest persistence-roundtrip
  (let ((path (merge-pathnames "demiurge-test-mem.json" (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (let ((mem (make-persistent-memory :path path :auto-save nil)))
             (mem-set mem "key1" "val1")
             (mem-set mem "key2" 42)
             (mem-append mem "list1" "a")
             (mem-append mem "list1" "b")
             (mem-save mem))
           (let ((mem2 (make-persistent-memory :path path :auto-save nil)))
             (ok (equal "val1" (mem-get mem2 "key1")))
             (ok (= 42 (mem-get mem2 "key2")))
             (ok (equal '("a" "b") (mem-get-list mem2 "list1")))))
      (when (probe-file path)
        (delete-file path)))))

(deftest transaction-batches-saves
  (let ((path (merge-pathnames "demiurge-test-txn.json" (uiop:temporary-directory))))
    (unwind-protect
         (let ((mem (make-persistent-memory :path path :auto-save t)))
           (with-memory-transaction (mem)
             (mem-set mem "a" 1)
             (mem-set mem "b" 2)
             (mem-set mem "c" 3))
           ;; Should exist on disk now
           (ok (probe-file path))
           (let ((mem2 (make-persistent-memory :path path)))
             (ok (= 1 (mem-get mem2 "a")))
             (ok (= 3 (mem-get mem2 "c")))))
      (when (probe-file path)
        (delete-file path)))))

;;; --- Memory keys (high-level API) ---

(deftest ks-performance-tracking
  (let ((mem (make-persistent-memory :auto-save nil)))
    (record-ks-execution mem "test-ks" :success t :duration-ms 100)
    (record-ks-execution mem "test-ks" :success t :duration-ms 200)
    (record-ks-execution mem "test-ks" :success nil :duration-ms 50)
    (ok (< 0.6 (ks-success-rate mem "test-ks") 0.7))
    (ok (numberp (ks-avg-duration mem "test-ks")))))

(deftest task-history
  (let ((mem (make-persistent-memory :auto-save nil)))
    (record-task-result mem :task-id "t1" :description "fix bug" :status :completed)
    (record-task-result mem :task-id "t2" :description "add feature" :status :failed)
    (ok (= 2 (length (recent-tasks mem 10))))))

(deftest learned-patterns
  (let ((mem (make-persistent-memory :auto-save nil)))
    (remember mem "test-framework" "Use rove for all CL test suites"
              :confidence 0.9 :source "user")
    (ok (equal "Use rove for all CL test suites" (recall mem "test-framework")))
    (forget mem "test-framework")
    (ok (null (recall mem "test-framework")))))

(deftest preferences
  (let ((mem (make-persistent-memory :auto-save nil)))
    (set-preference mem "model" "gemma-3")
    (ok (equal "gemma-3" (get-preference mem "model")))
    (ok (equal "default" (get-preference mem "nonexistent" "default")))))

(deftest interaction-log
  (let ((mem (make-persistent-memory :auto-save nil)))
    (log-interaction mem :role "user" :content "fix the bug" :model "gemma")
    (log-interaction mem :role "assistant" :content "done" :model "gemma" :tokens 42)
    (ok (= 2 (length (recent-interactions mem 10))))))
