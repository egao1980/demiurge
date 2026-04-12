(defpackage #:demiurge/tests/introspection-test
  (:use #:cl #:rove)
  (:import-from #:demiurge/src/introspection/object-registry
                #:make-object-registry #:register-object #:lookup-object #:inspectable-p)
  (:import-from #:demiurge/src/introspection/inspect
                #:inspect-object)
  (:import-from #:demiurge/src/introspection/render
                #:render-bb-summary #:render-capabilities)
  (:import-from #:demiurge/src/blackboard/core
                #:make-blackboard #:write-section)
  (:import-from #:demiurge/src/capabilities/registry
                #:register-capability)
  (:import-from #:demiurge/src/capabilities/llm
                #:llm-generation-capability))

(in-package #:demiurge/tests/introspection-test)

(deftest test-inspectable-p
  (ok (not (inspectable-p 42)))
  (ok (not (inspectable-p "hello")))
  (ok (not (inspectable-p :keyword)))
  (ok (not (inspectable-p nil)))
  (ok (inspectable-p (list 1 2 3)))
  (ok (inspectable-p (make-hash-table))))

(deftest test-object-registry
  (let ((reg (make-object-registry :capacity 5)))
    (let ((id1 (register-object reg (list 1 2 3))))
      (ok (numberp id1))
      (ok (equal '(1 2 3) (lookup-object reg id1))))))

(deftest test-registry-eviction
  (let ((reg (make-object-registry :capacity 3)))
    (let ((id1 (register-object reg "first")))
      (register-object reg "second")
      (register-object reg "third")
      ;; id1 should still be here
      (ok (lookup-object reg id1))
      ;; Fourth should evict first
      (register-object reg "fourth")
      (ok (null (lookup-object reg id1))))))

(deftest test-inspect-list
  (let* ((reg (make-object-registry))
         (id (register-object reg (list 1 2 3)))
         (result (inspect-object reg id)))
    (ok result)
    (ok (string= "list" (gethash "kind" result)))
    (ok (= 3 (length (gethash "elements" result))))))

(deftest test-inspect-hash-table
  (let* ((reg (make-object-registry))
         (ht (make-hash-table :test 'equal)))
    (setf (gethash "key" ht) "value")
    (let* ((id (register-object reg ht))
           (result (inspect-object reg id)))
      (ok result)
      (ok (string= "hash-table" (gethash "kind" result)))
      (ok (= 1 (gethash "count" result))))))

(deftest test-render-bb-summary
  (let ((bb (make-blackboard)))
    (write-section bb :tasks '(1 2 3))
    (let ((summary (render-bb-summary bb)))
      (ok (stringp summary))
      (ok (search "Blackboard State" summary))
      (ok (search "TASKS" summary)))))
