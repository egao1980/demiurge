(defpackage #:demiurge/tests/capabilities-test
  (:use #:cl #:rove)
  (:import-from #:demiurge/src/blackboard/core
                #:make-blackboard)
  (:import-from #:demiurge/src/capabilities/protocol
                #:capability #:capability-name #:capability-version
                #:capability-operations)
  (:import-from #:demiurge/src/capabilities/registry
                #:register-capability #:unregister-capability
                #:get-capability #:list-capabilities #:capability-schema)
  (:import-from #:demiurge/src/capabilities/llm
                #:llm-generation-capability #:generate-text))

(in-package #:demiurge/tests/capabilities-test)

;;; Mock LLM capability for testing
(defclass mock-llm (llm-generation-capability)
  ((responses :initarg :responses :initform '("mock response") :accessor mock-responses)))

(defmethod generate-text ((cap mock-llm) messages &key)
  (declare (ignore messages))
  (pop (mock-responses cap)))

(deftest test-register-capability
  (let ((bb (make-blackboard))
        (cap (make-instance 'mock-llm :name :llm-generation :version "0.1.0")))
    (register-capability bb cap)
    (ok (get-capability bb :llm-generation))
    (ok (eq cap (get-capability bb :llm-generation)))))

(deftest test-list-capabilities
  (let ((bb (make-blackboard)))
    (register-capability bb (make-instance 'mock-llm :name :llm-generation))
    (let ((caps (list-capabilities bb)))
      (ok (= 1 (length caps)))
      (ok (eq :llm-generation (getf (first caps) :name))))))

(deftest test-capability-schema
  (let ((cap (make-instance 'mock-llm :name :llm-generation :version "0.1.0")))
    (let ((schema (capability-schema cap)))
      (ok (eq :llm-generation (getf schema :name)))
      (ok (listp (getf schema :operations))))))

(deftest test-invoke-capability
  (let ((cap (make-instance 'mock-llm
                            :name :llm-generation
                            :responses '("hello world"))))
    (ok (string= "hello world" (generate-text cap '())))))

(deftest test-unregister-capability
  (let ((bb (make-blackboard)))
    (register-capability bb (make-instance 'mock-llm :name :llm-generation))
    (ok (get-capability bb :llm-generation))
    (unregister-capability bb :llm-generation)
    (ok (null (get-capability bb :llm-generation)))))
