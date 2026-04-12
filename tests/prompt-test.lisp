(defpackage #:demiurge/tests/prompt-test
  (:use #:cl #:rove)
  (:import-from #:demiurge/src/blackboard/core
                #:make-blackboard #:write-section #:read-section)
  (:import-from #:demiurge/src/capabilities/registry
                #:register-capability)
  (:import-from #:demiurge/src/capabilities/llm
                #:llm-generation-capability)
  (:import-from #:demiurge/src/persistence/memory
                #:make-persistent-memory)
  (:import-from #:demiurge/src/persistence/memory-keys
                #:set-preference #:remember #:record-task-result)
  (:import-from #:demiurge/src/controller/prompts
                #:build-supervisor-prompt #:build-task-prompt
                #:build-review-prompt #:build-self-improve-prompt
                #:*identity-preamble* #:*architecture-section*
                #:format-capability-catalog #:format-memory-context))

(in-package #:demiurge/tests/prompt-test)

(defclass mock-llm (llm-generation-capability) ())
(defmethod demiurge/src/capabilities/llm:generate-text ((c mock-llm) messages &key)
  (declare (ignore messages)) "mock")

(defun make-test-bb ()
  (let ((bb (make-blackboard)))
    (register-capability bb (make-instance 'mock-llm))
    (write-section bb :project "demiurge")
    (write-section bb :status :idle)
    bb))

(deftest supervisor-prompt-includes-identity
  (let ((prompt (build-supervisor-prompt (make-test-bb))))
    (ok (search "Demiurge" prompt))
    (ok (search "autonomous" prompt))))

(deftest supervisor-prompt-includes-architecture
  (let ((prompt (build-supervisor-prompt (make-test-bb))))
    (ok (search "Blackboard" prompt))
    (ok (search "Workspace" prompt))
    (ok (search "Capabilities" prompt))
    (ok (search "Knowledge Sources" prompt))))

(deftest supervisor-prompt-includes-capabilities
  (let ((prompt (build-supervisor-prompt (make-test-bb))))
    (ok (search "llm-generation" prompt))))

(deftest supervisor-prompt-includes-bb-state
  (let ((prompt (build-supervisor-prompt (make-test-bb))))
    (ok (search "PROJECT" prompt))
    (ok (search "demiurge" prompt))))

(deftest supervisor-prompt-includes-response-format
  (let ((prompt (build-supervisor-prompt (make-test-bb))))
    (ok (search "\"action\"" prompt))
    (ok (search "execute" prompt))
    (ok (search "schedule-ks" prompt))
    (ok (search "improve" prompt))
    (ok (search "idle" prompt))))

(deftest supervisor-prompt-with-memory
  (let ((bb (make-test-bb))
        (mem (make-persistent-memory :auto-save nil)))
    (set-preference mem "model" "gemma-3")
    (remember mem "rove" "Use rove for tests")
    (record-task-result mem :task-id "t1" :description "fix bug" :status :completed)
    (let ((prompt (build-supervisor-prompt bb :mem mem)))
      (ok (search "gemma-3" prompt))
      (ok (search "rove" prompt))
      (ok (search "fix bug" prompt)))))

(deftest task-prompt-includes-task-context
  (let ((prompt (build-task-prompt (make-test-bb) "Fix authentication bug")))
    (ok (search "Fix authentication bug" prompt))
    (ok (search "task" prompt))))

(deftest review-prompt-structure
  (let ((prompt (build-review-prompt (make-test-bb) "task-123")))
    (ok (search "task-123" prompt))
    (ok (search "Review" prompt))
    (ok (search "merge" prompt))))

(deftest capability-catalog-formatting
  (let ((catalog (format-capability-catalog (make-test-bb))))
    (ok (search "llm-generation" catalog))
    (ok (search "Capabilities" catalog))))

(deftest memory-context-with-no-memory
  (ok (equal "" (format-memory-context nil))))
