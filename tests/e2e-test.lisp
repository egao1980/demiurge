(defpackage #:demiurge/tests/e2e-test
  (:use #:cl #:rove)
  (:import-from #:demiurge/src/blackboard/core
                #:make-blackboard #:read-section)
  (:import-from #:demiurge/src/capabilities/registry
                #:register-capability #:list-capabilities)
  (:import-from #:demiurge/src/capabilities/llm
                #:llm-generation-capability #:generate-text)
  (:import-from #:demiurge/src/capabilities/compute
                #:compute-capability #:run-command)
  (:import-from #:demiurge/src/capabilities/code-editing
                #:code-editing-capability #:read-file #:write-file #:list-files)
  (:import-from #:demiurge/src/capabilities/vcs
                #:version-control-capability #:vcs-status)
  (:import-from #:demiurge/src/controller/agent-loop
                #:run-task #:run-issue-workflow)
  (:import-from #:demiurge/src/blackboard/workspace
                #:workspace-status #:+ws-completed+))

(in-package #:demiurge/tests/e2e-test)

;;; Mock capabilities for e2e testing

(defclass mock-llm (llm-generation-capability) ())
(defmethod generate-text ((cap mock-llm) messages &key)
  (declare (ignore messages))
  "Mock plan: 1. Fix bug in auth.lisp 2. Run tests")

(defclass mock-compute (compute-capability) ())
(defmethod run-command ((cap mock-compute) command &key)
  (declare (ignore command))
  (list 0 "All tests pass" ""))

(defclass mock-editor (code-editing-capability) ())
(defmethod read-file ((cap mock-editor) path &key) (declare (ignore path)) "contents")
(defmethod write-file ((cap mock-editor) path content &key)
  (declare (ignore path content)) t)
(defmethod list-files ((cap mock-editor) dir &key) (declare (ignore dir)) '("a.lisp"))

(defclass mock-vcs (version-control-capability) ())
(defmethod vcs-status ((cap mock-vcs) path &key) (declare (ignore path)) "clean")

(defun make-e2e-bb ()
  (let ((bb (make-blackboard)))
    (register-capability bb (make-instance 'mock-llm :name :llm-generation))
    (register-capability bb (make-instance 'mock-compute :name :compute))
    (register-capability bb (make-instance 'mock-editor :name :code-editing))
    (register-capability bb (make-instance 'mock-vcs :name :version-control))
    bb))

(deftest test-run-task
  (let ((bb (make-e2e-bb)))
    (multiple-value-bind (ws status) (run-task bb "Fix the auth bug")
      (ok (eq :completed status))
      (ok (eq +ws-completed+ (workspace-status ws))))))

(deftest test-run-issue-workflow
  (let ((bb (make-e2e-bb)))
    (multiple-value-bind (ws status steps)
        (run-issue-workflow bb (list :id 42 :title "Auth is broken"))
      (ok (eq :completed status))
      ;; With mock capabilities, we should get at least some steps
      (ok (listp steps))
      (ok (plusp (length steps))))))

(deftest test-capabilities-used
  (let ((bb (make-e2e-bb)))
    (ok (= 4 (length (list-capabilities bb))))
    (run-task bb "Test task")
    (ok (string= "Mock plan: 1. Fix bug in auth.lisp 2. Run tests"
                  (read-section bb :plan)))))
