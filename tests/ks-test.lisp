(defpackage #:demiurge/tests/ks-test
  (:use #:cl #:rove)
  (:import-from #:demiurge/src/blackboard/core
                #:make-blackboard #:read-section #:write-section)
  (:import-from #:demiurge/src/knowledge-source/protocol
                #:knowledge-source #:ks-name #:ks-version #:ks-priority
                #:ks-precondition #:ks-execute #:ks-postcondition)
  (:import-from #:demiurge/src/knowledge-source/registry
                #:register-ks #:find-ks #:list-ks)
  (:import-from #:demiurge/src/knowledge-source/versioned
                #:make-versioned-ks #:current-version #:candidate-version
                #:version-metrics #:promote-candidate)
  (:import-from #:demiurge/src/controller/scheduler
                #:find-eligible-ks #:schedule-next-ks))

(in-package #:demiurge/tests/ks-test)

;;; Mock KS
(defclass echo-ks (knowledge-source) ())

(defmethod ks-precondition ((ks echo-ks) bb)
  (not (null (read-section bb :input))))

(defmethod ks-execute ((ks echo-ks) bb)
  (let ((input (read-section bb :input)))
    (write-section bb :output (format nil "Echo: ~A" input))
    input))

(defclass always-ks (knowledge-source) ())

(defmethod ks-execute ((ks always-ks) bb)
  (declare (ignore bb))
  :done)

(deftest test-register-and-find-ks
  (let ((bb (make-blackboard))
        (ks (make-instance 'echo-ks :name "echo" :version "1.0")))
    (register-ks bb ks)
    (ok (find-ks bb "echo"))
    (ok (= 1 (length (list-ks bb))))))

(deftest test-ks-precondition
  (let ((bb (make-blackboard))
        (ks (make-instance 'echo-ks :name "echo")))
    (ok (not (ks-precondition ks bb)))
    (write-section bb :input "test")
    (ok (ks-precondition ks bb))))

(deftest test-ks-execute
  (let ((bb (make-blackboard))
        (ks (make-instance 'echo-ks :name "echo")))
    (write-section bb :input "hello")
    (ks-execute ks bb)
    (ok (string= "Echo: hello" (read-section bb :output)))))

(deftest test-scheduler
  (let ((bb (make-blackboard))
        (ks1 (make-instance 'echo-ks :name "echo" :priority 1))
        (ks2 (make-instance 'always-ks :name "always" :priority 10)))
    (register-ks bb ks1)
    (register-ks bb ks2)
    ;; echo-ks precondition not met (no :input), but always-ks always eligible
    (let ((next (schedule-next-ks bb)))
      (ok next)
      (ok (string= "always" (ks-name next))))))

(deftest test-versioned-ks
  (let* ((v1 (make-instance 'echo-ks :name "echo" :version "1.0"))
         (v2 (make-instance 'echo-ks :name "echo" :version "2.0"))
         (versioned (make-versioned-ks v1 :candidate v2 :split-ratio 1.0))
         (bb (make-blackboard)))
    (write-section bb :input "test")
    ;; With split-ratio 1.0, always uses candidate
    (ks-execute versioned bb)
    (ok (= 1 (length (gethash :candidate (version-metrics versioned)))))
    ;; Promote candidate
    (promote-candidate versioned)
    (ok (eq v2 (current-version versioned)))
    (ok (null (candidate-version versioned)))))
