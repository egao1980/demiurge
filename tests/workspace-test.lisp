(defpackage #:demiurge/tests/workspace-test
  (:use #:cl #:rove)
  (:import-from #:demiurge/src/blackboard/core
                #:make-blackboard #:read-section #:write-section)
  (:import-from #:demiurge/src/blackboard/workspace
                #:fork-workspace #:merge-workspace #:discard-workspace
                #:workspace-name #:workspace-status #:workspace-blackboard
                #:list-workspaces
                #:+ws-active+ #:+ws-completed+ #:+ws-discarded+))

(in-package #:demiurge/tests/workspace-test)

(deftest test-fork-workspace
  (let* ((bb (make-blackboard))
         (_ (write-section bb :data "original"))
         (ws (fork-workspace bb "test-ws")))
    (declare (ignore _))
    (ok (string= "test-ws" (workspace-name ws)))
    (ok (eq +ws-active+ (workspace-status ws)))
    ;; Can read parent data through COW
    (ok (string= "original" (read-section (workspace-blackboard ws) :data)))))

(deftest test-cow-isolation
  (let* ((bb (make-blackboard))
         (_ (write-section bb :data "original"))
         (ws (fork-workspace bb "isolated")))
    (declare (ignore _))
    ;; Write in workspace doesn't affect parent
    (write-section (workspace-blackboard ws) :data "modified")
    (ok (string= "modified" (read-section (workspace-blackboard ws) :data)))
    (ok (string= "original" (read-section bb :data)))))

(deftest test-merge-workspace
  (let* ((bb (make-blackboard))
         (_ (write-section bb :data "original"))
         (ws (fork-workspace bb "merge-test")))
    (declare (ignore _))
    (write-section (workspace-blackboard ws) :data "updated")
    (write-section (workspace-blackboard ws) :new-section "new data")
    (merge-workspace ws)
    (ok (eq +ws-completed+ (workspace-status ws)))
    (ok (string= "updated" (read-section bb :data)))
    (ok (string= "new data" (read-section bb :new-section)))))

(deftest test-discard-workspace
  (let* ((bb (make-blackboard))
         (_ (write-section bb :data "original"))
         (ws (fork-workspace bb "discard-test")))
    (declare (ignore _))
    (write-section (workspace-blackboard ws) :data "will be discarded")
    (discard-workspace ws)
    (ok (eq +ws-discarded+ (workspace-status ws)))
    (ok (string= "original" (read-section bb :data)))))

(deftest test-list-workspaces
  (let ((bb (make-blackboard)))
    (fork-workspace bb "ws-1")
    (fork-workspace bb "ws-2")
    (let ((ws-list (list-workspaces bb)))
      (ok (= 2 (length ws-list))))))

(deftest test-nested-workspaces
  (let* ((bb (make-blackboard))
         (_ (write-section bb :x 1))
         (ws1 (fork-workspace bb "parent-ws")))
    (declare (ignore _))
    (write-section (workspace-blackboard ws1) :y 2)
    (let ((ws2 (fork-workspace (workspace-blackboard ws1) "child-ws")))
      ;; Child can read both grandparent and parent data
      (ok (= 1 (read-section (workspace-blackboard ws2) :x)))
      (ok (= 2 (read-section (workspace-blackboard ws2) :y))))))
