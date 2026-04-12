(defpackage #:demiurge/src/blackboard/workspace
  (:use #:cl)
  (:import-from #:alexandria #:when-let)
  (:import-from #:demiurge/src/blackboard/core
                #:blackboard #:blackboard-sections #:blackboard-lock
                #:blackboard-workspaces #:blackboard-notify-fn
                #:read-section #:write-section #:list-sections)
  (:export #:workspace #:make-workspace #:workspace-name #:workspace-parent
           #:workspace-blackboard #:workspace-status #:workspace-metadata
           #:workspace-children #:cow-blackboard #:cow-parent #:cow-overrides
           #:fork-workspace #:merge-workspace #:discard-workspace
           #:list-workspaces #:get-workspace
           #:+ws-active+ #:+ws-completed+ #:+ws-failed+ #:+ws-discarded+))

(in-package #:demiurge/src/blackboard/workspace)

(defconstant +ws-active+ :active)
(defconstant +ws-completed+ :completed)
(defconstant +ws-failed+ :failed)
(defconstant +ws-discarded+ :discarded)

(defclass workspace ()
  ((name :initarg :name :reader workspace-name)
   (parent :initarg :parent :reader workspace-parent :initform nil)
   (blackboard :initarg :blackboard :reader workspace-blackboard)
   (status :initform +ws-active+ :accessor workspace-status)
   (metadata :initform (make-hash-table :test 'equal) :accessor workspace-metadata)
   (children :initform nil :accessor workspace-children)))

(defun make-workspace (name bb &key parent)
  (make-instance 'workspace :name name :blackboard bb :parent parent))

(defclass cow-blackboard (blackboard)
  ((parent-bb :initarg :parent-bb :reader cow-parent)
   (overrides :initform (make-hash-table :test 'eq) :reader cow-overrides)))

(defmethod read-section ((bb cow-blackboard) key &key default)
  (bt2:with-lock-held ((blackboard-lock bb))
    (multiple-value-bind (val found) (gethash key (cow-overrides bb))
      (if found val
          (read-section (cow-parent bb) key :default default)))))

(defmethod write-section ((bb cow-blackboard) key value &key merge-fn)
  (bt2:with-lock-held ((blackboard-lock bb))
    (let ((old (gethash key (cow-overrides bb))))
      (setf (gethash key (cow-overrides bb))
            (if (and merge-fn old)
                (funcall merge-fn old value)
                value))
      (when-let (fn (blackboard-notify-fn bb))
        (funcall fn key old value))
      value)))

(defmethod list-sections ((bb cow-blackboard))
  (let ((keys (list-sections (cow-parent bb))))
    (bt2:with-lock-held ((blackboard-lock bb))
      (maphash (lambda (k v) (declare (ignore v))
                 (pushnew k keys))
               (cow-overrides bb)))
    keys))

(defun find-root-bb (bb)
  "Walk COW chain to find root blackboard."
  (loop for b = bb then (cow-parent b)
        while (typep b 'cow-blackboard)
        finally (return b)))

(defun fork-workspace (bb name &key parent)
  "Create an isolated workspace with COW blackboard."
  (let* ((cow-bb (make-instance 'cow-blackboard :parent-bb bb))
         (ws (make-workspace name cow-bb :parent parent)))
    ;; Copy notify-fn from parent
    (setf (blackboard-notify-fn cow-bb) (blackboard-notify-fn bb))
    ;; Register workspace on root BB
    (let ((root-bb (find-root-bb bb)))
      (bt2:with-lock-held ((blackboard-lock root-bb))
        (setf (gethash name (blackboard-workspaces root-bb)) ws)))
    ws))

(defun merge-workspace (ws &key (strategy :overwrite))
  "Apply workspace's COW overrides back to parent."
  (declare (ignore strategy))
  (let ((cow-bb (workspace-blackboard ws)))
    (when (typep cow-bb 'cow-blackboard)
      (bt2:with-lock-held ((blackboard-lock cow-bb))
        (maphash (lambda (key value)
                   (write-section (cow-parent cow-bb) key value))
                 (cow-overrides cow-bb)))))
  (setf (workspace-status ws) +ws-completed+)
  ws)

(defun discard-workspace (ws)
  (setf (workspace-status ws) +ws-discarded+)
  ws)

(defun list-workspaces (bb &key status)
  (let ((root (find-root-bb bb)))
    (bt2:with-lock-held ((blackboard-lock root))
      (let ((result nil))
        (maphash (lambda (name ws)
                   (declare (ignore name))
                   (when (or (null status) (eq (workspace-status ws) status))
                     (push ws result)))
                 (blackboard-workspaces root))
        result))))

(defun get-workspace (bb name)
  (let ((root (find-root-bb bb)))
    (bt2:with-lock-held ((blackboard-lock root))
      (gethash name (blackboard-workspaces root)))))
