(defpackage #:demiurge/src/blackboard/workspace
  (:use #:cl)
  (:import-from #:alexandria #:when-let)
  (:import-from #:demiurge/src/blackboard/core
                #:blackboard #:blackboard-sections #:blackboard-lock
                #:blackboard-workspaces #:blackboard-notify-fn
                #:blackboard-capabilities #:blackboard-ks-registry
                #:read-section #:write-section #:list-sections
                #:bb-watchers #:bb-watchers-lock
                #:bb-agenda #:bb-agenda-lock #:bb-agenda-cv
                #:check-watchers-and-enqueue #:enqueue-ksar
                #:watcher #:make-watcher #:watcher-id #:watcher-requires
                #:watcher-one-shot-p #:watcher-priority
                #:make-ksar #:snapshot-context #:all-requires-present-p)
  (:export #:workspace #:make-workspace #:workspace-name #:workspace-parent
           #:workspace-blackboard #:workspace-status #:workspace-metadata
           #:workspace-children #:cow-blackboard #:cow-parent #:cow-overrides
           #:fork-workspace #:merge-workspace #:discard-workspace
           #:list-workspaces #:get-workspace #:find-root-bb
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

;;; ---------------------------------------------------------------------------
;;; COW Blackboard — workspace-local overrides, own watchers, shared root agenda
;;; ---------------------------------------------------------------------------

(defclass cow-blackboard (blackboard)
  ((parent-bb :initarg :parent-bb :reader cow-parent)
   (overrides :initform (make-hash-table :test 'eq) :reader cow-overrides)
   (root-bb :initarg :root-bb :reader cow-root-bb :initform nil
            :documentation "Root BB for centralized agenda scheduling.")))

(defmethod read-section ((bb cow-blackboard) key &key default)
  (bt2:with-lock-held ((blackboard-lock bb))
    (multiple-value-bind (val found) (gethash key (cow-overrides bb))
      (if found val
          (read-section (cow-parent bb) key :default default)))))

(defmethod write-section ((bb cow-blackboard) key value &key merge-fn)
  (let ((old nil)
        (new-val nil)
        (changed-p nil))
    (bt2:with-lock-held ((blackboard-lock bb))
      (setf old (gethash key (cow-overrides bb)))
      (setf new-val (if (and merge-fn old) (funcall merge-fn old value) value))
      (setf changed-p (not (equal old new-val)))
      (setf (gethash key (cow-overrides bb)) new-val))
    ;; Notify legacy hook
    (when (and changed-p (blackboard-notify-fn bb))
      (funcall (blackboard-notify-fn bb) key old new-val))
    ;; Check LOCAL watchers and enqueue KSARs to ROOT agenda
    (when changed-p
      (check-cow-watchers-and-enqueue bb key new-val))
    new-val))

(defmethod list-sections ((bb cow-blackboard))
  (let ((keys (list-sections (cow-parent bb))))
    (bt2:with-lock-held ((blackboard-lock bb))
      (maphash (lambda (k v) (declare (ignore v))
                 (pushnew k keys))
               (cow-overrides bb)))
    keys))

(defun check-cow-watchers-and-enqueue (cow-bb triggered-key new-value)
  "Check COW-BB's own watchers. Enqueue KSARs to the root BB's agenda."
  (declare (ignore new-value))
  (let ((root (or (cow-root-bb cow-bb) (find-root-bb cow-bb)))
        (to-fire nil)
        (to-remove nil))
    (bt2:with-lock-held ((bb-watchers-lock cow-bb))
      (maphash (lambda (id w)
                 (when (member triggered-key (watcher-requires w) :test #'eq)
                   ;; Check requires against this COW BB (reads through to parent)
                   (when (cow-all-requires-present-p cow-bb (watcher-requires w))
                     (push w to-fire)
                     (when (watcher-one-shot-p w)
                       (push id to-remove)))))
               (bb-watchers cow-bb))
      (dolist (id to-remove)
        (remhash id (bb-watchers cow-bb))))
    ;; Enqueue to ROOT agenda
    (dolist (w to-fire)
      (let ((ctx (cow-snapshot-context cow-bb (watcher-requires w))))
        (enqueue-ksar root (make-ksar :watcher-id (watcher-id w)
                                      :triggered-key triggered-key
                                      :priority (watcher-priority w)
                                      :context ctx))))))

(defun cow-all-requires-present-p (cow-bb requires)
  "Check requires presence via COW read path (checks overrides then parent)."
  (every (lambda (key)
           (bt2:with-lock-held ((blackboard-lock cow-bb))
             (multiple-value-bind (val found) (gethash key (cow-overrides cow-bb))
               (declare (ignore val))
               (if found t
                   ;; Fall through to parent
                   (nth-value 1 (gethash key (blackboard-sections (cow-parent cow-bb))))))))
         requires))

(defun cow-snapshot-context (cow-bb requires)
  "Build context alist reading through COW."
  (loop for key in requires
        collect (cons key (read-section cow-bb key))))

;;; ---------------------------------------------------------------------------
;;; Workspace operations
;;; ---------------------------------------------------------------------------

(defun find-root-bb (bb)
  "Walk COW chain to find root blackboard."
  (loop for b = bb then (cow-parent b)
        while (typep b 'cow-blackboard)
        finally (return b)))

(defun fork-workspace (bb name &key parent)
  "Create an isolated workspace with COW blackboard.
Workspace gets its own watchers (empty) but shares root BB's agenda for scheduling."
  (let* ((root (find-root-bb bb))
         (cow-bb (make-instance 'cow-blackboard :parent-bb bb :root-bb root))
         (ws (make-workspace name cow-bb :parent parent)))
    ;; Share parent's capabilities and KS registry
    (setf (blackboard-capabilities cow-bb) (blackboard-capabilities bb)
          (blackboard-ks-registry cow-bb) (blackboard-ks-registry bb))
    ;; Register workspace on root BB
    (bt2:with-lock-held ((blackboard-lock root))
      (setf (gethash name (blackboard-workspaces root)) ws))
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
