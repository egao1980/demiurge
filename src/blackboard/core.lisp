(defpackage #:demiurge/src/blackboard/core
  (:use #:cl)
  (:import-from #:alexandria #:when-let)
  (:export #:blackboard #:make-blackboard
           #:read-section #:write-section #:remove-section
           #:list-sections #:blackboard-lock #:blackboard-sections
           #:blackboard-notify-fn
           #:blackboard-capabilities #:blackboard-workspaces
           #:blackboard-ks-registry
           ;; Watcher API
           #:watcher #:make-watcher #:watcher-id #:watcher-requires
           #:watcher-handler #:watcher-priority #:watcher-one-shot-p
           #:watch #:unwatch #:list-watchers #:get-watcher
           ;; KSAR
           #:ksar #:make-ksar #:ksar-id #:ksar-watcher-id
           #:ksar-triggered-key #:ksar-trigger-time #:ksar-priority
           #:ksar-context #:ksar-status
           ;; Agenda + Scheduler
           #:bb-agenda #:bb-watchers #:bb-watchers-lock
           #:enqueue-ksar #:pop-agenda #:agenda-contents #:agenda-size
           #:bb-active-count #:bb-max-concurrency
           #:bb-agenda-lock #:bb-agenda-cv
           #:run-scheduler #:stop-scheduler #:bb-scheduler-running-p
           #:bb-scheduler-thread
           #:record-bb-error
           ;; Internal helpers needed by cow-blackboard
           #:check-watchers-and-enqueue #:snapshot-context
           #:all-requires-present-p))

(in-package #:demiurge/src/blackboard/core)

;;; ---------------------------------------------------------------------------
;;; Watcher — KS trigger pattern
;;; ---------------------------------------------------------------------------

(defstruct watcher
  (id nil :type symbol)
  (requires nil :type list)
  (handler nil :type (or function null))
  (priority 0 :type fixnum)
  (one-shot-p nil :type boolean))

;;; ---------------------------------------------------------------------------
;;; KSAR — Knowledge Source Activation Record
;;; ---------------------------------------------------------------------------

(defvar *ksar-counter* 0)

(defstruct ksar
  (id (incf *ksar-counter*))
  (watcher-id nil :type symbol)
  (triggered-key nil :type symbol)
  (trigger-time (get-internal-real-time))
  (priority 0 :type fixnum)
  (context nil :type list)
  (status :pending :type keyword))

;;; ---------------------------------------------------------------------------
;;; Priority queue (sorted list, simple and correct)
;;; ---------------------------------------------------------------------------

(defstruct pqueue
  (items nil :type list))

(defun pqueue-push (pq ksar)
  "Insert KSAR into priority queue sorted by descending priority, FIFO tiebreak."
  (let ((pri (ksar-priority ksar)))
    (if (or (null (pqueue-items pq))
            (> pri (ksar-priority (first (pqueue-items pq)))))
        (push ksar (pqueue-items pq))
        (loop for cell on (pqueue-items pq)
              when (or (null (cdr cell))
                       (> pri (ksar-priority (cadr cell))))
                do (setf (cdr cell) (cons ksar (cdr cell)))
                   (return)))))

(defun pqueue-pop (pq)
  "Remove and return the highest-priority KSAR, or NIL."
  (pop (pqueue-items pq)))

(defun pqueue-size (pq)
  (length (pqueue-items pq)))

(defun pqueue-contents (pq)
  (copy-list (pqueue-items pq)))

;;; ---------------------------------------------------------------------------
;;; Blackboard
;;; ---------------------------------------------------------------------------

(defclass blackboard ()
  ((sections :initform (make-hash-table :test 'eq) :reader blackboard-sections)
   (lock :initform (bt2:make-lock :name "blackboard") :reader blackboard-lock)
   (notify-fn :initarg :notify-fn :accessor blackboard-notify-fn :initform nil
              :documentation "Called with (key old-value new-value) on section change.")
   (capabilities :initform (make-hash-table :test 'eq) :accessor blackboard-capabilities)
   (workspaces :initform (make-hash-table :test 'equal) :accessor blackboard-workspaces)
   (ks-registry :initform (make-hash-table :test 'equal) :accessor blackboard-ks-registry)
   ;; Watcher system
   (watchers :initform (make-hash-table :test 'eq) :accessor bb-watchers)
   (watchers-lock :initform (bt2:make-lock :name "bb-watchers") :reader bb-watchers-lock)
   ;; Agenda
   (agenda :initform (make-pqueue) :accessor bb-agenda)
   (agenda-lock :initform (bt2:make-lock :name "bb-agenda") :reader bb-agenda-lock)
   (agenda-cv :initform (bt2:make-condition-variable :name "bb-agenda-cv") :reader bb-agenda-cv)
   ;; Scheduler
   (scheduler-running :initform nil :accessor bb-scheduler-running-p)
   (scheduler-thread :initform nil :accessor bb-scheduler-thread)
   (max-concurrency :initarg :max-concurrency :initform 4 :accessor bb-max-concurrency)
   (active-count :initform 0 :accessor bb-active-count)
   (active-lock :initform (bt2:make-lock :name "bb-active") :reader bb-active-lock)
   (active-cv :initform (bt2:make-condition-variable :name "bb-active-cv") :reader bb-active-cv)))

(defun make-blackboard (&key notify-fn (max-concurrency 4))
  (make-instance 'blackboard :notify-fn notify-fn :max-concurrency max-concurrency))

;;; ---------------------------------------------------------------------------
;;; Section CRUD
;;; ---------------------------------------------------------------------------

(defgeneric read-section (bb key &key default)
  (:documentation "Read a section from the blackboard."))

(defgeneric write-section (bb key value &key merge-fn)
  (:documentation "Write a section. On change, checks watchers and enqueues KSARs."))

(defgeneric remove-section (bb key)
  (:documentation "Remove a section from the blackboard."))

(defgeneric list-sections (bb)
  (:documentation "List all section keys."))

(defmethod read-section ((bb blackboard) key &key default)
  (bt2:with-lock-held ((blackboard-lock bb))
    (gethash key (blackboard-sections bb) default)))

(defun %read-section-unlocked (bb key)
  "Read without locking — caller must hold blackboard-lock."
  (gethash key (blackboard-sections bb)))

(defmethod write-section ((bb blackboard) key value &key merge-fn)
  (let ((old nil)
        (new-val nil)
        (changed-p nil))
    (bt2:with-lock-held ((blackboard-lock bb))
      (setf old (gethash key (blackboard-sections bb)))
      (setf new-val (if (and merge-fn old) (funcall merge-fn old value) value))
      (setf changed-p (not (equal old new-val)))
      (setf (gethash key (blackboard-sections bb)) new-val))
    ;; Notify legacy hook
    (when (and changed-p (blackboard-notify-fn bb))
      (funcall (blackboard-notify-fn bb) key old new-val))
    ;; Check watchers and enqueue KSARs (only on change)
    (when changed-p
      (check-watchers-and-enqueue bb key new-val))
    new-val))

(defmethod remove-section ((bb blackboard) key)
  (bt2:with-lock-held ((blackboard-lock bb))
    (remhash key (blackboard-sections bb))))

(defmethod list-sections ((bb blackboard))
  (bt2:with-lock-held ((blackboard-lock bb))
    (let ((keys nil))
      (maphash (lambda (k v) (declare (ignore v)) (push k keys))
               (blackboard-sections bb))
      keys)))

;;; ---------------------------------------------------------------------------
;;; Watcher API
;;; ---------------------------------------------------------------------------

(defun watch (bb &key id requires handler (priority 0) (one-shot nil))
  "Register a watcher. If all required keys are already present, immediately enqueues a KSAR."
  (let ((w (make-watcher :id id :requires requires :handler handler
                         :priority priority :one-shot-p one-shot)))
    (bt2:with-lock-held ((bb-watchers-lock bb))
      (setf (gethash id (bb-watchers bb)) w))
    ;; Check if preconditions are already satisfied
    (when (all-requires-present-p bb requires)
      (let ((ctx (snapshot-context bb requires)))
        (enqueue-ksar bb (make-ksar :watcher-id id
                                    :triggered-key :initial
                                    :priority priority
                                    :context ctx))
        (when one-shot
          (bt2:with-lock-held ((bb-watchers-lock bb))
            (remhash id (bb-watchers bb))))))
    w))

(defun unwatch (bb id)
  "Remove a watcher by ID."
  (bt2:with-lock-held ((bb-watchers-lock bb))
    (remhash id (bb-watchers bb))))

(defun get-watcher (bb id)
  "Look up a watcher by ID."
  (bt2:with-lock-held ((bb-watchers-lock bb))
    (gethash id (bb-watchers bb))))

(defun list-watchers (bb)
  "Return list of watcher structs."
  (bt2:with-lock-held ((bb-watchers-lock bb))
    (let ((result nil))
      (maphash (lambda (k v) (declare (ignore k)) (push v result))
               (bb-watchers bb))
      result)))

;;; ---------------------------------------------------------------------------
;;; Watcher checking (called from write-section)
;;; ---------------------------------------------------------------------------

(defun all-requires-present-p (bb requires)
  "Check that all keys in REQUIRES have values on BB."
  (bt2:with-lock-held ((blackboard-lock bb))
    (every (lambda (key) (nth-value 1 (gethash key (blackboard-sections bb))))
           requires)))

(defun snapshot-context (bb requires)
  "Build an alist of (key . value) for all REQUIRES keys."
  (bt2:with-lock-held ((blackboard-lock bb))
    (loop for key in requires
          collect (cons key (gethash key (blackboard-sections bb))))))

(defun check-watchers-and-enqueue (bb triggered-key new-value)
  "Check all watchers for TRIGGERED-KEY change. Enqueue KSARs for satisfied ones."
  (declare (ignore new-value))
  (let ((to-fire nil)
        (to-remove nil))
    ;; Collect matching watchers under lock
    (bt2:with-lock-held ((bb-watchers-lock bb))
      (maphash (lambda (id w)
                 (when (member triggered-key (watcher-requires w) :test #'eq)
                   (when (all-requires-present-p bb (watcher-requires w))
                     (push w to-fire)
                     (when (watcher-one-shot-p w)
                       (push id to-remove)))))
               (bb-watchers bb))
      ;; Remove one-shot watchers
      (dolist (id to-remove)
        (remhash id (bb-watchers bb))))
    ;; Enqueue KSARs outside watcher lock
    (dolist (w to-fire)
      (let ((ctx (snapshot-context bb (watcher-requires w))))
        (enqueue-ksar bb (make-ksar :watcher-id (watcher-id w)
                                    :triggered-key triggered-key
                                    :priority (watcher-priority w)
                                    :context ctx))))))

;;; ---------------------------------------------------------------------------
;;; Agenda
;;; ---------------------------------------------------------------------------

(defun enqueue-ksar (bb ksar)
  "Push a KSAR onto the agenda and signal the scheduler."
  (bt2:with-lock-held ((bb-agenda-lock bb))
    (pqueue-push (bb-agenda bb) ksar)
    (bt2:condition-notify (bb-agenda-cv bb))))

(defun pop-agenda (bb &key (timeout 1.0))
  "Pop the highest-priority KSAR. Blocks up to TIMEOUT seconds if empty."
  (bt2:with-lock-held ((bb-agenda-lock bb))
    (loop
      (when-let (ksar (pqueue-pop (bb-agenda bb)))
        (return ksar))
      (unless (bb-scheduler-running-p bb)
        (return nil))
      (bt2:condition-wait (bb-agenda-cv bb) (bb-agenda-lock bb) :timeout timeout))))

(defun agenda-contents (bb)
  "Return a copy of all pending KSARs."
  (bt2:with-lock-held ((bb-agenda-lock bb))
    (pqueue-contents (bb-agenda bb))))

(defun agenda-size (bb)
  "Number of pending KSARs."
  (bt2:with-lock-held ((bb-agenda-lock bb))
    (pqueue-size (bb-agenda bb))))

;;; ---------------------------------------------------------------------------
;;; Scheduler
;;; ---------------------------------------------------------------------------

(defun wait-for-slot (bb)
  "Block until active-count < max-concurrency."
  (bt2:with-lock-held ((bb-active-lock bb))
    (loop while (>= (bb-active-count bb) (bb-max-concurrency bb))
          do (bt2:condition-wait (bb-active-cv bb) (bb-active-lock bb) :timeout 0.5))))

(defun claim-slot (bb)
  (bt2:with-lock-held ((bb-active-lock bb))
    (incf (bb-active-count bb))))

(defun release-slot (bb)
  (bt2:with-lock-held ((bb-active-lock bb))
    (decf (bb-active-count bb))
    (bt2:condition-notify (bb-active-cv bb))))

(defun record-bb-error (bb ksar-or-nil condition)
  "Append an error record to the BB :errors section for later analysis."
  (let ((entry (list :time (get-universal-time)
                     :condition (format nil "~A" condition)
                     :type (type-of condition)
                     :ksar-id (when ksar-or-nil (ksar-id ksar-or-nil))
                     :watcher-id (when ksar-or-nil (ksar-watcher-id ksar-or-nil)))))
    (handler-case
        (let* ((lock (blackboard-lock bb))
               (existing (bt2:with-lock-held (lock)
                           (read-section bb :errors))))
          (write-section bb :errors (cons entry (if (listp existing) existing nil))))
      (serious-condition () nil))))

(defun run-ksar-handler (bb ksar watcher)
  "Execute a single KSAR's handler with full error recovery. Never signals."
  (handler-case
      (progn
        (setf (ksar-status ksar) :running)
        (funcall (watcher-handler watcher) bb ksar)
        (setf (ksar-status ksar) :completed))
    (serious-condition (e)
      (setf (ksar-status ksar) :failed)
      (format *error-output* "~&KSAR ~A (~A) failed: ~A~%"
              (ksar-id ksar) (ksar-watcher-id ksar) e)
      (record-bb-error bb ksar e))))

(defun run-scheduler (bb)
  "Scheduler thread main loop. Pops KSARs from agenda, submits to lparallel.
Respects max-concurrency. Blocks when agenda is empty.
Catches all serious-conditions to prevent daemon death."
  (setf (bb-scheduler-running-p bb) t)
  (loop while (bb-scheduler-running-p bb) do
    (handler-case
        (let ((ksar (pop-agenda bb :timeout 1.0)))
          (when ksar
            (wait-for-slot bb)
            (claim-slot bb)
            (let ((watcher (get-watcher bb (ksar-watcher-id ksar))))
              (if (and watcher (watcher-handler watcher))
                  (if lparallel:*kernel*
                      (lparallel:future
                        (unwind-protect
                             (run-ksar-handler bb ksar watcher)
                          (release-slot bb)))
                      (unwind-protect
                           (run-ksar-handler bb ksar watcher)
                        (release-slot bb)))
                  (release-slot bb)))))
      (serious-condition (e)
        (format *error-output* "~&[scheduler] Unhandled condition in loop iteration: ~A~%" e)
        (record-bb-error bb nil e)
        (sleep 1)))))

(defun stop-scheduler (bb &key (wait-seconds 10))
  "Signal the scheduler to stop and optionally wait for it."
  (setf (bb-scheduler-running-p bb) nil)
  ;; Wake the scheduler if it's blocked on the CV
  (bt2:with-lock-held ((bb-agenda-lock bb))
    (bt2:condition-notify (bb-agenda-cv bb)))
  (when-let (thread (bb-scheduler-thread bb))
    (when (bt2:thread-alive-p thread)
      ;; bt2:join-thread doesn't support :timeout in all versions; use a polling wait
      (let ((deadline (+ (get-internal-real-time)
                         (* wait-seconds internal-time-units-per-second))))
        (loop while (and (bt2:thread-alive-p thread)
                         (< (get-internal-real-time) deadline))
              do (sleep 0.1))
        (when (not (bt2:thread-alive-p thread))
          (ignore-errors (bt2:join-thread thread)))))
    (setf (bb-scheduler-thread bb) nil)))
