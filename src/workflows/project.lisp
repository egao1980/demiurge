(in-package #:demiurge/workflows)

(defclass project-milestone ()
  ((name :initarg :name :accessor milestone-name)
   (prompt :initarg :prompt :accessor milestone-prompt :initform nil)))

(defun project-milestone-p (x)
  (typep x 'project-milestone))

(defun make-project-milestone (&key name prompt)
  (make-instance 'project-milestone
                 :name (string-downcase (string name))
                 :prompt prompt))

(defun coerce-milestone (value)
  (cond
    ((project-milestone-p value) value)
    ((or (stringp value) (symbolp value))
     (make-project-milestone :name value))
    ((and (consp value) (keywordp (first value)))
     (make-project-milestone :name (or (getf value :name) (getf value :milestone))
                             :prompt (getf value :prompt)))
    (t
     (restart-case
         (error 'workflows-error
                :message (format nil "cannot coerce ~s to project-milestone" value))
       (use-value (ms)
         :report "Use a supplied milestone"
         (coerce-milestone ms))))))

(defclass project-spec ()
  ((name :initarg :name :accessor project-spec-name :initform nil)
   (milestones :initarg :milestones :accessor project-spec-milestones
               :initform nil)
   (schedule :initarg :schedule :accessor project-spec-schedule :initform nil)
   (board :initarg :board :accessor project-spec-board :initform nil)))

(defun project-spec-p (x)
  (typep x 'project-spec))

(defun make-project-spec (&key name milestones schedule board)
  (make-instance 'project-spec
                 :name (and name (string-downcase (string name)))
                 :milestones (mapcar #'coerce-milestone milestones)
                 :schedule schedule
                 :board board))

(defun coerce-project-spec (value)
  (cond
    ((project-spec-p value) value)
    ((and (consp value) (keywordp (first value)))
     (make-project-spec :name (getf value :name)
                        :milestones (getf value :milestones)
                        :schedule (getf value :schedule)
                        :board (getf value :board)))
    (t
     (restart-case
         (error 'workflows-error
                :message (format nil "cannot coerce ~s to project-spec" value))
       (use-value (spec)
         :report "Use a supplied project-spec"
         (coerce-project-spec spec))))))

(defclass project-workflow ()
  ((name :initarg :name :accessor project-workflow-name)
   (domain :initarg :domain :accessor project-workflow-domain :initform nil)
   (board :initarg :board :accessor project-workflow-board :initform nil)
   (task :initarg :task :accessor project-workflow-task :initform nil)
   (spec :initarg :spec :accessor project-workflow-spec :initform nil)
   (status :initarg :status :accessor project-workflow-status :initform :new)))

(defun project-workflow-p (x)
  (typep x 'project-workflow))

(defun make-project-workflow (&key name domain board task spec
                                (status :new))
  (make-instance 'project-workflow
                 :name (string-downcase (string (or name "project")))
                 :domain domain
                 :board board
                 :task task
                 :spec spec
                 :status status))

(defun %journal-for (domain journal)
  (or journal
      (let ((prof (and (expert-domain-p domain) (expert-profile domain))))
        (and (deployment-profile-p prof) (profile-journal prof)))
      (task:make-in-memory-journal)))

(defun %events (journal task)
  (task:journal-events journal task))

(defun %find-step (journal task name &key idempotency-key)
  (find-if (lambda (e)
             (and (typep e 'task:step-completed)
                  (equal name (task:step-name e))
                  (or (null idempotency-key)
                      (equal idempotency-key (task:step-idempotency-key e)))))
           (%events journal task)))

(defun %find-wait-input (journal task &key prompt)
  (find-if (lambda (e)
             (and (typep e 'task:wait-input)
                  (or (null prompt)
                      (equal prompt (task:wait-prompt e)))))
           (%events journal task)))

(defun record-milestone (name &key (task task:*task*))
  "Named journal checkpoint: a STEP-COMPLETED named milestone-reached."
  (check-type name (or string symbol))
  (let ((key (format nil "milestone/~a" (string-downcase (string name)))))
    (task:with-durable-step ("milestone-reached" :idempotency-key key)
      (list :milestone (string-downcase (string name)) :reached t))))

(defun await-approval (milestone &key (task task:*task*))
  "Durable WAIT-INPUT for MILESTONE. Signals APPROVAL-REQUIRED with
   restarts APPROVE / SKIP. Resume replays a recorded approval step."
  (let* ((ms (coerce-milestone milestone))
         (name (milestone-name ms))
         (prompt (or (milestone-prompt ms)
                     (format nil "approve milestone ~a" name)))
         (key (format nil "approval/~a" name))
         (journal (or (and task (task:durable-task-journal task))
                      task:*journal*)))
    (when (%find-step journal task "await-approval" :idempotency-key key)
      (return-from await-approval
        (task:with-durable-step ("await-approval" :idempotency-key key)
          (list :milestone name :approved t))))
    (unless (%find-wait-input journal task :prompt prompt)
      (record-milestone name :task task)
      (task:request-input task :prompt prompt))
    (setf (task:durable-task-status task) :waiting)
    (restart-case
        (error 'approval-required :milestone ms :prompt prompt)
      (approve ()
        :report "Approve the milestone and continue"
        (task:with-durable-step ("await-approval" :idempotency-key key)
          (list :milestone name :approved t)))
      (skip ()
        :report "Skip the milestone approval"
        (task:with-durable-step ("await-approval" :idempotency-key key)
          (list :milestone name :approved :skipped))))))

(defun schedule-project (task spec)
  "Arm SCHEDULE-RECURRING when that GF is exported and SPEC has a schedule."
  (let ((schedule (and spec (project-spec-schedule spec)))
        (fn (and (find-package '#:task-protocol)
                 (find-symbol "SCHEDULE-RECURRING" '#:task-protocol))))
    (when (and schedule fn (fboundp fn))
      (funcall fn task schedule))))

(defun start-project (domain spec &key journal task-id blackboard)
  "Named task-protocol task tree bound to one board.
   Milestones are durable wait-input checkpoints."
  (check-type domain expert-domain)
  (let* ((spec (coerce-project-spec spec))
         (name (or (project-spec-name spec)
                   (format nil "project/~a" (expert-name domain))))
         (journal (%journal-for domain journal))
         (board (or blackboard
                    (project-spec-board spec)
                    (bb:make-blackboard)))
         (task (task:make-durable-task
                :id (or task-id (format nil "project/~a" name))
                :journal journal))
         (wf (make-project-workflow :name name :domain domain
                                    :board board :task task :spec spec)))
    (task:with-durable-task (task journal)
      (when (eq (task:durable-task-status task) :completed)
        (setf (project-workflow-status wf) :completed)
        (return-from start-project wf))
      (setf (project-workflow-status wf) :running)
      (report-workflow-progress wf
                                :board board
                                :status :working
                                :summary (format nil "project ~a started" name))
      (dolist (ms (project-spec-milestones spec))
        (await-approval ms :task task)
        (report-workflow-progress
         wf :board board :status :working
         :summary (format nil "milestone ~a approved" (milestone-name ms))))
      (schedule-project task spec)
      (let ((result (list :status :completed :name name)))
        (task:complete-task task result)
        (setf (project-workflow-status wf) :completed)
        (report-workflow-progress wf :board board :status :completed
                                  :summary (format nil "project ~a completed" name))))
    wf))
