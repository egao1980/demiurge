(in-package #:demiurge)

(defvar *board-domains* (make-hash-table :test 'eq)
  "Root blackboard → EXPERT-DOMAIN.")

(defun domain-task-id (domain)
  (format nil "domain/~a" (expert-name domain)))

(defun %ksar-task-id (domain ks)
  (format nil "domain/~a/ksar/~a"
          (if (expert-domain-p domain)
              (expert-name domain)
              domain)
          (bb:ks-name ks)))

(defun %ensure-sqlite-backend ()
  (or (find-package '#:sql-backend-sqlite3)
      (ignore-errors
        (asdf:load-system "sql-backend-sqlite3" :verbose nil)
        (find-package '#:sql-backend-sqlite3))))

(defun %open-sql-journal (path)
  (unless (%ensure-sqlite-backend)
    (restart-case
        (error 'persistence-error
               :message "sql-backend-sqlite3 is not loadable")
      (use-value (journal)
        :report "Use a supplied task-protocol journal"
        (return-from %open-sql-journal journal))))
  (let ((conn (sql-protocol:connect :driver :sqlite3
                                    :database-name (namestring path))))
    (tbsql:make-sql-task-journal :connection conn :ensure-schema t)))

(defun open-domain-journal (domain profile)
  "Return a task-protocol journal for DOMAIN under PROFILE.
   Personal profile → SQLite via task-backend-sql (file under data-dir)."
  (check-type domain expert-domain)
  (cond
    ((and (deployment-profile-p profile) (profile-journal profile))
     (profile-journal profile))
    ((deployment-profile-p profile)
     (let* ((dir (or (profile-data-dir profile)
                     (demiurge-config-paths-data-dir (profile-config profile))
                     (uiop:xdg-data-home)))
            (root (uiop:ensure-directory-pathname
                   (merge-pathnames
                    (format nil "demiurge/~a/" (expert-name domain))
                    (uiop:ensure-directory-pathname dir))))
            (path (merge-pathnames "journal.sqlite" root)))
       (ensure-directories-exist root)
       (let ((journal (%open-sql-journal path)))
         (setf (profile-journal profile) journal)
         journal)))
    ((eq profile :personal)
     (open-domain-journal domain (make-personal-profile)))
    (t
     (restart-case
         (error 'persistence-error
                :message (format nil "cannot open journal for profile ~s" profile))
       (use-value (journal)
         :report "Use a supplied journal"
         journal)))))

(defun attach-domain-journal (blackboard journal &key domain task-id)
  "Attach JOURNAL as the blackboard-journal spine of BLACKBOARD."
  (let* ((id (or task-id
                 (and domain (domain-task-id domain))
                 "blackboard"))
         (ctx (bbj:enable-blackboard-journal blackboard journal :task-id id)))
    (when (expert-domain-p domain)
      (setf (gethash (bb:find-root-bb blackboard) *board-domains*) domain))
    ctx))

(defun %domain-for-board (blackboard)
  (gethash (bb:find-root-bb blackboard) *board-domains*))

(defun call-with-durable-ksar (blackboard ks thunk)
  "Run THUNK as a task-protocol WITH-DURABLE-STEP when the board has a journal.
   Each KS uses its own task id so board write-section events do not collide."
  (let ((journal (bbj:board-journal blackboard))
        (domain (%domain-for-board blackboard)))
    (if (and journal domain)
        (let ((task (task:make-durable-task
                     :id (%ksar-task-id domain ks)
                     :journal journal)))
          (task:with-durable-task (task journal)
            (task:with-durable-step ("execute"
                                     :idempotency-key
                                     (format nil "ksar/~a" (bb:ks-name ks)))
              (let ((run (funcall thunk)))
                (cond
                  ((and run (agent:agent-run-p run))
                   (or (agent:agent-run-text run) t))
                  ((or (stringp run) (symbolp run) (numberp run) (null run))
                   run)
                  (t t))))))
        (funcall thunk))))

(defun resume-domain (name profile)
  "Open the domain journal, REPLAY-BLACKBOARD onto a fresh board, re-arm timers.
   Re-registers the KS set. → EXPERT-CONTROLLER."
  (let* ((domain (require-expert name))
         (cfg (if (deployment-profile-p profile)
                  (or (profile-config profile) (current-demiurge-config))
                  (current-demiurge-config)))
         (journal (open-domain-journal domain profile))
         (task-id (domain-task-id domain))
         (board (bb:make-blackboard
                 :max-concurrency
                 (demiurge-config-agenda-max-concurrency cfg)))
         (replayed (bbj:replay-blackboard journal
                                         :blackboard board
                                         :task-id task-id)))
    (attach-domain-journal replayed journal :domain domain :task-id task-id)
    (task:fire-due-timers journal)
    (make-controller domain
                     :blackboard replayed
                     :journal journal
                     :profile profile
                     :register t)))
