(in-package #:demiurge)

(defvar *board-domains* (make-hash-table :test 'eq)
  "Root blackboard → EXPERT-DOMAIN.")

(defun %decoded-event-plist (data)
  "Normalize json-protocol/jzon object decode (vector or hash-table) to a plist
   so TASK-PROTOCOL:EVENT-FROM-PLIST can rehydrate SQL journal payloads.
   Walk nested values — board-step payloads are objects too."
  (labels ((kw (k)
             (cond
               ((keywordp k) k)
               ((symbolp k) (intern (symbol-name k) :keyword))
               (t (intern (string-upcase (string k)) :keyword))))
           (object-keys-p (seq)
             (and (plusp (length seq))
                  (evenp (length seq))
                  (let ((k (if (vectorp seq) (aref seq 0) (first seq))))
                    (or (stringp k) (symbolp k)))))
           (name-value (k v)
             (if (and (member k '(:key :triggered-key :watcher-id) :test #'eq)
                      (or (stringp v) (symbolp v))
                      (plusp (length (string v)))
                      (not (find #\/ (string v))))
                 (intern (string-upcase (string v)) :keyword)
                 v))
           (walk (x)
             (cond
               ((hash-table-p x)
                (let ((out '()))
                  (maphash (lambda (k v)
                             (let ((kk (kw k)))
                               (push (name-value kk (walk v)) out)
                               (push kk out)))
                           x)
                  out))
               ((and (vectorp x) (not (stringp x)) (object-keys-p x))
                (loop for i from 0 below (length x) by 2
                      for k = (kw (aref x i))
                      collect k
                      collect (name-value k (walk (aref x (1+ i))))))
               ((and (vectorp x) (not (stringp x)))
                (map 'list #'walk x))
               ((and (consp x) (object-keys-p x))
                (loop for (k v) on x by #'cddr
                      for kk = (kw k)
                      collect kk
                      collect (name-value kk (walk v))))
               ((consp x)
                (cons (walk (car x)) (walk (cdr x))))
               (t x))))
    (walk data)))

(defvar *event-from-plist-compat* nil)

(defun %install-event-from-plist-compat ()
  "B3 loads json-protocol (via serve/wire). task-protocol ENCODE-PAYLOAD then
   prefers JSON; DECODE returns a string-key vector, not a plist. Wrap until
   task-protocol accepts both."
  (unless *event-from-plist-compat*
    (let ((orig (fdefinition 'task-protocol:event-from-plist)))
      (setf (fdefinition 'task-protocol:event-from-plist)
            (lambda (data)
              (funcall orig (%decoded-event-plist data))))
      (setf *event-from-plist-compat* t))))

(%install-event-from-plist-compat)

(defun domain-task-id (domain)
  (let ((name (expert-name domain)))
    (if (current-tenant)
        (progn
          (assert-tenant-scope (tenant-task-id name) (current-tenant))
          (tenant-task-id name))
        (format nil "domain/~a" name))))

(defun %ksar-task-id (domain ks)
  (let ((name (if (expert-domain-p domain)
                  (expert-name domain)
                  domain)))
    (if (current-tenant)
        (format nil "tenant/~a/domain/~a/ksar/~a"
                (current-tenant) name (bb:ks-name ks))
        (format nil "domain/~a/ksar/~a" name (bb:ks-name ks)))))

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
