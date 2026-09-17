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

(defvar *durable-id-counter* 0
  "Monotonic suffix for FRESH-DURABLE-ID.")

(defvar *current-ksar* nil
  "KSAR bound by the watch wrapper while a handler runs.")

(defvar *board-run-ids* (make-hash-table :test 'eq)
  "Root blackboard → domain run-id string.")

(defun fresh-durable-id (prefix &optional name)
  "Unique id for a new run/cycle invocation. Explicit ids resume."
  (check-type prefix string)
  (format nil "~a/~@[~a/~]~d-~d"
          prefix
          (and name (string name))
          (get-universal-time)
          (incf *durable-id-counter*)))

(defun %domain-name (domain)
  (if (expert-domain-p domain)
      (expert-name domain)
      domain))

(defun %run-id-string (value)
  (cond
    ((null value) nil)
    ((task:run-id-p value) (task:run-id-value value))
    (t (princ-to-string value))))

(defun assign-board-run-id (blackboard &key domain run-id)
  "Bind RUN-ID on BLACKBOARD. Explicit RUN-ID resumes; otherwise mint once."
  (let ((root (bb:find-root-bb blackboard)))
    (cond
      (run-id
       (setf (gethash root *board-run-ids*) (%run-id-string run-id)))
      ((gethash root *board-run-ids*))
      (t (setf (gethash root *board-run-ids*)
               (fresh-durable-id "run" (and domain (%domain-name domain))))))))

(defun ensure-board-run-id (blackboard &optional domain)
  "Return the run id for BLACKBOARD, creating one on first use."
  (assign-board-run-id blackboard :domain domain))

(defun %ksar-task-id (domain ks &optional activation-id)
  (let ((name (%domain-name domain))
        (ks-name (bb:ks-name ks)))
    (if activation-id
        (if (current-tenant)
            (format nil "tenant/~a/domain/~a/ksar/~a/~a"
                    (current-tenant) name ks-name activation-id)
            (format nil "domain/~a/ksar/~a/~a" name ks-name activation-id))
        (if (current-tenant)
            (format nil "tenant/~a/domain/~a/ksar/~a"
                    (current-tenant) name ks-name)
            (format nil "domain/~a/ksar/~a" name ks-name)))))

(defun %ksar-id-string (ksar)
  (cond
    ((and ksar (bb:ksar-id ksar))
     (princ-to-string (bb:ksar-id ksar)))
    (t (format nil "anon-~d" (incf *durable-id-counter*)))))

(defun %trigger-event-seq (blackboard ksar)
  "Board-journal event-seq of the enqueue-ksar that created KSAR, or NIL."
  (let ((journal (bbj:board-journal blackboard))
        (task (bbj:board-journal-task blackboard))
        (id (and ksar (bb:ksar-id ksar))))
    (when (and journal task id)
      (dolist (ev (reverse (task:journal-events journal task)))
        (when (and (typep ev 'task:step-completed)
                   (equal (task:step-name ev) "enqueue-ksar"))
          (let ((r (task:step-result ev)))
            (when (equal (getf r :id) id)
              (return (or (task:event-seq ev) 0)))))))))

(defun durable-activation-id (blackboard ks &key domain ksar run-id generation)
  "Activation key: run-id / board-generation / ks-name / ksar-id."
  (let* ((ksar (or ksar *current-ksar*))
         (run (or run-id (ensure-board-run-id blackboard domain)))
         (gen (or generation
                  (%trigger-event-seq blackboard ksar)
                  (and ksar (bb:ksar-id ksar))
                  0))
         (ks-name (bb:ks-name ks))
         (kid (%ksar-id-string ksar)))
    (format nil "~a/~a/~a/~a" run gen ks-name kid)))

(defun %receipts-task-id (domain)
  (let ((name (%domain-name domain)))
    (if (current-tenant)
        (format nil "tenant/~a/domain/~a/receipts" (current-tenant) name)
        (format nil "domain/~a/receipts" name))))

(defun %lookup-effect-receipt (task journal activation-id &key run-id)
  "Replay TASK from JOURNAL without starting it, then look up the receipt."
  (when (and task journal)
    (setf (task:durable-task-journal task) journal)
    (when (task:journal-events journal task)
      (task:replay-journal journal task))
    (or (task:find-effect-receipt task activation-id
                                  :run-id run-id
                                  :activation-id activation-id)
        (task:find-effect-receipt task activation-id))))

(defun journal-effect-receipt (journal activation-id payload
                               &key domain task run-id)
  "Record a side-effect receipt keyed by ACTIVATION-ID on TASK (or the
   domain receipts task). Distinct from the activation step result."
  (check-type activation-id string)
  (let* ((run (and run-id (task:make-run-id run-id)))
         (act (task:make-activation-id activation-id run))
         (receipt-task (or (and task (task:durable-task-p task) task)
                           (task:make-durable-task
                            :id (if domain
                                    (%receipts-task-id domain)
                                    "receipts")
                            :journal journal
                            :run-id run
                            :activation-id act)))
         (existing (%lookup-effect-receipt receipt-task journal activation-id
                                           :run-id run)))
    (or existing
        (task:with-durable-task (receipt-task journal)
          (or (task:find-effect-receipt receipt-task activation-id
                                        :run-id run
                                        :activation-id act)
              (task:record-effect-receipt
               receipt-task
               (task:make-effect-receipt
                :idempotency-key activation-id
                :payload (append (list :activation-id activation-id) payload)
                :run-id run
                :activation-id act)
               :journal journal))))))

(defun find-effect-receipt (journal activation-id &key domain task run-id)
  "Find a previously journaled receipt for ACTIVATION-ID, or NIL."
  (let ((run (and run-id (task:make-run-id run-id))))
    (or (%lookup-effect-receipt task journal activation-id :run-id run)
        (and domain
             (%lookup-effect-receipt
              (task:make-durable-task :id (%receipts-task-id domain)
                                      :journal journal)
              journal activation-id :run-id run))
        (dolist (tid (task:journal-task-ids journal) nil)
          (let ((found (%lookup-effect-receipt
                        (task:make-durable-task :id tid :journal journal)
                        journal activation-id :run-id run)))
            (when found (return found)))))))

(defmethod bb:watch :around ((bb bb:blackboard) &key id requires handler
                            (priority 0) one-shot)
  "Bind *CURRENT-KSAR* so durable keys can include the KSAR id."
  (call-next-method bb
                    :id id
                    :requires requires
                    :handler (if handler
                                 (let ((inner handler))
                                   (lambda (board ksar)
                                     (let ((*current-ksar* ksar))
                                       (funcall inner board ksar))))
                                 handler)
                    :priority priority
                    :one-shot one-shot))

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

(defun attach-domain-journal (blackboard journal &key domain task-id run-id)
  "Attach JOURNAL as the blackboard-journal spine of BLACKBOARD.
   RUN-ID is optional: explicit value resumes that run; otherwise mint once."
  (let* ((id (or task-id
                 (and domain (domain-task-id domain))
                 "blackboard"))
         (ctx (bbj:enable-blackboard-journal blackboard journal :task-id id)))
    (when (expert-domain-p domain)
      (setf (gethash (bb:find-root-bb blackboard) *board-domains*) domain))
    (assign-board-run-id blackboard :domain domain :run-id run-id)
    ctx))

(defun %domain-for-board (blackboard)
  (gethash (bb:find-root-bb blackboard) *board-domains*))

(defun %canonicalize-activation-result (run)
  (cond
    ((and run (agent:agent-run-p run))
     (or (agent:agent-run-text run) t))
    ((or (stringp run) (symbolp run) (numberp run) (null run))
     run)
    (t t)))

(defun call-with-durable-ksar (blackboard ks thunk)
  "Run THUNK as a task-protocol WITH-DURABLE-STEP when the board has a journal.
   The durable key includes run id, board generation (trigger-event seq),
   KS name, and KSAR id so only that exact activation is replayed."
  (let ((journal (bbj:board-journal blackboard))
        (domain (%domain-for-board blackboard)))
    (if (and journal domain)
        (let* ((run-str (ensure-board-run-id blackboard domain))
               (run (task:make-run-id run-str))
               (activation (durable-activation-id blackboard ks
                                                  :domain domain
                                                  :run-id run-str))
               (act (task:make-activation-id activation run))
               (step-name (format nil "execute/~a" activation))
               (task (task:make-durable-task
                      :id (%ksar-task-id domain ks activation)
                      :journal journal
                      :run-id run
                      :activation-id act)))
          (task:with-durable-task (task journal)
            (prog1
                (task:with-durable-step (step-name
                                         :idempotency-key activation
                                         :run-id run
                                         :activation-id act)
                  (let ((run-result (funcall thunk)))
                    (%canonicalize-activation-result run-result)))
              (journal-effect-receipt journal activation
                                      (list :ks (string (bb:ks-name ks))
                                            :kind :ksar-activation)
                                      :domain domain
                                      :task task
                                      :run-id run-str))))
        (funcall thunk))))

(defun resume-domain (name profile &key run-id)
  "Open the domain journal, REPLAY-BLACKBOARD onto a fresh board, re-arm timers.
   Re-registers the KS set. → EXPERT-CONTROLLER.
   Explicit RUN-ID resumes that execution identity; otherwise mint a fresh one."
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
    (attach-domain-journal replayed journal
                          :domain domain
                          :task-id task-id
                          :run-id run-id)
    (task:fire-due-timers journal)
    (make-controller domain
                     :blackboard replayed
                     :journal journal
                     :profile profile
                     :run-id run-id
                     :register t)))
