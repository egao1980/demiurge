(in-package #:demiurge)

(defclass expert-controller ()
  ((domain :initarg :domain :accessor controller-domain)
   (blackboard :initarg :blackboard :accessor controller-blackboard)
   (stop-section :initarg :stop-section :accessor controller-stop-section
                 :initform :stop)))

(defun expert-controller-p (x)
  (typep x 'expert-controller))

(defun %config-for (domain)
  (let ((profile (and (expert-domain-p domain) (expert-profile domain))))
    (or (and (deployment-profile-p profile) (profile-config profile))
        (current-demiurge-config))))

(defun %prepare-agent-ks (ks domain)
  (when (agent-ks-p ks)
    (unless (agent-ks-catalogue ks)
      (setf (agent-ks-catalogue ks) (expert-catalogue domain)))
    (unless (agent-ks-steering ks)
      (setf (agent-ks-steering ks) (expert-steering domain)))
    (let ((profile (expert-profile domain)))
      (when (and (deployment-profile-p profile)
                 (null (agent-ks-memory ks))
                 (profile-session-store profile))
        (setf (agent-ks-memory ks)
              (conv:make-window-memory
               :store (profile-session-store profile)
               :window-size (demiurge-config-session-window-turns
                             (%config-for domain))
               :session (tenant-session-id (string (bb:ks-name ks))))))))
  ks)

(defun register-expert-ks (blackboard domain)
  "Register DOMAIN's KS set on BLACKBOARD. Watcher requires = KS-WATCH-KEYS."
  (setf (gethash (bb:find-root-bb blackboard) *board-domains*) domain)
  (dolist (ks (expert-ks-set domain))
    (%prepare-agent-ks ks domain)
    (bb:register-ks blackboard ks :requires (ks-watch-keys ks)))
  blackboard)

(defun make-controller (domain &key blackboard journal profile
                                 (stop-section :stop)
                                 max-concurrency
                                 run-id
                                 (register t))
  (check-type domain expert-domain)
  (let* ((cfg (%config-for domain))
         (max-c (or max-concurrency
                    (demiurge-config-agenda-max-concurrency cfg)))
         (prof (or profile (expert-profile domain)))
         (j (or journal
                (and (deployment-profile-p prof)
                     (open-domain-journal domain prof))))
         (board (or blackboard
                    (bb:make-blackboard :max-concurrency max-c)))
         (controller (make-instance 'expert-controller
                                    :domain domain
                                    :blackboard board
                                    :stop-section stop-section)))
    (when j
      (attach-domain-journal board j :domain domain :run-id run-id))
    (when (and run-id (not j))
      (assign-board-run-id board :domain domain :run-id run-id))
    (when register
      (register-expert-ks board domain))
    controller))

(defun %write-trigger (blackboard trigger)
  (etypecase trigger
    (null nil)
    (list
     (loop for (key value) on trigger by #'cddr
           do (bb:write-section blackboard key value)))))

(defmethod bb:enqueue-ksar :after (bb ksar)
  (declare (ignore ksar))
  (record-agenda-depth bb))

(defun run-controller (controller &key (until-empty t) timeout trigger run-id)
  "Write optional TRIGGER sections, then RUN-SCHEDULER until the agenda is
   empty (or START-SCHEDULER when UNTIL-EMPTY is NIL). If STOP-SECTION is
   already bound, return immediately. No polling loop.
   KSAR execution is journaled via CALL-WITH-DURABLE-KSAR when a journal is attached.
   Explicit RUN-ID resumes that execution identity; otherwise the board's
   existing run id is kept (minted on first attach)."
  (let* ((board (controller-blackboard controller))
         (domain (controller-domain controller))
         (stop (controller-stop-section controller))
         (cfg (%config-for domain))
         (timeout (or timeout (demiurge-config-ksar-timeout-seconds cfg)))
         (journal (bbj:board-journal board))
         (task (and journal (bbj:board-journal-task board))))
    (assign-board-run-id board :domain domain :run-id run-id)
    (when (and stop (bb:section-bound-p board stop))
      (return-from run-controller board))
    (flet ((run ()
             (%write-trigger board trigger)
             (record-agenda-depth board)
             (bb:run-scheduler board :until-empty until-empty :timeout timeout)
             (record-agenda-depth board)
             board))
      (if (and journal task)
          (let ((task:*task* task)
                (task:*journal* journal))
            (run))
          (run)))
    board))

(defun run-expert (domain &key board trigger timeout stop-section
                            max-concurrency journal profile run-id)
  "Make a controller, register the KS set, write TRIGGER, drain the agenda.
   Explicit RUN-ID resumes that execution identity; otherwise mint a fresh run."
  (let ((controller (make-controller domain
                                     :blackboard board
                                     :stop-section (or stop-section :stop)
                                     :max-concurrency max-concurrency
                                     :journal journal
                                     :profile profile
                                     :run-id run-id)))
    (run-controller controller :trigger trigger :timeout timeout :run-id run-id)
    (controller-blackboard controller)))
