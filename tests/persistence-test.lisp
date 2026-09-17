(in-package #:demiurge/tests)

(defun %memory-profile (tmp &optional journal)
  (make-personal-profile
   :data-dir tmp
   :journal (or journal (task-protocol:make-in-memory-journal))
   :session-store (conv:make-in-memory-conversation-store)
   :rag-store (rag-backend-memory:make-memory-vector-store)
   :chunker (rag-backend-text:make-recursive-character-chunker
             :size 200 :overlap 20)))

(deftest resume-domain-restores-sections-and-agenda
  (with-clean-registry
    (with-tmp-dir (tmp)
      (let* ((journal (task-protocol:make-in-memory-journal))
             (profile (%memory-profile tmp journal))
             (domain (make-expert-domain :name "resume-sec" :profile profile))
             (board (bb:make-blackboard)))
        (register-expert domain)
        (attach-domain-journal board journal :domain domain)
        (bb:write-section board :prompt "hi")
        (bb:write-section board :note 42)
        (bb:enqueue-ksar board
                         (bb:make-ksar :watcher-id 'echo
                                       :priority 2
                                       :triggered-key :prompt))
        (ok (= 1 (bb:agenda-size board)))
        (let* ((resumed (resume-domain "resume-sec" profile))
               (fresh (controller-blackboard resumed)))
          (ok (equal (%section-alist board) (%section-alist fresh)))
          (ok (= (bb:agenda-size board) (bb:agenda-size fresh)))
          (ok (eql 2 (bb:ksar-priority (first (bb:agenda-contents fresh))))))))))

(deftest resume-domain-after-echo-run
  (with-clean-registry
    (with-tmp-dir (tmp)
      (let* ((journal (task-protocol:make-in-memory-journal))
             (profile (%memory-profile tmp journal))
             (domain (make-echo-expert :backend (mock-llm)
                                       :name "resume-echo"
                                       :profile profile)))
        (register-expert domain)
        (let ((board (run-expert domain
                                 :journal journal
                                 :profile profile
                                 :trigger '(:prompt "hi"))))
          (ok (equal "echo: hi" (bb:read-section board :result)))
          (let* ((resumed (resume-domain "resume-echo" profile))
                 (fresh (controller-blackboard resumed)))
            (ok (equal "echo: hi" (bb:read-section fresh :result)))
            (ok (equal "hi" (bb:read-section fresh :prompt)))
            (ok (equal (%section-alist board) (%section-alist fresh)))))))))

(deftest resume-domain-sqlite-fresh-profile
  (if (not (%sqlite-available-p))
      (skip "sql-backend-sqlite3 not loadable")
      (with-clean-registry
        (with-tmp-dir (tmp)
          (let* ((profile (make-personal-profile :data-dir tmp))
                 (domain (make-echo-expert :backend (mock-llm)
                                           :name "resume-sql"
                                           :profile profile)))
            (register-expert domain)
            (let ((board (run-expert domain
                                     :profile profile
                                     :trigger '(:prompt "sql"))))
              (ok (equal "echo: sql" (bb:read-section board :result)))
              (let* ((fresh-profile (make-personal-profile :data-dir tmp))
                     (resumed (resume-domain "resume-sql" fresh-profile))
                     (fresh (controller-blackboard resumed)))
                (ok (equal "echo: sql" (bb:read-section fresh :result)))
                (ok (equal (%section-alist board) (%section-alist fresh))))))))))

(defun %enqueue-ksar-ids (journal task-id)
  (let ((task (task-protocol:make-durable-task :id task-id :journal journal)))
    (loop for ev in (task-protocol:journal-events journal task)
          when (and (typep ev 'task-protocol:step-completed)
                    (equal (task-protocol:step-name ev) "enqueue-ksar"))
            collect (getf (task-protocol:step-result ev) :id))))

(defun %counting-echo-backend (counter)
  (llm:make-mock-llm-backend
   :handler (lambda (backend turns &key &allow-other-keys)
              (declare (ignore backend turns))
              (incf (car counter))
              (llm:make-llm-response
               :parts (list (llm:make-llm-text-part
                             :text (format nil "echo: n=~a" (car counter))))))))

(deftest durable-ksar-same-ks-two-activations-execute-twice
  "Same KS twice on one domain = two executions. Replay of the first
   activation does not re-execute. Receipts are journaled per activation."
  (with-clean-registry
    (with-tmp-dir (tmp)
      (let* ((journal (task-protocol:make-in-memory-journal))
             (profile (%memory-profile tmp journal))
             (counter (list 0))
             (domain (make-echo-expert :backend (%counting-echo-backend counter)
                                       :name "ksar-twice"
                                       :profile profile)))
        (register-expert domain)
        (let* ((controller (make-controller domain
                                            :journal journal
                                            :profile profile))
               (board (controller-blackboard controller))
               (ks (first (expert-ks-set domain))))
          (run-controller controller :trigger '(:prompt "one"))
          (run-controller controller :trigger '(:prompt "two"))
          (ok (= 2 (car counter))
              "two live activations execute twice")
          (ok (equal "echo: n=2" (bb:read-section board :result)))
          (let* ((ids (%enqueue-ksar-ids journal (domain-task-id domain)))
                 (first-id (first ids))
                 (activation
                  (let ((*current-ksar* (bb:make-ksar :id first-id)))
                    (durable-activation-id board ks :domain domain))))
            (ok (= 2 (length ids)))
            (ok (not (equal (first ids) (second ids))))
            (ok (find-effect-receipt journal activation :domain domain)
                "side-effect receipt keyed by first activation")
            (ok (typep (find-effect-receipt journal activation :domain domain)
                       'task-protocol:effect-receipt)
                "receipt is a task-protocol effect-receipt, not the step result")
            (let ((*current-ksar* (bb:make-ksar :id first-id)))
              (call-with-durable-ksar board ks
                                      (lambda ()
                                        (incf (car counter))
                                        "should-not-run")))
            (ok (= 2 (car counter))
                "replay of the first activation does not re-execute")
            (ok (find-effect-receipt journal activation :domain domain))))))))

(deftest fresh-run-id-by-default-explicit-resumes
  (with-clean-registry
    (with-tmp-dir (tmp)
      (let* ((journal (task-protocol:make-in-memory-journal))
             (profile (%memory-profile tmp journal))
             (domain (make-echo-expert :backend (mock-llm)
                                       :name "run-ids"
                                       :profile profile)))
        (register-expert domain)
        (let* ((c1 (make-controller domain :journal journal :profile profile))
               (id1 (ensure-board-run-id (controller-blackboard c1) domain))
               (c2 (make-controller domain
                                    :blackboard (bb:make-blackboard)
                                    :journal journal
                                    :profile profile))
               (id2 (ensure-board-run-id (controller-blackboard c2) domain))
               (c3 (make-controller domain
                                    :blackboard (bb:make-blackboard)
                                    :journal journal
                                    :profile profile
                                    :run-id id1))
               (id3 (ensure-board-run-id (controller-blackboard c3) domain)))
          (ok (and (stringp id1) (plusp (length id1))))
          (ok (not (equal id1 id2))
              "new controller mints a fresh run id")
          (ok (equal id1 id3)
              "explicit run-id resumes"))))))

(deftest personal-profile-factory
  (with-tmp-dir (tmp)
    (let ((profile (make-personal-profile
                    :data-dir tmp
                    :journal (task-protocol:make-in-memory-journal)
                    :session-store (conv:make-in-memory-conversation-store)
                    :rag-store (rag-backend-memory:make-memory-vector-store))))
      (ok (personal-profile-p profile))
      (ok (eq :personal (profile-kind profile)))
      (ok (profile-journal profile))
      (ok (profile-session-store profile))
      (ok (profile-chunker profile))
      (ok (profile-rag-store profile))
      (ok (profile-llm-catalog profile)))))
