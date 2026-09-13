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
