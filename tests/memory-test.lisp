(in-package #:demiurge/tests)

(deftest chronicle-appends-and-injects
  (let* ((store (mem:make-in-memory-store))
         (now (dt:make-instant 1700000000)))
    (let ((dt:*clock* (dt:make-fixed-clock now)))
      (chronicle-turns store "freeze the budget"
                       :identity "echo-mem" :tenant "default" :session "s1")
      (let ((injected (inject-memory-state store "next turn"
                                          :identity "echo-mem"
                                          :tenant "default")))
        (ok (eq :system (llm:llm-turn-role (first injected))))
        (ok (search "freeze the budget" (llm:turn-text (first injected))))
        (ok (search "where we are" (llm:turn-text (first injected))))))))

(deftest agent-ks-chronicles-when-store-bound
  (let* ((store (mem:make-in-memory-store))
         (backend (llm:make-mock-llm-backend))
         (agent (agent:make-ai-agent :name "echo" :backend backend))
         (ks (make-agent-ks :name 'echo
                            :agent agent
                            :chronicle store
                            :memory (conv:make-buffer-memory :session "t")))
         (bb (bb:make-blackboard)))
    (bb:register-ks bb ks :requires (ks-watch-keys ks))
    (bb:write-section bb :prompt "talk about the budget")
    (drain bb)
    (ok (equal "echo: talk about the budget" (bb:read-section bb :result)))
    (let ((result (mem:query-memory
                   store
                   (mem:make-memory-query :text "budget"
                                          :identity "echo"
                                          :tenant "default"))))
      (ok (plusp (length (mem:memory-result-hits result)))))))

(deftest agent-ks-without-chronicle-unchanged
  (let* ((backend (llm:make-mock-llm-backend))
         (domain (make-echo-expert :backend backend :name "echo-no-mem"))
         (board (run-expert domain :trigger '(:prompt "hi"))))
    (ok (equal "echo: hi" (bb:read-section board :result)))))
