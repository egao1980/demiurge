(in-package #:demiurge/tests)

(deftest make-expert-domain-defaults
  (let ((domain (make-expert-domain :name 'demo)))
    (ok (equal "demo" (expert-name domain)))
    (ok (eq :personal (expert-profile domain)))
    (ok (typep (expert-catalogue domain) 'cap:capability-catalogue))
    (ok (eq :world (cap:catalogue-name (expert-catalogue domain))))
    (ok (null (expert-ks-set domain)))
    (ok (null (expert-corpora domain)))
    (ok (null (expert-eval-suites domain)))))

(deftest defexpert-registry
  (with-clean-registry
    (let ((domain (defexpert echo-reg
                    (:profile :personal)
                    (:catalogue (cap:make-catalogue :world)))))
      (ok (expert-domain-p domain))
      (ok (eq domain (find-expert 'echo-reg)))
      (ok (eq domain (find-expert "echo-reg")))
      (ok (member domain (list-experts) :test #'eq))
      (unregister-expert 'echo-reg)
      (ok (null (find-expert 'echo-reg))))))

(deftest defexpert-eval-suites
  (with-clean-registry
    (let* ((ds (eval:make-eval-dataset
                :name "smoke"
                :cases (list (eval:make-eval-case :input "hi" :expected "echo: hi"))))
           (domain (make-expert-domain :name "evaled" :eval-suites (list ds))))
      (register-expert domain)
      (ok (= 1 (length (expert-eval-suites (find-expert "evaled")))))
      (ok (eval:eval-dataset-p
           (first (expert-eval-suites (find-expert "evaled"))))))))

(deftest agent-ks-precondition-watches-section
  (let* ((backend (mock-llm))
         (agent (agent:make-ai-agent :name "echo" :backend backend))
         (ks (make-agent-ks :name 'echo :agent agent :watch '(:prompt)))
         (bb (bb:make-blackboard)))
    (ok (null (bb:ks-precondition ks bb)))
    (bb:write-section bb :prompt "hi")
    (ok (bb:ks-precondition ks bb))
    (ok (equal '(:prompt) (ks-watch-keys ks)))))

(deftest agent-ks-fires-on-section-write
  (let* ((backend (llm:make-mock-llm-backend))
         (domain (make-echo-expert :backend backend :name "echo-fire"))
         (board (run-expert domain :trigger '(:prompt "hi"))))
    (ok (equal "echo: hi" (bb:read-section board :result)))
    (ok (bb:section-bound-p board :result))))

(deftest echo-expert-via-controller
  (let* ((backend (llm:make-mock-llm-backend :prefix "got: "))
         (domain (make-echo-expert :backend backend :name "echo-ctl"))
         (controller (make-controller domain))
         (board (run-controller controller :trigger '(:prompt "ping"))))
    (ok (eq board (controller-blackboard controller)))
    (ok (equal "got: ping" (bb:read-section board :result)))))

(deftest agent-ks-steering-and-memory
  (let* ((backend (llm:make-mock-llm-backend))
         (agent (agent:make-ai-agent :name "echo" :backend backend))
         (rule (steer:make-steer-rule "cite" :body "Always cite."))
         (ks (make-agent-ks :name 'echo
                            :agent agent
                            :steering (list rule)
                            :memory (conv:make-buffer-memory :session "t")))
         (bb (bb:make-blackboard)))
    (bb:register-ks bb ks :requires (ks-watch-keys ks))
    (bb:write-section bb :prompt "hi")
    (drain bb)
    (ok (equal "echo: hi" (bb:read-section bb :result)))
    (ok (agent:ai-agent-memory (agent-ks-agent ks)))
    (ok (agent:ai-agent-steering (agent-ks-agent ks)))))

(deftest catalogue-function-tools-from-registered-cap
  (let ((cat (cap:make-catalogue :world))
        (cap (make-instance 'cap:communication-capability)))
    (cap:register-capability cat cap)
    (let ((tools (catalogue-function-tools cat)))
      (ok (plusp (length tools)))
      (ok (find "communication/send-message" tools
                :key #'llm:llm-tool-name :test #'equal)))))


(deftest defexpert-agent-ks-writes-result
  (with-clean-registry
    (let* ((backend (llm:make-mock-llm-backend))
           (domain (defexpert echo-ksar
                     (:ks-set (list (make-agent-ks
                                     :name 'echo
                                     :agent (agent:make-ai-agent
                                             :name "echo"
                                             :backend backend
                                             :memory (conv:make-window-memory
                                                      :window-size 4)))))))
           (board (run-expert domain :trigger '(:prompt "ksar"))))
      (ok (eq domain (find-expert 'echo-ksar)))
      (ok (equal "echo: ksar" (bb:read-section board :result))))))

(deftest stop-section-skips-scheduler

  (let* ((backend (mock-llm))
         (domain (make-echo-expert :backend backend :name "echo-stop"))
         (controller (make-controller domain)))
    (bb:write-section (controller-blackboard controller) :stop t)
    (run-controller controller :trigger '(:prompt "nope"))
    (ok (eq :absent
            (bb:read-section (controller-blackboard controller)
                             :result :default :absent)))))

(deftest cl-dev-expert-bundle
  (with-clean-registry
    (let ((domain (make-cl-dev-expert :backend (mock-llm) :name "cl-dev-test")))
      (ok (expert-domain-p domain))
      (ok (= 1 (length (expert-ks-set domain))))
      (ok (plusp (length (expert-corpora domain))))
      (ok (= 1 (length (expert-eval-suites domain))))
      (let ((cases (eval:eval-dataset-cases
                    (first (expert-eval-suites domain)))))
        (ok (>= (length cases) 20))
        (ok (find-if (lambda (c)
                       (member :critical
                               (getf (eval:eval-case-metadata c) :tags)))
                     cases)))
      (ok (cap:get-capability (expert-catalogue domain) :lisp-dev))
      (ok (null (cap:get-capability (expert-catalogue domain) :compute)))
      (let ((tools (catalogue-function-tools (expert-catalogue domain))))
        (ok (find "lisp-dev/lookup-symbol" tools
                  :key #'llm:llm-tool-name :test #'equal))
        (ok (find "lisp-dev/run-tests" tools
                  :key #'llm:llm-tool-name :test #'equal))))))

(deftest cl-dev-run-tests-gated
  (let* ((cat (make-cl-dev-catalogue))
         (cap (cap:get-capability cat :lisp-dev)))
    (ok (signals (let ((demiurge::*operation-catalogue* cat))
                   (run-tests cap "demiurge"))
                 'compute-denied))))

(deftest skill-tool-source-is-direct
  (let* ((sk (steer:make-steer-skill
              "lookup"
              :body "Look up symbols."
              :extra (list :skill-tools
                           (list (llm:make-llm-tool
                                  :name "lookup"
                                  :description "Look up a symbol")))))
         (src (first (demiurge::%skill-tool-sources
                      (steer:make-in-memory-steering (list sk))))))
    (ok (not (null src)))
    (ok (typep src (find-class 'ai-agent-protocol::skill-tool-source)))))
