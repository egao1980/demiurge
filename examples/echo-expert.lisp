(in-package #:demiurge)

;;; Bundled reference expert: mock (or any) LLM echoes :prompt → :result.

(defun make-echo-expert (&key backend name (profile :personal)
                           (watch '(:prompt))
                           instructions)
  "Tiny coding/echo stub. BACKEND is typically MAKE-MOCK-LLM-BACKEND."
  (let* ((agent (agent:make-ai-agent
                 :name "echo"
                 :backend backend
                 :instructions (or instructions "Echo the user.")
                 :memory (conv:make-window-memory
                          :window-size (demiurge-config-session-window-turns
                                        (current-demiurge-config))
                          :session "echo")))
         (ks (make-agent-ks :name 'echo
                            :agent agent
                            :watch watch
                            :prompt-key :prompt
                            :result-key :result)))
    (make-expert-domain
     :name (or name "echo")
     :catalogue :world
     :ks-set (list ks)
     :profile profile)))

(defun echo-expert (&rest args &key &allow-other-keys)
  (apply #'make-echo-expert args))
