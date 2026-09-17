(in-package #:demiurge/serve)

(defun %steering-skills (domain)
  "A2A agent skills from DOMAIN's steering directives."
  (let ((steering (expert-steering domain))
        (skills '()))
    (when steering
      (dolist (d (steer:list-directives steering))
        (push (a2a:make-agent-skill
               (string-downcase (string (steer:steer-directive-name d)))
               :name (string (steer:steer-directive-name d))
               :description (or (steer:steer-directive-description d)
                                (steer:steer-directive-body d)
                                "")
               :tags '("steering"))
              skills)))
    (or (nreverse skills)
        (list (a2a:make-agent-skill
               (expert-name domain)
               :name (expert-name domain)
               :description (format nil "Expert domain ~a" (expert-name domain))
               :tags '("expert"))))))

(defun expert-agent-card (domain &key url)
  (a2a:make-agent-card
   :name (expert-name domain)
   :description (format nil "Demiurge expert ~a" (expert-name domain))
   :version "0.3.1"
   :url url
   :skills (%steering-skills domain)))

(defun run-expert-as-a2a-task (domain &key blackboard prompt
                                        (sections '(:result :feedback-id))
                                        task-id timeout)
  "Register DOMAIN's KS set and drain the agenda as an A2A task."
  (check-type domain expert-domain)
  (with-request-session (nil :transport (or *request-transport* :http)
                             :conversation-id task-id)
    (let ((board (or blackboard (bb:make-blackboard))))
      (unless (bb:list-watchers board)
        (register-expert-ks board domain))
      (let ((task (wire.a2a:board-run-as-a2a-task
                   board
                   :sections sections
                   :task-id task-id
                   :trigger-key (and prompt :prompt)
                   :trigger-value prompt
                   :timeout timeout)))
        (when (and prompt (not (bb:section-bound-p board :feedback-id)))
          (bb:write-section board :feedback-id (make-feedback-id)))
        task))))

(defun make-expert-a2a-agent (domain &key url)
  (a2a:make-a2a-agent
   :name (expert-name domain)
   :card (expert-agent-card domain :url url)
   :handler (lambda (agent message task)
              (declare (ignore agent))
              (let* ((text (or (a2a:message-text message) ""))
                     (board (bb:make-blackboard))
                     (a2a-task (with-request-session
                                   (nil :transport (or *request-transport* :http)
                                        :conversation-id
                                        (or (a2a:a2a-message-context-id message)
                                            (a2a:a2a-message-task-id message)
                                            (and task (a2a:a2a-task-context-id task))
                                            (and task (a2a:a2a-task-id task))))
                                 (run-expert-as-a2a-task
                                  domain :blackboard board :prompt text))))
                (setf (a2a:a2a-task-state task) (a2a:a2a-task-state a2a-task)
                      (a2a:a2a-task-artifacts task)
                      (a2a:a2a-task-artifacts a2a-task))
                task))))
