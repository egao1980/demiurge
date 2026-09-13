(in-package #:demiurge/tests)

(defparameter *research-questions*
  '("What is KSAR?" "What is a blackboard?" "What is a journal?"))

(defun %turns-text (turns)
  (cond
    ((stringp turns) turns)
    ((listp turns)
     (with-output-to-string (s)
       (dolist (tn turns)
         (write-string (if (stringp tn) tn (llm:turn-text tn)) s))))
    (t (princ-to-string turns))))

(defun %research-llm (&key (questions *research-questions*))
  (llm:make-mock-llm-backend
   :handler
   (lambda (backend turns &key &allow-other-keys)
     (declare (ignore backend))
     (let ((text (%turns-text turns)))
       (cond
         ((search "Gap analysis" text)
          (llm:make-llm-response
           :parts (list (llm:make-llm-text-part :text "none"))
           :output (make-research-plan :question "q" :subquestions nil)))
         ((search "Decompose" text)
          (llm:make-llm-response
           :parts (list (llm:make-llm-text-part :text "plan"))
           :output (make-research-plan
                    :question "CL expert systems"
                    :subquestions
                    (loop for q in questions
                          for i from 1
                          collect (make-research-subquestion
                                   :id (format nil "q~d" i)
                                   :question q)))))
         (t
          (llm:make-llm-response
           :parts (list (llm:make-llm-text-part :text "synthesis ok")))))))))

(defun %research-websearch ()
  (web:make-mock-websearch-backend
   :handler
   (lambda (backend query &key &allow-other-keys)
     (declare (ignore backend))
     (list (web:make-search-hit
            :url (format nil "https://ex.test/~a"
                         (substitute #\- #\Space (string query)))
            :title (string query)
            :snippet (format nil "ANSWER:~a" query)
            :rank 1
            :source "mock")))))

(defun %research-domain (&key name)
  (make-expert-domain :name (or name "research-demo")
                      :catalogue (cap:make-catalogue :world)
                      :profile :personal))

(defun %count-events (journal type &optional task-id)
  (loop for id in (if task-id
                      (list task-id)
                      (task:journal-task-ids journal))
        sum (count-if (lambda (e) (typep e type))
                      (task:journal-events
                       journal (task:make-durable-task :id id)))))

(defun %run-research (&key journal task-id llm websearch budget blackboard
                        (max-rounds 1) (question "CL expert systems"))
  (run-deep-research (%research-domain)
                     question
                     :max-rounds max-rounds
                     :budget budget
                     :llm (or llm (%research-llm))
                     :websearch (or websearch (%research-websearch))
                     :journal (or journal (task:make-in-memory-journal))
                     :task-id (or task-id "research-e2e")
                     :blackboard blackboard))

(deftest deep-research-e2e-mock-llm-websearch
  (let* ((board (bb:make-blackboard))
         (result (%run-research :task-id "research-e2e"
                                :blackboard board)))
    (ok (member (getf result :verdict) '(:pass :fail)))
    (ok (stringp (getf result :markdown)))
    (ok (= 3 (length (getf result :children))))
    (ok (bb:section-bound-p board :round-summary))
    (dolist (q *research-questions*)
      (ok (search (format nil "ANSWER:~a" q) (getf result :markdown))
          (format nil "report cites ~a" q)))))

(deftest deep-research-kill-and-resume-replays-finished-children
  (let* ((journal (task:make-in-memory-journal))
         (task-id "research-resume")
         (exec 0)
         (llm (%research-llm))
         (web (%research-websearch)))
    (let ((*research-child-exec-hook*
           (lambda (in)
             (declare (ignore in))
             (incf exec)))
          (*research-child-hook*
           (lambda (child input)
             (declare (ignore input))
             (when (and (eq :completed (task:durable-task-status child))
                        (= exec 2))
               (error "killed after 2 of 3 children")))))
      (handler-case
          (%run-research :journal journal :task-id task-id
                         :llm llm :websearch web)
        (error ())))
    (ok (= 2 exec) "two children executed before kill")
    (ok (= 2 (%count-events journal 'task:child-spawned task-id))
        "two child-spawned events after kill")
    (let ((completed-after-kill
           (%count-events journal 'task:task-completed)))
      (let ((*research-child-exec-hook*
             (lambda (in)
               (declare (ignore in))
               (incf exec)))
            (*research-child-hook* nil))
        (let ((result (%run-research :journal journal :task-id task-id
                                     :llm llm :websearch web)))
          (ok (= 3 exec) "finished children replay, only the rest execute")
          (ok (= 3 (%count-events journal 'task:child-spawned task-id))
              "child-spawned count is 3, not re-appended")
          (ok (>= (%count-events journal 'task:task-completed)
                  completed-after-kill)
              "replay does not drop finished-child completions")
          (ok (member (getf result :verdict) '(:pass :fail)))
          (dolist (q *research-questions*)
            (ok (search (format nil "ANSWER:~a" q) (getf result :markdown))
                (format nil "resumed report cites ~a" q))))))))

(deftest project-milestone-wait-input-resume
  (let* ((journal (task:make-in-memory-journal))
         (task-id "project-ms")
         (domain (%research-domain :name "project-demo"))
         (spec (make-project-spec
                :name "compliance"
                :milestones '("review")
                :schedule (lambda (last) (+ last 86400))))
         (waited nil))
    (handler-case
        (start-project domain spec :journal journal :task-id task-id)
      (approval-required (c)
        (setf waited t)
        (ok (approval-required-milestone c))))
    (ok waited "first run waits at the milestone")
    (ok (plusp (%count-events journal 'task:wait-input task-id))
        "wait-input is journaled")
    (ok (plusp (%count-events journal 'task:step-completed task-id))
        "milestone-reached checkpoint is journaled")
    (let ((wf (handler-bind ((approval-required
                              (lambda (c)
                                (invoke-approve c))))
                (start-project domain spec :journal journal :task-id task-id))))
      (ok (project-workflow-p wf))
      (ok (eq :completed (project-workflow-status wf)))
      (ok (eq :completed (task:durable-task-status
                          (project-workflow-task wf))))
      (ok (find-if (lambda (e)
                     (and (typep e 'task:timer-set)
                          (task:timer-recurring-p e)))
                   (task:journal-events journal
                                        (project-workflow-task wf)))
          "schedule-recurring armed a recurring timer"))))

(deftest deep-research-budget-exhaustion-incomplete
  (let ((result (%run-research
                 :task-id "research-budget"
                 :budget (llm:make-llm-budget :max-tokens 0))))
    (ok (eq :incomplete (getf result :verdict)))
    (ok (stringp (getf result :markdown)))
    (ok (search "incomplete" (string-downcase (getf result :markdown))))))
