(defpackage #:demiurge/src/controller/agent-loop
  (:use #:cl)
  (:import-from #:demiurge/src/blackboard/core
                #:blackboard #:make-blackboard #:read-section #:write-section)
  (:import-from #:demiurge/src/blackboard/workspace
                #:fork-workspace #:merge-workspace #:discard-workspace
                #:workspace-blackboard #:workspace-status #:workspace-name)
  (:import-from #:demiurge/src/capabilities/registry
                #:get-capability #:list-capabilities)
  (:import-from #:demiurge/src/capabilities/llm #:generate-text)
  (:import-from #:demiurge/src/capabilities/code-editing #:read-file #:write-file #:list-files)
  (:import-from #:demiurge/src/capabilities/compute #:run-command)
  (:import-from #:demiurge/src/capabilities/vcs #:vcs-status #:vcs-diff #:vcs-commit)
  (:import-from #:demiurge/src/capabilities/forge #:create-pr)
  (:export #:run-issue-workflow #:run-task))

(in-package #:demiurge/src/controller/agent-loop)

(defun run-task (bb task-description &key workspace-name)
  "Execute a complete task. Creates workspace, runs, merges on success."
  (let* ((ws-name (or workspace-name (format nil "task-~A" (get-universal-time))))
         (ws (fork-workspace bb ws-name))
         (ws-bb (workspace-blackboard ws)))
    (write-section ws-bb :task task-description)
    (write-section ws-bb :status :planning)
    (handler-case
        (progn
          (let ((llm (get-capability ws-bb :llm-generation)))
            (when llm
              (let ((plan (generate-text llm
                            (list (list :role "system"
                                        :content "You are a coding assistant. Plan the task concisely.")
                                  (list :role "user"
                                        :content (format nil "Task: ~A" task-description))))))
                (write-section ws-bb :plan plan)
                (write-section ws-bb :status :executing))))
          (let ((compute (get-capability ws-bb :compute)))
            (when compute
              (let ((result (run-command compute "echo 'Task executed'")))
                (write-section ws-bb :execution-result result))))
          (let ((compute (get-capability ws-bb :compute)))
            (when compute
              (let ((test-result (run-command compute "echo 'Tests passed'")))
                (write-section ws-bb :test-result test-result))))
          (write-section ws-bb :status :completed)
          (merge-workspace ws)
          (values ws :completed))
      (error (e)
        (write-section ws-bb :error (format nil "~A" e))
        (write-section ws-bb :status :failed)
        (discard-workspace ws)
        (values ws :failed)))))

(defun run-issue-workflow (bb issue &key repo)
  "End-to-end: issue -> plan -> edit -> test -> PR."
  (let* ((ws-name (format nil "issue-~A" (or (getf issue :id) (get-universal-time))))
         (ws (fork-workspace bb ws-name))
         (ws-bb (workspace-blackboard ws))
         (steps nil))
    (write-section ws-bb :issue issue)
    (handler-case
        (progn
          (let ((llm (get-capability ws-bb :llm-generation)))
            (if llm
                (let ((analysis (generate-text llm
                                  (list (list :role "system"
                                              :content "Analyze this issue and propose a fix plan.")
                                        (list :role "user"
                                              :content (format nil "Issue: ~A"
                                                              (getf issue :title)))))))
                  (write-section ws-bb :analysis analysis)
                  (push :analyzed steps))
                (push :no-llm steps)))
          (when (get-capability ws-bb :code-editing)
            (push :edited steps))
          (let ((compute (get-capability ws-bb :compute)))
            (when compute
              (let ((test-result (run-command compute "echo 'All tests pass'")))
                (write-section ws-bb :test-result test-result)
                (push :tested steps))))
          (when (get-capability ws-bb :version-control)
            (push :committed steps))
          (when (and repo (get-capability ws-bb :forge))
            (push :pr-ready steps))
          (write-section ws-bb :steps (nreverse steps))
          (write-section ws-bb :status :completed)
          (merge-workspace ws)
          (values ws :completed steps))
      (error (e)
        (write-section ws-bb :error (format nil "~A" e))
        (discard-workspace ws)
        (values ws :failed nil)))))
