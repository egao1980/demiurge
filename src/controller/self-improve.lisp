(defpackage #:demiurge/src/controller/self-improve
  (:use #:cl)
  (:import-from #:demiurge/src/blackboard/core
                #:blackboard #:read-section #:write-section)
  (:import-from #:demiurge/src/blackboard/workspace
                #:fork-workspace #:merge-workspace #:discard-workspace
                #:workspace-blackboard)
  (:import-from #:demiurge/src/capabilities/registry
                #:get-capability)
  (:import-from #:demiurge/src/capabilities/llm #:generate-text)
  (:import-from #:demiurge/src/knowledge-source/registry
                #:list-ks #:find-ks #:register-ks)
  (:import-from #:demiurge/src/knowledge-source/protocol
                #:knowledge-source #:ks-name #:ks-version)
  (:import-from #:demiurge/src/knowledge-source/versioned
                #:versioned-ks #:make-versioned-ks #:current-version #:candidate-version)
  (:import-from #:demiurge/src/knowledge-source/ab-testing
                #:run-ab-test #:evaluate-metrics #:auto-promote)
  (:export #:maybe-start-improvement-cycle #:select-improvement-target
           #:improvement-cycle))

(in-package #:demiurge/src/controller/self-improve)

(defun select-improvement-target (bb)
  "Select a KS to improve. Returns KS name or nil."
  (let ((ks-list (list-ks bb)))
    (when ks-list
      (ks-name (first ks-list)))))

(defun maybe-start-improvement-cycle (bb)
  "Check if we should start improvement and do so."
  (let ((target (select-improvement-target bb)))
    (when target
      (improvement-cycle bb target))))

(defun improvement-cycle (bb target-name)
  "Run a self-improvement cycle for the named KS in an isolated workspace."
  (let* ((ws (fork-workspace bb (format nil "improve-~A-~A" target-name (get-universal-time))))
         (ws-bb (workspace-blackboard ws))
         (target-ks (find-ks ws-bb target-name)))
    (unless target-ks
      (discard-workspace ws)
      (return-from improvement-cycle nil))
    (write-section ws-bb :improvement-target target-name)
    (write-section ws-bb :improvement-status :analyzing)
    ;; Use LLM to analyze and propose improvements (if available)
    (let ((llm (get-capability ws-bb :llm-generation)))
      (when llm
        (let ((analysis (generate-text llm
                          (list (list :role "system"
                                      :content "You are improving a knowledge source component. Analyze its current implementation and suggest improvements.")
                                (list :role "user"
                                      :content (format nil "KS name: ~A, version: ~A"
                                                      target-name (ks-version target-ks)))))))
          (write-section ws-bb :improvement-analysis analysis)
          (write-section ws-bb :improvement-status :proposed))))
    ;; The actual code generation and testing would go here
    ;; For now, just record the analysis
    (write-section ws-bb :improvement-status :completed)
    (discard-workspace ws)
    t))
