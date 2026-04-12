(defpackage #:demiurge/src/knowledge-source/ab-testing
  (:use #:cl)
  (:import-from #:demiurge/src/blackboard/core
                #:blackboard #:make-blackboard #:read-section #:write-section)
  (:import-from #:demiurge/src/blackboard/workspace
                #:fork-workspace #:merge-workspace #:discard-workspace
                #:workspace-blackboard #:workspace-status)
  (:import-from #:demiurge/src/knowledge-source/protocol
                #:knowledge-source #:ks-name #:ks-execute)
  (:import-from #:demiurge/src/knowledge-source/versioned
                #:versioned-ks #:current-version #:candidate-version
                #:version-metrics #:promote-candidate #:demote-candidate)
  (:export #:run-ab-test #:evaluate-metrics #:auto-promote))

(in-package #:demiurge/src/knowledge-source/ab-testing)

(defun run-ab-test (bb versioned-ks input &key (trials 5))
  "Run A/B test: execute both current and candidate in isolated workspaces."
  (let ((current-results nil)
        (candidate-results nil))
    (dotimes (i trials)
      ;; Run current version
      (let* ((ws-a (fork-workspace bb (format nil "ab-current-~A" i)))
             (ws-bb-a (workspace-blackboard ws-a)))
        (write-section ws-bb-a :input input)
        (let ((start (get-internal-real-time)))
          (handler-case
              (let ((result (ks-execute (current-version versioned-ks) ws-bb-a)))
                (push (list :success t
                            :duration (/ (- (get-internal-real-time) start)
                                         internal-time-units-per-second)
                            :result result)
                      current-results))
            (error (e)
              (push (list :success nil :error (format nil "~A" e)) current-results))))
        (discard-workspace ws-a))
      ;; Run candidate version (if exists)
      (when (candidate-version versioned-ks)
        (let* ((ws-b (fork-workspace bb (format nil "ab-candidate-~A" i)))
               (ws-bb-b (workspace-blackboard ws-b)))
          (write-section ws-bb-b :input input)
          (let ((start (get-internal-real-time)))
            (handler-case
                (let ((result (ks-execute (candidate-version versioned-ks) ws-bb-b)))
                  (push (list :success t
                              :duration (/ (- (get-internal-real-time) start)
                                           internal-time-units-per-second)
                              :result result)
                        candidate-results))
              (error (e)
                (push (list :success nil :error (format nil "~A" e)) candidate-results))))
          (discard-workspace ws-b))))
    (list :current (nreverse current-results)
          :candidate (nreverse candidate-results))))

(defun evaluate-metrics (ab-results)
  "Evaluate A/B test results. Returns :promote, :demote, or :inconclusive."
  (let* ((current (getf ab-results :current))
         (candidate (getf ab-results :candidate))
         (c-success (count-if (lambda (r) (getf r :success)) current))
         (d-success (count-if (lambda (r) (getf r :success)) candidate))
         (c-total (length current))
         (d-total (length candidate)))
    (cond
      ((zerop d-total) :no-candidate)
      ((> (/ d-success (max 1 d-total))
          (/ c-success (max 1 c-total)))
       :promote)
      ((< (/ d-success (max 1 d-total))
          (/ c-success (max 1 c-total)))
       :demote)
      (t :inconclusive))))

(defun auto-promote (versioned-ks ab-results)
  "Automatically promote or demote based on A/B results."
  (let ((verdict (evaluate-metrics ab-results)))
    (case verdict
      (:promote (promote-candidate versioned-ks) :promoted)
      (:demote (demote-candidate versioned-ks) :demoted)
      (t verdict))))
