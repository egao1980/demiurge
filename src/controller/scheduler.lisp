(defpackage #:demiurge/src/controller/scheduler
  (:use #:cl)
  (:import-from #:demiurge/src/blackboard/core
                #:blackboard #:read-section)
  (:import-from #:demiurge/src/knowledge-source/protocol
                #:knowledge-source #:ks-precondition #:ks-priority)
  (:import-from #:demiurge/src/knowledge-source/registry
                #:list-ks)
  (:export #:find-eligible-ks #:schedule-next-ks))

(in-package #:demiurge/src/controller/scheduler)

(defun find-eligible-ks (bb)
  "Find all KSs whose preconditions are met, sorted by priority."
  (let ((eligible nil))
    (dolist (ks (list-ks bb))
      (when (handler-case (ks-precondition ks bb)
              (error () nil))
        (push ks eligible)))
    (sort eligible #'> :key #'ks-priority)))

(defun schedule-next-ks (bb)
  "Select the highest-priority eligible KS."
  (first (find-eligible-ks bb)))
