(defpackage #:demiurge/src/persistence/observability
  (:use #:cl)
  (:import-from #:demiurge/src/blackboard/core
                #:blackboard #:list-sections #:read-section
                #:agenda-size #:bb-active-count #:list-watchers)
  (:import-from #:demiurge/src/blackboard/workspace
                #:list-workspaces #:workspace-name #:workspace-status)
  (:import-from #:demiurge/src/capabilities/registry #:list-capabilities)
  (:import-from #:demiurge/src/knowledge-source/registry #:list-ks)
  (:import-from #:demiurge/src/knowledge-source/protocol #:ks-name #:ks-version)
  (:export #:bb-stats #:health-check))

(in-package #:demiurge/src/persistence/observability)

(defun bb-stats (bb)
  "Return a plist of blackboard statistics."
  (list :sections (length (list-sections bb))
        :workspaces (length (list-workspaces bb))
        :active-workspaces (length (list-workspaces bb :status :active))
        :capabilities (length (list-capabilities bb))
        :knowledge-sources (length (list-ks bb))
        :agenda-size (agenda-size bb)
        :active-handlers (bb-active-count bb)
        :watchers (length (list-watchers bb))))

(defun health-check (bb)
  "Return health status of the system."
  (let ((stats (bb-stats bb)))
    (list :status :healthy
          :stats stats
          :timestamp (get-universal-time))))
