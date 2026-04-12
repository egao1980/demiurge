(defpackage #:demiurge/src/capabilities/vcs
  (:use #:cl)
  (:import-from #:demiurge/src/capabilities/protocol #:capability #:capability-operations
                #:make-capability-operation)
  (:import-from #:demiurge/src/capabilities/macros #:defcapability)
  (:export #:version-control-capability
           #:vcs-status #:vcs-diff #:vcs-commit #:vcs-branch #:vcs-log))

(in-package #:demiurge/src/capabilities/vcs)

(defcapability :version-control
  "Version control system operations"
  (:operation vcs-status ((path string))
   :returns t
   :doc "Get VCS status for path")
  (:operation vcs-diff ((path string))
   :returns string
   :doc "Get VCS diff for path")
  (:operation vcs-commit ((path string) (message string))
   :returns t
   :doc "Commit changes")
  (:operation vcs-branch ((name string))
   :returns t
   :doc "Create or switch branch")
  (:operation vcs-log ((path string))
   :returns list
   :doc "Get VCS log"))
