(defpackage #:demiurge/src/capabilities/compute
  (:use #:cl)
  (:import-from #:demiurge/src/capabilities/protocol #:capability #:capability-operations
                #:make-capability-operation)
  (:import-from #:demiurge/src/capabilities/macros #:defcapability)
  (:export #:compute-capability #:run-command #:create-environment
           #:exec-in-environment #:destroy-environment))

(in-package #:demiurge/src/capabilities/compute)

(defcapability :compute
  "Execution environments and process management"
  (:operation run-command ((command string))
   :returns list
   :doc "Run a shell command, returns (exit-code stdout stderr)")
  (:operation create-environment ((spec list))
   :returns t
   :doc "Create an execution environment from spec")
  (:operation exec-in-environment ((env t) (command string))
   :returns list
   :doc "Execute command in environment")
  (:operation destroy-environment ((env t))
   :returns boolean
   :doc "Destroy an execution environment"))
