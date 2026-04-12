(defpackage #:demiurge/src/capabilities/communication
  (:use #:cl)
  (:import-from #:demiurge/src/capabilities/protocol #:capability #:capability-operations
                #:make-capability-operation)
  (:import-from #:demiurge/src/capabilities/macros #:defcapability)
  (:export #:communication-capability
           #:expose-tools #:call-remote-tool #:discover-agents #:delegate-task))

(in-package #:demiurge/src/capabilities/communication)

(defcapability :communication
  "Agent protocol communication (MCP, A2A)"
  (:operation expose-tools ((tools list))
   :returns boolean
   :doc "Register tools for external access")
  (:operation call-remote-tool ((server string) (tool string) (args t))
   :returns t
   :doc "Call a tool on a remote MCP server")
  (:operation discover-agents ()
   :returns list
   :doc "Discover available agents")
  (:operation delegate-task ((agent string) (task t))
   :returns t
   :doc "Delegate a task to another agent"))
