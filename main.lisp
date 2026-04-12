(defpackage #:demiurge/main
  (:nicknames #:demiurge)
  (:use #:cl)
  ;; Blackboard
  (:import-from #:demiurge/src/blackboard/core
                #:blackboard #:make-blackboard
                #:read-section #:write-section #:remove-section #:list-sections
                #:blackboard-notify-fn)
  (:import-from #:demiurge/src/blackboard/events
                #:event-bus #:make-event-bus
                #:subscribe #:unsubscribe #:emit-event
                #:run-event-loop #:stop-event-loop
                #:bb-event #:section-changed #:task-received
                #:workspace-transitioned #:capability-registered
                #:timer-tick #:ks-completed #:idle-detected
                #:make-section-changed #:make-task-received
                #:make-workspace-transitioned #:make-capability-registered
                #:make-timer-tick #:make-ks-completed #:make-idle-detected)
  (:import-from #:demiurge/src/blackboard/workspace
                #:workspace #:make-workspace #:workspace-name #:workspace-parent
                #:workspace-blackboard #:workspace-status #:workspace-metadata
                #:fork-workspace #:merge-workspace #:discard-workspace
                #:list-workspaces #:get-workspace)
  ;; Capabilities
  (:import-from #:demiurge/src/capabilities/protocol
                #:capability #:capability-name #:capability-version
                #:capability-description #:capability-operations
                #:invoke-operation)
  (:import-from #:demiurge/src/capabilities/registry
                #:register-capability #:unregister-capability
                #:get-capability #:list-capabilities #:capability-schema)
  (:import-from #:demiurge/src/capabilities/macros
                #:defcapability)
  ;; Capability definitions
  (:import-from #:demiurge/src/capabilities/llm
                #:llm-generation-capability #:generate-text #:generate-embedding)
  (:import-from #:demiurge/src/capabilities/compute
                #:compute-capability)
  (:import-from #:demiurge/src/capabilities/forge
                #:forge-capability)
  (:import-from #:demiurge/src/capabilities/code-intelligence
                #:code-intelligence-capability)
  (:import-from #:demiurge/src/capabilities/code-editing
                #:code-editing-capability)
  (:import-from #:demiurge/src/capabilities/vcs
                #:version-control-capability)
  (:import-from #:demiurge/src/capabilities/communication
                #:communication-capability)
  ;; Knowledge sources
  (:import-from #:demiurge/src/knowledge-source/protocol
                #:knowledge-source #:ks-name #:ks-version #:ks-priority
                #:ks-precondition #:ks-execute #:ks-postcondition)
  (:import-from #:demiurge/src/knowledge-source/registry
                #:register-ks #:unregister-ks #:find-ks #:list-ks)
  (:import-from #:demiurge/src/knowledge-source/versioned
                #:versioned-ks #:make-versioned-ks
                #:current-version #:candidate-version #:promote-candidate)
  ;; Controller
  (:import-from #:demiurge/src/controller/main-loop
                #:run-demiurge #:make-demiurge-instance #:stop-demiurge
                #:demiurge-ctx-bb #:demiurge-ctx-bus)
  (:import-from #:demiurge/src/controller/scheduler
                #:find-eligible-ks #:schedule-next-ks)
  (:import-from #:demiurge/src/controller/handlers
                #:handle-new-task #:execute-ks-in-workspace)
  (:import-from #:demiurge/src/controller/agent-loop
                #:run-task #:run-issue-workflow)
  (:import-from #:demiurge/src/controller/timers
                #:start-timer #:stop-timer)
  ;; Introspection
  (:import-from #:demiurge/src/introspection/object-registry
                #:object-registry #:make-object-registry
                #:register-object #:lookup-object #:inspectable-p)
  (:import-from #:demiurge/src/introspection/inspect
                #:inspect-object)
  (:import-from #:demiurge/src/introspection/render
                #:render-bb-summary #:render-workspace #:render-capabilities
                #:render-ks-list)
  ;; Utils
  (:import-from #:demiurge/src/utils/config
                #:get-config #:load-config)
  ;; Re-export
  (:export ;; BB
           #:blackboard #:make-blackboard
           #:read-section #:write-section #:remove-section #:list-sections
           #:blackboard-notify-fn
           ;; Events
           #:event-bus #:make-event-bus
           #:subscribe #:unsubscribe #:emit-event
           #:run-event-loop #:stop-event-loop
           #:bb-event #:section-changed #:task-received
           #:workspace-transitioned #:capability-registered
           #:timer-tick #:ks-completed #:idle-detected
           #:make-task-received #:make-ks-completed
           ;; Workspaces
           #:workspace #:workspace-name #:workspace-status
           #:workspace-blackboard #:workspace-metadata
           #:fork-workspace #:merge-workspace #:discard-workspace
           #:list-workspaces #:get-workspace
           ;; Capabilities
           #:capability #:capability-name #:capability-version
           #:register-capability #:unregister-capability
           #:get-capability #:list-capabilities #:capability-schema
           #:invoke-operation #:defcapability
           ;; Capability types
           #:llm-generation-capability #:generate-text #:generate-embedding
           #:compute-capability #:forge-capability
           #:code-intelligence-capability #:code-editing-capability
           #:version-control-capability #:communication-capability
           ;; KS
           #:knowledge-source #:ks-name #:ks-version #:ks-priority
           #:ks-precondition #:ks-execute #:ks-postcondition
           #:register-ks #:unregister-ks #:find-ks #:list-ks
           #:versioned-ks #:make-versioned-ks
           ;; Controller
           #:run-demiurge #:make-demiurge-instance #:stop-demiurge
           #:find-eligible-ks #:schedule-next-ks
           #:handle-new-task #:execute-ks-in-workspace
           #:run-task #:run-issue-workflow
           #:start-timer #:stop-timer
           ;; Introspection
           #:object-registry #:make-object-registry
           #:register-object #:lookup-object #:inspectable-p
           #:inspect-object
           #:render-bb-summary #:render-workspace #:render-capabilities
           ;; Config
           #:get-config #:load-config))

(in-package #:demiurge/main)
