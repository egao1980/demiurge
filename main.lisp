(defpackage #:demiurge/main
  (:nicknames #:demiurge)
  (:use #:cl)
  ;; Blackboard
  (:import-from #:demiurge/src/blackboard/core
                #:blackboard #:make-blackboard
                #:read-section #:write-section #:remove-section #:list-sections
                #:blackboard-notify-fn
                ;; Watcher/KSAR/Agenda/Scheduler
                #:watcher #:make-watcher #:watcher-id #:watcher-requires
                #:watcher-handler #:watcher-priority #:watcher-one-shot-p
                #:watch #:unwatch #:list-watchers #:get-watcher
                #:ksar #:make-ksar #:ksar-id #:ksar-watcher-id
                #:ksar-triggered-key #:ksar-priority #:ksar-context #:ksar-status
                #:enqueue-ksar #:pop-agenda #:agenda-contents #:agenda-size
                #:bb-active-count #:bb-max-concurrency
                #:run-scheduler #:stop-scheduler #:bb-scheduler-running-p)
  (:import-from #:demiurge/src/blackboard/workspace
                #:workspace #:make-workspace #:workspace-name #:workspace-parent
                #:workspace-blackboard #:workspace-status #:workspace-metadata
                #:fork-workspace #:merge-workspace #:discard-workspace
                #:list-workspaces #:get-workspace #:find-root-bb)
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
  (:import-from #:demiurge/src/capabilities/web-search
                #:web-search-capability #:web-search #:fetch-page)
  ;; Knowledge sources
  (:import-from #:demiurge/src/knowledge-source/protocol
                #:knowledge-source #:ks-name #:ks-version #:ks-priority
                #:ks-precondition #:ks-execute #:ks-postcondition)
  (:import-from #:demiurge/src/knowledge-source/registry
                #:register-ks #:unregister-ks #:find-ks #:list-ks)
  (:import-from #:demiurge/src/knowledge-source/versioned
                #:versioned-ks #:make-versioned-ks
                #:current-version #:candidate-version #:promote-candidate)
  (:import-from #:demiurge/src/knowledge-source/ab-testing
                #:run-ab-test #:evaluate-metrics #:auto-promote)
  ;; Controller
  (:import-from #:demiurge/src/controller/main-loop
                #:run-demiurge #:make-demiurge-instance #:stop-demiurge
                #:demiurge-ctx-bb)
  (:import-from #:demiurge/src/controller/scheduler
                #:find-eligible-ks #:schedule-next-ks)
  (:import-from #:demiurge/src/controller/handlers
                #:handle-new-task #:execute-ks-in-workspace
                #:init-kernel #:shutdown-kernel)
  (:import-from #:demiurge/src/controller/agent-loop
                #:run-task #:run-issue-workflow)
  (:import-from #:demiurge/src/controller/timers
                #:start-timer #:stop-timer)
  (:import-from #:demiurge/src/controller/prompts
                #:build-supervisor-prompt #:build-task-prompt
                #:build-review-prompt #:build-self-improve-prompt
                #:build-merge-review-prompt
                #:*identity-preamble* #:*architecture-section*
                #:format-capability-catalog #:format-memory-context)
  ;; Introspection
  (:import-from #:demiurge/src/introspection/object-registry
                #:object-registry #:make-object-registry
                #:register-object #:lookup-object #:inspectable-p)
  (:import-from #:demiurge/src/introspection/inspect
                #:inspect-object)
  (:import-from #:demiurge/src/introspection/render
                #:render-bb-summary #:render-workspace #:render-capabilities
                #:render-ks-list)
  ;; Persistence
  (:import-from #:demiurge/src/persistence/snapshot
                #:save-blackboard #:load-blackboard
                #:snapshot-to-file #:restore-from-file)
  (:import-from #:demiurge/src/persistence/observability
                #:bb-stats #:health-check)
  ;; Memory
  (:import-from #:demiurge/src/persistence/memory
                #:persistent-memory #:make-persistent-memory
                #:mem-get #:mem-set #:mem-delete #:mem-keys #:mem-has-p
                #:mem-append #:mem-get-list #:mem-get-list-last
                #:mem-increment #:mem-get-number
                #:mem-save #:mem-load #:with-memory-transaction)
  (:import-from #:demiurge/src/persistence/memory-keys
                #:record-ks-execution #:ks-success-rate #:ks-avg-duration
                #:record-task-result #:recent-tasks
                #:remember #:recall #:forget
                #:set-preference #:get-preference
                #:log-interaction #:recent-interactions)
  ;; Utils
  (:import-from #:demiurge/src/utils/config
                #:get-config #:load-config)
  ;; Re-export
  (:export ;; BB
           #:blackboard #:make-blackboard
           #:read-section #:write-section #:remove-section #:list-sections
           #:blackboard-notify-fn
           ;; Watcher/KSAR/Agenda
           #:watcher #:watch #:unwatch #:list-watchers #:get-watcher
           #:ksar #:ksar-id #:ksar-watcher-id #:ksar-context #:ksar-status
           #:enqueue-ksar #:agenda-contents #:agenda-size
           #:bb-active-count #:bb-max-concurrency
           #:run-scheduler #:stop-scheduler
           ;; Workspaces
           #:workspace #:workspace-name #:workspace-status
           #:workspace-blackboard #:workspace-metadata
           #:fork-workspace #:merge-workspace #:discard-workspace
           #:list-workspaces #:get-workspace #:find-root-bb
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
           #:web-search-capability #:web-search #:fetch-page
           ;; KS
           #:knowledge-source #:ks-name #:ks-version #:ks-priority
           #:ks-precondition #:ks-execute #:ks-postcondition
           #:register-ks #:unregister-ks #:find-ks #:list-ks
           #:versioned-ks #:make-versioned-ks
           #:run-ab-test #:evaluate-metrics #:auto-promote
           ;; Controller
           #:run-demiurge #:make-demiurge-instance #:stop-demiurge
           #:find-eligible-ks #:schedule-next-ks
           #:handle-new-task #:execute-ks-in-workspace
           #:init-kernel #:shutdown-kernel
           #:run-task #:run-issue-workflow
           #:start-timer #:stop-timer
           ;; Prompts
           #:build-supervisor-prompt #:build-task-prompt
           #:build-review-prompt #:build-self-improve-prompt
           #:build-merge-review-prompt
           #:*identity-preamble* #:*architecture-section*
           #:format-capability-catalog #:format-memory-context
           ;; Introspection
           #:object-registry #:make-object-registry
           #:register-object #:lookup-object #:inspectable-p
           #:inspect-object
           #:render-bb-summary #:render-workspace #:render-capabilities
           ;; Persistence
           #:save-blackboard #:load-blackboard
           #:snapshot-to-file #:restore-from-file
           #:bb-stats #:health-check
           ;; Memory
           #:persistent-memory #:make-persistent-memory
           #:mem-get #:mem-set #:mem-delete #:mem-keys #:mem-has-p
           #:mem-append #:mem-get-list #:mem-get-list-last
           #:mem-increment #:mem-get-number
           #:mem-save #:mem-load #:with-memory-transaction
           #:record-ks-execution #:ks-success-rate #:ks-avg-duration
           #:record-task-result #:recent-tasks
           #:remember #:recall #:forget
           #:set-preference #:get-preference
           #:log-interaction #:recent-interactions
           ;; Config
           #:get-config #:load-config))

(in-package #:demiurge/main)
