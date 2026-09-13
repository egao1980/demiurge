(defpackage #:demiurge
  (:use #:cl)
  (:nicknames #:stack-demiurge)
  (:local-nicknames (#:bb #:blackboard-protocol)
                    (#:bbj #:blackboard-journal)
                    (#:cap #:capability-protocol)
                    (#:agent #:ai-agent-protocol)
                    (#:agent.mcp #:ai-agent-protocol/mcp)
                    (#:conv #:conversation-protocol)
                    (#:csql #:conversation-backend-sql)
                    (#:steer #:steer-protocol)
                    (#:eval #:eval-protocol)
                    (#:event #:event-protocol)
                    (#:log #:log-protocol)
                    (#:rag #:rag-protocol)
                    (#:llm #:llm-protocol)
                    (#:task #:task-protocol)
                    (#:tbsql #:task-backend-sql)
                    (#:tel #:telemetry-protocol)
                    (#:cfg #:cl-stack-config))
  (:export
   #:demiurge-error
   #:demiurge-error-message
   #:demiurge-error-cause
   #:unknown-expert
   #:unknown-expert-name
   #:invalid-expert
   #:missing-event-backend
   #:compute-denied
   #:persistence-error
   #:call-with-demiurge-restarts
   #:with-demiurge-restarts
   #:invoke-retry
   #:invoke-use-value
   #:invoke-skip

   #:demiurge-config
   #:demiurge-config-p
   #:*demiurge-config*
   #:load-demiurge-config
   #:current-demiurge-config
   #:demiurge-config-agenda-max-concurrency
   #:demiurge-config-ksar-timeout-seconds
   #:demiurge-config-session-window-turns
   #:demiurge-config-llm-default-model
   #:demiurge-config-llm-catalog
   #:demiurge-config-paths-data-dir
   #:demiurge-config-improve-enabled

   #:call-with-ksar-observe
   #:call-with-agent-observe

   #:open-domain-journal
   #:attach-domain-journal
   #:resume-domain
   #:domain-task-id
   #:call-with-durable-ksar

   #:deployment-profile
   #:deployment-profile-p
   #:personal-profile
   #:personal-profile-p
   #:make-personal-profile
   #:profile-kind
   #:profile-data-dir
   #:profile-config
   #:profile-session-store
   #:profile-journal
   #:profile-chunker
   #:profile-rag-store
   #:profile-llm-catalog
   #:profile-default-model

   #:expert-domain
   #:expert-domain-p
   #:make-expert-domain
   #:expert-name
   #:expert-catalogue
   #:expert-ks-set
   #:expert-steering
   #:expert-corpora
   #:expert-eval-suites
   #:expert-profile
   #:register-expert
   #:unregister-expert
   #:find-expert
   #:require-expert
   #:list-experts
   #:clear-expert-registry
   #:defexpert

   #:agent-ks
   #:agent-ks-p
   #:make-agent-ks
   #:agent-ks-agent
   #:agent-ks-watch
   #:agent-ks-prompt-key
   #:agent-ks-result-key
   #:agent-ks-memory
   #:agent-ks-steering
   #:agent-ks-catalogue
   #:agent-ks-mcp-peer
   #:agent-ks-durability
   #:ks-watch-keys
   #:catalogue-function-tools
   #:collect-agent-ks-tools

   #:expert-controller
   #:expert-controller-p
   #:make-controller
   #:controller-domain
   #:controller-blackboard
   #:controller-stop-section
   #:register-expert-ks
   #:run-controller
   #:run-expert

   #:make-echo-expert
   #:echo-expert
   #:make-cl-dev-expert
   #:make-cl-dev-catalogue
   #:cl-dev-expert
   #:lookup-symbol
   #:search-corpus
   #:run-tests)
  (:documentation
   "Expert-domain model + KSAR controller. Persistence is the task-protocol journal."))

(in-package #:demiurge)
