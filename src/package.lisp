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
                    (#:cfg #:cl-stack-config)
                    (#:oauth2 #:cl-stack-oauth2)
                    (#:jwt #:cl-stack-jwt)
                    (#:ldap #:ldap-protocol))
  (:export
   #:demiurge-error
   #:demiurge-error-message
   #:demiurge-error-cause
   #:unknown-expert
   #:unknown-expert-name
   #:invalid-expert
   #:expert-config-error
   #:expert-config-error-path
   #:expert-config-error-issues
   #:unknown-expert-config-key
   #:unknown-expert-config-key-name
   #:unknown-expert-config-valid-keys
   #:unknown-expert-config-section
   #:missing-event-backend
   #:compute-denied
   #:persistence-error
   #:capability-denied
   #:capability-denied-capability
   #:capability-denied-operation
   #:capability-denied-principal
   #:capability-denied-tenant
   #:tenant-isolation-error
   #:tenant-isolation-expected
   #:tenant-isolation-actual
   #:tenant-isolation-reference
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
   #:demiurge-config-workspace-root
   #:demiurge-config-workspace-seed
   #:demiurge-config-improve-enabled
   #:demiurge-config-corporate-oidc-issuer
   #:demiurge-config-corporate-oidc-client-id
   #:demiurge-config-corporate-ldap-url
   #:demiurge-config-corporate-ldap-base-dn
   #:demiurge-config-corporate-ldap-group-role-map
   #:demiurge-config-corporate-postgres-dsn
   #:demiurge-config-corporate-otlp-endpoint
   #:demiurge-config-corporate-tenant-id
   #:demiurge-config-corporate-role-grants

   #:call-with-ksar-observe
   #:call-with-agent-observe
   #:record-agenda-depth
   #:wrap-llm-observe
   #:bare-llm-backend

   #:open-domain-journal
   #:attach-domain-journal
   #:resume-domain
   #:domain-task-id
   #:call-with-durable-ksar
   #:*current-ksar*
   #:fresh-durable-id
   #:durable-activation-id
   #:ensure-board-run-id
   #:assign-board-run-id
   #:journal-effect-receipt
   #:find-effect-receipt

   #:deployment-profile
   #:deployment-profile-p
   #:personal-profile
   #:personal-profile-p
   #:make-personal-profile
   #:corporate-profile
   #:corporate-profile-p
   #:make-corporate-profile
   #:profile-tenant
   #:profile-kind
   #:profile-data-dir
   #:profile-config
   #:profile-session-store
   #:profile-journal
   #:profile-chunker
   #:profile-rag-store
   #:profile-llm-catalog
   #:profile-default-model
   #:profile-skill-store
   #:profile-require-hitl-p
   #:resolve-profile-llm
   #:profile-backend-summary

   #:expert-domain
   #:expert-domain-p
   #:make-expert-domain
   #:instantiate-expert-domain
   #:load-expert-config
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
   #:run-tests

   #:*tenant*
   #:current-tenant
   #:with-tenant
   #:tenant-scope
   #:tenant-of-reference
   #:assert-tenant-scope
   #:tenant-session-id
   #:tenant-task-id
   #:tenant-corpus-name
   #:tenant-budget-scope
   #:*principal*
   #:*principal-roles*
   #:*principal-catalogue*
   #:*capability-denial-audit*
   #:make-principal-catalogue
   #:filter-catalogue-for-roles
   #:role-allowed-ops
   #:operation-granted-p
   #:catalogue-for-request
   #:ldap-groups-for-dn
   #:map-groups-to-roles
   #:wrap-corporate-auth
   #:parse-postgres-dsn
   #:postgres-claimable-lease-sql
   #:claim-task-postgres)
  (:documentation
   "Expert-domain model + KSAR controller. Persistence is the task-protocol journal."))

(in-package #:demiurge)
