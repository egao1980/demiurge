# cl-stack / Demiurge corpus (bundled)

`defexpert` is thin sugar over `make-instance` of `expert-domain` plus registry.
KSAR control lives on the blackboard-protocol agenda — no polling loop.
Section writes, KSAR enqueue, and workspace fork/merge are journaled through
`task-protocol` via `blackboard-journal`. Replay is `replay-blackboard`.

Logs use `log-protocol` `with-context` with `:trace-id` / `:span-id`. Never emit
spans through the logger; spans are `telemetry-protocol` (`demiurge.ksar.execute`,
`demiurge.agent.run`).

Personal profile: SQLite (`conversation-backend-sql`, `task-backend-sql`),
file corpora via `rag-protocol` + `rag-backend-text` splitter and
`rag-backend-memory` / `rag-backend-sql`, local LLM catalog
`llm-backend-llama-cpp` or LM Studio. Config is `cl-stack-config` (TOML + env).

`agent-ks` wraps an `ai-agent-protocol` agent. Durable steps use
`with-durable-step`. `run-tests` is gated behind a granted `:compute`
capability. Steering skills are `steer-protocol`. The self-improvement
promotion gate is `no-critical-regression-gate` composed with
`mean-improvement-gate`.
