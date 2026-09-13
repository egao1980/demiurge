# demiurge

Product core for [cl-stack](https://github.com/egao1980/cl-stack) expert systems ([#197](https://github.com/egao1980/cl-stack/issues/197)): an `expert-domain` is a catalogue + KS set + steering skills + RAG corpora + eval suites. `defexpert` is thin sugar over `make-instance`. Control stays on the `blackboard-protocol` KSAR agenda.

| System | Role |
|--------|------|
| `demiurge` (`stack-demiurge`) 0.3.3 | `expert-domain`, `defexpert`, `agent-ks`, controller, personal profile |
| `demiurge/improve` 0.3.3 | Versioned-KS improvement cycle |
| `demiurge/observe` 0.3.3 | Span/metric taxonomy, `/healthz` + `/readyz`, profiles |
| `demiurge/serve` 0.3.3 | MCP / A2A / AG-UI Clack app + feedback |
| `demiurge/ingest` 0.3.3 | Durable file / IMAP / object-store ingest |
| `demiurge/workflows` 0.3.3 | Durable project workflows + deep-research fan-out |
| `demiurge/bundle` 0.3.3 | Expert-bundle pack / hash-verified install / rollback (local OCI layout) |

```lisp
(asdf:load-system "demiurge")

(let ((board (stack-demiurge:run-expert
              (stack-demiurge:make-echo-expert
               :backend (stack-llm:make-mock-llm-backend))
              :trigger '(:prompt "hi"))))
  (blackboard-protocol:read-section board :result))
;; ⇒ "echo: hi"
```

**Config** (`cl-stack-config`, TOML + `DEMIURGE_*` env): `agenda.max-concurrency`, `ksar.timeout-seconds`, `session.window-turns`, `llm.default-model`, `llm.catalog`, `paths.data-dir`, `improve.enabled`. `(load-demiurge-config &key path)`.

**Persistence:** `task-protocol` journal (SQLite via `task-backend-sql` in the personal profile) + `blackboard-journal`. `(resume-domain name profile)` replays the board and re-arms timers. KSAR execute is a `with-durable-step`.

**Observe:** spans `demiurge.ksar.execute` / `demiurge.agent.run`. Logs use `log-protocol` `with-context` `:trace-id` / `:span-id`. Never emit spans through the logger.

**Personal profile:** `(make-personal-profile &key data-dir)` — SQLite sessions + journal, `rag-backend-text` + memory/sql store, LLM catalog (llama-cpp or LM Studio from config).

**Serve** (`demiurge/serve`): `(make-expert-app domain profile)` is a Clack dispatcher (AG-UI POST→SSE, `/feedback`, `/healthz`, `/readyz`). `/readyz` mounts `demiurge/observe` when that system is loaded and the profile has stores; otherwise the stub (`*readyz-fn*` / `domain-ready-p`). `(serve-expert domain &key transports)` starts stdio-MCP and/or HTTP. Feedback (`demiurge.feedback` / MCP `record_feedback`) calls `eval-protocol:add-case` with `:source :human-feedback`.

**Ingest** (`demiurge/ingest`): `(run-ingest domain source &key store)` is a durable task. `file-source` (pathlib glob), `imap-source`, `s3-source` enumerate items with a content-hash idempotency key; extract → `chunk-extracted-document` / `block-tree-chunker` → embed/upsert; mark-and-sweep drops hashes the source no longer lists.

**Workflows** (`demiurge/workflows`): `(start-project domain spec)` is a named `task-protocol` tree bound to one board. Milestones are `milestone-reached` journal checkpoints plus durable `wait-input` (`await-approval`). `(run-deep-research domain question &key max-rounds budget)` plans (schema-typed) → `spawn-child-task` per sub-question (RAG + `search-web` + optional browser) → `join-children :policy :all` → bounded gap rounds → C3d `extracted-document` with citation annotations → A1 eval gate → C3e markdown (PDF if loaded). Progress writes a board section and, when serve/wire is loaded, A2A task state + AG-UI `STATE_DELTA`.

**Bundle** (`demiurge/bundle`): `(pack-expert domain &key registry version)` writes a local OCI layout (`oci-layout` + `index.json` + `blobs/sha256/…`) with checksum annotations (cosign slot reserved). `(install-expert ref &key profile)` is a durable task: pull → verify every content hash (`bundle-verification-error` on mismatch; no `skip-verification` restart) → register domain → `run-ingest` as child durable steps with unique `ingest-item/<hash>` names. `(rollback-expert name version)` re-registers the prior manifest and `rollback-skill` on the A4/`steer-protocol` file skill store.

**Reference experts:** `make-echo-expert` (minimal) and `make-cl-dev-expert` (lookup-symbol / search-corpus / `:compute`-gated run-tests, steering skills, docs corpus, ~20 eval cases).

## License

MIT — see [LICENSE](LICENSE).
