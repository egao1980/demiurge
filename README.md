# demiurge

Product core for [cl-stack](https://github.com/egao1980/cl-stack) expert systems ([#197](https://github.com/egao1980/cl-stack/issues/197)): an `expert-domain` is a catalogue + KS set + steering skills + RAG corpora + eval suites. `defexpert` is thin sugar over `make-instance`. Control stays on the `blackboard-protocol` KSAR agenda.

| System | Role |
|--------|------|
| `demiurge` (`stack-demiurge`) 0.2.0 | `expert-domain`, `defexpert`, `agent-ks`, controller, personal profile |

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

**Persistence:** `task-protocol` journal (SQLite via `task-backend-sql` in the personal profile) + `blackboard-protocol/journal`. `(resume-domain name profile)` replays the board and re-arms timers. KSAR execute is a `with-durable-step`.

**Observe:** spans `demiurge.ksar.execute` / `demiurge.agent.run`. Logs use `log-protocol` `with-context` `:trace-id` / `:span-id`. Never emit spans through the logger.

**Personal profile:** `(make-personal-profile &key data-dir)` — SQLite sessions + journal, `rag-backend-text` + memory/sql store, LLM catalog (llama-cpp or LM Studio from config).

**Reference experts:** `make-echo-expert` (minimal) and `make-cl-dev-expert` (lookup-symbol / search-corpus / `:compute`-gated run-tests, steering skills, docs corpus, ~20 eval cases).

## License

MIT — see [LICENSE](LICENSE).
