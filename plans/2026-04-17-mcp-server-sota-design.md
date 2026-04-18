# Wile MCP Server — Bringing to State of the Art

**Status:** Proposed (design only)
**Author:** Audit against `mcp-server-dev:build-mcp-server` best-practices skill
**Related prior work:**
- `2026-03-26-wile-mcp-server-design.md` — original server design (shipped in PR #588)
- `memory/MCP-EVAL-HARDENING.md` — P0 hardening (stdout isolation, timeouts, output capture) — shipped
- `cmd/wile/mcp.go` — current implementation (698 lines)
- `docs/embedding/mcp.md` — user-facing docs

## Goal

Bring the Wile MCP server from a solid baseline (stdio transport, 9 well-described tools, 3 resources, 1 prompt, panic-safe, timeout-gated) to alignment with current MCP best-practices. The audit found one real UX gap (progress reporting during long evals) and several polish / future-capability items. This plan phases them by cost and user-visible impact.

## Current state — audit summary

Against the `build-mcp-server` skill's checklist:

| Dimension | Status | Notes |
|---|---|---|
| Tool count & pattern | ✅ | 9 tools, one-per-action — correct choice for this surface |
| Tool descriptions | ✅ | Multi-sentence *when to use*, return-shape documented |
| Capability declarations | ✅ | `WithToolCapabilities`/`WithPromptCapabilities`/`WithResourceCapabilities` explicit |
| Server instructions | ✅ | `WithInstructions` populated — many servers skip this |
| Resources primitive | ✅ | 3 structured JSON resources with `MIMEType` set |
| Prompts primitive | ✅ | `wile-scheme` with `task` argument |
| Panic recovery | ✅ | `defer recover()` on every VM-invoking handler |
| Output isolation | ✅ | `current-output-port` redirected off stdout |
| Mutex serialization | ✅ | Engine is single-threaded; server honors it |
| Timeout / cancellation | ✅ | `ctx.Done()` checked every 1024 VM ops (`machine/machine_context.go:329-336`); per-call override supported |
| Framework | ✅ | `github.com/mark3labs/mcp-go v0.45.0` — the standard Go SDK |
| **Progress notifications** | ❌ | Long-running `eval` is silent until completion |
| **Elicitation capability** | ❌ | Not advertised, no tools need it today |
| **Remote transport option** | ❌ | stdio only; no streamable-HTTP path for shared deployments |
| Doc description accuracy | ⚠️ | `doc` tool description oversells structure ("returns signature, description, parameter types, category") — handler returns formatted text |
| Design-rationale docs | ⚠️ | `docs/embedding/mcp.md` doesn't explain *why* stdio, trade-offs of HTTP |
| Prompt templating | ⚠️ | Naive `strings.ReplaceAll` on `{{k}}` — order-dependent if user input contains other `{{…}}` |

## Non-goals

- **Not** migrating to MCPB — Wile ships as a single Go binary; MCPB's purpose (bundle Node/Python runtime) doesn't apply.
- **Not** building MCP apps (rich UI widgets) — no concrete use case; would force an `@modelcontextprotocol/ext-apps` dependency.
- **Not** removing stdio — it remains the default and correct transport for single-developer workflows.
- **Not** changing framework (mcp-go) — it supports every capability this plan needs (verified in `github.com/mark3labs/mcp-go v0.45.0`: progress, elicitation, streamable HTTP).

## Phased plan

Phases are ordered by **UX impact / cost ratio**. Earlier phases deliver more for less. Later phases are capability-building or speculative.

---

### Phase 1 — Progress notifications for `eval`

**Problem:** A `wile eval` call running a Gabriel benchmark, a zebra puzzle, or an infinite loop approaching its 30 s timeout emits zero progress until it finishes (or times out). Clients render a frozen spinner.

**Solution:** If the client included a `progressToken` on the call, emit `notifications/progress` on a ticker while `engine.Run` executes.

**Signal shape.** The natural progress signal is instruction count — the VM already tracks `mc.counters.OpsExecuted` for cancellation batching. A progress goroutine can read it periodically and emit:

```
{ "progressToken": ..., "progress": <ops>, "message": "<ops> ops executed" }
```

Progress is *monotonic* but has no known total — that's MCP-spec-legal (`total` is optional). If the VM exposes a lighter surface (e.g., compile ops / eval ops split), surface it; otherwise ops-executed is good enough.

**Cadence.** Every 250ms — fast enough for UI responsiveness, slow enough not to flood the transport.

**Implementation sketch (in `cmd/wile/mcp.go`):**

```
// inside handleEval, after evalTimeout is set up:
progressToken := req.Params.Meta.ProgressToken  // nil if client didn't request
if progressToken != nil {
    ticker := time.NewTicker(250 * time.Millisecond)
    defer ticker.Stop()
    done := make(chan struct{})
    defer close(done)
    go func() {
        for {
            select {
            case <-done:
                return
            case <-ticker.C:
                ops := p.engine.VM().OpsExecuted() // needs minor plumbing
                server.SendNotificationToClient(ctx,
                    mcp.NewProgressNotification(progressToken, float64(ops), ""))
            }
        }
    }()
}
```

**Open questions:**
1. Does `engine.VM().OpsExecuted()` already exist as a public accessor? If not, adding one is trivial but needs an API decision (see `docs/embedding/api-design.md`).
2. Should we emit progress for the `disassemble` tool too? (Answer: no — disassembly is always fast; ops-count progress would be noise.)
3. `runMeta` wraps several tools (`doc`, `apropos`, …) that *could* be slow for large result sets. Punt — measure first.

**Files changed:** `cmd/wile/mcp.go` (progress emitter), possibly `wile/engine.go` (expose ops counter).
**Test:** `cmd/wile/mcp_test.go` — send an eval with a `_meta.progressToken`, verify ≥ 1 progress notification arrives before completion for a deliberately slow expression.

**Estimated size:** one day, ≤ 100 LoC.

---

### Phase 2 — Documentation accuracy & design rationale

Two sub-items, both doc-only.

#### 2a. Correct the `doc` tool description

`mcp.go:117-122` promises structured fields ("signature, description, parameter types, category, and source") but the handler delegates to `,doc` which returns human-formatted text. Rewrite the description to match reality:

> "Show formatted documentation for a Scheme binding or library. For bindings, pass the name (e.g. `car`, `map`, `define`). For libraries, pass the name in parentheses (e.g. `(scheme base)`). Returns a human-readable block containing signature, parameters, description, category, and source when available."

#### 2b. Add "Why stdio" section to `docs/embedding/mcp.md`

A paragraph explaining:
- stdio suits single-developer workflows (one client, one session, no auth, no hosting cost)
- remote HTTP would be a *different product* (shared sessions, sandbox concerns, deployment)
- MCPB is redundant — Wile is already a single binary; `go install` is the install path
- when remote HTTP *would* make sense (shared team Scheme scratchpad, CI-driven analysis)

This prevents the "shouldn't this be HTTP?" conversation from recurring.

**Files changed:** `cmd/wile/mcp.go` (description text), `docs/embedding/mcp.md` (new section).
**Estimated size:** one hour.

---

### Phase 3 — Streamable HTTP transport (opt-in, behind flag)

**Rationale:** The MCP spec's recommended distribution path is remote streamable HTTP. Wile will stay stdio-default, but adding an opt-in `--mcp-http <addr>` flag opens the door to:
- shared-team Scheme scratchpad servers
- CI / Claude-in-CI integration
- experimental multi-user use cases (e.g., Wile-as-a-shared-notebook-kernel)

`mcp-go v0.45` ships `server.NewStreamableHTTPServer`. The existing `doMCP` can dispatch based on a flag:

```
if opts.MCPHTTP != "" {
    httpSrv := server.NewStreamableHTTPServer(s, server.WithStateful(false))
    return httpSrv.Start(opts.MCPHTTP)
}
return server.ServeStdio(s)
```

**Hard constraints:**
1. **No authentication in v1.** Remote HTTP Wile is explicitly documented as *trusted-network only* — the server runs `KitchenSink` profile with full filesystem + process + eval access. Exposing this to the internet is equivalent to giving remote code execution to any caller.
2. **Single-session model retained.** The engine mutex serializes access regardless of transport — HTTP adds no concurrency to the VM. Multiple concurrent HTTP clients would queue; document this.
3. **Behind a flag with a loud warning.** `--mcp-http` prints `WARNING: trusted-network only; no authentication` on startup.

**Open questions:**
1. Should HTTP mode switch to a reduced profile (`Small` or `Console`) by default? Probably yes — but then stdio mode and HTTP mode expose different tools, which is confusing. Defer until there's a real user.
2. Authentication: OAuth / bearer token in a later phase. The skill's `references/auth.md` has the canonical pattern — out of scope for v1.

**Files changed:** `cmd/wile/main.go` (flag), `cmd/wile/mcp.go` (transport switch), `docs/embedding/mcp.md` (new "Remote HTTP" section with warnings).
**Test:** integration test that spins up an HTTP server, issues an `initialize` + `eval`, tears down.

**Estimated size:** two days, ≈ 200 LoC + tests.

---

### Phase 4 — Elicitation capability (infrastructure, no consumers yet)

**Rationale:** MCP elicitation lets a server request structured user input mid-tool-call. `mcp-go v0.45` supports it via `session.RequestElicitation`. Wile has no tool that *needs* elicitation today, but:

1. Advertising the capability costs nothing.
2. Once advertised, a future tool (e.g., `authorize-dangerous-op` for a `system-write` gate, or `confirm-destructive-eval` before `reset`) can use it without a protocol change.

**Scope:**
- Add `server.WithElicitationCapability()` (or equivalent — check mcp-go API) to the `NewMCPServer` options.
- **Do not** add any tool that uses it yet.
- Document the availability in `docs/embedding/mcp.md` so future tool authors know to consider it.

**Open question:** Is there a near-term use case? Candidates:
- `reset` could elicit confirmation when session has ≥ N user-defined bindings (prevents accidental loss).
- A hypothetical `eval-with-authorizer` tool could elicit per-resource approval.

If no concrete use case materializes within 60 days of Phase 4 shipping, *defer* this phase — an advertised-but-unused capability is noise in the client's UI.

**Files changed:** `cmd/wile/mcp.go` (capability flag), `docs/embedding/mcp.md` (note).
**Estimated size:** one hour if trivial; the real work is deciding whether to ship it.

---

### Phase 5 — Speculative / not scheduled

Items worth naming so we don't forget, but that the audit found *low priority*:

| Item | Why deferred |
|---|---|
| Parameterized resources (`wile://doc/<name>`) | `doc` tool works; resources would duplicate surface. Revisit if a client treats resources as cheaper than tool calls. |
| `resources.listChanged` notifications | Resource list is static. Would only matter if `reset` changed available resources, which it doesn't. |
| Migrate `set-timeout` from tool to writable resource | Semantically cleaner but client support for writable resources is uneven. |
| MCP app widgets (rich UI) | No use case; would add a large dependency surface. |
| Reduce profile for HTTP mode | See Phase 3 open question — wait for real user feedback. |
| Structured output from `doc`/`apropos` (JSON variants) | Adds API surface; no consumer requesting it. Contrast with `wile://primitives` resource which already provides structured output. |

---

## Risks

1. **Progress notifications could fire after `handleEval` returns**, if the goroutine doesn't shut down cleanly. Mitigation: `defer close(done)` before starting the goroutine; `<-done` wins the select. Verify in test.
2. **Phase 3 (HTTP) expands the blast radius** if a deployment misunderstands the "trusted-network only" caveat. Mitigation: loud startup warning, docs with threat model, keep it behind an explicit flag.
3. **Capability advertisements change client behavior.** Adding `WithElicitationCapability` may cause some clients to render UI elements for a feature Wile doesn't use yet. Test against Claude Code and one other client before shipping Phase 4.
4. **mcp-go API drift.** Phases 1, 3, 4 all depend on mcp-go APIs. Pin the version when implementing each phase; don't do `go get -u` mid-plan.

## Success criteria

After Phase 1: A client issuing `eval` with a `progressToken` receives ≥ 1 progress notification for any computation running ≥ 250 ms. Verified by an integration test.

After Phase 2: No description in `mcp.go` promises a shape the handler doesn't deliver. `docs/embedding/mcp.md` answers the "why stdio?" question in one paragraph.

After Phase 3: `wile --mcp-http :8080` serves an MCP session indistinguishable (from the client's point of view) from `wile --mcp`, modulo the transport.

After Phase 4: Elicitation capability appears in the `initialize` response. Future tools can call `session.RequestElicitation` without server surgery.

## Out-of-scope (explicitly not in this plan)

- Authentication / authorization for HTTP mode — separate design, separate plan.
- Rewriting the server in TypeScript (the skill's default recommendation) — would lose in-process engine access; the Go-native integration is the point.
- Migrating to MCPB — irrelevant for a single static Go binary.
- Anything touching the Scheme VM beyond exposing a read-only `OpsExecuted()` accessor.
