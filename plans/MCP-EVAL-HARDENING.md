# MCP Eval Hardening

Fix four issues with the MCP eval tool, from critical to minor.

## Issues

1. **Critical: stdout corruption** — `display`/`write` writes to `os.Stdout`, which IS the MCP JSON-RPC transport.
2. **Important: forward references** — `EvalMultiple` compiles each expression independently; `(define (f) (g))` fails if `g` isn't yet defined.
3. **Minor: no output capture** — eval result includes only the final value, not any `display`/`write` output.
4. **Minor: no eval timeout** — infinite loops hang the server with no recovery.

## Design

### Output redirect + capture (issues 1 & 3)

- In `initLocked`, after engine creation: redirect `current-output-port` to `io.Discard` via `ioext.SetCurrentOutputPort()`. This prevents any Scheme output from reaching the MCP transport.
- In `handleEval`: redirect to a `bytes.Buffer` before eval, capture output, restore to `io.Discard` after.
- In `handleReset`: call `ioext.ResetState()` to clean up global port state.
- Return structured JSON result: `{"output":"...", "value":"..."}` with `omitempty`.

### Begin wrapping (issue 2)

Wrap user code in `(begin ...)` before eval, matching the pattern in `runFile` (`cmd/wile/main.go:361`):
```go
code = "(begin " + code + "\n)"
```

### Configurable timeout (issue 4)

- `--mcp-timeout` CLI flag (float64 seconds, default 30).
- `eval` tool gets optional `timeout` parameter (float64 seconds) for per-call override.
- `set-timeout` MCP tool for changing the session default.
- Implementation: `context.WithTimeout(ctx, duration)` before eval.

## Files changed

| File | Changes |
|------|---------|
| `cmd/wile/main.go` | Add `MCPTimeout` to Options struct |
| `cmd/wile/mcp.go` | Bulk: output redirect, begin wrap, timeout, structured result, set-timeout tool |
| `cmd/wile/prompts/wile-scheme.md` | Document timeout, output capture, set-timeout |
