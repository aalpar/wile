# Wile MCP Server Design

**Status:** **Complete** (PR #588). Implemented as integrated `--mcp` flag in `cmd/wile` rather than standalone binary. Tools: `eval`, `disassemble`, `doc`, `apropos`, `topic`, `topics`, `libraries`, `reset`.

## Summary

A standalone MCP server binary (`wile-mcp`) that exposes the Wile Scheme
interpreter as an MCP tool over stdio. Separate repo and Go module, consuming
only Wile's public embedding API. V1 ships a single `eval` tool; v2 adds
introspection (disassembly, primitive documentation, search).

## Motivation

1. **Prove the embedding API.** If `wile-mcp` can't do something useful, that's
   a gap in the public API — not a reason to reach into internals.
2. **AI assistant productivity.** A persistent Scheme session lets AI assistants
   smoke-test expressions, verify behavior, and explore the language without
   reading Go source.
3. **Broader reach.** Any MCP-compatible client (Claude Code, Cursor, Windsurf,
   Zed, etc.) gains access to a Scheme evaluator.

## Architecture

```
wile-mcp (single binary, stdio MCP server)
  ┌──────────┐    ┌─────────────────────────┐
  │ mcp-go   │───→│ Wile Engine             │
  │ stdio    │    │  + AllExtensions()      │
  │ server   │←───│  + stdlib embedded FS   │
  └──────────┘    │  + OS file resolver     │
       ↕          └─────────────────────────┘
   stdin/stdout
   (JSON-RPC)
```

- **Repo:** `github.com/aalpar/wile-mcp`
- **Module:** `github.com/aalpar/wile-mcp`
- **Binary:** `wile-mcp` — single purpose, no CLI flags beyond `--version`
- **Engine lifetime:** One persistent Engine per session, lazy-initialized on
  first `eval` call
- **Engine config:** `WithAllExtensions()`, `WithSourceFS(stdlib.FS)`,
  `WithSourceOS()`, `WithLibraryPaths(...)` — mirrors `cmd/wile`

## V1: `eval` Tool

Single tool. Ship fast, prove the pattern.

### Tool Definition

| Parameter | Type   | Required | Description                        |
|-----------|--------|----------|------------------------------------|
| `code`    | string | yes      | Scheme expression(s) to evaluate   |

### Description (shown to MCP clients)

> Evaluate Scheme expressions in a persistent R7RS session. State (defines,
> imports) persists across calls. All standard extensions loaded (filesystem,
> math, system, threads, eval, Go interop).

### Behavior

- Uses `Engine.EvalMultiple(ctx, code)`
- Success: returns `val.SchemeString()` as text; empty string for void results
- Errors: MCP tool error (`IsError: true`) with the error message — parse
  failures, runtime exceptions, all surface here
- No explicit reset — restarting the MCP server resets state

### New Public API Required

None. V1 uses only existing public APIs.

## V2 Roadmap

V2 adds introspection tools and sandboxing.

### New Tools

| Tool                 | Description                                      | New wile API needed                    |
|----------------------|--------------------------------------------------|----------------------------------------|
| `disassemble`        | Compile expression, return bytecode listing      | `CompiledCode.Disassemble() string`    |
| `describe-primitive` | Name → signature, param count, variadic, doc     | None — `Registry().Primitives()` works |
| `search-primitives`  | Pattern → matching primitives (name or category) | None — filter `Primitives()` locally   |
| `reset`              | Destroy and recreate engine                      | None — internal to wile-mcp            |

### New wile Public API: Disassemble

The minimal change is one method on `CompiledCode`:

```go
// Disassemble returns a human-readable bytecode listing.
func (p *CompiledCode) Disassemble() string
```

Delegates to `NativeTemplate` internally. Keeps the template field unexported.

### Primitive Documentation

The `Doc` field on `PrimitiveSpec` exists but is sparsely populated. The
`describe-primitive` and `search-primitives` tools depend on a documentation
pass across all extensions in the `wile` repo. This is a prerequisite for v2,
not part of it.

### Sandboxing

`--safe` flag: uses `SafeExtensions()` instead of `AllExtensions()`, excludes
`WithSourceOS()`. For hosted or shared MCP server deployments.

## Dependencies

| Dependency                         | Purpose                  |
|------------------------------------|--------------------------|
| `github.com/aalpar/wile`           | Engine, stdlib, extensions |
| `github.com/mark3labs/mcp-go`      | MCP server framework     |

No other dependencies.

## Project Structure

```
wile-mcp/
├── main.go          # entry point, MCP server setup, eval handler
├── go.mod
├── go.sum
├── README.md
├── LICENSE          # Apache 2.0 (matches wile)
└── .mcp.json        # example MCP client config
```

Single file for v1. The entire server is ~80 lines. Split when v2 adds tools.

## Workspace Integration

Add to `go.work` at the workspace root alongside `wile` and `wile-goast`:

```
use (
    ./wile
    ./wile-goast
    ./wile-mcp
    ./wile-extension-example
)
```

## Install

```bash
go install github.com/aalpar/wile-mcp@latest
```

## Alternatives Considered

### Binary architecture

- **`cmd/wile-mcp/` inside the wile repo:** Simpler dependency management but
  doesn't prove the public API is sufficient. Rejected.
- **`wile --mcp` flag:** Mirrors wile-goast pattern, but wile is already the
  CLI and this mixes concerns. Rejected.
- **Separate repo (`wile-mcp`):** Chosen. Proves embedding API, clean boundary,
  independent release cycle.

### Tool granularity

- **One tool per primitive:** Maximum discoverability but rigid — adding a
  primitive means updating the MCP surface. Rejected.
- **Layer-oriented tools:** Middle ground. Deferred to v2.
- **Single `eval` tool:** Chosen for v1. Maximum flexibility, thinnest wrapper.

### Engine lifecycle

- **New engine per call:** Stateless but expensive and loses session context.
  Rejected.
- **Lazy init, persistent:** Chosen. Matches wile-goast pattern.
