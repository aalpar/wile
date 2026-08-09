# MCP Server

Wile includes a built-in [Model Context Protocol](https://modelcontextprotocol.io/)
server that exposes the Scheme interpreter to AI assistants over stdio. Any
MCP-compatible client (Claude Code, Cursor, Windsurf, Zed, etc.) can evaluate
Scheme, browse documentation, and search primitives without reading Go source.

## Starting the server

```bash
wile --mcp
```

The server communicates via JSON-RPC over stdin/stdout. It is mutually exclusive
with `-e`, `-f`, and `-i`.

| Flag | Default | Description |
|------|---------|-------------|
| `--mcp` | — | Start as MCP server on stdio |
| `--mcp-timeout <seconds>` | 30 | Default eval timeout (0 = no caller-supplied deadline, bounded by the 10-minute server maximum) |

## Client configuration

### Claude Code

In `.mcp.json` (project root) or `~/.claude.json` (global):

```json
{
  "mcpServers": {
    "wile": {
      "type": "stdio",
      "command": "wile",
      "args": ["--mcp"]
    }
  }
}
```

### Cursor / Windsurf

In `.cursor/mcp.json` or equivalent:

```json
{
  "mcpServers": {
    "wile": {
      "command": "wile",
      "args": ["--mcp"]
    }
  }
}
```

### VS Code (Copilot)

In `.vscode/mcp.json`:

```json
{
  "servers": {
    "wile": {
      "type": "stdio",
      "command": "wile",
      "args": ["--mcp"]
    }
  }
}
```

If `wile` is not on `$PATH`, use the full path to the binary.

## Tools

Every tool and resource handler runs under one session lock, so requests are
serialized. A handler that cannot take the session within 30 seconds answers
`server busy: ...` rather than blocking. That bound is deliberate and
independent of any eval's timeout: mcp-go handles non-tool messages inline on
its read loop, so a handler that blocked without bound would stop the server
reading *any* further stdin message, not just delay one call.

### `eval`

Evaluate one or more Scheme expressions in a persistent session. Definitions,
imports, and state carry forward across calls. Forward references between
definitions within a single call work: the tool routes through
`Engine.EvalProgram`, which splices every top-level form into one `(begin ...)`.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `code` | string | yes | Scheme expression(s) to evaluate |
| `timeout` | number | no | Per-call timeout in seconds (overrides session default; 0 = no caller-supplied deadline, bounded by the 10-minute server maximum) |

Returns JSON: `{"output":"...", "value":"..."}` where `output` is captured stdout
(`display`, `write`, `newline`) and `value` is the result of the last expression.
Fields are omitted when empty.

Errors (parse failures, runtime exceptions, timeouts) are returned as MCP tool
errors (`IsError: true`) with the error message. If output was captured before the
error, it is included alongside the error text.

### `doc`

Show documentation for a Scheme binding or library.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `name` | string | yes | Binding name (e.g. `car`) or library name (e.g. `(scheme base)`) |
| `examples` | boolean | no | Include usage examples in the output (default `false`) |

Returns signature, description, parameter types, category, and source.

### `apropos`

Search Scheme bindings by name, documentation text, category, or keywords.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `pattern` | string | yes | Search pattern (substring, case-insensitive) |

### `topics`

List all documentation categories with entry counts. No parameters.

### `topic`

List all bindings in a documentation category.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `category` | string | yes | Category name (use `topics` to see available categories) |

### `libraries`

List the libraries currently loaded in the session, then the libraries
discoverable but not yet imported, each sorted alphabetically with its
description. No parameters.

### `disassemble`

Show bytecode disassembly for a Scheme procedure.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `name` | string | yes | Procedure name (bound in the current session) |

Returns a textual listing for the named procedure. Scheme (`lambda`)
procedures yield a full bytecode disassembly (opcodes, literals, branch
targets, cached binding names, source locations). Go-implemented
(`foreign`) primitives yield a one-line summary (name, parameter count,
variadic flag, docstring) — foreign primitives have no Scheme bytecode to
disassemble. Errors are returned as MCP tool errors if the name is
unbound or does not resolve to a procedure (e.g., a syntax binding or
plain value).

### `reset`

Discard all session state (definitions, imports) and reinitialize the engine on
the next call. No parameters.

### `set-timeout`

Set the default eval timeout for the session.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `seconds` | number | yes | Timeout in seconds (0 = no caller-supplied deadline, bounded by the 10-minute server maximum) |

## Resources

Resources are read-only JSON snapshots of engine state, fetched via
`resources/read`. They do not mutate the session.

### `wile://session`

Current session state. Returns JSON:

```json
{
  "initialized": true,
  "libraryCount": 42,
  "primitiveCount": 180,
  "timeoutSeconds": 30
}
```

If the engine has not been initialized yet, `initialized` is `false` and the
count fields are omitted.

### `wile://libraries`

The libraries currently loaded in the session. Unlike the `libraries` tool, this
resource omits the discoverable-but-not-yet-imported ones. Initializes the
engine on first access. Returns a JSON array:

```json
[
  {"name": "(scheme base)", "description": "R7RS base library"},
  {"name": "(wile algebra)", "description": "Algebraic structures"}
]
```

### `wile://primitives`

All registered primitives with signatures and documentation. Initializes the
engine on first access. Returns a JSON array:

```json
[
  {"name": "car", "category": "pair", "paramCount": 1, "doc": "..."},
  {"name": "+", "category": "arithmetic", "paramCount": 0, "variadic": true, "doc": "..."}
]
```

Fields `category`, `variadic`, and `doc` are omitted when empty/false.

## Prompts

### `wile-scheme`

A prompt template that provides session model documentation, available libraries,
common patterns, and discovery tool usage. Accepts a `task` argument describing
what the AI assistant should accomplish.

## Session model

- **Persistent state**: Definitions, imports, and bindings accumulate across
  `eval` calls within a single MCP session.
- **All extensions loaded**: The engine starts with the `KitchenSink` profile —
  filesystem, math, system, threads, eval, Go interop, introspection, and more.
- **Stdout isolation**: Scheme output (`display`, `write`) is captured per-call and
  returned in the `output` field. It never reaches the MCP JSON-RPC transport.
- **Panic recovery**: VM panics (e.g. uncaught `raise`) are caught and returned as
  tool errors. The server remains operational.
- **Serialized access**: All tool calls are serialized via mutex. The engine is not
  accessed concurrently.

## Engine configuration

The MCP server creates an engine equivalent to the CLI's, plus a mutable top
level:

```go
wile.NewEngine(ctx,
    wile.WithProfile(wile.KitchenSink),
    wile.WithSourceFS(stdlib.FS),
    wile.WithSourceOS(),
    wile.WithLibraryPaths(...),
    wile.WithMutableTopLevel(),
)
```

This means all R7RS libraries and wile extensions are available via `(import ...)`.
`WithMutableTopLevel` opts out of the engine-wide immutable-top-level default:
redefining a binding across `eval` calls is a primary workflow here, as in the
REPL. It costs the frame-reclamation optimizer's top-level payoff.
