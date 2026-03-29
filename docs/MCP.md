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
| `--mcp-timeout <seconds>` | 30 | Default eval timeout (0 = no timeout) |

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

### `eval`

Evaluate one or more Scheme expressions in a persistent session. Definitions,
imports, and state carry forward across calls. Forward references between
definitions within a single call work (code is wrapped in `(begin ...)`).

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `code` | string | yes | Scheme expression(s) to evaluate |
| `timeout` | number | no | Per-call timeout in seconds (overrides session default; 0 = no timeout) |

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

Returns signature, description, parameter types, category, and source.

### `apropos`

Search Scheme bindings by name, documentation text, or category.

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

List all Scheme libraries currently loaded in the session. No parameters.

### `reset`

Discard all session state (definitions, imports) and reinitialize the engine on
the next call. No parameters.

### `set-timeout`

Set the default eval timeout for the session.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `seconds` | number | yes | Timeout in seconds (0 = no timeout) |

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

All Scheme libraries available in the session. Initializes the engine on first
access. Returns a JSON array:

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
- **All extensions loaded**: The engine starts with `AllExtensions()` — filesystem,
  math, system, threads, eval, Go interop, introspection, and more.
- **Stdout isolation**: Scheme output (`display`, `write`) is captured per-call and
  returned in the `output` field. It never reaches the MCP JSON-RPC transport.
- **Panic recovery**: VM panics (e.g. uncaught `raise`) are caught and returned as
  tool errors. The server remains operational.
- **Serialized access**: All tool calls are serialized via mutex. The engine is not
  accessed concurrently.

## Engine configuration

The MCP server creates an engine equivalent to the CLI:

```go
wile.NewEngine(ctx,
    wile.WithAllExtensions(),
    wile.WithSourceFS(stdlib.FS),
    wile.WithSourceOS(),
    wile.WithLibraryPaths(...),
)
```

This means all R7RS libraries and wile extensions are available via `(import ...)`.
