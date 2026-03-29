# Design: `available-libraries` — Library Discovery

**Date:** 2026-03-28
**Status:** Approved — Implementation plan: `2026-03-28-available-libraries-impl.md`

## Problem

Wile has no mechanism to discover which libraries are importable. Users at the
REPL and MCP server tooling need to answer "what can I import?" without
trial-and-error.

## Use Cases

1. **REPL exploration** — user types `(available-libraries)` to browse what's available
2. **MCP server tooling** — programmatic discovery for LLM-driven IDE support

## Design Decisions

- **Names only** — returns library names, not exports. Callers drill into
  individual libraries via other tools (e.g., `(wile doc)`).
- **Both Go and Scheme API** — `Engine.AvailableLibraries(ctx)` for Go,
  `(available-libraries)` in introspection extension for Scheme.
- **Single mechanism** — enumeration uses the same FileResolver chain that
  resolves named imports. No parallel scanning infrastructure.
- **Path inference** — library names are inferred from file paths (reverse of
  `ToFilePath()`). This is correct for all importable libraries because the
  loader enforces path/name consistency.
- **Union with registry** — synthetic extension libraries (e.g., `(wile io)`)
  exist only in the LibraryRegistry, not on disk. Results are the union of
  filesystem-discovered and registry-known libraries.

## Architecture

### LibraryEnumerator Interface

New optional interface in `machine/`:

```go
type LibraryEnumerator interface {
    EnumerateLibraries() ([]LibraryName, error)
}
```

Enumeration is the inverse of resolution — same directories, same priority,
opposite direction:

| Resolver           | Resolution (name → file)                      | Enumeration (file → names)                        |
|--------------------|-----------------------------------------------|---------------------------------------------------|
| FSFileResolver     | Join search paths + name, try root             | Walk each search path (strip prefix); walk root   |
| OSFileResolver     | Join search paths + name, SCHEME_INCLUDE_PATH  | Walk search paths, SCHEME_INCLUDE_PATH, CWD       |
| ChainFileResolver  | Try each child in order                        | Union children's enumerations, first wins on dedup |
| EmbedFileResolver  | Direct lookup (bootstrap only)                 | Does not implement — bootstrap isn't libraries     |

### Discovery Flow

```
env.FileResolver() → type-assert LibraryEnumerator → EnumerateLibraries()
  + env.LibraryRegistry() → All() → synthetic extension library names
  → union, deduplicate by Key(), sort
```

Both `Engine.AvailableLibraries(ctx)` and `(available-libraries)` use this
same path.

### Path → LibraryName Conversion

- Strip extension (`.sld` or `.scm`)
- Split on `/`
- Each segment becomes a `LibraryName.Parts` element
- e.g., `scheme/base.sld` → `LibraryName{Parts: ["scheme", "base"]}`

## Deduplication

- Same library via multiple resolvers: first resolver wins (matches resolution priority)
- Same library as `.sld` and `.scm` within one resolver: `.sld` wins (matches loader preference)
- Same library on filesystem and in registry: included once
- Dedup key: `LibraryName.Key()`

## Filtering

- Skip hidden files/directories (starting with `.`)
- Only include `.sld` and `.scm` files
- OS filesystem: skip unreadable directories gracefully (continue walking)

## Security

Enumeration lists file names, doesn't read contents. No authorization check
needed — the authorizer gates `ActionLoad` on file opens, not directory listing.

## Error Handling

- I/O errors during walk (broken symlink, etc.): skip entry, continue
- No resolvers implement `LibraryEnumerator`: return just registry libraries
- Library system not enabled (no `WithLibraryPaths`): return empty list
- Empty result: return empty list, not an error

## API

### Go

```go
func (p *Engine) AvailableLibraries(ctx context.Context) ([]machine.LibraryName, error)
```

### Scheme

```scheme
(available-libraries)  ; => ((scheme base) (scheme char) (wile io) ...)
```

List of library name lists, sorted. Components are symbols or exact
nonnegative integers (matching R7RS library name syntax).

Lives in the introspection extension. Zero parameters, not variadic.

## Files to Change

| File | Change |
|------|--------|
| `machine/file_resolver.go` | Add `LibraryEnumerator` interface |
| `machine/file_resolver.go` | Implement on `FSFileResolver`, `OSFileResolver`, `ChainFileResolver` |
| `machine/library_registry.go` | Add `filePathToLibraryName()` helper, `AllNames()` method |
| `engine.go` | Add `Engine.AvailableLibraries(ctx)` method |
| `extensions/introspection/prim_introspection.go` | Add `available-libraries` primitive |
| `machine/file_resolver_test.go` | Tests for enumeration on each resolver type |
| `extensions/introspection/prim_introspection_test.go` | Integration test for Scheme primitive |
