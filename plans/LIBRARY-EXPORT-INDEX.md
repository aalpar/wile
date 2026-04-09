# Library Export Index — Apropos for Unloaded Libraries

**Status:** Complete (PRs #623, #625)
**Date:** 2026-04-09
**Current-state doc:** `plans/DOCUMENTATION-SEARCH.md`

## Problem

`apropos` only searches loaded bindings. Procedures in unloaded libraries are invisible.
An LLM calling `apropos "fold"` gets no hits for SRFI-1's `fold` because `(srfi 1)` isn't
imported. The LLM has no way to discover which library to import without guessing.

## Solution

Build a static export index by parsing `.sld` files without compiling them. On first
`apropos` call, scan all discoverable libraries, extract export names and descriptions,
and cache the index for the session. `SearchDoc` queries this index as a 6th search source,
returning results tagged as `"not imported"` with the source library name.

## Design

### New Types (`machine/compilation/library_export_index.go`)

```go
// LibrarySummary holds statically-parsed metadata from an .sld file.
type LibrarySummary struct {
    Name        LibraryName
    Description string
    Exports     []string // external names only
    SourceFile  string
}

// LibraryExportIndex maps library keys to their summaries.
// Built by scanning .sld files without compiling them.
// Immutable after construction.
type LibraryExportIndex struct {
    entries map[string]*LibrarySummary // keyed by LibraryName.Key()
}
```

### Static Parsing — `ParseLibrarySummary`

```go
func ParseLibrarySummary(ctx context.Context, r io.Reader, filePath string, name LibraryName) (*LibrarySummary, error)
```

1. Create parser: `parser.NewParserWithFile(nil, false, bufio.NewReader(r), filePath)`.
   The parser does not use the environment field — nil is safe.
2. `p.ReadSyntax(ctx)` — parse the first form.
3. Verify it's `(define-library <name> <decl> ...)` or `(library <name> <decl> ...)`.
4. Walk declarations. For each:
   - `(export <spec> ...)` — extract external names (symbol or `(rename _ external)`).
   - `(description <string>)` — extract string.
   - Everything else (`import`, `begin`, `include`, `cond-expand`) — skip.
5. Return `&LibrarySummary{Name, Description, Exports, SourceFile}`.

The declaration walker is new code that mirrors the keyword dispatch in
`processLibraryDeclaration` but simplified — only checks for `"export"` and
`"description"`, ignoring everything else. Worth the small duplication to avoid
coupling to the compiler.

### Index Construction — `BuildExportIndex`

```go
func BuildExportIndex(ctx context.Context, resolver FileResolver, reg *LibraryRegistry) (*LibraryExportIndex, error)
```

1. Enumerate all library names via `LibraryEnumerator` interface on the resolver.
2. For each name, skip if `reg.Lookup(name) != nil` (already loaded).
3. Open `.sld` file via resolver (try `.sld` first, then `.scm`, mirroring `LoadLibrary`).
4. Call `ParseLibrarySummary`.
5. On any error — skip silently. Best-effort index; one bad file must not poison search.
6. Store in `index.entries[name.Key()] = summary`.

If the resolver does not implement `LibraryEnumerator`, returns an empty index.

### SearchDoc Integration (`registry/search.go`)

`SearchDoc` signature changes:

```go
func SearchDoc(reg *Registry, env *environment.EnvironmentFrame,
    libReg *compilation.LibraryRegistry,
    exportIndex *compilation.LibraryExportIndex,
    pattern string) []DocSearchResult
```

New step 6 after loaded libraries:

```go
if exportIndex != nil {
    for _, r := range searchUnloadedExports(exportIndex, libReg, lowerPattern) {
        if seen[r.Name] { continue }
        seen[r.Name] = true
        q = append(q, r)
    }
}
```

`searchUnloadedExports` iterates the index, skipping libraries that are now loaded
(in case import happened after index construction), and matches export names against
the pattern. Results use:
- `Name`: the export name (e.g., `"fold"`)
- `Doc`: library description prefixed with library name (e.g., `"(srfi 1) — SRFI 1: List library"`)
- `Category`: `"not imported"`

### Lazy Construction (`repl/registry_doc_provider.go`)

`RegistryDocProvider` gains `indexOnce sync.Once` and `exportIndex *LibraryExportIndex`.
The index is built on the first `Search()` call:

```go
func (p *RegistryDocProvider) Search(pattern string) []registry.DocSearchResult {
    p.indexOnce.Do(func() {
        resolver := p.env.FileResolver()
        if resolver != nil {
            p.exportIndex, _ = compilation.BuildExportIndex(
                context.Background(), resolver, p.libReg)
        }
    })
    return registry.SearchDoc(p.reg, p.env, p.libReg, p.exportIndex, pattern)
}
```

No changes to `NewRegistryDocProvider` signature. No changes to `mcp.go`.

## Files Changed

| File | Change |
|------|--------|
| `machine/compilation/library_export_index.go` | **New.** `LibrarySummary`, `LibraryExportIndex`, `ParseLibrarySummary`, `BuildExportIndex` |
| `machine/compilation/library_export_index_test.go` | **New.** Tests for static parsing and index building |
| `registry/search.go` | `SearchDoc` gains `exportIndex` parameter; new `searchUnloadedExports` |
| `registry/search_test.go` | Tests for unloaded export search |
| `repl/registry_doc_provider.go` | `RegistryDocProvider` gains lazy index; `Search()` builds on first call |

## Implementation Notes

PR #625 extended `searchUnloadedExports` to also match library names and
descriptions (not just export names), mirroring `searchLibraries` for loaded
libraries. Results use category `"library (not imported)"` for library-level
matches and `"not imported"` for export-level matches.

PR #624 changed `RegistryDocProvider` to read the library registry dynamically
from the environment on each `Search()` call, rather than caching it at
construction time. This ensures libraries loaded after provider construction
are visible.

## Not In Scope

- `doc` for unloaded libraries (stays "not loaded")
- Per-binding docs from unloaded libraries (requires compilation)
- Per-export keywords from unloaded libraries (requires parsing implementation files)
- Cache invalidation (session-lifetime cache is sufficient)
- `topic`/`topics` integration (unloaded exports have no category)
- Scheme-level `(apropos)` export index access (see `PRIM-APROPOS-EXPORT-INDEX.md`)
