# Documentation Search Architecture

**Status:** Current (consolidated from earlier design docs)
**Date:** 2026-04-09

**Supersedes** (these remain as historical records of design decisions; this file
describes the implemented system):
- `2026-03-27-apropos-topic-browsing-design.md` — original apropos & topic design
- `2026-04-08-doc-keywords-design.md` — Keywords field design
- `2026-04-08-doc-keywords-impl.md` — Keywords implementation plan
- `2026-04-08-unified-apropos-design.md` — unified `SearchDoc` design
- `2026-04-08-unified-apropos-impl.md` — unified search implementation plan
- `2026-04-08-eager-doc-index-design.md` — eager library metadata scan (superseded by export index)
- `LIBRARY-EXPORT-INDEX.md` — export index design
- `LIBRARY-EXPORT-INDEX-IMPL.md` — export index implementation plan

## Overview

The documentation search system enables discovery of procedures, special forms,
macros, and libraries via `apropos` (substring search) and topic browsing
(category-based). Two entry points exist:

| Entry Point | Layer | Access |
|-------------|-------|--------|
| `,apropos <pattern>` | REPL meta-command | `RegistryDocProvider.Search` → `registry.SearchDoc` |
| `(apropos <pattern>)` | Scheme primitive | `PrimApropos` → `registry.SearchDoc` |

Both delegate to the same `registry.SearchDoc` function, which searches six
sources in priority order.

## Search Pipeline

### `registry.SearchDoc` (`registry/search.go`)

```go
func SearchDoc(
    reg      *Registry,
    env      *environment.EnvironmentFrame,
    libReg   *compilation.LibraryRegistry,
    exportIndex *compilation.LibraryExportIndex,
    pattern  string,
) []DocSearchResult
```

Six sources, searched in order. Earlier sources take precedence via dedup:

| # | Source | What Matches | Category |
|---|--------|-------------|----------|
| 1 | Registry primitives | name, doc, category, keywords | from spec |
| 2 | Registry binding specs | name, doc, category, keywords (parsed) | from docstring |
| 3 | Registry doc entries | name, doc, category, keywords (parsed) | from docstring |
| 4 | Environment bindings | name, doc, category, keywords (from value) | from docstring |
| 5 | Loaded libraries | library name, description | `"library"` |
| 6 | Unloaded library exports | library name, description, export names | `"library (not imported)"` or `"not imported"` |

Match logic: case-insensitive substring against name, doc text, category, or any
keyword. Results sorted by name. Duplicates eliminated by name (earlier source wins).

### Source 6: Unloaded Library Exports

`searchUnloadedExports` (`registry/search.go`) searches the `LibraryExportIndex`
for libraries not yet loaded. It emits two kinds of results:

1. **Library-level match**: When the library name or description matches the pattern,
   emits a result with `Name = "(wile algebra)"`, `Category = "library (not imported)"`,
   `Doc = description`. This mirrors `searchLibraries` for loaded libraries.

2. **Export-level match**: When an individual export name matches, emits a result
   with `Name = "make-group"`, `Category = "not imported"`,
   `Doc = "(wile algebra) — description"`.

Libraries already present in `libReg` are skipped (they were imported after the
index was built and are covered by source 5).

### `matchesDoc` (`registry/search.go`)

Shared match predicate used by sources 1-4:

```go
func matchesDoc(name, doc, category string, keywords []string, pattern string) bool
```

Case-insensitive substring match against any of the four fields.

## Library Export Index

### Types (`machine/compilation/library_export_index.go`)

```go
type LibrarySummary struct {
    Name        LibraryName
    Description string
    Exports     []string   // external names only
    SourceFile  string
}

type LibraryExportIndex struct {
    entries map[string]*LibrarySummary // keyed by LibraryName.Key()
}
```

### Static Parsing — `ParseLibrarySummary`

Parses `.sld` files without compiling or executing. Extracts:
- Library name (from `define-library` or `library` keyword)
- `(export ...)` declarations — simple symbols and `(rename internal external)` specs
- `(description "...")` declaration

All other declarations (`import`, `begin`, `include`, `cond-expand`) are skipped.
Uses `internal/parser` and `internal/syntax` — no compiler involvement.

### Index Construction — `BuildExportIndex`

1. Enumerate all library names via `LibraryEnumerator` interface on the resolver
2. Skip libraries already in `LibraryRegistry` (already loaded)
3. Open `.sld` file via resolver (try `.sld` first, then `.scm`)
4. Call `ParseLibrarySummary`
5. On any error — skip silently (best-effort; one bad file must not poison search)

If the resolver does not implement `LibraryEnumerator`, returns an empty index.

### Lazy Construction — `RegistryDocProvider`

`RegistryDocProvider` (`repl/registry_doc_provider.go`) builds the index lazily
on the first `Search()` call via `sync.Once`. The library registry is read
dynamically from the environment on each call (not cached at construction time),
so libraries loaded after provider construction are visible.

```go
type RegistryDocProvider struct {
    reg *registry.Registry
    env *environment.EnvironmentFrame

    indexOnce   sync.Once
    exportIndex *compilation.LibraryExportIndex
}
```

Constructor takes `(reg, env)` — no `libReg` parameter. The library registry
is retrieved dynamically via `registry.ExtractLibraryRegistry(env)`.

## Entry Points

### REPL `,apropos` (`repl/meta.go`)

`cmdApropos` delegates to `DocSearchProvider.Search()`. The provider is
`RegistryDocProvider`, which calls `SearchDoc` with all six sources including
the lazy export index. Output format:

```
  name                  [category]  one-line description
```

### Scheme `(apropos)` (`registry/core/prim_reflection.go`)

`PrimApropos` calls `SearchDoc` directly. Currently passes `nil` for `exportIndex`,
so unloaded library exports are **not** searched from the Scheme primitive.
See "Known Asymmetry" below.

Returns a flat list of symbols (names from all matching results).

### MCP `apropos` tool (`cmd/wile/mcp.go`)

Delegates to `,apropos` via the REPL meta-command handler. Gets full search
coverage including unloaded libraries.

## Topic Browsing

Two REPL commands and two Scheme primitives for category-based browsing:

| Command/Primitive | Function |
|-------------------|----------|
| `,topics` / `(doc-topics)` | List all categories with counts |
| `,topic <cat>` / `(doc-topic <cat>)` | List all bindings in a category |

Topic browsing only covers registry primitives and parsed doc entries — environment
bindings and unloaded exports have no category metadata for browsing.

## Keywords

`Keywords` is a `[]string` field on `PrimitiveSpec`, `DocSearchResult`, and parsed
from structured docstrings via `docparse.ParseDocstring`. Format in docstrings:

```
Keywords: sort, ordering, comparison
```

Comma-separated, single line. Keywords enable discovery when the procedure name
and doc prose don't contain the search term (e.g., searching "algebra" finds
`make-group` via its `Keywords: group, inverse, abelian, symmetry, algebraic structure`).

Keywords are searched by `matchesDoc` for sources 1-4. For source 6 (unloaded
exports), keywords from docstrings are **not** available because the export index
only parses `.sld` declarations, not implementation files containing docstrings.

## Known Asymmetry: Scheme-Level `(apropos)` vs REPL `,apropos`

`PrimApropos` passes `nil` for the `exportIndex` parameter, so it does not search
unloaded library exports. The REPL `,apropos` (via `RegistryDocProvider`) does.

**Why**: `PrimApropos` runs inside the VM via `CallContext`. It has no access to
the cached `LibraryExportIndex`, which lives on `RegistryDocProvider` in the REPL
layer.

**Proposed fix** (deferred): Store the cached index on `Namespace` (accessible from
both layers) via `ExportIndex()`/`SetExportIndex()`, mirroring the existing
`LibraryRegistry()` pattern. See `plans/PRIM-APROPOS-EXPORT-INDEX.md`.

**Impact**: Low. The MCP tool (primary LLM consumer) uses the REPL path. The
Scheme `(apropos)` is used interactively where `(import ...)` is the natural
discovery mechanism.

## Files

| File | Role |
|------|------|
| `registry/search.go` | `SearchDoc`, `DocSearchResult`, `matchesDoc`, all search helpers |
| `registry/search_test.go` | Unit tests for all search sources |
| `machine/compilation/library_export_index.go` | `LibrarySummary`, `LibraryExportIndex`, `ParseLibrarySummary`, `BuildExportIndex` |
| `machine/compilation/library_export_index_test.go` | Unit tests for static parsing and index building |
| `repl/registry_doc_provider.go` | `RegistryDocProvider` with lazy export index |
| `repl/meta.go` | `,apropos`, `,topics`, `,topic` commands |
| `registry/core/prim_reflection.go` | `PrimApropos`, `PrimDocTopics`, `PrimDocTopic` |
| `docparse/docparse.go` | `ParseDocstring` — extracts Category, Keywords, Parameters, etc. |

## Design History

| Document | Status | Notes |
|----------|--------|-------|
| `2026-03-27-apropos-topic-browsing-design.md` | **Complete** | Original apropos & topic design |
| `2026-04-08-doc-keywords-design.md` | **Complete** | Keywords field design |
| `2026-04-08-doc-keywords-impl.md` | **Complete** | Keywords implementation plan |
| `2026-04-08-unified-apropos-design.md` | **Complete** | Unified `SearchDoc` design |
| `2026-04-08-unified-apropos-impl.md` | **Complete** | Unified search implementation plan |
| `2026-04-08-eager-doc-index-design.md` | **Superseded** | Resolved by `LibraryExportIndex` (simpler version) |
| `LIBRARY-EXPORT-INDEX.md` | **Complete** | Export index design (PR #623) |
| `LIBRARY-EXPORT-INDEX-IMPL.md` | **Complete** | Export index implementation plan (PR #623) |
| `PRIM-APROPOS-EXPORT-INDEX.md` | **Proposed** | Fix Scheme-level asymmetry (deferred) |
| `2026-04-06-structured-docstring-metadata-design.md` | **Complete** | Structured docstring parsing |
| `2026-04-06-structured-docstring-metadata-impl.md` | **Complete** | Structured docstring implementation |
| `2026-03-28-library-level-documentation-design.md` | **Complete** | Library description fields |
| `2026-03-27-scheme-library-docstrings-design.md` | **Complete** | Scheme docstring conventions |
| `2026-03-27-special-form-macro-docstrings-design.md` | **Complete** | Special form docs |
| `2026-03-27-procedure-documentation-design.md` | **Complete** | `procedure-documentation` primitive |

## Implementation PRs

| PR | Change |
|----|--------|
| #620 | Unify documentation conventions across special forms and primitives |
| #621 | Unify documentation output format across all form types |
| #622 | Propagate Keywords through doc-only primitive registration |
| #623 | Library export index for unloaded library discovery in apropos |
| #624 | Read library registry dynamically in RegistryDocProvider |
| #625 | Search unloaded library names and descriptions in apropos |
