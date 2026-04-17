# PrimApropos Export Index — Closing the Scheme-Level Asymmetry

**Status:** Complete
**Date:** 2026-04-09

> Implemented: ExportIndex()/SetExportIndex() on Namespace, Engine stores on Namespace after building, PrimApropos lazy-builds and passes to SearchDoc.
**Context:** Crosscheck finding from PR #623

## Problem

The MCP `apropos` tool searches unloaded library exports via
`RegistryDocProvider`'s lazy `LibraryExportIndex`. The Scheme-level
`(apropos "fold")` primitive (`PrimApropos`) passes `nil` for the export
index, so it never finds procedures in unloaded libraries.

Same logical operation, different result sets depending on entry point.

## Why It Exists

`PrimApropos` runs inside the VM via `CallContext`. It has access to:
- `mc.Context()` — Go context
- `mc.EnvironmentFrame()` — environment (has `FileResolver()`, `LibraryRegistry()`)

It does NOT have access to:
- A cached `LibraryExportIndex` — this lives on `RegistryDocProvider` in the REPL layer

The index must be built once and cached. Currently the cache lives on
`RegistryDocProvider` (via `sync.Once`). `PrimApropos` operates at a lower layer
(`registry/core/`) with no access to the REPL's doc provider.

## Proposed Solution

Store the cached `LibraryExportIndex` on the `Namespace`, which is accessible
from both layers.

### Changes Required

| File | Change |
|------|--------|
| `environment/namespace.go` | Add `ExportIndex() *compilation.LibraryExportIndex` and `SetExportIndex(...)` methods |
| `repl/registry_doc_provider.go` | After building the index in `sync.Once`, store it on the namespace via `SetExportIndex` |
| `registry/core/prim_reflection.go` | `PrimApropos` retrieves the index via `env.Namespace().ExportIndex()` and passes to `SearchDoc` |
| `registry/search.go` | No change — `SearchDoc` already accepts the index parameter |

### Flow

```
First apropos call (either REPL or Scheme):
  RegistryDocProvider.Search() → sync.Once → BuildExportIndex → ns.SetExportIndex(idx)

Subsequent calls:
  RegistryDocProvider.Search() → sync.Once (no-op) → ns.ExportIndex()
  PrimApropos → env.Namespace().ExportIndex() → SearchDoc(... idx ...)
```

### Alternative: Build in PrimApropos directly

`PrimApropos` could call `BuildExportIndex` itself with its own `sync.Once`.
This avoids touching `Namespace` but creates two independent index caches —
one in `RegistryDocProvider`, one in some package-level variable in
`registry/core/`. Worse: they could disagree about which libraries were
loaded at index-build time.

Single cache on `Namespace` is cleaner.

### Import Cycle Risk

`environment/namespace.go` would need to reference `*compilation.LibraryExportIndex`.
Current dependency: `environment/` does NOT import `machine/compilation/`.

Two options to avoid the cycle:
1. **Interface**: Define an `ExportIndexer` interface in `environment/` that
   `LibraryExportIndex` satisfies. `PrimApropos` type-asserts to the concrete type.
2. **`any` field**: Store as `any` on `Namespace` (like `LibraryRegistry()` already does).
   Type-assert at retrieval sites.

Option 2 matches the existing `LibraryRegistry()` pattern — it's stored as `any`
and type-asserted to `*compilation.LibraryRegistry` at each use site.

### Scope

Small change (~20 lines of production code). No new packages, no new interfaces
if using option 2. The `Namespace.ExportIndex()` / `SetExportIndex()` pair mirrors
the existing `LibraryRegistry()` / `SetLibraryRegistry()` pair exactly.

## Decision

Implemented. The asymmetry is closed: both REPL/MCP and Scheme-level `(apropos)`
now search unloaded library exports via the same `SearchDoc` + `LibraryExportIndex` path.
