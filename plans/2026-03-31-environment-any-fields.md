# environment/ any Fields: Type Safety Plan

**Status**: Complete
**Date**: 2026-03-31
**Related**: TODO.md "Extract interface types from `environment/` `any` fields"

## Problem

`Namespace` stores four fields as `any` to avoid circular imports with higher packages:

| Field | Concrete type | Package |
|-------|--------------|---------|
| `fileResolver` | `machine/compilation.FileResolver` | `machine/compilation/` |
| `libraryRegistry` | `*machine/compilation.LibraryRegistry` | `machine/compilation/` |
| `registry` | `*registry.Registry` | `registry/` |
| `authorizer` | `security.Authorizer` | `security/` |

Every callsite type-asserts the returned `any`, losing type safety at the boundary.

## Import Graph Analysis

```
environment/ ──imports──> values/, internal/syntax, werr/
machine/compilation/ ──imports──> machine/ ──imports──> environment/
registry/ ──imports──> environment/, machine/
security/ ──imports──> werr/ ONLY
```

## What Can Be Fixed

### Phase 1: authorizer — Direct import of security/

`security/` only imports `werr/`. No cycle is introduced by `environment/ → security/`.

Changes:
- `environment/` imports `"github.com/aalpar/wile/security"`
- `Namespace.authorizer any` → `authorizer security.Authorizer`
- `Namespace.Authorizer() any` → `Authorizer() security.Authorizer`
- `Namespace.SetAuthorizer(auth any)` → `SetAuthorizer(auth security.Authorizer)`
- `NamespaceDeriveConfig.Authorizer any` → `Authorizer security.Authorizer`
- `EnvironmentFrame` delegation methods updated accordingly
- All callers: type assertions `.(security.Authorizer)` removed (5 sites in file_resolver.go, 1 in machine_context.go)

### Phase 2: fileResolver — Move interface to environment/

`FileResolver` uses only stdlib types (`context.Context`, `io/fs.File`).
`machine/compilation/` already imports `environment/`.

Changes:
- New file `environment/file_resolver.go` defines:
  ```go
  type FileResolver interface {
      ResolveAndOpen(ctx context.Context, path string) (fs.File, string, error)
  }
  ```
- `machine/compilation/file_resolver.go`: replace interface definition with type alias:
  `type FileResolver = environment.FileResolver`
- `Namespace.fileResolver any` → `fileResolver FileResolver`
- `Namespace.FileResolver() any` → `FileResolver() FileResolver`
- `Namespace.SetFileResolver(any)` → `SetFileResolver(FileResolver)`
- `EnvironmentFrame.FileResolver() any` → `FileResolver() FileResolver`
- `EnvironmentFrame.SetFileResolver(any)` → `SetFileResolver(FileResolver)`
- Panics in `compile_time_continuation.go` (guarding against wrong type in `any`) removed
- Type assertions in engine.go, eval extension, introspection extension simplified

### Phase 3: libraryRegistry — Narrow interface in environment/

`environment/` never calls methods on `libraryRegistry` — just stores/propagates it.
`machine/compilation/file_resolver.go` only needs `GetSearchPaths() []string` from it.

Changes:
- `environment/file_resolver.go` (or new `environment/library.go`) adds:
  ```go
  // LibrarySearcher is implemented by library registries that support
  // path-based file discovery.
  type LibrarySearcher interface {
      GetSearchPaths() []string
  }
  ```
- `Namespace.libraryRegistry any` → `libraryRegistry LibrarySearcher`
- `Namespace.LibraryRegistry() any` → `LibraryRegistry() LibrarySearcher`
- `Namespace.SetLibraryRegistry(any)` → `SetLibraryRegistry(LibrarySearcher)`
- `EnvironmentFrame.LibraryRegistry() any` → `LibraryRegistry() LibrarySearcher`
- `EnvironmentFrame.SetLibraryRegistry(any)` → `SetLibraryRegistry(LibrarySearcher)`
- `machine/compilation/file_resolver.go`: type assertions `regAny.(*LibraryRegistry)` removed;
  use `searcher.GetSearchPaths()` directly (4 sites)
- `engine.go` still type-asserts to `*compilation.LibraryRegistry` where full type is needed
  (this is valid — Go allows type assertion from any interface to concrete type)

### Phase 4: registry — Leave as any

`registry/` imports both `environment/` and `machine/`, making a cycle unavoidable.
`environment/` never calls any methods on `registry` — just stores/propagates.
All callers already hold direct references to `*registry.Registry` and type-assert.
No feasible narrow interface.

## Files Affected

| File | Change |
|------|--------|
| `environment/namespace.go` | Change 3 field types + import security |
| `environment/environment_frame.go` | Update FileResolver/LibraryRegistry signatures |
| `environment/file_resolver.go` | NEW — FileResolver interface + LibrarySearcher interface |
| `machine/compilation/file_resolver.go` | Type alias + remove type assertions |
| `machine/compilation/compile_time_continuation.go` | Remove panics + simplify nil check |
| `machine/machine_context.go` | Remove Authorizer() type assertion |
| `engine.go` | Simplify Authorizer nil check |
| `internal/extensions/eval/prim_eval.go` | Remove FileResolver type assertion |
| `extensions/introspection/prim_introspection.go` | Remove FileResolver type assertion |

## Verification

`make lint && make covercheck` must both pass after all phases.
