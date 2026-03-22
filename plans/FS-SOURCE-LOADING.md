# fs.FS for Source Loading

**Status:** Complete
**Date:** 2026-03-21

## Goal

All source file loading operations (include, load, library import) must support
`fs.FS`. When an embedder provides a virtual filesystem, it becomes the exclusive
source for Scheme files — the OS filesystem is not consulted.

## Scope

Category A only: read-only source loading. File I/O primitives (`open-input-file`,
`delete-file`, etc.) are out of scope.

## Public API

One new engine option:

```go
func WithSourceFS(fsys fs.FS) EngineOption
```

When set, all source loading resolves within `fsys`. The OS filesystem,
`SCHEME_INCLUDE_PATH`, and CWD fallbacks are not consulted. Library search paths
(from `WithLibraryPaths`) become relative paths within the FS.

When not set, behavior is unchanged — `OSFileResolver` is used.

## New Type: `FSFileResolver`

In `machine/file_resolver.go`:

```go
type FSFileResolver struct {
    fsys fs.FS
    env  *environment.EnvironmentFrame
}
```

Resolution strategy (same priority order as `OSFileResolver`, minus OS-specific
fallbacks):

1. Absolute paths rejected — `fs.FS` has no concept of absolute paths
2. Try relative to current load directory (from `LoadPathStack`)
3. Try each library registry search path
4. Try path as-is (relative to FS root)

Key differences from `OSFileResolver`:

| Concern              | OSFileResolver          | FSFileResolver                   |
|----------------------|-------------------------|----------------------------------|
| Path joining         | `filepath.Join`         | `path.Join`                      |
| Stat check           | `os.Stat`               | `fs.Stat(fsys, ...)`            |
| Open                 | `os.Open`               | `fsys.Open`                      |
| Absolute paths       | Supported               | Rejected                         |
| SCHEME_INCLUDE_PATH  | Consulted               | Ignored                          |
| CWD fallback         | Yes                     | No (FS root is implicit fallback)|
| Security auth        | Enforced                | Enforced                         |

`EmbedFileResolver` is unchanged — it stays as-is for bootstrap (no resolution
logic, direct path lookup).

## LoadPathStack Changes

Currently `LoadPathStack.Push` rejects non-absolute paths, and include/load skip
pushing when the path is relative. This breaks relative includes within virtual
filesystems: if `lib/math.sld` includes `"impl.scm"`, the stack doesn't know
we're in `lib/`, so `impl.scm` can't be found.

Fix: allow relative paths in `LoadPathStack`.

- Remove the `filepath.IsAbs` guard from `Push`
- Remove the `filepath.IsAbs` guard from include/load push sites
- `CurrentDir()` uses `path.Dir` for relative paths, `filepath.Dir` for
  absolute paths (distinguished by `filepath.IsAbs`)

The absolute-path requirement existed because on OS filesystems, relative paths
are ambiguous (relative to what CWD?). In an `fs.FS`, all paths are relative to
the FS root — they're unambiguous.

## Library Loading Through FileResolver

Currently `LoadLibrary` bypasses `FileResolver`:

```
filePath, err := environment.ResolveFile(stack, name.ToFilePath(), ...)
filePath, err = registry.FindLibraryFile(name)  // fallback, also os.Stat
file, err := os.Open(filePath)                   // bypasses FileResolver
```

Fix: route through `FileResolver`:

```
resolver := env.FileResolver().(FileResolver)
f, path, err := resolver.ResolveAndOpen(ctx, name.ToFilePath())    // .sld
if err != nil:
    f, path, err = resolver.ResolveAndOpen(ctx, name.ToFilePathScm()) // .scm
```

This eliminates direct `os.Open` in `loadLibraryFromFile` and `os.Stat` in
`FindLibraryFile`. The `.sld`/`.scm` extension fallback stays in `LoadLibrary`
— library naming conventions are a library-system concern, not a file-resolution
concern.

`FindLibraryFile` becomes dead code for the library loading path and should be
removed.

## What Doesn't Change

- **Bootstrap**: already uses `EmbedFileResolver(core.BootstrapFS)`
- **`environment/resolve.go` `ResolveFile`**: implementation detail of
  `OSFileResolver`, stays as-is
- **`runtime/runtime.go` `Load`**: takes `io.Reader` directly, no resolver
- **`cmd/wile/main.go`**: CLI opens files with `os.Open` before handing to
  engine — correct for an OS program
- **Existing tests**: no `WithSourceFS` means `OSFileResolver`, same behavior

## Files Changed

| File | Change |
|------|--------|
| `options.go` | Add `WithSourceFS(fs.FS)` option, `sourceFS` field on `engineConfig` |
| `engine.go` | Wire `FSFileResolver` when source FS is set |
| `machine/file_resolver.go` | Add `FSFileResolver` type |
| `machine/library_loader.go` | Route through `FileResolver` instead of `os.Open` |
| `environment/load_path_stack.go` | Allow relative paths in `Push` |
| `machine/compile_time_continuation_include.go` | Remove `filepath.IsAbs` guard on push |
| `internal/extensions/eval/prim_eval.go` | Remove `filepath.IsAbs` guard on push |

## Testing

**Unit tests for `FSFileResolver`** (in `machine/file_resolver_test.go`):
- Rejects absolute paths
- Resolves relative to load path stack current dir
- Resolves via library registry search paths
- Falls back to FS root
- Returns error for missing files with searched paths in message
- Security authorization enforced
- Empty path rejected

All tests use `testing/fstest.MapFS` — no OS filesystem needed.

**Unit tests for `LoadPathStack`** (in `environment/load_path_stack_test.go`):
- Relative paths accepted by `Push`
- `CurrentDir()` returns correct directory for relative paths
- Mixed absolute and relative paths on the stack

**Integration tests** (in root package or `machine/`):
- Library import from `fstest.MapFS` via `WithSourceFS` + `WithLibraryPaths`
- `(load ...)` from `fs.FS`
- Nested load/include resolves correctly within the FS
- Include within a library resolves relative to the library file
