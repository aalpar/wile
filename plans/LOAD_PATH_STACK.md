# Plan: Load-Path Stack

**Status**: Planned — Not started

## Problem

Three file-loading entry points (`load`, `include`, `import`) resolve paths independently and inconsistently. None track which file is currently being loaded. `(load "helper.scm")` looks relative to CWD, not the loading file's directory. `PrimLoad` doesn't pass filename to parser (no source tracking).

## Architecture

### Where the stack lives

`TopLevelEnvironment.loadPathStack` — the only type reachable from all three entry points (runtime, compile-time, library loading) without import cycles. Child environments delegate to parent via pointer chain.

### Unified resolver

All entry points share one resolver:

```
resolve(path, stack, fallbackDirs) → absPath
  1. Absolute path → use as-is
  2. Stack non-empty → try relative to CurrentDir()
  3. Try each fallback directory
  4. Error listing all searched paths
```

Each entry point provides its own fallbacks:
- `PrimLoad`: `[]string{cwd}`
- `CompileInclude`: `$SCHEME_INCLUDE_PATH` entries
- `LoadLibrary`: `registry.searchPaths`

### Thread safety

`sync.Mutex` on the stack. Known limitation: concurrent `(load ...)` from multiple SRFI-18 threads can corrupt LIFO ordering (mutex prevents data races but not logical interleaving). Matches most Scheme implementations.

## Phases

| Phase | Description | Files |
|-------|-------------|-------|
| 1 | Core data structure — `LoadPathStack` | `environment/load_path_stack.go` (new) |
| 2 | Unified file resolver | `environment/resolve.go` (new) |
| 3 | Wire into `TopLevelEnvironment` | `environment/top_level_environment.go`, `environment_frame.go` |
| 4 | Integrate with `PrimLoad` | `internal/extensions/eval/prim_eval.go` |
| 5 | Integrate with `CompileInclude` | `machine/compile_time_continuation.go` |
| 6 | Integrate with `LoadLibrary` | `machine/library_loader.go` |
| 7 | Scheme primitives: `current-load-path`, `current-load-directory` | `internal/extensions/eval/` |
| 8 | Engine API: `WithLoadPath`, `PushLoadPath`, `PopLoadPath` | `engine.go` |
| 9 | Integration tests | Various |

Phases 4, 5, 6 are independent after Phase 3.

## Side Effects / Fixes

- **Fixes**: `PrimLoad` will use `NewParserWithFile()` (correct source locations in errors)
- **Fixes**: `findFile` not-found returns error with searched paths (was `nil, "", nil`)
- **Eliminates TOCTOU**: Removes `os.Stat` then `os.Open` double-syscall pattern

## Design Decisions

- `Push` enforces absolute paths (panics on relative — Design by Contract)
- Push happens **after** `os.Open` succeeds (no stale entries from nonexistent files)
- `Pop()` returns void (no caller inspects return value)
- Per-thread stack keying deferred (concurrent `load` is uncommon)
