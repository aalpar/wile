# Plan: Load-Path Stack

**Status**: Planned

## Context

Wile has three file-loading entry points (`load`, `include`, `import`) that all resolve paths independently and inconsistently. None of them track which file is currently being loaded, which means:

- `(load "helper.scm")` inside `/app/scripts/main.scm` looks for `helper.scm` relative to CWD, not relative to `main.scm`
- R7RS `include` only searches `$SCHEME_INCLUDE_PATH`, not relative to the including file
- Nested loads have no provenance chain for error reporting
- `PrimLoad` doesn't even pass the filename to the parser (uses `NewParser` not `NewParserWithFile`)

This plan adds a **load-path stack** to `TopLevelEnvironment` that tracks which files are currently being loaded. All three entry points push on entry and pop on exit, enabling relative path resolution and laying the foundation for future load-path authorization (sandboxing).

Inspired by SLIB's `program-vicinity` concept, adapted for Wile's R7RS architecture.

## Goals

1. Relative `load` resolution: `(load "helper.scm")` resolves from the loading file's directory
2. Relative `include` resolution: `(include "utils.scm")` resolves from the including file's directory
3. Relative `import` resolution: library imports try relative to the importing library first
4. Scheme-visible introspection: `(current-load-path)` and `(current-load-directory)`
5. Go embedding API: `Engine.CurrentLoadPath()` etc. for embedder visibility
6. Fix existing bug: `PrimLoad` now passes filename to the parser for source tracking

## Architecture

### Where the stack lives

```
                     TopLevelEnvironment
                    ┌──────────────────────────────┐
                    │ loadPathStack *LoadPathStack  │
                    └──────────┬───────────────────┘
                               │
         ┌─────────────────────┼──────────────────────┐
         │                     │                      │
  PrimLoad (runtime)    CompileInclude         LoadLibrary
  mc.env                (compile-time)         (compile-time)
    │                   p.env                  callerEnv
    └────► env.TopLevelEnv().LoadPathStack() ◄─┘
```

`TopLevelEnvironment` is the only type reachable from all three entry points without import cycles. `MachineContext` is runtime-only (no access at compile time). `LibraryRegistry` is stored as `any` to break import cycles — not suitable.

Child `TopLevelEnvironment` instances access the stack via parent delegation (not a stored pointer), consistent with how `symbolInterns` delegates. The stack only lives on the root `TopLevelEnvironment`.

### Resolution priority — unified resolver

All three entry points share a single resolution function rather than implementing resolve logic independently. The core operation is a left fold over resolution strategies that short-circuits on first success:

```
resolve(path, stack, fallbackDirs) → (absPath string, err error)

  1. If absolute path → use as-is
  2. If load stack non-empty → try relative to CurrentDir()
  3. Try each fallback directory in order
  4. Error listing all searched paths
```

Each entry point provides its own fallback directories:
- `PrimLoad`: `[]string{cwd}` (CWD-relative, existing behavior)
- `CompileInclude`: `$SCHEME_INCLUDE_PATH` entries (existing behavior)
- `LoadLibrary`: `registry.searchPaths` (existing behavior)

The resolver guarantees all returned paths are absolute. Callers never call `filepath.Abs` at push sites — the invariant is enforced at the resolver boundary.

### Thread safety

`LoadPathStack` uses `sync.Mutex`, consistent with `TopLevelEnvironment.symbolInternsMu`. The mutex protects against data races on the slice.

**Known limitation**: Concurrent `(load ...)` calls from multiple SRFI-18 threads sharing a `TopLevelEnvironment` can corrupt the LIFO ordering — Thread A's `Pop()` may remove Thread B's entry. The mutex prevents data races but not logical interleaving. This matches most Scheme implementations, which do not guarantee thread-safe `load`. Single-threaded and sequential loading (the common case) is fully correct.

```
Time    Thread A                    Stack state              Thread B
─────   ────────────────────────    ─────────────────────    ────────
t₁      push("/app/main.scm")      [/app/main.scm]
t₂                                  [/app/main.scm,          push("/lib/util.scm")
                                     /lib/util.scm]
t₃      pop() → gets               [/app/main.scm]          ← WRONG entry
         /lib/util.scm ← WRONG
```

## Phases

### Phase 1: Core data structure — `LoadPathStack`

**New file: `environment/load_path_stack.go`**

```go
type LoadPathStack struct {
    mu    sync.Mutex
    paths []string // absolute paths, top = paths[len-1]
}

func NewLoadPathStack() *LoadPathStack
func (s *LoadPathStack) Push(absPath string)   // panics if !filepath.IsAbs
func (s *LoadPathStack) Pop()
func (s *LoadPathStack) Current() string       // top without removing, "" if empty
func (s *LoadPathStack) CurrentDir() string    // filepath.Dir(Current()), "" if empty
func (s *LoadPathStack) Depth() int
```

`Push` enforces the representation invariant: all entries are absolute paths. If called with a relative path, it panics (Design by Contract — the caller's obligation).

**New file: `environment/load_path_stack_test.go`**

Table-driven tests: empty returns, LIFO ordering, CurrentDir extraction, concurrent push/pop, Push-with-relative-path panic.

### Phase 2: Unified file resolver

**New file: `environment/resolve.go`**

```go
// ResolveFile finds a file by trying resolution strategies in order:
// 1. If path is absolute, use as-is
// 2. If stack has a current directory, try relative to it
// 3. Try each fallback directory
// Returns the absolute path of the first match, or an error listing all searched paths.
func ResolveFile(stack *LoadPathStack, path string, fallbackDirs []string) (string, error)
```

On not-found, the error includes all paths that were attempted, e.g.: `"file "helper.scm" not found; searched: /app/scripts/, /usr/share/scheme/ (set $SCHEME_INCLUDE_PATH or load from a file context)"`.

**New file: `environment/resolve_test.go`**

Table-driven tests: absolute passthrough, stack-relative resolution, fallback directory search, not-found error message content, empty stack + empty fallbacks.

### Phase 3: Wire into `TopLevelEnvironment`

**Modify: `environment/top_level_environment.go`**

Add `loadPathStack *LoadPathStack` field to `TopLevelEnvironment` struct.

Initialize eagerly in `NewTopLevelEnvironment()`:
```go
q := &TopLevelEnvironment{
    symbolInterns: make(map[values.Symbol]*values.Symbol),
    syntaxInterns: make(map[values.Value]syntax.SyntaxValue),
    loadPathStack: NewLoadPathStack(),  // NEW
}
```

Do NOT store the stack in `NewChildTopLevelEnvironment()`. Use parent delegation:
```go
func (p *TopLevelEnvironment) LoadPathStack() *LoadPathStack {
    if p.parent != nil {
        return p.parent.LoadPathStack()
    }
    return p.loadPathStack
}
```

This is consistent with how `InternSymbol` delegates via the `parent` pointer.

**Modify: `environment/environment_frame.go`**

Add convenience accessor:
```go
func (p *EnvironmentFrame) LoadPathStack() *LoadPathStack {
    if p.topLevel == nil {
        return nil
    }
    return p.topLevel.LoadPathStack()
}
```

### Phase 4: Integrate with `PrimLoad`

**Modify: `internal/extensions/eval/prim_eval.go`**

Current `PrimLoad` (lines 78-137):
```
filename → os.Open(filename) → NewParser(env, true, rdr) → loop
```

New flow:
```
filename → ResolveFile(stack, filename, []string{cwd})
         → os.Open(absPath)
         → stack.Push(absPath) / defer stack.Pop()    ← push AFTER open succeeds
         → NewParserWithFile(env, true, rdr, absPath)  // fixes missing source tracking
         → loop
```

Push happens **after** `os.Open` succeeds. If `os.Open` fails, the stack is never modified — no stale entries from nonexistent files.

### Phase 5: Integrate with `CompileInclude`

**Modify: `machine/compile_time_continuation.go`**

Update `findFile` — use the unified resolver and always return an error for not-found (never `nil, "", nil`):

```go
func findFile(p *CompileTimeContinuation, _ CompileTimeCallContext, path string) (fs.File, string, error) {
    stack := p.env.LoadPathStack()
    fallbacks := filepath.SplitList(os.Getenv(SchemeIncludePathEnv))

    absPath, err := environment.ResolveFile(stack, path, fallbacks)
    if err != nil {
        return nil, "", err  // error includes searched paths
    }

    f, err := os.Open(absPath)
    if err != nil {
        return nil, "", err
    }
    return f, absPath, nil
}
```

Update `compileIncludeImpl` — push/pop around each included file:
```go
stack := p.env.LoadPathStack()
if stack != nil {
    stack.Push(absPath)  // absPath guaranteed absolute by ResolveFile
    defer stack.Pop()
}
```

Callers of `findFile` now check only `err != nil` (no more `file == nil` with `err == nil`).

### Phase 6: Integrate with `LoadLibrary`

**Modify: `machine/library_loader.go`**

In `LoadLibrary`, use the unified resolver before `FindLibraryFile`:
```go
stack := env.LoadPathStack()
absPath, err := environment.ResolveFile(stack, name.ToFilePath(), registry.SearchPaths())
if err != nil {
    // ResolveFile didn't find it — fall back to FindLibraryFile for backward compat
    absPath, err = registry.FindLibraryFile(name)
}
```

In `loadLibraryFromFile`, push/pop around file loading:
```go
stack := callerEnv.LoadPathStack()
if stack != nil {
    stack.Push(absPath)  // absPath guaranteed absolute by resolver/FindLibraryFile
    defer stack.Pop()
}
```

### Phase 7: Scheme primitives

**Modify: `internal/extensions/eval/prim_eval.go`**

Add two new primitives:
```go
// (current-load-path) → string or #f
func PrimCurrentLoadPath(_ context.Context, mc *machine.MachineContext) error

// (current-load-directory) → string or #f
func PrimCurrentLoadDirectory(_ context.Context, mc *machine.MachineContext) error
```

Returns the file/directory at top of stack, or `#f` if stack is empty (e.g., REPL).

**Modify: `internal/extensions/eval/register.go`**

```go
{"current-load-path", 0, false, PrimCurrentLoadPath},
{"current-load-directory", 0, false, PrimCurrentLoadDirectory},
```

### Phase 8: Engine API

**Modify: `engine.go`**

Primary scoped API:
```go
func (e *Engine) WithLoadPath(absPath string, fn func() error) error {
    e.PushLoadPath(absPath)
    defer e.PopLoadPath()
    return fn()
}
```

Secondary raw API for advanced embedders:
```go
func (e *Engine) CurrentLoadPath() string
func (e *Engine) CurrentLoadDirectory() string
func (e *Engine) PushLoadPath(absPath string)
func (e *Engine) PopLoadPath()
```

`WithLoadPath` is the recommended API — it guarantees balanced push/pop via `defer`. Raw Push/Pop is available for embedders who need fine-grained control.

### Phase 9: Tests

**Phase 1-2 tests**: Unit tests for `LoadPathStack` and `ResolveFile` (created in those phases).

**Modify/New: `internal/extensions/eval/` tests** — test `current-load-path` / `current-load-directory` return `#f` when no file is being loaded.

**Integration tests**:
1. Temp directory with `main.scm` that `(load "sub/helper.scm")`
2. Verify relative resolution works
3. Verify nested loads resolve correctly (main → sub/helper → ../util)
4. Verify `(current-load-path)` returns correct value at each nesting level
5. Verify stack is empty after all loads complete (depth-zero assertion)
6. Verify `findFile` error messages include searched paths

## Phase Dependencies

```
Phase 1 ──► Phase 2 ──► Phase 3 ──┬──► Phase 4 (PrimLoad)
                                    ├──► Phase 5 (CompileInclude)
                                    ├──► Phase 6 (LoadLibrary)
                                    │
                                    └──► Phase 7 (Scheme primitives)
                                              │
                                              ├──► Phase 8 (Engine API)
                                              └──► Phase 9 (Tests)
```

Phases 4, 5, 6 are independent of each other and can be done in any order after Phase 3.

## Files Summary

| File | Change | Phase |
|------|--------|-------|
| `environment/load_path_stack.go` | **New** — stack type with Push invariant | 1 |
| `environment/load_path_stack_test.go` | **New** | 1, 9 |
| `environment/resolve.go` | **New** — unified file resolver | 2 |
| `environment/resolve_test.go` | **New** | 2, 9 |
| `environment/top_level_environment.go` | Modify — add field, init, parent delegation accessor | 3 |
| `environment/environment_frame.go` | Modify — add `LoadPathStack()` accessor | 3 |
| `internal/extensions/eval/prim_eval.go` | Modify — update PrimLoad, add new primitives | 4, 7 |
| `internal/extensions/eval/register.go` | Modify — register new primitives | 7 |
| `machine/compile_time_continuation.go` | Modify — rewrite `findFile` to use resolver | 5 |
| `machine/library_loader.go` | Modify — push/pop in `loadLibraryFromFile`, resolver in `LoadLibrary` | 6 |
| `engine.go` | Modify — add `WithLoadPath` + raw load path API | 8 |

## Side effects / fixes

- **Fixes existing bug**: `PrimLoad` currently uses `parser.NewParser()` (no filename). After this change, it uses `parser.NewParserWithFile()`, so expressions loaded via `(load ...)` will have correct source locations in error messages.
- **Fixes error diagnostics**: `findFile` currently returns `(nil, "", nil)` for not-found — indistinguishable from a zero-value success. After this change, not-found always returns an error listing searched paths.
- **Eliminates TOCTOU**: The original plan used `os.Stat` then `os.Open` in `PrimLoad` and `LoadLibrary`. The unified resolver and the rewritten `findFile` eliminate this double-syscall pattern.

## Verification

1. `make test` — all existing tests pass
2. `make lint` — no lint issues
3. New unit tests for `LoadPathStack` pass
4. New unit tests for `ResolveFile` pass
5. Integration test: create temp dir tree, verify relative `load` works
6. REPL test: `(current-load-path)` returns `#f` in interactive mode
7. File test: `(current-load-path)` returns correct path inside loaded file
8. Nested test: stack depth matches nesting; paths resolve correctly at each level
9. Stack depth == 0 after all top-level operations complete

## Notes

*Low-risk, marginal, or deferred items from review. Not blocking implementation.*

### `Pop()` return value

The original plan specified `Pop() string`. No call site inspects the return value (all use `defer stack.Pop()`). The consolidated plan uses `Pop()` (void) for simplicity. If a future debugging need arises for asserting the popped value, the signature can be changed then. Go's `container/heap.Pop` returns a value, but it serves a different purpose (the caller doesn't know what's on top). Here, the caller always knows — they pushed it.

### Stack leak on recovered panic

If a runtime panic occurs between `Push` and the deferred `Pop`, and something recovers the panic higher in the call stack (REPL loop, embedder `recover()`), the stack retains the entry permanently. Pushing after `os.Open` (Phase 4) narrows the window. The depth-zero assertion in tests (Phase 9) will catch this in test scenarios. For production, this is accepted as a rare edge case — a recovered panic in a long-running process is already an unusual state.

### Child `TopLevelEnvironment` stack sharing semantics

Path resolution context is shared between parent and child environments via parent delegation. An `eval` in a child environment resolves relative paths from the parent's current load directory. This is the intended behavior — child environments are binding-isolated but share the load context.

### Per-thread stack keying (future)

If concurrent SRFI-18 `(load ...)` becomes a real use case, the stack can be keyed on `MachineContext.threadID` (`machine/vm_state.go:49`) using `map[uint64][]string`. This is deferred because concurrent `load` is uncommon and most Schemes don't guarantee it.
