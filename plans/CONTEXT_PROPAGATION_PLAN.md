# Context Propagation Plan

**Status:** PLANNED — Audit complete, implementation not started

## Problem

The VM loop in `MachineContext.Run()` checks `ctx.Done()` per iteration, but sub-loops called from operations — macro expansion, pattern matching, compilation, library loading, primitive list iteration — fabricate their own `context.TODO()` or `context.Background()`. A long-running sub-loop (pathological macro, large pattern match, deep include chain) is invisible to cancellation until control returns to `Run()`.

## Design Principle

Every `for` loop in the MachineContext call graph must have access to exactly **one** `context.Context` traceable back to `MachineContext.ctx`. No fabricated contexts (`context.TODO()`, `context.Background()`), no duplicate contexts (parameter + receiver both carrying a context).

**Exception:** SRFI-18 threads create clean breaks — each thread gets its own `MachineContext` with its own `context.Context`.

## Audit Summary

67 `context.TODO()` / `context.Background()` call sites in production code (excluding tests). Grouped into 9 categories by structural cause.

### Category 1: `ExpanderTimeContinuation` — No Context Field

`ExpanderTimeContinuation` holds only `env`. Every method fabricates `context.TODO()`.

| File:Line | Call |
|-----------|------|
| `expander_time_continuation.go:355` | `CompileSyntaxRules(context.TODO(), ...)` |
| `expander_time_continuation.go:743` | `ctx := context.TODO()` in `expandImportForm` |
| `expander_time_continuation.go:1066` | `compileTransformerToMachineClosure(context.TODO(), ...)` |
| `expander_time_continuation.go:1283` | `NewMachineContextFromMachineClosure(context.TODO(), ...)` |
| `expander_time_continuation.go:1387` | `NewMachineContextFromMachineClosure(context.TODO(), ...)` |
| `expander_time_continuation.go:1439` | `SyntaxForEach(context.TODO(), ...)` |

Lines 1283 and 1387 create sub-VMs for macro transformers that run `mc.Run()` with no cancellation.

### Category 2: `CompileTimeContinuation` — No Context Field

`CompileTimeContinuation` holds only `env` and `template`.

| File:Line | Call |
|-----------|------|
| `compile_time_continuation.go:247` | `fileParser.ReadSyntax(context.TODO())` — include loop |
| `compile_time_continuation.go:780` | `SyntaxForEach(context.TODO(), ...)` |
| `compile_time_continuation.go:1094` | `validate.ValidateExpression(context.TODO(), ...)` |
| `compile_time_continuation.go:1320` | `SyntaxForEach(context.TODO(), ...)` |
| `compile_time_continuation.go:1634` | `SyntaxForEach(context.TODO(), ...)` |
| `compile_time_continuation.go:1750` | `SyntaxForEach(context.TODO(), ...)` |
| `compile_time_continuation.go:1772` | `SyntaxForEach(context.TODO(), ...)` |
| `compile_time_continuation.go:2222` | `SyntaxForEach(context.TODO(), ...)` |

### Category 3: Compile-Time VM Execution — `context.Background()`

These create `MachineContext` instances that can never be cancelled:

| File:Line | Call |
|-----------|------|
| `compile_define_for_syntax.go:118` | `NewMachineContext(context.Background(), cont)` |
| `compile_begin_for_syntax.go:81` | `NewMachineContext(context.Background(), cont)` |
| `compile_time_call_context.go:71` | `ctx: context.Background()` — default field value |

### Category 4: Compile-Time Helpers — `context.TODO()`

| File:Line | Call |
|-----------|------|
| `compile_eval_when.go:62,122,163,223` | 4 x `context.TODO()` |
| `compile_begin_for_syntax.go:63` | `SyntaxForEach(context.TODO(), ...)` |
| `compile_syntax_rules.go:558` | `SyntaxForEach(context.TODO(), ...)` |
| `compile_syntax_case.go:97` | `SyntaxForEach(context.TODO(), ...)` |
| `compile_quasisyntax.go:257` | `SyntaxForEach(context.TODO(), ...)` |

### Category 5: Library Loader — Bug

| File:Line | Call | Note |
|-----------|------|------|
| `library_loader.go:135` | `p.ReadSyntax(context.TODO())` | Has `ctx` parameter — just not using it |

### Category 6: Import Set Parsing — No Context Parameter

| File:Line | Call |
|-----------|------|
| `import_set_datum.go:33` | `pair.ForEach(context.TODO(), ...)` |
| `import_set_datum.go:203` | `renamesPair.ForEach(context.TODO(), ...)` |
| `import_set_datum.go:315` | `pair.ForEach(context.TODO(), ...)` |

### Category 7: Primitives — Context Available But Not Used

Foreign functions receive `ctx context.Context` as their first parameter but pass `context.TODO()` to `ForEach` and `ReadSyntax`:

| File:Line | Call |
|-----------|------|
| `prim_byte_vectors.go:73,289` | `ForEach(context.TODO(), ...)` |
| `prim_strings.go:216,274` | `ForEach(context.TODO(), ...)` |
| `prim_lists.go:103,140,173,200,499` | `ForEach(context.TODO(), ...)` |
| `prim_control.go:63` | `ForEach(context.TODO(), ...)` |
| `prim_eval.go:109` | `ReadSyntax(context.TODO())` |
| `prim_exceptions.go:277` | `ForEach(context.TODO(), ...)` |
| `prim_read_write.go:122,216` | `ReadSyntax(context.TODO())` |
| `prim_all.go:192` | `ForEach(context.TODO(), ...)` |

### Category 8: `internal/match` — No Context Access

The `Matcher` struct has no context field or parameter. Key loops that could run long:

| File:Line | Loop | Risk |
|-----------|------|------|
| `match.go:201` | `for len(p.syntaxStack) > 0` — main match | High |
| `match.go:350` | `for len(p.syntaxStack) > 0` — ellipsis | High |
| `match.go:518` | `for len(p.valueStack) > 0` — value match | High |
| `syntax_compiler.go:141` | `for len(stack) > 0` — compilation | Medium |
| `syntax_compiler.go:317` | `for { ... }` — unbounded | Medium |

### Category 9: `Operation` Interface — Duplication

The `Operation` interface requires `Apply(ctx context.Context, mc *MachineContext)`. Since `mc` already carries `ctx`, every operation has two references to the same context.

## Implementation Phases

### Phase 1: Primitives (Low Risk, High Coverage)

Replace `context.TODO()` with the `ctx` parameter that foreign functions already receive. Pure mechanical substitution — no API changes.

**Files:**
- `registry/core/prim_byte_vectors.go` (2 sites)
- `registry/core/prim_strings.go` (2 sites)
- `registry/core/prim_lists.go` (5 sites)
- `registry/core/prim_control.go` (1 site)
- `internal/extensions/eval/prim_eval.go` (1 site)
- `internal/extensions/exceptions/prim_exceptions.go` (1 site)
- `internal/extensions/io/prim_read_write.go` (2 sites)
- `internal/extensions/all/prim_all.go` (1 site)

**Total:** ~15 `context.TODO()` -> `ctx` replacements. No signature changes.

### Phase 2: Library Loader Bug Fix (Trivial)

`library_loader.go:135` — change `context.TODO()` to `ctx` (parameter already exists).

**Total:** 1 line change.

### Phase 3: `CompileTimeContinuation` Context Field

Add `ctx context.Context` field to `CompileTimeContinuation`. Thread from callers.

**Struct change:**
```go
type CompileTimeContinuation struct {
    ctx      context.Context  // new
    env      *environment.EnvironmentFrame
    template *NativeTemplate
    libraryCallback func(*CompiledLibrary)
}
```

**Constructor change:** `NewCompiletimeContinuation` gains a `ctx` parameter. All callers updated.

**Internal changes:** Replace all `context.TODO()` in `compile_time_continuation.go` methods with `p.ctx`.

**Downstream:** `compile_define_for_syntax.go`, `compile_begin_for_syntax.go`, `compile_eval_when.go`, `compile_syntax_rules.go`, `compile_syntax_case.go`, `compile_quasisyntax.go` — replace `context.TODO()` / `context.Background()` with `p.ctx` (where `p` is the `CompileTimeContinuation`).

**`CompileTimeCallContext`:** Change default `ctx` from `context.Background()` to require explicit context.

**Callers to update:**
- `engine.go: compileExpr()` — pass `context.Background()` (Engine has no context at compile time, but this is the entry point — the user's `Eval(ctx, ...)` context should thread here eventually)
- `compile_transformer.go` — already receives `ctx`, pass through
- `library_loader.go` — already receives `ctx`, pass through
- `expander_time_continuation.go` — depends on Phase 4

**Total:** ~15 sites in compile_*.go files, plus constructor call site updates.

### Phase 4: `ExpanderTimeContinuation` Context Field

Add `ctx context.Context` field to `ExpanderTimeContinuation`. Thread from callers.

**Struct change:**
```go
type ExpanderTimeContinuation struct {
    ctx context.Context  // new
    env *environment.EnvironmentFrame
}
```

**Constructor change:** `NewExpanderTimeContinuation` gains a `ctx` parameter. All callers updated.

**Internal changes:** Replace all `context.TODO()` in expander methods with `p.ctx`. The critical ones are lines 1283 and 1387 where `NewMachineContextFromMachineClosure(context.TODO(), ...)` creates sub-VMs — these become `NewMachineContextFromMachineClosure(p.ctx, ...)`.

**Callers to update:**
- `engine.go: compileExpr()` — thread context
- `compile_transformer.go` — thread context
- `library_loader.go` — thread context
- Internal recursive calls within expander

**Total:** ~6 sites in `expander_time_continuation.go`, plus constructor call site updates.

### Phase 5: Import Set Parsing

Add `ctx context.Context` parameter to `parseImportSet`, `parseImportSetHelper`, and `parseLibraryName`.

**Files:**
- `import_set_datum.go` (3 sites, function signature changes)

**Total:** 3 function signatures gain `ctx` parameter.

### Phase 6: `internal/match` Context Support

Add `ctx context.Context` to `Matcher` struct. Check `ctx.Done()` in the three main match loops.

**Performance consideration:** Checking `ctx.Done()` on every loop iteration adds a channel receive. Use a counter to check every N iterations (e.g., every 1024):

```go
// In the main match loop:
iterations := 0
for len(p.syntaxStack) > 0 {
    iterations++
    if iterations&0x3FF == 0 {
        select {
        case <-p.ctx.Done():
            return p.ctx.Err()
        default:
        }
    }
    // ... rest of loop
}
```

**Struct change:**
```go
type Matcher struct {
    ctx          context.Context  // new
    variables    map[string]struct{}
    codes        []SyntaxCommand
    // ...
}
```

**Constructor changes:** All `NewMatcher*` functions gain `ctx` parameter.

**Callers:** `OperationSyntaxRulesTransform` (has `mc.ctx` available), `OperationSyntaxCase` (same).

**Total:** 3-5 function signatures, 3 loop modifications, caller updates.

### Phase 7: `Operation` Interface Deduplication (Optional)

**Option A — Remove `ctx` from interface:**
```go
// Before:
type Operation interface {
    Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error)
}
// After:
type Operation interface {
    Apply(mc *MachineContext) (*MachineContext, error)
}
```
Operations use `mc.Context()`. This is clean but touches every operation file (~30 files).

**Option B — Keep both, document invariant:**
Add a comment to the `Operation` interface documenting that `ctx` must always be `mc.Context()`. No code changes. Relies on discipline.

**Recommendation:** Option A is correct but high-churn. Defer until the other phases land, then evaluate whether the churn is worth it as a separate cleanup.

### Phase 8: Engine Entry Point

Thread the user's `ctx` from `Engine.Eval(ctx, code)` through compilation, not just execution.

Currently `Engine.Compile()` uses `context.Background()` internally. After Phases 3-4, it should accept and thread a context:

```go
func (p *Engine) Compile(ctx context.Context, code string) (*CompiledCode, error)
```

This is a **public API change**. The current `Compile(code string)` signature would need to change or a `CompileWithContext` variant added.

**Recommendation:** Add context to Compile. The current signature is a convenience that can be preserved as `Compile(code) = CompileContext(context.Background(), code)` if backward compatibility matters.

## Dependency Order

```
Phase 1 (primitives) ─── independent, no prerequisites
Phase 2 (library loader bug) ─── independent
Phase 3 (CompileTimeContinuation) ─── prerequisite for Phase 4
Phase 4 (ExpanderTimeContinuation) ─── depends on Phase 3
Phase 5 (import_set_datum) ─── independent
Phase 6 (internal/match) ─── independent
Phase 7 (Operation interface) ─── after all others
Phase 8 (Engine API) ─── after Phases 3-4
```

Phases 1, 2, 5, and 6 can proceed in parallel. Phase 3 must precede Phase 4 (expander calls compiler). Phase 7 is optional cleanup. Phase 8 is the final public API change.

## Testing Strategy

Each phase should verify:
1. All existing tests pass (no behavioral change)
2. Context cancellation works at the new check points — add targeted tests:
   - Cancel during macro expansion (Phase 4)
   - Cancel during pattern matching (Phase 6)
   - Cancel during compilation (Phase 3)
   - Cancel during library loading (Phase 2)
3. No performance regression on benchmarks (Phase 6 counter-based checking)

## Risk Assessment

| Phase | Risk | Rationale |
|-------|------|-----------|
| 1 | Low | Mechanical substitution, no API changes |
| 2 | Trivial | 1 line bug fix |
| 3 | Medium | Struct change propagates to all compiler callers |
| 4 | Medium | Struct change propagates to all expander callers |
| 5 | Low | Small scope, bounded iteration |
| 6 | Medium | Performance-sensitive hot path, needs benchmarking |
| 7 | High churn | ~30 files, interface change, but mechanically simple |
| 8 | Medium | Public API change |
