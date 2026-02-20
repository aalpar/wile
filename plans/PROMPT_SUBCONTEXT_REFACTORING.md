# Sub-Context Lifecycle Refactoring

## Status: Proposed

## Problem

Eight primitive functions in `registry/core/` follow the same sub-context lifecycle pattern, hand-unrolled 13 times:

```go
sub := mc.NewSubContext()
defer machine.ReleaseSubContext(sub)
sub.SetWindingStack(mc.WindingStack())    // 10 of 13 sites
// optional: sub.SetPromptTag(tag), sub.SetBarrierValid(token), sub.SetEscapeCont(cont)
_, err := sub.Apply(closure, args...)
if err != nil { return err }
err = sub.Run()
// error handling varies
mc.SetValues(sub.GetValues()...)
```

This pattern emerged as each continuation primitive was added independently. The recent escape continuation + barrier work (commits `24df89a`, `27599a7`) added `BarrierValid` propagation to several sites, making the duplication more visible.

## Inventory

| Function | File:Line | Sub-Contexts | WindingStack | Extra Config | Error Handling |
|----------|-----------|:---:|:---:|---|---|
| `PrimApply` | `prim_control.go:76` | 1 | ~~NO~~ yes (fixed) | none | passthrough |
| `PrimCallCC` (sub-ctx mode) | `prim_control.go:152` | 1 | yes | none | catches `ErrPromptAbort{DefaultPromptTag}` |
| `PrimDynamicWind` | `prim_control.go:271-319` | 3 | yes | `SetEscapeCont` on thunk | passthrough (before/after); deferred error (thunk) |
| `PrimCallWithValues` | `prim_control.go:376-404` | 2 | ~~NO~~ yes (fixed) | none | passthrough |
| `PrimCallWithExit` | `prim_exit.go:73` | 1 | yes | none | catches `ErrExitEscape{tag}`, unwinds |
| `PrimCallWithContinuationBarrier` | `prim_barrier.go:59` | 1 | yes | `SetBarrierValid` | passthrough |
| `PrimCallWithContinuationPrompt` | `prim_prompt.go:100` | 2 | yes | `SetPromptTag` | catches `ErrPromptAbort{tag}`, unwinds, runs handler |
| `PrimCallWithComposableContinuation` | `prim_prompt.go:230` | 1 | yes | none | passthrough, then aborts |
| `newComposeAbortEscapeClosure` | `prim_control.go:217` | 1 | yes | none | passthrough, then aborts |

**Total: 13 sub-context create+run sequences across 8 functions.**

## Analysis

### What's the irreducible core?

Every site needs: **create sub-context → configure → apply closure → run → handle result**.

### Are the differences real or accidental?

**Real differences (semantic):**
- Error handling: some catch specific error types (Exit, PromptAbort), others passthrough
- Configuration: PromptTag, BarrierValid, EscapeCont are semantically distinct
- `PrimDynamicWind` uses 3 sub-contexts with interlocking state (push/pop winding frame between them)

**Accidental differences (evolved separately):**
- `PrimApply` and `PrimCallWithValues` omit `SetWindingStack` while all others include it (see Open Question 1)
- `PrimApply` uses `ApplyCallable` (dispatches on value type), others use `Apply` (requires `*MachineClosure`)
- The unwinding check in `PrimCallWithExit` and `PrimCallWithContinuationPrompt` is identical code

### What could be extracted?

**Layer 1: Post-escape unwinding helper** (smallest, highest confidence)

Two sites have identical unwinding logic:

```go
// PrimCallWithExit:83-91 and PrimCallWithContinuationPrompt:118-123
if sub.WindingStack().Depth() > mc.WindingStack().Depth() {
    unwindErr := sub.UnwindTo(mc.WindingStack().Depth())
    if unwindErr != nil {
        return unwindErr
    }
}
```

Extract to a method on `MachineContext`:

```go
// UnwindSubContextToParent unwinds the sub-context's dynamic-wind frames
// down to the parent's depth. Returns nil if no unwinding is needed.
func (mc *MachineContext) UnwindSubContextToParent(sub *MachineContext) error {
    if sub.WindingStack().Depth() > mc.WindingStack().Depth() {
        return sub.UnwindTo(mc.WindingStack().Depth())
    }
    return nil
}
```

Covers 2 sites. Small, safe, no abstraction overhead.

**Layer 2: Simple sub-context runner** (medium scope)

A helper for the "passthrough" pattern where the caller doesn't need the sub-context after Run:

```go
// RunClosureInSubContext creates a sub-context with inherited winding stack,
// applies the closure, runs it, and returns the result values.
// For use when no special error handling or sub-context configuration is needed.
func (mc *MachineContext) RunClosureInSubContext(
    cls *MachineClosure, args ...values.Value,
) ([]values.Value, error) {
    sub := mc.NewSubContext()
    defer ReleaseSubContext(sub)
    sub.SetWindingStack(mc.WindingStack())
    _, err := sub.Apply(cls, args...)
    if err != nil {
        return nil, err
    }
    err = sub.Run()
    if err != nil {
        return nil, err
    }
    return sub.GetValues(), nil
}
```

Covers: `PrimDynamicWind` before/after thunks (2 sites), `PrimCallWithContinuationBarrier` (if we add a variant that accepts config), `PrimCallWithComposableContinuation` (partially — still needs post-run abort).

The catch: sites needing error inspection or sub-context access after Run can't use this. That's 6 of 13 sites.

**Layer 3: Configurable sub-context runner** (largest scope, highest risk)

A more general helper with a config struct:

```go
type SubContextConfig struct {
    InheritWindingStack bool              // default: true
    PromptTag           *PromptTag        // optional
    BarrierValid        *BarrierToken     // optional
    EscapeCont          *MachineContinuation // optional
}

func (mc *MachineContext) RunInSubContext(
    cfg SubContextConfig, cls *MachineClosure, args ...values.Value,
) (sub *MachineContext, err error) {
    sub = mc.NewSubContext()
    if cfg.InheritWindingStack {
        sub.SetWindingStack(mc.WindingStack())
    }
    if cfg.PromptTag != nil { sub.SetPromptTag(cfg.PromptTag) }
    if cfg.BarrierValid != nil { sub.SetBarrierValid(cfg.BarrierValid) }
    if cfg.EscapeCont != nil { sub.SetEscapeCont(cfg.EscapeCont) }
    _, err = sub.Apply(cls, args...)
    if err != nil { return sub, err }
    err = sub.Run()
    return sub, err
}
```

Problem: caller must `defer ReleaseSubContext(sub)` but `sub` comes from the return value. This requires:
```go
sub, err := mc.RunInSubContext(cfg, cls, args...)
defer machine.ReleaseSubContext(sub) // must be non-nil even on error
```
This works if RunInSubContext always returns a non-nil sub (which it does — sub is created first). But it's a subtle contract.

Covers most sites, but adds a struct type and makes the simple cases slightly more verbose.

## Recommendation

**Do Layer 1 only.** Extract the unwinding helper. It's a concrete win (eliminates duplicated unwinding logic) with zero abstraction cost.

Layers 2 and 3 trade readability for DRY. Each primitive is currently self-contained and readable. The variation in error handling means most callers would still have significant post-helper logic. The 4-line create+apply+run sequence is not a maintenance burden — it's the *error handling* that's complex and unique per site.

If future primitives add more sub-context sites, revisit Layer 2.

## Open Questions

### 1. Missing `SetWindingStack` in `PrimApply` and `PrimCallWithValues` — CONFIRMED BUG, FIXED

`PrimApply` and `PrimCallWithValues` were the only sub-context sites that didn't call `sub.SetWindingStack(mc.WindingStack())`.

**Bug mechanism:** When `call/cc` captures a continuation inside `apply`'s sub-context (inline mode, `mc.Parent() != nil`), it copies `mc.WindingStack()`. Since `mc` is `apply`'s sub-context with an empty winding stack, the captured continuation has an empty winding stack. On re-invocation, `RestoreWithWindingFrom` sees empty target → no before/after thunks fire.

**Escape propagation is NOT affected:** Simple escapes (ErrExitEscape, ErrPromptAbort) propagate through apply's sub-context to the parent, which has the correct winding stack and handles unwinding. The bug only manifests when **continuations are captured** inside the sub-context.

**Test:** `TestApplyWindingStackInheritance` in `prim_apply_test.go` — captures `call/cc` inside `apply` (and `call-with-values`) within `dynamic-wind`, re-invokes via prompt, checks that before thunk fires on re-entry. Baseline (direct call) passes, apply/call-with-values cases fail without fix, pass with fix.

**Fix:** Added `sub.SetWindingStack(mc.WindingStack())` to:
- `PrimApply` (`prim_control.go:78`)
- `PrimCallWithValues` producer sub-context (`prim_control.go:379`)
- `PrimCallWithValues` consumer sub-context (`prim_control.go:395`)

**Remaining:** `applyParameter` in `machine/machine_context.go:454` has the same missing inheritance for its converter sub-context. Low priority — parameter converters rarely capture continuations.

### 2. `ApplyCallable` vs `Apply`

`PrimApply` and `newComposeAbortEscapeClosure` use `ApplyCallable` (dispatches on value type — handles closures, case-lambda, parameters, composable continuations). All others use `Apply` (requires `*MachineClosure`).

This is a real semantic difference: `apply` must handle any callable, while the other primitives have already type-checked their argument. No unification needed.

## Completed

### Winding stack inheritance bug fix

| File | Change | Status |
|------|--------|--------|
| `registry/core/prim_control.go:78` | Add `sub.SetWindingStack(mc.WindingStack())` to `PrimApply` | Done |
| `registry/core/prim_control.go:379,395` | Add `sub.SetWindingStack(mc.WindingStack())` to both sub-contexts in `PrimCallWithValues` | Done |
| `registry/core/prim_apply_test.go` | Add `TestApplyWindingStackInheritance` (3 cases: baseline, apply, call-with-values) | Done |
| `machine/machine_context.go:455` | Add `sub.SetWindingStack(p.WindingStack())` to `applyParameter` converter sub-context | Done |

## Remaining (Layer 1 refactoring)

| File | Change |
|------|--------|
| `machine/machine_context.go` | Add `UnwindSubContextToParent` method |
| `registry/core/prim_exit.go:87-91` | Replace inline unwinding with `mc.UnwindSubContextToParent(sub)` |
| `registry/core/prim_prompt.go:118-123` | Replace inline unwinding with `mc.UnwindSubContextToParent(sub)` |
