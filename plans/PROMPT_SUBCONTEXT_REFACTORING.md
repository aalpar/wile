# Sub-Context Lifecycle Refactoring

## Status: Layer 1 remaining (low priority)

## Problem

Eight primitive functions in `registry/core/` follow the same sub-context lifecycle pattern, hand-unrolled 13 times. Two sites share identical unwinding logic that could be extracted.

## Inventory

| Function | File:Line | Sub-Contexts | WindingStack | Extra Config | Error Handling |
|----------|-----------|:---:|:---:|---|---|
| `PrimApply` | `prim_control.go:76` | 1 | yes | none | passthrough |
| `PrimCallCC` (sub-ctx mode) | `prim_control.go:152` | 1 | yes | none | catches `ErrPromptAbort{DefaultPromptTag}` |
| `PrimDynamicWind` | `prim_control.go:271-319` | 3 | yes | `SetEscapeCont` on thunk | passthrough (before/after); deferred error (thunk) |
| `PrimCallWithValues` | `prim_control.go:376-404` | 2 | yes | none | passthrough |
| `PrimCallWithExit` | `prim_exit.go:73` | 1 | yes | none | catches `ErrExitEscape{tag}`, unwinds |
| `PrimCallWithContinuationBarrier` | `prim_barrier.go:59` | 1 | yes | `SetBarrierValid` | passthrough |
| `PrimCallWithContinuationPrompt` | `prim_prompt.go:100` | 2 | yes | `SetPromptTag` | catches `ErrPromptAbort{tag}`, unwinds, runs handler |
| `PrimCallWithComposableContinuation` | `prim_prompt.go:230` | 1 | yes | none | passthrough, then aborts |
| `newComposeAbortEscapeClosure` | `prim_control.go:217` | 1 | yes | none | passthrough, then aborts |

**Total: 13 sub-context create+run sequences across 8 functions.**

## Remaining: Layer 1 — Post-escape unwinding helper

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
func (mc *MachineContext) UnwindSubContextToParent(sub *MachineContext) error {
    if sub.WindingStack().Depth() > mc.WindingStack().Depth() {
        return sub.UnwindTo(mc.WindingStack().Depth())
    }
    return nil
}
```

| File | Change |
|------|--------|
| `machine/machine_context.go` | Add `UnwindSubContextToParent` method |
| `registry/core/prim_exit.go:87-91` | Replace inline unwinding with `mc.UnwindSubContextToParent(sub)` |
| `registry/core/prim_prompt.go:118-123` | Replace inline unwinding with `mc.UnwindSubContextToParent(sub)` |
