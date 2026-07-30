# Continuation Escape Design

## Summary

Call/cc follows Racket's model, where `call/cc` is defined in terms of a
composable capture delimited at the default prompt plus an abort to that prompt:

```scheme
(call/cc f) ≡
  (call-with-composable-continuation
    (lambda (k)
      (f (lambda (v) (abort-current-continuation default-prompt-tag (k v)))))
    default-prompt-tag)
```

That is the *semantics*. The *mechanism* is the resume trampoline. Call/cc
returns a `CapturedContinuation` value (defined in
`machine/captured_continuation.go`), which wraps the `ComposableContinuation`
rather than building a Go closure directly. When invoked,
`applyCapturedContinuation` does not run the captured chain: it checks thread
and barrier identity and then *returns* an `ErrResumeContinuation` carrying the
segment **unrun**, the resume values, and a copy of the winding stack live at
the `(k v)` site. That control signal rides the VM's ordinary `return err`
plumbing to the nearest `DefaultPromptTag` driver (`RunResumable`), which
reinstalls the segment on its own live chain via `ReinstallSegment` and keeps
looping. Resuming therefore costs O(1) Go frames and reconciles `dynamic-wind`
exactly once. See [`resume-trampoline.md`](resume-trampoline.md) for the full
mechanism and the bugs it fixed.

## Design Rationale

### Why one control signal instead of a payload carrier?

The previous design used a `continuationEscapePayload` carrier tunneled
through `ErrPromptAbort`, with a dedicated `HandleContinuationEscapeAbort`
function to detect and process escape payloads. This required:
- A carrier type implementing `values.Value` (solely for transport)
- Special-case detection in two places (PrimCallCC sub-context + RunWithEscapeHandling)
- A `pendingEscape` field for nested escape scenarios
- `escapeCont` tracking for sub-context chain breaks

Routing the escape through `ErrPromptAbort` eliminated all of this:
- No special carrier type, no detection logic, no pending escape mechanism
- The driver handles every abort uniformly via `FindPrompt`

An intermediate form of that design still had the escape closure *run* the
captured chain in a fresh sub-context and then abort with the result. That was
superseded in turn by the trampoline, because running the chain on the spot
nests one Go frame per resume (the `ctak` stack overflow under `-race`) and
reconciles `dynamic-wind` twice on an escape out of an extent.

### Dynamic-wind integration

`ReinstallSegment` performs the *single* winding reconcile, calling
`RestoreWithWindingFrom` with the `SourceWinding` carried on the resume signal:
the winding live at the `(k v)` site, which may be a deeper sub-context than the
driver's. That is what makes after thunks fire exactly once and makes a deeper
sub-context's after thunks fire at all. Marks are installed before the reconcile,
because before/after thunks are arbitrary Scheme that may read parameters.

### Thread and barrier checks

Both checks happen in `applyCapturedContinuation` (and its composable twin
`applyComposableContinuation`) at the point the continuation is invoked, before
the resume signal is built and before any continuation manipulation:

1. **Thread check**: Compares capture-time thread ID with invocation-time
   thread ID. Prevents cross-thread continuation invocation that would
   corrupt VM state (per SRFI-18 semantics).

2. **Barrier check**: Compares capture-time `*BarrierToken` pointer with
   invocation-time pointer. Pointer inequality means the continuation
   would cross a `with-continuation-barrier` boundary. `BarrierToken` is
   an opaque identity type — only pointer identity matters.

### Two execution modes in PrimCallCC

The continuation is captured **once**, before the mode is chosen, and delimited
at `FindPrompt(DefaultPromptTag)`, not unconditionally at the whole chain. At
the top-level context boundary that lookup yields nil and the whole chain is
sliced; inside a `call-with-continuation-prompt` reusing the default tag it
yields that prompt's chain frame, so only the delimited segment is captured. The
two modes then differ in one thing only: which context the lambda is applied in,
i.e. driver provenance.

**Inline mode** (`mc.Parent() != nil`): The lambda runs directly in the
current VM context via `mc.ApplyCallable()`. This preserves the full continuation
chain, critical for cooperative coroutines and patterns that capture/invoke
multiple continuations. Resume is resolved by the ambient `DefaultPromptTag`
driver already running above this frame. No PC compensation is needed:
`applyForeign` does not post-increment `pc`.

**Sub-context mode** (`mc.Parent() == nil`): call/cc is rootless (inside another
foreign function's sub-context, or at a thread root), so there is no ambient
driver. The lambda is applied in a fresh sub-context which then runs
`RunWithEscapeHandling` (installing its own `DefaultPromptTag` and resolving
this call/cc's resume signal) before its value(s) are delivered to `mc`. This
ensures call/cc works in contexts that would otherwise call `Run()` directly.

`PrimCallWithComposableContinuation` mirrors the same single-seam shape: capture,
then select `mc` (proc runs in place, composing) or a fresh sub-context when
rootless.

## Code Locations

| Component | File |
|-----------|------|
| `PrimCallCC` | `registry/core/prim_control.go` |
| `NewCapturedContinuation`, `applyCapturedContinuation`, `CapturedContinuation` | `machine/captured_continuation.go` |
| `ComposableContinuation`, `AcquireSegment` | `machine/composable_continuation.go` |
| `ErrResumeContinuation`, `ErrPromptAbort` | `machine/prompt_abort.go` |
| `BarrierToken` | `machine/barrier_token.go` |
| `ReinstallSegment`, `applyComposableContinuation` | `machine/machine_context_apply.go` |
| `RunResumable`, `RunWithEscapeHandling`, `resolveAbort` | `machine/machine_context.go` |
| `RestoreWithWindingFrom` | `machine/machine_context_winding.go` |

For operational details (error propagation paths, driver pseudocode, end-to-end
examples), see [`prompt-abort.md`](prompt-abort.md). For the resume mechanism
itself, see [`resume-trampoline.md`](resume-trampoline.md).
