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
`pkg/machine/captured_continuation.go`), which wraps the `ComposableContinuation`
rather than building a Go closure directly. When invoked,
`applyCapturedContinuation` does not run the captured chain: it checks thread
and barrier identity and then *returns* an `ErrResumeContinuation` carrying the
segment **unrun**, the resume values, and a copy of the winding stack live at
the `(k v)` site. That control signal rides the VM's ordinary `return err`
plumbing to the nearest `DefaultPromptTag` driver (`RunResumable`), which
reinstalls the segment on its own live chain via `reinstallSegment` and keeps
looping. Resuming therefore costs O(1) Go frames and reconciles `dynamic-wind`
exactly once. See [`resume-trampoline.md`](resume-trampoline.md) for the full
mechanism and the bugs it fixed.

## Design Rationale

### Why control signals instead of a payload carrier?

The previous design used a `continuationEscapePayload` carrier tunneled
through `ErrPromptAbort`, with a dedicated `HandleContinuationEscapeAbort`
function to detect and process escape payloads. This required:
- A carrier type implementing `values.Value` (solely for transport)
- Special-case detection in two places (PrimCallCC sub-context + RunWithEscapeHandling; both since removed)
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

`reinstallSegment` performs the *single* winding reconcile, calling
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

### Where a continuation is delimited, and who resumes it

The continuation is captured **once** and delimited at
`FindPrompt(DefaultPromptTag)`, not unconditionally at the whole chain. Inside a
`call-with-continuation-prompt` reusing the default tag that lookup yields the
prompt's chain frame, so only the delimited segment is captured. With no such
frame it yields nil and the whole chain is sliced: the continuation is delimited
at the capturing context's **boundary**, and `DelimitAt` stamps the
context's boundary token on it; in every case it also records the root of the
context chain (`rootIdentity`: the nearest `RunResumable` owner, else the
parentless context). Every context has a boundary: the top level, a thread
root, each sub-context (a `dynamic-wind` thunk run by a winding reconcile,
`eval`, `load`), and the expansion-time roots (a transformer body, a library
body, the bootstrap), which have no parent but may have a runtime driver above
them on the Go stack.

The lambda then runs directly in the current VM context via `mc.ApplyCallable()`,
preserving the full continuation chain, critical for cooperative coroutines and
patterns that capture/invoke multiple continuations. No PC compensation is
needed: `applyForeign` does not post-increment `pc`. With no chain above
(`cont == nil`: a tail call at the root of a context) the lambda's normal return
ends the context's run, as the thunk's own would have.

A resume is resolved by the driver that owns where the continuation was
delimited (`resumePrompt`): a default-tag frame on the driver's own chain, or
the driver's own boundary when the tokens match. `RunWithinBoundary` re-raises
every other resume to the enclosing driver, so a sub-context's edge never acts
as a prompt the program did not write; `RunResumable`, a root, resolves whatever
reaches it. The consequence that motivated the rule: a continuation captured at
the top level and invoked from an escape's after thunk replaces the whole
program's continuation, escape in flight included, instead of running the rest
of the program on the thunk's sub-context and then again on the driver that was
mid-escape. Until 2026-10 a `call/cc` with no chain above it applied the lambda
on a fresh sub-context under a `RunResumable` of its own, whose edge was exactly
such a prompt.

`PrimCallWithComposableContinuation` applies `proc` in place the same way.

## Code Locations

| Component | File |
|-----------|------|
| `PrimCallCC` | `pkg/registry/core/prim_control.go` |
| `NewCapturedContinuation`, `applyCapturedContinuation`, `CapturedContinuation` | `pkg/machine/captured_continuation.go` |
| `ComposableContinuation`, `AcquireSegment` | `pkg/machine/composable_continuation.go` |
| `ErrResumeContinuation`, `ErrPromptAbort` | `pkg/machine/prompt_abort.go` |
| `BarrierToken` | `pkg/machine/barrier_token.go` |
| `reinstallSegment`, `applyComposableContinuation` | `pkg/machine/machine_context_apply.go` |
| `RunResumable`, `RunWithEscapeHandling`, `resolveAbort` | `pkg/machine/machine_context.go` |
| `RestoreWithWindingFrom` | `pkg/machine/machine_context_winding.go` |

For operational details (error propagation paths, driver pseudocode, end-to-end
examples), see [`prompt-abort.md`](prompt-abort.md). For the resume mechanism
itself, see [`resume-trampoline.md`](resume-trampoline.md).
