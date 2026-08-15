# Prompt/Abort System — Implementation Details

This document describes the implementation of the prompt/abort system,
including error propagation, handler dispatch, dynamic-wind integration,
and the unification of `call/cc` escapes with delimited continuations.

For design rationale, see [`delimited.md`](delimited.md) and
[`escape-design.md`](escape-design.md).

## Error propagation path

Continuation control flow travels as one of two Go error values, both of which
are VM *control signals* rather than failures:

- `ErrPromptAbort`: carries a `*PromptTag`, a `[]values.Value` payload, and the
  `SourceWinding` live at the escape point. Emitted by
  `abort-current-continuation`, by the `call-with-exit` exit closure, and by
  `shift`/`control` (which abort on top of a raw composable capture).
- `ErrResumeContinuation`: carries a `*PromptTag`, the captured segment
  **unrun**, the resume values, and `SourceWinding`. Emitted only by invoking a
  `call/cc` continuation. See [`resume-trampoline.md`](resume-trampoline.md).

Both are declared in `machine/prompt_abort.go`.

```
      ┌──────────────────────────────────────────────────────────────┐
      │ Source                                                       │
      │  • PrimAbortCurrentContinuation   (user tag)                 │
      │  • call-with-exit exit closure    (private tag)              │
      │      → ErrPromptAbort                                        │
      │  • applyCapturedContinuation      (DefaultPromptTag)         │
      │      → ErrResumeContinuation                                 │
      └───────────────────────────────┬──────────────────────────────┘
                                      │
      ┌──────────────────────────────────────────────────────────────┐
      │ applyCallableError (machine/foreign_closure.go), reached via │
      │ bridgeForeignError from applyForeign, callForeignCached,     │
      │ the promoted-op call sites and OperationPushWind             │
      │                                                              │
      │ errors.As matches a control signal?                          │
      │   YES → return it unchanged                                  │
      │   NO  → RaiseInPlace as a Scheme condition                   │
      └───────────────────────────────┬──────────────────────────────┘
                                      │
      ┌──────────────────────────────────────────────────────────────┐
      │ Run() returns the signal to its driver                       │
      └───────────────────────────────┬──────────────────────────────┘
                                      │
                    ┌─────────────────┴─────────────────┐
                    ▼                                   ▼
   ┌────────────────────────────────┐  ┌────────────────────────────────┐
   │ RunWithinBoundary              │  │ RunResumable                   │
   │ (surviving sub-context)        │  │ (the DefaultPromptTag driver;  │
   │                                │  │ RunWithEscapeHandling          │
   │ FindPrompt on OWN chain        │  │ delegates to it)               │
   │ found → resolveAbort           │  │                                │
   │ not found → re-raise           │  │ abort  → resolveAbort          │
   │ resume signals always          │  │ resume → ReinstallSegment      │
   │   re-raise                     │  │ timer  → resolveTimerInterrupt │
   └────────────────────────────────┘  └────────────────────────────────┘
```

### Error priority in applyCallableError

`applyCallableError` (`machine/foreign_closure.go`) has a strict order:

```go
// 1. ErrPromptAbort          (errors.As) → pass through unchanged
// 2. ErrExceptionEscape      (errors.As) → pass through unchanged
// 3. ErrTimerInterrupt       (errors.As) → pass through unchanged
// 4. ErrResumeContinuation   (errors.As) → pass through unchanged
// 5. any other Go error → RaiseInPlace(ConditionFromError(err))
```

No live call site recovers panics around this function. The one that did,
`OperationForeignFunctionCall.Apply`, was deleted in 2026-08 as unreachable
code; a control signal that escapes as a *panic* is therefore recognized only
at the VM boundary (`RunResumable`), which contains it as an uncatchable
`*SchemeError` rather than re-classifying it. Control signals travel as
returned errors, not as panics.

The control-signal checks must precede the fallthrough: without them, a prompt
abort or a resume would be converted into a catchable Scheme condition and never
reach its driver.

Note: `call-with-exit` uses `ErrPromptAbort` with a private `PromptTag` created
per invocation. It is no longer caught by a Go-stack `errors.As` in
`PrimCallWithExit`; the boundary is a reified continuation frame
(`RunBodyUnderExitFrame`) that the driver's `FindPrompt` routes to.

## RunResumable

`machine/machine_context.go`. `RunWithEscapeHandling` is a one-line delegation
to it, kept as the name embedders and thread roots call.

This is the driver loop under the default prompt. It installs `DefaultPromptTag`
on the context (so `call/cc` resumes have a target), recovers panics in a
`defer`, then enters a `for` loop that repeatedly calls `Run()` and dispatches
on what comes back. There is no special-case detection for call/cc: an abort is
routed by `FindPrompt`, a resume by `ReinstallSegment`.

```
┌─ RunResumable ─────────────────────────────────────────────────────┐
│                                                                    │
│  p.promptTag = DefaultPromptTag                                    │
│                                                                    │
│  loop:                                                             │
│    err := pending, else p.Run()  (pending = a routed signal)       │
│    │                                                               │
│    ├─ err == nil (normal completion)                               │
│    │   ├─ UnwindTo(0) if winding frames remain                     │
│    │   └─ return nil                                               │
│    │                                                               │
│    ├─ ErrPromptAbort                                               │
│    │   FindPrompt(tag); not found → error "no prompt found"        │
│    │   resolveAbort(abortErr, prompt)                              │
│    │   control signal? → pending; continue loop                    │
│    │   done? return nil  (context-level deliver)                   │
│    │   continue loop                                               │
│    │                                                               │
│    ├─ ErrResumeContinuation  (the trampoline bounce)               │
│    │   boundary := FindPrompt(tag)                                 │
│    │   ReinstallSegment(seg, bnd, srcWinding, vals, true, arms)    │
│    │   control signal? → pending; continue loop                    │
│    │   wasEmpty? boundary == nil → return nil; else Restore(bnd)   │
│    │   continue loop                                               │
│    │                                                               │
│    ├─ ErrTimerInterrupt → resolveTimerInterrupt; continue loop     │
│    │                                                               │
│    └─ other error → return err                                     │
└────────────────────────────────────────────────────────────────────┘
```

### resolveAbort

`resolveAbort` is the shared abort arm of `RunResumable` and
`RunWithinBoundary`. Given an already-matched prompt it:

1. reconciles dynamic-wind from `abortErr.SourceWinding` (the winding at the
   escape point, possibly a deeper sub-context) — or, when the abort carries no
   `SourceWinding`, from the driver's own winding (value-delivery aborts) — to
   `prompt.windingStack`;
2. `Restore(prompt)`: restores *past* the prompt frame, skipping it;
3. applies the prompt's handler to the abort values, or, when there is no
   handler, delivers all of them with `SetValues` (R7RS §6.10: zero values stay
   zero values, not a fabricated Void).

The two drivers differ only in how they treat a *not-found* prompt: the
top-level `RunResumable` raises, while `RunWithinBoundary` re-raises the abort
so its enclosing driver owns it.

**Step 1 runs after-thunks, and an after-thunk can transfer control.** A `raise`
there caught by a `guard` placed *around* the `call/cc` escapes via
`call-with-exit`; so does a bare `call-with-exit` with no exception anywhere.
Both surface as `resolveAbort`'s (and, on the resume arm,
`ReinstallSegment`'s) error return, and both are addressed to a prompt still on
this chain. The driver therefore classifies that error with `isControlSignal`
and feeds it back to the top of its own loop as `pending` rather than returning
it — the two reconcile paths are the only places a control signal is produced
*by* the driver rather than delivered *to* it. Returning it instead is what put
a raw `abort to prompt #<continuation-prompt-tag:exit>` in front of the
embedder. Classification is by control-signal-ness, not by exception
provenance: the no-exception shape leaks identically.

### Context-level abort

When `FindPrompt` returns `prompt == nil` it matched the *context-level* default
prompt rather than a chain frame. `resolveAbort` returns `done = true` for that
case: there is no handler and no remaining code to execute, so the driver returns
nil immediately after setting the value.

### RunWithinBoundary

Because `call-with-continuation-prompt` and `call-with-exit` are now reified as
continuation *chain frames*, such a boundary can land inside a surviving
sub-context (a `with-continuation-barrier` thunk, a `RaiseInPlace` handler, a
`dynamic-wind` thunk, a parameter converter). `RunWithinBoundary` drives such a
sub-context like `Run`, but resolves an abort whose tag names a prompt on *that*
chain inline. An abort targeting an outer boundary, and every
`ErrResumeContinuation`, re-raise unchanged. It installs no `DefaultPromptTag`
and does no panic handling; those belong to the one top-level `RunResumable`.

## RestoreWithWindingFrom

`machine/machine_context_winding.go`

The central dynamic-wind transition function. Reached from `ReinstallSegment`
(both composable and call/cc resume) and from `resolveAbort`.

```
RestoreWithWindingFrom(cont, sourceStack, targetStack)
    │
    ├─ FindCommonWindingPrefix(sourceStack, targetStack) → commonDepth
    │
    ├─ unwindStackTo(sourceStack, commonDepth)
    │   run after thunks: sourceStack[len-1] down to sourceStack[commonDepth]
    │
    ├─ RewindTo(targetStack, commonDepth)
    │   run before thunks: targetStack[commonDepth] up to targetStack[len-1]
    │
    └─ cont != nil? Restore(cont)
```

The `FindCommonWindingPrefix` comparison uses the atomic `ID` field on
each `DynamicWindFrame`. Because frames follow LIFO discipline, the
common prefix uniquely identifies the shared ancestor.

## PrimCallCC: capture once, one apply seam

`registry/core/prim_control.go`

`call/cc` is implemented using the Racket model where a call/cc escape
is equivalent to:

```scheme
(call/cc f) ≡
  (call-with-composable-continuation
    (lambda (k)
      (f (lambda (v) (abort-current-continuation default-prompt-tag (k v)))))
    default-prompt-tag)
```

Concretely, `PrimCallCC`:
1. Delimits the capture at `FindPrompt(DefaultPromptTag)`, then
   `SliceContinuationAt(capturePrompt)`. At the top-level context boundary that
   lookup yields nil and the whole chain is sliced; inside a
   `call-with-continuation-prompt` reusing the default tag it yields that
   prompt's chain frame, so only the delimited segment is captured.
2. Copies the winding stack
3. Creates a `ComposableContinuation` from the segment + winding stack + thread ID + barrier token
4. Snapshots the reachable parameter/handler marks into it
   (`SnapshotReachableMarksInto`), so resume restores the *captured* dynamic
   environment. Composable continuations deliberately do not snapshot: they
   compose with the invoker's marks.
5. Builds a `CapturedContinuation` escape value via `NewCapturedContinuation`

The capture above is shared by both modes below; the only per-mode difference is
driver provenance, so mode is a single target selection rather than two
hand-written apply arms.

### Inline mode (mc.Parent() != nil)

The lambda runs directly in the current VM context. This preserves the
full continuation chain for coroutine patterns. Resume is resolved by the
ambient `DefaultPromptTag` driver already running above this frame.

```
PrimCallCC
  capturePrompt, _ = mc.FindPrompt(DefaultPromptTag)
  segment = mc.SliceContinuationAt(capturePrompt)
  windingStack = mc.WindingStack().Copy()
  comp = NewComposableContinuation(segment, windingStack, threadID, barrierValid)
  mc.SnapshotReachableMarksInto(comp)
  capt = NewCapturedContinuation(comp, threadID, barrierValid)
  mc.ApplyCallable(mcls, capt)
  return nil
```

### Sub-context mode (mc.Parent() == nil)

call/cc is rootless (invoked inside another foreign function's sub-context,
e.g. `apply` or `dynamic-wind`, or at a thread root), so there is no ambient
driver.

```
PrimCallCC
  ... same capture as above ...
  sub = mc.NewSubContext()    // inherits winding stack
  defer ReleaseSubContext(sub)
  sub.ApplyCallable(mcls, capt)
  err = sub.RunWithEscapeHandling()   // installs its own DefaultPromptTag
  if err: return err
  mc.SetValues(sub.GetValues()...)
```

The sub-context *is* the implicit `call-with-continuation-prompt` for this
captured continuation: `RunWithEscapeHandling` (not `RunWithinBoundary`)
resolves this call/cc's resume signal plus any reified boundary on the sub's
chain, so call/cc works in contexts that would otherwise call `Run()` directly.

## call/cc escape value

`machine/captured_continuation.go`: `CapturedContinuation`,
`applyCapturedContinuation`

Invoking a `CapturedContinuation`:
1. Checks thread identity (captured vs invoking thread ID) → `ErrCrossThreadContinuation`
2. Checks barrier identity (captured vs invoking barrier token) → `ErrContinuationBarrier`
3. Copies the invocation values off the eval stack (the resume's `Restore`
   recycles that backing array)
4. Returns `ErrResumeContinuation{DefaultPromptTag, segment, values, SourceWinding}`

It does **not** run the captured chain. Handing the segment back unrun is what
makes resume cost O(1) Go frames and reconcile dynamic-wind exactly once; see
[`resume-trampoline.md`](resume-trampoline.md).

## PrimCallWithContinuationPrompt

`registry/core/prim_prompt.go`

The prompt is a continuation **chain frame**, not a sub-context. `RunBodyUnderPrompt`
(`machine/run_body_under_frame.go`) pushes a transparent prompt frame carrying
the tag and handler onto `mc.cont`, then inline-applies the thunk on the live
chain.

```
PrimCallWithContinuationPrompt(thunk, tag, handler)
  mc.RunBodyUnderPrompt(thunk, tag, handler)
    frame = NewMachineContinuationWithPrompt(mc.cont, returnTemplate, env, tag, handler)
    frame inherits winding stack, marks, barrier token, thread ID
    mc.cont = frame
    mc.ApplyCallable(thunk)
  │
  ├─ thunk returns normally → the transparent frame passes its value(s) through
  │
  └─ abort to tag → routed by the driver's FindPrompt to this frame, then
     resolveAbort: reconcile winding, Restore past the frame, apply handler
     (or deliver the abort values when handler is nil / #f)
```

Two consequences of reifying the prompt on the chain: a continuation captured
inside the thunk *spans* the prompt frame (the old sub-context truncated it), and
`call-with-composable-continuation`'s `FindPrompt(tag)` finds a real frame, so
`SliceContinuationAt(frame)` delimits at it.

## Composable continuation application

`machine/machine_context_apply.go`: `applyComposableContinuation`

```
applyComposableContinuation(cc, args)
  │
  ├─ Thread check: reject if p.threadID != cc.threadID
  │
  ├─ Barrier check: reject if cc.BarrierValid() != p.barrierValid
  │
  ├─ copy args off the eval stack (Restore recycles the backing array)
  │
  ├─ select the escalator arms this resume revives, against p.cont
  │    (segment carries them, the invoker's live chain does not)
  │
  └─ ReinstallSegment(cc, boundary = p.cont, p.windingStack, vals, p.isolatedMarks, revived)
       ├─ install captured marks
       ├─ segment = cc.AcquireSegment()
       │    first invocation: original frames, chain marked shared
       │    re-invocation:    deep copy, so resumes are independent
       ├─ RestoreWithWindingFrom(nil, srcWinding, cc.WindingStack())
       ├─ mark the selected arms revived   // only once the reconcile succeeded
       ├─ GraftContinuation(segment, boundary)   // p.cont = EXTEND (compose)
       ├─ Restore(segment)
       └─ SetValues(vals...)
```

`boundary = p.cont` is what makes this *composable*: the segment extends the live
chain rather than replacing it. The abortive call/cc resume calls the same
`ReinstallSegment` with `boundary` from `FindPrompt` (nil at the top-level
context boundary = replace the whole chain).

`AcquireSegment`'s share-then-copy discipline is critical: without the
first-invocation shared marking, a normal return through a reinstalled frame
could pool an environment the captured segment still needs; without the
re-invocation copy, a second resume would corrupt the shared frames.

## Type and file inventory

### Types

| Type | File | Purpose |
|------|------|---------|
| `PromptTag` | `machine/prompt_tag.go` | Opaque identity, pointer equality, atomic ID |
| `ErrPromptAbort` | `machine/prompt_abort.go` | Abort carrier: tag, values, SourceWinding |
| `ErrResumeContinuation` | `machine/prompt_abort.go` | Resume signal: tag, unrun segment, values, SourceWinding |
| `BarrierToken` | `machine/barrier_token.go` | Opaque barrier identity, pointer equality |
| `ComposableContinuation` | `machine/composable_continuation.go` | Callable delimited continuation segment |
| `CapturedContinuation` | `machine/captured_continuation.go` | call/cc escape value wrapping a `ComposableContinuation` |
| `DynamicWindFrame` | `machine/dynamic_wind.go` | Before/after thunks + atomic ID |
| `WindingStack` | `machine/dynamic_wind.go` | `[]DynamicWindFrame` slice (frames by value) |

### Functions

| Function | File | Purpose |
|----------|------|---------|
| `RunResumable` | `machine/machine_context.go` | The DefaultPromptTag driver loop |
| `RunWithEscapeHandling` | `machine/machine_context.go` | Delegates to `RunResumable` |
| `RunWithinBoundary` | `machine/machine_context.go` | Sub-context driver for reified boundaries on its own chain |
| `resolveAbort` | `machine/machine_context.go` | Shared abort arm of both drivers |
| `applyCallableError` | `machine/foreign_closure.go` | Control-signal passthrough vs. `RaiseInPlace` |
| `FindPrompt` | `machine/machine_context_continuation.go` | Walk continuation chain + check context tag |
| `SliceContinuationAt` | `machine/machine_context_continuation.go` | Deep-copy continuation segment to prompt |
| `GraftContinuation` | `machine/machine_context_continuation.go` | Splice segment onto target chain |
| `RestoreWithWindingFrom` | `machine/machine_context_winding.go` | Unwind/rewind + restore continuation |
| `FindCommonWindingPrefix` | `machine/dynamic_wind.go` | Common ancestor of two winding stacks |
| `ReinstallSegment` | `machine/machine_context_apply.go` | The single resume primitive (abortive + composable) |
| `applyComposableContinuation` | `machine/machine_context_apply.go` | Apply composable continuation value |
| `applyCapturedContinuation` | `machine/captured_continuation.go` | Return the resume signal (checks thread + barrier) |
| `RunBodyUnderPrompt` | `machine/run_body_under_frame.go` | Push a transparent prompt frame, inline-apply the body |
| `PrimCallCC` | `registry/core/prim_control.go` | call/cc primitive (inline + sub-context) |
| `NewCapturedContinuation` | `machine/captured_continuation.go` | Build the call/cc escape value |
| `PrimCallWithContinuationPrompt` | `registry/core/prim_prompt.go` | Install prompt frame, run thunk |
| `PrimAbortCurrentContinuation` | `registry/core/prim_prompt.go` | Return ErrPromptAbort |
| `PrimCallWithComposableContinuation` | `registry/core/prim_prompt.go` | Capture composable continuation |

## End-to-end example: call/cc escape through dynamic-wind

```scheme
(define k #f)
(dynamic-wind
  (lambda () (display "before\n"))
  (lambda () (call/cc (lambda (c) (set! k c) 1)))
  (lambda () (display "after\n")))
;; prints: before, after
;; k is now the escape continuation
```

At capture time, `PrimCallCC` captures a `ComposableContinuation` wrapping
the continuation chain inside the dynamic-wind thunk, plus the winding stack
`[D1]`. `NewCapturedContinuation` wraps this into a `CapturedContinuation`
escape value.

```scheme
(k 42)
;; prints: before, after
;; result: 42
```

What happens at `(k 42)` (as separate REPL interactions; inside one `begin`-wrapped
program the captured continuation includes the rest of the program, and re-invoking
it loops):

1. `applyCapturedContinuation` invoked with value 42
2. Thread check and barrier check pass
3. It selects the escalator arms to revive against **its own** `p.cont`, then returns
   `ErrResumeContinuation{DefaultPromptTag, segment, [42], SourceWinding: [], escalatorRevivals: …}`.
   The segment is **not** run here. Both site-local fields are frozen now for the
   same reason: step 5 may hand the signal to a driver several sub-contexts out.
4. `applyCallableError` passes the signal through unchanged
5. `Run()` returns it to `RunResumable`
6. `boundary, _ = FindPrompt(DefaultPromptTag)` → nil (context-level boundary),
   so the reinstalled segment *replaces* the live chain
7. `ReinstallSegment(segment, nil, [], [42], isolate = true, revivals)`:
   a. install the captured mark snapshot
   b. `AcquireSegment()`: original frames on first invoke, deep copy after
   c. `RestoreWithWindingFrom(nil, [], [D1])`:
      `FindCommonWindingPrefix([], [D1])` = 0; no unwinding (source is empty);
      rewind D1 → before thunk prints "before"; then mark the selected arms
   d. `GraftContinuation(segment, nil)`, `Restore(segment)`, `SetValues(42)`
8. The driver loops; the resumed chain runs on its own `Run()` loop, O(1) Go frames
9. The dynamic-wind body returns 42; its after thunk prints "after"
10. `Run()` returns nil, `RunResumable` returns 42

The winding reconcile happens exactly once, in step 7c/9. The earlier design
reconciled in both `ReinstallSegment` and an abort catch, and printed "after" twice.

## End-to-end example: composable continuation

```scheme
(define tag (make-continuation-prompt-tag))
(call-with-continuation-prompt
  (lambda ()
    (+ 1 (call-with-composable-continuation
            (lambda (k) (k 10))
            tag)))
  tag
  #f)
;; result: 12
```

What happens:

1. `PrimCallWithContinuationPrompt` pushes a transparent prompt frame carrying
   `tag` onto `mc.cont` and inline-applies the thunk
2. Thunk runs, reaches `call-with-composable-continuation`
3. `PrimCallWithComposableContinuation`:
   a. `FindPrompt(tag)` → the prompt **chain frame**
   b. `SliceContinuationAt(frame)` → deep-copy the delimited segment (the `(+ 1 <hole>)` frame)
   c. Create `ComposableContinuation` with segment + winding stack. No mark
      snapshot: composable resume composes the invoker's marks
   d. Apply `(lambda (k) (k 10))` **in place** on the live chain (`mc.Parent() != nil`)
   e. `(k 10)` runs `applyComposableContinuation` → `ReinstallSegment` with
      `boundary = p.cont`, extending the live chain, then `SetValues(10)`
   f. The composed `(+ 1 10)` yields 11, which is `proc`'s result
4. `proc`'s 11 flows in place into the *live* `(+ 1 _)` frame, which the capture
   did not remove → 12
5. The thunk returns 12 normally; the transparent prompt frame passes it through

Step 4 is what makes this *composable* rather than *control*: the capture does
not abort, so the delimited frames run again. Verified against Racket v9.2.
`shift`/`control` add their own `abort-current-continuation` on top of this raw
capture (`pkg/stdlib/lib/wile/control.scm`); the primitive itself must not.
