# Delimited Continuations

This document describes Wile's implementation of delimited continuations and the problem they solve. It covers the interaction between `call/cc`, `dynamic-wind`, Go-implemented primitives, and the prompt/abort/composable mechanism.

## The Problem: Truncated Continuations in Go Primitives

### What a continuation contains

When Scheme captures a continuation with `call/cc`, the captured object represents "the rest of the computation." In Wile's VM, a continuation is a linked list of `MachineContinuation` frames, each holding a saved program counter, environment, eval stack, and template. The chain encodes the full call stack from the capture point back to the top level.

### Where Go primitives break this

Before this change, `map` and `for-each` were implemented in Go:

```go
func PrimMap(mc *machine.MachineContext) error {
    // ...
    for {
        // Collect one element from each list
        // ...
        sub := mc.NewSubContext()
        _, err := sub.Apply(mcls, args...)
        err = sub.Run()
        // ...
        results = append(results, sub.GetValue())
    }
    mc.SetValue(values.List(results...))
    return nil
}
```

The Go `for` loop, the `results` accumulator, and the `sub.Run()` call are not Scheme continuation frames. They exist on the Go call stack, invisible to the Scheme continuation system. When `call/cc` captures inside the callback:

```scheme
(map (lambda (x)
       (if (= x 2)
           (call/cc (lambda (k) (set! saved-k k) 200))
           (* x 10)))
     '(1 2 3))
```

The captured continuation includes only the callback's internal frames. It does not include "continue iterating `map` from element 3" or "cons the result onto `map`'s accumulator" because those live in Go, not in Scheme frames.

Re-invoking `saved-k` later re-enters the callback at `x=2` but cannot resume the `map` iteration. The remaining elements are lost, and the outer computation that called `map` never sees a result.

### The general pattern

Any Go primitive that calls a Scheme closure in a sub-context creates a boundary that truncates continuations. This affected `map`, `for-each`, `apply`, and `call-with-values`. The two remedies below were the first; the general one arrived later, when every such boundary was *reified* as a continuation chain frame that the body runs under inline (`RunBodyUnderFrame` and friends in `machine/run_body_under_frame.go`): `call-with-values` now pushes a consumer frame, `call-with-exit` a finalizer frame, `call-with-continuation-prompt` a transparent prompt frame. A continuation captured inside the body spans those frames instead of stopping at them.

## The Solution: Two Complementary Changes

### 1. Rewrite map/for-each in Scheme

The direct fix: make the iteration itself consist of Scheme continuation frames.

```scheme
(define map
  (case-lambda
    ((f lst)
     (let loop ((rest lst) (acc '()))
       (if (null? rest)
           (reverse acc)
           (loop (cdr rest) (cons (f (car rest)) acc)))))
    ((f lst . lsts)
     (let loop ((all (cons lst lsts)) (acc '()))
       (if (let any-null? ((ls all))
             (if (null? ls) #f
                 (if (null? (car ls)) #t
                     (any-null? (cdr ls)))))
           (reverse acc)
           (loop (map cdr all)
                 (cons (apply f (map car all)) acc)))))))
```

Now the named-let `loop` compiles to Scheme frames. When `call/cc` captures inside `(f (car rest))`, the continuation chain includes the pending `cons`, the `loop` call, and the full outer computation. Re-entering the continuation resumes the iteration naturally.

The accumulate-and-reverse shape is load-bearing, not a stylistic choice. The structural form `(cons (f (car lst)) (loop (cdr lst)))` leaves a `cons` pending per element, so a long list raises `ErrCallDepthExceeded`; the tail form has no ceiling. A `set-cdr!` tail pointer allocates n+1 cells instead of 2n and measured much slower. Do not "simplify" it back; see `pkg/wile/map_depth_test.go` for the pin.

The multi-list case uses `any-null?` (a named-let helper) to check if any input list is exhausted, matching R7RS's behavior of stopping at the shortest list.

The definitions live in `registry/core/bootstrap_procedures.scm`, alongside `vector-map`, `vector-for-each`, `string-map`, `string-for-each`, `member`, and `assoc`, all moved to Scheme for the same reason.

### 2. Add delimited continuations

Delimited continuations (Racket-style prompts) provide a general mechanism for controlling how much of the continuation is captured. While the immediate motivation was `map`/`for-each`, delimited continuations are independently useful for implementing generators, coroutines, effect handlers, and other control patterns.

## Architecture

### Types

```
PromptTag                    Opaque identity (pointer equality, atomic ID)
ComposableContinuation       Callable value: continuation segment + winding stack
CapturedContinuation         call/cc escape value wrapping a ComposableContinuation
ErrPromptAbort               Control signal for abort propagation
ErrResumeContinuation        Control signal for call/cc resume (the trampoline)
```

### Where prompts live

A prompt can be either:

1. **On the continuation chain**: a `MachineContinuation` frame with a non-nil `promptTag` field. This is what `call-with-continuation-prompt` installs, via `RunBodyUnderPrompt`.
2. **On the MachineContext**: the `promptTag` field, set only by `RunResumable` when it installs `DefaultPromptTag` on its own context.

`FindPrompt` checks both, returning `(frame, true)`, `(nil, true)` for the context-level boundary, or `(nil, false)`. The two "true" answers are distinct and both are used: a chain frame is a delimiter to slice at or restore past, while `nil` means "the boundary of this context", which for a resume means *replace the whole chain* (the abortive case).

Earlier versions ran the prompt thunk in a sub-context and used `SetPromptTag` to mark it. That truncated any continuation captured inside the thunk, which is exactly the sub-context boundary problem described above. `SetPromptTag` has no production caller today.

### Error propagation

Two control signals, both declared in `machine/prompt_abort.go`, ride the VM's ordinary `return err` plumbing:

```
        abort-current-continuation       call/cc continuation invoked
        call-with-exit exit closure      │
        shift / control                  │
        │                                │
        ▼                                ▼
        ErrPromptAbort                   ErrResumeContinuation
        {tag, values,                    {tag, unrun segment, values,
         SourceWinding}                   SourceWinding}
        │                                │
        └────────────────┬───────────────┘
                         ▼
   applyCallableError (machine/foreign_closure.go): control signals
   pass through unchanged; anything else becomes a Scheme condition
                         │
                         ▼
           Run() returns the signal to its driver
                         │
        ┌────────────────┴───────────────┐
        ▼                                ▼
        RunWithinBoundary                RunResumable
        (prompt frame on the             (the DefaultPromptTag driver;
         sub-context's own chain;         RunWithEscapeHandling
         otherwise re-raises)             delegates to it)
```

The order in `applyCallableError` matters: `ErrPromptAbort`, `ErrExceptionEscape`, `ErrTimerInterrupt`, and `ErrResumeContinuation` are each recognized *before* the fallthrough. Without those checks, an abort or a resume would be converted into a catchable Scheme condition and its driver would never see it.

## How Each Primitive Works

### call-with-continuation-prompt

```scheme
(call-with-continuation-prompt thunk tag handler)
```

Implementation in Go (`PrimCallWithContinuationPrompt` → `RunBodyUnderPrompt`):

1. Push a *transparent* prompt frame onto `mc.cont`, carrying `tag` and `handler`. The frame inherits the current winding stack, marks, barrier token, and thread ID.
2. Inline-apply `thunk` on the live chain. No sub-context.
3. If the thunk returns normally, the frame's `returnTemplate` passes its value(s) straight through.
4. An abort to `tag` is routed by the driver's `FindPrompt` to this frame, and `resolveAbort` reconciles dynamic-wind, restores *past* the frame, and applies the handler (or delivers the abort values when the handler is nil / `#f`).
5. An abort to a different tag simply does not match this frame and keeps walking.

Reifying the prompt on the chain is what lets a continuation captured inside the thunk span the prompt, and what lets `call-with-composable-continuation` slice at a real frame.

### abort-current-continuation

```scheme
(abort-current-continuation tag v ...)
```

Returns `&ErrPromptAbort{Tag: tag, Values: vs, SourceWinding: …}`. The signal propagates up through the VM until a driver's `FindPrompt` matches it. (The `call-with-exit` exit closure and `shift`/`control` emit the same signal, `call-with-exit` with a private per-invocation tag. All three carry a `SourceWinding` copy: the winding live at the abort origin, which may be a deeper sub-context than the driver holding the prompt — without it a `(dynamic-wind …)` established inside an `(eval …)` loses its after-thunk.)

### call-with-composable-continuation

```scheme
(call-with-composable-continuation proc tag)
```

This is the most complex primitive. Implementation:

1. `FindPrompt(tag)`: walk the continuation chain and check the context-level prompt. No match is an error: there is nothing to delimit at.
2. `SliceContinuationAt(prompt)`: deep-copy the continuation frames from the current position down to (but not including) the prompt. The bottom frame's parent is set to nil, creating a standalone segment. The live source chain is marked shared, since the copies alias its environment frames.
3. Create a `ComposableContinuation` wrapping the segment and a copy of the current winding stack (plus thread ID and barrier token). No mark snapshot is taken: composable resume composes the invoker's marks, unlike `call/cc`.
4. Apply `proc` with the composable continuation **in place** on the live chain, mirroring `call/cc`'s inline mode (`prim_control.go`), NOT aborting. The deep copy in step 2 left the originals on the live chain, so `proc`'s result flows back through them. When the capture is rootless (`mc.Parent() == nil`), `proc` runs in a fresh sub-context under its own `DefaultPromptTag` driver instead, exactly as call/cc's sub-context mode; its value(s) are then delivered to `mc`.

This is the defining behavior of a *composable* continuation (Racket semantics, verified against Racket v9.2): `proc` runs in the continuation of the `call-with-composable-continuation` call — it does not remove the current continuation — and applying the captured continuation **composes** (extends) rather than replaces, so the captured frames may legitimately run more than once. For example `(+ 1 (call-with-composable-continuation (lambda (k) (k (k 10))) tag))` under a prompt yields **13**: `(k 10)`→11, `(k 11)`→12, then `proc`'s 12 flows in place into the live `(+ 1 _)` → 13. `shift`/`control` add their own `abort-current-continuation` on top of this raw capture (`wile/control.scm`); the primitive itself must not. (Earlier versions aborted to the prompt — `control`/frame-removing semantics — which was non-conformant to the Racket primitive this primitive follows.)

### Applying a composable continuation

When a `ComposableContinuation` is called as a procedure (dispatched by `ApplyCallable` to `applyComposableContinuation`):

1. Thread check: reject if invoked from a different SRFI-18 thread. Barrier check: reject if the barrier token differs.
2. Copy the argument values off the eval stack, whose backing array `Restore` recycles.
3. Delegate to `ReinstallSegment` with `boundary = p.cont`, which:
   a. installs the captured marks and bumps `resumeGeneration`;
   b. calls `AcquireSegment()`: the original frames on first invocation (marking the chain shared), a deep copy on re-invocation;
   c. reconciles dynamic-wind: `RestoreWithWindingFrom(nil, current, captured)` unwinds extents not in the captured stack and rewinds captured extents not in the current stack;
   d. grafts the segment's bottom frame onto `boundary`, restores from the segment's top frame, and delivers the values.

`ReinstallSegment` is the *single* resume primitive: the abortive `call/cc` resume goes through it too, differing only in `boundary` (from `FindPrompt`, `nil` = replace the whole chain) and `isolate` (true, restoring the captured mark snapshot).

The share-then-copy discipline in step 3b is essential in both directions. Without the first-invocation shared marking, a normal return through a reinstalled frame could pool an environment the captured segment still needs. Without the re-invocation copy, a second resume would corrupt frames the first one mutated.

## Interaction with dynamic-wind

### The winding stack model

Each `DynamicWindFrame` has an atomic ID. The winding stack is a slice of frame pointers, outermost at index 0. `FindCommonWindingPrefix` compares two stacks by ID to find where they diverge.

When transitioning between dynamic extents (whether from `call/cc`, abort, or composable continuation application), `RestoreWithWindingFrom` runs:

1. After thunks from innermost to the common ancestor (unwinding).
2. Before thunks from the common ancestor to the target (rewinding).

### Abort + dynamic-wind

When an abort crosses a `dynamic-wind` boundary, the after thunks must run. This is handled once, in the driver's `resolveAbort`, which reconciles from `abortErr.SourceWinding` (the winding live at the *escape point*, possibly a deeper sub-context than the driver's) to the found prompt frame's winding. The abort's emitter must therefore not unwind itself: `call-with-exit`'s exit closure carries the source winding and deliberately does no `UnwindTo`, because unwinding at both ends double-fires the after thunks, and reconciling from the driver's own winding instead skips a deeper sub-context's.

### Composable continuation + dynamic-wind

When a composable continuation is applied, `RestoreWithWindingFrom` handles the winding transition. The captured winding stack records what extents were active at capture time. The prefix algorithm correctly identifies which extents to unwind (current but not captured) and which to rewind (captured but not current).

## Why the winding stack model is sufficient

The prefix-based comparison works because both R7RS `dynamic-wind` and Racket-style prompts follow stack discipline: frames are pushed and popped in LIFO order. The atomic frame IDs give each extent a permanent, unique identity, so the comparison is unambiguous.

Consider a composable continuation K captured with winding `[D1, D3]`, applied in a context with winding `[D1, D2]`:

```
FindCommonWindingPrefix([D1, D2], [D1, D3]) = 1
Unwind: call after(D2)   → stack becomes [D1]
Rewind: call before(D3)  → stack becomes [D1, D3]
```

This is correct: D2 is exited, D3 is entered, and D1 (the common ancestor) is left undisturbed.

## Unified continuation control flow

Both kinds of continuation control flow share one resume primitive (`ReinstallSegment`) and one driver (`RunResumable`), and differ only in the signal that reaches it. Call/cc still follows Racket's model, where a call/cc escape is a composable capture delimited at the default prompt plus an abort to it, but the *mechanism* is the resume trampoline rather than an abort carrying an already-computed result. See [`resume-trampoline.md`](resume-trampoline.md).

```
┌──────────────────┬───────────────────────────┬─────────────────────────────┐
│                  │ call/cc resume            │ Prompt abort                │
├──────────────────┼───────────────────────────┼─────────────────────────────┤
│ Signal type      │ ErrResumeContinuation     │ ErrPromptAbort              │
│                  │ (to DefaultPromptTag)     │ (to a user or exit tag)     │
├──────────────────┼───────────────────────────┼─────────────────────────────┤
│ Carries          │ Unrun segment, values,    │ Tag, values,                │
│                  │ SourceWinding             │ SourceWinding               │
├──────────────────┼───────────────────────────┼─────────────────────────────┤
│ Resolved by      │ RunResumable →            │ RunResumable or             │
│                  │ ReinstallSegment          │ RunWithinBoundary →         │
│                  │                           │ resolveAbort                │
├──────────────────┼───────────────────────────┼─────────────────────────────┤
│ Effect           │ Reinstalls the segment;   │ Restores past the prompt;   │
│                  │ nil boundary replaces     │ runs its handler or         │
│                  │ the live chain            │ delivers the values         │
├──────────────────┼───────────────────────────┼─────────────────────────────┤
│ Composable       │ No (replaces)             │ Yes (via ComposableCont.)   │
└──────────────────┴───────────────────────────┴─────────────────────────────┘
```

`ComposableContinuation` is the qualitatively new capability. Unlike a `call/cc` continuation, which replaces the live chain, a composable continuation splices frames in and resumes normally. This enables patterns like:

```scheme
(define tag (make-continuation-prompt-tag))

(+ 10 (call-with-continuation-prompt
         (lambda ()
           (+ 1 (call-with-composable-continuation
                   (lambda (k)
                     (+ (k 2) (k 3)))  ; k can be called multiple times
                   tag)))
         tag
         #f))
;; k captures (+ 1 <hole>)
;; (k 2) = 3, (k 3) = 4, sum = 7
;; the capture did NOT remove the live (+ 1 _), so proc's 7 flows in place
;;   through it => 8; prompt returns 8, outer + 10 = 18
```

## Continuation chain operations

### SliceContinuationAt

Creates a standalone copy of the continuation chain from the current position to a boundary:

```
Before:                        After slicing at P:
  mc.cont → F1 → F2 → P → F3 → nil
                                      Segment: F1' → F2' → nil
                                      (deep copy, parent = nil)
```

When the prompt is at the context boundary (not a continuation frame), `SliceContinuationAt(nil)` copies the entire chain.

### GraftContinuation

Splices a segment onto a target chain by walking the segment to its bottom and setting `parent = target`:

```
Before:
  segment: F1 → F2 → nil
  target:  G1 → G2 → nil

After:
  segment: F1 → F2 → G1 → G2 → nil
```

### DeepCopy

Creates an independent copy of an entire continuation chain. Every frame is `Copy()`'d with parent pointers relinked to the copies. Used before grafting to ensure re-invocation safety.

## Code locations

| File | Contents |
|------|----------|
| `machine/prompt_tag.go` | `PromptTag` type, `DefaultPromptTag` |
| `machine/composable_continuation.go` | `ComposableContinuation` callable value, `AcquireSegment` |
| `machine/prompt_abort.go` | `ErrPromptAbort`, `ErrResumeContinuation` |
| `machine/dynamic_wind.go` | `DynamicWindFrame`, `WindingStack`, `FindCommonWindingPrefix` |
| `machine/machine_continuation.go` | `promptTag`/`promptHandler` fields, `DeepCopy()` |
| `machine/barrier_token.go` | `BarrierToken` opaque barrier identity |
| `machine/machine_context.go` | `RunResumable`, `RunWithEscapeHandling`, `RunWithinBoundary`, `resolveAbort` |
| `machine/run_body_under_frame.go` | `RunBodyUnderPrompt` and the other reified-boundary constructors |
| `machine/machine_context_continuation.go` | `FindPrompt`, `SliceContinuationAt`, `GraftContinuation` |
| `machine/machine_context_winding.go` | `RestoreWithWindingFrom` |
| `machine/machine_context_apply.go` | `ReinstallSegment`, `applyComposableContinuation` |
| `machine/foreign_closure.go` | `applyCallableError` (control-signal passthrough) |
| `machine/machine_context_apply.go` | `bridgeForeignError` (the only wrapper around it) |
| `registry/core/prim_prompt.go` | Prompt primitive implementations |
| `machine/captured_continuation.go` | `CapturedContinuation` and `applyCapturedContinuation`: the call/cc escape value |
| `registry/core/prim_control.go` | `PrimCallCC` |
| `registry/core/prim_barrier.go` | `PrimCallWithContinuationBarrier` |
| `registry/core/prim_exit.go` | `PrimCallWithExit` (reified exit frame) |
| `registry/core/prompts.go` | Primitive registration |
| `registry/core/bootstrap_procedures.scm` | Scheme `map`/`for-each` definitions |

## References

- Flatt, Yu, Findler, Felleisen. "Adding Delimited and Composable Control to a Production Programming Environment." ICFP 2007. The Racket model this implementation follows.
- Felleisen. "The Theory and Practice of First-Class Prompts." POPL 1988. Original formalization of prompts and aborts.
- Danvy, Filinski. "Abstracting Control." LFP 1990. Shift/reset as the composable variant.
- Hieb, Dybvig. "Continuations and Concurrency." PPoPP 1990. Spawn operator.
- Queinnec, Serpette. "A Dynamic Extent Control Operator for Partial Continuations." POPL 1991. Set/cupto.
- R7RS section 6.10 (dynamic-wind, call/cc).

## Derived Operators: (wile control)

The `(wile control)` library provides all named delimited continuation operators
from Racket's `racket/control` module, derived entirely from the three core
primitives above (no additional VM paths).

### Operator Matrix

| Family          | Handler reinstalls prompt? | k wraps in prompt? | Source |
|-----------------|---------------------------|--------------------|--------|
| prompt/control  | Yes                       | No                 | Felleisen 1988 |
| reset/shift     | Yes                       | Yes                | Danvy & Filinski 1990 |
| prompt0/control0| No                        | No                 | — |
| reset0/shift0   | No                        | Yes                | — |
| spawn           | — (captures and applies)  | —                  | Hieb & Dybvig 1990 |
| set/cupto       | No                        | No                 | Queinnec & Serpette 1991 |

All operators have `-at` tagged variants that accept an explicit prompt tag.
The untagged forms use `(default-continuation-prompt-tag)`.

### Implementation Note: Self-Referential Macros

Operators whose handlers reinstall the prompt (`prompt-at`, `reset-at`) cannot
be implemented as self-referential `syntax-rules` macros — the handler template
`(lambda (thunk) (reset-at t (thunk)))` causes infinite compile-time expansion
because each expansion of `reset-at` produces another `reset-at` in the handler.

The solution is a runtime helper function `%prompt-reinstall` that calls itself
recursively. Function bodies are compiled once; the recursion happens at runtime
when the handler is actually invoked.
