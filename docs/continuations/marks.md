# Continuation Marks

Imagine you're debugging a program and you want to know: "what's the current
call stack?" In most languages, you'd call something like `Thread.currentThread().getStackTrace()` or inspect a backtrace. The runtime walks the call stack and hands you a list of frames.

Now imagine you want something richer. Not just "which functions are on the stack" but arbitrary metadata: "what's the current log level at each frame?" or "which exception handlers are active?" or "what security context am I running in?"

You *could* use a global variable. But globals don't know about stack structure. Set a global, call a function, it's still set when you return. You have to manually restore it. Miss a restore path — say, an exception — and the value leaks.

You *could* use `dynamic-wind`. That's the Scheme mechanism for running cleanup code when control enters or leaves a scope. But dynamic-wind runs thunks — actual code — on every entry and exit. For a simple annotation that says "I'm in the logging context now," that's like hiring a moving crew to put a sticky note on your desk.

Continuation marks are the right tool for this. They let you attach key-value pairs directly to continuation frames, with zero ceremony and correct tail-call behavior.

## The Problem, Precisely

Here's the scenario that forces this design. Suppose you want to implement a
stack-aware profiler. Every function call should annotate the current frame with
its name, and at any point you can collect all the annotations to see the "logical
call stack."

First attempt — a mutable list:

```scheme
(define *stack* '())

(define (enter name)
  (set! *stack* (cons name *stack*)))

(define (leave)
  (set! *stack* (cdr *stack*)))

(define (f)
  (enter 'f)
  (let ((result (g)))
    (leave)
    result))
```

This breaks in three ways:

1. **Exception safety.** If `g` raises an exception, `leave` never runs. The
   stack list is now permanently wrong.

2. **Tail calls.** If `f` tail-calls `g`, there's no return to `f` — so `leave`
   never runs either. But this time it's not an error, it's *by design*. Tail
   calls are supposed to reuse the frame. Your annotation mechanism shouldn't
   prevent that.

3. **Continuations.** If `g` captures a continuation and someone invokes it
   later, the global `*stack*` is whatever it happens to be at invocation time,
   not what it was when the continuation was captured. The metadata is detached
   from the control flow it's supposed to describe.

The core issue: **metadata about the call stack should live *on* the call
stack**, not beside it in a global variable.

## The Key Insight

In Wile, the "call stack" isn't a hardware stack. It's a linked list of
`MachineContinuation` objects. When you call a non-tail function, `SaveContinuation` creates a new frame and links it onto the chain:

```
MachineContext
  cont ──> frame₂ ──> frame₁ ──> frame₀ ──> nil
```

Each frame holds its own `env`, `template`, `pc`, `evals` — everything needed
to resume execution when the callee returns. This is what makes `call/cc`
possible: the entire chain is capturable data, not ephemeral stack memory.

Continuation marks exploit this structure. Each frame carries an optional
slice of key-value entries:

```
frame.marks = [(key₁, val₁), (key₂, val₂), ...]
```

Most frames carry no marks (the slice stays `nil` — zero cost). When you
write `(with-continuation-mark key val body)`, the runtime records the
entry on the *current* frame, evaluates `body`, and the mark naturally
disappears when the frame is popped.

No cleanup thunks. No global state. The mark's lifetime is the frame's lifetime.

**Why a slice, not a map.** Keys are compared with `eq?` — pointer identity,
not hash-equality. Scheme allows arbitrary values as keys (pairs, vectors,
symbols, procedures), and two structurally-identical values may or may not
be `eq?`. A Go map would compare by structural equality on primitive types
and panic on non-comparable types (`[]T`, functions, maps). A slice of
`(key, val)` entries with a linear scan is correct by construction and fast
because mark sets are typically small (0-3 entries per frame). The switch
from map to slice was PR #508 for exactly this reason.

## Tail Position: The Defining Behavior

Here's where it gets interesting. Consider:

```scheme
(with-continuation-mark 'k 1
  (with-continuation-mark 'k 2
    (current-marks 'k)))
```

What should `(current-marks 'k)` return? The answer depends on whether the
inner `with-continuation-mark` is in *tail position* relative to the outer one.

In this case, it is. The body of the outer form is the inner form — no
intervening computation. That means both marks target the *same frame*. The
inner `(with-continuation-mark 'k 2 ...)` doesn't create a new frame; it
*replaces* `k`'s value on the current frame:

```
current frame: marks = { k: 2 }    <-- 1 was overwritten
```

Result: `(2)` — a list with one element.

Now contrast:

```scheme
(with-continuation-mark 'k 1
  (list (with-continuation-mark 'k 2
          (current-marks 'k))))
```

The `list` call is *not* in tail position (its result is passed to `list`).
So `SaveContinuation` creates a new frame before evaluating the inner
`with-continuation-mark`. Now there are two frames, each with its own mark:

```
current frame: marks = { k: 2 }
parent frame:  marks = { k: 1 }
```

Result: `((2 1))` — `current-marks` collected both values.

This is the defining feature of continuation marks: **same frame = replacement,
new frame = accumulation.** The mechanism inherits tail-call semantics for free
because tail calls don't create new frames.

Why does this matter? Because it means a tail-recursive loop with a mark
doesn't grow the mark collection:

```scheme
(define (loop n)
  (with-continuation-mark 'iteration n
    (if (= n 0)
        (current-marks 'iteration)
        (loop (- n 1)))))    ;; tail call -- same frame, mark replaced

(loop 1000000)  ;; => (0), not a million-element list
```

If marks accumulated on tail calls, you'd have a million entries — a space
leak that defeats the purpose of tail-call optimization.

## Collecting Marks: Walking the Chain

To collect marks, you walk the continuation chain — the same linked list that
`CaptureStackTrace` already walks in `machine/machine_context.go:995`:

```go
// CaptureStackTrace walks mc.cont chain for error reporting.
// Mark collection follows the same pattern:
cont := p.cont
for cont != nil {
    for _, entry := range cont.marks {
        if eqIdentity(entry.key, key) {
            result = append(result, entry.val)
            break // one value per frame; inner shadows outer
        }
    }
    cont = cont.parent
}
```

The walk produces a list of values for a given key, ordered from innermost
(current frame) to outermost (top-level). This is a `ContinuationMarkSet` —
a snapshot of the marks visible at the point of collection.

### Prompt Delimiting

Collection doesn't always walk the entire chain. If a *prompt tag* is
specified, the walk stops at the first continuation frame with a matching
`promptTag`. This is how delimited continuations interact with marks:
marks below the prompt boundary are invisible.

```scheme
(call-with-continuation-prompt
  (lambda ()
    (with-continuation-mark 'k 'inner
      (continuation-mark-set->list
        (current-continuation-marks) 'k)))
  my-tag)
;; Only collects marks above the prompt
```

Wile already has `FindPrompt(tag)` for locating prompt boundaries in the
continuation chain (`machine/machine_context_continuation.go`). Mark
collection reuses this: walk frames, collect marks, stop at prompt.

## What Would Break Without Marks

Consider implementing `parameterize` (Scheme's scoped dynamic bindings). The
current implementation uses `dynamic-wind`:

```scheme
(parameterize ((current-log-level 'debug))
  (do-stuff))
```

Under the hood, `dynamic-wind` runs a thunk on entry to set the parameter and
a thunk on exit to restore it. That's two closure allocations and two
invocations per `parameterize` — even if nothing ever reads the parameter.

With continuation marks, `parameterize` becomes:

```scheme
(with-continuation-mark <parameter-key> 'debug
  (do-stuff))
```

Reading the parameter means "find the nearest mark for this key." No thunks,
no allocations, no entry/exit overhead. This is exactly how Racket implements
parameters, and it's why continuation marks were invented.

Without marks, you're stuck choosing between:
- **Global mutation** (wrong in the presence of continuations)
- **dynamic-wind** (correct but expensive)
- **Thread-local storage** (doesn't compose with continuations at all)

Marks give you the correctness of `dynamic-wind` at the cost of a slice append (O(1) amortized; typical sets are 0-3 entries per frame, so no reallocation churn).

## The Subtle Parts

**Interaction with `call/cc`.** When a continuation is captured, its marks
come with it — they're part of the frame data. When you invoke a captured
continuation, the marks are restored along with everything else. This is
correct: the marks describe the execution context, and restoring a
continuation means restoring that context.

But `Copy()` vs `DeepCopy()` matters. A shallow copy shares the marks map;
if the original frame's marks are later mutated (by another
`with-continuation-mark` on that frame), the copy sees the mutation. Deep
copy avoids this but costs more. For composable continuations (which can be
invoked multiple times), deep copy is required.

**Lazy allocation.** Most frames never carry marks. The `marks` field is a
slice initialized to `nil`. Only `with-continuation-mark` allocates it, on
first use. The common case (no marks) adds zero overhead — a nil slice is
the zero value and costs only the header (three pointer-sized words).

**Save/restore semantics.** When `SaveContinuation` fires, it copies
`mc.marks` into the new frame and nils out `mc.marks` — the callee starts
with a clean mark set (see comments at `machine/vm_state.go:219-223`). On
return, `Restore`/`PopContinuation` lifts the saved marks back. This is why
a tail-recursive loop with a mark does not accumulate entries: tail calls
skip `SaveContinuation` and just overwrite the current frame's entry.

**Tail-position detection.** The compiler must know whether the body of
`with-continuation-mark` is in tail position. If it is, no `SaveContinuation`
is emitted and the mark is set on the current frame. If it isn't,
`SaveContinuation` creates a new frame first, and the mark goes on that new
frame. This is the same tail-position tracking (`CompileTimeCallContext.inTail`)
that already governs whether function calls are optimized.

## Summary

Continuation marks are per-frame key-value annotations on the continuation
chain. They solve a specific problem: attaching metadata to execution context
in a way that respects tail calls, survives continuation capture, and costs
nothing when unused. The mechanism is simple — a map on each frame, a chain
walk to collect — but the interaction with tail-call semantics is what makes
it genuinely useful rather than just a different spelling for global variables.
