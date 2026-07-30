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

## Tail Position: The Defining Behavior

Here's where it gets interesting. (In the examples below, `current-marks`
is pedagogical shorthand for `(continuation-mark-set->list
(current-continuation-marks) 'k)` — the real R7RS entry point.) Consider:

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
`CaptureStackTrace` (`machine/machine_context.go`) already walks:

```go
// CaptureStackTrace walks mc.cont chain for error reporting.
// Mark collection follows the same pattern — but must scan the
// current frame first (marks set on the live mc before the next
// SaveContinuation live on p.marks, not in any cont frame yet):
for _, entry := range p.marks {
    if values.EqIdentity(entry.key, key) {
        result = append(result, entry.val)
        break
    }
}
cont := p.cont
for cont != nil {
    for _, entry := range cont.marks {
        if values.EqIdentity(entry.key, key) {
            result = append(result, entry.val)
            break // one value per frame; inner shadows outer
        }
    }
    cont = cont.parent
}
```

The real implementation (`CollectContinuationMarks` in `machine/continuation_mark_set.go`) does this in the opposite order — builds a `frames` slice with the current frame's marks appended first, then walks the chain — and returns a `ContinuationMarkSet` rather than a raw list. Same invariant either way: current frame first, innermost-to-outermost. Its sibling `CollectMarksFromContinuation` runs the same walk over a *captured* chain, which is what `(continuation-marks k)` uses.

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

Collection does its own walk rather than calling `FindPrompt(tag)`
(`machine/machine_context_continuation.go`): it stops at the first *frame*
whose `promptTag` matches and deliberately does not consult the context's own
`promptTag`, since `current-continuation-marks` is only reachable from inside
that boundary anyway.

## What Would Break Without Marks

Take `parameterize` (Scheme's scoped dynamic bindings):

```scheme
(parameterize ((current-log-level 'debug))
  (do-stuff))
```

The `dynamic-wind` spelling runs a thunk on entry to set the parameter and a
thunk on exit to restore it. That's two closure allocations and two invocations
per `parameterize` (even if nothing ever reads the parameter), and the
before/after thunks can clobber an unrelated `parameterize` extent when a
composable continuation is spliced in.

Wile's `parameterize` is a mark form instead. The macro
(`registry/core/bootstrap_macros.scm`) evaluates each parameter object and its
converted value in the *outer* dynamic extent, per R7RS §4.2.6, then nests one
`with-continuation-mark` per binding:

```scheme
(with-continuation-mark <parameter-object> 'debug
  (do-stuff))
```

Reading the parameter means "find the nearest mark for this key"
(`findParameterInMarks` in `machine/machine_context_apply.go`, falling back to
the parameter's base value). No thunks, no entry/exit overhead, and composing a
captured continuation carries its parameter bindings automatically because the
marks ride the frames. This is how Racket implements parameters, and it's why
continuation marks were invented.

Without marks, you're stuck choosing between:
- **Global mutation** (wrong in the presence of continuations)
- **dynamic-wind** (correct but expensive)
- **Thread-local storage** (doesn't compose with continuations at all)

Marks give you the correctness of `dynamic-wind` at the cost of an append to a small slice.

## The Subtle Parts

**Interaction with `call/cc`.** When a continuation is captured, its marks
come with it — they're part of the frame data. When you invoke a captured
continuation, the marks are restored along with everything else. This is
correct: the marks describe the execution context, and restoring a
continuation means restoring that context.

Marks are not shared across a copy: `MachineContinuation.Copy()` already does
`q.marks = cloneMarks(p.marks)`, so a later `with-continuation-mark` on the
original frame cannot be seen by the copy. `Copy()` vs `DeepCopy()` is a
question of *how much chain* is duplicated (one frame versus every frame down
to the root), not of mark sharing. Composable continuations, which can be
invoked multiple times, need the whole chain, so `AcquireSegment` uses
`DeepCopy` on re-invocation.

**Slice, not map.** The `marks` field is a slice of `(key, val)` entries,
not a Go map. Keys are compared with `eq?` via `values.EqIdentity`
(`values/utils.go`): pointer equality for most values, but *symbols
compare by name* (`sa.Key == sb.Key`) — symbol interning was removed in
PR #529, so two `'foo` symbol values may be distinct pointers that must
still compare equal. A Go map keyed by `values.Value` can't express that at
all: it hashes by dynamic type and value, so the two `'foo` allocations land
on different keys. That is the whole reason PR #508 switched to a slice with
linear scan. (The older second reason, that a map key would panic on a
non-comparable `Value`, is gone: Go-comparability is now a hard contract on
`Value`.) Cheap in practice because mark sets are typically small (0-3 entries
per frame).

**Lazy allocation.** Most frames never carry marks. The `marks` field is a
slice initialized to `nil`. Only `with-continuation-mark` allocates backing
storage, on first use. The common case adds zero allocations.

**Unordered semantics.** Marks on a frame are a *set* keyed by `eq?`, not
an ordered list. `SetMark` overwrites in place when the key already exists;
`DeleteMark` uses swap-with-last and does not preserve insertion order.
Code should not depend on iteration order within a frame. (Ordering across
frames is well-defined: the chain walk produces innermost-first values.)

**Save/restore semantics.** When `SaveContinuation` fires, it **transfers**
`mc.marks` to the new frame: the slice header moves over (`q.marks = mc.marks`
in `NewMachineContinuationFromMachineContext`) and then `SaveContinuation`
nils `mc.marks`, so the callee starts clean (see the `marks` comment on
`vmState` in `machine/vm_state.go`). This is a move, not a copy: the backing
array is shared until `cloneMarks` duplicates it, which `Copy`, `Restore`, and
`RestoreAndRelease`'s shared branch all do, since a chain that may be
re-invoked cannot share mutable mark storage with the live context. On normal
return, `Restore`/`RestoreAndRelease` lifts the saved marks back. This is why a
tail-recursive loop with a mark does not accumulate entries: tail calls
skip `SaveContinuation` and just overwrite the current frame's entry.

**Tail-position detection.** The compiler must know whether the body of
`with-continuation-mark` is in tail position
(`CompileValidatedWithContinuationMark`, `machine/compilation/compile_validated.go`),
using the same tail-position tracking (`CompileTimeCallContext.inTail`) that
governs whether function calls are optimized. In tail position it emits a bare
`SetContMark` and compiles the body in tail: the mark lands on the current
frame, overwriting any prior value for that key, with no restore. Out of tail
position it brackets the body with `SaveContMark` / `RestoreContMark`, which
stash the key's previous value on the eval stack and put it back afterwards.
The *new frame* in the accumulating example comes from the enclosing non-tail
call's own `SaveContinuation`, not from the mark form.

## Summary

Continuation marks are per-frame key-value annotations on the continuation
chain. They solve a specific problem: attaching metadata to execution context
in a way that respects tail calls, survives continuation capture, and costs
nothing when unused. The mechanism is simple — a small slice on each frame, a
chain walk to collect — but the interaction with tail-call semantics is what makes
it genuinely useful rather than just a different spelling for global variables.
