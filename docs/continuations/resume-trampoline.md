# The Resume Trampoline

> **Prerequisites**: [Continuations (general concept)](concepts.md) and
> [Continuations in Wile](implementation.md). You should know what `call/cc`
> does at the Scheme level and that Wile keeps its continuation chain on the
> heap as a linked list of `MachineContinuation` frames, separate from the Go
> call stack. This document explains *how a captured continuation is put back
> to work* when you invoke it — the mechanism Wile calls the **resume
> trampoline**.

Here is a program that, until recently, crashed Wile when the race detector was
on:

```scheme
(define (ctak x y z)
  (call/cc (lambda (k) (ctak-aux k x y z))))
(define (ctak-aux k x y z)
  (if (not (< y x))
      (k z)
      (ctak-aux k
        (call/cc (lambda (k1) (ctak-aux k1 (- x 1) y z)))
        (call/cc (lambda (k2) (ctak-aux k2 (- y 1) z x)))
        (call/cc (lambda (k3) (ctak-aux k3 (- z 1) x y))))))
(ctak 18 12 6)   ; => 7
```

It returns `7`. But it gets there by capturing a continuation, invoking it,
capturing another *inside the resumed computation*, invoking that, and so on —
hundreds of nested resumes deep. The answer was always correct. The problem was
*how much Go stack it burned to compute it*. Under `-race` (which fattens every
Go frame) it overflowed and the process died. The CI `race` job was chronically
red because of exactly this.

The fix wasn't to grow the stack. It was to stop using the Go stack for resumes
at all. That fix is the trampoline.

## The Problem: One Go Frame Per Resume

Think about what "invoke a continuation" has to *do*. You captured a chain of
frozen frames — call it the segment. Invoking the continuation means: splice
that segment back onto the live computation, hand it the values you called it
with, and let it run to whatever it does next.

The obvious implementation runs the segment right there, on the spot:

```
applyCapturedContinuation(k, vals):
    sub := freshSubContext(k's segment, vals)
    result := sub.Run()              // <-- runs the whole resumed chain HERE
    abortToPrompt(DefaultPromptTag, result)
```

This works. It also has a fatal shape. `sub.Run()` is a Go function call. So
every time the resumed computation invokes *another* continuation — which is
exactly what `ctak` does — you nest another `sub.Run()` inside the current one.
The Go call stack grows one frame deep per resume:

```
RunResumable
  └─ applyCapturedContinuation → sub.Run()
       └─ applyCapturedContinuation → sub.Run()
            └─ applyCapturedContinuation → sub.Run()
                 └─ ... (one Go frame per resume; ctak goes hundreds deep)
```

The whole point of Wile's heap-based continuation chain was to *not* be limited
by the Go stack. Yet here, resume-heavy code reintroduced exactly that limit.

There was a second, quieter bug in the same code. Escaping *out* of a
`dynamic-wind` ran its after-thunk twice — once when `ReinstallSegment`
reconciled the winding, and again when the abort-to-prompt unwound past it. Two
mechanisms both thought they owned the unwind. We'll come back to why the
trampoline fixes this for free.

## The Key Insight: Don't Run It — Hand It Back

Here is the inversion. When you invoke a continuation, `applyCapturedContinuation`
does **not** run the segment. It packages the segment, the values, and a snapshot
of the current dynamic-wind state into a control signal and *returns* it:

```go
// applyCapturedContinuation, pkg/machine/captured_continuation.go
return p, &ErrResumeContinuation{
    Tag:           DefaultPromptTag,
    Segment:       capt.cc,            // carried UNRUN
    Values:        vals,              // already copied off the eval stack
    SourceWinding: p.windingStack.Copy(),
}
```

`ErrResumeContinuation` (`pkg/machine/prompt_abort.go`) implements Go's
`error` interface. That is the clever part: by being an error, it rides the VM's
existing `return err` plumbing all the way up — through however many nested Go
frames happen to be on the stack — to the **one** loop that knows how to deal
with it. No special unwinding path. `errors.As` finds it whether it's bare or
wrapped inside other errors.

That one loop is `RunResumable` (`pkg/machine/machine_context.go`), the
single driver under the default prompt. It catches the signal, reinstalls the
segment onto *its own* live chain, and loops:

```go
// RunResumable's resume arm, pkg/machine/machine_context.go (condensed)
var resumeErr *ErrResumeContinuation
if errors.As(err, &resumeErr) {
    boundary, _ := p.FindPrompt(resumeErr.Tag)
    wasEmpty, reErr := p.ReinstallSegment(
        resumeErr.Segment, boundary,
        resumeErr.SourceWinding, resumeErr.Values, true)
    ...
    continue                          // <-- the bounce
}
```

This is a **trampoline** in the precise CS sense: instead of one function
calling the next and growing the stack, each step *returns to a central loop*
that launches the next step. The resumed computation runs on the driver's own
`Run()` loop — the same loop that was already running — so it costs **O(1) Go
frames no matter how many times you resume**. `ctak` now bounces off
`RunResumable` hundreds of times without growing the Go stack one frame.

```
        ┌─────────────────────────┐
        │      RunResumable        │
        │   for { err = Run() }    │
        └───────────┬─────────────┘
                    │ Run() returns ErrResumeContinuation
                    ▼
          ReinstallSegment(seg, boundary, ...)
                    │ graft segment onto live chain
                    │ continue  ── bounce back up ──┐
                    └──────────────────────────────┘
              (Go stack depth never grows)
```

## How It Works: Anatomy of One Bounce

A single resume is four moves. Let's name the pieces, then walk them.

The actor is `ReinstallSegment` (`pkg/machine/machine_context_apply.go`).
It is the *one* resume primitive — both the abortive `call/cc` resume and
composable-continuation resume route through it. Its signature:

```go
func (p *MachineContext) ReinstallSegment(
    comp *ComposableContinuation,   // the captured segment
    boundary *MachineContinuation,  // where to graft it (nil = replace all)
    srcWinding WindingStack,        // winding live at the (k v) call site
    vals []values.Value,            // the resume values
    isolate bool,                   // restore captured marks vs. compose
) (bool, error)
```

**Move 1 — marks, then winding.** It installs the captured mark snapshot
(parameters, exception handlers) *before* reconciling dynamic-wind. Order is
load-bearing: dynamic-wind before/after thunks are arbitrary Scheme that may
read parameters, so the marks must already be in place when they run.

**Move 2 — acquire the segment.** `comp.AcquireSegment()`
(`pkg/machine/composable_continuation.go`) decides one-shot vs. multi-shot.
On the *first* invocation it returns the original frames and marks the chain
shared. On *re-invocation* it deep-copies, so a second resume of the same
continuation gets independent frames. (This is what makes the multi-shot
generator pattern work — more below.)

**Move 3 — reconcile dynamic-wind.** `RestoreWithWindingFrom` unwinds the
extents you're leaving and rewinds the ones you're entering, computed against
`srcWinding`. This is the *single* winding reconcile — there is no second one,
which is why escaping out of a `dynamic-wind` now fires its after-thunk exactly
once.

**Move 4 — graft and restore.** `GraftContinuation`
(`pkg/machine/machine_context_continuation.go`) walks to the bottom frame of
the segment and points its parent at `boundary`, splicing the segment onto the
live chain. Then `p.Restore(segment)` makes that the current state and
`SetValues(vals...)` drops the resume values into the value register. The driver
loops, and the resumed computation just... runs.

### The boundary: replace vs. extend

The `boundary` argument is where escape-past and delimited resume diverge — and
it's the whole reason a *full* `call/cc` continuation behaves differently from a
*delimited* one.

`boundary` comes from `FindPrompt(tag)`. For a top-level `call/cc`, there is no
inner prompt, so `FindPrompt` returns `nil`:

```
boundary == nil  →  graft onto nothing  →  REPLACE the whole live chain
```

That is the abortive case. A full continuation invocation *discards* whatever
the live computation was about to do (the consumer/exit/prompt frames currently
on the chain) and replaces it with the captured one. That's the "escape-past"
semantics: `(k z)` abandons the present and resumes the captured future.

When the tag *does* match an inner `call-with-continuation-prompt` frame:

```
boundary == prompt frame  →  graft onto it  →  EXTEND; segment's result
                                               flows through the prompt
```

The delimited segment runs and delivers its result to that prompt, which passes
it on. Same primitive, one pointer's difference.

## The Subtle Parts

Three things here trip people up. Each is a real bug the trampoline either fixed
or had to be careful not to reintroduce.

### Why `SourceWinding` is copied at the call site

Look back at the signal: it carries `p.windingStack.Copy()` taken *at the moment
`(k v)` is called*, not the driver's winding. Why?

Because the continuation may be invoked from a *deeper* sub-context than the
driver — inside a `force`/`delay` thunk, a parameter converter, a nested
`dynamic-wind` body. The frames between that call site and the driver have
dynamic-wind extents that must be unwound. If `ReinstallSegment` reconciled
against the *driver's* winding it would miss them, and a deeper after-thunk would
be skipped. Carrying the source winding forward is what lets the single reconcile
see the full picture. This is the "deeper-sub after-thunk" case, and it's why the
old double-reconcile design was *also* wrong in the other direction — it ran some
after-thunks twice and others not at all.

### `escalatorArm`: telling "resumed through" from "returned normally"

This is the trickiest interaction in the whole subsystem, and a green test suite
hid the bug for a while — twice, with two different wrong answers.

When an exception handler is installed for a *non-continuable* `raise`, Wile arms
a finalizer frame: if the handler returns normally (instead of escaping), that's
an error per R7RS §6.11 and a *secondary* exception must be escalated to the
outer handlers. But there's an exception to the exception: a `guard` whose
clauses miss re-raises via `raise-continuable`, which legitimately *resumes* the
handler's captured continuation *through* this finalizer frame. In that case the
value arrived by a resume, not a normal return — and must be forwarded, not
escalated.

How do you tell those two apart? They look identical at the frame: a value shows
up. The answer is a per-arm object, `escalatorArm`, minted when the escalator is
armed and stored *both* on the finalizer frame and in the escalator closure
(`pkg/machine/exception_raise.go`). `Copy` shares the pointer, so every copy the
capture machinery makes of that frame reports to the same flag:

```go
arm := &escalatorArm{}
escalateFn := func(finCC CallContext) error {
    ...
    if arm.revived {
        // a reinstatement re-entered this frame's extent FROM OUTSIDE:
        // the value came THROUGH a resume — forward it
        finMC.SetValues(vals...)
        return nil
    }
    // unrevived: handler returned naturally — escalate the secondary
    return finMC.raiseToHandlers(secondary, false, parent)
}
```

*Which* arms get the flag is the whole trick. The measured fact that makes it
non-obvious: in both the legitimate `guard` forward and the illegitimate
escape-inside-the-handler, the captured segment **contains** the finalizer frame.
Segment membership discriminates nothing. The two differ only in the **live
chain** at the `(k v)` site:

| | segment carries the frame | live chain carries it | verdict |
|---|---|---|---|
| `guard` miss re-raise | yes | **no** — guard-k's `call-with-exit` abort discarded it | revive → forward |
| `call/cc` escape inside the handler | yes | **yes** — the jump never left the extent | leave unrevived → escalate |

So `pendingEscalatorRevivals` (`machine_context_apply.go`) selects exactly the
arms the segment carries and the live chain does not. "Resumed through" means
"re-entered from outside", not "appears in the segment".

The set is decided **by the invoker**, alongside `SourceWinding` and for the same
reason: the resume trampolines to the nearest `DefaultPromptTag` driver, which is
not the context the continuation was invoked on. A raise handled inside a
sub-context — every `dynamic-wind` before/after thunk is one, and `RunWithinBoundary`
re-raises `ErrResumeContinuation` rather than resolving it — arms its finalizer
frame on the *sub's* chain, and the top driver's chain never held it. Asking the
driver reads "absent", i.e. a revival, for a jump that never left the extent, and
swallows the secondary exactly as the two earlier answers did. So
`applyCapturedContinuation` computes the set and carries it on
`ErrResumeContinuation.EscalatorRevivals`; `applyComposableContinuation`, which
resumes in place, computes it directly. `ReinstallSegment` only *applies* it, and
only *after* `RestoreWithWindingFrom` succeeds — a failed reinstatement never
re-entered the frame, so it contributes no revival.

> Two earlier answers were wrong, in opposite directions. A context-global
> `isolatedMarks` flag *stays true* after any prior resume on the driver, so once
> the program had resumed *any* continuation, every later non-continuable handler
> return silently swallowed its mandatory secondary exception. A per-driver
> `resumeGeneration` counter fixed that but was still too coarse in the other
> axis: it answers "did any resume happen in this window", so an ordinary
> `call/cc` escape *inside* the handler — which never leaves the finalizer's
> extent — also cleared the gate and converted the illegal return into a value.
> Attribution has to be per-arm and directional. A full green suite, `-race`, and
> the escape-past oracle missed both; only adversarial cross-checks caught them.

### One-shot vs. multi-shot

`AcquireSegment` marking the chain shared on first use, then deep-copying on
re-invocation, is what lets a single captured continuation be invoked many times
with independent results. Without the shared-marking, a normal return through a
reinstalled frame could pool an environment the captured segment still needs —
the "tail-frame-recycling-unsound" failure class, reverted three times in
Wile's history. Over-marking only forgoes pooling; it never corrupts. That
conservatism is deliberate.

## Seeing It In Action

The multi-shot pattern, traced:

```scheme
(define k #f)
(define count 0)
(define r (+ 1 (call/cc (lambda (c) (set! k c) 0))))
(set! count (+ count 1))
(if (< count 4) (k count))
(list 'r r 'count count)   ; => (r 4 count 4)
```

First pass: `call/cc` captures the continuation "add 1, store in `r`, bump
`count`, maybe loop" and saves it in `k`, returning `0`. So `r` becomes `1`,
`count` becomes `1`. Then `(k 1)` resumes — bounces off the driver,
`ReinstallSegment` deep-copies the segment (re-invocation) and delivers `1`, so
`r` becomes `2`, `count` becomes `2`. Again at `count=2`, `count=3`. At
`count=4` the `if` is false, no resume, and we read `r=4 count=4`. Every one of
those resumes was a trampoline bounce, not a nested Go call.

And the dynamic-wind single-fire:

```scheme
(define trace '())
(call/cc (lambda (k)
  (dynamic-wind
    (lambda () (set! trace (cons 'before trace)))
    (lambda () (set! trace (cons 'body trace)) (k 'out))
    (lambda () (set! trace (cons 'after trace))))))
(reverse trace)   ; => (before body after)
```

`after` appears exactly once. The single winding reconcile in `ReinstallSegment`
owns the unwind; nothing else fires it.

## What Would Break

Remove the trampoline and run the segment in a fresh sub-context per resume, and
`ctak 18 12 6` overflows the Go stack under `-race` — the original red CI. The
answer would still be `7` on the runs that didn't crash, which is exactly what
made this bug so durable: *correct output, wrong resource shape*.

Reconcile winding in two places instead of one, and escaping a `dynamic-wind`
fires its after-thunk twice (`(before body after after)`), corrupting any
cleanup that isn't idempotent.

Replace the per-arm `escalatorArm` with anything coarser — the context-global
`isolatedMarks` flag, or a per-driver resume counter — and the
secondary-exception test passes *in isolation* while the mandatory R7RS §6.11
exception is swallowed: by the flag for every non-continuable handler that
returns after any earlier resume, by the counter for every handler that takes a
`call/cc` escape inside its own body. Move the arm decision from the `(k v)` site
to the driver and the same hole reopens, narrower: only for a raise handled in a
sub-context, which is every `dynamic-wind` thunk. All three are silent
correctness holes that only surface in programs which both resume continuations
and misuse exception handlers.

The lesson the Wile memory records bluntly: for continuations, *a green suite is
not evidence of correctness*. The trampoline shipped only after an A/B
cross-check found the one regression that every other gate missed.

## See Also

- [Continuations in Wile](implementation.md) — the chain, `vmState`, capture
- [Prompt/Abort System](prompt-abort.md) — `ErrPromptAbort`, the driver, handler dispatch
- [Delimited Continuations](delimited.md) — prompts, composable continuations, the abort/compose model
  - [Continuation Marks](marks.md) — what the captured/isolated mark snapshots carry
