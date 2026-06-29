# Continuations in Wile

> **Prerequisite**: [Continuations (general concept)](concepts.md). This document assumes you understand what a continuation is and how `call/cc` works at the Scheme level. Here we explain how Wile makes it happen inside a bytecode VM written in Go.

Picture this: Wile's VM is a `for` loop in Go, stepping through bytecode instructions one at a time. It has a program counter, a value register, an eval stack, and — most importantly — a continuation chain. When you call a function, the VM saves its current state as a link in this chain. When the function returns, it pops the link and resumes.

Now someone writes `(call/cc f)`. The Scheme program is asking: "give me everything the VM is currently waiting on — every saved frame, every pending return — as a value I can invoke later." How do you do that when the VM's state is just a Go struct with a linked list inside it?

## The Problem: Go's Stack Won't Help

In a naive interpreter, every Scheme function call would be a Go function call. The "continuation" would be the Go call stack itself. Capturing it would mean... copying the Go stack? Go doesn't let you do that.

Even if you could, it would be the wrong abstraction. Scheme continuations can be invoked multiple times from arbitrary points. They survive past the original function return. They compose with `dynamic-wind`. The Go call stack is ephemeral and single-use.

So Wile doesn't use the Go call stack for Scheme control flow at all. It manages its own continuation chain, entirely on the heap.

## The Key Structures

Three types form the core of the system. Each one has a distinct role.

### vmState: The Frozen Moment

Every saveable VM state shares the same shape, defined as `vmState` in `machine/vm_state.go`. The load-bearing fields:

```go
type vmState struct {
    env          *environment.EnvironmentFrame
    template     *NativeTemplate
    singleValue  values.Value
    multiValues  MultipleValues
    evals        *Stack
    pc           int
    windingStack WindingStack
    promptTag    *PromptTag
    threadID     uint64
    callDepth    int
    envPooled    bool
    marks        []markEntry
}
```

`singleValue` and `multiValues` form a split value register (single-value fast path plus R7RS `values` slow path); `envPooled` is a release flag for the environment-frame pool; `marks` carries continuation-mark entries (see [`marks.md`](marks.md)).

This is everything the VM needs to resume execution from a given point: which function it's in (`template`), where in that function (`pc`), what variables are in scope (`env`), what intermediate values are on the eval stack (`evals`), and what dynamic-wind extent is active (`windingStack`).

Both `MachineContext` (the running VM) and `MachineContinuation` (a saved frame) embed `vmState`. They're the same shape because saving a continuation is literally: copy these fields into a new struct and link it.

### MachineContinuation: A Linked List of Saved Frames

Defined as `MachineContinuation` in `machine/machine_continuation.go`:

```go
type MachineContinuation struct {
    vmState
    parent        *MachineContinuation
    promptHandler Closure
    shared        bool
}
```

Each frame points to its parent. The chain reads bottom-up: the root frame (parent == nil) is the oldest pending return; the top of the chain is the most recent. When a function returns, the VM pops the top frame and restores its state. When `call/cc` captures a continuation, it copies this entire chain.

The `shared` flag matters for performance: once a frame has been captured by `call/cc`, it's marked shared, meaning the VM can't destructively reclaim it on return — someone might re-invoke it later.

### MachineContext: The Live VM

`MachineContext` also embeds `vmState`, plus the continuation chain pointer (`cont`), a parent pointer (for sub-contexts), and other runtime state. The relationship is:

```
MachineContext (live VM state)
    │
    ├── vmState (current frame's state: env, pc, template, evals...)
    │
    └── cont ──→ MachineContinuation (frame N)
                      │
                      └── parent ──→ MachineContinuation (frame N-1)
                                          │
                                          └── parent ──→ ... ──→ nil
```

The VM loop (`MachineContext.Run`) steps through instructions. Two opcodes manage the chain:

## Save and Restore: The Calling Convention

When the compiler encounters a non-tail call like `(f (+ 1 2))`, it emits `OpSaveContinuation` before the call and expects `OpRestoreContinuation` after the callee finishes.

**OpSaveContinuation**: Takes the current `vmState` — the program counter, environment, eval stack, everything — packages it into a new `MachineContinuation`, and pushes it onto the chain. The offset argument tells it where to resume: "when this frame is restored, set `pc` to here."

```
before SaveContinuation:              after SaveContinuation:

MachineContext                        MachineContext
├── pc: 5                             ├── pc: 6 (advanced past save)
├── env: E1                           ├── env: E1
├── evals: [a, b]                     ├── evals: []  (new stack)
└── cont ──→ (frame 0)                └── cont ──→ (frame 1: pc=8, env=E1, evals=[a,b])
                                                        │
                                                        └── parent ──→ (frame 0)
```

**OpRestoreContinuation**: Pops the top frame off the chain and overwrites the current `vmState` with its contents. The VM is now back where it was before the call, with the callee's result sitting in the value register.

```go
case OpRestoreContinuation:
    if mc.cont == nil {
        return nil       // No more frames — execution is complete
    }
    mc.RestoreAndRelease(mc.cont)
```

If `cont` is nil, there's nothing to return to — execution is done.

**Tail calls** skip `SaveContinuation` entirely. The callee's `RestoreContinuation` pops the *caller's* caller's frame directly, which is exactly tail-call optimization: no frame growth for tail-position calls.

## How call/cc Works

The implementation lives in `PrimCallCC` in `registry/core/prim_control.go`. Here's the sequence:

**1. Capture the continuation chain.** `SliceContinuationAt` deep-copies every frame from `mc.cont` down to the nearest `DefaultPromptTag`. Each frame is individually copied so that future mutations to the live chain don't affect the captured one.

```go
capturePrompt, _ := mc.FindPrompt(machine.DefaultPromptTag)
segment := mc.SliceContinuationAt(capturePrompt)
windingStack := mc.WindingStack().Copy()
comp := machine.NewComposableContinuation(segment, windingStack, mc.ThreadID(), mc.BarrierValid())
mc.SnapshotReachableMarksInto(comp)            // restore outer marks on resume
capt := machine.NewCapturedContinuation(comp, mc.ThreadID(), mc.BarrierValid())
```

The capture is **delimited**, not absolute. `FindPrompt(DefaultPromptTag)` returns `(nil, true)` at the top-level context boundary — `SliceContinuationAt(nil)` then grabs the whole chain — or a chain *frame* when the `call/cc` sits inside a `call-with-continuation-prompt` reusing the default tag, in which case only the segment down to that prompt is captured. Capturing more would loop forever: the chain above the prompt includes the re-invocation site itself.

**2. Build the captured continuation value.** `call/cc` does not return a Go closure. It returns a `CapturedContinuation` (`machine/captured_continuation.go`) — a value that is both *callable* (invoking it resumes the captured point) and *introspectable* (`continuation-marks` can read its chain). The escape logic — thread-ID check, barrier check, and the resume itself — lives in `applyCapturedContinuation`, not inside a closure.

When the value is invoked with `v`, `applyCapturedContinuation` checks the thread ID (no cross-thread jumps) and barrier token (no crossing `with-continuation-barrier`), then **returns the segment unrun** as an `ErrResumeContinuation` control signal. It does *not* run the captured chain on the spot. The nearest `DefaultPromptTag` driver reinstalls it — the [resume trampoline](resume-trampoline.md).

The semantic model still follows Racket's unification: `call/cc` is composable-continuation capture plus abort —

```
(call/cc f) ≡
  (call-with-composable-continuation
    (lambda (k)
      (f (lambda (v) (abort-current-continuation default-prompt-tag (k v)))))
    default-prompt-tag)
```

— but read this as the *meaning*, not the runtime path. The implementation reinstalls the captured segment directly via `ReinstallSegment` (with `boundary == nil`, the abortive "replace the whole chain" case) rather than literally raising an abort after running it.

**3. Two execution modes.** `PrimCallCC` has a critical branch on `mc.Parent() != nil`:

- **Inline mode** (`mc.Parent() != nil`): The lambda runs directly in the current VM context via `mc.ApplyCallable()`. This preserves the full continuation chain — crucial for coroutines where multiple continuations interact with the same call stack.
- **Sub-context mode** (`mc.Parent() == nil`): The lambda runs in an isolated sub-context. Used when `call/cc` is itself inside a foreign function's sub-context (e.g., inside `apply`).

## The Escape Path: Two Control Signals, One Driver

When a continuation is invoked, it doesn't just "return" — it abandons whatever computation is running and jumps to a known boundary. Wile uses Go's error propagation for this: the invocation returns an `error`-typed control signal that rides the `return err` plumbing up through `Run()` and any foreign-call wrappers until it reaches the driver loop. Because the signal is an `error`, `errors.As` finds it whether it arrives bare or wrapped inside other errors.

There are **two** such signals, and they mean different things:

- **`ErrResumeContinuation`** — a `call/cc`-captured continuation was invoked. It carries the captured segment *unrun*. The driver reinstalls it onto the live chain and keeps looping (the [resume trampoline](resume-trampoline.md)). This is the resume path.
- **`ErrPromptAbort`** — `abort-current-continuation` was called (directly, or as the abort half of a value delivery). It carries values, not a segment. The driver reconciles winding, restores past the matching prompt, and runs its handler or delivers the values. See [prompt/abort details](prompt-abort.md).

Both are caught by the same loop. `RunWithEscapeHandling` is a thin entry point that delegates to `RunResumable`, the single driver under `DefaultPromptTag`:

```go
func (p *MachineContext) RunResumable() error {
    p.promptTag = DefaultPromptTag
    for {
        err := p.Run()
        if err == nil { /* unwind remaining dynamic-wind, return */ }

        var abortErr *ErrPromptAbort
        if errors.As(err, &abortErr) {
            // reconcile winding, restore past the prompt, run handler / deliver
            continue
        }

        var resumeErr *ErrResumeContinuation
        if errors.As(err, &resumeErr) {
            boundary, _ := p.FindPrompt(resumeErr.Tag)
            p.ReinstallSegment(resumeErr.Segment, boundary,
                resumeErr.SourceWinding, resumeErr.Values, true)
            continue                     // the trampoline bounce
        }
        // ... timer interrupts, then real errors fall through
    }
}
```

So `call/cc` is still *unified* with delimited continuations — both resume through the one shared `ReinstallSegment` primitive — but the resume and the abort are now distinct signals rather than a single abort-after-running. That split is what makes the trampoline possible: returning the segment unrun (instead of running it and aborting the result) is what keeps deep resumes at O(1) Go frames.

## The Subtle Parts

### Shared Frames and the Copy Problem

When `call/cc` captures a continuation, it calls `SliceContinuationAt` which deep-copies every frame via `Copy()` per frame. The copy is what gets stored in the `ComposableContinuation` — the live chain is not marked or mutated by `call/cc` directly.

`MarkChainShared()` is called by `CurrentContinuation()` and by `ComposableContinuation.AcquireSegment()`, not by `call/cc`. It sets `shared = true` on every frame in the live chain when those paths are used.

Normally, when a function returns, `RestoreAndRelease` destructively transfers the frame's eval stack to the VM and pools the frame for reuse. But if a frame has been marked shared, it might be re-invoked later. Destroying the eval stack would corrupt the captured continuation.

Shared frames use the safe path: `evals.Copy()` instead of transfer, and the frame is left for GC instead of pooled. This is the performance cost of shared frames — even on the normal return path, they pay for a stack copy.

### Dynamic-Wind Integration

When a continuation is invoked, the VM can't just slam in the new state — it has to respect `dynamic-wind` contracts. `RestoreWithWindingFrom` compares the current winding stack with the target:

1. Calls "after" thunks for frames being exited (innermost first)
2. Calls "before" thunks for frames being entered (outermost first)

This happens transparently whenever a continuation crosses a dynamic-wind boundary, whether via `call/cc` escape or composable continuation invocation.

### The Sub-Context Architecture

Foreign functions (Go primitives) that need to call Scheme closures create sub-contexts via `NewSubContext()`. Sub-contexts have their own call stacks (`cont = nil`) but share the global environment. This matters for continuations because:

- A continuation captured in a sub-context only captures frames up to the sub-context boundary — not the parent's frames.
- The `parentMC` pointer lets `call/cc` detect whether inline mode is safe.
- Cross-context continuation jumps are mediated by control signals (`ErrResumeContinuation` for a `call/cc` resume, `ErrPromptAbort` for a value-delivery abort), not by direct frame manipulation.

## Seeing It In Action

Consider this Scheme program:

```scheme
(define saved #f)
(+ 1 (call/cc (lambda (k) (set! saved k) 10)))
; => 11
(saved 42)
; => 43
```

Here's what happens inside the VM:

1. `(+ 1 ...)` compiles to: push `1`, then evaluate the `call/cc` expression, then apply `+`.
2. Before the `call/cc` call, `SaveContinuation` saves a frame: "resume at the `+` application, with `1` on the eval stack."
3. `PrimCallCC` fires. It deep-copies the continuation chain (which includes the frame from step 2). It builds an escape closure wrapping this copy.
4. The lambda `(lambda (k) (set! saved k) 10)` runs. It stashes `k` (the escape closure) in `saved` and returns `10`.
5. `10` flows back through `RestoreContinuation`, the saved frame is popped, `(+ 1 10)` evaluates to `11`.
6. Later, `(saved 42)` invokes the continuation with `42`. Rather than running the captured chain on the spot, it returns it *unrun* as an `ErrResumeContinuation` control signal. The nearest `DefaultPromptTag` driver (`RunResumable`) catches the signal, grafts the captured chain onto its own live continuation, puts `42` in the value register, and keeps looping — resuming at the `+` application with `1` on the eval stack. `(+ 1 42)` evaluates to `43`, which is returned.

> Step 6 is the **resume trampoline**. An earlier design ran the captured chain in a fresh sub-context and aborted the result back to `DefaultPromptTag` — which cost one Go stack frame *per resume* (deep `call/cc` programs like `ctak` overflowed the Go stack under `-race`) and reconciled `dynamic-wind` winding twice. Returning the segment unrun and letting the single driver reinstall it onto itself runs every resume on the one `Run()` loop: O(1) Go frames, one winding reconcile. See [The Resume Trampoline](resume-trampoline.md) for the full mechanism.

## What Would Break

Remove the continuation chain and use Go's call stack instead. What happens?

- **`call/cc` becomes impossible.** You can't snapshot Go's call stack. You'd need `setjmp/longjmp` (C, not Go) or coroutine support (not in Go's runtime model for user code).
- **Tail-call optimization disappears.** TCO in Wile works by not emitting `SaveContinuation` for tail calls. Without an explicit chain, every call would grow the Go stack, and `(let loop () (loop))` would eventually overflow.
- **`dynamic-wind` can't unwind across continuations.** The winding stack is saved per-frame. Without explicit frames, there's nowhere to put it.
- **Sub-context isolation breaks.** The parent/child relationship between contexts enables safe foreign function calls into Scheme. With a single Go stack, a Go function calling a Scheme closure would be trapped in the middle of the stack — invisible to continuation operations.

The explicit continuation chain is not an optimization or a convenience. It's the mechanism that makes Scheme's control operators possible inside a language (Go) that doesn't have them.
