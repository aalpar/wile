# Continuations in Wile

> **Prerequisite**: [Continuations (general concept)](../learn/continuations.md). This document assumes you understand what a continuation is and how `call/cc` works at the Scheme level. Here we explain how Wile makes it happen inside a bytecode VM written in Go.

Picture this: Wile's VM is a `for` loop in Go, stepping through bytecode instructions one at a time. It has a program counter, a value register, an eval stack, and — most importantly — a continuation chain. When you call a function, the VM saves its current state as a link in this chain. When the function returns, it pops the link and resumes.

Now someone writes `(call/cc f)`. The Scheme program is asking: "give me everything the VM is currently waiting on — every saved frame, every pending return — as a value I can invoke later." How do you do that when the VM's state is just a Go struct with a linked list inside it?

## The Problem: Go's Stack Won't Help

In a naive interpreter, every Scheme function call would be a Go function call. The "continuation" would be the Go call stack itself. Capturing it would mean... copying the Go stack? Go doesn't let you do that.

Even if you could, it would be the wrong abstraction. Scheme continuations can be invoked multiple times from arbitrary points. They survive past the original function return. They compose with `dynamic-wind`. The Go call stack is ephemeral and single-use.

So Wile doesn't use the Go call stack for Scheme control flow at all. It manages its own continuation chain, entirely on the heap.

## The Key Structures

Three types form the core of the system. Each one has a distinct role.

### vmState: The Frozen Moment

Every saveable VM state shares the same shape, defined in `machine/vm_state.go:63-87`:

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
    callDepth    uint64
}
```

This is everything the VM needs to resume execution from a given point: which function it's in (`template`), where in that function (`pc`), what variables are in scope (`env`), what intermediate values are on the eval stack (`evals`), and what dynamic-wind extent is active (`windingStack`).

Both `MachineContext` (the running VM) and `MachineContinuation` (a saved frame) embed `vmState`. They're the same shape because saving a continuation is literally: copy these fields into a new struct and link it.

### MachineContinuation: A Linked List of Saved Frames

Defined in `machine/machine_continuation.go:25-30`:

```go
type MachineContinuation struct {
    vmState
    parent        *MachineContinuation
    promptHandler *MachineClosure
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

The VM loop (`machine_context.go:567`) steps through instructions. Two opcodes manage the chain:

## Save and Restore: The Calling Convention

When the compiler encounters a non-tail call like `(f (+ 1 2))`, it emits `OpSaveContinuation` before the call and expects `OpRestoreContinuation` after the callee finishes.

**OpSaveContinuation** (`machine_context.go:655-660`): Takes the current `vmState` — the program counter, environment, eval stack, everything — packages it into a new `MachineContinuation`, and pushes it onto the chain. The offset argument tells it where to resume: "when this frame is restored, set `pc` to here."

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

**OpRestoreContinuation** (`machine_context.go:637-641`): Pops the top frame off the chain and overwrites the current `vmState` with its contents. The VM is now back where it was before the call, with the callee's result sitting in the value register.

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

The implementation lives in `registry/core/prim_control.go:115-178`. Here's the sequence:

**1. Capture the continuation chain.** `SliceContinuationAt(nil)` (`machine_context.go:1129-1146`) deep-copies every frame from `mc.cont` down to the bottom. Each frame is individually copied so that future mutations to the live chain don't affect the captured one.

```go
segment := mc.SliceContinuationAt(nil)
windingStack := mc.WindingStack().Copy()
cc := machine.NewComposableContinuation(segment, windingStack, mc.ThreadID(), mc.BarrierValid())
```

**2. Build the escape closure.** This is a Go function wrapped as a Scheme closure. When invoked with a value `v`, it:
- Checks the thread ID (no cross-thread jumps)
- Checks the barrier token (no crossing `with-continuation-barrier`)
- Creates a sub-context, grafts the copied chain onto it, runs the restored frames to completion
- Aborts to `DefaultPromptTag` with the result

The model follows Racket's unification: `call/cc` is defined in terms of composable continuations plus abort:

```
(call/cc f) ≡
  (call-with-composable-continuation
    (lambda (k)
      (f (lambda (v) (abort-current-continuation default-prompt-tag (k v)))))
    default-prompt-tag)
```

**3. Two execution modes.** `PrimCallCC` has a critical branch at `prim_control.go:132`:

- **Inline mode** (`mc.Parent() != nil`): The lambda runs directly in the current VM context via `mc.Apply()`. This preserves the full continuation chain — crucial for coroutines where multiple continuations interact with the same call stack.
- **Sub-context mode** (`mc.Parent() == nil`): The lambda runs in an isolated sub-context. Used when `call/cc` is itself inside a foreign function's sub-context (e.g., inside `apply`).

## The Escape Path: ErrPromptAbort

When the escape closure fires, it doesn't just "return" — it needs to abandon whatever computation is currently running and jump back to a known boundary. Wile uses Go's error propagation for this.

The escape closure returns an `ErrPromptAbort` error targeting `DefaultPromptTag`. This error propagates up through the Go call stack (through `Run()`, through any `OperationForeignFunctionCall` wrappers) until it hits `RunWithEscapeHandling` (`machine_context.go:1227`).

`RunWithEscapeHandling` is the outermost execution loop. It installs `DefaultPromptTag` as the context-level prompt, runs the VM, and catches any `ErrPromptAbort`:

```go
func (p *MachineContext) RunWithEscapeHandling() error {
    p.promptTag = DefaultPromptTag
    for {
        err := p.Run()
        if err == nil {
            // ... normal completion
            return nil
        }
        var abortErr *ErrPromptAbort
        if errors.As(err, &abortErr) {
            // Unwind dynamic-wind, restore to prompt, invoke handler...
        }
    }
}
```

This is the same mechanism used by delimited continuations (`abort-current-continuation`). There's no separate "escape continuation" path — `call/cc` reuses the composable-continuation-plus-abort infrastructure.

## The Subtle Parts

### Shared Frames and the Copy Problem

When `call/cc` captures a continuation, it calls `MarkChainShared()` on the live chain (`machine_continuation.go:184-191`). This sets `shared = true` on every frame. Why?

Normally, when a function returns, `RestoreAndRelease` destructively transfers the frame's eval stack to the VM and pools the frame for reuse. But if someone captured this frame via `call/cc`, they might re-invoke it later. Destroying the eval stack would corrupt the captured continuation.

Shared frames use the safe path: `evals.Copy()` instead of transfer, and the frame is left for GC instead of pooled. This is the performance cost of `call/cc` — even on the normal return path, shared frames pay for a stack copy.

### Dynamic-Wind Integration

When a continuation is invoked, the VM can't just slam in the new state — it has to respect `dynamic-wind` contracts. `RestoreWithWindingFrom` (`machine_context.go:1075-1103`) compares the current winding stack with the target:

1. Calls "after" thunks for frames being exited (innermost first)
2. Calls "before" thunks for frames being entered (outermost first)

This happens transparently whenever a continuation crosses a dynamic-wind boundary, whether via `call/cc` escape or composable continuation invocation.

### The Sub-Context Architecture

Foreign functions (Go primitives) that need to call Scheme closures create sub-contexts via `NewSubContext()`. Sub-contexts have their own call stacks (`cont = nil`) but share the global environment. This matters for continuations because:

- A continuation captured in a sub-context only captures frames up to the sub-context boundary — not the parent's frames.
- The `parentMC` pointer lets `call/cc` detect whether inline mode is safe.
- Cross-context continuation jumps are mediated by `ErrPromptAbort`, not by direct frame manipulation.

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
6. Later, `(saved 42)` calls the escape closure with `42`. The closure creates a sub-context, grafts the copied chain onto it, and runs the restored frames — which resume at the `+` application with `1` on the eval stack and `42` in the value register. `(+ 1 42)` evaluates to `43`. The result aborts to `DefaultPromptTag` and is returned.

## What Would Break

Remove the continuation chain and use Go's call stack instead. What happens?

- **`call/cc` becomes impossible.** You can't snapshot Go's call stack. You'd need `setjmp/longjmp` (C, not Go) or coroutine support (not in Go's runtime model for user code).
- **Tail-call optimization disappears.** TCO in Wile works by not emitting `SaveContinuation` for tail calls. Without an explicit chain, every call would grow the Go stack, and `(let loop () (loop))` would eventually overflow.
- **`dynamic-wind` can't unwind across continuations.** The winding stack is saved per-frame. Without explicit frames, there's nowhere to put it.
- **Sub-context isolation breaks.** The parent/child relationship between contexts enables safe foreign function calls into Scheme. With a single Go stack, a Go function calling a Scheme closure would be trapped in the middle of the stack — invisible to continuation operations.

The explicit continuation chain is not an optimization or a convenience. It's the mechanism that makes Scheme's control operators possible inside a language (Go) that doesn't have them.
