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

Any Go primitive that calls a Scheme closure in a loop creates a "sub-context boundary" that truncates continuations. This affected `map`, `for-each`, `apply` (when the applied procedure is called in a sub-context), and `call-with-values`.

## The Solution: Two Complementary Changes

### 1. Rewrite map/for-each in Scheme

The direct fix: make the iteration itself consist of Scheme continuation frames.

```scheme
(define map
  (case-lambda
    ((f lst)
     (let loop ((lst lst))
       (if (null? lst) '()
           (cons (f (car lst)) (loop (cdr lst))))))
    ((f lst . lsts)
     (let loop ((all (cons lst lsts)))
       (if (let any-null? ((ls all))
             (if (null? ls) #f
                 (if (null? (car ls)) #t
                     (any-null? (cdr ls)))))
           '()
           (cons (apply f (map car all))
                 (loop (map cdr all))))))))
```

Now the named-let `loop` compiles to tail-recursive Scheme frames. When `call/cc` captures inside `(f (car lst))`, the continuation chain includes the `cons`, the recursive `loop` call, and the full outer computation. Re-entering the continuation resumes the iteration naturally.

The multi-list case uses `any-null?` (a named-let helper) to check if any input list is exhausted, matching R7RS's behavior of stopping at the shortest list.

### 2. Add delimited continuations

Delimited continuations (Racket-style prompts) provide a general mechanism for controlling how much of the continuation is captured. While the immediate motivation was `map`/`for-each`, delimited continuations are independently useful for implementing generators, coroutines, effect handlers, and other control patterns.

## Architecture

### Types

```
PromptTag                    Opaque identity (pointer equality, atomic ID)
ComposableContinuation       Callable value: continuation segment + winding stack
ErrPromptAbort               Error type for abort propagation
```

### Where prompts live

A prompt can be either:

1. **On the continuation chain**: a `MachineContinuation` frame with a non-nil `promptTag` field.
2. **On the MachineContext**: the `promptTag` field of a sub-context, set by `call-with-continuation-prompt`.

`FindPrompt` checks both locations. The context-level prompt exists because `call-with-continuation-prompt` runs its thunk in a sub-context (for error isolation), and the sub-context's continuation chain starts empty. The prompt tag on the context marks the sub-context boundary.

### Error propagation

`ErrPromptAbort` propagates through the call stack:

```
Scheme code calls abort-current-continuation (or call/cc escape closure)
    │
    ▼
Returns ErrPromptAbort
    │
    ▼
OperationForeignFunctionCall sees ErrPromptAbort, propagates it
    │                         (does NOT wrap in ErrExceptionEscape)
    ▼
Run() returns ErrPromptAbort to caller
    │
    ▼
Caught by either:
  ┌─ PrimCallWithContinuationPrompt (sub-context, user prompt tag)
  ├─ PrimCallCC sub-context mode    (call/cc escapes to DefaultPromptTag)
  └─ RunWithEscapeHandling          (top-level, all aborts via FindPrompt)
```

The key detail in `OperationForeignFunctionCall`: `ErrPromptAbort` is checked before wrapping errors as exceptions:

```go
// Priority order for error handling:
1. Panic recovery (deferred)  — Go panics from values.Number arithmetic
2. ErrPromptAbort             — prompt aborts and call/cc escapes
3. ErrExceptionEscape         — Scheme exceptions (propagated as-is)
4. Any other error            — wrapped in ErrExceptionEscape
```

Without the `ErrPromptAbort` check, the abort would be wrapped in a Scheme exception and the prompt handler would never see it.

## How Each Primitive Works

### call-with-continuation-prompt

```scheme
(call-with-continuation-prompt thunk tag handler)
```

Implementation in Go (`PrimCallWithContinuationPrompt`):

1. Create a sub-context. Set its `promptTag` to `tag`.
2. Apply `thunk` in the sub-context. Run it.
3. If the thunk returns normally, propagate its value.
4. If `ErrPromptAbort` is caught and the tag matches:
   a. Unwind the sub-context's winding stack down to the parent's depth (call after thunks for any `dynamic-wind` extents entered inside the prompt).
   b. Run the handler with the abort values (in a fresh sub-context).
5. If the tag doesn't match, propagate the abort upward.

### abort-current-continuation

```scheme
(abort-current-continuation tag v ...)
```

Simply returns `&ErrPromptAbort{Tag: tag, Values: vs}`. The error propagates up through the VM until a matching prompt catches it.

### call-with-composable-continuation

```scheme
(call-with-composable-continuation proc tag)
```

This is the most complex primitive. Implementation:

1. `FindPrompt(tag)` — walk the continuation chain and check the context-level prompt.
2. `SliceContinuationAt(prompt)` — deep-copy the continuation frames from the current position down to (but not including) the prompt. The bottom frame's parent is set to nil, creating a standalone segment.
3. Create a `ComposableContinuation` wrapping the segment and a copy of the current winding stack.
4. Run `proc` with the composable continuation in a sub-context.
5. After `proc` returns, abort to the prompt with `proc`'s result. This is critical: the abort skips past the captured frames in the current context, delivering the result directly to the prompt boundary.

Step 5 is what distinguishes composable continuations from undelimited ones. Without the abort, the result would flow through the captured computation a second time (the frames exist both in the captured segment and in the current continuation chain).

### Applying a composable continuation

When a `ComposableContinuation` is called as a procedure (dispatched by `ApplyCallable`):

1. Thread check: reject if invoked from a different SRFI-18 thread.
2. Deep-copy the segment for safe re-invocation.
3. Graft the segment's bottom frame onto the current continuation chain.
4. Handle dynamic-wind: `RestoreWithWindingFrom(nil, current, captured)` unwinds extents not in the captured stack and rewinds captured extents not in the current stack.
5. Restore from the segment's top frame and set the argument as the value.

The deep copy in step 2 is essential. Without it, re-invoking the composable continuation would corrupt the continuation chain (the frames are mutable and get modified during execution).

## Interaction with dynamic-wind

### The winding stack model

Each `DynamicWindFrame` has an atomic ID. The winding stack is a slice of frame pointers, outermost at index 0. `FindCommonWindingPrefix` compares two stacks by ID to find where they diverge.

When transitioning between dynamic extents (whether from `call/cc`, abort, or composable continuation application), `RestoreWithWindingFrom` runs:

1. After thunks from innermost to the common ancestor (unwinding).
2. Before thunks from the common ancestor to the target (rewinding).

### Abort + dynamic-wind

When an abort crosses a `dynamic-wind` boundary, the after thunks must run. This is handled in `PrimCallWithContinuationPrompt`: after catching the abort, it calls `sub.UnwindTo(mc.WindingStack().Depth())` to run after thunks for any dynamic-wind extents that were active inside the prompt.

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

All continuation control flow — both `call/cc` escapes and delimited continuations — uses `ErrPromptAbort` as the single error propagation mechanism. Call/cc uses the composable-continuation-then-abort model: the escape closure applies a composable continuation in a sub-context, then aborts to `DefaultPromptTag` with the result. This follows Racket's model where `call/cc` is defined in terms of composable continuations and abort.

```
┌──────────────────┬─────────────────────────┬────────────────────────────┐
│                  │ call/cc escape           │ Prompt abort               │
├──────────────────┼─────────────────────────┼────────────────────────────┤
│ Error type       │ ErrPromptAbort           │ ErrPromptAbort             │
│                  │ (to DefaultPromptTag)    │ (to user tag)              │
├──────────────────┼─────────────────────────┼────────────────────────────┤
│ Carries          │ Result value (from       │ Prompt tag + values        │
│                  │ composable cont. run)    │                            │
├──────────────────┼─────────────────────────┼────────────────────────────┤
│ Caught by        │ PrimCallCC sub-context   │ PrimCallWithContinuation   │
│                  │ or RunWithEscapeHandling │   Prompt (sub-context)     │
│                  │ (unified FindPrompt)     │ or RunWithEscapeHandling   │
│                  │                         │   (top-level)              │
├──────────────────┼─────────────────────────┼────────────────────────────┤
│ Effect           │ Replaces current         │ Unwinds to prompt,         │
│                  │ continuation             │ invokes handler             │
├──────────────────┼─────────────────────────┼────────────────────────────┤
│ Composable       │ No (aborts computation)  │ Yes (via ComposableCont.)  │
└──────────────────┴─────────────────────────┴────────────────────────────┘
```

`ComposableContinuation` is the qualitatively new capability. Unlike `call/cc` escapes which discard the current computation, composable continuations splice frames in and resume normally. This enables patterns like:

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
;; prompt returns 7, outer + 10 = 17
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
| `machine/composable_continuation.go` | `ComposableContinuation` callable value |
| `machine/prompt_abort.go` | `ErrPromptAbort` error type |
| `machine/dynamic_wind.go` | `DynamicWindFrame`, `WindingStack`, `FindCommonWindingPrefix` |
| `machine/machine_continuation.go` | `promptTag`/`promptHandler` fields, `DeepCopy()` |
| `machine/barrier_token.go` | `BarrierToken` opaque barrier identity |
| `machine/machine_context.go` | `FindPrompt`, `SliceContinuationAt`, `GraftContinuation`, `RestoreWithWindingFrom`, `RunWithEscapeHandling`, `applyComposableContinuation` |
| `machine/operation_foreign_function_call.go` | `ErrPromptAbort` passthrough |
| `registry/core/prim_prompt.go` | Prompt primitive implementations |
| `registry/core/prim_control.go` | `PrimCallCC`, `newComposeAbortEscapeClosure` |
| `registry/core/prompts.go` | Primitive registration |
| `registry/core/bootstrap.go` | Scheme `map`/`for-each` definitions |

## References

- Flatt, Yu, Findler, Felleisen. "Adding Delimited and Composable Control to a Production Programming Environment." ICFP 2007. The Racket model this implementation follows.
- Felleisen. "The Theory and Practice of First-Class Prompts." POPL 1988. Original formalization of prompts and aborts.
- Danvy, Filinski. "Abstracting Control." LFP 1990. Shift/reset as the composable variant.
- R7RS section 6.10 (dynamic-wind, call/cc).
