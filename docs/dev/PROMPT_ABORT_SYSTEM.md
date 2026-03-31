# Prompt/Abort System — Implementation Details

This document describes the implementation of the prompt/abort system,
including error propagation, handler dispatch, dynamic-wind integration,
and the unification of `call/cc` escapes with delimited continuations.

For design rationale, see `docs/design/DELIMITED_CONTINUATIONS.md` and
`docs/design/CONTINUATION_ESCAPE_DESIGN.md`.

## Error propagation path

All continuation control flow uses `ErrPromptAbort` as the single
propagation mechanism. The error carries a `*PromptTag` and a
`[]values.Value` payload.

```
                     ┌─────────────────────────────────┐
                     │ Source of ErrPromptAbort         │
                     │                                 │
                     │ • PrimAbortCurrentContinuation   │
                     │   (user abort to user tag)       │
                     │                                 │
                     │ • call/cc escape closure         │
                     │   (abort to DefaultPromptTag     │
                     │    with result value)            │
                     └───────────────┬─────────────────┘
                                     │
                                     ▼
                     ┌─────────────────────────────────┐
                     │ OperationForeignFunctionCall     │
                     │ (machine/operation_foreign_      │
                     │  function_call.go)               │
                     │                                 │
                     │ errors.As(err, &abortErr)?       │
                     │   YES → return nil, err          │
                     │         (pass through, do NOT    │
                     │          wrap as exception)      │
                     └───────────────┬─────────────────┘
                                     │
                                     ▼
                     ┌─────────────────────────────────┐
                     │ Run() returns err to caller      │
                     └───────────────┬─────────────────┘
                                     │
               ┌─────────────────────┼─────────────────────┐
               ▼                     ▼                     ▼
   ┌───────────────────┐ ┌──────────────────┐ ┌───────────────────────┐
   │ PrimCallWith       │ │ PrimCallCC       │ │ RunWithEscapeHandling │
   │ ContinuationPrompt │ │ (sub-context)    │ │ (top-level loop)      │
   │                    │ │                  │ │                       │
   │ Tag match?         │ │ ErrPromptAbort   │ │ FindPrompt(tag)       │
   │ YES → unwind,      │ │ to Default tag?  │ │ RestoreWithWinding    │
   │   invoke handler   │ │ YES → extract    │ │ Restore(prompt)       │
   │ NO → propagate     │ │   value, return  │ │ handler → Apply       │
   └───────────────────┘ └──────────────────┘ └───────────────────────┘
```

### Error priority in OperationForeignFunctionCall

The error handling in `Apply()` has a strict priority order:

```go
// 1. deferred panic recovery (Go panics from Number arithmetic)
//    → always becomes ErrExceptionEscape
//
// 2. ErrPromptAbort check (errors.As)
//    → pass through unchanged
//
// 3. ErrExceptionEscape check (errors.As)
//    → pass through unchanged
//
// 4. any other Go error
//    → wrap via goErrorToSchemeException → ErrExceptionEscape
```

Panic recovery is deferred, so it runs after all other checks. The priority
of step 2 over step 3 is critical: without it, prompt aborts would be wrapped
as Scheme exceptions and never reach their handlers.

Note: `call-with-exit` uses `ErrPromptAbort` with a private `PromptTag`
(created per invocation). The exit closure returns `ErrPromptAbort` which
propagates through FFC unchanged and is caught by `PrimCallWithExit` in its
sub-context via tag match. This unifies all continuation control flow under
a single error type.

## RunWithEscapeHandling

`machine/machine_context.go:1228`

This is the top-level execution loop. It installs `DefaultPromptTag` on the
context (so `call/cc` escapes have a target), then enters a `for` loop
that repeatedly calls `Run()` and handles what comes back.

All `ErrPromptAbort` errors are handled uniformly via `FindPrompt` —
there is no special-case detection for call/cc escapes. The
composable-continuation-then-abort model means call/cc escape closures
produce regular `ErrPromptAbort` to `DefaultPromptTag`, which the
standard prompt handling path catches.

```
┌─ RunWithEscapeHandling ────────────────────────────────────────────┐
│                                                                    │
│  p.promptTag = DefaultPromptTag                                    │
│                                                                    │
│  loop:                                                             │
│    err := p.Run()                                                  │
│    │                                                               │
│    ├─ err == nil (normal completion)                                │
│    │   ├─ UnwindTo(0) if winding frames remain                     │
│    │   └─ return nil                                               │
│    │                                                               │
│    ├─ ErrPromptAbort                                               │
│    │   FindPrompt(tag)                                             │
│    │   RestoreWithWindingFrom(nil, current, prompt.windingStack)   │
│    │   prompt != nil? Restore(prompt)                              │
│    │   prompt has handler? Apply(handler, values...)               │
│    │   no handler?                                                 │
│    │     SetValue(values[0])                                       │
│    │     prompt == nil? return nil  (context-level abort)          │
│    │   continue loop                                               │
│    │                                                               │
│    └─ other error → return err                                     │
└────────────────────────────────────────────────────────────────────┘
```

### Context-level abort

When `FindPrompt` returns `prompt == nil`, the abort reached the
context-level default prompt. This happens when a call/cc escape closure
fires: the composable continuation ran to completion inside the escape
closure's sub-context, and the abort carries the final result. Since
there is no handler and no remaining code to execute (the FFC at `p.pc`
was not advanced), `RunWithEscapeHandling` returns nil immediately after
setting the value.

## RestoreWithWindingFrom

`machine/machine_context.go:1080`

The central dynamic-wind transition function. Used by:
- `applyComposableContinuation` (composable continuation application)
- `RunWithEscapeHandling` (prompt abort handling, including call/cc escapes)

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

## PrimCallCC — composable-continuation-then-abort model

`registry/core/prim_control.go:116`

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
1. Captures a composable continuation via `SliceContinuationAt(nil)` (deep-copies the entire chain)
2. Copies the winding stack
3. Creates a `ComposableContinuation` from the segment + winding stack + thread ID + barrier token
4. Builds a `CapturedContinuation` escape value via `NewCapturedContinuation`

### Inline mode (mc.Parent() != nil)

The lambda runs directly in the current VM context. This preserves the
full continuation chain for coroutine patterns.

```
PrimCallCC
  segment = mc.SliceContinuationAt(nil)
  windingStack = mc.WindingStack().Copy()
  cc = NewComposableContinuation(segment, windingStack, threadID, barrierValid)
  contClosure = NewCapturedContinuation(cc, threadID, barrierValid)
  mc.Apply(mcls, contClosure)
  return nil
```

### Sub-context mode (mc.Parent() == nil)

Falls back to an isolated sub-context when call/cc is inside another foreign
function's sub-context where there's no saved continuation to return to.

```
PrimCallCC
  segment = mc.SliceContinuationAt(nil)
  windingStack = mc.WindingStack().Copy()
  cc = NewComposableContinuation(segment, windingStack, threadID, barrierValid)
  contClosure = NewCapturedContinuation(cc, threadID, barrierValid)
  sub = mc.NewSubContext()    // inherits winding stack
  sub.Apply(mcls, contClosure)
  err = sub.Run()
  if err:
    if ErrPromptAbort to DefaultPromptTag:
      mc.SetValue(abortErr.Values[0])   // extract result
      return nil
    return err
  mc.SetValue(sub.GetValue())
```

In sub-context mode, the escape closure's abort to `DefaultPromptTag` is
caught directly here rather than propagating to `RunWithEscapeHandling`.
This ensures call/cc works in contexts without `RunWithEscapeHandling`
(e.g., threads that call `Run()` directly).

## call/cc escape value

`machine/captured_continuation.go` (`NewCapturedContinuation`)

The escape value is a `CapturedContinuation` that:
1. Checks thread identity (captured vs invoking thread ID)
2. Checks barrier identity (captured vs invoking barrier token)
3. Applies the composable continuation in a sub-context with the passed value
4. Runs the restored frames to completion
5. Returns `ErrPromptAbort{DefaultPromptTag, [result]}` with the final value

This is the key simplification over the old model: the escape closure does
actual work (applying the composable continuation) rather than packing a
payload for someone else to handle.

## PrimCallWithContinuationPrompt

`registry/core/prim_prompt.go:71`

```
PrimCallWithContinuationPrompt(thunk, tag, handler)
  sub = mc.NewSubContext()    // inherits winding stack
  sub.SetPromptTag(tag)              // mark boundary
  sub.Apply(thunk)
  err = sub.Run()
  │
  ├─ err == nil → mc.SetValues(sub.GetValues())
  │
  ├─ ErrPromptAbort with matching tag:
  │   sub.UnwindTo(mc.WindingStack().Depth())   // run after thunks
  │   handler != nil?
  │     handlerSub.Apply(handler, abortErr.Values...)
  │     mc.SetValues(handlerSub.GetValues())
  │   handler == nil?
  │     mc.SetValue(abortErr.Values[0])
  │
  ├─ ErrPromptAbort with different tag → propagate
  │
  └─ other error → propagate
```

## Composable continuation application

`machine/machine_context.go:486`

```
applyComposableContinuation(cc, [arg])
  │
  ├─ Thread check: reject if p.threadID != cc.threadID
  │
  ├─ Barrier check: reject if cc.BarrierValid() != p.barrierValid
  │
  ├─ segment = cc.Cont().DeepCopy()
  │
  ├─ GraftContinuation(segment, p.cont)
  │   walks segment to bottom, sets parent = p.cont
  │
  ├─ RestoreWithWindingFrom(nil, p.windingStack, cc.WindingStack())
  │   unwind current extents not in captured, rewind captured not in current
  │
  ├─ Restore(segment)
  │   resume execution from top of segment
  │
  └─ SetValue(arg)
```

The `DeepCopy()` before grafting is critical: without it, re-invoking
the composable continuation corrupts the shared frames.

## Type and file inventory

### Types

| Type | File | Purpose |
|------|------|---------|
| `PromptTag` | `machine/prompt_tag.go` | Opaque identity, pointer equality, atomic ID |
| `ErrPromptAbort` | `machine/prompt_abort.go` | Error propagation carrier |
| `BarrierToken` | `machine/barrier_token.go` | Opaque barrier identity, pointer equality |
| `ComposableContinuation` | `machine/composable_continuation.go` | Callable delimited continuation segment |
| `DynamicWindFrame` | `machine/dynamic_wind.go` | Before/after thunks + atomic ID |
| `WindingStack` | `machine/dynamic_wind.go` | `[]*DynamicWindFrame` slice |

### Functions

| Function | File | Purpose |
|----------|------|---------|
| `RunWithEscapeHandling` | `machine/machine_context.go:1228` | Top-level execution loop |
| `FindPrompt` | `machine/machine_context.go:1114` | Walk continuation chain + check context tag |
| `SliceContinuationAt` | `machine/machine_context.go:1130` | Deep-copy continuation segment to prompt |
| `GraftContinuation` | `machine/machine_context.go:1151` | Splice segment onto target chain |
| `RestoreWithWindingFrom` | `machine/machine_context.go:1080` | Unwind/rewind + restore continuation |
| `FindCommonWindingPrefix` | `machine/dynamic_wind.go:78` | Common ancestor of two winding stacks |
| `applyComposableContinuation` | `machine/machine_context.go:486` | Apply composable continuation value |
| `PrimCallCC` | `registry/core/prim_control.go:116` | call/cc primitive (inline + sub-context) |
| `NewCapturedContinuation` | `machine/captured_continuation.go` | Build call/cc escape value: apply cc then abort |
| `PrimCallWithContinuationPrompt` | `registry/core/prim_prompt.go:71` | Install prompt, run thunk, handle abort |
| `PrimAbortCurrentContinuation` | `registry/core/prim_prompt.go:161` | Return ErrPromptAbort |
| `PrimCallWithComposableContinuation` | `registry/core/prim_prompt.go:197` | Capture composable continuation |

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

What happens at `(k 42)`:

1. Escape closure invoked with value 42
2. Thread check and barrier check pass
3. Escape closure applies the composable continuation in a sub-context:
   a. `DeepCopy` the continuation segment
   b. `GraftContinuation` onto sub-context's chain
   c. `RestoreWithWindingFrom(nil, [], [D1])`:
      - `FindCommonWindingPrefix([], [D1])` = 0
      - No unwinding (source is empty)
      - Rewind D1: call before thunk → prints "before"
   d. `Restore(segment)` → resume inside dynamic-wind thunk
   e. `SetValue(42)`
4. Thunk returns 42, continuation frames run to completion
5. Sub-context `Run()` returns nil
6. Escape closure returns `ErrPromptAbort{DefaultPromptTag, [42]}`
7. `OperationForeignFunctionCall` passes it through
8. `Run()` returns the error to `RunWithEscapeHandling`
9. `FindPrompt(DefaultPromptTag)` → nil (context-level prompt)
10. `RestoreWithWindingFrom(nil, windingStack, nil)`:
    - Unwind remaining frames → call after thunk → prints "after"
11. `prompt == nil`, no handler → `SetValue(42)`, return nil

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
;; result: 11
```

What happens:

1. `PrimCallWithContinuationPrompt` creates sub-context with `promptTag=tag`
2. Thunk runs, reaches `call-with-composable-continuation`
3. `PrimCallWithComposableContinuation`:
   a. `FindPrompt(tag)` → nil (prompt is on context, not continuation frame)
   b. `SliceContinuationAt(nil)` → deep-copy entire continuation chain (the `(+ 1 <hole>)` frame)
   c. Create `ComposableContinuation` with segment + winding stack
   d. Run `(lambda (k) (k 10))` in sub-context
   e. `(k 10)` applies the composable continuation:
      - DeepCopy segment
      - Graft onto current continuation
      - Restore from segment top
      - SetValue(10)
   f. Execution continues: `(+ 1 10)` = 11
   g. Sub-context returns 11
   h. Abort to tag with value 11
4. Back in `PrimCallWithContinuationPrompt`:
   a. Catches abort, tag matches
   b. No handler (`#f`), so returns first value: 11
