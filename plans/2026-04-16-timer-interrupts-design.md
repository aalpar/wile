# Timer Interrupts Design

**Status:** Draft — not started
**Date:** 2026-04-16

> **Incomplete:** All items. No ErrTimerInterrupt type, no timerHandler/timerCancel fields, no interrupt points, no RunWithEscapeHandling dispatch, no with-timeout primitive.

## Problem

Embedders need to bound computation: "run this Scheme script but stop it after
500ms." The mechanism must cover both bytecode execution and foreign function
calls (Go code performing I/O of unpredictable duration).

Today, `context.WithTimeout` + the 1024-op `ctx.Done()` check kills the
computation (`return mc.ctx.Err()`). This is adequate for hard kills but
insufficient for the engine pattern, where the suspended computation is
captured as a resumable continuation and delivered to a Scheme handler.

### Current State

| Mechanism | Exists? | Limitation |
|-----------|---------|------------|
| Op counter (`OpsExecuted`) | Yes — increments every bytecode op | No handler dispatch, no user-set threshold |
| `ctx.Done()` check (1024-op) | Yes — `machine_context.go:306` | Returns `ctx.Err()` (kills), doesn't capture continuation |
| `CallContext.Context()` | Yes — foreign functions can access context | Opt-in; no handler dispatch on return |
| Post-foreign-call ctx check | No | Up to 1024 ops latency after slow foreign call |

### Design Goal

A wall-clock timeout that, instead of killing the computation, captures the
current continuation and delivers it to a Scheme handler. The handler decides
whether to resume, discard, or re-fuel the suspended computation.

## Design

### Interrupt Delivery via ErrTimerInterrupt

New error type following the `ErrExceptionEscape` / `ErrPromptAbort` pattern:

```go
// machine/timer_interrupt.go
type ErrTimerInterrupt struct {
    Handler values.Callable
}

func (e *ErrTimerInterrupt) Error() string {
    return "timer interrupt"
}
```

This is a signal, not an exception. It propagates through the same Go error
return path but is handled by the VM loop (or `RunWithEscapeHandling`), not
by Scheme exception handlers.

### Timer State on MachineContext

```go
// machine/machine_context.go (new fields)
type MachineContext struct {
    // ...existing fields...

    timerHandler values.Callable  // nil = no timer active
    timerCancel  context.CancelFunc  // cancels the child timeout context
}
```

When a timer is active, `mc.ctx` is a child context derived from the
original via `context.WithTimeout`. The `timerHandler` field holds the
Scheme handler to invoke on expiry. `timerCancel` allows cancellation
when the guarded computation completes normally.

### Two Interrupt Points

**Point 1: Bytecode loop** (`machine_context.go:306`)

```go
if mc.counters.OpsExecuted&contextCheckMask == 0 {
    select {
    case <-mc.ctx.Done():
        if mc.timerHandler != nil {
            return &ErrTimerInterrupt{Handler: mc.timerHandler}
        }
        return mc.ctx.Err()
    default:
    }
}
```

The timer handler takes priority over the kill path. If no handler is
installed, the existing kill behavior is preserved.

**Point 2: After foreign function return** (`machine_context_apply.go`, after line 131)

```go
err = fcls.fn(p)
if err != nil { ... }

// Immediate timeout check after foreign call.
// Don't wait up to 1024 ops for the bytecode loop to notice.
if p.timerHandler != nil {
    select {
    case <-p.ctx.Done():
        return nil, &ErrTimerInterrupt{Handler: p.timerHandler}
    default:
    }
}
```

This closes the latency gap: a foreign function that blocks for 3 seconds
and then returns normally triggers the handler immediately, not after 1024
more bytecode ops.

### Handler Dispatch

`RunWithEscapeHandling` catches `ErrTimerInterrupt` alongside `ErrPromptAbort`
and `ErrExceptionEscape`:

```go
var timerErr *ErrTimerInterrupt
if errors.As(err, &timerErr) {
    // 1. Capture the current computation as a composable continuation.
    //    SliceContinuationAt(nil) captures the full chain back to the
    //    default prompt, same as call/cc.
    segment := p.SliceContinuationAt(nil)
    windingCopy := p.WindingStack().Copy()
    resumable := NewComposableContinuation(
        segment, windingCopy, p.threadID, p.barrierValid,
    )

    // 2. Clear the timer state (prevent re-entry).
    p.timerHandler = nil

    // 3. Install a fresh context (the old one is cancelled).
    p.SetContext(context.Background())

    // 4. Call the handler with the resumable continuation.
    _, applyErr := p.ApplyCallable(timerErr.Handler, resumable)
    if applyErr != nil {
        return applyErr
    }
    // Handler now executes in the VM loop (re-enters Run()).
    continue
}
```

The handler receives one argument: a composable continuation representing
the suspended computation. Calling the continuation resumes it. Discarding
it abandons the computation (equivalent to the current kill behavior).

### Scheme API

One primitive: `with-timeout`.

```scheme
(with-timeout milliseconds handler thunk)
;; milliseconds: exact non-negative integer
;; handler: (lambda (resumable-continuation) ...)
;; thunk: (lambda () ...)
;; Returns: result of thunk, or result of handler
```

Semantics:
- Runs `thunk` with a wall-clock deadline of `milliseconds` ms.
- If `thunk` completes within the deadline, returns its result.
  The timer is cancelled.
- If the deadline expires, `handler` is called with a composable
  continuation that, when invoked with a value, resumes the
  suspended computation as if `thunk` had returned that value.
- If `handler` calls the continuation, the computation resumes
  with a fresh (no-timeout) context. The handler can install a
  new `with-timeout` around the continuation call to re-fuel.
- If `handler` returns without calling the continuation, its
  return value becomes the result of `with-timeout`.

### Engine Pattern (Derived)

Chez-style engines are ~30 lines of Scheme on top of `with-timeout`:

```scheme
(define (make-engine thunk)
  (lambda (ms complete expire)
    (let ((start (current-milliseconds)))
      (with-timeout ms
        (lambda (k)
          ;; Expired: wrap continuation in a new engine
          (expire (make-engine (lambda () (k (void))))))
        (lambda ()
          ;; Completed: report remaining time
          (let ((result (thunk)))
            (complete (- ms (- (current-milliseconds) start))
                      result)))))))
```

This is not a built-in — it's a derived form that embedders can customize
(different fuel semantics, logging, retry policies).

### Context-Aware Foreign Functions

Foreign functions that perform I/O should pass `mc.Context()` to the
underlying Go call. This is already possible — `CallContext.Context()`
exists. No API change needed. Examples:

```go
// Good: context-aware foreign function
func PrimHTTPGet(mc machine.CallContext) error {
    url := mc.Arg(0)
    req, _ := http.NewRequestWithContext(mc.Context(), "GET", url, nil)
    resp, err := http.DefaultClient.Do(req)
    // ...
}

// Also fine: non-context-aware (blocks until done, handler fires on return)
func PrimSleep(mc machine.CallContext) error {
    time.Sleep(5 * time.Second)
    return nil
}
```

For non-context-aware functions, the timeout fires at Point 2 (post-foreign-call
check). The foreign function runs to completion; the handler fires immediately
after.

### Dynamic-Wind Interaction

When a timer interrupt fires, the handler runs in the dynamic extent of
`RunWithEscapeHandling` — outside any `dynamic-wind` frames from the
interrupted computation. The captured continuation carries the winding
stack, so resuming it properly re-enters all before-thunks (same as
invoking any other composable continuation).

Cleanup is correct: if the handler discards the continuation, after-thunks
from the interrupted computation are NOT run. This matches Chez engine
semantics — an expired engine's winding state is frozen in the continuation,
not unwound.

### Thread Interaction

Each SRFI-18 thread has its own `MachineContext` with its own `ctx`,
`timerHandler`, and `timerCancel`. A `with-timeout` in one thread does not
affect others. This is correct: timers are per-thread, not global.

Cross-thread continuation invocation is already rejected by thread ID
comparison (`applyComposableContinuation`). A continuation captured by
a timer interrupt on thread A cannot be resumed on thread B.

### Nesting

`with-timeout` nests correctly via context derivation:

```scheme
(with-timeout 1000 outer-handler
  (lambda ()
    (with-timeout 200 inner-handler
      (lambda () (expensive-computation)))))
```

The inner timeout fires first (200ms). Its handler receives the continuation.
If the inner handler resumes and the outer timeout also fires, the outer
handler receives a continuation that includes the inner computation.

Implementation: each `with-timeout` derives a child context from the
current one (`context.WithTimeout(mc.ctx, duration)`). Go's context
hierarchy ensures the inner deadline fires first when it's shorter.
The `timerHandler` field is saved and restored via `dynamic-wind`
(or a dedicated save/restore in the primitive implementation).

## Implementation Order

1. `ErrTimerInterrupt` type (`machine/timer_interrupt.go`)
2. `timerHandler` + `timerCancel` fields on `MachineContext`
3. Bytecode loop interrupt point (modify existing `ctx.Done()` check)
4. Post-foreign-call interrupt point (modify `applyForeign`)
5. `RunWithEscapeHandling` dispatch (new `errors.As` branch)
6. `with-timeout` primitive (`registry/core/prim_timer.go`)
7. Timer state save/restore for nesting

Steps 1-5 are machine infrastructure. Step 6 is the Scheme-visible API.
Step 7 is correctness for nested timeouts.

## Out of Scope

| Item | Reason |
|------|--------|
| Op-counting fuel | Already exists (OpsExecuted counter). Adding handler dispatch at the op check is trivial once the interrupt machinery exists, but wall-clock is the priority. |
| `set-timer` / `timer-interrupt-handler` (Chez API) | `with-timeout` is a better API for Go embedders (scoped, no global state). Chez API is derivable if needed. |
| Deterministic fuel (Chez-style procedure call counting) | Different semantics from wall-clock. Separate follow-on if needed. |
| Engine library (`(wile engine)`) | Derived form (~30 lines), not a primitive. Ship after `with-timeout` is stable. |

## Risks

**Foreign function that never returns.** If a Go function enters an infinite
loop without checking context, the timeout cannot fire until it returns.
`with-timeout` does not (and cannot) kill Go goroutines. Mitigation:
document that context-aware foreign functions are recommended for I/O.

**Handler re-entrancy.** The handler runs with `timerHandler = nil`, preventing
re-entry from the same expired context. But the handler could install a new
`with-timeout`, which is valid (re-fueling pattern). No additional protection
needed.

**Continuation size.** Capturing the full continuation chain on every timeout
is O(stack depth). For deeply recursive computations, this could be expensive.
In practice, the continuation chain is heap-allocated linked list nodes — the
capture is a pointer copy of the chain segment, not a deep copy (first
invocation). Same cost as `call/cc`.

## Testing

1. **Basic timeout:** `(with-timeout 50 handler (lambda () (let loop () (loop))))` —
   infinite loop, verify handler is called with a continuation.
2. **Normal completion:** `(with-timeout 5000 handler (lambda () 42))` —
   fast computation, verify result is 42, handler is not called.
3. **Resumption:** Handler calls continuation with a value, verify computation
   resumes and returns.
4. **Foreign function timeout:** `(with-timeout 50 handler (lambda () (sleep 1)))` —
   verify handler fires after sleep returns (if sleep is not context-aware) or
   during sleep (if it is).
5. **Nesting:** Inner timeout fires before outer. Verify correct handler receives
   correct continuation.
6. **Dynamic-wind:** Timeout inside `dynamic-wind` body. Resume continuation.
   Verify before/after thunks fire correctly.
7. **Thread isolation:** Timeout in one thread does not affect another.
8. **Handler discards continuation:** Verify after-thunks are NOT called (frozen
   winding stack, not unwound).
