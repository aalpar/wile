# Plan: `call-with-exit` and `with-baffle` — IMPLEMENTED

S7-inspired escape continuation and continuation barrier primitives for Wile.

> **Status: IMPLEMENTED.** Both `call-with-exit` and `with-continuation-barrier`/`with-baffle` are implemented and tested. See `registry/core/prim_exit.go`, `registry/core/prim_barrier.go`, `machine/exit_escape.go`, `machine/barrier_token.go`. The `barrier.go` file mentioned in Phase 2 was implemented as `barrier_token.go` with `BarrierToken` instead of `BarrierID`.

## Background

**S7 Scheme** (Bill Schottstaedt, CCRMA) provides two control-flow primitives
not found in R7RS that are useful for embedding contexts:

- **`call-with-exit`** — lightweight escape-only continuation (like Racket's
  `call/ec`, Guile's `call-with-escape-continuation`). The exit procedure is
  valid only during the dynamic extent of its body. No continuation capture
  required, making it much cheaper than `call/cc`.

- **`with-continuation-barrier`** (alias `with-baffle`) — continuation barrier
  (like Guile's `with-continuation-barrier`). Prevents continuations from
  crossing the barrier boundary in either direction. Returns exactly once.

Both reflect a pragmatic design philosophy suited to embedding: escape
continuations cover >90% of `call/cc` use cases at near-zero cost, and
continuation barriers protect host-application invariants from unexpected
re-entry.

### References

- [S7 documentation](https://ccrma.stanford.edu/software/snd/snd/s7.html)
- [Guile: Continuation Barriers](https://www.gnu.org/software/guile/manual/html_node/Continuation-Barriers.html)
- [Guile: Prompt Primitives](https://www.gnu.org/software/guile/manual/html_node/Prompt-Primitives.html)
- [SRFI 226: Control Features](https://srfi.schemers.org/srfi-226/srfi-226.html)

---

## Phase 1: `call-with-exit`

### Semantics

```scheme
(call-with-exit proc)  →  value
```

- Calls `proc` with one argument: an **exit procedure**.
- If the body returns normally, `call-with-exit` returns that value.
- If the exit procedure is called with a value, `call-with-exit` immediately
  returns that value, properly unwinding any `dynamic-wind` after thunks.
- The exit procedure is **only valid during the dynamic extent** of
  `call-with-exit`. Calling it after the body returns signals an error.
- Unlike `call/cc`, **no continuation is captured** — the exit procedure is
  a one-shot upward escape, not a reified continuation.

### Design

**Error type:** New lightweight `ErrExitEscape` in `machine/` (analogous to
`ErrContinuationEscape` but without a captured continuation):

```go
// machine/exit_escape.go

type exitTag struct{}  // unique-per-invocation identity

type ErrExitEscape struct {
    tag   *exitTag
    Value values.Value
}
```

**Validity tracking:** A `*atomic.Bool` shared between the `call-with-exit`
invocation and the exit closure. Set to `false` when the body returns normally.
The exit closure checks this before escaping.

**Primitive implementation** (`registry/core/prim_control.go`):

1. Create `tag := &exitTag{}` and `valid := &atomic.Bool{}` (initially true)
2. Build a foreign closure that:
   - Checks `valid` → error if false ("exit procedure called outside dynamic extent")
   - Checks cross-thread → error if different thread ID
   - Returns `&ErrExitEscape{tag, val}`
3. Run proc in a sub-context with the exit closure as argument
4. On normal return: set `valid = false`, return body value
5. On `ErrExitEscape` matching our tag: set `valid = false`, unwind
   `dynamic-wind` frames if needed, return escape value
6. On any other error: propagate

**Registration** (`registry/core/control.go`):

```go
{Name: "call-with-exit", ParamCount: 1, Impl: PrimCallWithExit,
    Doc: "Calls proc with an escape procedure valid only during the call.",
    ParamNames: []string{"proc"}, Category: "control"},
```

**`dynamic-wind` interaction:** The sub-context inherits the parent's winding
stack. If exit escapes past `dynamic-wind` frames, `UnwindTo` runs the after
thunks before returning. This matches the existing pattern in
`PrimCallWithContinuationPrompt`.

### Files to Create/Modify

| File | Action |
|------|--------|
| `machine/exit_escape.go` | **Create** — `exitTag`, `ErrExitEscape` |
| `registry/core/prim_exit.go` | **Create** — `PrimCallWithExit` |
| `registry/core/control.go` | **Modify** — register `call-with-exit` |
| `registry/core/register.go` | No change needed (control already registered) |
| `registry/core/prim_exit_test.go` | **Create** — tests |

### Test Cases

1. Normal return: `(call-with-exit (lambda (exit) 42))` → `42`
2. Exit with value: `(call-with-exit (lambda (exit) (exit 99) 42))` → `99`
3. Exit from nested call: `(call-with-exit (lambda (exit) (map (lambda (x) (if (> x 3) (exit 'found) x)) '(1 2 3 4 5))))` → `found`
4. Dynamic-wind after thunk runs:
   ```scheme
   (let ((log '()))
     (call-with-exit (lambda (exit)
       (dynamic-wind
         (lambda () (set! log (cons 'before log)))
         (lambda () (exit 'done))
         (lambda () (set! log (cons 'after log))))))
     (reverse log))
   ```
   → `(before after)`
5. Exit procedure invalid after return:
   ```scheme
   (let ((saved #f))
     (call-with-exit (lambda (exit) (set! saved exit) 42))
     (saved 99))
   ```
   → error: "exit procedure called outside dynamic extent"
6. Nested call-with-exit:
   ```scheme
   (call-with-exit (lambda (outer)
     (call-with-exit (lambda (inner)
       (inner 'inner-val)))
     'outer-val))
   ```
   → `outer-val`
7. Exit skips remaining computation:
   ```scheme
   (+ 1 (call-with-exit (lambda (exit) (+ 100 (exit 10)))))
   ```
   → `11`

---

## Phase 2: `with-continuation-barrier` (alias `with-baffle`)

### Semantics

```scheme
(with-continuation-barrier body ...)  →  value
(with-baffle body ...)                →  value   ; alias
```

- Evaluates `body` expressions sequentially, returning the value of the last.
- Establishes a **continuation barrier**. Any attempt to invoke a continuation
  that would cross the barrier boundary signals an error.
- `with-continuation-barrier` returns **exactly once** — it cannot be re-entered.
- Escape continuations (`call-with-exit`) inside the barrier work normally
  since they don't cross the barrier.
- Exceptions and prompt aborts propagate through the barrier normally.

### Barrier Semantics (what gets blocked)

| Scenario | Behavior |
|----------|----------|
| `call/cc` escape from inside barrier to outside | **Blocked** — error |
| Continuation captured outside, invoked inside to jump out | **Blocked** — error |
| Continuation captured inside, invoked outside to re-enter | **Blocked** — error |
| Composable continuation crossing barrier | **Blocked** — error |
| Composable continuation entirely inside barrier | **Allowed** — doesn't cross |
| `call-with-exit` inside barrier | **Allowed** — caught before barrier |
| `abort-current-continuation` through barrier | **Allowed** — upward-only unwind |
| Exception raised inside barrier | **Allowed** — propagates normally |
| Normal return from barrier body | **Allowed** — returns value |

### Design

**Barrier ID on MachineContext:** Add an optional `barrierID` field (or a
barrier stack) to `MachineContext`. This marks the current dynamic extent as
baffled.

```go
// machine/barrier.go

type BarrierID struct{}  // unique-per-invocation identity

// On MachineContext:
//   barrierID *BarrierID  // nil = no barrier
```

**Preventing escape out (case 1, 2):** `PrimCallWithContinuationBarrier` runs
the body in a sub-context. After `sub.Run()`, if the error is
`ErrContinuationEscape`, the barrier catches it and converts to an error:

```go
var escapeErr *machine.ErrContinuationEscape
if errors.As(err, &escapeErr) {
    return values.WrapForeignErrorf(ErrContinuationBarrier,
        "with-continuation-barrier: continuation cannot cross barrier")
}
```

**Prompt aborts pass through:** `ErrPromptAbort` errors are NOT caught by the
barrier — they propagate normally, same as exceptions.

**Preventing re-entry (case 3):** Continuations captured by `call/cc` inside
a baffled sub-context carry a barrier ID (stored on `ErrContinuationEscape` or
on the captured continuation itself). When the continuation closure is invoked,
it checks whether the barrier is still active. If the barrier has returned
(the `with-baffle` body completed), the continuation is invalid.

Implementation approach: the escape continuation closure created by `PrimCallCC`
will capture the current barrier ID. When invoked, it checks whether the
invoking context's barrier chain is compatible. If the captured barrier ID is
not present in the current barrier chain, the continuation crosses a barrier →
error.

**Alternative (simpler) approach:** Since `with-baffle` runs in a sub-context
and `call/cc` inside a sub-context captures the sub-context's continuation
chain, if the sub-context is gone (baffle returned), `RunWithEscapeHandling`
would attempt to restore a stale continuation. We can add a `*atomic.Bool`
validity flag to the barrier, shared with all continuations captured inside it.
When the barrier body returns, the flag is set to `false`. Continuation closures
check this flag before escaping.

This parallels `call-with-exit`'s validity pattern and avoids modifying
`MachineContext`'s core fields.

### Implementation Choice

**Decided:** Validity-flag approach.

- `PrimCallWithContinuationBarrier` creates `valid := &atomic.Bool{}` (true)
- All `call/cc` and composable continuations captured inside inherit a
  reference to `valid` (requires threading through the sub-context)
- When body returns: `valid.Store(false)`
- Continuation closures check `valid` → error if false

**Threading the validity flag:** Add an optional `BarrierValid *atomic.Bool`
field to `MachineContext`. `PrimCallCC` reads it and passes it to the escape
closure. `PrimCallWithComposableContinuation` reads it and wraps the composed
continuation with the same check. The flag is nil when no barrier is active
(zero-cost check).

### As a Special Form vs Primitive

`with-continuation-barrier` takes a body (implicit `begin`), so it should be
either:
- A **macro** expanding to `(call-with-continuation-barrier (lambda () body ...))`, or
- A **compiled special form** like `dynamic-wind`

**Decided:** Implement as a primitive `call-with-continuation-barrier` that
takes a thunk, then provide `with-continuation-barrier` and `with-baffle` as
syntax-rules macros:

```scheme
(define-syntax with-continuation-barrier
  (syntax-rules ()
    ((_ body ...)
     (call-with-continuation-barrier (lambda () body ...)))))

(define-syntax with-baffle
  (syntax-rules ()
    ((_ body ...)
     (call-with-continuation-barrier (lambda () body ...)))))
```

### Error Sentinel

```go
// values/foreign_error.go
ErrContinuationBarrier = NewStaticError("continuation barrier violation")
```

### Files to Create/Modify

| File | Action |
|------|--------|
| `machine/barrier.go` | **Create** — barrier validity infrastructure |
| `machine/machine_context.go` | **Modify** — add `barrierValid` field |
| `registry/core/prim_barrier.go` | **Create** — `PrimCallWithContinuationBarrier` |
| `registry/core/control.go` | **Modify** — register `call-with-continuation-barrier` |
| `registry/core/prim_control.go` | **Modify** — `PrimCallCC` reads barrier validity, passes to escape closure |
| `registry/core/prim_prompt.go` | **Modify** — `PrimCallWithComposableContinuation` checks barrier validity |
| `values/foreign_error.go` | **Modify** — add `ErrContinuationBarrier` sentinel |
| `lib/base.scm` or bootstrap | **Modify** — add `with-continuation-barrier` and `with-baffle` macros |
| `registry/core/prim_barrier_test.go` | **Create** — tests |

### Test Cases

1. Normal return: `(with-continuation-barrier 1 2 3)` → `3`
2. Escape continuation blocked:
   ```scheme
   (call/cc (lambda (k)
     (with-continuation-barrier (k 42))))
   ```
   → error: "continuation barrier violation"
3. Exit continuation inside works:
   ```scheme
   (with-continuation-barrier
     (call-with-exit (lambda (exit) (exit 42))))
   ```
   → `42`
4. `call/cc` inside barrier, invoked outside:
   ```scheme
   (let ((k #f))
     (with-continuation-barrier
       (call/cc (lambda (c) (set! k c) 42)))
     (k 99))
   ```
   → error: "continuation barrier violation"
5. Exception propagates normally:
   ```scheme
   (guard (e (#t 'caught))
     (with-continuation-barrier (error "boom")))
   ```
   → `caught`
6. Dynamic-wind inside barrier works:
   ```scheme
   (let ((log '()))
     (with-continuation-barrier
       (dynamic-wind
         (lambda () (set! log (cons 'before log)))
         (lambda () 42)
         (lambda () (set! log (cons 'after log)))))
     (reverse log))
   ```
   → `(before after)`
7. Nested barriers:
   ```scheme
   (with-continuation-barrier (with-continuation-barrier 42))
   ```
   → `42`
8. Composable continuation blocked at barrier:
   ```scheme
   (let ((k #f)
         (tag (make-continuation-prompt-tag 'test)))
     (with-continuation-barrier
       (call-with-continuation-prompt
         (lambda ()
           (call-with-composable-continuation
             (lambda (c) (set! k c) 42)
             tag))
         tag
         (lambda (v) v)))
     (k 99))
   ```
   → error: "continuation barrier violation"
9. Composable continuation entirely inside barrier works:
   ```scheme
   (with-continuation-barrier
     (let ((tag (make-continuation-prompt-tag 'test)))
       (call-with-continuation-prompt
         (lambda ()
           (+ 1 (call-with-composable-continuation
                   (lambda (k) (k (k 10)))
                   tag)))
         tag
         (lambda (v) v))))
   ```
   → `12`
10. Prompt abort passes through barrier:
    ```scheme
    (let ((tag (make-continuation-prompt-tag 'test)))
      (call-with-continuation-prompt
        (lambda ()
          (with-continuation-barrier
            (abort-current-continuation tag 42)))
        tag
        (lambda (v) v)))
    ```
    → `42`
11. `with-baffle` alias works:
    ```scheme
    (with-baffle 42)
    ```
    → `42`

---

## Phase 3: Integration & Documentation

### Integration Tests

Add integration tests in `integration/` that exercise:
- `call-with-exit` + `dynamic-wind` interaction
- `with-continuation-barrier` + `call/cc` barrier enforcement
- `with-continuation-barrier` + composable continuations (blocked at boundary)
- `with-continuation-barrier` + `abort-current-continuation` (passes through)
- `with-continuation-barrier` + `call-with-exit` (should work)
- `with-continuation-barrier` + exceptions (should propagate)
- `with-continuation-barrier` + threads (each thread has its own barrier)
- `with-baffle` alias
- Nested combinations of all three

### Documentation

- Update `PRIMITIVES.md` with new entries
- Update `CHANGELOG.md`
- Update `TODO.md` if these were tracked

### Registration Test

- Update `registry/core/register_test.go` `expectedPrims` list

---

## Implementation Order

1. **Phase 1:** `call-with-exit` — standalone, no dependencies on Phase 2
2. **Phase 2:** `with-continuation-barrier` — requires modification to both
   `PrimCallCC`'s escape closure and `PrimCallWithComposableContinuation`
   to check barrier validity
3. **Phase 3:** Integration tests, docs

Phases 1 and 2 are independent of each other in terms of functionality, but
Phase 2's barrier validity check in `PrimCallCC` and
`PrimCallWithComposableContinuation` should be designed to be zero-cost when
no barrier is active (nil check on the `BarrierValid` field).

---

## Design Decisions (Resolved)

1. **Naming:** Primary name `with-continuation-barrier` (matches existing
   Racket-style naming in the prompt system: `make-continuation-prompt-tag`,
   `call-with-composable-continuation`, etc.). Alias `with-baffle` for S7
   compatibility. Underlying primitive: `call-with-continuation-barrier`.

2. **Barrier blocks all continuation types.** Both escape continuations
   (`call/cc`) and composable continuations (`call-with-composable-continuation`)
   are blocked at barriers. A barrier that can be crossed isn't a barrier.
   Composable continuations that stay entirely within the barrier work fine —
   only cross-boundary invocations are blocked.

3. **Prompt aborts pass through barriers.** `abort-current-continuation` is
   upward-only unwinding (like exceptions), not continuation re-entry. Blocking
   it would break structured control flow patterns where a prompt is outside
   the barrier and an abort inside needs to unwind past it. Exceptions propagate
   through barriers; prompt aborts should too.

4. **Thread boundaries are already implicit barriers.** Wile blocks cross-thread
   continuation invocation via `ErrCrossThreadContinuation`. No additional
   thread-specific logic needed. `with-continuation-barrier` adds intra-thread
   barriers for protecting code regions.
