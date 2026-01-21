# Implementation Plan: R7RS-Conformant raise-continuable

## Problem Statement

The current `raise-continuable` implementation does not conform to R7RS §6.11. When a handler returns for a continuable exception, the handler's return value becomes the result of `with-exception-handler` rather than the result of `raise-continuable`, preventing execution from continuing at the call site.

**Current Behavior:**
```scheme
(with-exception-handler
  (lambda (e) (+ e 100))
  (lambda () (+ (raise-continuable 5) 1)))
;; Returns: 105 (handler's return value)
```

**R7RS-Required Behavior:**
```scheme
(with-exception-handler
  (lambda (e) (+ e 100))
  (lambda () (+ (raise-continuable 5) 1)))
;; Returns: 106 (handler returns 105, then +1 = 106)
```

---

## Architecture Analysis

### Current Flow

1. `PrimWithExceptionHandler` installs handler and runs thunk in sub-context
2. `PrimRaiseContinuable` captures continuation (`mc.Parent()`) and returns `ErrExceptionEscape`
3. Exception propagates up, `sub.Run()` exits
4. `handleException` calls handler in new sub-context
5. **Bug:** For continuable exceptions, handler's return value is set as result of `with-exception-handler`

### Required Flow

1-4. Same as above
5. For continuable exceptions, **resume execution** from the captured continuation with handler's return value
6. Result of resumed execution becomes result of `with-exception-handler`

### Key Insight

The captured continuation (`excErr.Continuation`) contains the PC pointing to the instruction after `raise-continuable`. Using `Restore()` on a new sub-context and `SetValue()` with the handler's return positions us to continue exactly where `raise-continuable` would have returned.

---

## Implementation Plan

### Phase 1: Copy Continuation in raise-continuable

**File:** `go/runtime/primitives/prim_raise_continuable.go`

**Rationale:** The continuation must be copied to prevent mutation issues if the handler or resumed code modifies shared state. This follows the pattern established by `call/cc`.

**Change:**
```go
func PrimRaiseContinuable(_ context.Context, mc *machine.MachineContext) error {
    obj := mc.Arg(0)

    // Copy continuation to prevent mutation issues
    cont := mc.Parent()
    if cont != nil {
        cont = cont.Copy()
    }

    return &machine.ErrExceptionEscape{
        Condition:    obj,
        Continuable:  true,
        Continuation: cont,
        Handled:      false,
    }
}
```

---

### Phase 2: Restructure handleException with Helper Functions

**File:** `go/runtime/primitives/prim_with_exception_handler.go`

**Rationale:** The current `handleException` mixes handler invocation with result handling. Separating these concerns makes the resumption logic clearer and enables proper handling of nested exceptions.

**Add helper functions:**

```go
// callExceptionHandler invokes the exception handler with the given condition.
// Returns the handler's return value, or an error if the handler raised an exception.
func callExceptionHandler(ctx context.Context, mc *machine.MachineContext,
    condition values.Value, handler values.Value) (values.Value, error) {

    sub := mc.NewSubContext()
    sub.SetExceptionHandler(mc.ExceptionHandler())

    switch h := handler.(type) {
    case *machine.MachineClosure:
        if _, err := sub.Apply(h, condition); err != nil {
            return nil, err
        }
    case *machine.CaseLambdaClosure:
        if _, err := sub.ApplyCaseLambda(h, condition); err != nil {
            return nil, err
        }
    default:
        return nil, values.WrapForeignErrorf(values.ErrNotAProcedure,
            "with-exception-handler: handler must be a procedure but got %T", handler)
    }

    err := sub.Run(ctx)

    // Handler raised another exception - propagate it
    var innerExc *machine.ErrExceptionEscape
    if errors.As(err, &innerExc) {
        return nil, err
    }

    // Continuation escape - propagate it
    var contErr *machine.ErrContinuationEscape
    if errors.As(err, &contErr) {
        return nil, err
    }

    if err != nil && !errors.Is(err, machine.ErrMachineHalt) {
        return nil, err
    }

    return sub.GetValue(), nil
}

// resumeFromContinuation resumes execution from a captured continuation with the given value.
// Returns the result of the resumed execution, or an error.
func resumeFromContinuation(ctx context.Context, mc *machine.MachineContext,
    cont *machine.MachineContinuation, value values.Value) (values.Value, error) {

    resumeSub := mc.NewSubContext()
    resumeSub.SetExceptionHandler(mc.ExceptionHandler())
    resumeSub.Restore(cont)
    resumeSub.SetValue(value)

    err := resumeSub.Run(ctx)

    if err != nil && !errors.Is(err, machine.ErrMachineHalt) {
        return nil, err
    }

    return resumeSub.GetValue(), nil
}
```

---

### Phase 3: Implement Resumption with Exception Handler Stack Management

**File:** `go/runtime/primitives/prim_with_exception_handler.go`

**Rationale:** Per R7RS §6.11:
- Handler is called with parent exception handler active (current behavior)
- After handler returns, if resuming, the original handler must be active again for subsequent exceptions

**Rewrite `handleException`:**

```go
func handleException(ctx context.Context, mc *machine.MachineContext,
    excErr *machine.ErrExceptionEscape, handler values.Value) error {

    // Pop this handler before calling it (so re-raises use parent handler per R7RS)
    mc.PopExceptionHandler()

    for {
        // Call handler with the condition
        handlerResult, err := callExceptionHandler(ctx, mc, excErr.Condition, handler)
        if err != nil {
            return err
        }

        // Non-continuable exception - handler should not return
        if !excErr.Continuable {
            return values.NewForeignError("exception handler returned from non-continuable exception")
        }

        // Continuable: resume execution from the captured continuation
        // Push handler back so subsequent exceptions in resumed code use this handler
        mc.PushExceptionHandler(handler)

        resumeResult, resumeErr := resumeFromContinuation(ctx, mc, excErr.Continuation, handlerResult)

        // Check if resumed code raised another exception
        var newExcErr *machine.ErrExceptionEscape
        if errors.As(resumeErr, &newExcErr) && !newExcErr.Handled {
            // Pop handler (will be pushed again if this is also continuable)
            mc.PopExceptionHandler()
            excErr = newExcErr
            continue // Loop to handle new exception
        }

        // Check for continuation escape from resumed code
        var contErr *machine.ErrContinuationEscape
        if errors.As(resumeErr, &contErr) {
            mc.PopExceptionHandler()
            return resumeErr // Propagate the escape
        }

        // Clean up handler stack
        mc.PopExceptionHandler()

        if resumeErr != nil && !errors.Is(resumeErr, machine.ErrMachineHalt) {
            return resumeErr
        }

        // Normal completion
        mc.SetValue(resumeResult)
        excErr.Handled = true
        return nil
    }
}
```

---

### Phase 4: Update Tests

**File:** `go/runtime/primitives/prim_exception_test.go`

**Update existing tests that now have different expected values:**

```go
// This test expected 105, now should expect 106
{
    name: "continuable exception handler return value becomes raise-continuable return",
    code: `(with-exception-handler
        (lambda (e) (+ e 100))
        (lambda () (+ (raise-continuable 5) 1)))`,
    out: values.NewInteger(106), // Changed from 105
},
```

**Add new R7RS conformance tests:**

```go
func TestRaiseContinuableResumption(t *testing.T) {
    tcs := []struct {
        name string
        code string
        out  values.Value
    }{
        {
            name: "handler return continues at call site",
            code: `(with-exception-handler
                (lambda (e) (+ e 100))
                (lambda () (+ (raise-continuable 5) 1)))`,
            out: values.NewInteger(106),
        },
        {
            name: "multiple expressions after raise-continuable",
            code: `(with-exception-handler
                (lambda (e) 'recovered)
                (lambda ()
                    (define x (raise-continuable 'warning))
                    (list 'after x)))`,
            out: values.List(values.NewSymbol("after"), values.NewSymbol("recovered")),
        },
        {
            name: "nested continuable exceptions",
            code: `(with-exception-handler
                (lambda (e) (+ e 10))
                (lambda ()
                    (+ (raise-continuable 1)
                       (raise-continuable 2))))`,
            out: values.NewInteger(23), // (+ 11 12)
        },
        {
            name: "continuable in conditional",
            code: `(with-exception-handler
                (lambda (e) #t)
                (lambda ()
                    (if (raise-continuable #f) 'yes 'no)))`,
            out: values.NewSymbol("yes"),
        },
        {
            name: "raise-continuable in let binding",
            code: `(with-exception-handler
                (lambda (e) (* e 2))
                (lambda ()
                    (let ((x (raise-continuable 5)))
                        (+ x 3))))`,
            out: values.NewInteger(13), // (* 5 2) = 10, + 3 = 13
        },
        {
            name: "multiple raise-continuable in sequence",
            code: `(with-exception-handler
                (lambda (e) (string-append "handled-" (symbol->string e)))
                (lambda ()
                    (list (raise-continuable 'first)
                          (raise-continuable 'second))))`,
            out: values.List(values.NewString("handled-first"), values.NewString("handled-second")),
        },
    }
    // ... test runner
}
```

**Add edge case tests:**

```go
func TestRaiseContinuableEdgeCases(t *testing.T) {
    // Handler that raises continuable exception itself
    // Continuation escape during resumed execution
    // Nested with-exception-handler with continuable
    // Empty continuation (raise-continuable at top level - should error gracefully)
}
```

---

### Phase 5: Handle Edge Cases

#### 5.1 Handler raises another exception

Already handled by the loop structure - if handler returns an error, it propagates.

#### 5.2 Continuation escape (call/cc) during resumption

Already handled - `ErrContinuationEscape` is propagated from `resumeFromContinuation`.

#### 5.3 Multiple values from handler

**Consideration:** Handler might return multiple values. Currently using `sub.GetValue()` which returns only the first value.

**Decision:** For now, use single value. R7RS §6.11 says "values returned by the handler" (plural), but typical use is single value. Document as a known limitation or implement `GetValues()` support.

#### 5.4 Empty or nil continuation

**Add check in `resumeFromContinuation`:**
```go
if cont == nil {
    return nil, values.NewForeignError("cannot resume: no continuation captured")
}
```

---

### Phase 6: Documentation Updates

**File:** `plans/R7RS_SEMANTIC_DIFFERENCES.md`
- Remove `raise-continuable` entry from summary table
- Remove section 8 about raise-continuable

**File:** `plans/TESTING_PLAN.md`
- Update Phase 10 section to note full R7RS conformance achieved

**File:** `go/runtime/primitives/CLAUDE.md`
- Update any notes about exception handling behavior

---

## Testing Strategy

### Unit Tests
1. Basic resumption (handler return continues at call site)
2. Multiple expressions after `raise-continuable`
3. Nested `raise-continuable` calls
4. `raise-continuable` in various contexts (if, let, begin, etc.)

### Integration Tests
1. `call/cc` interaction with `raise-continuable`
2. `dynamic-wind` interaction with `raise-continuable`
3. Nested `with-exception-handler` with continuable exceptions
4. Handler that invokes captured continuation

### Regression Tests
1. All existing exception tests still pass (with updated expectations)
2. Non-continuable exceptions still work correctly
3. Handler that re-raises still works

---

## Risk Assessment

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Breaking existing exception handling | Medium | High | Comprehensive regression tests before/after |
| Infinite loop from repeated continuable | Low | Medium | Document as user responsibility (same as infinite recursion) |
| Memory leaks from continuation copies | Low | Low | Continuations are GC'd normally |
| Stack overflow from nested exceptions | Low | Medium | Loop-based (not recursive) implementation |
| Performance regression | Low | Low | One extra continuation copy per exception |

---

## Estimated Effort

| Phase | Effort |
|-------|--------|
| Phase 1: Copy continuation | 15 min |
| Phase 2: Helper functions | 30 min |
| Phase 3: Resumption logic | 1-2 hours |
| Phase 4: Update tests | 1 hour |
| Phase 5: Edge cases | 1 hour |
| Phase 6: Documentation | 30 min |
| **Total** | **4-6 hours** |

---

## Verification Checklist

Before merging:

- [ ] All existing exception tests pass (with updated expectations)
- [ ] New R7RS conformance tests pass
- [ ] `(+ (raise-continuable 5) 1)` returns 106 not 105
- [ ] Nested exceptions work correctly
- [ ] `call/cc` interaction works
- [ ] Non-continuable exceptions still error on handler return
- [ ] `make test` passes
- [ ] `make lint` passes
- [ ] Documentation updated

---

## Alternative Approaches Considered

### A: Inline exception handling in VM loop

Would avoid the "exit sub.Run() and re-enter" pattern by handling exceptions directly in the VM loop.

**Rejected because:** Would require significant VM changes and could introduce subtle bugs. The trampoline approach is simpler and follows the `call/cc` pattern already in use.

### B: Compile-time transformation

Transform `raise-continuable` into a continuation-based form at compile time.

**Rejected because:** Would complicate the compiler and wouldn't work for dynamically loaded code.

### C: Use ErrContinuationEscape instead of loop

Return an `ErrContinuationEscape` from `handleException` to resume.

**Rejected because:** The continuation belongs to a different execution context (the sub-context that ran the thunk). The loop approach correctly creates a new sub-context and restores it.

---

## Dependencies

None. This change is self-contained within the primitives package.

---

## Rollback Plan

If issues are discovered after deployment:
1. Revert the changes to `prim_raise_continuable.go` and `prim_with_exception_handler.go`
2. Restore old test expectations
3. Re-add the `raise-continuable` entry to `R7RS_SEMANTIC_DIFFERENCES.md`
