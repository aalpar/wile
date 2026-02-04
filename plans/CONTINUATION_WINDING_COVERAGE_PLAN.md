# Plan: Test Coverage for Continuation and Dynamic-Wind Methods

**Status: COMPLETE** — All methods at 100%. Package coverage: 83.0% → 83.7%.

## Results

| Method | Before | After |
|--------|--------|-------|
| `UnwindTo` | 0.0% | **100%** |
| `RestoreWithWinding` | 0.0% | **100%** |
| `RestoreWithWindingFrom` | 84.2% | **100%** |
| `RewindTo` | 84.6% | **100%** |
| `FindPrompt` | 66.7% | **100%** |
| `SliceContinuationAt` | 53.8% | **100%** |

Test file: `go/machine/continuation_winding_coverage_test.go` (16 tests).

## Original State

Critical VM methods in `machine_context.go` had inadequate test coverage:

| Method | Coverage | Risk Level | Impact |
|--------|----------|------------|--------|
| `RestoreWithWinding` | 0.0% | HIGH | Wrapper never called directly |
| `UnwindTo` | 0.0% | CRITICAL | After-thunk execution on unwind |
| `RestoreWithWindingFrom` | 84.2% | CRITICAL | Core continuation restoration |
| `RewindTo` | 84.6% | CRITICAL | Before-thunk execution on rewind |
| `FindPrompt` | 66.7% | HIGH | Delimited continuation support |
| `SliceContinuationAt` | 53.8% | HIGH | Delimited continuation segments |

**Why This Matters**: These methods implement R7RS `call/cc` and `dynamic-wind` semantics. Bugs here corrupt continuation chains, skip dynamic-wind thunks, or break exception handlers.

## Current Test Coverage

Existing tests in `coverage_fullruntime_test.go` claim to test these methods but don't achieve full coverage:

- `TestCoverageDynamicWindWithCallCC` - exercises RestoreWithWindingFrom via call/cc escapes, but doesn't hit all branches
- Various dynamic-wind tests - test basic functionality but miss edge cases
- No direct tests for UnwindTo path (only called on successful completion with non-empty winding stack)

## Root Cause Analysis

### RestoreWithWinding (0%)

```go
func (p *MachineContext) RestoreWithWinding(cont *MachineContinuation, targetStack WindingStack) error {
    return p.RestoreWithWindingFrom(cont, p.windingStack, targetStack)
}
```

**Issue**: This wrapper is never called by production code. All callers use `RestoreWithWindingFrom` directly:
- `machine_context.go:702` - continuation escape handling
- `machine_context.go:727` - prompt abort handling
- `operation_apply.go:141` - composable continuation application

**Solution**: Either remove this dead code or add tests that call it directly.

### UnwindTo (0%)

```go
func (p *MachineContext) UnwindTo(commonDepth int) error {
    // Run after thunks from innermost to outermost
    for i := len(p.windingStack) - 1; i >= commonDepth; i-- {
        // ... call after thunk in sub-context ...
    }
    p.windingStack = p.windingStack[:commonDepth]
    return nil
}
```

**Called from**: `machine_context.go:675` in `RunWithEscapeHandling`:

```go
if err == nil || errors.Is(err, ErrMachineHalt) {
    if len(p.windingStack) > 0 {
        unwindErr := p.UnwindTo(0)  // <- Only call site
        if unwindErr != nil {
            return unwindErr
        }
    }
    // ...
}
```

**Issue**: Existing tests never complete with a non-empty winding stack. All dynamic-wind tests either:
1. Escape via call/cc (hits RestoreWithWindingFrom, not UnwindTo)
2. Complete normally but the winding stack is already empty

**Solution**: Create a test that evaluates a top-level expression inside dynamic-wind that completes successfully without unwinding explicitly.

### RestoreWithWindingFrom (84.2%)

Missing branches:
- Error handling when `frame.After != nil` fails (line 552-558)
- Continuation restoration when `cont != nil` (line 576-578) might be partially covered

### RewindTo (84.6%)

Missing branches:
- Error handling when `frame.Before != nil` fails (line 502-508)
- Frame addition to winding stack (line 511)

### FindPrompt (66.7%)

Missing branches:
- Context-level prompt tag matching (line 596-598): when prompt is set directly on `mc.promptTag` rather than on a continuation frame

### SliceContinuationAt (53.8%)

Multiple branches uncovered - needs investigation of what cases are missing.

## Test Strategy

### 1. UnwindTo - Normal Completion with Dynamic-Wind (0% → 100%)

**Test Goal**: Top-level expression completes successfully while inside dynamic-wind.

```scheme
(define log '())
(dynamic-wind
  (lambda () (set! log (cons 'before log)))
  (lambda () (set! log (cons 'thunk log)) 42)
  (lambda () (set! log (cons 'after log))))
```

**Expected Path**:
1. Execute dynamic-wind inline bytecode
2. Push winding frame
3. Thunk completes, returns 42
4. Pop winding frame (during compilation)
5. Return to top level
6. `RunWithEscapeHandling` sees `err == nil` with `len(p.windingStack) > 0`
7. Calls `UnwindTo(0)` to run remaining after thunks

**Wait, there's a problem**: Looking at `CompileValidatedDynamicWind`, it emits `POP_WIND` inline before the after thunk, so the winding stack should be empty when we return. Let me check if there's a way to have a non-empty stack at completion...

**Revised Understanding**: The inline compilation pops the wind frame explicitly. `UnwindTo` is only called when execution completes abnormally (early return, exception) and there are still frames on the stack that never got properly popped.

**Better Test**: Nested dynamic-wind where inner completes but outer has frames:

```scheme
(define outer-log '())
(dynamic-wind
  (lambda () (set! outer-log (cons 'outer-before outer-log)))
  (lambda ()
    ; Inner dynamic-wind that completes and pops its frame
    (dynamic-wind
      (lambda () #f)
      (lambda () 'done)
      (lambda () #f))
    ; Now we're in outer's thunk, winding stack has 1 frame
    ; Complete successfully - should trigger UnwindTo
    'result)
  (lambda () (set! outer-log (cons 'outer-after outer-log))))
```

**Even Better Test**: Exception or call/cc that leaves frames on the stack, then gets handled and execution completes normally:

Actually, reviewing the code more carefully: `UnwindTo` is called when `RunWithEscapeHandling` completes (nil or ErrMachineHalt) and the winding stack is non-empty. This handles the case where a sub-context or foreign function returns to the top level but didn't properly unwind its frames.

**Correct Test Approach**: Call a foreign function that creates a sub-context with dynamic-wind, capture a continuation, and invoke it to escape. But that hits `RestoreWithWindingFrom`, not `UnwindTo`.

**Actually Correct Approach**: After reviewing again, `UnwindTo` is called only when the entire top-level execution completes successfully but there are leftover frames. This shouldn't happen in correct code - it's a safety net. To test it, we'd need to deliberately leave frames on the stack, which requires:

1. A foreign function that pushes wind frames but doesn't pop them
2. Or a composable continuation that resumes with extra frames

Let me look at `operation_apply.go:141` to see how composable continuations handle this...

```go
err := mc.RestoreWithWindingFrom(nil, mc.windingStack, cc.WindingStack())
```

This suggests composable continuations can change the winding stack. If a composable continuation has a different winding stack than the current context, and it resumes, then completes, we'd have leftover frames.

**Final Test Strategy for UnwindTo**:
- Use delimited continuations (`call-with-composable-continuation`)
- Capture a continuation inside dynamic-wind
- Resume it outside the dynamic-wind
- Let it complete - should leave frames on stack
- Top level completion triggers `UnwindTo(0)`

### 2. RestoreWithWinding - Direct Call (0% → 100%)

**Simple approach**: Since this is just a wrapper, write a unit test that calls it directly:

```go
func TestMachineContext_RestoreWithWinding(t *testing.T) {
    env := newTestEnv(t)
    mc := newTestContext(t, env)

    // Set up a winding stack
    beforeCalled := false
    afterCalled := false
    before := newTestClosure(t, func() { beforeCalled = true })
    after := newTestClosure(t, func() { afterCalled = true })

    mc.PushWindingFrame(&DynamicWindFrame{Before: before, After: after})

    // Capture continuation
    cont := mc.SaveContinuation()

    // Invoke wrapper
    err := mc.RestoreWithWinding(cont, WindingStack{})

    c.Assert(err, qt.IsNil)
    c.Assert(afterCalled, qt.IsTrue) // Should have unwound
}
```

**Alternative**: Accept that this is dead code and remove it. Check git history to see if it was ever called.

### 3. RestoreWithWindingFrom Error Paths (84.2% → 95%+)

**Missing: After-thunk error during unwind**

```scheme
(call-with-current-continuation
  (lambda (escape)
    (dynamic-wind
      (lambda () 'setup)
      (lambda () (escape 'early-exit))
      (lambda () (error "after thunk failed")))))
```

**Expected**: When unwinding to restore the captured continuation, the after thunk throws an exception. `RestoreWithWindingFrom` should propagate this error.

**Missing: Before-thunk error during rewind**

```scheme
(let ((k #f))
  (call-with-current-continuation
    (lambda (cont) (set! k cont) 'captured))

  ; Outside the dynamic-wind now
  (dynamic-wind
    (lambda () (error "before thunk failed"))
    (lambda () 'in-wind)
    (lambda () 'cleanup))

  ; Invoke continuation - must rewind to target, hits error in before
  (k 'resume))
```

### 4. RewindTo Error Paths (84.6% → 95%+)

Same test as #3 "before-thunk error during rewind" covers this.

### 5. FindPrompt - Context-Level Prompt (66.7% → 100%)

**Missing**: Prompt set on `mc.promptTag` rather than on a continuation frame.

```scheme
; call-with-continuation-prompt sets tag on sub-context
(call-with-continuation-prompt
  (lambda ()
    (abort-current-continuation default-prompt-tag 'aborted))
  default-prompt-tag
  (lambda (v) v))  ; Handler receives 'aborted
```

The sub-context created by `call-with-continuation-prompt` has `promptTag` set. When `abort-current-continuation` calls `FindPrompt`, it should find the prompt on the context itself, returning `(nil, true)`.

### 6. SliceContinuationAt - Comprehensive Coverage (53.8% → 90%+)

Need to investigate which branches are uncovered. Likely scenarios:
- Empty continuation chain
- Prompt at different depths
- Prompt not found (error case)
- Deep nesting

Tests with varying continuation chain depths:

```scheme
; Shallow - prompt immediately
(call-with-continuation-prompt
  (lambda ()
    (call-with-composable-continuation
      (lambda (k) k)))
  (make-continuation-prompt-tag))

; Deep - prompt after several frames
(call-with-continuation-prompt
  (lambda ()
    (let ((a (+ 1 2)))
      (let ((b (+ 3 4)))
        (call-with-composable-continuation
          (lambda (k) k)))))
  (make-continuation-prompt-tag))
```

## Test File Organization

Create `go/machine/continuation_winding_coverage_test.go` with:

```go
// Tests for continuation and dynamic-wind coverage gaps
package machine_test

// TestUnwindTo_NormalCompletionWithWindingFrames
// TestRestoreWithWinding_DirectCall
// TestRestoreWithWindingFrom_AfterThunkError
// TestRestoreWithWindingFrom_BeforeThunkError
// TestRewindTo_BeforeThunkError
// TestFindPrompt_ContextLevelTag
// TestSliceContinuationAt_VariousDepths
```

Use table-driven tests where appropriate. Use `runSchemeExpr` / `runSchemeExprs` helpers for integration tests.

## Non-Goals

- Do not add tests for branches that are genuinely unreachable (defensive nil checks)
- Do not refactor production code to make it more testable
- Do not aim for 100% on every method - some error paths are legitimately rare

## Expected Impact

| Method | Before | After | Priority |
|--------|--------|-------|----------|
| `UnwindTo` | 0% | ~100% | CRITICAL |
| `RestoreWithWinding` | 0% | 100% or REMOVE | LOW (dead code) |
| `RestoreWithWindingFrom` | 84.2% | ~95% | CRITICAL |
| `RewindTo` | 84.6% | ~95% | CRITICAL |
| `FindPrompt` | 66.7% | ~100% | HIGH |
| `SliceContinuationAt` | 53.8% | ~90% | HIGH |

**Overall Package Coverage**: Should increase from 83.0% to ~87-88%.

## References

- R7RS §6.10 - Control features (call/cc, dynamic-wind)
- Flatt et al. 2007 - "Adding Delimited and Composable Control to a Production Programming Environment"
- `docs/design/CONTINUATION_ESCAPE_DESIGN.md` - Wile's continuation implementation
- `docs/design/DELIMITED_CONTINUATIONS.md` - Wile's delimited continuation design
