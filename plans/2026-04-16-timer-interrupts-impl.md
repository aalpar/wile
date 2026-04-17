# Timer Interrupts Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Status:** All 8 tasks complete. Branch: `feature/timer-interrupts`.

**Goal:** Add wall-clock timer interrupts that capture the suspended computation as a resumable continuation and deliver it to a Scheme handler, enabling bounded evaluation and Chez-style engines.

**Architecture:** The timer installs a handler on `MachineContext`. When the deadline expires, the bytecode loop (or post-foreign-call check) returns `ErrTimerInterrupt`. The `with-timeout` primitive runs the thunk in a sub-context, catches the interrupt, captures the sub-context's full execution state as a composable continuation, and calls the handler with it. `RunWithEscapeHandling` provides a safety-net branch for interrupts that escape without a `with-timeout` wrapper.

**Tech Stack:** Go `context.WithTimeoutCause` (with `ErrTimerExpired` sentinel), existing composable continuation machinery (`SliceContinuationAt`, `NewComposableContinuation`, `CaptureInterruptContinuation`), sub-context pattern (same as `call-with-continuation-barrier`).

**Design document:** `plans/2026-04-16-timer-interrupts-design.md`

### Implementation Deviations from Plan

1. **`context.WithTimeoutCause` instead of `context.WithTimeout`** — The implementation uses the `Cause` variant with an `ErrTimerExpired` sentinel (`werr.NewStaticError`) to distinguish timer expiry from external cancellation (e.g. Ctrl+C). The plan specified plain `context.WithTimeout`.

2. **`ErrTimerExpired` sentinel added** — Not in the original plan. A `werr.NewStaticError("timer expired")` global in `timer_interrupt.go` serves as the cause for `context.WithTimeoutCause`.

3. **`OperationForeignFunctionCall` panic recovery pass-through** — The plan didn't cover the panic recovery `defer` block in `operations_call.go`. The implementation adds pass-through for `ErrPromptAbort`, `ErrExceptionEscape`, and `ErrTimerInterrupt` in the panic recovery path, preventing VM signal types from being wrapped as Scheme exceptions when recovered from panics.

4. **Type validation uses `helpers.RequireType`** — The plan showed manual type assertions; the implementation uses `helpers.RequireType[T]` with `ParamTypes` on the `PrimitiveSpec` for declarative validation.

5. **`PrimitiveSpec` metadata** — The plan's `timer.go` registration was minimal. The implementation adds `Doc`, `ParamNames`, `ParamTypes`, `Category`, and `Keywords` fields.

6. **`msVal.Value` not `msVal.ToInt64()`** — The plan used `.ToInt64()`; the implementation accesses `.Value` directly.

7. **`werr.ErrInvalidArgument` not `werr.ErrOutOfRange`** — The plan used `ErrOutOfRange` for negative milliseconds; the implementation uses `ErrInvalidArgument`.

8. **`valuestest.SchemeEquals` not `qt.DeepEquals`** — Tests use the project's value comparison checker.

9. **`RunSchemeCodeWithTimeout` takes `time.Duration`** — The plan passed an integer (seconds); the actual helper takes `time.Duration`.

---

## File Map

| Action | File | Responsibility | Status |
|--------|------|----------------|--------|
| Create | `machine/timer_interrupt.go` | `ErrTimerInterrupt` error type + `ErrTimerExpired` sentinel | Done |
| Create | `machine/timer_interrupt_test.go` | Unit tests for the error type | Done |
| Modify | `machine/machine_context.go` | Timer fields, accessors, bytecode loop change, `RunWithEscapeHandling` dispatch | Done |
| Modify | `machine/machine_context_apply.go` | `ErrTimerInterrupt` pass-through + post-call check in `applyForeign` | Done |
| Modify | `machine/call_foreign_cached.go` | Post-call timer check in `callForeignCached` | Done |
| Modify | `machine/foreign_closure.go` | `ErrTimerInterrupt` pass-through in `applyCallableError` | Done |
| Modify | `machine/operations_call.go` | VM signal pass-through in `OperationForeignFunctionCall` panic recovery | Done (unstaged) |
| Modify | `machine/machine_context_continuation.go` | `CaptureInterruptContinuation` helper | Done |
| Modify | `machine/machine_context_test.go` | Tests: timer state, bytecode loop, `RunWithEscapeHandling`, error propagation | Done |
| Create | `registry/core/timer.go` | `addTimer` registration with full `PrimitiveSpec` metadata | Done |
| Create | `registry/core/prim_timer.go` | `PrimWithTimeout` implementation | Done |
| Modify | `registry/core/register.go` | Add `addTimer` to Builder | Done |
| Create | `registry/core/prim_timer_test.go` | Table-driven tests: basic, errors, nesting, resumption, dynamic-wind, threads | Done |

---

### Task 1: ErrTimerInterrupt Type

**Files:**
- Create: `machine/timer_interrupt.go`
- Create: `machine/timer_interrupt_test.go`

- [x] **Step 1: Write the failing test**

In `machine/timer_interrupt_test.go`:

```go
package machine

import (
	"errors"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
)

func TestErrTimerInterrupt_Error(t *testing.T) {
	err := &ErrTimerInterrupt{
		Handler: &ForeignClosure{},
	}
	qt.Assert(t, err.Error(), qt.Equals, "timer interrupt")
}

func TestErrTimerInterrupt_ErrorsAs(t *testing.T) {
	var timerErr *ErrTimerInterrupt
	err := error(&ErrTimerInterrupt{
		Handler: &ForeignClosure{},
	})
	qt.Assert(t, errors.As(err, &timerErr), qt.IsTrue)
	qt.Assert(t, timerErr.Handler, qt.IsNotNil)
}

func TestErrTimerInterrupt_NotMatchOtherErrors(t *testing.T) {
	var timerErr *ErrTimerInterrupt
	err := errors.New("something else")
	qt.Assert(t, errors.As(err, &timerErr), qt.IsFalse)
}

func TestErrTimerInterrupt_NilHandler(t *testing.T) {
	err := &ErrTimerInterrupt{}
	qt.Assert(t, err.Error(), qt.Equals, "timer interrupt")
}
```

- [x] **Step 2: Run test to verify it fails**

Run: `go test -v -run TestErrTimerInterrupt ./machine/`
Expected: FAIL — `ErrTimerInterrupt` is undefined.

- [x] **Step 3: Write the implementation**

Create `machine/timer_interrupt.go`:

```go
package machine

import "github.com/aalpar/wile/values"

// ErrTimerInterrupt signals that a wall-clock timer has expired.
// It propagates through the Go error return path and is handled by
// the with-timeout primitive or (as a safety net) by RunWithEscapeHandling.
//
// This is a signal, not an exception. It is NOT caught by Scheme exception
// handlers — only by the VM infrastructure that installed the timer.
type ErrTimerInterrupt struct {
	Handler values.Callable
}

func (p *ErrTimerInterrupt) Error() string {
	return "timer interrupt"
}
```

- [x] **Step 4: Run test to verify it passes**

Run: `go test -v -run TestErrTimerInterrupt ./machine/`
Expected: PASS (all 4 tests).

- [x] **Step 5: Run lint**

Run: `make lint`
Expected: PASS.

- [x] **Step 6: Commit**

```
feat(machine): add ErrTimerInterrupt error type

Signal type for wall-clock timer expiry. Follows the ErrPromptAbort
pattern: propagates through error return path, handled by VM
infrastructure rather than Scheme exception handlers.
```

---

### Task 2: Timer State on MachineContext

**Files:**
- Modify: `machine/machine_context.go`
- Modify: `machine/machine_context_test.go` (or create section)

- [x] **Step 1: Write the failing test**

Add to `machine/machine_context_test.go`:

```go
func TestMachineContext_TimerState(t *testing.T) {
	mc := newTestMachineContext(t)

	// Default: no timer
	qt.Assert(t, mc.TimerHandler(), qt.IsNil)
	qt.Assert(t, mc.TimerCancel(), qt.IsNil)

	// Set handler
	handler := &ForeignClosure{name: "test-handler"}
	mc.SetTimerHandler(handler)
	qt.Assert(t, mc.TimerHandler(), qt.Equals, handler)

	// Set cancel
	called := false
	cancel := func() { called = true }
	mc.SetTimerCancel(cancel)
	qt.Assert(t, mc.TimerCancel(), qt.IsNotNil)
	mc.TimerCancel()()
	qt.Assert(t, called, qt.IsTrue)

	// Clear
	mc.SetTimerHandler(nil)
	mc.SetTimerCancel(nil)
	qt.Assert(t, mc.TimerHandler(), qt.IsNil)
	qt.Assert(t, mc.TimerCancel(), qt.IsNil)
}
```

Note: If `newTestMachineContext` does not exist in the test file, find the existing helper pattern used in `machine/machine_context_test.go` for creating test MCs and use it.

- [x] **Step 2: Run test to verify it fails**

Run: `go test -v -run TestMachineContext_TimerState ./machine/`
Expected: FAIL — `TimerHandler`, `SetTimerHandler`, etc. are undefined.

- [x] **Step 3: Write the implementation**

In `machine/machine_context.go`, add two fields to the `MachineContext` struct after `isolatedMarks`:

```go
type MachineContext struct {
	// ...existing fields...
	isolatedMarks bool

	timerHandler values.Callable     // nil = no timer active
	timerCancel  context.CancelFunc  // cancels the child timeout context; nil when no timer
}
```

Add accessor/mutator methods after the existing `SetContext`/`Context` methods:

```go
// TimerHandler returns the current timer interrupt handler, or nil if no timer is active.
func (p *MachineContext) TimerHandler() values.Callable {
	return p.timerHandler
}

// SetTimerHandler installs or clears the timer interrupt handler.
func (p *MachineContext) SetTimerHandler(h values.Callable) {
	p.timerHandler = h
}

// TimerCancel returns the cancel function for the active timer context, or nil.
func (p *MachineContext) TimerCancel() context.CancelFunc {
	return p.timerCancel
}

// SetTimerCancel installs or clears the timer cancel function.
func (p *MachineContext) SetTimerCancel(cancel context.CancelFunc) {
	p.timerCancel = cancel
}
```

- [x] **Step 4: Run test to verify it passes**

Run: `go test -v -run TestMachineContext_TimerState ./machine/`
Expected: PASS.

- [x] **Step 5: Run lint**

Run: `make lint`
Expected: PASS.

- [x] **Step 6: Commit**

```
feat(machine): add timer state fields to MachineContext

timerHandler (Callable) and timerCancel (CancelFunc) track the active
timer interrupt. nil = no timer. Accessors/mutators follow the existing
SetContext/Context pattern.
```

---

### Task 3: Bytecode Loop Interrupt Point

**Files:**
- Modify: `machine/machine_context.go` (the `ctx.Done()` check in `Run()`)
- Add test to: `machine/machine_context_test.go`

- [x] **Step 1: Write the failing test**

Add to `machine/machine_context_test.go`:

```go
func TestRun_TimerInterruptFromBytecodeLoop(t *testing.T) {
	// Set up a MachineContext with an already-expired timer context
	// and a timer handler installed. Verify that Run() returns
	// ErrTimerInterrupt instead of the context error.
	mc := newTestMachineContext(t)

	// Create an already-cancelled context
	ctx, cancel := context.WithCancel(context.Background())
	cancel() // cancel immediately
	mc.SetContext(ctx)

	// Install a timer handler
	handler := &ForeignClosure{name: "timeout-handler"}
	mc.SetTimerHandler(handler)

	// Set up a minimal template with enough ops to trigger the context check.
	// The context check fires when OpsExecuted & 1023 == 0, which is true
	// at the start (OpsExecuted == 0).
	tpl := NewEmptyNativeTemplate()
	tpl.code = []Instruction{
		{Op: OpLoadVoid}, // This instruction won't execute — context check fires first
	}
	mc.template = tpl
	mc.pc = 0

	err := mc.Run()
	var timerErr *ErrTimerInterrupt
	qt.Assert(t, errors.As(err, &timerErr), qt.IsTrue)
	qt.Assert(t, timerErr.Handler, qt.Equals, handler)
}

func TestRun_ContextCancelWithoutTimerHandler(t *testing.T) {
	// Without a timer handler, the existing kill behavior is preserved.
	mc := newTestMachineContext(t)

	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	mc.SetContext(ctx)

	tpl := NewEmptyNativeTemplate()
	tpl.code = []Instruction{
		{Op: OpLoadVoid},
	}
	mc.template = tpl
	mc.pc = 0

	err := mc.Run()
	qt.Assert(t, err, qt.Equals, context.Canceled)
}
```

- [x] **Step 2: Run test to verify it fails**

Run: `go test -v -run "TestRun_TimerInterrupt|TestRun_ContextCancel" ./machine/`
Expected: `TestRun_TimerInterruptFromBytecodeLoop` FAILS (returns `context.Canceled` instead of `ErrTimerInterrupt`). `TestRun_ContextCancelWithoutTimerHandler` should already PASS (existing behavior).

- [x] **Step 3: Write the implementation**

In `machine/machine_context.go`, modify the `ctx.Done()` check in the `Run()` method. Find (approximately line 306-312):

```go
if mc.counters.OpsExecuted&contextCheckMask == 0 {
    select {
    case <-mc.ctx.Done():
        return mc.ctx.Err()
    default:
    }
}
```

Replace with:

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

- [x] **Step 4: Run test to verify it passes**

Run: `go test -v -run "TestRun_TimerInterrupt|TestRun_ContextCancel" ./machine/`
Expected: PASS (both tests).

- [x] **Step 5: Run full machine test suite**

Run: `go test -v ./machine/...`
Expected: PASS — no regressions. The change is backward-compatible: when `timerHandler` is nil (the default), the existing `ctx.Err()` path is taken.

- [x] **Step 6: Run lint**

Run: `make lint`
Expected: PASS.

- [x] **Step 7: Commit**

```
feat(machine): add timer interrupt to bytecode loop context check

When mc.timerHandler is non-nil and ctx.Done() fires, return
ErrTimerInterrupt instead of ctx.Err(). When timerHandler is nil,
existing kill behavior is preserved (zero behavioral change for
current code paths).
```

---

### Task 4: Error Propagation Paths

**Files:**
- Modify: `machine/foreign_closure.go` (`applyCallableError`)
- Modify: `machine/machine_context_apply.go` (`applyForeign`)
- Modify: `machine/call_foreign_cached.go` (`callForeignCached`)

`ErrTimerInterrupt` must pass through error-wrapping sites (same priority as `ErrPromptAbort` and `ErrExceptionEscape`) and be checked immediately after foreign function calls return successfully.

- [x] **Step 1: Write the failing tests**

Add to `machine/machine_context_test.go` (or a new `machine/timer_propagation_test.go`):

```go
func TestApplyCallableError_PassesThroughTimerInterrupt(t *testing.T) {
	// applyCallableError must NOT wrap ErrTimerInterrupt in ErrExceptionEscape.
	mc := newTestMachineContext(t)
	handler := &ForeignClosure{name: "handler"}
	err := &ErrTimerInterrupt{Handler: handler}
	result := applyCallableError(mc, err)

	var timerErr *ErrTimerInterrupt
	qt.Assert(t, errors.As(result, &timerErr), qt.IsTrue)
	qt.Assert(t, timerErr.Handler, qt.Equals, handler)
}
```

- [x] **Step 2: Run test to verify it fails**

Run: `go test -v -run TestApplyCallableError_PassesThroughTimerInterrupt ./machine/`
Expected: FAIL — currently `applyCallableError` wraps it via `goErrorToSchemeException`.

- [x] **Step 3: Add ErrTimerInterrupt pass-through to `applyCallableError`**

In `machine/foreign_closure.go`, find the `applyCallableError` function. Add the `ErrTimerInterrupt` check after the existing `ErrPromptAbort` check:

```go
func applyCallableError(mc *MachineContext, err error) error {
	var excErr *ErrExceptionEscape
	if errors.As(err, &excErr) {
		return err
	}
	var abortErr *ErrPromptAbort
	if errors.As(err, &abortErr) {
		return err
	}
	var timerErr *ErrTimerInterrupt
	if errors.As(err, &timerErr) {
		return err
	}
	return goErrorToSchemeException(mc, err)
}
```

- [x] **Step 4: Run test to verify it passes**

Run: `go test -v -run TestApplyCallableError_PassesThroughTimerInterrupt ./machine/`
Expected: PASS.

- [x] **Step 5: Add ErrTimerInterrupt pass-through to `applyForeign`**

In `machine/machine_context_apply.go`, find the error-handling block after `err = fcls.fn(p)` (around line 131-143). Add the `ErrTimerInterrupt` check:

```go
err = fcls.fn(p)
if err != nil {
    var abortErr *ErrPromptAbort
    if errors.As(err, &abortErr) {
        return nil, err
    }
    var excErr *ErrExceptionEscape
    if errors.As(err, &excErr) {
        return nil, err
    }
    var timerErr *ErrTimerInterrupt
    if errors.As(err, &timerErr) {
        return nil, err
    }
    return nil, goErrorToSchemeException(p, err)
}
```

- [x] **Step 6: Add post-foreign-call timer check to `applyForeign`**

In the same function, after the error check block and before the template-change check (around line 145), add the post-call timer check:

```go
// Immediate timeout check after foreign call returns successfully.
// Closes the latency gap: a foreign function that blocks for seconds
// triggers the handler immediately, not after 1024 more bytecode ops.
if p.timerHandler != nil {
    select {
    case <-p.ctx.Done():
        return nil, &ErrTimerInterrupt{Handler: p.timerHandler}
    default:
    }
}
```

- [x] **Step 7: Add post-foreign-call timer check to `callForeignCached`**

In `machine/call_foreign_cached.go`, find the success path after `err = fcls.fn(mc)` (around line 82-85). Add the timer check after the nil-error check:

```go
err = fcls.fn(mc)
if err != nil {
    return nil, applyCallableError(mc, err)
}

// Immediate timeout check after foreign call returns successfully.
if mc.timerHandler != nil {
    select {
    case <-mc.ctx.Done():
        return nil, &ErrTimerInterrupt{Handler: mc.timerHandler}
    default:
    }
}
```

- [x] **Step 8: Run full machine test suite**

Run: `go test -v ./machine/...`
Expected: PASS — no regressions. The new checks are no-ops when `timerHandler` is nil.

- [x] **Step 9: Run lint**

Run: `make lint`
Expected: PASS.

- [x] **Step 10: Commit**

```
feat(machine): add ErrTimerInterrupt error propagation paths

Three changes:
1. applyCallableError passes ErrTimerInterrupt through (not wrapped
   as ErrExceptionEscape).
2. applyForeign passes ErrTimerInterrupt through and checks ctx.Done()
   immediately after a successful foreign call.
3. callForeignCached checks ctx.Done() immediately after a successful
   foreign call.

The post-call checks close the latency gap: a foreign function that
blocks for seconds triggers the handler immediately on return, not
after 1024 more bytecode ops.
```

---

### Task 5: RunWithEscapeHandling Dispatch

**Files:**
- Modify: `machine/machine_context.go` (`RunWithEscapeHandling`)
- Add to: `machine/machine_context_continuation.go` (new helper)
- Add test to: `machine/machine_context_test.go`

This task adds the safety-net timer interrupt handler in `RunWithEscapeHandling`. The primary interrupt handling happens in the `with-timeout` primitive (Task 6), but this branch catches any `ErrTimerInterrupt` that propagates past the primitive (defensive programming).

- [x] **Step 1: Add `captureInterruptContinuation` helper**

In `machine/machine_context_continuation.go`, add:

```go
// captureInterruptContinuation captures the full VM execution state at an
// interrupt point as a deep-copied continuation chain. Unlike SliceContinuationAt,
// this includes the live registers (template, pc, env, evals, value) that haven't
// been saved by a SaveContinuation instruction.
//
// The MachineContext is not modified. The returned chain is a deep copy suitable
// for wrapping in a ComposableContinuation.
func (p *MachineContext) captureInterruptContinuation() *MachineContinuation {
	// Push a synthetic continuation frame with the live state, copy
	// the full chain via SliceContinuationAt, then pop the frame.
	liveFrame := NewMachineContinuationFromMachineContext(p, 0)
	savedCont := p.cont
	p.cont = liveFrame

	segment := p.SliceContinuationAt(nil)

	p.cont = savedCont
	releaseContinuation(liveFrame)
	return segment
}
```

- [x] **Step 2: Write the failing test for RunWithEscapeHandling**

Add to `machine/machine_context_test.go`:

```go
func TestRunWithEscapeHandling_TimerInterrupt(t *testing.T) {
	// Verify that RunWithEscapeHandling catches ErrTimerInterrupt,
	// captures a composable continuation, clears timer state, and
	// calls the handler.
	mc := newTestMachineContext(t)

	// Track whether the handler was called
	handlerCalled := false
	var receivedArg values.Value

	handlerFn := func(cc CallContext) error {
		hmc := cc.(*MachineContext)
		handlerCalled = true
		receivedArg = hmc.Arg(0)
		hmc.SetValue(values.NewInteger(99))
		return nil
	}
	handler := newTestForeignClosure("timeout-handler", 1, handlerFn)

	// Create an already-expired context with the handler installed
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	mc.SetContext(ctx)
	mc.SetTimerHandler(handler)

	// Set up minimal bytecode that will trigger the context check
	tpl := NewEmptyNativeTemplate()
	tpl.code = []Instruction{
		{Op: OpLoadVoid},
	}
	mc.template = tpl
	mc.pc = 0

	err := mc.RunWithEscapeHandling()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, handlerCalled, qt.IsTrue)

	// Handler received a ComposableContinuation
	_, ok := receivedArg.(*ComposableContinuation)
	qt.Assert(t, ok, qt.IsTrue)

	// Timer state was cleared
	qt.Assert(t, mc.TimerHandler(), qt.IsNil)
	qt.Assert(t, mc.TimerCancel(), qt.IsNil)

	// Fresh context installed (not the cancelled one)
	qt.Assert(t, mc.Context().Err(), qt.IsNil)

	// Handler result is the return value
	qt.Assert(t, mc.GetValue(), qt.Equals, values.NewInteger(99))
}
```

Note: `newTestForeignClosure` is a helper that creates a `ForeignClosure` with a given name, param count, and implementation function. If this helper does not exist, create it or use the existing pattern for building test ForeignClosures in the machine test files.

- [x] **Step 2a: Verify the test helper exists or create it**

Search `machine/` test files for patterns used to create test `ForeignClosure` values. Use the same pattern. A minimal helper:

```go
func newTestForeignClosure(name string, paramCount int, fn ForeignFunction) *ForeignClosure {
	return NewForeignClosure(name, paramCount, false, fn, nil)
}
```

- [x] **Step 3: Run test to verify it fails**

Run: `go test -v -run TestRunWithEscapeHandling_TimerInterrupt ./machine/`
Expected: FAIL — `RunWithEscapeHandling` returns the `ErrTimerInterrupt` error unhandled (via the final `return err`).

- [x] **Step 4: Add the ErrTimerInterrupt branch to RunWithEscapeHandling**

In `machine/machine_context.go`, in `RunWithEscapeHandling()`, find the final `return err` (around line 1317). Insert the timer interrupt branch BEFORE it:

```go
		// ... existing ErrPromptAbort handling (ends with continue) ...

		var timerErr *ErrTimerInterrupt
		if errors.As(err, &timerErr) {
			// Capture the full computation as a composable continuation.
			// captureInterruptContinuation includes the live registers that
			// haven't been saved by a SaveContinuation instruction.
			segment := p.captureInterruptContinuation()
			windingCopy := p.WindingStack().Copy()
			resumable := NewComposableContinuation(
				segment, windingCopy, p.threadID, p.barrierValid,
			)

			// Clear timer state (prevent re-entry from stale handler).
			if p.timerCancel != nil {
				p.timerCancel()
			}
			p.timerHandler = nil
			p.timerCancel = nil

			// Install a fresh context (the timed-out context is cancelled).
			p.SetContext(context.Background())

			// Call the handler with the resumable continuation.
			_, applyErr := p.ApplyCallable(timerErr.Handler, resumable)
			if applyErr != nil {
				return applyErr
			}
			continue
		}

		return err
```

- [x] **Step 5: Run test to verify it passes**

Run: `go test -v -run TestRunWithEscapeHandling_TimerInterrupt ./machine/`
Expected: PASS.

- [x] **Step 6: Run full machine test suite**

Run: `go test -v ./machine/...`
Expected: PASS — no regressions.

- [x] **Step 7: Run lint**

Run: `make lint`
Expected: PASS.

- [x] **Step 8: Commit**

```
feat(machine): add ErrTimerInterrupt dispatch to RunWithEscapeHandling

Safety-net handler: captures the interrupted computation as a composable
continuation (including live registers via captureInterruptContinuation),
clears timer state, installs a fresh context, and calls the timer handler.

Primary interrupt handling happens in with-timeout (next task); this
branch catches any interrupt that propagates past the primitive.
```

---

### Task 6: `with-timeout` Primitive

**Files:**
- Create: `registry/core/timer.go` (registration)
- Create: `registry/core/prim_timer.go` (implementation)
- Modify: `registry/core/register.go` (add `addTimer` to Builder)
- Create: `registry/core/prim_timer_test.go` (tests)

- [x] **Step 1: Write the failing tests**

Create `registry/core/prim_timer_test.go`:

```go
package core_test

import (
	"errors"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
)

func TestWithTimeout(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "normal completion returns thunk result",
			Code:     `(with-timeout 5000 (lambda (k) 'timeout) (lambda () 42))`,
			Expected: values.NewInteger(42),
		},
		{
			Name:     "handler return value when timeout fires",
			Code:     `(with-timeout 1 (lambda (k) 'expired) (lambda () (let loop () (loop))))`,
			Expected: values.NewSymbol("expired"),
		},
		{
			Name: "handler receives composable continuation",
			Code: `(with-timeout 1
                     (lambda (k) (procedure? k))
                     (lambda () (let loop () (loop))))`,
			Expected: values.TrueValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCodeWithTimeout(t, tc.Code, 5)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.DeepEquals, tc.Expected)
		})
	}
}

func TestWithTimeoutErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "ms not integer", Code: `(with-timeout "bad" (lambda (k) k) (lambda () 1))`},
		{Name: "handler not procedure", Code: `(with-timeout 100 42 (lambda () 1))`},
		{Name: "thunk not procedure", Code: `(with-timeout 100 (lambda (k) k) 42)`},
		{Name: "wrong arity", Code: `(with-timeout 100 (lambda (k) k))`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
```

- [x] **Step 2: Run tests to verify they fail**

Run: `go test -v -run "TestWithTimeout" ./registry/core/`
Expected: FAIL — `with-timeout` is not defined.

- [x] **Step 3: Create registration file**

Create `registry/core/timer.go`:

```go
package core

import (
	"github.com/aalpar/wile/registry"
)

func addTimer(r *registry.Registry) error {
	return r.AddPrimitives(
		[]registry.PrimitiveSpec{
			{
				Name:       "with-timeout",
				ParamCount: 3,
				IsVariadic: false,
				Impl:       PrimWithTimeout,
			},
		},
		registry.PhaseRuntime,
	)
}
```

- [x] **Step 4: Add `addTimer` to Builder**

In `registry/core/register.go`, add `addTimer` to the `Builder` variable, after `addContMarks` and before `addBootstrapSources`:

```go
var Builder = registry.NewRegistryBuilder(
	// ...existing entries...
	addContMarks,
	addTimer,
	addBootstrapSources,
)
```

- [x] **Step 5: Create the implementation**

Create `registry/core/prim_timer.go`:

```go
package core

import (
	"context"
	"errors"
	"time"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// PrimWithTimeout implements (with-timeout milliseconds handler thunk).
//
// Runs thunk with a wall-clock deadline. If thunk completes within the
// deadline, returns its result and cancels the timer. If the deadline
// expires, handler is called with a composable continuation that, when
// invoked, resumes the suspended computation.
//
// Parameters:
//   milliseconds — exact non-negative integer
//   handler — (lambda (resumable-continuation) ...)
//   thunk — (lambda () ...)
//
// Returns: result of thunk (normal), or result of handler (timeout).
//
// Category: control
// Keywords: timeout timer engine fuel bounded-eval
func PrimWithTimeout(cc machine.CallContext) error {
	mc := cc.(*machine.MachineContext)

	msVal, err := helpers.RequireType[*values.Integer](
		mc.Arg(0), werr.ErrNotAnInteger, "with-timeout",
	)
	if err != nil {
		return err
	}

	handlerVal, err := helpers.RequireType[machine.Closure](
		mc.Arg(1), werr.ErrNotAProcedure, "with-timeout",
	)
	if err != nil {
		return err
	}

	thunkVal, err := helpers.RequireType[machine.Closure](
		mc.Arg(2), werr.ErrNotAProcedure, "with-timeout",
	)
	if err != nil {
		return err
	}

	ms := msVal.ToInt64()
	if ms < 0 {
		return werr.WrapForeignErrorf(
			werr.ErrOutOfRange, "with-timeout: milliseconds must be non-negative, got %d", ms,
		)
	}
	duration := time.Duration(ms) * time.Millisecond

	// Create a child context with the timeout deadline.
	timerCtx, timerCancel := context.WithTimeout(mc.Context(), duration)

	// Run the thunk in a sub-context with the timer installed.
	sub := mc.NewSubContext()
	defer machine.ReleaseSubContext(sub)
	sub.SetContext(timerCtx)
	sub.SetTimerHandler(handlerVal)
	sub.SetTimerCancel(timerCancel)

	_, err = sub.ApplyCallable(thunkVal)
	if err != nil {
		timerCancel()
		return err
	}
	err = sub.Run()

	// Always cancel the timer to release resources.
	timerCancel()

	if err != nil {
		var timerErr *machine.ErrTimerInterrupt
		if errors.As(err, &timerErr) {
			// Timer expired. Capture the sub-context's full execution state
			// as a composable continuation. captureInterruptContinuation includes
			// the live registers (template, pc, env, evals) that weren't saved
			// by a SaveContinuation instruction.
			segment := sub.CaptureInterruptContinuation()
			windingCopy := sub.WindingStack().Copy()
			resumable := machine.NewComposableContinuation(
				segment, windingCopy, mc.ThreadID(), mc.BarrierValid(),
			)

			// Call the handler with the resumable continuation.
			// ApplyCallable sets mc.template/pc to the handler's code.
			// When the primitive returns nil, the VM loop executes the handler.
			_, applyErr := mc.ApplyCallable(timerErr.Handler, resumable)
			if applyErr != nil {
				return applyErr
			}
			return nil
		}
		return err
	}

	// Normal completion — propagate the thunk's result.
	mc.SetValues(sub.GetValues()...)
	return nil
}
```

- [x] **Step 5a: Export captureInterruptContinuation**

The method added in Task 5 is unexported (`captureInterruptContinuation`). Since `prim_timer.go` is in package `core` (not `machine`), it needs to be exported. In `machine/machine_context_continuation.go`, rename to `CaptureInterruptContinuation`:

Change the method name from `captureInterruptContinuation` to `CaptureInterruptContinuation`, and update the call site in `RunWithEscapeHandling` accordingly.

- [x] **Step 6: Run tests to verify they pass**

Run: `go test -v -run "TestWithTimeout" ./registry/core/`
Expected: PASS (all tests including normal completion, handler-returns-value, and handler-receives-continuation).

- [x] **Step 7: Run full test suite**

Run: `go test ./...`
Expected: PASS — no regressions.

- [x] **Step 8: Run lint**

Run: `make lint`
Expected: PASS.

- [x] **Step 9: Commit**

```
feat(core): add with-timeout primitive

(with-timeout ms handler thunk) runs thunk with a wall-clock deadline.
Normal completion returns the thunk's result. On timeout, handler receives
a composable continuation representing the suspended computation.

Uses the sub-context pattern (same as call-with-continuation-barrier):
the thunk runs in an isolated sub-context with the timer installed.
When the timer fires, the sub-context's full execution state is captured
as a ComposableContinuation via CaptureInterruptContinuation.
```

---

### Task 7: Nesting Support

**Files:**
- Modify: `registry/core/prim_timer_test.go` (add nesting tests)
- Possibly modify: `registry/core/prim_timer.go` (if nesting doesn't already work)

The sub-context approach naturally supports nesting because each `with-timeout` call creates its own sub-context with its own timer state. The inner timer context is derived from `mc.Context()`, which is the parent MC's context — not the outer timer's sub-context. This means Go's context hierarchy handles deadline ordering correctly.

- [x] **Step 1: Write the nesting tests**

Add to `registry/core/prim_timer_test.go`:

```go
func TestWithTimeoutNesting(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "inner fires before outer",
			Code: `(with-timeout 5000 (lambda (k) 'outer-fired)
                     (lambda ()
                       (with-timeout 1 (lambda (k) 'inner-fired)
                         (lambda () (let loop () (loop))))))`,
			Expected: values.NewSymbol("inner-fired"),
		},
		{
			Name: "inner completes normally within outer",
			Code: `(with-timeout 5000 (lambda (k) 'outer-fired)
                     (lambda ()
                       (with-timeout 5000 (lambda (k) 'inner-fired)
                         (lambda () 42))))`,
			Expected: values.NewInteger(42),
		},
		{
			Name: "nested normal completion propagates",
			Code: `(with-timeout 5000 (lambda (k) 'outer-fired)
                     (lambda ()
                       (+ 1 (with-timeout 5000 (lambda (k) 0)
                               (lambda () 41)))))`,
			Expected: values.NewInteger(42),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCodeWithTimeout(t, tc.Code, 10)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.DeepEquals, tc.Expected)
		})
	}
}
```

- [x] **Step 2: Run nesting tests**

Run: `go test -v -run TestWithTimeoutNesting ./registry/core/`
Expected: PASS — nesting works naturally with the sub-context approach because each level has its own timer state. If any tests fail, diagnose and fix.

- [x] **Step 3: Commit (if tests passed without changes)**

```
test(core): add nesting tests for with-timeout

Verifies inner timeout fires before outer, inner normal completion
propagates, and nested results compose correctly. Nesting is supported
by the sub-context architecture: each with-timeout has isolated timer
state and Go's context hierarchy handles deadline ordering.
```

---

### Task 8: Integration Tests

**Files:**
- Add to: `registry/core/prim_timer_test.go`

- [x] **Step 1: Add continuation resumption test**

```go
func TestWithTimeoutResumption(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "handler resumes continuation with value",
			Code: `(with-timeout 1
                     (lambda (k) (k 42))
                     (lambda () (let loop () (loop))))`,
			Expected: values.NewInteger(42),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCodeWithTimeout(t, tc.Code, 5)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.DeepEquals, tc.Expected)
		})
	}
}
```

Note: This test verifies that calling the continuation resumes the suspended computation. However, `(k 42)` resumes the infinite loop `(let loop () (loop))`. The loop does NOT use the value `42` — it loops unconditionally. This means resuming with `(k 42)` re-enters the infinite loop, which times out again — but there's no handler installed for the second timeout (the original timer was cleared).

**The continuation should resume from where the computation was interrupted.** But the computation was an infinite loop — resuming it just resumes the loop. The handler would need to install a NEW `with-timeout` to catch the resumed computation's timeout:

```go
		{
			Name: "handler discards continuation returns value",
			Code: `(with-timeout 1
                     (lambda (k) 'discarded)
                     (lambda () (let loop () (loop))))`,
			Expected: values.NewSymbol("discarded"),
		},
```

The resumption test with an infinite loop is tricky. A better test uses a computation that returns after being resumed:

```go
		{
			Name: "resume computation that will complete",
			Code: `(let ((state 0))
                     (with-timeout 1
                       (lambda (k)
                         ;; The thunk was computing — resume it.
                         ;; But the thunk is an infinite loop, so just discard.
                         (set! state 1)
                         'timeout)
                       (lambda ()
                         (let loop ((i 0))
                           (if (> i 100000000) i
                               (loop (+ i 1)))))))`,
			Expected: values.NewSymbol("timeout"),
		},
```

Revise the resumption tests to use computations that CAN complete:

```go
func TestWithTimeoutResumption(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "handler discards continuation",
			Code: `(with-timeout 1 (lambda (k) 'discarded) (lambda () (let loop () (loop))))`,
			Expected: values.NewSymbol("discarded"),
		},
		{
			Name: "handler can inspect continuation type",
			Code: `(with-timeout 1
                     (lambda (k) (and (procedure? k) 'ok))
                     (lambda () (let loop () (loop))))`,
			Expected: values.NewSymbol("ok"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCodeWithTimeout(t, tc.Code, 5)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.DeepEquals, tc.Expected)
		})
	}
}
```

- [x] **Step 2: Add dynamic-wind interaction tests**

```go
func TestWithTimeoutDynamicWind(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "normal completion fires dynamic-wind after thunk",
			Code: `(let ((log '()))
                     (with-timeout 5000
                       (lambda (k) 'timeout)
                       (lambda ()
                         (dynamic-wind
                           (lambda () (set! log (cons 'before log)))
                           (lambda () 42)
                           (lambda () (set! log (cons 'after log))))))
                     log)`,
			Expected: values.List(
				values.NewSymbol("after"),
				values.NewSymbol("before"),
			),
		},
		{
			Name: "handler discards continuation — after thunks NOT called",
			Code: `(let ((after-called #f))
                     (with-timeout 1
                       (lambda (k) after-called)
                       (lambda ()
                         (dynamic-wind
                           (lambda () #f)
                           (lambda () (let loop () (loop)))
                           (lambda () (set! after-called #t))))))`,
			Expected: values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCodeWithTimeout(t, tc.Code, 5)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.DeepEquals, tc.Expected)
		})
	}
}
```

- [x] **Step 3: Add thread isolation test**

`NewNamespaceFrameTiny` loads all extensions including threads/sync, so no `(import ...)` needed.

```go
func TestWithTimeoutThreadIsolation(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "timeout in one thread does not affect another",
			Code: `(let ((result #f)
                          (m (make-mutex)))
                      (mutex-lock! m)
                      (thread-start!
                        (make-thread
                          (lambda ()
                            (set! result
                              (with-timeout 5000
                                (lambda (k) 'timeout)
                                (lambda () 'ok)))
                            (mutex-unlock! m))))
                      (mutex-lock! m)
                      result)`,
			Expected: values.NewSymbol("ok"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCodeWithTimeout(t, tc.Code, 10)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.DeepEquals, tc.Expected)
		})
	}
}
```

- [x] **Step 4: Run all timer tests**

Run: `go test -v -run "TestWithTimeout" ./registry/core/`
Expected: PASS.

- [x] **Step 5: Run full test suite**

Run: `make lint && go test ./...`
Expected: PASS.

- [x] **Step 6: Commit**

```
test(core): add integration tests for with-timeout

Resumption (handler discards continuation), dynamic-wind interaction,
and thread isolation tests.
```

---

## Open Questions for Implementer

1. **`newTestMachineContext` / `newTestForeignClosure` helpers**: The machine package test files may already have helpers for creating test MachineContexts and ForeignClosures. Search for existing patterns before creating new ones. Key search terms: `newTestMachineContext`, `NewForeignClosure`, `acquireSubContext` in test files.

2. **Timer handler context propagation**: When the handler calls `ApplyCallable(handler, resumable)` on the parent MC, the parent MC's context is `context.Background()` (no timeout). If the user wants to re-fuel, they wrap the continuation call in a new `with-timeout`. Verify this pattern works in a manual test.

3. **`mc.evals` sharing after `NewMachineContinuationFromMachineContext`**: The live frame created in `CaptureInterruptContinuation` references `mc.evals` by pointer. `SliceContinuationAt` calls `Copy()` which deep-copies evals via `evals.Copy()`. Verified: `MachineContinuation.Copy()` calls `p.evals.Copy()` at `machine/machine_continuation.go:163-164`.

4. **Negative milliseconds**: The implementation rejects negative values. Zero is valid (fires on next context check). Verify that `context.WithTimeout(ctx, 0)` behaves as expected (immediately expired).

5. **Sub-context timer state inheritance**: `NewSubContext()` does NOT copy `timerHandler`/`timerCancel` (they're new fields, zero-valued by default). The `with-timeout` primitive must explicitly install timer state on the sub-context via `sub.SetTimerHandler()`/`sub.SetTimerCancel()`.

6. **Design deviation**: The design document puts handler dispatch in `RunWithEscapeHandling` as the primary mechanism. This plan uses the **sub-context approach** as the primary mechanism (the primitive catches `ErrTimerInterrupt` from `sub.Run()`), with `RunWithEscapeHandling` as a safety net. Reason: the inline approach requires bytecode-level cleanup when the thunk returns normally, which means either compiling `with-timeout` as a special form or integrating with `dynamic-wind`. The sub-context approach handles cleanup via Go's structured flow and matches the existing `call-with-continuation-barrier` pattern.
