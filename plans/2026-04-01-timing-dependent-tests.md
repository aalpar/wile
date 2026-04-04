# Timing-Dependent Concurrency Tests

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Replace 10 `time.Sleep` calls in test code with observation-based synchronization to eliminate CI flakiness.

**Architecture:** Categorize sleeps into 4 patterns (A-D), apply pattern-specific fixes using polling, Go callbacks via the extension system, and goroutine count stabilization. One sleep (Pattern D: deliberate race) is kept.

**Tech Stack:** Go, quicktest (`qt`), existing `registry.NewExtension` / `WaiterCount()` / `runtime.NumGoroutine()`

---

## Background

11 `time.Sleep` calls across 4 test files assume the scheduler makes sufficient progress in N milliseconds. On loaded CI machines, that assumption breaks. The fix replaces timing assumptions with observations.

### Sleep Inventory

| # | File | Line | Sleep | Pattern | Intent |
|---|------|------|-------|---------|--------|
| 1 | `wile_test.go` | 919 | 200ms | A | Let infinite loop start, then cancel |
| 2 | `extensions/threads/prim_threads_test.go` | 240 | 20ms | A | Let Scheme sleep start, then cancel |
| 3 | `extensions/threads/prim_threads_test.go` | 783 | 200ms | A | Let thread enter loop, then cancel |
| 4 | `values/condition_variable_test.go` | 123 | 50ms | B | Signal after Wait starts |
| 5 | `values/condition_variable_test.go` | 157 | 50ms | B | Broadcast after Wait starts |
| 6 | `values/condition_variable_test.go` | 174 | 50ms | B | Signal (nil timeout case) |
| 7 | `values/condition_variable_test.go` | 215 | 50ms | B | Broadcast (concurrent waiters) |
| 8 | `values/condition_variable_test.go` | 94 | 100ms | C | Goroutine count baseline |
| 9 | `values/condition_variable_test.go` | 108 | 100ms | C | Goroutine count final |
| 10 | `values/channel_test.go` | 262 | 20ms | A | Delay send for Select test |
| 11 | `values/condition_variable_test.go` | 188 | 45ms | D | Deliberate race -- **keep** |

### Pattern Guide

| Pattern | Problem | Fix |
|---------|---------|-----|
| A | Sleep N ms, hope code is running | Inject `(test-ready!)` Scheme primitive that closes a Go channel; wait on channel |
| B | Sleep N ms, hope `cv.Wait` is blocked | Poll `cv.WaiterCount() > 0` |
| C | Sleep N ms, hope goroutines settled | Poll `runtime.NumGoroutine()` for consecutive stable reads |
| D | Deliberate race at timeout boundary | **Keep** -- intentional scheduler exercise |

## Scope

**In scope:**
- `internal/testutil` package with `PollUntil` and `ReadyExtension`
- `stableGoroutineCount` local helper in `condition_variable_test.go`
- Rewrite 10 sleep sites across 4 files
- Keep Pattern D (deliberate race) unchanged

**Not in scope:**
- Adding production API surface
- Changing `ConditionVariable` or `Channel` internals

---

### Task 1: Create `internal/testutil` package

**Files:**
- Create: `internal/testutil/poll.go`
- Create: `internal/testutil/ready_extension.go`

**Step 1:** Create `internal/testutil/poll.go`:

```go
package testutil

import (
	"runtime"
	"time"
)

// PollUntil calls check() repeatedly until it returns true or deadline elapses.
// Returns true if check() succeeded, false on timeout.
func PollUntil(check func() bool, deadline time.Duration) bool {
	end := time.Now().Add(deadline)
	for time.Now().Before(end) {
		if check() {
			return true
		}
		runtime.Gosched()
		time.Sleep(1 * time.Millisecond)
	}
	return false
}
```

**Step 2:** Create `internal/testutil/ready_extension.go`:

```go
package testutil

import (
	"sync"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry"
	"github.com/aalpar/wile/values"
)

// ReadyExtension creates an extension that registers a `test-ready!`
// primitive. When Scheme code calls `(test-ready!)`, it closes the
// returned channel exactly once, providing a deterministic "code is
// running" signal to Go test code.
//
// Each call returns a fresh extension and channel. Do not reuse across
// subtests -- create a new ReadyExtension per subtest to get a fresh channel.
func ReadyExtension() (registry.Extension, <-chan struct{}) {
	ready := make(chan struct{})
	var once sync.Once
	ext := registry.NewExtension("test-ready", func(r *registry.Registry) error {
		r.AddPrimitive(registry.PrimitiveSpec{
			Name:       "test-ready!",
			ParamCount: 0,
			Impl: func(mc *machine.MachineContext) error {
				once.Do(func() {
					close(ready)
				})
				mc.SetValue(values.Void)
				return nil
			},
		}, registry.PhaseRuntime)
		return nil
	})
	return ext, ready
}
```

**Step 3:** Verify build.

Run: `go build ./internal/testutil/...`

**Step 4:** Commit:
```
add internal/testutil package for deterministic test synchronization

Provides PollUntil for observation-based waiting and ReadyExtension
for injecting a Scheme-callable ready signal into test engines.
```

---

### Task 2: Fix Pattern C -- goroutine leak detection

**Files:**
- Modify: `values/condition_variable_test.go`

Pattern C replaces `runtime.GC() + time.Sleep + runtime.NumGoroutine()` with a polling helper that waits for two consecutive stable reads.

**Step 1:** Add `stableGoroutineCount` helper to `values/condition_variable_test.go`. Place it after the import block, before the first test function:

```go
// stableGoroutineCount polls runtime.NumGoroutine() until two consecutive
// reads return the same value, or the deadline elapses.
func stableGoroutineCount(deadline time.Duration) int {
	end := time.Now().Add(deadline)
	prev := runtime.NumGoroutine()
	for time.Now().Before(end) {
		runtime.Gosched()
		runtime.GC()
		time.Sleep(1 * time.Millisecond)
		curr := runtime.NumGoroutine()
		if curr == prev {
			return curr
		}
		prev = curr
	}
	return prev
}
```

**Step 2:** In `TestConditionVariable_Wait_NoGoroutineLeak`, replace the baseline measurement (lines 93-95):

```go
// Before:
runtime.GC()
time.Sleep(100 * time.Millisecond)
baseline := runtime.NumGoroutine()

// After:
baseline := stableGoroutineCount(2 * time.Second)
```

**Step 3:** In the same function, replace the final measurement (lines 107-110):

```go
// Before:
runtime.GC()
time.Sleep(100 * time.Millisecond)

final := runtime.NumGoroutine()

// After:
final := stableGoroutineCount(2 * time.Second)
```

**Step 4:** Run the test.

Run: `go test -v -run TestConditionVariable_Wait_NoGoroutineLeak ./values/...`
Expected: PASS

**Step 5:** Commit:
```
replace GC stabilization sleeps with goroutine count polling

Leak detection test now polls NumGoroutine for consecutive stable
reads instead of assuming 100ms is enough for goroutines to exit.
```

---

### Task 3: Fix Pattern B -- condition variable signal/broadcast tests

**Files:**
- Modify: `values/condition_variable_test.go`

Pattern B replaces `time.Sleep(50ms)` before signal/broadcast with `PollUntil(cv.WaiterCount() > 0)`. This waits until the target goroutine is actually blocked in `cv.Wait`.

**Step 1:** Add import `"github.com/aalpar/wile/internal/testutil"` to the import block.

**Step 2:** In `TestConditionVariable_Wait_SignalBeforeTimeout` (line 122-125), replace the goroutine:

```go
// Before:
go func() {
    time.Sleep(50 * time.Millisecond)
    cv.Signal()
}()

// After:
go func() {
    testutil.PollUntil(func() bool { return cv.WaiterCount() > 0 }, 2*time.Second)
    cv.Signal()
}()
```

**Step 3:** In `TestConditionVariable_Wait_BroadcastBeforeTimeout` (lines 156-159), same transformation:

```go
// Before:
go func() {
    time.Sleep(50 * time.Millisecond)
    cv.Broadcast()
}()

// After:
go func() {
    testutil.PollUntil(func() bool { return cv.WaiterCount() > 0 }, 2*time.Second)
    cv.Broadcast()
}()
```

**Step 4:** In `TestConditionVariable_Wait_NilTimeout` (lines 173-176), same transformation:

```go
// Before:
go func() {
    time.Sleep(50 * time.Millisecond)
    cv.Signal()
}()

// After:
go func() {
    testutil.PollUntil(func() bool { return cv.WaiterCount() > 0 }, 2*time.Second)
    cv.Signal()
}()
```

**Step 5:** In `TestConditionVariable_Wait_ConcurrentWaiters` (line 215), replace the inline sleep. This one is NOT in a goroutine -- it's in the main test body:

```go
// Before:
time.Sleep(50 * time.Millisecond)
cv.Broadcast()

// After:
testutil.PollUntil(func() bool { return cv.WaiterCount() >= numWaiters }, 2*time.Second)
cv.Broadcast()
```

Note: polls for `>= numWaiters` (50), not just `> 0`, to ensure all waiters are blocked before broadcast.

**Step 6:** Run all condvar tests.

Run: `go test -v -run TestConditionVariable ./values/...`
Expected: PASS

**Step 7:** Commit:
```
replace timing sleeps with WaiterCount polling in condvar tests

Signal and broadcast tests now wait until WaiterCount confirms the
target goroutine is blocked, instead of assuming 50ms is enough.
```

---

### Task 4: Fix Pattern A -- cancel during computation (`wile_test.go`)

**Files:**
- Modify: `wile_test.go`

This test starts an infinite loop `(let loop () (loop))` and cancels the context after 200ms. Replace the sleep with `(test-ready!)` injected via `ReadyExtension`.

**Step 1:** Add import `"github.com/aalpar/wile/internal/testutil"` to the import block.

**Step 2:** Rewrite `TestEval_CancelDuringComputation` (lines 912-926). Note: `wile_test.go` is `package wile`, so `NewEngine`, `WithExtension` are called without package prefix:

```go
func TestEval_CancelDuringComputation(t *testing.T) {
	c := qt.New(t)
	ext, ready := testutil.ReadyExtension()
	engine, err := NewEngine(context.Background(), WithExtension(ext))
	c.Assert(err, qt.IsNil)

	ctx, cancel := context.WithCancel(context.Background())
	go func() {
		<-ready
		cancel()
	}()

	_, err = engine.Eval(ctx, engine.MustParse(ctx, "(begin (test-ready!) (let loop () (loop)))"))
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, context.Canceled), qt.IsTrue)
}
```

**Step 3:** Run the test.

Run: `go test -v -run TestEval_CancelDuringComputation .`
Expected: PASS

**Step 4:** Commit:
```
replace timing sleep with ready signal in cancel-during-eval test

Scheme code calls (test-ready!) before its infinite loop, giving
Go test code a deterministic signal to cancel the context.
```

---

### Task 5: Fix Pattern A -- thread cancellation tests

**Files:**
- Modify: `extensions/threads/prim_threads_test.go`

Two tests to fix:

1. `TestThreadSleepContextCancellation` (line 218) -- sleeps 20ms before cancelling, shared engine across subtests
2. `TestThreadParentContextCancellation` (line 762) -- sleeps 200ms before cancelling

**Important:** `TestThreadSleepContextCancellation` shares one engine across two subtests. Since `ReadyExtension` returns a one-shot channel, a shared engine can't reuse it across subtests. Fix: create a fresh engine + extension per subtest.

**Step 1:** Add import `"github.com/aalpar/wile/internal/testutil"` to the import block.

**Step 2:** Rewrite `TestThreadSleepContextCancellation` (lines 218-251). Move Scheme code into the test table with `(test-ready!)` prepended. Create per-subtest engine:

```go
func TestThreadSleepContextCancellation(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		code string
	}{
		{"cancel during integer sleep",
			`(begin (test-ready!) (thread-sleep! 60))`},
		{"cancel during float sleep",
			`(begin (test-ready!) (thread-sleep! 60.0))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			ext, ready := testutil.ReadyExtension()
			engine, err := wile.NewEngine(context.Background(),
				wile.WithExtension(extthreads.Extension),
				wile.WithExtension(ext),
			)
			c.Assert(err, qt.IsNil)

			ctx, cancel := context.WithCancel(context.Background())
			done := make(chan error, 1)
			go func() {
				_, err := engine.Eval(ctx, engine.MustParse(ctx, tc.code))
				done <- err
			}()
			<-ready
			cancel()

			select {
			case err := <-done:
				c.Assert(err, qt.IsNotNil)
			case <-time.After(2 * time.Second):
				t.Fatal("thread-sleep! did not respect context cancellation")
			}
		})
	}
}
```

**Step 3:** Rewrite `TestThreadParentContextCancellation` (lines 762-794). The `(test-ready!)` call goes inside the thread's thunk, so the signal fires once the thread is actually running its loop:

```go
func TestThreadParentContextCancellation(t *testing.T) {
	c := qt.New(t)
	ext, ready := testutil.ReadyExtension()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extthreads.Extension),
		wile.WithExtension(ext),
	)
	c.Assert(err, qt.IsNil)

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	errCh := make(chan error, 1)
	go func() {
		_, err := engine.Eval(ctx, engine.MustParse(ctx,
			`(let ((th (make-thread (lambda () (begin (test-ready!) (let loop () (loop)))))))
			   (thread-start! th)
			   (thread-join! th))`))
		errCh <- err
	}()

	<-ready
	cancel()

	select {
	case err := <-errCh:
		c.Assert(err, qt.IsNotNil)
	case <-time.After(5 * time.Second):
		t.Fatal("scheme evaluation did not return after context cancellation")
	}
}
```

**Step 4:** Run both tests.

Run: `go test -v -run 'TestThreadSleepContext|TestThreadParentContext' ./extensions/threads/...`
Expected: PASS

**Step 5:** Commit:
```
replace timing sleeps with ready signal in thread cancellation tests

Thread sleep and parent cancellation tests now use (test-ready!)
to confirm Scheme code is executing before cancelling the context.
```

---

### Task 6: Fix Pattern A -- channel select test

**Files:**
- Modify: `values/channel_test.go`

`TestChannelSelectBlocking` (line 252) delays a send by 20ms so `ChannelSelect` is blocking when the send arrives. On an unbuffered channel, `Send` blocks until a receiver is ready -- so the sleep is redundant. Removing it is correct: the sender goroutine naturally waits for `ChannelSelect` to enter its receive.

**Step 1:** Remove the sleep (lines 261-263):

```go
// Before:
go func() {
    time.Sleep(20 * time.Millisecond)
    _ = ch.Send(values.NewInteger(99))
}()

// After:
go func() {
    _ = ch.Send(values.NewInteger(99))
}()
```

**Step 2:** Run the test.

Run: `go test -v -run TestChannelSelectBlocking ./values/...`
Expected: PASS

**Step 3:** Commit:
```
remove unnecessary sleep in channel select blocking test

Unbuffered channel semantics already synchronize sender and
receiver -- the 20ms sleep was redundant.
```

---

### Task 7: Lint and full verification

**Step 1:** Run formatter on all changed files:

Run: `goimports -w internal/testutil/poll.go internal/testutil/ready_extension.go values/condition_variable_test.go wile_test.go extensions/threads/prim_threads_test.go values/channel_test.go`

**Step 2:** Run lint.

Run: `make lint`
Expected: clean

**Step 3:** Run all affected tests together.

Run: `go test -v -run 'TestConditionVariable|TestChannelSelectBlocking|TestEval_CancelDuringComputation|TestThreadSleepContext|TestThreadParentContext' ./...`
Expected: all PASS

**Step 4:** Run full test suite.

Run: `make test`
Expected: PASS

**Step 5:** Run coverage check.

Run: `make covercheck`
Expected: PASS

---

## Verification Summary

After all tasks, these three must pass:

```bash
go test -v -run 'TestConditionVariable|TestChannelSelectBlocking|TestEval_Cancel|TestThreadSleepContext|TestThreadParentContext' ./...
make lint
make covercheck
```

## Sleep Accounting

| # | Pattern | Disposition |
|---|---------|-------------|
| 1 | A | Replaced with `ReadyExtension` (Task 4) |
| 2 | A | Replaced with `ReadyExtension` (Task 5) |
| 3 | A | Replaced with `ReadyExtension` (Task 5) |
| 4 | B | Replaced with `WaiterCount` polling (Task 3) |
| 5 | B | Replaced with `WaiterCount` polling (Task 3) |
| 6 | B | Replaced with `WaiterCount` polling (Task 3) |
| 7 | B | Replaced with `WaiterCount` polling (Task 3) |
| 8 | C | Replaced with `stableGoroutineCount` (Task 2) |
| 9 | C | Replaced with `stableGoroutineCount` (Task 2) |
| 10 | A | Removed -- unbuffered channel sync (Task 6) |
| 11 | D | **Kept** -- deliberate race |
