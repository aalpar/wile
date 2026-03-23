# Thread-Safe NoCopyApply Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Status:** Superseded — NoCopyApply removed entirely (PR #561) instead of gating with threadShared latch.

**Original goal:** Prevent data races when SRFI-18 threads concurrently call closures that use the NoCopyApply optimization, by latching an atomic `threadShared` flag on first thread invocation.

**Architecture:** Add `threadShared uint32` to `MachineClosure` and `ForeignClosure`. At `Apply`/`callForeignCached`/`applyForeign` entry, if `mc.threadID != 0`, atomically latch the flag. When latched, force the copy path (allocate fresh env frame from pool). Single-threaded code is unaffected — the flag is never latched, and the read is one `atomic.LoadUint32`.

**Tech Stack:** Go, `sync/atomic`, existing `sync.Pool` env frame recycling.

**Design doc:** `plans/THREAD-SAFE-NOCOPYAPPLY.md`

---

### Task 1: Add `threadShared` field to `MachineClosure`

**Files:**
- Modify: `machine/machine_closure.go:44-47`

**Step 1: Add the field**

In `MachineClosure` struct, add the atomic flag:

```go
type MachineClosure struct {
	env          *environment.EnvironmentFrame
	template     *NativeTemplate
	threadShared uint32 // atomic; 0=single-threaded, 1=thread-shared (latched)
}
```

**Step 2: Add accessor for tests**

Below the struct, add:

```go
// ThreadShared returns the current value of the threadShared flag.
// Used by tests to verify latch behavior.
func (p *MachineClosure) ThreadShared() uint32 {
	return atomic.LoadUint32(&p.threadShared)
}
```

Add `"sync/atomic"` to the import block.

**Step 3: Run existing tests**

Run: `go test ./machine/ -run TestMachineContext_Apply -count=1 -v`
Expected: All existing Apply tests PASS (no behavioral change yet).

**Step 4: Commit**

```
feat(machine): add threadShared field to MachineClosure
```

---

### Task 2: Gate NoCopyApply on `threadShared` in `Apply`

**Files:**
- Modify: `machine/machine_context_apply.go:26-79`

**Step 1: Write the failing test**

Add to `machine/machine_context_apply_test.go`:

```go
func TestApply_ThreadSharedLatch(t *testing.T) {
	c := qt.New(t)
	topEnv := environment.NewNamespace().Runtime()
	lenv := environment.NewLocalEnvironment(1)
	env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)

	// NoCopyApply template: no SaveContinuation, no MakeClosure
	tpl := NewNativeTemplate(1, 0, false)
	cls := NewClosureWithTemplate(tpl, env)

	// Verify NoCopyApply is true
	c.Assert(tpl.NoCopyApply(), qt.IsTrue)

	// Call from primordial thread (threadID == 0) — flag stays 0
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, topEnv))
	_, err := mc.Apply(cls, values.NewInteger(1))
	c.Assert(err, qt.IsNil)
	c.Assert(cls.ThreadShared(), qt.Equals, uint32(0))
	// NoCopyApply path: mc.env IS the closure's env
	c.Assert(mc.env, qt.Equals, cls.Env())

	// Call from non-primordial thread (threadID != 0) — flag latches to 1
	mc2 := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, topEnv))
	thread := values.NewThread(nil, "test")
	mc2.SetThread(thread)
	_, err = mc2.Apply(cls, values.NewInteger(2))
	c.Assert(err, qt.IsNil)
	c.Assert(cls.ThreadShared(), qt.Equals, uint32(1))
	// Copy path: mc2.env is NOT the closure's env
	c.Assert(mc2.env != cls.Env(), qt.IsTrue)

	// Call from primordial again — flag stays 1, copy path taken
	mc3 := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, topEnv))
	_, err = mc3.Apply(cls, values.NewInteger(3))
	c.Assert(err, qt.IsNil)
	c.Assert(cls.ThreadShared(), qt.Equals, uint32(1))
	c.Assert(mc3.env != cls.Env(), qt.IsTrue)
}
```

**Step 2: Run test to verify it fails**

Run: `go test ./machine/ -run TestApply_ThreadSharedLatch -count=1 -v`
Expected: FAIL — `cls.ThreadShared()` returns 0 after thread call, `mc2.env` equals closure env.

**Step 3: Implement the latch and gated branch**

In `machine_context_apply.go`, add `"sync/atomic"` to imports. Replace the NoCopyApply branch (lines 47-72) with:

```go
	// Latch threadShared on first invocation from a non-primordial thread.
	if p.threadID != 0 && atomic.LoadUint32(&mcls.threadShared) == 0 {
		atomic.StoreUint32(&mcls.threadShared, 1)
	}

	if tpl.NoCopyApply() && atomic.LoadUint32(&mcls.threadShared) == 0 {
		// No-copy path: the template contains no SaveContinuation and no
		// MakeClosure, and the closure has never been called from a thread.
		// Safe to mutate the closure's own bindings in place.
		env = mcls.env
		bnds = env.LocalEnvironment().Bindings()
		// envPooled: closure's own env, not from pool.
		p.envPooled = false
		p.counters.NoCopyApplies++
		p.counters.NoCopyBindingsSaved += uint64(len(bnds))
	} else {
		// Copy path: acquire a frame from the pool and populate it.
		// Required when the template has SaveContinuation/MakeClosure,
		// or when the closure has been called from a thread (preventing
		// concurrent parameter corruption on shared binding slots).
		env = acquireEnvFrame()
		mcls.env.InitApplyFrame(env)
		bnds = env.LocalEnvironment().Bindings()
		// envPooled: frame from envFramePool; RestoreAndRelease will recycle it.
		p.envPooled = true
		p.counters.EnvsCopied++
		p.counters.BindingsCopied += uint64(len(bnds))
		p.counters.KeysShared++
	}
```

**Step 4: Run test to verify it passes**

Run: `go test ./machine/ -run TestApply_ThreadSharedLatch -count=1 -v`
Expected: PASS

**Step 5: Run full Apply test suite**

Run: `go test ./machine/ -run TestMachineContext_Apply -count=1 -v`
Expected: All PASS

**Step 6: Commit**

```
feat(machine): gate NoCopyApply on threadShared flag in Apply
```

---

### Task 3: Add `threadShared` field to `ForeignClosure`

**Files:**
- Modify: `machine/foreign_closure.go:60-66`

**Step 1: Add the field and accessor**

```go
type ForeignClosure struct {
	fn           ForeignFunction
	env          *environment.EnvironmentFrame
	paramCount   int
	isVariadic   bool
	name         string
	threadShared uint32 // atomic; 0=single-threaded, 1=thread-shared (latched)
}
```

Add accessor:

```go
// ThreadShared returns the current value of the threadShared flag.
func (p *ForeignClosure) ThreadShared() uint32 {
	return atomic.LoadUint32(&p.threadShared)
}
```

Add `"sync/atomic"` to imports.

**Step 2: Run existing tests**

Run: `go test ./machine/ -run TestApplyForeign -count=1 -v`
Expected: All PASS

**Step 3: Commit**

```
feat(machine): add threadShared field to ForeignClosure
```

---

### Task 4: Add copy path to `callForeignCached`

**Files:**
- Modify: `machine/call_foreign_cached.go:55-68`
- Test: `machine/call_foreign_cached_test.go`

**Step 1: Write the failing test**

Add to `machine/call_foreign_cached_test.go`:

```go
func TestCallForeignCached_ThreadSharedLatch(t *testing.T) {
	c := qt.New(t)
	topEnv := environment.NewNamespace().Runtime()

	// Create a ForeignClosure that records which env it saw.
	var seenEnv *environment.EnvironmentFrame
	fcls := NewForeignClosure(topEnv, 1, false, func(mc *MachineContext) error {
		seenEnv = mc.env
		mc.SetValue(mc.Arg(0))
		return nil
	})

	// Call from primordial — flag stays 0, uses closure's own env
	mc := newTestMC(topEnv)
	mc.evals.Push(values.NewInteger(1))
	tpl := mc.template
	tpl.AppendCachedBinding(fcls.env.GetBinding(values.NewSymbol("__placeholder")))
	// We need to set up a proper cachedBindings entry. Instead, test via applyForeign.
	_, err := mc.applyForeign(fcls, values.NewInteger(1))
	c.Assert(err, qt.IsNil)
	c.Assert(fcls.ThreadShared(), qt.Equals, uint32(0))
	c.Assert(seenEnv, qt.Equals, fcls.Env())

	// Call from thread — flag latches to 1
	mc2 := newTestMC(topEnv)
	thread := values.NewThread(nil, "test")
	mc2.SetThread(thread)
	_, err = mc2.applyForeign(fcls, values.NewInteger(2))
	c.Assert(err, qt.IsNil)
	c.Assert(fcls.ThreadShared(), qt.Equals, uint32(1))
	c.Assert(seenEnv != fcls.Env(), qt.IsTrue)

	// Call from primordial again — flag stays 1, copy path
	mc3 := newTestMC(topEnv)
	seenEnv = nil
	_, err = mc3.applyForeign(fcls, values.NewInteger(3))
	c.Assert(err, qt.IsNil)
	c.Assert(fcls.ThreadShared(), qt.Equals, uint32(1))
	c.Assert(seenEnv != fcls.Env(), qt.IsTrue)
}
```

Note: If `newTestMC` doesn't exist in the test file, create a local helper following the pattern in existing tests (use `NewMachineContext` with a continuation).

**Step 2: Run test to verify it fails**

Run: `go test ./machine/ -run TestCallForeignCached_ThreadSharedLatch -count=1 -v`
Expected: FAIL

**Step 3: Add copy path to `applyForeign`**

In `machine_context_apply.go`, replace lines 93-105 of `applyForeign`:

```go
	p.counters.ClosuresApplied++

	// Latch threadShared on first invocation from a non-primordial thread.
	if p.threadID != 0 && atomic.LoadUint32(&fcls.threadShared) == 0 {
		atomic.StoreUint32(&fcls.threadShared, 1)
	}

	var env *environment.EnvironmentFrame
	var bnds []environment.Binding

	if atomic.LoadUint32(&fcls.threadShared) == 0 {
		// No-copy path: reuse the closure's own env.
		env = fcls.env
		bnds = env.LocalEnvironment().Bindings()
		p.envPooled = false
		p.counters.NoCopyApplies++
		p.counters.NoCopyBindingsSaved += uint64(len(bnds))
	} else {
		// Copy path: closure has been called from a thread.
		env = acquireEnvFrame()
		fcls.env.InitApplyFrame(env)
		bnds = env.LocalEnvironment().Bindings()
		p.envPooled = true
		p.counters.EnvsCopied++
		p.counters.BindingsCopied += uint64(len(bnds))
		p.counters.KeysShared++
	}

	bindArgs(bnds, vs, l, fcls.isVariadic, p.buildRestArg)

	p.env = env
```

**Step 4: Add copy path to `callForeignCached`**

In `call_foreign_cached.go`, add `"sync/atomic"` to imports. Replace lines 55-68:

```go
	mc.counters.ClosuresApplied++
	mc.counters.ForeignCalls++
	mc.counters.RecordCall(fcls.name)

	// Latch threadShared on first invocation from a non-primordial thread.
	if mc.threadID != 0 && atomic.LoadUint32(&fcls.threadShared) == 0 {
		atomic.StoreUint32(&fcls.threadShared, 1)
	}

	var env *environment.EnvironmentFrame
	var bnds []environment.Binding

	if atomic.LoadUint32(&fcls.threadShared) == 0 {
		env = fcls.env
		bnds = env.LocalEnvironment().Bindings()
		mc.envPooled = false
		mc.counters.NoCopyApplies++
		mc.counters.NoCopyBindingsSaved += uint64(len(bnds))
	} else {
		env = acquireEnvFrame()
		fcls.env.InitApplyFrame(env)
		bnds = env.LocalEnvironment().Bindings()
		mc.envPooled = true
		mc.counters.EnvsCopied++
		mc.counters.BindingsCopied += uint64(len(bnds))
		mc.counters.KeysShared++
	}

	bindArgs(bnds, vs, l, fcls.isVariadic, mc.buildRestArg)

	mc.env = env
```

**Step 5: Run test to verify it passes**

Run: `go test ./machine/ -run TestCallForeignCached_ThreadSharedLatch -count=1 -v`
Expected: PASS

**Step 6: Run full foreign closure test suite**

Run: `go test ./machine/ -run "TestApplyForeign|TestCallForeignCached" -count=1 -v`
Expected: All PASS

**Step 7: Commit**

```
feat(machine): add copy path to ForeignClosure for thread safety
```

---

### Task 5: Integration test — concurrent SRFI-18 threads

**Files:**
- Modify: `extensions/threads/prim_threads_test.go`

**Step 1: Write the concurrency correctness test**

Add to `extensions/threads/prim_threads_test.go`:

```go
func TestConcurrentNoCopyApplySafety(t *testing.T) {
	c := qt.New(t)

	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extthreads.Extension),
	)
	c.Assert(err, qt.IsNil)

	// tag? compiles to only promoted opcodes (NoCopyApply = true).
	// 8 threads call it concurrently on different data.
	// Without the fix, torn reads on Binding.value cause wrong results.
	code := `
		(define (tag? node t)
			(and (pair? node) (eq? (car node) t)))

		(define (run-test)
			(let* ((data (list
					(cons 'a 1) (cons 'b 2) (cons 'c 3) (cons 'd 4)
					(cons 'e 5) (cons 'f 6) (cons 'g 7) (cons 'h 8)))
				(threads
					(map (lambda (item)
						(thread-start!
							(make-thread
								(lambda ()
									;; Each thread checks its own item 10000 times.
									;; If tag? races, (car item) could return wrong value.
									(let loop ((i 0) (ok #t))
										(if (>= i 10000) ok
											(loop (+ i 1)
												(and ok (tag? item (car item))))))))))
					data)))
			(map thread-join! threads)))
	`

	result, err := engine.EvalMultiple(context.Background(), code)
	c.Assert(err, qt.IsNil)

	// All threads should return #t
	list, ok := result.(values.Tuple)
	c.Assert(ok, qt.IsTrue)
	list.ForEach(func(v values.Value) error {
		c.Assert(v, valuestest.SchemeEquals, values.TrueValue)
		return nil
	})
}
```

**Step 2: Run the test**

Run: `go test ./extensions/threads/ -run TestConcurrentNoCopyApplySafety -count=1 -v -timeout 30s`
Expected: PASS

**Step 3: Run the test with race detector**

Run: `go test ./extensions/threads/ -run TestConcurrentNoCopyApplySafety -count=1 -v -race -timeout 60s`
Expected: PASS with no race warnings on `MachineClosure.threadShared` or `Binding.value`.

**Step 4: Commit**

```
test(threads): add concurrency safety test for NoCopyApply
```

---

### Task 6: Full test suite and lint

**Step 1: Run full test suite**

Run: `make test`
Expected: All PASS

**Step 2: Run lint**

Run: `make lint`
Expected: Clean

**Step 3: Run benchmarks for regression check**

Run: `make bench-gabriel`
Expected: No significant regression on single-threaded benchmarks. NoCopyApply closures that are never called from threads should show identical performance (the only added cost is one `atomic.LoadUint32` per Apply, which is a single instruction on x86).

**Step 4: Commit any lint/format fixes**

```
chore: lint and format fixes
```

---

### Task 7: Update documentation

**Files:**
- Modify: `machine/CLAUDE.local.md` (Gotchas section)
- Modify: `machine/CLAUDE.md` (if thread safety is mentioned)

**Step 1: Add gotcha to `machine/CLAUDE.local.md`**

Add to the Gotchas section:

```markdown
- **NoCopyApply is disabled for thread-shared closures**: `MachineClosure` and `ForeignClosure` have an atomic `threadShared` flag that latches to 1 on first invocation from a non-primordial SRFI-18 thread (`mc.threadID != 0`). Once latched, all future calls (including from the primordial thread) use the copy path. This prevents torn reads on `Binding.value` (a 2-word `values.Value` interface) when multiple goroutines concurrently write parameters to shared binding slots. The latch is one-way and idempotent — no CAS needed. Single-threaded code is unaffected.
```

**Step 2: Commit**

```
docs(machine): document NoCopyApply thread-safety latch
```
