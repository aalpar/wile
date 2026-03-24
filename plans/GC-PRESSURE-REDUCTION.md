# GC Pressure Reduction Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Reduce per-call heap allocations in recursive Scheme programs from ~179/call-iteration to near zero after warmup, eliminating the ~95% GC overhead that dominates CPU profiles.

**Architecture:** Three changes to the object pooling infrastructure: (1) migrate continuation and stack pools from `sync.Pool` to `FreeList` so recycled objects survive GC cycles, (2) pre-size the binding array in the env frame pool's constructor so fresh frames don't require a second allocation in `copyForApplyInto`, (3) fix an env frame leak in `ReleaseTopLevelContext` / `ReleaseSubContext` where pooled env frames are silently dropped.

**Tech Stack:** Go, `machine/` and `environment/` packages. No new dependencies.

---

## Baseline

Measured on M4 Max, branch `perf/envframe-prealloc-bindings`, `BenchmarkRun/Fibonacci` (fib(10), 177 closure calls):

| Metric | Value |
|--------|-------|
| ns/op | 24,300 |
| allocs/op | 179 |
| B/op | 10,053 |
| CPU in GC | ~95% |

Top allocators (steady state, 10K iterations):
- `copyForApplyInto` (line 206): 49% of alloc objects — `make([]Binding, n)` when `cap(dst.bindings) < n`
- `envFramePool.newFn` (pool.go:102): 40% of alloc objects — fresh `EnvironmentFrame` from freelist miss
- Remaining 11%: stack pool, continuation pool, misc

## Approach

### Why the env frame freelist misses

The `envFramePool` uses `FreeList` (survives GC), yet misses ~83 times per iteration on a workload that should be fully warmed after iteration 1. Two contributing factors:

1. **Env frame leak in `ReleaseTopLevelContext`**: When `Run()` halts (continuation chain exhausted), `mc.env` may be a pooled frame (`mc.envPooled == true`). `ReleaseTopLevelContext` zeros the MC via `subContextPool.Release` without checking `envPooled`, leaking the frame. Over thousands of iterations the freelist slowly drains.

2. **Fresh frames lack binding capacity**: When the freelist DOES miss, `newFn` returns `&EnvironmentFrame{}` with `local.bindings == nil` (cap 0). Every such frame triggers a `make([]Binding, n)` inside `copyForApplyInto`. Pre-sizing to cap 4 eliminates this second allocation.

### Why continuation/stack pools should also be FreeList

`continuationPool` and `stackPool` use `sync.Pool`, which is cleared every GC cycle. In recursive Scheme workloads, GC runs 1000+ times/sec (confirmed by the env frame migration in commit `d0ae70d0`). Moving these to `FreeList` applies the same fix that gave env frames a -14% geo mean improvement.

---

## Task 1: Fix env frame leak in release paths

**Files:**
- Modify: `machine/pool.go:128-164` (`ReleaseSubContext`, `ReleaseTopLevelContext`)
- Test: `machine/pool_test.go`

### Step 1: Write the failing test

Add to `machine/pool_test.go`:

```go
func TestReleaseTopLevelContext_ReleasesPooledEnvFrame(t *testing.T) {
	// Simulate: after Run() halts, mc.env is a pooled frame.
	// ReleaseTopLevelContext must release it before zeroing the MC.
	env := acquireEnvFrame()
	tpl := NewEmptyNativeTemplate()
	ctx := context.Background()
	mc := AcquireTopLevelContext(ctx, tpl, env)
	mc.envPooled = true // as set by Apply

	before := envFramePool.Stats()
	ReleaseTopLevelContext(mc)
	after := envFramePool.Stats()

	// The pooled env frame should have been released.
	released := after.Releases - before.Releases
	qt.Assert(t, released, qt.Equals, uint64(1))
}

func TestReleaseSubContext_ReleasesPooledEnvFrame(t *testing.T) {
	env := acquireEnvFrame()
	mc := acquireSubContext()
	mc.env = env
	mc.envPooled = true

	before := envFramePool.Stats()
	ReleaseSubContext(mc)
	after := envFramePool.Stats()

	released := after.Releases - before.Releases
	qt.Assert(t, released, qt.Equals, uint64(1))
}
```

### Step 2: Run tests — expect FAIL

```bash
go test -run='TestReleaseTopLevelContext_ReleasesPooledEnvFrame|TestReleaseSubContext_ReleasesPooledEnvFrame' -v ./machine/
```

Expected: FAIL — `released` is 0 because current code doesn't check `envPooled`.

### Step 3: Implement the fix

In `machine/pool.go`, modify both release functions to check `envPooled`:

```go
func ReleaseSubContext(mc *MachineContext) {
	if mc == nil {
		return
	}
	if mc.parentMC != nil {
		mc.parentMC.counters.SubContextPoolReleases++
	}
	if mc.envPooled {
		releaseEnvFrame(mc.env)
	}
	subContextPool.Release(mc)
}

func ReleaseTopLevelContext(mc *MachineContext) {
	if mc == nil {
		return
	}
	if mc.envPooled {
		releaseEnvFrame(mc.env)
	}
	subContextPool.Release(mc)
}
```

### Step 4: Run tests — expect PASS

```bash
go test -run='TestReleaseTopLevelContext_ReleasesPooledEnvFrame|TestReleaseSubContext_ReleasesPooledEnvFrame' -v ./machine/
```

### Step 5: Run full test suite — no regressions

```bash
make test
```

### Step 6: Commit

```
fix(machine): release pooled env frames in ReleaseTopLevelContext/ReleaseSubContext
```

---

## Task 2: Pre-size binding capacity in envFramePool

**Files:**
- Modify: `machine/pool.go:100-107` (envFramePool newFn)
- Modify: `environment/environment_frame.go:212-220` (ResetForPool)
- Test: `machine/pool_test.go`

### Step 1: Write the failing test

```go
// defaultBindingsCap is the pre-allocated binding capacity for pooled
// env frames. Covers >95% of lambdas (1-3 params) without waste.
// Defined in pool.go; tested here for stability.

func TestEnvFramePool_FreshFrameHasBindingCapacity(t *testing.T) {
	// A fresh frame from the pool should have pre-allocated binding capacity,
	// so copyForApplyInto can reslice instead of allocating.
	frame := envFramePool.Acquire()
	defer envFramePool.Release(frame)

	// Access the local bindings slice directly (keys may be nil on fresh frames).
	// After pre-sizing, cap should be >= defaultBindingsCap.
	qt.Assert(t, cap(frame.LocalBindingsSlice()), qt.CmpEquals(
		cmp.Comparer(func(a, b int) bool { return a >= b }),
	), defaultBindingsCap)
}

func TestEnvFramePool_ResetPreservesCapacity(t *testing.T) {
	frame := envFramePool.Acquire()

	// Simulate use: InitApplyFrame sets bindings to len=2
	ns := environment.NewNamespace()
	parent := ns.Runtime()
	lenv := environment.NewLocalEnvironment(2)
	src := environment.NewEnvironmentFrameWithParent(lenv, parent)
	src.InitApplyFrame(frame)

	capBefore := cap(frame.LocalEnvironment().Bindings())
	envFramePool.Release(frame)
	frame2 := envFramePool.Acquire()

	// Capacity should survive the release+acquire round trip.
	qt.Assert(t, cap(frame2.LocalBindingsSlice()), qt.Equals, capBefore)
	envFramePool.Release(frame2)
}
```

### Step 2: Run tests — expect FAIL

The `LocalBindingsSlice()` method doesn't exist yet, and the fresh frame has cap 0.

### Step 3: Implement

**`environment/environment_frame.go`** — add accessor for the raw binding slice (needed because `LocalEnvironment()` returns nil when keys is nil):

```go
// LocalBindingsSlice returns the raw local bindings slice, bypassing the
// nil-keys check in LocalEnvironment(). This exposes the pre-allocated
// capacity that pooled frames retain across reset cycles.
func (p *EnvironmentFrame) LocalBindingsSlice() []Binding {
	return p.local.bindings
}
```

**`machine/pool.go`** — add constant and update newFn:

```go
// defaultBindingsCap is the pre-allocated binding capacity for fresh env
// frames from the pool. Most lambdas take 1-3 parameters; cap 4 covers
// >95% of closures without waste. Frames that need more will grow via
// make([]Binding, n) in copyForApplyInto — a one-time cost per frame
// that is retained across subsequent pool cycles via ResetForPool.
const defaultBindingsCap = 4
```

Update the envFramePool newFn (pool.go:101-102):

```go
var envFramePool = registerFreeList(pools, NewFreeList("env_frame",
	func() *environment.EnvironmentFrame {
		f := &environment.EnvironmentFrame{}
		f.PreAllocateBindings(defaultBindingsCap)
		return f
	},
	func(f *environment.EnvironmentFrame) {
		f.ResetForPool()
	},
))
```

**`environment/environment_frame.go`** — add PreAllocateBindings:

```go
// PreAllocateBindings sets the local bindings slice to a zero-length slice
// with the given capacity. Used by the env frame pool to ensure fresh frames
// have sufficient capacity for copyForApplyInto to reslice instead of allocate.
// Must only be called on freshly constructed frames (before any other use).
func (p *EnvironmentFrame) PreAllocateBindings(cap int) {
	p.local.bindings = make([]Binding, 0, cap)
}
```

### Step 4: Run tests — expect PASS

```bash
go test -run='TestEnvFramePool_FreshFrameHasBindingCapacity|TestEnvFramePool_ResetPreservesCapacity' -v ./machine/
```

### Step 5: Run full test suite

```bash
make test
```

### Step 6: Commit

```
perf(machine): pre-size binding capacity in env frame pool
```

---

## Task 3: Move continuationPool to FreeList

**Files:**
- Modify: `machine/pool.go:79-87` (continuationPool declaration)
- Modify: `machine/pool.go:181-197` (acquireContinuation, releaseContinuation)
- Test: `machine/pool_test.go`

### Step 1: Write the failing test

```go
func TestContinuationPool_SurvivesGC(t *testing.T) {
	// Verify that continuations are retained across GC, unlike sync.Pool.
	cont := acquireContinuation()
	releaseContinuation(cont)

	before := continuationPool.Stats()

	// Force GC — sync.Pool would clear; FreeList should not.
	runtime.GC()
	runtime.GC()

	cont2 := acquireContinuation()
	after := continuationPool.Stats()
	releaseContinuation(cont2)

	// Should be a hit (no new miss) after GC.
	qt.Assert(t, after.Misses-before.Misses, qt.Equals, uint64(0))
}
```

### Step 2: Run test — expect FAIL (sync.Pool clears on GC)

### Step 3: Implement

Change `continuationPool` from `Pool` to `FreeList` in `pool.go`:

```go
var continuationPool = registerFreeList(pools, NewFreeList("continuation",
	func() *MachineContinuation {
		return &MachineContinuation{}
	},
	func(cont *MachineContinuation) {
		releaseStack(cont.evals)
		*cont = MachineContinuation{}
	},
))
```

Update `acquireContinuation` and `releaseContinuation` — these already use the pool variable generically, but verify the type references compile. Both `Pool[T]` and `FreeList[T]` expose `Acquire() *T` and `Release(*T)`, so no call-site changes are needed.

### Step 4: Run test — expect PASS

### Step 5: Full suite

```bash
make test
```

### Step 6: Commit

```
perf(machine): move continuationPool from sync.Pool to FreeList
```

---

## Task 4: Move stackPool to FreeList

**Files:**
- Modify: `machine/pool.go:46-58` (stackPool declaration)
- Test: `machine/pool_test.go`

### Step 1: Write the failing test

```go
func TestStackPool_SurvivesGC(t *testing.T) {
	s := acquireStack()
	releaseStack(s)

	before := stackPool.Stats()
	runtime.GC()
	runtime.GC()

	s2 := acquireStack()
	after := stackPool.Stats()
	releaseStack(s2)

	qt.Assert(t, after.Misses-before.Misses, qt.Equals, uint64(0))
}
```

### Step 2: Run test — expect FAIL

### Step 3: Implement

```go
var stackPool = registerFreeList(pools, NewFreeList("stack",
	func() *Stack {
		s := make(Stack, 0, StackInitialCap)
		return &s
	},
	func(s *Stack) {
		full := (*s)[:cap(*s)]
		for i := range full {
			full[i] = nil
		}
		*s = full[:0]
	},
))
```

### Step 4: Run test — expect PASS

### Step 5: Full suite

```bash
make test
```

### Step 6: Commit

```
perf(machine): move stackPool from sync.Pool to FreeList
```

---

## Task 5: Benchmark validation

**Files:** None modified — measurement only.

### Step 1: Run fib benchmark and compare

```bash
go test -bench='BenchmarkRun/Fibonacci' -benchtime=1000x -count=5 -benchmem . 2>&1
```

**Expected improvement:**
- allocs/op: 179 → <20 (after warmup, nearly all pool hits)
- B/op: 10,053 → <2,000
- ns/op: improvement proportional to GC reduction

### Step 2: Run Gabriel benchmarks

```bash
make bench-gabriel
```

Compare against baseline in `plans/PERFORMANCE.md`.

### Step 3: Run lint and covercheck

```bash
make lint && make covercheck
```

### Step 4: Commit benchmark results if significant

```
perf(machine): benchmark results after GC pressure reduction
```

---

## Task 6: Clean up subContextPool (optional)

If Tasks 1-4 don't fully close the gap, consider moving `subContextPool` to `FreeList` as well. Sub-contexts are created by foreign functions calling back into Scheme (`map`, `for-each`, `apply`). For fib this is not a factor, but for list-processing benchmarks it may matter.

Same pattern as Tasks 3-4: change `NewPool` → `NewFreeList`, add GC-survival test.

---

## Risk Assessment

| Risk | Severity | Mitigation |
|------|----------|------------|
| FreeList never shrinks | Low | Peak depth is bounded by `maxCallDepth`. Typical: 100-1000 frames × ~80 bytes = 8-80 KB. |
| Mutex contention on FreeList | Low | Single goroutine (no SRFI-18 threads in benchmarks). SRFI-18 threads each have their own MC. |
| Pre-sized bindings waste memory | Low | 4 × 32 bytes = 128 bytes per fresh frame. Negligible vs. the 10KB/op savings. |
| Leak fix changes observable behavior | None | Only affects pool stats. No semantic change to Scheme execution. |

## Success Criteria

1. `BenchmarkRun/Fibonacci` allocs/op < 20 (from 179)
2. No regression > 5% on any Gabriel benchmark
3. `make test && make lint && make covercheck` pass
4. CPU profile shows <50% GC (from ~95%)
