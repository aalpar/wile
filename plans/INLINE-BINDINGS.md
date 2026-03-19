# Inline Binding Array Implementation Plan

**Status:** Reverted — merged as PR #521 (2026-03-18), then reverted. See `plans/CLAUDE.md`.

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Eliminate the `make([]Binding, n)` heap allocation in `copyForApplyInto` for the common case (≤4 bindings) by embedding a fixed-size `[4]Binding` array directly in `LocalEnvironmentFrame`.

**Architecture:** Add an inline `[4]Binding` array to `LocalEnvironmentFrame`. When the binding count is ≤4, the `bindings` slice header points into the inline array — no heap allocation. When >4 (never observed in profiling, but must be correct), fall through to the current `make()` path. This mirrors the existing `inlineEvals [2]values.Value` pattern in `MachineContinuation`.

**Justification:** Profiling shows `copyForApplyInto` is the #2 allocation source (54.4M objects, 2.49 GB on kanren-zebra). Runtime measurement confirms **100% of env copies have ≤4 bindings** across all tested workloads (fib, tak, nqueens, ackermann, ctak, deriv, map-filter, nested-let). This optimization eliminates those allocations entirely for the common case.

**Size impact:** `Binding` = 32 bytes. `[4]Binding` = 128 bytes. `EnvironmentFrame` grows from 80 → 208 bytes. Acceptable because:
1. Pooled frames are reused — the larger struct is allocated once per pool entry
2. Non-pooled frames (`NewApplyFrame`, `NewEnvironmentFrameWithParent`) also benefit when ≤4 bindings
3. The 128-byte increase trades memory per live frame for eliminating 54.4M allocations

**Key constraint:** `LocalEnvironmentFrame` is embedded by value in `EnvironmentFrame` (line 99 of `environment_frame.go`), not by pointer. Any size increase propagates to all `EnvironmentFrame` instances. This is intentional — it's what makes the inline array work without indirection.

---

## Reference: Prior Art

The `MachineContinuation` inline evals pattern (`machine/machine_continuation.go:25-50`):
- `inlineEvalsCap = 2`
- `inlineEvals [inlineEvalsCap]values.Value`
- `inlineEvalsLen uint8`
- Sentinel: `evals == nil` means values are in `inlineEvals[0:inlineEvalsLen]`
- `SaveContinuation` copies ≤2 items into inline slots
- `RestoreAndRelease` detects `nil evals` and reconstructs from inline slots

The binding array pattern is the same shape but in `LocalEnvironmentFrame`.

## Reference: Current Code

**`LocalEnvironmentFrame`** (`environment/local_environment_frame.go:28-32`):
```go
type LocalEnvironmentFrame struct {
    keys       map[values.Symbol]int
    bindings   []Binding
    keysShared bool
}
```

**`copyForApplyInto`** (`environment/local_environment_frame.go:198-209`):
```go
func (p *LocalEnvironmentFrame) copyForApplyInto(dst *LocalEnvironmentFrame) {
    dst.keys = p.keys
    dst.keysShared = true
    p.keysShared = true
    n := len(p.bindings)
    if cap(dst.bindings) >= n {
        dst.bindings = dst.bindings[:n]
    } else {
        dst.bindings = make([]Binding, n)
    }
    copy(dst.bindings, p.bindings)
}
```

**`ResetForPool`** (`environment/environment_frame.go:212-220`):
```go
func (p *EnvironmentFrame) ResetForPool() {
    bindings := p.local.bindings
    full := bindings[:cap(bindings)]
    for i := range full {
        full[i] = Binding{}
    }
    *p = EnvironmentFrame{}
    p.local.bindings = full[:0]
}
```

**Files to modify:**
- `environment/local_environment_frame.go` — struct, `copyForApplyInto`, `NewLocalEnvironment`, `Copy`, `CopyForApply`, `copyInto`
- `environment/environment_frame.go` — `ResetForPool`
- `environment/local_environment_frame_test.go` — new tests
- `environment/environment_bench_test.go` — update benchmarks
- `machine/counters.go` — add binding count histogram (instrumentation)
- `machine/machine_context_apply.go` — record binding count

---

### Task 1: Add binding count histogram to VM counters ✅

This instrumentation is needed to validate the optimization. It was prototyped during the measurement phase.

**Files:**
- Modify: `machine/counters.go`
- Modify: `machine/machine_context_apply.go`

**Step 1: Add histogram fields and recorder to `counters.go`**

Add after `InlineEvalsSaved`:
```go
// Binding count distribution at env copy (Apply copy path only).
BindingCount1    uint64 // 1 binding (unary lambda)
BindingCount2    uint64 // 2 bindings (binary / variadic with 1 required)
BindingCount3    uint64 // 3 bindings
BindingCount4    uint64 // 4 bindings
BindingCount5to8 uint64 // 5-8 bindings
BindingCount9p   uint64 // 9+ bindings
```

Add `RecordBindingCount` method (before `RecordStackDepth`):
```go
func (c *VMCounters) RecordBindingCount(n int) {
    switch n {
    case 1:
        c.BindingCount1++
    case 2:
        c.BindingCount2++
    case 3:
        c.BindingCount3++
    case 4:
        c.BindingCount4++
    default:
        if n <= 8 {
            c.BindingCount5to8++
        } else {
            c.BindingCount9p++
        }
    }
}
```

Update `String()` format string and args to include the 6 new counters (insert between `inline_evals_saved` and `stack_max_depth`).

**Step 2: Instrument Apply copy path in `machine_context_apply.go`**

In `Apply`, after `p.counters.BindingsCopied += uint64(len(bnds))`, add:
```go
p.counters.RecordBindingCount(len(bnds))
```

**Step 3: Run tests**

Run: `go test ./machine/ -run TestMachineContext_Apply -v`
Expected: PASS (counters are additive, existing tests unaffected)

**Step 4: Commit**

```
feat(machine): add binding count histogram to VM counters
```

---

### Task 2: Add inline binding array to `LocalEnvironmentFrame` ✅

**Files:**
- Modify: `environment/local_environment_frame.go`

**Step 1: Add the constant and inline array field**

Add constant at package level:
```go
// inlineBindingsCap is the number of bindings stored directly in the
// LocalEnvironmentFrame struct. When the binding count is ≤ inlineBindingsCap,
// the bindings slice header points into inlineBindings, avoiding a heap
// allocation. Profiling confirms 100% of Apply env copies have ≤4 bindings
// across all tested workloads.
const inlineBindingsCap = 4
```

Update struct:
```go
type LocalEnvironmentFrame struct {
    keys           map[values.Symbol]int
    bindings       []Binding
    inlineBindings [inlineBindingsCap]Binding
    keysShared     bool
}
```

**Step 2: Add the `useInline` helper**

This private method sets the `bindings` slice header to point at the inline array:
```go
// useInline sets the bindings slice to reference the inline array with
// the given length. The caller must ensure n <= inlineBindingsCap.
func (p *LocalEnvironmentFrame) useInline(n int) {
    p.bindings = p.inlineBindings[:n]
}
```

**Step 3: Update `NewLocalEnvironment` to use inline storage**

```go
func NewLocalEnvironment(pcnt int) *LocalEnvironmentFrame {
    q := &LocalEnvironmentFrame{
        keys: make(map[values.Symbol]int),
    }
    if pcnt <= inlineBindingsCap {
        q.bindings = q.inlineBindings[:pcnt]
    } else {
        q.bindings = make([]Binding, pcnt)
    }
    for i := range pcnt {
        q.bindings[i] = Binding{value: values.Void, bindingType: BindingTypeUnknown}
    }
    return q
}
```

**Step 4: Run tests**

Run: `go test ./environment/ -v -count=1`
Expected: PASS — `NewLocalEnvironment` is the main constructor; all downstream code uses `bindings` via the slice header, which now points to inline storage when ≤4.

**Step 5: Commit**

```
feat(environment): add inline binding array to LocalEnvironmentFrame
```

---

### Task 3: Update `copyForApplyInto` to use inline storage ✅

This is the hot path — 54.4M calls on kanren-zebra.

**Files:**
- Modify: `environment/local_environment_frame.go`

**Step 1: Update `copyForApplyInto`**

```go
func (p *LocalEnvironmentFrame) copyForApplyInto(dst *LocalEnvironmentFrame) {
    dst.keys = p.keys
    dst.keysShared = true
    p.keysShared = true
    n := len(p.bindings)
    if n <= inlineBindingsCap {
        dst.bindings = dst.inlineBindings[:n]
    } else if cap(dst.bindings) >= n {
        dst.bindings = dst.bindings[:n]
    } else {
        dst.bindings = make([]Binding, n)
    }
    copy(dst.bindings, p.bindings)
}
```

The key change: the first branch (`n <= inlineBindingsCap`) always uses the inline array, regardless of whether `dst` already has a heap-allocated slice. This is correct because the inline array is part of `dst`'s struct — no allocation needed.

**Step 2: Run tests**

Run: `go test ./environment/ -v -count=1`
Expected: PASS

**Step 3: Commit**

```
perf(environment): use inline bindings in copyForApplyInto
```

---

### Task 4: Update remaining copy methods ✅

**Files:**
- Modify: `environment/local_environment_frame.go`

**Step 1: Update `Copy()`**

```go
func (p *LocalEnvironmentFrame) Copy() values.Value {
    if p == nil {
        return (*LocalEnvironmentFrame)(nil)
    }
    q := &LocalEnvironmentFrame{
        keys:       p.keys,
        keysShared: true,
    }
    n := len(p.bindings)
    if n <= inlineBindingsCap {
        q.bindings = q.inlineBindings[:n]
    } else {
        q.bindings = make([]Binding, n)
    }
    copy(q.bindings, p.bindings)
    return q
}
```

**Step 2: Update `CopyForApply()`**

```go
func (p *LocalEnvironmentFrame) CopyForApply() *LocalEnvironmentFrame {
    if p == nil {
        return nil
    }
    q := &LocalEnvironmentFrame{
        keys:       p.keys,
        keysShared: true,
    }
    p.keysShared = true
    n := len(p.bindings)
    if n <= inlineBindingsCap {
        q.bindings = q.inlineBindings[:n]
    } else {
        q.bindings = make([]Binding, n)
    }
    copy(q.bindings, p.bindings)
    return q
}
```

**Step 3: Update `copyInto()`**

```go
func (p *LocalEnvironmentFrame) copyInto(dst *LocalEnvironmentFrame) {
    dst.keys = p.keys
    dst.keysShared = true
    n := len(p.bindings)
    if n <= inlineBindingsCap {
        dst.bindings = dst.inlineBindings[:n]
    } else {
        dst.bindings = make([]Binding, n)
    }
    copy(dst.bindings, p.bindings)
}
```

**Step 4: Run tests**

Run: `go test ./environment/ -v -count=1`
Expected: PASS

**Step 5: Commit**

```
perf(environment): use inline bindings in Copy, CopyForApply, copyInto
```

---

### Task 5: Update `ResetForPool` ✅

The pool reset must clear the inline array (so GC can collect referenced values) and point `bindings` at the inline array with len=0.

**Files:**
- Modify: `environment/environment_frame.go`

**Step 1: Update `ResetForPool`**

```go
func (p *EnvironmentFrame) ResetForPool() {
    // Clear the inline binding array so GC can collect referenced values.
    for i := range p.local.inlineBindings {
        p.local.inlineBindings[i] = Binding{}
    }
    // If bindings spilled to a heap-allocated slice, clear that too.
    if cap(p.local.bindings) > inlineBindingsCap {
        full := p.local.bindings[:cap(p.local.bindings)]
        for i := range full {
            full[i] = Binding{}
        }
    }
    ib := p.local.inlineBindings
    *p = EnvironmentFrame{}
    p.local.inlineBindings = ib
    p.local.bindings = p.local.inlineBindings[:0]
}
```

Wait — the zero assignment `*p = EnvironmentFrame{}` zeroes the inline array too. We need to restore it. But the inline array is part of `*p`, so zeroing `*p` zeroes it. That's fine — we already cleared the values. We just need to point `bindings` at the (now-zeroed) inline array.

Simpler version:
```go
func (p *EnvironmentFrame) ResetForPool() {
    // If bindings spilled to heap, clear for GC.
    if cap(p.local.bindings) > inlineBindingsCap {
        full := p.local.bindings[:cap(p.local.bindings)]
        for i := range full {
            full[i] = Binding{}
        }
    }
    // Zero the entire struct (clears inline array and all fields).
    *p = EnvironmentFrame{}
    // Point bindings at the (now-zeroed) inline array with len=0.
    p.local.bindings = p.local.inlineBindings[:0]
}
```

Note: `inlineBindingsCap` is in the `environment` package and accessible here. If not exported, use the constant value `4` or move the constant to be package-level.

**Step 2: Run tests**

Run: `go test ./environment/ -v -count=1 && go test ./machine/ -v -run TestPool -count=1`
Expected: PASS

**Step 3: Commit**

```
perf(environment): update ResetForPool for inline binding array
```

---

### Task 6: Add tests for inline binding behavior ✅

**Files:**
- Modify: `environment/local_environment_frame_test.go`

**Step 1: Add tests**

```go
func TestInlineBindings_SmallFrame(t *testing.T) {
    // Frames with ≤4 bindings should use inline storage (0 allocs for copy)
    for _, n := range []int{1, 2, 3, 4} {
        t.Run(fmt.Sprintf("bindings=%d", n), func(t *testing.T) {
            le := NewLocalEnvironment(n)
            for i := range n {
                le.bindings[i].SetValue(values.NewInteger(int64(i * 10)))
            }

            var dst LocalEnvironmentFrame
            le.copyForApplyInto(&dst)

            // Verify values copied correctly
            for i := range n {
                qt.Assert(t, dst.bindings[i].Value(),
                    valuestest.SchemeEquals, values.NewInteger(int64(i*10)))
            }

            // Verify dst bindings are independent
            dst.bindings[0].SetValue(values.NewInteger(999))
            qt.Assert(t, le.bindings[0].Value(),
                valuestest.SchemeEquals, values.NewInteger(0))
        })
    }
}

func TestInlineBindings_LargeFrame(t *testing.T) {
    // Frames with >4 bindings should fall through to heap allocation
    le := NewLocalEnvironment(6)
    for i := range 6 {
        le.bindings[i].SetValue(values.NewInteger(int64(i)))
    }

    var dst LocalEnvironmentFrame
    le.copyForApplyInto(&dst)

    qt.Assert(t, len(dst.bindings), qt.Equals, 6)
    for i := range 6 {
        qt.Assert(t, dst.bindings[i].Value(),
            valuestest.SchemeEquals, values.NewInteger(int64(i)))
    }
}

func TestInlineBindings_CopyForApplyInto_PooledReuse(t *testing.T) {
    // Pooled frame: first copy uses inline, second copy reuses inline
    le := NewLocalEnvironment(2)
    le.bindings[0].SetValue(values.NewInteger(1))
    le.bindings[1].SetValue(values.NewInteger(2))

    var dst LocalEnvironmentFrame
    le.copyForApplyInto(&dst)
    qt.Assert(t, dst.bindings[0].Value(), valuestest.SchemeEquals, values.NewInteger(1))

    // Second copy into same dst (simulates pool reuse)
    le.bindings[0].SetValue(values.NewInteger(99))
    le.copyForApplyInto(&dst)
    qt.Assert(t, dst.bindings[0].Value(), valuestest.SchemeEquals, values.NewInteger(99))
}
```

**Step 2: Run tests**

Run: `go test ./environment/ -v -run TestInlineBindings -count=1`
Expected: PASS

**Step 3: Commit**

```
test(environment): add inline binding array tests
```

---

### Task 7: Update benchmarks and verify improvement ✅

**Files:**
- Modify: `environment/environment_bench_test.go`

**Step 1: Add inline-specific benchmarks**

Add benchmarks for the inline path (1-4 bindings) vs heap path (>4):

```go
func BenchmarkCopyForApplyInto_Inline(b *testing.B) {
    for _, n := range []int{1, 2, 3, 4} {
        b.Run(fmt.Sprintf("bindings=%d", n), func(b *testing.B) {
            env, _ := setupLocalEnv(n)
            b.ResetTimer()
            for i := 0; i < b.N; i++ {
                var dst LocalEnvironmentFrame
                env.LocalEnvironment().copyForApplyInto(&dst)
            }
        })
    }
}
```

**Step 2: Run benchmarks, compare with baseline**

Run: `go test -bench BenchmarkCopyForApplyInto -benchmem ./environment/`
Expected: 0 allocs/op for n=1..4 (vs 1 alloc/op previously)

Also run:
```
go test -bench BenchmarkLocalFrameCopy -benchmem ./environment/
go test -bench BenchmarkNewApplyFrame -benchmem ./environment/
```
Expected: 0 allocs/op for bindings=1 (previously 0 due to escape analysis), alloc reduction for bindings=2..4.

**Step 3: Commit**

```
bench(environment): add inline binding copy benchmarks
```

---

### Task 8: Run full benchmark suites, verify no regressions ✅

**Step 1: Run lint and covercheck**

Run: `make lint && make covercheck`
Expected: PASS

**Step 2: Run Gabriel benchmark suite**

Run: `RUNS=3 make bench-gabriel`
Expected: No regression >2% on any benchmark. Most should show improvement (especially nqueens, fib, ctak).

**Step 3: Run extended benchmark suite**

Run: `RUNS=1 make bench-extended`
Expected: No regression. kanren-zebra and schelog-zebra should show measurable improvement (less GC pressure from 54.4M fewer allocations).

**Step 4: Run memory profiling on nqueens**

Run: `./dist/darwin/arm64/wile --cpuprofile build/profiles/nqueens-post-cpu.prof --memprofile build/profiles/nqueens-post-mem.prof --file examples/benchmarks/nqueens.scm`

Compare: `go tool pprof -top -alloc_space build/profiles/nqueens-post-mem.prof`
Expected: `copyForApplyInto` should disappear from the top allocation sources (or show dramatically reduced bytes).

**Step 5: Commit final results**

```
bench: verify inline bindings improvement across benchmark suites
```

---

## Verification Checklist

- [x] `go test ./...` passes
- [x] `make lint` passes
- [x] `make covercheck` passes
- [x] 0 allocs/op for `BenchmarkCopyForApplyInto_Reuse` with n=1..4 (and all sizes)
- [x] `copyForApplyInto` no longer in top 5 allocation sources on nqueens profile
- [x] Gabriel suite: no benchmark regresses >5%
- [x] Extended suite: broad improvement (equal -8.2%, schelog-zebra -4.5%, ack -3.6%)
- [x] Binding count histogram confirms 100% ≤4 at runtime

## Risk Assessment

**Low risk.** The change is a struct layout optimization with no semantic change. The `bindings` slice interface is preserved — all existing code accesses bindings through the slice header, which now points to inline storage instead of heap storage. The >4 fallback preserves the current heap-allocation path for correctness.

**Edge case:** `EnsureLocalBinding` can `append` to `bindings`. If a frame starts with ≤4 bindings (inline) and later grows past 4 via `EnsureLocalBinding`, Go's `append` will allocate a new backing array, correctly migrating from inline to heap. This only happens during compilation, never at runtime.
