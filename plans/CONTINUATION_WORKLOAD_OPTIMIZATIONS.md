# Continuation-Heavy Workload Optimizations

**Status:** Complete (6 of 6)
**Date:** 2026-02-18
**Benchmark:** Zebra puzzle (Schelog logic programming)
**Baseline:** 24.22s, 36.2 GB allocated, 907.8M allocations (Apple M4 Max, GOGC=100)
**Current:** 18.6s, 21.5 GB allocated, 300.9M allocations (after optimization #6)

## Profile Summary

~70% of CPU time is the Go garbage collector. The actual `Run()` loop accounts for ~11% of wall time. The bottleneck is allocation pressure and resulting memory traffic, not VM dispatch.

| GOGC | Wall Time | Note |
|------|-----------|------|
| 100 (default) | 24.22s | |
| 400 | 20.65s | 15% faster (less GC frequency) |
| off | 21.87s | Slower than 400 — 36 GB working set trashes caches |

`GOGC=off` being slower than `GOGC=400` confirms the problem is memory traffic, not just GC overhead.

### Allocation Sources

| Source | Bytes | % | Count | Code Location |
|--------|-------|---|-------|---------------|
| `CopyForApply` | 15.5 GB | 44.6% | 112.9M | `environment/local_environment_frame.go:175` |
| `Stack.Push` (growth) | 7.9 GB | 22.8% | — | `machine/stack.go:33` |
| `NewEnvironmentFrameWithParent` | 5.6 GB | 16.0% | 112.9M | `environment/environment_frame.go:135` |
| `MachineContinuation.Copy` | 2.9 GB | 8.5% | — | `machine/machine_continuation.go:123` |
| `NewLocalIndex` | 1.7 GB | 4.9% | ~105M | `environment/local_index.go:35` |

### VM Counters (post-#6)

| Counter | Value |
|---------|-------|
| ops_executed | 1,051,582,741 |
| closures_applied | 112,882,037 |
| envs_copied | 100,585,309 |
| bindings_copied | 145,647,350 |
| no_copy_applies | 12,296,728 |
| no_copy_bindings_saved | 12,983,908 |
| continuations_saved | 74,785,918 |
| continuations_restored | 74,531,801 |
| foreign_calls | 66,207,132 |

## Optimizations

### 1. Eliminate `NewLocalIndex` Heap Allocation ✓

**Impact:** ~1.7 GB (4.9%), ~105M fewer allocations
**Effort:** Low | **Risk:** Low
**Status:** Complete — PR #286, merged 2026-02-19

`NewLocalIndex` returns `*LocalIndex` (pointer to `[2]int`), forcing a heap allocation on every `OpLoadLocal` and `OpStoreLocal` in the VM loop (`machine_context.go:666-683`).

The VM already has `slot` and `depth` as raw integers from `DecodeLocalIndex`. Fix: add `GetLocalBindingBySlotDepth(slot, depth int)` and `SetLocalValueBySlotDepth(slot, depth int, v values.Value)` to `EnvironmentFrame`, bypassing the `*LocalIndex` allocation entirely.

**Measured result:** -1.8 GB bytes, -110.7M allocations (34.4 GB / 797.1M vs baseline 36.2 GB / 907.8M)

**Files:** `environment/environment_frame.go`, `machine/machine_context.go` (OpLoadLocal/OpStoreLocal cases)

### 2. Binding Storage: `[]*Binding` → `[]Binding` ✓

**Impact:** ~2-3 GB (est.), ~112.9M fewer allocations
**Effort:** Medium | **Risk:** Medium
**Status:** Complete — implemented 2026-02-18

Changed `LocalEnvironmentFrame.bindings` from `[]*Binding` to `[]Binding`. Eliminates one allocation per `CopyForApply` (pointer slice) and N allocations per `NewLocalEnvironment` (individual Binding heap objects). All accessors return `&p.bindings[i]` (pointer to slice element); mutation sites use direct index access. Audit confirmed no code holds `*Binding` across frame copies.

**Measured result:** -1.3 GB bytes, -113.1M allocations (33.1 GB / 684.0M vs post-#1 34.4 GB / 797.1M). Wall time 23.76s (vs 24.52s).

**Files:** `environment/local_environment_frame.go`, `environment/environment_frame.go`, `machine/compile_validated.go`, test files

### 3. EnvironmentFrame Struct Fusion ✓

**Impact:** ~5.6 GB (16%), ~112.9M fewer allocations
**Effort:** Medium | **Risk:** Medium
**Status:** Complete — PR #289, merged 2026-02-18

Each `Apply` created both a `LocalEnvironmentFrame` (via `CopyForApply`) and an `EnvironmentFrame` (via `NewEnvironmentFrameWithParent`). The `EnvironmentFrame` was a thin wrapper (~56 bytes).

**Fix:** Embedded `LocalEnvironmentFrame` by value in `EnvironmentFrame`, eliminating one allocation per `Apply`.

**Measured result:** -1.6 GB bytes, -113.1M allocations (27.1 GB / 378.2M vs post-#4 28.6 GB / 491.3M). Wall time 18.5s (vs 21.5s). The -113.1M alloc count matches `closures_applied` exactly — one allocation eliminated per Apply.

**Files:** `environment/environment_frame.go`, `machine/machine_context.go` (Apply), `machine/operation_make_closure.go`

### 4. Stack PopAll Backing Array Retention ✓

**Impact:** -4.5 GB (-13.5%), -192.7M fewer allocations
**Effort:** Low | **Risk:** Low
**Status:** Complete — implemented 2026-02-18

**Investigation result:** Stack depth histogram (added to VMCounters) showed 97.4% of PopAll events at depth 0-2. Only 3 out of 112.7M exceeded the pool capacity of 8. The pool capacity is adequate — capacity tuning is unnecessary.

**Root cause found:** `PopAll` set `*p = nil`, giving away the backing array. After every `Apply` (112.9M times), the stack was nil. The next `OpPush` did `append(nil, v)`, allocating a fresh backing array. This growth chain produced 264.7M allocations (7.8 GB) — 38.7% of all allocations.

**Fix:** Changed `PopAll` to copy data out and retain the backing array. The stack keeps its cap-8 backing array from the pool across Apply cycles. One `make([]values.Value, n)` per non-empty PopAll instead of a growth chain from nil.

**Stack depth distribution (zebra benchmark):**

| Bucket | Count | % |
|--------|-------|---|
| 0-2 | 109,805,111 | 97.4% |
| 3-4 | 2,812,184 | 2.5% |
| 5-8 | 64,376 | 0.06% |
| 9+ | 3 | ~0% |
| Max depth | 42 | — |

**Measured result:** -4.5 GB bytes, -192.7M allocations (28.6 GB / 491.3M vs post-#2 33.1 GB / 684.0M). Wall time 21.5s (vs 23.76s).

**Residual:** Stack.Push still accounts for 3.4 GB (74M allocs), likely from stacks restored via `Restore` (call/cc path) where `slices.Clone` creates right-sized copies that need to grow on reuse.

**Files:** `machine/stack.go` (PopAll), `machine/counters.go` (depth histogram), `machine/machine_context.go` (instrumentation), `machine/operation_apply.go` (instrumentation)

### 5. Shared-Flag Continuation Optimization ✓

**Impact:** ~2.9 GB (8.5%), ~21M fewer frame copies
**Effort:** High | **Risk:** High
**Status:** Complete — PR #290, merged 2026-02-18

`CurrentContinuation()` called `DeepCopy()`, deep-copying the entire continuation chain. This was needed because `RestoreAndRelease` pools frames — if captured continuations share frames with the live chain, pooling corrupts the capture.

**Fix:** Added `shared bool` to `MachineContinuation`. `MarkChainShared()` walks the chain setting `shared=true` (early-exits on already-shared frames — all ancestors must already be shared from a prior capture). `RestoreAndRelease` checks the flag: unshared frames follow the existing transfer-and-pool fast path; shared frames copy evals and skip pooling, preserving them for re-invocation. `DeepCopy` eliminated from the call/cc path.

The ~0.7 GB of evals copies moves from `DeepCopy` to `RestoreAndRelease` — same work, different location.

**Measured result:** -3.8 GB bytes, -52.8M allocations (23.3 GB / 325.4M vs post-#3 27.1 GB / 378.2M). Wall time 17.6s (vs 18.5s). Savings exceeded the ~2.2 GB estimate — likely second-order effects from reduced GC pressure.

**Scope:** call/cc path only. Composable continuation `DeepCopy` (`applyComposableContinuation`) is unchanged.

**Files:** `machine/machine_continuation.go`, `machine/machine_context.go` (RestoreAndRelease, CurrentContinuation, RestoreWithWindingFrom, RunWithEscapeHandling), `registry/core/prim_control.go` (PrimCallCC), `machine/counters.go`, `machine/pool.go`

### 6. CopyForApply Avoidance (Escape Analysis) ✓

**Impact:** -1.8 GB (-7.7%), -24.6M fewer allocations
**Effort:** Medium | **Risk:** Low
**Status:** Complete — implemented 2026-02-18

Added compile-time escape analysis: `NativeTemplate.computeNoCopyApply()` scans `code[]` for `OpSaveContinuation` and `sideTable[]` for `*OperationMakeClosure`. If neither is present, the template's bindings cannot escape the call frame (no continuation capture, no inner closure). `Apply` branches on the flag: safe templates reuse the closure's own `EnvironmentFrame` in place (0 allocations); unsafe templates take the existing `NewApplyFrame()` copy path.

The analysis is conservative — any `SaveContinuation` or `MakeClosure` anywhere in the template disables the optimization, even if unreachable. 10.9% of closure applications (12.3M of 112.9M) take the no-copy path in the zebra benchmark.

**Measured result:** -1.8 GB bytes, -24.6M allocations (21.5 GB / 300.9M vs post-#5 23.3 GB / 325.4M). Wall time ~18.6s (vs ~17.6s post-#5; variance within run-to-run noise).

**VM counter changes:**

| Counter | Before | After | Delta |
|---------|--------|-------|-------|
| `envs_copied` | 112,882,037 | 100,585,309 | -12,296,728 |
| `bindings_copied` | 158,631,258 | 145,647,350 | -12,983,908 |
| `no_copy_applies` | — | 12,296,728 | new |
| `no_copy_bindings_saved` | — | 12,983,908 | new |

**Files:** `machine/native_template.go` (flag + analysis), `machine/compile_validated.go` (hook after `compileBody`), `machine/machine_context.go` (Apply branch), `machine/counters.go` (instrumentation)

## Priority Order

| # | Optimization | Alloc Savings | Effort | Risk |
|---|-------------|---------------|--------|------|
| 1 | LocalIndex → value type | **-1.8 GB measured** | Low | ✓ Complete |
| 2 | `[]*Binding` → `[]Binding` | ~2-3 GB (est.) | Medium | ✓ Complete |
| 3 | EnvironmentFrame fusion | ~5.6 GB (16%) | Medium | ✓ Complete |
| 4 | Stack PopAll array retention | **-4.5 GB measured** | Low | ✓ Complete |
| 5 | Shared-flag continuations | **-3.8 GB measured** | High | ✓ Complete |
| 6 | CopyForApply avoidance | **-1.8 GB measured** | Medium | ✓ Complete |

## Verification

For each optimization:
1. `make test` — correctness
2. `make profile-zebra` — measure allocation reduction
3. `make bench` — no regressions on other benchmarks
4. `make bench-schelog` — end-to-end Schelog performance
5. Compare GOGC=100 and GOGC=400 to verify GC pressure reduction

## References

- `plans/PERFORMANCE_REFACTORING_PLAN.md` — Phase 0-6 history
- `plans/APPLY_OPTIMIZATION_OPPORTUNITIES.md` — Prior Apply path analysis
- `machine/pool.go` — Existing stack/continuation pooling
- `environment/local_environment_frame.go:175` — CopyForApply implementation
- `environment/local_index.go:35` — NewLocalIndex pointer return
