# Continuation-Heavy Workload Optimizations

**Status:** Proposed
**Date:** 2026-02-18
**Benchmark:** Zebra puzzle (Schelog logic programming)
**Baseline:** 24.22s, 36.2 GB allocated, 907.8M allocations (Apple M4 Max, GOGC=100)

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

### VM Counters

| Counter | Value |
|---------|-------|
| ops_executed | 1,051,582,741 |
| closures_applied | 112,882,037 |
| envs_copied | 112,882,037 |
| bindings_copied | 158,631,258 |
| continuations_saved | 74,785,918 |
| continuations_restored | 74,531,801 |
| foreign_calls | 66,207,132 |

## Optimizations

### 1. Eliminate `NewLocalIndex` Heap Allocation

**Impact:** ~1.7 GB (4.9%), ~105M fewer allocations
**Effort:** Low | **Risk:** Low

`NewLocalIndex` returns `*LocalIndex` (pointer to `[2]int`), forcing a heap allocation on every `OpLoadLocal` and `OpStoreLocal` in the VM loop (`machine_context.go:666-683`).

The VM already has `slot` and `depth` as raw integers from `DecodeLocalIndex`. Fix: add `GetLocalBindingBySlotDepth(slot, depth int)` and `SetLocalValueBySlotDepth(slot, depth int, v values.Value)` to `EnvironmentFrame`, bypassing the `*LocalIndex` allocation entirely.

**Files:** `environment/environment_frame.go`, `machine/machine_context.go` (OpLoadLocal/OpStoreLocal cases)

### 2. Binding Storage: `[]*Binding` → `[]Binding`

**Impact:** ~2-3 GB (est.), ~112.9M fewer allocations
**Effort:** Medium | **Risk:** Medium

`CopyForApply` allocates two slices per call:
```go
allBindings := make([]Binding, len(p.bindings))   // batch values
q.bindings = make([]*Binding, len(p.bindings))     // pointer slice
```

If `LocalEnvironmentFrame.bindings` were `[]Binding` instead of `[]*Binding`, only one allocation per `CopyForApply`. Every accessor that dereferences `*Binding` needs updating — mechanical but touches many call sites.

**Audit required:** Every place holding a `*Binding` pointer must not persist across mutations.

**Files:** `environment/local_environment_frame.go`, `environment/environment_frame.go`, all binding accessors

### 3. EnvironmentFrame Struct Pooling or Fusion

**Impact:** ~5.6 GB (16%), ~112.9M fewer allocations
**Effort:** Medium | **Risk:** Medium

Each `Apply` creates both a `LocalEnvironmentFrame` (via `CopyForApply`) and an `EnvironmentFrame` (via `NewEnvironmentFrameWithParent`). The `EnvironmentFrame` is a thin wrapper (~56 bytes).

**Option A — Pool:** `sync.Pool` for `EnvironmentFrame`. Lifetime follows acquire/release: created in `Apply`, dead when `RestoreAndRelease` overwrites `mc.env`. A "captured" flag set by `OperationMakeClosure` prevents pooling captured frames.

**Option B — Fuse:** Single struct embedding both `EnvironmentFrame` and `LocalEnvironmentFrame`. Eliminates one allocation per `Apply`.

**Files:** `environment/environment_frame.go`, `machine/machine_context.go` (Apply), `machine/operation_make_closure.go`

### 4. Stack Capacity Tuning

**Impact:** Unknown (potentially 1-3 GB)
**Effort:** Low | **Risk:** Low

Stack pool capacity is 8 (`machine/pool.go:32`). Average stack depth at PopAll is ~1.4, but a long tail of deeper stacks triggers growth allocations. Measure actual depth distribution; bump to 16 or 32 if warranted.

**Investigation:** Add max-depth tracking to VMCounters, run zebra benchmark, check distribution.

**Files:** `machine/pool.go`, `machine/machine_context.go` (counters)

### 5. Copy-on-Write Continuation Sharing

**Impact:** ~2.9 GB (8.5%)
**Effort:** High | **Risk:** High

`CurrentContinuation()` calls `DeepCopy()`, deep-copying the entire continuation chain. This is needed because `RestoreAndRelease` pools frames — if captured continuations share frames with the live chain, pooling corrupts the capture.

**Alternative:** Mark frames as "shared" when captured. Normal return (`RestoreAndRelease`) checks the flag and skips pooling for shared frames (falls through to GC). `DeepCopy` becomes unnecessary.

**Correctness concern:** Any bug silently corrupts continuations. Requires exhaustive testing with coroutines, dynamic-wind, and composable continuations.

**Files:** `machine/machine_continuation.go`, `machine/machine_context.go` (SaveContinuation, RestoreAndRelease, CurrentContinuation)

### 6. CopyForApply Avoidance (Escape Analysis)

**Impact:** Up to 15.5 GB (44.6%)
**Effort:** Very High | **Risk:** Very High

The largest single allocation source. Every closure application copies the local environment to prevent recursive parameter corruption.

Many closures are called in tail position or are non-recursive. The compiler could emit a "no-copy apply" for templates where escape analysis proves bindings aren't accessed concurrently. Effectively: lightweight stack frames for safe closures.

**Prerequisite:** Compiler escape analysis pass — needs to prove that no `SaveContinuation` intervenes between binding creation and last use within a template.

**Files:** `machine/compile_validated.go`, `machine/machine_context.go` (Apply), `environment/local_environment_frame.go`

## Priority Order

| # | Optimization | Alloc Savings | Effort | Risk |
|---|-------------|---------------|--------|------|
| 1 | LocalIndex → value type | ~1.7 GB (4.9%) | Low | Low |
| 2 | `[]*Binding` → `[]Binding` | ~2-3 GB (est.) | Medium | Medium |
| 3 | EnvironmentFrame pool/fusion | ~5.6 GB (16%) | Medium | Medium |
| 4 | Stack capacity tuning | ~1-3 GB (est.) | Low | Low |
| 5 | CoW continuation sharing | ~2.9 GB (8.5%) | High | High |
| 6 | CopyForApply avoidance | ~15.5 GB (44.6%) | Very High | Very High |

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
