# Opcode Promotion Plan

**Status:** All 3 phases complete. Phase 1 (#497), Phase 2 (#498), Phase 3: cons/*/÷.
**Date:** 2026-03-13
**Prereq:** Profiling infrastructure fix (pool opcodeHits bug) — merged (#495)

## Context

The VM dispatch loop (`machine/machine_context.go:Run()`) uses a two-tier model:
- **Primary switch**: ~64 opcodes inlined in the main dispatch loop (post Phase 1+2+3; was ~37 pre-promotion)
- **OpComplex side table**: Complex operations dispatched via `sideTable[arg].Apply(mc)`

Peephole optimizer (`machine/peephole.go`) fuses common instruction sequences:
- Wave 4: Load+Push fusions (`OpPushLiteral`, `OpPushGlobal`, `OpPushLocal`)
- Wave 5: `OpPullApply` (Pull + Apply)
- Wave 7: `OpCallForeignCached` / `OpCallForeignCachedTail` (eliminate SaveCont+Push+Pull+Apply for foreign closures)
- Wave 8: `OpCallLocal` / `OpCallCachedBinding` (same for non-foreign callables)
- Wave 9: Promoted primitives (`OpEqQ`, `OpVectorQ`, `OpVectorRef` + tail variants)

Wave 9 ops bypass the full foreign call path (Drain + arity check + env bind + indirect call) with direct inline logic.

### Current Promoted Primitives

| Primitive | Opcodes | Arity | Inline Logic | Target Workload |
|-----------|---------|-------|-------------|-----------------|
| `eq?` | `OpEqQ`, `OpEqQTail` | 2 | Pointer/symbol-key compare | miniKanren/schelog solver |
| `vector?` | `OpVectorQ`, `OpVectorQTail` | 1 | Type assert on `*values.Vector` | Solver, pattern matching |
| `vector-ref` | `OpVectorRef`, `OpVectorRefTail` | 2 | Type assert + bounds check + get | Solver, vector-heavy code |

### Promotion Pattern (reference: `machine/call_promoted.go`)

Each promoted primitive needs:
1. **Inline function** (e.g., `inlineEq`) — pops args via `PopN(arity)`, sets value register
2. **Name guard** in dispatch — type assert to `*ForeignClosure` AND name field check; fallback to `callPromotedFallback` on mismatch
3. **Non-tail + tail opcode pair** — non-tail does `mc.pc++`, tail does `mc.returnImmediate()`
4. **Peephole recognition** — `promotedOpForName()` switch in `machine/peephole.go`
5. **Opcode registration** — constants in `machine/opcode.go`, metadata in `opcodeTable`

The peephole optimizer deletes `SaveContinuation` for promoted ops (they don't need stack isolation), so the fallback path uses `PopN(arity)` instead of `Drain()` and manually saves continuation for non-tail calls.

## Profiling Data

### Method

Instrumented `callForeignCached` and `applyForeign` with per-primitive name counting (behind `WILE_OPCODE_HITS` guard). Ran the full Larceny/Gabriel canonical benchmark suite plus nqueens, browse, destruct.

### Bug Fixed: Pool opcodeHits Initialization

`AcquireTopLevelContext` (the pool path used by `Engine.Eval`/`Engine.Run`) acquired a zero-value `MachineContext` from `subContextPool` without initializing `counters.opcodeHits`. The `WILE_OPCODE_HITS` feature never worked through the public API.

Fix: added `mc.counters.opcodeHits = newOpcodeHits()` to `AcquireTopLevelContext` in `machine/pool.go`. Sub-contexts (never read via `LastCounters()`) remain zero-cost. Tests added: `TestAcquireTopLevelContext_InitializesOpcodeHits`, `TestAcquireTopLevelContext_OpcodeHitsZeroedAfterReuse`.

### Per-Primitive Call Frequency (Larceny Suite)

**Numeric-heavy benchmarks** (tak, cpstak, fib, triangl, diviter, divrec, ackermann, sumfp, primes, peval):

| Benchmark | Ops | Foreign Calls | Top Primitives |
|-----------|-----|---------------|----------------|
| tak | 15.4M | 1.1M | `<` 57%, `-` 43% |
| cpstak | 18.0M | 1.2M | `<` 57%, `-` 43% |
| fib | 44.1M | 6.7M | `<=` 40%, `-` 40%, `+` 20% |
| triangl | 3.5M | 0.5M | `<=` 40%, `-` 40%, `+` 20% |
| sumfp | 136.0M | 24.0M | `+` 67%, `>=` 33% |
| diviter | 102.2M | 18.0M | `<=` 33%, `/` 33%, `-` 33% |
| divrec | 34.0M | 6.0M | `<=` 33%, `/` 33%, `-` 33% |
| ackermann | 72.9M | 12.5M | `=` 50%, `-` 33%, `+` 17% |
| primes | 17.8M | 2.9M | `>` 22%, `+` 19%, `*` 19%, `modulo` 18%, `=` 18% |
| peval | 4.8M | 0.6M | `+` 50%, `*` 33%, `<` 17% |

**List-heavy benchmarks** (takl, destruct, browse):

| Benchmark | Ops | Foreign Calls | Top Primitives |
|-----------|-----|---------------|----------------|
| takl | 123.0M | 17.7M | `cdr` 49%, `pair?` 27%, `null?` 24% |
| destruct | 4.6M | 1.0M | `cdr` 40%, `null?` 20%, `car` 20%, `append` 10%, `cons` 10% |
| browse | 28.8M | 5.1M | `pair?` 40%, `+` 20%, `cdr` 20%, `car` 20% |

**Mixed benchmarks** (deriv, sieve, nqueens):

| Benchmark | Ops | Foreign Calls | Top Primitives |
|-----------|-----|---------------|----------------|
| deriv | 8.2M | 1.2M | `car` 19%, `eq?` 17%, `null?` 14%, `cdr` 14%, `cons` 14%, `pair?` 11%, `list` 10% |
| sieve | 8.3M | 1.4M | `car` 36%, `null?` 13%, `cons` 13%, `cdr` 12%, `=` 12%, `modulo` 12% |
| nqueens | 227.8M | 28.0M | `=` 32%, `+` 22%, `null?` 12%, `car` 12%, `-` 10%, `cdr` 8% |

### Aggregate Primitive Rankings

Across all 16 successful benchmarks, ranked by total call volume:

| Rank | Primitive | Approx Total Calls | Workload Coverage |
|------|-----------|-------------------|-------------------|
| 1 | `+` | ~28M | Numeric + mixed |
| 2 | `-` | ~16M | Numeric |
| 3 | `=` | ~16M | Numeric + mixed |
| 4 | `cdr` | ~13M | List + mixed |
| 5 | `<=` | ~9M | Numeric |
| 6 | `null?` | ~8M | List + mixed |
| 7 | `>=` | ~8M | Numeric (sumfp) |
| 8 | `/` | ~8M | Numeric (diviter) |
| 9 | `pair?` | ~7M | List |
| 10 | `car` | ~5M | List + mixed |
| 11 | `>` | ~2M | Numeric |
| 12 | `<` | ~1.4M | Numeric (tak) |
| 13 | `*` | ~0.7M | Numeric |
| 14 | `modulo` | ~0.7M | Numeric |
| 15 | `cons` | ~0.6M | List + mixed |
| 16 | `not` | — | Scheme-defined `MachineClosure` — invisible to `ForeignClosure` profiling |

### Opcode-Level Profile (fib example)

From 361K ops in `(fib 20)`:

| Opcode | Share | Role |
|--------|-------|------|
| SaveContinuation | 18.2% | Non-tail call frame save |
| PushLiteral | 12.1% | Load literal + push |
| PushLocal | 12.1% | Load local + push |
| CallForeignCached | 12.1% | Primitive call |
| Push | 12.1% | Value reg to stack |
| PullApply | 9.1% | Dequeue proc + apply |
| PushCachedBinding | 9.1% | Load cached binding + push |
| BranchOnFalseValue | 6.1% | Conditional branch |

`CallForeignCached` is 7-18% of all ops across every benchmark.

## Promotion Plan

### Phase 1: List Predicates and Accessors — Complete (#497)

| Primitive | Opcodes | Arity | Inline Logic |
|-----------|---------|-------|-------------|
| `null?` | `OpNullQ`/`Tail` | 1 | `values.IsEmptyList(o)` |
| `pair?` | `OpPairQ`/`Tail` | 1 | `_, ok := o.(*values.Pair)` |
| `car` | `OpCar`/`Tail` | 1 | Type assert Tuple + `.Car()` |
| `cdr` | `OpCdr`/`Tail` | 1 | Type assert Tuple + `.Cdr()` |

Results (list-heavy Larceny benchmarks):

| Benchmark | Change |
|-----------|--------|
| takl | **-43%** |
| browse | **-32%** |
| destruct | **-29%** |
| deriv | **-17%** |

### Phase 2: Binary Arithmetic and Comparisons — Complete (#498)

2-arg only; variadic calls stay on `CallForeignCached`.

| Primitive | Opcodes | Arity | Inline Logic |
|-----------|---------|-------|-------------|
| `+` | `OpAdd`/`Tail` | 2 | `a.Add(b)` |
| `-` | `OpSub`/`Tail` | 2 | `a.Subtract(b)` |
| `<` | `OpNumLt`/`Tail` | 2 | Complex check + `a.LessThan(b)` |
| `<=` | `OpNumLe`/`Tail` | 2 | Complex check + NaN check + `!b.LessThan(a)` |
| `>` | `OpNumGt`/`Tail` | 2 | Complex check + `b.LessThan(a)` |
| `>=` | `OpNumGe`/`Tail` | 2 | Complex check + NaN check + `!a.LessThan(b)` |
| `=` | `OpNumEq`/`Tail` | 2 | `numericEquals(a, b)` (IEEE 754 + cross-type) |

Results (numeric-heavy Larceny benchmarks, vs pre-promotion master):

| Benchmark | Change |
|-----------|--------|
| sumfp | **-71%** |
| ackermann | **-57%** |
| fib | **-40%** |
| tak | **-30%** |
| diviter | **-16%** |

#### No defer/recover Needed

The initial plan assumed promoted arithmetic would need `defer/recover` to catch panics from the `Number` interface. This was wrong on two counts:

**1. The hot path already has no defer/recover.** `callForeignCached` (the path that promoted ops replace) calls `fcls.fn(mc)` directly with no defer/recover. The defer/recover exists only in `OperationForeignFunctionCall.Apply`, which is the `OpComplex` side table path for `NewVMForeignClosure` primitives (`map`, `for-each`, `apply`, `call/cc` — things that do nested VM execution). Promoted ops replace `callForeignCached`, not `OperationForeignFunctionCall`.

| Path | defer/recover? | Used by |
|------|---------------|---------|
| `callForeignCached` | **No** | `OpCallForeignCached` — all peephole-optimized primitives |
| `applyForeign` | **No** | `OpApply`/`OpPullApply` — uncached foreign calls |
| `OperationForeignFunctionCall.Apply` | **Yes** | `OpComplex` side table — nested-VM primitives only |

**2. The numeric tower cannot panic from valid Number inputs.** A systematic audit of every panic site in `values/` found:

- **All Number methods** (`Add`, `Subtract`, `Multiply`, `LessThan`, `Compare`) — cannot panic when both operands implement `Number`. The dispatch tables are complete (validated at init by `validatePromotionTable`). No gaps exist.
- **Division by exact zero** — caught by guard clauses in every `Divide` method, returns `(nil, ErrDivisionByZero)`. Does not panic.
- **Division by inexact zero** (e.g., `1.0 / 0.0`) — produces `+Inf` or `NaN` via IEEE 754. Go's `float64` division does not panic. `big.Float` division catches `big.ErrNaN` locally in `recoverNaN`. None of this reaches the VM.
- **Overflow** (`int64` to `BigInteger`) — handled by `addInt64`/`subInt64`/`mulInt64` helpers that detect overflow and auto-promote. No panic.

The only panics in `values/` are programmer assertions: type-switch exhaustiveness guards (`"unsupported type %T"`), startup validation (`validatePromotionTable`), and `emptyList.Car()`/`emptyList.Cdr()` (guarded by the primitives that call them).

**Conclusion:** Type-assert both args as `Number`, then call the method directly. No defer/recover, no residual panic risk.

Implemented in `machine/call_promoted_arithmetic.go` via shared `popTwoNumbers` helper (pop + type assert + counter update) called by all seven arithmetic inline functions.

#### Variadic Arity

`+`, `-`, `*`, `/` are variadic in R7RS. The peephole optimizer currently checks `argCount == promotedArity`. For Phase 2, promote only the 2-arg case (which is >99% of calls in practice). The variadic case stays on the `CallForeignCached` path.

### Phase 3: cons, *, / — Complete

| Primitive | Opcodes | Arity | Inline Logic |
|-----------|---------|-------|-------------|
| `cons` | `OpCons`/`Tail` | 2 | `values.NewCons(car, cdr)` — no validation |
| `*` | `OpMul`/`Tail` | 2 | `a.Multiply(b)` — same pattern as `+`/`-` |
| `/` | `OpDiv`/`Tail` | 2 | `a.Divide(b)` — division-by-zero error handling |

2-arg only; variadic `*` and `/` calls stay on `CallForeignCached`.

**Not promoted (diminishing returns):**

| Primitive | Reason |
|-----------|--------|
| `modulo` | 700K calls but `integerDivisionOp` logic (BigInteger dispatch, inexact tracking, ExtractInteger) too complex to duplicate for the benefit |
| `not` | Scheme-defined procedure — requires compiler-level recognition, not peephole |
| `list` | Variadic, allocating. Not a good candidate |
| `append` | O(n), allocating. Dispatch overhead negligible vs work done |

## Demotion Analysis

**No current opcodes warrant demotion.** The primary switch has ~64 cases, well within Go's jump table efficiency. All opcodes either:
- Are hot across most workloads (Push, PushLocal, SaveContinuation, etc.)
- Serve specific workloads where they're dominant (vector ops for solvers)
- Have trivial inline logic (the dispatch cost of OpComplex indirection would be comparable)

`OpMakeClosure` was previously in OpComplex and was promoted — it shows 3.5% in nqueens (closure-creating workloads). Worth keeping.

## Files to Modify

| File | Changes |
|------|---------|
| `machine/opcode.go` | New opcode constants + metadata entries |
| `machine/call_promoted.go` | `promotedOpForName` switch, `execPromoted`, `callPromotedFallback`, predicate/accessor inlines |
| `machine/call_promoted_arithmetic.go` | Arithmetic/comparison inlines, `popTwoNumbers` helper, `numericEquals` |
| `machine/machine_context.go` | New `case` arms in `Run()` dispatch switch |
| `machine/peephole.go` | Already generic — `promotedOpForName` is the only change needed |
| `machine/opcode_test.go` | Opcode metadata coverage |
| `machine/opcode_fusion_test.go` | Peephole fusion + fallback tests for new ops |

## Measurement Plan

1. Commit profiling fix (pool opcodeHits)
2. Capture baseline: `WILE_OPCODE_HITS=1` on full Larceny suite via `Engine.LastCounters().OpcodeHistogram()`
3. Implement Phase 1, measure delta on takl, destruct, browse, deriv, sieve, nqueens
4. Implement Phase 2, measure delta on full numeric suite (fib, tak, ackermann, sumfp, diviter)
5. Compare `go test -bench=BenchmarkRun` before/after for regression check

## Open Questions

1. ~~**Should per-primitive call counting be permanent infrastructure?**~~ **Resolved: yes.** `callCounts map[string]uint64` added to `VMCounters`, gated by `WILE_OPCODE_HITS`. `RecordCall(name)` called at three dispatch sites: `execPromoted` (all promoted ops), `callForeignCached` (peephole non-promoted), `applyForeign` (uncached). `CallHistogram()` formats output by frequency. Access via `engine.LastCounters().CallHistogram()`.

2. ~~**Profiling blind spot: `MachineClosure` calls are untracked.**~~ **Resolved.** `RecordCall` now also fires in `Apply()` for named `MachineClosure` calls. Anonymous lambdas (empty template name) are skipped. The `CallHistogram()` (formerly `PrimitiveCallHistogram()`) now shows both foreign primitives and Scheme-defined procedures in a single unified ranking.

3. **`sum` benchmark overflows call depth.** `sum(10000)` is non-tail-recursive with n exactly at the `DefaultMaxCallDepth` limit (10,000). Not a VM bug — the benchmark needs `n` lowered or a tail-recursive rewrite.

4. ~~**Can the defer/recover in `OperationForeignFunctionCall.Apply` be moved to `RunWithEscapeHandling`?**~~ **Resolved: no.** Post-promotion, `OperationForeignFunctionCall` has exactly one user (call/cc escape closure). Moving defer/recover to Run would save ~30ns on this single cold path while losing continuation context at the panic point (needed by `goErrorToSchemeException` for stack traces). The panic audit confirmed all reachable panics are programmer assertions, not runtime conditions. Cost/benefit is poor.

## Panic Audit Summary (for reference)

Audit date: 2026-03-13. Traced every `panic()` in `values/` reachable from arithmetic.

**Not reachable from normal Scheme arithmetic (programmer assertions only):**
- `big_complex.go:72,115,140,395` — type-switch exhaustiveness in BigComplex helpers
- `big_float.go:87` — re-panic of non-`big.ErrNaN` panics (Go stdlib edge case)
- `promotion.go:278,283` — startup validation of dispatch tables
- `promotion.go:308,343,367` — `Promote`, `NumberToFloat64`, `NumberToComplex128` type guards
- `numeric_tower.go:157` — `ExactnessOf` type guard
- `empty_list.go:83,88` — `Car`/`Cdr` on empty list (guarded by primitives)
- `pair.go:139,169,233,236` — `Append`/`Must` on improper lists (guarded by primitives)

**Division by zero:** All 7 `Divide` methods guard exact zero with `(nil, ErrDivisionByZero)` error return. Inexact zero produces IEEE 754 `+Inf`/`NaN` — Go `float64` does not panic. `big.Float` NaN is caught locally by `recoverNaN`.
