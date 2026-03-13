# Opcode Promotion Plan

**Status:** Profiling complete, implementation not started
**Date:** 2026-03-13
**Prereq:** Profiling infrastructure fix (pool opcodeHits bug) — done, not yet committed

## Context

The VM dispatch loop (`machine/machine_context.go:Run()`) uses a two-tier model:
- **Primary switch**: ~37 opcodes inlined in the main dispatch loop
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
| 16 | `not` | — | Absorbed into branching (special form, not primitive call) |

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

### Phase 1: List Predicates and Accessors

Zero-risk, follows existing `eq?` pattern exactly. No numeric tower, no variadic args, no allocations.

| Primitive | Arity | Inline Logic | Approx Savings |
|-----------|-------|-------------|----------------|
| `null?` | 1 | `values.IsEmptyList(o)` | 8M calls (takl, nqueens, sieve, destruct) |
| `pair?` | 1 | `_, ok := o.(*values.Pair)` | 7M calls (takl, browse, deriv) |
| `car` | 1 | Type assert `*values.Pair` + `.Car()` | 5M calls |
| `cdr` | 1 | Type assert `*values.Pair` + `.Cdr()` | 13M calls |

Implementation: 4 new inline functions in `call_promoted.go`, 8 new opcodes (non-tail + tail each), extend `promotedOpForName` switch.

`car` and `cdr` need error handling for non-pair input (return wrapped `ErrNotAPair`), same pattern as `inlineVectorRef`.

**Expected impact on takl:** `cdr`+`pair?`+`null?` are 100% of foreign calls (17.7M). Each currently goes through Drain + arity check + env bind + indirect call. Promotion eliminates all of that.

### Phase 2: Binary Arithmetic and Comparisons

Higher payoff but needs numeric tower handling.

| Primitive | Arity | Inline Logic | Challenge |
|-----------|-------|-------------|-----------|
| `+` (2-arg) | 2 | `a.(Number).Add(b.(Number))` | Panics on type mismatch |
| `-` (2-arg) | 2 | `a.(Number).Subtract(b.(Number))` | Same |
| `<` | 2 | `a.(Number).LessThan(b.(Number))` | Same |
| `<=` | 2 | `!b.(Number).LessThan(a.(Number))` | Same |
| `>` | 2 | `b.(Number).LessThan(a.(Number))` | Same |
| `>=` | 2 | `!a.(Number).LessThan(b.(Number))` | Same |
| `=` (2-arg) | 2 | `a.(Number).Equal(b.(Number))` | Same |

#### The Panic Problem

The `values.Number` interface methods (`Add`, `Subtract`, `LessThan`, etc.) return `Number`, not `(Number, error)`. They panic on type mismatch (e.g., adding a pair to an integer). Currently, `OperationForeignFunctionCall.Apply` has a `defer/recover` that catches these panics and converts them to `ErrExceptionEscape`.

**Option A: Per-op defer/recover.** Each promoted arithmetic op wraps its inline logic in `defer/recover`. Simple, follows existing pattern. Cost: ~30ns per call on the non-panic path (Go defer/recover overhead). At 28M `+` calls in sumfp, that's ~0.84s of overhead — likely worse than the savings.

**Option B: Type-check before calling Number methods.** Assert both args are `Number` before calling. If either isn't, fall back to `callPromotedFallback`. The Number methods themselves won't panic because both args are already validated as Number. This avoids defer/recover entirely.

```go
func inlineAdd(mc *MachineContext) error {
    b := mc.evals.Pop()
    a := mc.evals.Pop()
    an, ok := a.(values.Number)
    if !ok {
        return applyCallableError(mc, werr.WrapForeignErrorf(
            werr.ErrNotANumber, "+: expected number, got %s", a.SchemeString()))
    }
    bn, ok := b.(values.Number)
    if !ok {
        return applyCallableError(mc, werr.WrapForeignErrorf(
            werr.ErrNotANumber, "+: expected number, got %s", b.SchemeString()))
    }
    mc.SetValue(an.Add(bn))
    return nil
}
```

**Remaining risk:** `Number.Add` can still panic for division-by-zero, overflow to bignum edge cases, etc. These are extremely rare in practice. A single `defer/recover` per Run() iteration (wrapping the entire switch) could catch them, but that changes the dispatch loop structure.

**Option C: Change Number interface to return (Number, error).** Clean but massive — touches every numeric type (Integer, BigInteger, Float, BigFloat, Rational, Complex) and every caller. Deferred to a separate plan.

**Recommendation:** Option B for Phase 2. Type-check args, skip defer/recover. Accept the theoretical panic risk for rare edge cases (they'd crash the VM, which is already what happens for other panics not caught by OperationForeignFunctionCall).

#### Variadic Arity

`+`, `-`, `*`, `/` are variadic in R7RS. The peephole optimizer currently checks `argCount == promotedArity`. For Phase 2, promote only the 2-arg case (which is >99% of calls in practice). The variadic case stays on the `CallForeignCached` path.

### Phase 3: Remaining Candidates (Lower Priority)

| Primitive | Notes |
|-----------|-------|
| `cons` | Allocates a new `*values.Pair` — promotion saves dispatch but not allocation |
| `modulo` | 700K calls (primes, sieve). Numeric tower + potential division-by-zero |
| `not` | Already absorbed into branching by special form compilation — not a primitive call |
| `list` | Variadic, allocating. Not a good candidate. |
| `append` | O(n), allocating. Dispatch overhead negligible vs work done. |

## Demotion Analysis

**No current opcodes warrant demotion.** The primary switch has ~37 cases, well within Go's jump table efficiency. All opcodes either:
- Are hot across most workloads (Push, PushLocal, SaveContinuation, etc.)
- Serve specific workloads where they're dominant (vector ops for solvers)
- Have trivial inline logic (the dispatch cost of OpComplex indirection would be comparable)

`OpMakeClosure` was previously in OpComplex and was promoted — it shows 3.5% in nqueens (closure-creating workloads). Worth keeping.

## Files to Modify

| File | Changes |
|------|---------|
| `machine/opcode.go` | New opcode constants + metadata entries |
| `machine/call_promoted.go` | New inline functions + extend `promotedOpForName` |
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

1. **Should per-primitive call counting be permanent infrastructure?** The profiling data was collected via temporary instrumentation. Adding a `PrimitiveCalls map[string]uint64` field to `VMCounters` (behind `WILE_OPCODE_HITS`) would make future analysis trivial. Cost: one map write per foreign call when profiling is enabled.

2. **Tail-call arithmetic:** For tail-position `(+ a b)`, the promoted op does `inlineAdd` + `returnImmediate`. But `returnImmediate` restores the caller's continuation. If the caller is also doing arithmetic in tail position, this chains correctly. Confirm no edge cases with `call/cc` capturing mid-arithmetic.

3. **`sum` benchmark overflows call depth.** `sum(1000000)` hits the 10K call depth limit. This is a non-tail recursive sum — expected behavior. The benchmark needs adjustment or the limit needs raising for benchmarking only.
