# Bignum Allocation Reduction for Tight-Loop Arithmetic

**Status:** Design draft — not started.

**Scope:** Reduce heap allocations in `(*BigInteger).Add` / `.Multiply` / `.Subtract` / `.Negate` and the corresponding dispatch-closure tables. Add an internal in-place arithmetic API for callers operating in tight loops (Bellman-Ford-shaped algorithms, large-bignum exponentiation, accumulator patterns). No change to the public R7RS-immutable `Number` interface; in-place ops are an internal-only optimization that must not leak into Scheme-visible semantics.

**Repository:** `aalpar/wile`. Files: `values/big_integer.go`, `values/integer.go`, `values/promotion.go`, plus new `values/numeric_scratch.go` (proposed).

## Motivation

The 3-hour `(graph-query-all ga-count run-name)` query on the `machine` package (`feedback-counting-semiring-on-cycles.md`) was dominated by bignum allocation, not bignum arithmetic. Wile's Bellman-Ford inner loop performs roughly E × V edge relaxations; on a 539-node, 623-edge graph with cycles that's ~335K relaxations per iteration × 538 iterations = ~180M relaxations. Each relaxation invokes `(*BigInteger).Add` and `(*BigInteger).Multiply`, and each of those allocates:

```go
// values/big_integer.go:192-198
func (p *BigInteger) Add(o Number) Number {
    v, ok := o.(*BigInteger)
    if ok {
        return &BigInteger{value: newBigIntFromOp((*big.Int).Add, p.value, v.value)}
        //     ^                    ^
        //     |                    one *big.Int per call
        //     one *BigInteger per call (3 allocations including the *big.Int's internal []Word)
    }
    ...
}
```

`newBigIntFromOp` at `big_integer.go:90` does `return fn(new(big.Int), v0, v1)` — allocating a fresh `*big.Int` for every operation. The wrapping `&BigInteger{...}` allocates the Number-interface wrapper. The `*big.Int`'s `[]Word` storage grows as values grow, with each growth re-allocating.

Conservative estimate for the 3-hour query: ~600M heap allocations during the run (multiplications also allocate `*big.Int` for the product and the `*BigInteger` wrapper). At ~100ns per allocation including GC amortization, that's ~60 seconds of pure allocation overhead. The remaining hours are bignum arithmetic on continually-growing values (which `math/big` already handles optimally via Karatsuba/Toom-Cook — that part is not optimizable from outside the standard library).

The right fix: provide an in-place arithmetic API for known-target patterns, plus a pool for transient `*big.Int` scratch values. This is library-internal — the public `Number` interface stays immutable per R7RS semantics.

## Current state audit

What's already in place (verified by reading `values/big_integer.go` and `values/integer.go`):

| Capability | Status | Notes |
|---|---|---|
| Fixnum fast-path (`Integer` with int64) | ✓ | `integer.go` lines 114, 124, 137, 146 — overflow-detecting `addInt64`/`subInt64`/`mulInt64`/`negateInt64` from `Hacker's Delight` |
| Integer cache (-32768..32767) | ✓ | per `values/CLAUDE.md` "Optimizations" |
| Karatsuba multiplication | ✓ | `math/big` provides this transparently |
| Toom-Cook 3-way / FFT | ✓ | `math/big` switches automatically at higher digit counts |
| Dispatch tables (per-type, per-Kind closures) | ✓ | `promotion.go`: `makeArithmeticDispatch` builds `[numKinds]func` arrays — 41 tables, 294 closures |
| Result pooling | ✗ | every `Add`/`Multiply` allocates fresh `*big.Int` |
| In-place arithmetic API | ✗ | no `AddInPlace`/`MulInPlace` exposed even internally |
| Scratch buffer reuse | ✗ | no convention for callers in tight loops |

The first four items mean the "make bignum arithmetic faster" question is already answered as well as the standard library allows. The bottom three are the actual optimization surface remaining.

## Design

### Layer 1 — internal in-place arithmetic on `*big.Int`

Add unexported helpers in `values/numeric_scratch.go` (new file):

```go
// addBigIntInPlace stores p.value + v.value into dest.value, reusing dest's
// existing []Word storage when capacity allows. Returns dest for chaining.
// dest may alias p or v.
func addBigIntInPlace(dest, p, v *BigInteger) *BigInteger {
    dest.value.Add(p.value, v.value)
    return dest
}

func mulBigIntInPlace(dest, p, v *BigInteger) *BigInteger {
    dest.value.Mul(p.value, v.value)
    return dest
}

// negateBigIntInPlace stores -p.value into dest.value.
func negateBigIntInPlace(dest, p *BigInteger) *BigInteger {
    dest.value.Neg(p.value)
    return dest
}
```

These are unexported. The public `(*BigInteger).Add` / `.Multiply` etc. remain immutable and allocate fresh results. Callers that *know* they own the destination (Bellman-Ford with a per-iteration result map) use the in-place variants.

Critically: `math/big.Int.Add(x, y, z)` is documented to "set z to the sum x+y and return z", and does NOT reallocate `z`'s `[]Word` if `z` has sufficient capacity. So in-place reuse genuinely eliminates allocation when the result fits in already-allocated space. When the result is larger, `math/big` reallocates, but only by the amount needed — much cheaper than allocating fresh storage every call.

### Layer 2 — scratch pool

Add a free-list pool for transient `*BigInteger` values used as scratch in arithmetic compositions like `dest = dest + (a × b)`:

```go
// numericScratchPool returns *BigInteger scratch values for tight loops.
// Caller MUST return values via PutScratch when done.
type numericScratchPool struct {
    mu   sync.Mutex
    pool []*BigInteger
}

func (p *numericScratchPool) Get() *BigInteger {
    p.mu.Lock()
    defer p.mu.Unlock()
    if len(p.pool) == 0 {
        return &BigInteger{value: new(big.Int)}
    }
    last := len(p.pool) - 1
    v := p.pool[last]
    p.pool = p.pool[:last]
    return v
}

func (p *numericScratchPool) Put(b *BigInteger) {
    if b == nil { return }
    p.mu.Lock()
    p.pool = append(p.pool, b)
    p.mu.Unlock()
}
```

Pool storage is small (16-64 `*BigInteger` slots typically suffice; the high-water mark is the depth of the deepest compound expression in the inner loop).

For single-threaded VMs (Wile's default) the mutex is over-engineering — use a sync.Pool or just a goroutine-local slice. The mutex above is for the conservative case; the actual implementation should profile and pick the right strategy.

### Layer 3 — caller patterns

Document a convention for arithmetic in tight loops:

```go
// Inside a Bellman-Ford-style inner loop:
scratch := scratchPool.Get()
defer scratchPool.Put(scratch)

for each edge (u, v, w) {
    mulBigIntInPlace(scratch, d[u], w)        // scratch = d[u] * w, no alloc
    addBigIntInPlace(d[v], d[v], scratch)     // d[v] += scratch, no alloc
}
```

The result is that an inner loop doing E relaxations allocates O(V) bignums total (one per `d[v]` slot, pre-allocated once), not O(E × V) bignums (one per arithmetic op).

For the 3-hour query: ~600M allocations → ~540 allocations (V = 539). Roughly a 1,000,000× reduction in allocation count.

### Layer 4 — Bellman-Ford integration (in `(wile algebra graph)`)

The graph library would need to know to use the in-place API. This couples the algebra layer to the values layer in a way that's slightly invasive but bounded. Two implementation options:

**Option A: expose the in-place API to Scheme.** Add `big-integer-add!` / `big-integer-mul!` primitives that take a destination and two operands. The graph library's Bellman-Ford inner loop uses these directly when it detects the carrier is BigInteger.

**Option B: keep in-place internal to values/, add a fast-path detector in the algebra layer.** The graph library's `make-graph-analysis` could pre-allocate distance maps as BigInteger slots when the semiring is counting, and the Bellman-Ford inner loop could use a Go-side fast path that calls the in-place helpers directly without Scheme-level dispatch.

**Default:** Option B. Keeping in-place mutation out of Scheme-visible territory respects R7RS immutability semantics; the fast path is an implementation detail of the algebra library.

## Open design questions

- **Q-1 — Pool granularity.** Per-VM, per-goroutine, or global? Wile is single-threaded per Engine but supports SRFI-18 threads within an Engine. **Default:** per-VM pool stored on the MachineContext. Cheap to plumb; threads share via the existing VM coordination.

- **Q-2 — In-place API exposure.** Internal-only (Option B), or expose to Scheme via `!`-suffixed primitives (Option A)? **Default:** internal only. R7RS makes Numbers immutable; mutation primitives would be a deviation worth justifying separately, not bundled with this optimization.

- **Q-3 — Scratch lifecycle.** Pool indefinitely, or shrink under GC pressure? **Default:** sync.Pool semantics — Go's GC handles eviction automatically. Simpler than a hand-managed shrinking strategy.

- **Q-4 — Fast-path detection in algebra layer.** How does `make-graph-analysis` know "this is the counting semiring on BigInteger carriers" without coupling to values/ specifics? **Default:** add a `numeric-carrier?` predicate to the semiring record that returns the carrier type (`int64`, `big-int`, `float64`, `log-float`). The algebra layer dispatches based on this. Adds one slot to the semiring record; orthogonal to the approximate-counting plan's `approximate-semiring?` slot.

## Implementation plan

### Phase 1 — internal in-place helpers + tests

- Add `values/numeric_scratch.go` with `addBigIntInPlace`, `mulBigIntInPlace`, `subBigIntInPlace`, `negateBigIntInPlace`. Unexported.
- Add `values/big_integer_scratch_test.go`:
  - Correctness: in-place result matches allocating result for sample inputs (small, large, negative, zero).
  - Aliasing: `dest = dest + dest`, `dest = dest * dest` produce correct results.
  - Storage reuse: verify (via `runtime.MemStats` or capacity inspection) that repeated in-place ops on the same `*BigInteger` don't reallocate when value grows slowly within capacity.

### Phase 2 — scratch pool

- Add scratch pool implementation. Default: sync.Pool-based (Go GC handles eviction).
- Add pool to `MachineContext` (per-VM) or to `Engine` (per-Engine, shared across VMs) — TBD by Q-1.
- Add `values/numeric_scratch_pool_test.go` with Get/Put correctness, no-leak invariants under panic.

### Phase 3 — numeric-carrier predicate on semiring records

- Add `(semiring-numeric-carrier s)` accessor returning a symbol from `'fixnum | 'big-int | 'float64 | 'log-float | 'modular | 'saturating | 'opaque` (or `#f` if unknown / mixed).
- Built-in semirings declare their carrier in the constructor:
  - `counting-semiring` → `'big-int` (worst-case)
  - `modular-counting-semiring P` → `'modular` (carrier is int64 mod P)
  - `log-counting-semiring` → `'log-float`
  - `approximate-counting-semiring CAP` → `'saturating`
  - `boolean-semiring` → `'fixnum` (single bit)
  - `tropical-semiring` → `'fixnum` or `'float64` depending on weight type
- This change is independent of the in-place arithmetic work and could ship alongside the approximate-counting plan instead. Note in both plans; pick one for primary owner.

### Phase 4 — Bellman-Ford fast path in `(wile algebra graph)`

- In `make-graph-analysis`, when the semiring carrier is `'big-int`, pre-allocate the distance map with V scratch-pool-backed `*BigInteger` slots.
- Inner loop uses `addBigIntInPlace` / `mulBigIntInPlace` directly, bypassing the dispatch closure for the hot operation.
- Other carriers (fixnum, float64, log-float, modular, saturating) get their own type-specialized fast paths in subsequent commits — out of scope for this plan, file separately.

### Phase 5 — benchmark + acceptance

- Add bench in `values/integer_bench_test.go` (existing file): `BenchmarkBigIntAddInPlace` vs `BenchmarkBigIntAddAllocating`. Expect ≥3× speedup on small-bignum workloads, ≥10× on large-bignum (where alloc dominates).
- Add bench in `bench/algebra-graph.sld` (or wherever the graph benches live): cyclic counting on a 50-node graph with reachable cycle (small enough to terminate; large enough for allocation to matter). With this plan + worklist B-F + the SCC-condensation plan, the cyclic-counting case should be tractable.
- Re-run the original 3-hour query under a 5-minute timeout. Acceptance: completes in under 5 minutes OR errors cleanly via the k-closedness check (whichever lands first).

### Phase 6 — docs + PR

- Update `values/CLAUDE.md` to document the in-place API and pool conventions.
- Update `docs/numeric/tower.md` to note the allocation-reduction option for hot-loop callers.
- Open PR, dual review.

## Risks

- **R-1 — R7RS immutability breach via in-place leakage.** If the in-place API escapes into Scheme-visible code (Option A leakage), Scheme programs could observe mutation of a value held by another variable, violating Number immutability. Mitigation: keep the API unexported (`addBigIntInPlace` not `AddBigIntInPlace`); document explicitly in `values/CLAUDE.md` that these are not for general use. The fast-path detector in the algebra layer accesses them via internal Go-side calls, never through Scheme dispatch.
- **R-2 — Aliasing bugs.** `addBigIntInPlace(d[v], d[v], scratch)` where `d[v]` aliases itself is the intended use, but `addBigIntInPlace(scratch, d[u], scratch)` where the scratch is both output and input could compute wrong results if `math/big`'s implementation doesn't handle aliasing. Mitigation: `math/big.Int.Add(x, y, z)` *does* handle aliasing per the standard library docs ("Add sets x to y+z" — x may alias y or z). Verify in Phase 1 tests with explicit aliasing cases.
- **R-3 — Pool churn under GC pressure.** A naive pool could pin scratch bignums indefinitely, defeating GC. Mitigation: use `sync.Pool` which the Go runtime drains under GC pressure.
- **R-4 — Coupling between algebra and values layers.** The fast-path detector in `(wile algebra graph)` needs to know "this is a BigInteger carrier" to invoke the right in-place ops. Mitigation: the `semiring-numeric-carrier` predicate (Phase 3) abstracts this as a symbol, not a Go type — the algebra layer dispatches on symbol, the values layer provides the in-place helpers. Layered cleanly.
- **R-5 — Per-VM pool requires plumbing through MachineContext.** Touching MachineContext is sensitive (per `memory/machine-tech-debt-plan-notes.md` and the structural-reduction work). Mitigation: start with a sync.Pool global (no MachineContext change); migrate to per-VM only if benchmarks show the global is a contention point. Likely unnecessary for single-threaded use.

## Acceptance criteria

- `(*BigInteger).Add` / `.Multiply` (the public methods) unchanged in behavior — same return types, same R7RS-visible immutability.
- In-place internal API exists and passes correctness + aliasing tests.
- Scratch pool exists and passes leak-under-panic tests.
- `semiring-numeric-carrier` accessor exposed on the semiring record.
- Bellman-Ford on counting semiring with BigInteger carrier uses the fast path (verifiable by allocation-counting in a bench).
- Microbench: in-place BigInteger.Add ≥3× faster than allocating equivalent for small bignums; ≥10× for ~1KB bignums.
- Macrobench: cyclic counting on a small (50-node) graph terminates in under 1 second using all optimizations from this + sibling plans. Original 3-hour query either terminates in under 5 minutes OR errors via k-closedness check.
- `make lint && make covercheck && make ci` all green.

## Out of scope

- Karatsuba / Toom-Cook implementations — `math/big` already provides them.
- Fixnum fast-path on `Integer` — already exists (`addInt64` etc.).
- Specializing Bellman-Ford for non-BigInteger carriers (fixnum, float64, log-float, modular, saturating) — file separately as a "carrier-specialized inner loops" plan after this lands. Each carrier has its own fast-path opportunity; bundling them creates a huge PR with cross-cutting concerns.
- BigFloat / BigComplex / Rational allocation reduction — same pattern applies, same fix shape, but separate plan to keep this one focused on BigInteger which is what the 3-hour incident exposed.
- Worklist Bellman-Ford convergence detection — sibling plan `2026-05-24-graph-worklist-bellman-ford.md`.
- Approximate counting semirings — sibling plan `2026-05-24-approximate-counting-semirings.md`.
- SCC condensation primitive — separate plan, not yet written, needed to make exact counting tractable on cyclic graphs.
- k-closedness check at `make-graph-analysis` time — separate plan, complements this work by preventing the pathological case at construction rather than at runtime.

## References

- `feedback-counting-semiring-on-cycles.md` — 3-hour incident memory.
- `2026-05-24-graph-worklist-bellman-ford.md` — sibling plan on convergence detection in the graph library.
- `2026-05-24-approximate-counting-semirings.md` — sibling plan on bounded-carrier semiring alternatives.
- `values/CLAUDE.md` — confirms fixnum fast-path exists, documents the numeric tower's promotion lattice, and the dispatch table architecture.
- `values/big_integer.go:192` — current `(*BigInteger).Add` allocation pattern.
- `values/integer.go:114` — current `(*Integer).Add` with overflow promotion to BigInteger.
- Go `math/big` package — Karatsuba, Toom-Cook, and FFT multiplication are already provided transparently; in-place ops are documented to handle aliasing.
- Warren, *Hacker's Delight* §2-12 — overflow detection idioms used by `addInt64`/`subInt64`/`mulInt64`.
- `2026-04-17-algebra-foundations-directions.md` — algebra roadmap (add §6 entry for "numeric-tower optimizations" in next revision).
