# Bignum Allocation Reduction for Tight-Loop Arithmetic

**Status:** Design draft — not started.

**Scope:** Reduce heap allocations in `(*BigInteger).Add` / `.Multiply` / `.Subtract` / `.Negate` and the corresponding dispatch-closure tables. Add an internal in-place arithmetic API for callers operating in tight loops (Bellman-Ford-shaped algorithms on **DAGs**, large-bignum exponentiation, accumulator patterns). No change to the public R7RS-immutable `Number` interface; in-place ops are an internal-only optimization that must not leak into Scheme-visible semantics.

**What this plan ships:** the monotone-add DAG kernel (Sub-path 4A) that all future Σ-semiring DAG variants (saturating, modular, log-float) will inherit, plus the weighted-bigint variant (Sub-path 4B) for user-constructed semirings with non-trivial weight functions. The named `bigint-counting-semiring` consumes 4A; the named `counting-semiring` is unchanged.

**What this plan does NOT ship:** cyclic-graph counting. The motivating 3-hour query has cycles; the counting semiring diverges on cycles regardless of per-op cost. Cyclic acceptance is gated on sibling plans (algebraic, structural, or correctness fixes). This plan is the *kernel* those sibling plans will compose with, not a standalone fix for cyclic counting.

**Repository:** `aalpar/wile`. Files: `values/big_integer.go`, `values/integer.go`, `values/promotion.go`, plus new `values/numeric_scratch.go` (proposed).

## Motivation

### Scope: one layer of a multi-layer failure

This plan addresses *one* of several orthogonal cost/correctness layers that combine to produce slow counting-semiring queries. The motivating incident — a 3-hour `(graph-query-all ga-count run-name)` query on the `machine` package (`feedback-counting-semiring-on-cycles.md`) — is a five-layer failure, only one of which is in scope here:

| Layer | Failure mode in the 3-hour case | Fix lives in |
|---|---|---|
| Algebraic | `Σ` over the infinite path set diverges in `(ℕ, +, ×, 0, 1)` — no finite answer exists | `2026-05-24-approximate-counting-semirings.md` (saturating, modular, log Σ-semiring variants) |
| Algorithmic | Naive Bellman-Ford doesn't detect convergence | Already addressed: `graph.scm:39-73` is worklist-style with per-edge `(equal? merged old-val)` convergence detection. Sibling plan `2026-05-24-graph-worklist-bellman-ford.md` describes a starting state that doesn't match current code. |
| Structural | Cyclic input forces iteration count proportional to cycle structure | `plans/2026-05-26-scc-condensation.md` (shipped) |
| Total-correctness gate | Reject ill-posed queries before running | k-closedness check plan (not yet written) |
| **Implementation cost** | **`(*BigInteger).Add` allocates ~3 heap objects per call, multiplied by every edge relaxation** | **This plan** |

The implementation-cost layer is a *multiplicative cost factor* on whatever algorithm and algebra eventually land at the upper layers. Fixing it is necessary for any heavy counting-semiring workload — including non-cyclic DAG path counting and finite-trace workloads where the counting semiring is well-defined and converges. It is **not sufficient** to make the 3-hour cyclic query terminate; that requires fixes at the algebraic, algorithmic, structural, and gate layers above. Conversely, fixing the upper layers without this one leaves a fast-converging algorithm whose every step is still bottlenecked on heap allocation.

The plan is *not* about changing the algebraic structure of the counting semiring. It is about reducing the per-operation cost of `+` and `*` on the carrier set, which the counting semiring (and every other semiring carrying `*BigInteger` values) realizes by calling `(*BigInteger).Add` and `(*BigInteger).Multiply`.

### What 4A actually delivers — the DAG kernel for Σ-semirings

Sub-path 4A's monotone-add kernel is more than an optimization for one named semiring; it is the **foundational DAG inner loop for the whole Σ-semiring family**. Future variants — saturating-counting (the approximate-counting plan's bounded variant), modular-counting (counts mod P), log-counting (log-domain Σ accumulator), and any future Σ-semiring whose addition is monotone-non-decreasing on its carrier — all share the same algorithmic shape:

```
on edge (u, v):  if d[u] contributes non-trivially, d[v] := d[v] ⊕ d[u]
```

The structural property that makes 4A scratch-free — *monotone non-decreasing addition, where the result equals the destination iff the source contributes the additive identity* — is shared across every Σ-semiring on a DAG. The carrier type varies (`*BigInteger`, `int64-mod-P`, `float64-log-domain`, bounded saturating); the inner loop's shape doesn't.

This means:
- 4A is the *first instance* of a pattern, not a one-off. Subsequent carrier-specialized sub-paths (4D modular, 4E log-float, 4F saturating, ...) will reuse the same control flow with different in-place arithmetic.
- The "non-trivial contribution" check is `d[u].Sign() != 0` for `'big-int`, but generalizes: for `'modular` it's `d[u] != 0 mod P`; for `'log-float` it's `d[u] != -∞`; for `'saturating` it's `d[u] != 0 && d[v] != cap`. The skip-condition shape is `d[u] = additive-identity OR (d[v] = absorbing-element)`.
- The Pattern 3A inner-loop sketch in Layer 3 is the canonical implementation of this shape. Later plans implementing carrier-specialized sub-paths can lift the shape directly and substitute carrier-specific arithmetic.

Practical consequence: SCC condensation has now shipped in `plans/2026-05-26-scc-condensation.md` (`algebra/graph/scc.go` + `CountPathsCyclic`). Cyclic counting workloads are converted to DAG-on-condensed-graph workloads, and Sub-path 4A becomes the inner loop that runs on the condensed DAG. The work in this plan is the *prerequisite* for the algebraic-and-structural fix to deliver its full value (allocation reduction) on real call graphs.

### Per-operation allocation cost

Wile's Bellman-Ford inner loop performs roughly E × V edge relaxations; on a 539-node, 623-edge graph with cycles that's ~335K relaxations per iteration × 538 iterations = ~180M relaxations. Each relaxation invokes `(*BigInteger).Add` and `(*BigInteger).Multiply`, and each of those allocates:

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

Back-of-envelope estimate (no `pprof` run on the 3-hour query yet — these numbers are derivations, not measurements): ~600M heap allocations during the run (multiplications also allocate `*big.Int` for the product and the `*BigInteger` wrapper). At ~100ns per allocation including GC amortization, that's ~60 seconds of pure allocation overhead. The remaining hours are dominated by the upper-layer failures (the algorithm not converging, the algebra not having a well-defined answer) plus bignum arithmetic on continually-growing values — and `math/big` already handles the latter optimally via Karatsuba/Toom-Cook, so that part is not optimizable from outside the standard library.

**Profile-first gate (Phase 0, see Implementation plan).** Existing micro-benchmarks `BenchmarkBigIntegerAdd` and `BenchmarkBigIntegerMultiply` in `values/integer_bench_test.go` give per-op cost in isolation. Phase 0 (1) re-runs those benches under `-benchmem` to derive an allocation-cost baseline, and (2) runs a heap-alloc `pprof` on a tractable counting-semiring workload (a small DAG path-count benchmark that completes in seconds, not the 3-hour cyclic case) to confirm allocation dominates per-operation cost. If `math/big` arithmetic dominates instead, this plan attacks the wrong cost factor — it would still help allocation-bound workloads, but the 3-hour incident would be the wrong reference point and Phase 4 should not ship.

### The candidate fix

If profiling confirms allocation dominance: provide an in-place arithmetic API for known-target patterns, plus a pool for transient `*big.Int` scratch values. Library-internal — the public `Number` interface stays immutable per R7RS semantics. The optimization is orthogonal to all four upper-layer plans: it speeds up the carrier-set operations regardless of which semiring, algorithm, convergence strategy, or correctness gate eventually uses them.

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
// dest may alias p or v (verified empirically in Phase 1 tests).
func addBigIntInPlace(dest, p, v *BigInteger) *BigInteger {
    dest.value.Add(p.value, v.value)
    return dest
}

func subBigIntInPlace(dest, p, v *BigInteger) *BigInteger {
    dest.value.Sub(p.value, v.value)
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

Critically: `(z *big.Int).Add(x, y)` is documented as "Add sets z to the sum x+y and returns z" (`go doc math/big.Int.Add`). The stdlib doc does **not** explicitly promise aliasing safety, but math/big implementations have historically handled receiver-aliasing-input correctly. Reuse of `z`'s `[]Word` storage is empirical behavior, not a written contract. Both properties (aliasing and storage reuse) are pinned by Phase-1 tests so a future stdlib change cannot regress us silently. When the result exceeds current capacity, `math/big` reallocates, but only by the amount needed — much cheaper than allocating fresh storage every call.

**Pre-allocation via `Bits()`/`SetBits()`.** A freshly-constructed `*big.Int` has zero `[]Word` capacity, so the *first* arithmetic op into it allocates from scratch — identical to allocating a fresh result. Reuse only kicks in on subsequent ops. To skip the growth phase entirely when the working width is known, use `(*big.Int).SetBits(make([]big.Word, 0, N))` to pre-allocate the backing slice with capacity N. `SetBits` preserves the slice's `cap` even though it normalizes `len` (trims trailing zeros). This is the only `math/big` API that exposes the backing storage; everything else treats it as opaque internal state.

For the scratch pool: when callers know the expected operand width (e.g., from a "expected counts up to 2^K" hint on the semiring), the pool's `Get()` should pre-size its returned scratch values via `SetBits`. For the destination slots in the distance map, the same applies — pre-size at map construction. Pool-returned values that have already cycled through arithmetic will already have grown capacity; only the *first* allocation per slot benefits from explicit `SetBits` pre-sizing.

**Hint-parameter source and unit.** The pool's `New` callback and the per-query slot pre-sizer both consume an `int` "words" hint with unit `big.Word` count, matching `(*big.Int).SetBits`. The canonical source for this value is the semiring's `(hint-words . N)` opt (see Phase 3). The Phase-4 fast path reads `(semiring-hint-words S)` once at construction and threads the result down to both the pool's `New` factory and the per-query slot allocator. Default 0 means "no hint; first op allocates from 0-cap nat". On 64-bit platforms one word is 8 bytes; on 32-bit, 4 bytes — callers thinking in bits convert as `hint-words = ceil(expected-bits / 64)` for the common case.

### Layer 2 — scratch pool

Default to `sync.Pool` (aligns with Q-3: Go GC handles eviction; aligns with Q-1: no atomic-contention surface to engineer ourselves). The pool stores **transient** `*BigInteger` values only — values consumed and released within a single fast-path iteration. Per-query distance-map slots are *not* pool-backed (see Layer 4 below; this avoids the escape and immutability issues that arise when pool values flow back to Scheme).

```go
// numericScratchPool returns *BigInteger scratch values for tight loops.
// Caller MUST return values via Put when done. Get does NOT zero the
// returned value — the caller's first op must fully overwrite it
// (e.g., mulBigIntInPlace(scratch, a, b) or scratch.value.Set(src)),
// not an accumulator-style read-then-write.
type numericScratchPool struct {
    pool       sync.Pool
    hintWords  int
}

func newNumericScratchPool(hintWords int) *numericScratchPool {
    p := &numericScratchPool{hintWords: hintWords}
    p.pool.New = func() any {
        b := new(big.Int)
        if hintWords > 0 {
            b.SetBits(make([]big.Word, 0, hintWords))
        }
        return &BigInteger{value: b}
    }
    return p
}

func (p *numericScratchPool) Get() *BigInteger {
    return p.pool.Get().(*BigInteger)
}

func (p *numericScratchPool) Put(b *BigInteger) {
    if b == nil {
        return
    }
    p.pool.Put(b)
}
```

Pool storage is small (16-64 `*BigInteger` slots typically suffice; the high-water mark is the depth of the deepest compound expression in the inner loop). `sync.Pool.New` pre-sizes new entries via `SetBits` so the *first* op into a fresh scratch doesn't allocate; pool-recycled values already have grown capacity from prior arithmetic.

**Q-1 closed:** use `sync.Pool` wrapped in a per-consumer struct (`numericScratchPool`). One pool per `*BigIntFastPath` attached to a `<graph-analysis>` — the hint baked into `New` is consumer-specific, so a global pool would conflate hint values from different graph-analyses. `sync.Pool`'s internal per-P stash gives the right contention story for concurrent queries on the same graph-analysis. Per-`MachineContext` storage avoided so this plan doesn't perturb the `machine/` structural-reduction work.

### Layer 3 — caller patterns

There are two inner-loop patterns, distinguished by whether the workload is unit-weight counting (no multiplication) or weighted (multiplication on every relaxation). The counting case exploits the monotonicity of the counting semiring to eliminate scratches entirely; the weighted case needs scratches to preserve the convergence-detection comparison under in-place mutation.

#### Pattern 3A — Counting (monotone, scratch-free)

Used for `bigint-counting-semiring` proper: unit weights, no `wfn`, distance values are monotonically non-decreasing path counts. The convergence check *"did `d[v] + d[u]` differ from `d[v]`?"* is algebraically equivalent to *"is `d[u]` non-zero?"* — both addends are non-negative, addition is strict-positive iff `d[u] > 0`. So we test the *source* before mutating, never compute the sum into a scratch:

```go
// Inside a Bellman-Ford inner loop on bigint-counting-semiring.
// d[v] is a per-query distance-map slot, allocated once per query.
// NO scratch pool, NO multiplication.

for _, e := range edges {
    if d[e.u].value.Sign() == 0 {
        continue                                       // source contributes nothing
    }
    d[e.v].value.Add(d[e.v].value, d[e.u].value)       // d[v] := d[v] + d[u], in-place
    enqueue(e.v)
}
```

Per-edge cost: one `Sign()` (cheap, examines the leading word's high bit) and one in-place `Add` (no allocation in steady state). Per-query state: V `*BigInteger` slots and nothing else.

#### Pattern 3B — Weighted (general, two scratches)

Used for user-constructed bigint-carrier semirings with non-trivial `wfn`. The weight is pre-resolved into a Go-side table (Q-6 closed); each relaxation does `d[v] := d[v] + (d[u] * weight)`. We can't use the source-sign trick because `weight` may be zero in some semirings, and the result `d[v] + d[u]*weight` doesn't have the simple "did the source contribute?" structure. The compare-then-commit pattern preserves convergence detection:

```go
// Inside a Bellman-Ford inner loop on a user-constructed weighted bigint
// semiring. prod and cand are borrowed from numericScratchPool.

prod := scratchPool.Get()
cand := scratchPool.Get()
defer scratchPool.Put(prod)
defer scratchPool.Put(cand)

for _, e := range edges {
    mulBigIntInPlace(prod, d[e.u], weights[e.idx])  // prod = d[u] * w
    addBigIntInPlace(cand, d[e.v], prod)            // cand = d[v] + prod
    if cand.value.Cmp(d[e.v].value) == 0 {
        continue                                    // no change — skip enqueue
    }
    d[e.v].value.Set(cand.value)                    // commit (in-place into slot)
    enqueue(e.v)
}
```

Per-edge cost: one `Mul`, one `Add`, one `Cmp`, one `Set`. Per-query state: V `*BigInteger` slots + 2 pool-borrowed scratches.

#### Allocation accounting

The result is that an inner loop doing E relaxations allocates O(V) bignums total (one per `d[v]` slot, pre-allocated once per query), not O(E × V) bignums (one per arithmetic op). The two scratches in Pattern 3B are amortized across the entire loop; Pattern 3A has none.

For the 3-hour query: ~600M allocations → ~540 allocations (V = 539). Up to ~1,000,000× reduction in the steady-state case where bignum size fits the slot's `[]Word` capacity; smaller in growing-bignum regimes where `math/big` reallocates as values exceed current capacity. In pathological cases (path counts growing superpolynomially on cyclic graphs), every operation may still trigger a reallocation and the gain approaches 1×.

**Self-loops** (u == v) under Pattern 3A: `d[v].Add(d[v], d[u])` with `e.u == e.v` is `d[v].Add(d[v], d[v])` — an aliased add. `math/big` handles this (Phase 1 tests pin the behavior). Under Pattern 3B the `cand.Cmp(d[e.v])` happens *before* `Set`, so even when `prod` was computed from `d[e.v]` the unchanged-detection works on a stable `d[e.v]`. Phase 1 tests cover both forms.

### Layer 4 — Bellman-Ford integration (in `(wile algebra graph)`)

**Terminology.**

- **`wfn`** is the conventional shortened name (used in `graph.scm` and throughout this plan) for the **weight function** — a Scheme procedure with informal signature `edge-data → semiring-carrier-value`. It originates as the third positional argument to `make-graph-analysis` (where the public API names it `weight-fn`). If the caller passes `#f`, the library substitutes `(lambda (_) (semiring-one S))`. The resulting procedure is stored in the `<graph-analysis>` record's `weight-fn` field and retrieved at query time via `ga-weight-fn`. The shortened name `wfn` appears at two binding sites in the library (`make-graph-analysis` at `graph.scm:32` and `compute-single-source` at `graph.scm:42`); both refer to the same procedure. It is not a primitive, not a Wile-specific concept, and not an extension point — just a procedure stored in a record field.

- A **weight** is the semiring carrier value associated with a single edge — concretely, the result of `(wfn edge-data)` for that edge's data payload from the adjacency list. It is the operand `w` in the per-relaxation expression `(semiring-times S node-dist w)` at `graph.scm:61`. Distinct from *edge-data* (raw user payload), `wfn` itself (the procedure that produces weights), and *edge* (the `(neighbor . edge-data)` pair). When `wfn` is the defaulted constant function, every edge gets weight `1` (the multiplicative identity). The carrier type of a weight is determined by the semiring; for `bigint-counting-semiring` every weight is a `*BigInteger`.

The graph library would need to know to use the in-place API. This couples the algebra layer to the values layer in a way that's slightly invasive but bounded. Two implementation options:

**Option A: expose the in-place API to Scheme.** Add `big-integer-add!` / `big-integer-mul!` primitives that take a destination and two operands. The graph library's Bellman-Ford inner loop uses these directly when it detects the carrier is BigInteger.

**Option B: keep in-place internal to values/, add a fast-path detector in the algebra layer.** The graph library's `make-graph-analysis` could pre-allocate distance maps as BigInteger slots when the semiring is counting, and the Bellman-Ford inner loop could use a Go-side fast path that calls the in-place helpers directly without Scheme-level dispatch.

**Default:** Option B. Keeping in-place mutation out of Scheme-visible territory respects R7RS immutability semantics; the fast path is an implementation detail of the algebra library.

**Current `(wile algebra graph)` state (verified at `stdlib/lib/wile/algebra/graph.scm:39-73`):** `compute-single-source` already implements worklist Bellman-Ford with per-edge convergence detection (`(equal? merged old-val)` at line 65, neighbor re-enqueue only on change at line 71-72). The Layer-4 fast path inherits this control structure; the change is that the inner-loop arithmetic moves to Go and uses in-place ops on per-query distance-map slots. The sibling plan `2026-05-24-graph-worklist-bellman-ford.md` describes a starting state that does not match the current code; this plan's Layer-4 work supersedes that sibling plan's primary deliverable for the BigInteger-carrier case.

**Distance-map slot ownership:** Per query, allocate `V` fresh `*BigInteger` slots (one for each reachable node). These slots are **not** pool-backed — they're per-query owned storage, live for the duration of the query, returned (via copy-on-return) to Scheme as the result alist. The scratch pool only holds the transient `prod` and `cand` values (Layer 3). This makes the lifetime story trivial:
- Slots: created at query start, freed by Go GC when the alist becomes unreachable.
- Scratches: Get/Put scoped to the inner loop; sync.Pool drains under GC pressure.
- No pool-backed value ever escapes to Scheme — Q-2 (R7RS immutability) is preserved structurally.

**Copy-on-return:** Before the fast path returns the distance alist to Scheme, each slot's value is copied to a fresh `*BigInteger` (`new(big.Int).Set(slot.value)`) so subsequent queries that reuse the slot allocator can't mutate previously-returned results. This is a one-time O(V) copy per query, far below the per-edge cost being eliminated.

**Carrier-transition policy (closes G-9):** When the semiring carrier is declared `'big-int`, the fast path **pre-promotes** all distance-map slots to `*BigInteger` at query start (initial value 0 = zero-length nat, no `[]Word` allocation). This loses the fixnum fast-path for small instances but keeps the in-place arithmetic homogeneous. The fast path is opt-in by carrier; semirings whose values stay in fixnum range should declare a fixnum carrier (Phase 3) and get their own specialized loop in a later plan.

### Hypothetical usage (Scheme surface, post-implementation)

Concrete examples showing how the three Phase-4 sub-paths and the escape hatch are reached from Scheme. All examples assume Phase 4 has shipped. Values like `#z5` follow Wile's `#z` prefix for `*BigInteger` literals.

**Example 1 — Unit-weight counting on a DAG (sub-path 4A).** No `wfn` supplied; library substitutes the unit-weight default; fast path uses Pattern 3A (scratch-free). **`call-graph` here is acyclic** — every edge points to a node with no path back to its source. This is the case `bigint-counting-semiring` is designed for.

```scheme
;; An acyclic call graph (DAG): count paths from "main" to every reachable function.
(define call-graph
  '(("main"  . (("init") ("run")))
    ("init"  . (("setup")))
    ("run"   . (("setup") ("loop")))
    ("setup" . ())
    ("loop"  . (("setup")))))    ; no edge back to main, init, or run

(define ga (make-graph-analysis (bigint-counting-semiring)
                                call-graph
                                #f))                       ; #f = unit weights

(graph-analysis-fast-path? ga)                              ; => #t
(graph-analysis-fast-path-kind ga)                          ; => 'unit-weight-counting

(graph-query-all ga "main")
;; => (("main"  . #z1)
;;     ("init"  . #z1)
;;     ("run"   . #z1)
;;     ("setup" . #z3)        ; reached via main→init→setup, main→run→setup, main→run→loop→setup
;;     ("loop"  . #z1))
```

**Cyclic graphs are not handled by this fast path.** Real-world call graphs typically contain recursion (mutual or direct), which creates cycles. `bigint-counting-semiring` on a cyclic graph does not terminate — the counting semiring `(ℕ, +, ×, 0, 1)` diverges on cycles (no finite path count exists). For cyclic call graphs, use one of:

- `(approximate-counting-semiring CAP)` — saturating carrier (bounded counts; the future approximate-counting plan ships this);
- SCC condensation pre-pass — collapses each SCC into a super-node, runs `bigint-counting-semiring` on the resulting condensed DAG. Shipped in `plans/2026-05-26-scc-condensation.md` (`algebra/graph/scc.go` + `CountPathsCyclic`); the bignum fast path will plug in here once Phase 4 lands.
- a k-closedness gate — rejects the query at construction time if the graph isn't k-closed (a future plan, not yet written).

This plan ships the inner-loop kernel that all three options will use on the DAG portion of their work.

**Example 2 — Weighted bigint counting (sub-path 4B).** User-constructed bigint-carrier semiring with edge multiplicities; fast path uses Pattern 3B (pool + multiply):

```scheme
;; Count paths weighted by edge multiplicities. Each edge-data is an integer
;; multiplicity; wfn promotes it to *BigInteger.
(define weighted-bigint-counting
  (make-semiring + *
                 (integer->big-integer 0)
                 (integer->big-integer 1)
                 '(carrier . big-int)
                 '(hint-words . 4)))                        ; advisory storage hint

(define multigraph
  '(("A" . (("B" . 3) ("C" . 2)))                          ; 3 parallel A→B, 2 parallel A→C
    ("B" . (("C" . 1)))
    ("C" . ())))

(define ga (make-graph-analysis weighted-bigint-counting
                                multigraph
                                (lambda (mult) (integer->big-integer mult))))

(graph-analysis-fast-path? ga)                              ; => #t
(graph-analysis-fast-path-kind ga)                          ; => 'weighted-bigint

(graph-query ga "A" "C")
;; => #z5     ; 2 direct (A→C with mult 2) + 3*1 via B (A→B mult 3, B→C mult 1) = 5
```

**Example 3 — Escape hatch for stateful `wfn` (fall-through).** User has a `wfn` that closes over mutable state; the snapshot semantics of pre-resolution would be wrong. They opt out of the fast path explicitly:

```scheme
;; A wfn that records every edge it sees (instrumentation).
(define visited-edges '())
(define (instrumented-wfn edge-data)
  (set! visited-edges (cons edge-data visited-edges))
  (integer->big-integer 1))                                  ; unit weight, but stateful

(define ga (make-graph-analysis (make-semiring + *
                                                (integer->big-integer 0)
                                                (integer->big-integer 1)
                                                '(carrier . big-int))
                                some-graph
                                instrumented-wfn
                                '(weight-mode . dynamic)))   ; opt out; preserve per-edge calls

(graph-analysis-fast-path? ga)                              ; => #f
;; Each relaxation will call instrumented-wfn, and visited-edges will
;; grow as the algorithm runs — exactly the semantics the user wants
;; for instrumentation.
```

**Example 4 — Default `counting-semiring` (no fast path).** Existing behavior, unchanged. Result values are `*Integer` (with overflow-promotion to `*BigInteger` if counts exceed int64). Fast path not eligible because no `(carrier . big-int)` is declared:

```scheme
(define ga (make-graph-analysis (counting-semiring)         ; no carrier slot
                                call-graph
                                #f))

(graph-analysis-fast-path? ga)                              ; => #f

(graph-query-all ga "main")
;; => (("main"  . 1)              ; *Integer, not *BigInteger
;;     ("init"  . 1)
;;     ("run"   . 1)
;;     ("setup" . 3)
;;     ("loop"  . 1))
;; Same numeric results as Example 1, different carrier types.
```

**Example 5 — Rejection on type-incompatible `wfn` (sub-path 4B construction check).** `wfn` returns a non-coercible value; construction errors with a clear pointer to the available remedies:

```scheme
(define ga (make-graph-analysis weighted-bigint-counting
                                multigraph
                                (lambda (_) "not a number")))
;; => Error: weight-fn returned "not a number"; bigint-carrier semirings
;;    require wfn to return an integer-valued *Number. Remedies:
;;     - omit wfn (or pass #f) for unit-weight counting;
;;     - return *BigInteger or *Integer from wfn;
;;     - pass '(weight-mode . dynamic) to keep dynamic dispatch.
```

## Resolved design questions

All design questions resolved; plan is implementation-ready pending the Phase 0 profile gate.


- **Q-1 — Pool granularity.** **Closed: global `sync.Pool`.** Decision drivers: (1) Q-3 already chose sync.Pool semantics, (2) sync.Pool's per-P internal storage already handles SRFI-18 thread contention without explicit engineering, (3) per-`MachineContext` storage would perturb the `machine/` structural-reduction work. If contention shows up under benchmarking, escalate to per-Engine; this is a runtime-only swap (the call sites use a constructor function).

- **Q-2 — In-place API exposure.** **Closed: internal only.** R7RS makes Numbers immutable; mutation primitives would be a Wile-specific deviation. Specific implementations that need mutable-integer semantics consume the in-place API from Go, not from Scheme. No `!`-suffixed bignum primitives, no `*MutableBigInt` value type, no Scheme-visible mutation surface — these are out of scope (see below). The in-place helpers exist solely to be called from Go-side code paths (the algebra-layer fast path in Phase 4, and any future Go consumers).

- **Q-3 — Scratch lifecycle.** **Closed: sync.Pool semantics.** Go's GC handles eviction automatically; simpler than a hand-managed shrinking strategy.

- **Q-4 — Fast-path detection in algebra layer.** **Closed: carrier symbol on `<semiring>` record via opts-alist.** The algebra library convention (`stdlib/lib/wile/algebra/CLAUDE.md` "Options-alist discipline") extends structure records via a trailing `. opts` alist; this avoids breaking existing constructors. Add an optional `(carrier . SYM)` slot. Carrier symbols **reuse the existing `NumericTypeSpec.schemeName` vocabulary** (`values/numeric_registry.go`) where applicable — `'integer`, `'rational`, `'real`, `'complex` — extended with `'boolean`, `'log-float`, `'modular`, `'saturating`, `'opaque` for non-numeric or non-tower carriers. The algebra layer dispatches on the symbol; the values layer provides the in-place helpers. Orthogonal to the approximate-counting plan's `approximate-semiring?` slot.

- **Q-5 — Group allocation for cache locality.** Workload sensitivity: an arena strategy fits workloads where bignums stay below ~4 words; for the counting-on-cycles motivating workload, path counts grow past that within a few iterations and the arena's locality benefit is consumed while pinning cost remains. **Resolution:** defer until Phase 1 + Phase 2 land with naive `new(big.Int)`. Measure cache-miss rate via `perf stat` or pprof block/CPU profiles, then decide per-workload. Plan does not commit either strategy as Phase-N work; revisit if profiling shows locality matters.

- **Q-6 — Phase-4 weight-function dispatch.** **Closed: pre-resolve at construction.** A *weight* is the semiring carrier value `(wfn edge-data)` for a single edge — see Layer 4 terminology block. `make-graph-analysis` walks the adjacency list once, applies `wfn` to each edge's data, coerces the result to `*BigInteger`, and stores the values into a Go-side weight table indexed by edge position (`[]*BigInteger`). The fast-path inner loop reads `weights[edge.idx]` directly; `wfn` is never called during the loop. This preserves the dispatch-bypass goal and makes the continuation-safety story trivial (Scheme code only runs at construction time, before any pool borrowing).

  Constraints introduced: `wfn` is called exactly once per edge, deterministically, at construction time. The fast path assumes the weight associated with an edge is fixed for the lifetime of the `<graph-analysis>` record. Weight functions that depend on iteration state or mutable closure variables are not supported by the fast path; semirings whose use cases require such behavior should not declare `'big-int` carrier and will fall through to the existing Scheme inner loop. This is acceptable for the motivating counting workload (every edge has a fixed `*BigInteger` weight derived from its edge-data) and documented in Phase 4 / Phase 6.

- **Q-7 — Semiring carrier declaration for `counting-semiring`.** **Closed: new `bigint-counting-semiring`.** `(counting-semiring)` is left untouched — it declares carrier `'integer` (or omits the carrier slot; treated as no fast-path), keeps R7RS-compatible fixnum-with-overflow-promotion behavior, and runs through the existing Scheme dispatch. A new built-in `(bigint-counting-semiring)` is added that declares carrier `'big-int` and operates strictly on `*BigInteger` from the start (zero is `(integer->big-integer 0)`, one is `(integer->big-integer 1)`). Callers opt into the fast path by choosing the variant. Cleanest separation; users that need fixnum-counting performance get it from the default, users that know they're in big-int territory get the allocation-reduced path.

- **Q-8 — Mutation visibility across queries.** **Closed: fresh slots per query (Phase 4); slot-array reuse is a deferred follow-up.** Phase 4 allocates V fresh `*BigInteger` slots on every `graph-query-all` invocation. Predictable, no cross-query state, copy-on-return is straightforward. If Phase 5 benches show slot construction dominates remaining cost, open a follow-up plan for per-source slot-array caching.

## Implementation plan

### Phase 0 — profile-first baseline

- Run `go test -bench=BenchmarkBigInteger -benchmem ./values/` and record per-op allocs/B for the existing `BenchmarkBigIntegerAdd` and `BenchmarkBigIntegerMultiply` baselines.
- Build a tractable counting-semiring micro-workload (e.g., DAG path-count on a 50-node acyclic graph — terminates in seconds, well-defined, no semantic pathology). Wrap with `pprof.WriteHeapProfile`; confirm `(*BigInteger).Add` / `(*BigInteger).Multiply` plus `newBigIntFromOp` show up as the dominant allocation sites.
- Decision gate: if `math/big` arithmetic (not allocation) dominates the workload's CPU profile, stop. The plan attacks the wrong cost factor and Phase 4 should not ship. Otherwise proceed.

### Phase 1 — internal in-place helpers + tests

- Add `values/numeric_scratch.go` with `addBigIntInPlace`, `subBigIntInPlace`, `mulBigIntInPlace`, `negateBigIntInPlace`. Unexported.
- Add `values/big_integer_scratch_test.go`:
  - Correctness: in-place result matches allocating result for sample inputs (small, large, negative, zero).
  - Aliasing: `dest = dest + dest`, `dest = dest * dest`, `dest = -dest`, and `dest = dest + scratch` where `scratch` was just computed from `dest` (self-loop pattern from Layer 3) all produce correct results.
  - Storage reuse: verify (via `runtime.MemStats` or `(*big.Int).Bits()` capacity inspection) that repeated in-place ops on the same `*BigInteger` don't reallocate when value grows slowly within capacity.
  - Pin the aliasing behavior empirically so a future `math/big` change can't regress us silently (the stdlib doc doesn't promise aliasing safety in writing).

### Phase 2 — scratch pool (weighted sub-path only)

**Scope:** the scratch pool exists solely for Pattern 3B (weighted-bigint Bellman-Ford). The unit-weight counting fast path (Pattern 3A, used by `bigint-counting-semiring`) is scratch-free and does not consume the pool. If Phase 5 measurements show user-constructed weighted bigint semirings are rare in practice, Phase 2 could be deferred to a follow-up plan without affecting the unit-weight counting motivating workload.

- Add `values/numeric_scratch_pool.go` with `numericScratchPool` struct wrapping `sync.Pool` (Q-1, Q-3 closed). Constructed via `newNumericScratchPool(hintWords)` — the hint is baked into the pool's `New` callback so that fresh entries arrive with pre-sized `[]Word` backing. Pool lifetime is tied to its owning consumer (one pool per `*BigIntFastPath` payload on a `<graph-analysis>` whose semiring is bigint-carrier with non-trivial `wfn`); when that consumer is GC'd, the pool goes with it.
- Pool is **not** a global singleton because different consumers may declare different `hint-words` values, and `sync.Pool.New` can only be set once at construction. Per-consumer pools keep the hint exact and avoid cross-consumer contamination of pool entries. Concurrent queries against the same graph-analysis share the pool naturally via `sync.Pool`'s per-P stash semantics.
- Add `values/numeric_scratch_pool_test.go`:
  - Get/Put correctness (round-trip identity, hint pre-sizing).
  - No-leak invariants under panic (`defer Put` survives a panic in the borrower).
  - Hint-pre-sized scratch's first op does not reallocate `[]Word` (verify via `Bits()` `cap`).
  - Two pools with different hints produce correctly-sized fresh entries (no cross-contamination).

### Phase 3 — semiring carrier slot + storage hint + graph-analysis opt

- Extend `<semiring>` via opts-alist (per `stdlib/lib/wile/algebra/CLAUDE.md` "Options-alist discipline"): `make-semiring` accepts two new trailing options. `validate-opts-keys` rejects typos. No breaking change to the positional signature.
  - **`(carrier . SYM)`** — type-changing: declares the carrier type, drives fast-path eligibility. Default `#f` (no fast path). See "Carrier vocabulary" below.
  - **`(hint-words . N)`** — advisory, type-preserving: expected `big.Word` count for typical operand values. Drives pool pre-sizing (`numericScratchPool.New`) and per-query slot pre-sizing via `(*big.Int).SetBits(make([]big.Word, 0, N))`. Type-preserving because it only changes the `[]Word` backing capacity, not the value type itself. Default `0` (no pre-sizing; first op allocates from 0-cap nat).
- Also extend `make-graph-analysis` with a trailing opts-alist (currently a fixed-arity 3-positional procedure; this conversion is non-breaking — existing callers passing exactly three args continue to work). The only opt defined by this plan is:
  - **`(weight-mode . SYM)`** — controls whether the fast path is eligible to attach. `'static` (default; pre-resolved weights, snapshot semantics, fast-path eligible). `'dynamic` (suppresses fast-path; existing Scheme inner loop runs even on big-int carriers, dynamic-weight semantics preserved). Mitigation #3 from the Mitigations section.
- Accessors: `(semiring-carrier s)`, `(semiring-hint-words s)`, `(graph-analysis-weight-mode ga)`. Each returns the relevant symbol or `#f` if not declared. Naming aligns with `semiring-zero` / `semiring-one` / `ga-semiring`.
- **Unit of `hint-words` is `big.Word` count**, matching `(*big.Int).SetBits` directly. Platform-dependent (8 bytes on 64-bit, 4 bytes on 32-bit) — documented in the Phase 6 docstring update. Callers thinking in bits convert as `hint-words = ceil(expected-bits / 64)` for the common 64-bit case; over-estimating costs at most a few words of unused backing.
- Carrier vocabulary reuses `NumericTypeSpec.schemeName` (`values/numeric_registry.go`) where applicable: `'integer`, `'rational`, `'real`, `'complex`. Extended with `'boolean`, `'log-float`, `'modular`, `'saturating`, `'opaque`.
- Built-in semiring declarations (Q-7 closed; both counting variants accept `hint-words`):
  - `(counting-semiring . opts)` — accepts `(hint-words . N)`; carrier omitted (Integer with overflow promotion); not fast-path eligible. The hint is accepted-but-unused under Phase 4; reserved for a future fixnum-fast-path that may consume it for pre-sizing pool entries when promotion occurs.
  - `(bigint-counting-semiring . opts)` — **new**; accepts `(hint-words . N)`; carrier `'big-int`; zero and one are `*BigInteger` (specifically `(integer->big-integer 0)` and `(integer->big-integer 1)`); eligible for fast-path sub-path 4A (unit-weight counting, scratch-free). When this semiring is passed to `make-graph-analysis`, the `weight-fn` argument **must be `#f` or omitted** — the unit-weight inner loop has no place to consume a weight function. A non-`#f` `weight-fn` raises an error pointing the user to construct a `(make-semiring + * (integer->big-integer 0) (integer->big-integer 1) '(carrier . big-int))` semiring instead (which routes to sub-path 4B). The `hint-words` value flows to the Go fast path and pre-sizes the per-query distance-map slots. No pool consumed by this sub-path.
  - `boolean-semiring` → carrier `'boolean`; `hint-words` accepted but irrelevant (single bit; no bignum storage).
  - `tropical-semiring` → carrier `'real`; `hint-words` accepted but irrelevant (float64).
  - `modular-counting-semiring P` (from approximate-counting plan) → carrier `'modular`.
  - `log-counting-semiring` (from approximate-counting plan) → carrier `'log-float`.
  - `approximate-counting-semiring CAP` (from approximate-counting plan) → carrier `'saturating`.
- This change is shared with the approximate-counting plan; pick one for primary owner.

### Phase 4 — Bellman-Ford fast path in `(wile algebra graph)`

Three sub-paths are dispatched at `make-graph-analysis` time based on the semiring's carrier slot and whether `wfn` is omitted. The dispatch is sticky — once chosen at construction, it doesn't switch per query.

```
                 ┌───────────────────────────────┐
                 │  carrier slot on semiring?    │
                 └───────────────┬───────────────┘
                                 │
                 ┌───────────────┴───────────────┐
                 │                               │
       not 'big-int                       'big-int
       (or absent)                              │
                 │                ┌─────────────┴─────────────┐
                 │                │  weight-mode opt is       │
                 │                │  'dynamic?                │
                 │                └─────────────┬─────────────┘
                 │                              │
                 │                  ┌───────────┴───────────┐
                 │                  │                       │
                 │                yes                       no
                 │                  │            ┌──────────┴──────────┐
                 │                  │            │  wfn omitted        │
                 │                  │            │  (or #f)?           │
                 │                  │            └──────────┬──────────┘
                 │                  │                       │
                 │                  │                ┌──────┴──────┐
                 │                  │                │             │
                 │                  │              yes            no
                 │                  │                │             │
                 ▼                  ▼                ▼             ▼
        Existing Scheme    Existing Scheme    Sub-path 4A   Sub-path 4B
        slow path          slow path          (unit-weight  (weighted
        (dynamic           (escape hatch)     counting)     bigint)
        dispatch)
```

#### Sub-path 4A — Unit-weight bigint counting (`bigint-counting-semiring` proper)

- **Eligibility:** `(eq? (semiring-carrier S) 'big-int)`, `weight-mode` ≠ `'dynamic`, AND `wfn` is `#f` or omitted at `make-graph-analysis`.
- **Construction:** allocate the `*BigIntFastPath` payload but **do not** call `wfn` (there isn't one). No weight table. No scratch pool.
- **Per-query:** allocate V fresh `*BigInteger` slots (per-query owned storage). If `(semiring-hint-words S)` is set, pre-size each slot's `[]Word` backing via `SetBits(make([]big.Word, 0, hint))`. Initialize `d[source] = (integer->big-integer 1)`, others = `(integer->big-integer 0)`. Run the worklist loop with Pattern 3A (Sign-check + in-place Add). On exit, copy-on-return each slot to a fresh `*BigInteger` for the alist.
- **State per query:** V `*BigInteger`s + worklist + visited set. No scratches. No pool.

#### Sub-path 4B — Weighted bigint (user-constructed semiring with non-trivial `wfn`)

- **Eligibility:** `(eq? (semiring-carrier S) 'big-int)`, `weight-mode` ≠ `'dynamic`, AND `wfn` is non-`#f` at `make-graph-analysis`.
- **Construction:** pre-resolve weights (Q-6) — walk the adjacency list, call `wfn` per edge, coerce each result to `*BigInteger` (Integer auto-promotes; non-numeric or non-coercible types error at construction with a clear message pointing the user to either omit `wfn` for unit-weight counting, switch carriers, or use `(weight-mode . dynamic)`). Store into a Go-side `[]*BigInteger` weight table indexed by edge position. Construct a `numericScratchPool` with the semiring's `hint-words`.
- **Per-query:** allocate V fresh `*BigInteger` slots (per-query owned storage), borrow two scratches (`prod`, `cand`) from the pool, run the worklist loop with Pattern 3B (Mul + Add + Cmp + Set). On exit, copy-on-return.
- **State per query:** V `*BigInteger`s + 2 pool-borrowed scratches + worklist + visited set.

#### Sub-path 4C — Fall-through

- **Eligibility:** any case not matching 4A or 4B (no carrier slot, carrier ≠ `'big-int`, OR `weight-mode = 'dynamic`).
- **Behavior:** existing `compute-single-source` Scheme inner loop. No Go fast path attached. `wfn` is called per edge per relaxation, as today.

#### Shared infrastructure

- All three sub-paths inherit the existing worklist control structure from `graph.scm:39-73` (per-edge convergence detection, neighbor re-enqueue on change). The fast paths express the same control flow in Go; the fall-through stays in Scheme.
- `(graph-analysis-fast-path? ga)` returns `#t` iff sub-path 4A or 4B is attached, `#f` otherwise. Optionally, `(graph-analysis-fast-path-kind ga)` could return `'unit-weight-counting` / `'weighted-bigint` / `#f` for finer introspection.
- Other carriers (`'integer`, `'real`, `'log-float`, `'modular`, `'saturating`) get their own type-specialized fast paths in subsequent commits — out of scope for this plan, file separately.

### Phase 5 — benchmark + acceptance

This plan's acceptance is measured **entirely on DAG workloads**. The cyclic-counting case (the 3-hour query) cannot terminate without the algebraic, structural, or gate fixes from sibling plans, and is therefore not a meaningful gate on this plan's success. The bench list below reflects that.

- Existing baselines: `BenchmarkBigIntegerAdd`, `BenchmarkBigIntegerMultiply` in `values/integer_bench_test.go` (recorded in Phase 0). Add `BenchmarkBigIntAddInPlace` and `BenchmarkBigIntMulInPlace` alongside (microbench for Layer-1 helpers).
- Microbench acceptance: in-place variant ≥3× faster for small bignums (within initial 4-word capacity) and ≥10× for ~1KB-class bignums (where allocation overhead dominates). These are derived against the Phase-0 baseline, not invented — first measurement establishes the regression gate.
- Graph-level benches under `examples/benchmarks/` (the project's actual benchmark directory; `bench/` does not exist):
  - **`bench-bigint-counting-unit-weight.scm` (sub-path 4A) — primary acceptance signal.** Counting semiring on a 50-node DAG (acyclic, well-defined, terminates). Expected: per-edge allocation count drops to O(V) total (one per slot) plus zero scratches; total query time dominated by `(*big.Int).Add` cost on growing bignums. This bench validates the kernel that future Σ-semiring DAG variants will inherit.
  - **`bench-bigint-counting-weighted.scm` (sub-path 4B) — secondary signal.** User-constructed bigint-carrier semiring with non-trivial `wfn` (e.g., edge multiplicities). Same 50-node DAG. Expected: per-edge allocation count drops to O(V) + 2 pool transients; per-edge cost is one `Mul` + one `Add` + one `Cmp` + one `Set`, measurably higher than 4A's single `Add`. Side-by-side comparison quantifies the cost of supporting weights.
- **Cyclic and macro workloads are out of scope** for this plan's acceptance. The 3-hour `(graph-query-all ga-count run-name)` query cannot be made to terminate by reducing per-op cost; it requires algebraic, structural, or gate fixes from sibling plans. Subsequent plans (`2026-05-24-approximate-counting-semirings.md`, future SCC-condensation, future k-closedness check) own those acceptance criteria. This plan's contribution to those plans' eventual macrobench wins is the kernel itself, validated by the DAG bench above.

### Phase 6 — docs + PR

- Update `values/CLAUDE.md` to document the in-place API and pool conventions.
- Update `docs/numeric/tower.md` to note the allocation-reduction option for hot-loop callers.
- Update `stdlib/lib/wile/algebra/CLAUDE.md` to document the carrier-slot extension and reused `schemeName` vocabulary.
- Open PR, dual review.

## Risks

- **R-1 — R7RS immutability breach via in-place leakage.** If a pool-backed `*BigInteger` ever escapes to Scheme, Scheme programs could observe mutation. Mitigation: structural — the pool only holds transient scratches (Layer 3); per-query distance-map slots are not pool-backed (Layer 4); copy-on-return at the fast-path boundary ensures no slot pointer escapes to Scheme. The in-place API stays unexported in `values/`.
- **R-2 — Aliasing bugs.** `addBigIntInPlace(cand, d[v], prod)` followed by `d[v].value.Set(cand.value)` is the intended pattern. The stdlib doc does not promise aliasing safety in writing; math/big has historically handled it correctly. Mitigation: Phase 1 tests pin the aliasing behavior empirically (self-aliasing in receiver, in operand, and in both) so a future stdlib change is caught at CI time, not in production.
- **R-3 — Pool churn under GC pressure.** A naive pool could pin scratch bignums indefinitely, defeating GC. Mitigation: use `sync.Pool` which the Go runtime drains under GC pressure (Q-3 closed).
- **R-4 — Coupling between algebra and values layers.** The fast-path detector in `(wile algebra graph)` needs to know "this is a BigInteger carrier" to invoke the right in-place ops. Mitigation: the `semiring-carrier` accessor (Phase 3) abstracts this as a symbol drawn from the existing `NumericTypeSpec.schemeName` vocabulary — the algebra layer dispatches on symbol, the values layer provides the in-place helpers. Layered cleanly.
- **R-5 — Sibling-plan dependency drift.** The plan's sibling `2026-05-24-graph-worklist-bellman-ford.md` describes a starting state that does not match the current `graph.scm` (verified at `stdlib/lib/wile/algebra/graph.scm:39-73`). Mitigation: this plan's Layer-4 work inherits the *current* worklist structure directly; no dependency on the sibling plan landing first. Documented in Layer 4 above.
- **R-6 — Carrier mis-declaration penalizes fixnum workloads.** Resolved by Q-7: `counting-semiring` is left untouched (no fast-path), and the new `bigint-counting-semiring` is the opt-in entry point. Callers that don't need big-int counting pay nothing.
- **R-7 — Pre-resolved weights diverge from dynamic-weight semantics.** Q-6 closed the inner-loop dispatch by pre-resolving `wfn` at construction time. Semirings that need per-iteration dynamic weights cannot use the fast path. Mitigation: the fast-path eligibility check (`(eq? (semiring-carrier S) 'big-int)`) is the gate; semirings that omit the carrier slot or declare anything else fall through to the existing Scheme inner loop, preserving dynamic-weight semantics for them. Document the constraint in Phase 6's `algebra/CLAUDE.md` update.
- **R-8 — Silent semantic drift for impure `wfn` (weighted sub-path 4B only).** Pre-resolution applies a *purity contract* to `wfn` (pure deterministic function of `edge-data`) that is not statically checkable. A user who passes a stateful `wfn` to a user-constructed bigint-carrier semiring and triggers sub-path 4B will get arithmetically-correct results against the construction-time snapshot — but those results may differ from what the slow path's dynamic dispatch would produce. **No runtime signal alerts the user to this divergence.** Does not apply to sub-path 4A (`bigint-counting-semiring` rejects `wfn` at construction) or 4C (fall-through preserves existing dynamic semantics). Mitigations are operational (next section).

## Mitigations

The purity contract introduced by Q-6's pre-resolution (R-8) is the central operational risk for sub-path 4B. The following mitigations layer on top of the structural separation already in the plan (R-1 through R-7). They apply to the weighted-bigint sub-path; sub-path 4A has no `wfn` to be impure and needs none of these.

1. **Document the purity contract loudly (Phase 6).** Add an explicit docstring section on `make-graph-analysis`'s description of its `weight-fn` argument — specifically for the case where the supplied semiring has `(carrier . big-int)` and `weight-fn` is non-`#f` (sub-path 4B). Required wording (subject to docstring-format conventions):

   > When this semiring has `(carrier . big-int)` and `weight-fn` is supplied (not `#f`), the supplied `weight-fn` must be a *pure deterministic function* of `edge-data` alone. Closures over mutable state, observation-based weight functions, and weight functions that depend on iteration or query state will produce incorrect results because weights are pre-resolved at construction time and not re-evaluated during traversal. For weight functions that genuinely need stateful or visit-style semantics, pass `(weight-mode . dynamic)` to opt out of the fast path, or use the visit-style variant (see *Two-implementation design paths* in the bignum-allocation-reduction plan).

   No contract is needed on `bigint-counting-semiring` proper (sub-path 4A) because it doesn't accept a `weight-fn`; the unit-weight case is purity-by-construction. Phase 6 deliverable.

2. **`graph-analysis-fast-path?` predicate (Phase 4).** Already specified. Lets users programmatically verify whether the optimization engaged. If they suspect snapshot semantics is wrong for their workload, they can detect it without inferring from timing.

3. **Escape opt on `make-graph-analysis`: `(weight-mode . dynamic)`.** Accept a trailing opt on `make-graph-analysis` (extending it to support the algebra-CLAUDE-MD opts-alist convention). When `(weight-mode . dynamic)` is present, the fast path is suppressed even for big-int carriers and the existing Scheme inner loop runs. Lets users opt out of snapshot semantics without giving up the big-int carrier type. Trade-off: runtime-only switch — `wfn` continues to allocate per call, no allocation reduction. Acceptable for the correctness-over-performance use case. **Default** `(weight-mode . static)` (or absence; treated as static). Add to Phase 3's opts list and Phase 4's fast-path eligibility check.

4. **Lazy memoization (deferred).** Alternative to eager pre-resolution: call `wfn` the first time each edge is relaxed and cache the result. Defers construction-time cost (R-7 risk side B), surfaces errors at query time (closer to current semantics), but still invokes `wfn` exactly once per edge (snapshot semantics, lazy — same purity contract applies). Not in scope for this plan; revisit if Phase 0 / Phase 5 measurements show construction-time cost or error-timing is a real complaint.

Implementation placement:
- Docstring contract (1) — Phase 6
- `graph-analysis-fast-path?` (2) — Phase 4 (already in plan)
- `(weight-mode . dynamic)` opt (3) — Phase 3 (add to `make-graph-analysis` opts list) and Phase 4 (gate the fast-path attach on this opt)
- Lazy memoization (4) — out of scope; future plan if needed

## Acceptance criteria

- Phase 0 profile confirms allocation dominance on the DAG counting micro-workload; otherwise plan stops.
- `(*BigInteger).Add` / `.Multiply` (the public methods) unchanged in behavior — same return types, same R7RS-visible immutability.
- In-place internal API exists and passes correctness + aliasing tests (including self-loop pattern).
- Scratch pool exists and passes leak-under-panic tests.
- `semiring-carrier` accessor exposed on the semiring record; vocabulary reuses `NumericTypeSpec.schemeName` symbols where applicable.
- Bellman-Ford on a `'big-int`-carrier semiring uses the fast path (verifiable by allocation-counting in a bench). No pool-backed `*BigInteger` escapes to Scheme (verifiable by pointer-identity test against pool reuse).
- Microbench: in-place BigInteger.Add measured against existing `BenchmarkBigIntegerAdd` baseline (recorded in Phase 0). Targets: ≥3× faster for small bignums (within initial 4-word capacity); ≥10× for ~1KB bignums. Subsequent runs are regression-gated against this run.
- DAG-counting bench on a 50-node acyclic graph terminates in well under 1 second with the fast path enabled, and the per-edge allocation count drops to O(V) total (one per slot; zero scratches for sub-path 4A, two pool transients for sub-path 4B).
- **Cyclic-counting acceptance is explicitly out of scope.** The 3-hour incident cannot be resolved by reducing per-op cost; it requires upper-layer fixes from sibling plans. This plan's DAG bench is the only macro-level acceptance signal; cyclic-counting acceptance moves to whichever sibling plan owns the algebraic/structural/gate fix.
- `make lint && make covercheck && make ci` all green.

## Out of scope

- Karatsuba / Toom-Cook implementations — `math/big` already provides them.
- Fixnum fast-path on `Integer` — already exists (`addInt64` etc.).
- Specializing Bellman-Ford for non-BigInteger carriers (fixnum, float64, log-float, modular, saturating) — file separately as a "carrier-specialized inner loops" plan after this lands. Each carrier has its own fast-path opportunity; bundling them creates a huge PR with cross-cutting concerns.
- BigFloat / BigComplex / Rational allocation reduction — same pattern applies, same fix shape, but separate plan to keep this one focused on BigInteger which is what the 3-hour incident exposed.
- Worklist Bellman-Ford convergence detection — already present in `stdlib/lib/wile/algebra/graph.scm:39-73`. The sibling plan `2026-05-24-graph-worklist-bellman-ford.md` describes a starting state that doesn't match the current code; this plan inherits the existing worklist structure directly.
- Approximate counting semirings — sibling plan `2026-05-24-approximate-counting-semirings.md`.
- SCC condensation primitive — shipped in `plans/2026-05-26-scc-condensation.md` (`algebra/graph/scc.go` + `CountPathsCyclic`). Makes exact counting tractable on cyclic graphs via condensation; the 539-node machine-package incident runs in ~36 µs (vs. the 3-hour baseline).
- k-closedness check at `make-graph-analysis` time — separate plan, complements this work by preventing the pathological case at construction rather than at runtime.
- **Scheme-visible mutable-integer types or primitives.** No `*MutableBigInt` value type, no `make-mutable-bigint` / `mutable-bigint-add!` / `mutable-bigint-mul-into!` primitives, no `!`-suffixed bignum arithmetic exposed to Scheme. The in-place API exists for Go callers only. Implementations that need mutable-integer semantics implement that loop in Go (e.g., the algebra-layer fast path in Phase 4). This is a load-bearing decision: it preserves R7RS Number immutability for all Scheme code and confines mutation to library-internal Go boundaries.
- **Visit-style / stateful-`wfn` Bellman-Ford variant** — see *Two-implementation design paths* below. This plan ships only the static-weight variant (Path A); Path B is documented now as a future-work design surface so the static decision doesn't foreclose it.

## Two-implementation design paths

The Bellman-Ford-shaped graph-analysis family splits cleanly into two algorithms that share infrastructure but differ fundamentally in how they treat `wfn`. This plan ships Path A; Path B is documented as a deferred future plan so the API surface introduced here doesn't accidentally foreclose Path B's design space.

### Path A — Static-weight Bellman-Ford (this plan, Phase 4)

Internally split into two sub-paths (4A unit-weight, 4B weighted); both share the static-weight characterization from Path B's perspective:

- **`wfn` contract:** **4A** — `wfn` must be omitted (or `#f`); the unit-weight inner loop has no place for it. **4B** — `wfn` must be a pure deterministic function of `edge-data` alone (snapshot semantics).
- **Weight resolution:** **4A** — no weights; every edge contributes its source's count via in-place `Add`. **4B** — eager at construction time; `make-graph-analysis` walks the adjacency list once, applies `wfn` per edge, caches each weight in a Go-side `[]*BigInteger` table indexed by edge position.
- **Inner loop:** dispatch-free. **4A** uses Pattern 3A (Sign-check + in-place Add, scratch-free); **4B** uses Pattern 3B (Mul + Add + Cmp + Set, two pool-borrowed scratches).
- **Allocation-reduction-eligible:** yes — this is the target of this plan. 4A reduces per-edge ops further by skipping the multiplication entirely.
- **Semantics:** **4A** — pure counting (unit weights, monotone). **4B** — snapshot (the weight associated with an edge is fixed for the lifetime of the `<graph-analysis>` record).
- **Opt-in:** **4A** — `(bigint-counting-semiring)` (or any `(carrier . big-int)` semiring) AND `wfn` is `#f`/omitted AND `weight-mode` ≠ `'dynamic`. **4B** — user-constructed `(carrier . big-int)` semiring AND non-`#f` `wfn` AND `weight-mode` ≠ `'dynamic`.

### Path B — Visit-style Bellman-Ford (deferred plan)

- **`wfn` contract:** *visitor procedure* — called per edge per relaxation. May be stateful, may have observable side effects, may depend on iteration or query state, may produce different weights on different calls.
- **Weight resolution:** dynamic. Each relaxation calls `wfn`; no caching.
- **Inner loop:** crosses Go↔Scheme per edge. Allocation per call is unavoidable.
- **Allocation-reduction-eligible:** no. Per-call dispatch and per-call allocation defeat the optimization. Different optimization target entirely.
- **Semantics:** observation. The weight at relaxation time is whatever `wfn` returns at relaxation time, including any side effects.
- **Use case:** instrumented traversal (logging, profiling), adaptive algorithms (annealing schedules, iteration-dependent costs), online algorithms (oracle-driven weights), debugging hooks. Algorithms where the per-edge invocation of `wfn` is itself part of the computation, not an implementation detail.
- **Opt-in:** TBD by Path B's plan; three API shapes considered (below).

### API-shape options for Path B (decision deferred to Path B's plan)

**Option 1 — Semiring opt: `(bigint-counting-semiring '(weight-mode . stateful))`.** Reject. The semiring is an *algebraic structure*; whether the algorithm consuming it pre-resolves or dynamically dispatches its weights is not a property of the algebra. The semiring's responsibilities are the carrier set, the two operations, the identities, and the laws — nothing about traversal-time behavior of consumers. Carrying algorithm-traversal hints on the semiring conflates two genuinely separate concerns.

**Option 2 — `make-graph-analysis` opt: `(make-graph-analysis S adj wfn '(weight-mode . stateful))`.** Acceptable. Path A's `weight-mode` opt is exactly this mechanism (already added in Phase 3 of this plan, with value `'dynamic` suppressing the fast path). For Path B, the opt could grow additional values: `'static` (default, Path A eager), `'dynamic` (Path A's escape hatch — runs existing slow path with no caching), `'stateful` (Path B's full visitor semantics with its own inner loop). All three values would mean: same `<graph-analysis>` record shape, different runtime behavior. Concern: a behavioral switch hidden in an opt — every call site reader must know what values exist and what they do.

**Option 3 — Separate procedures: `make-graph-traversal` / `graph-traverse`.** Cleanest. Path B becomes a parallel API:

```scheme
;; Path A — what this plan ships
(make-graph-analysis S adj wfn)
(graph-query ga source target)
(graph-query-all ga source)

;; Path B — what a future plan would add
(make-graph-traversal S adj visitor)         ; constructs the dual structure
(graph-traverse-from gt source target)        ; visitor called per edge per relaxation
(graph-traverse-from-all gt source)
```

The semantic difference is visible at the call site. The visitor's signature can evolve independently of `wfn`'s. The two algorithms can live in separate files. The cost is API surface — two parallel constructors and two parallel query primitives.

**Recommended default for Path B's future plan:** Option 3, with a fallback to Option 2 if API surface is at a premium. Both are workable; the choice depends on whether the Path B implementer wants the two algorithms presented as siblings (Option 3) or as modes (Option 2).

### Open design questions for Path B's future plan

Not blocking this plan; listed so the future plan inherits a starting set rather than relitigating from scratch:

- **Visitor signature.** `(visitor edge-data) → weight` (preserves Path A's `wfn` shape), or `(visitor u v edge-data) → weight` (visitor-style with source/target context), or `(visitor u v edge-data iteration) → weight` (iteration-aware), or `(visitor u v edge-data current-distance) → weight` (state-aware)? Each surfaces more context to the visitor; each adds parameters that visitors that don't need them have to ignore.
- **Visitor mask semantics.** Should the visitor be allowed to return `#f` (or a sentinel) to skip this edge in this iteration? Adaptive masking is a real use case but introduces a new control-flow path.
- **Visitor termination.** Should the visitor be allowed to signal "stop the algorithm" (e.g., return a wrapped value)? Useful for early-exit search; complicates the worklist loop.
- **Concurrency.** Under SRFI-18 threads, does the visitor run on the thread that issued the query, or on a worker pool? Stateful visitors expect a consistent identity; pool dispatch may not provide that.
- **Interaction with `graph-analysis`'s cache layer.** `graph.scm:77-82` caches results per-source. If the visitor is stateful, are results meaningfully cacheable? Probably not — visit-style would likely disable the cache, since "the answer" is partly the side effects.
- **Relationship to `graph-query-all`'s return shape.** Does `graph-traverse-from-all` return a distance alist (like Path A) or something more visitor-shaped (an enumeration of traversal events)? The latter is closer to true visit-style semantics.

These do not block Path A. The Path B plan, when written, inherits this question set as its design surface.

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
