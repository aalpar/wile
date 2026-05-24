# Approximate Counting Semirings

**Status:** Design draft — not started.

**Scope:** Add explicit overflow-aware counting semiring constructors to `(wile algebra semiring)`. Existing `counting-semiring` (exact, bignum-backed) stays unchanged. New constructors provide bounded-carrier alternatives with documented trade-offs.

**Repository:** `aalpar/wile`, file `lib/wile/algebra/semiring.sld` (plus tests + doc).

## Motivation

The current `counting-semiring` is the strict mathematical object `(ℕ, +, ×, 0, 1)`. Its carrier ℕ is unbounded, so on workloads where walk/path counts grow beyond int64 it promotes silently to bignums. For most combinatorial graphs of any nontrivial size, this happens almost immediately — a graph with average out-degree 5 produces ≥ 2^64 length-28 walks, and most call graphs have substructure at least that deep.

The 3-hour incident on the `machine` package (539 nodes, 12 back-edges) was a concrete manifestation: counting walks exceeded int64 within ~3 Bellman-Ford iterations, and the remaining ~535 iterations operated on continually-growing bignums. The honest framing isn't "the user asked a pathological question" — it's "the library only exposes the exact form of a question that almost never has a usefully-exact answer."

The principled fix is to give callers explicit choice of which approximation regime to live in:

- **Exact, slow, possibly intractable:** `counting-semiring` (current behavior).
- **Bounded magnitude, possibly inaccurate:** saturating clamp at some cap K — *not* a true semiring (distributivity breaks at the cap), but practically useful for ranking.
- **Bounded magnitude, exactly correct in ℤ/Pℤ:** modular counting for large prime P — IS a true semiring, useful for "is the count nonzero?" with collision probability 1/P.
- **Unbounded range, bounded precision:** log-space counting using `float64` — IS a true semiring (log-sum-exp for ⊕, + for ⊗), useful for ranking when only relative magnitudes matter and absolute precision past ~15 decimal digits is noise.

All three alternatives keep arithmetic in machine-word range, eliminating bignum allocation and growth from the inner loop. Speedup vs. exact counting on cyclic graphs is plausibly multiple orders of magnitude (untested — see acceptance criteria).

## Background

See:
- `feedback-counting-semiring-on-cycles.md` for the 3-hour incident memory.
- `2026-05-24-graph-worklist-bellman-ford.md` (sibling plan) for the orthogonal graph-side speedup. The two plans are complementary: worklist B-F speeds up convergent queries; approximate counting makes the (otherwise non-convergent) counting-on-cycles question tractable in bounded time at the cost of precision.

## Design

### Three constructors

Add three new exports to `(wile algebra semiring)`:

#### (1) `saturating-counting-semiring CAP`

Carrier: int64, clamped to `[0, CAP]`. Operations:

- `⊕(a, b) = min(a + b, CAP)`
- `⊗(a, b) = min(a × b, CAP)` (with overflow check on the multiplication itself before clamping)
- `semiring-zero` = 0
- `semiring-one` = 1

**Not a strict semiring.** Distributivity breaks once saturation activates:

```
a = b = c = CAP/2 + 1
(a + b) * c = CAP * c = CAP    (saturated)
a*c + b*c = ...                ≠ CAP in general
```

Callers must understand this is an *approximate aggregation*, not an algebraically-valid semiring. The library docs MUST flag this clearly. The constructor name's `-counting-` (not just `-counting-semiring`) reinforces "this is a counting-shaped operator, not strictly a semiring."

Useful for: ranking, threshold queries ("any count above K?"), instrumentation. Default CAP suggestion: 2^53 (largest exactly-representable as float64, in case callers convert).

#### (2) `modular-counting-semiring P`

Carrier: int64, values reduced mod P after each operation. Operations:

- `⊕(a, b) = (a + b) mod P`
- `⊗(a, b) = (a × b) mod P`
- `semiring-zero` = 0
- `semiring-one` = 1

**Is a true semiring** (modular arithmetic preserves all semiring axioms). Carrier bounded by P, so values always fit if P < 2^63 (to avoid intermediate overflow in `×` — use 2^62 to be safe).

**Loses exact count, preserves zero-detection.** If true count `c ≡ 0 (mod P)`, modular result is 0 — false-zero collision probability 1/P. For large P (close to 2^62), this is negligible.

Useful for: "exists a path?" with stronger guarantees than boolean, hash-like fingerprints for graph isomorphism via path-count multisets, walk-count-mod-2 (parity) analyses.

Default P suggestion: a large Mersenne or Fermat prime fitting in 62 bits. The library should expose a few standard choices: `mersenne-31`, `mersenne-61`, etc.

#### (3) `log-counting-semiring`

Carrier: float64 in log-space. Stored values are `log(true-count)`. Operations:

- `⊕(a, b) = log-sum-exp(a, b) = max(a, b) + log(1 + exp(min(a, b) - max(a, b)))`
- `⊗(a, b) = a + b`
- `semiring-zero` = -∞
- `semiring-one` = 0

**Is a true semiring** (log-sum-exp is associative and commutative, regular `+` distributes over it). Carrier float64 with ~15 decimal digits of precision; effective magnitude range is `exp(±1e308) ≈ 10^±10^307` — practically unbounded.

**Loses precision past ~2^53, preserves orders of magnitude.** Two paths with counts `10^50` and `10^50 + 1` are indistinguishable; counts of `10^50` vs `10^60` are clearly ranked.

Useful for: relative-magnitude ranking (fan-in pressure, where exact counts past 10^9 are uninteresting anyway), Viterbi-like maximum-likelihood path queries, any analysis where the question is "which node has the most" not "exactly how many."

This is the structural cousin of the tropical semiring (`max`/`+`) — same arithmetic shape, soft-max instead of hard-max.

### Constructor signatures

```scheme
;; Saturating: explicit cap parameter, no default
(saturating-counting-semiring cap)
  -> approximate-semiring

;; Modular: explicit modulus, predefined large primes for convenience
(modular-counting-semiring modulus)
  -> semiring

;; Log-space: no parameters; float64 range is fixed
(log-counting-semiring)
  -> semiring

;; Predefined primes
mersenne-31    ; 2^31 - 1, fits in i32 multiplication after mod
mersenne-61    ; 2^61 - 1, fits in i64 multiplication after mod
fermat-2^32+15 ; example; choose actual primes after research
```

### Type predicate

Add `approximate-semiring?` predicate. Returns `#t` for `saturating-counting-semiring` instances (because they violate distributivity), `#f` for true semirings (including the modular and log variants).

This lets `make-graph-analysis` (and any future consumer) optionally warn or refuse when given an approximate-semiring where strict semiring semantics matter (e.g., closure-via-matrix-power requires distributivity).

## Open design questions

- **Q-1:** Should `saturating-counting-semiring` be called `-semiring` at all if it's not a strict semiring? Alternatives: `saturating-counter`, `bounded-counter`, `approximate-counting-semiring`. **Default:** `approximate-counting-semiring CAP` with `approximate-semiring?` predicate. Honest in the name, parallel structure with the other two.

- **Q-2:** Default modulus for `modular-counting-semiring` if called with no arg? **Default:** require an explicit modulus; no default. Lets callers pick the right size for their false-zero tolerance.

- **Q-3:** Should `log-counting-semiring` use natural log or log base 2? **Default:** natural log. Standard in stochastic-CFG / HMM literature. log2 would be marginally faster for very large counts (no `log(2)` factor) but the difference is irrelevant on modern FPUs.

- **Q-4:** Per-operation underflow handling in `log-counting-semiring` — when `min - max` is very negative, `exp(min - max)` underflows to 0 and `log(1+0) = 0`, giving `log-sum-exp(a, b) = max(a, b)`. This is mathematically correct ("a count of 10^300 plus a count of 10^200 is indistinguishably 10^300") but worth documenting. **Default:** rely on IEEE-754 underflow-to-zero behavior; no explicit guard.

## Implementation plan

### Phase 1 — modular semiring (lowest risk, true semiring)

- Add `modular-counting-semiring P` constructor to `(wile algebra semiring)`.
- Add predefined prime constants (`mersenne-61`, etc.) — actual prime choice based on i64 arithmetic safety.
- Tests:
  - Semiring axioms hold (associativity, commutativity, distributivity, identity) for sample values mod a small prime where they can be checked exhaustively.
  - Counter increments correctly under sequences of `⊕` and `⊗`.
  - Multiplication of values just under sqrt(P) doesn't overflow before mod.

### Phase 2 — log-space semiring (true semiring, float carrier)

- Add `log-counting-semiring` constructor.
- Implementation of `log-sum-exp` with numerical-stability guard (subtract max before exp).
- Tests:
  - Semiring axioms hold approximately (within float epsilon).
  - Log-sum of large counts doesn't overflow (the whole point).
  - Underflow case (one value much smaller than the other) returns the larger correctly.

### Phase 3 — saturating/approximate counter (not a strict semiring)

- Add `approximate-counting-semiring CAP` constructor.
- Add `approximate-semiring?` predicate.
- Tests:
  - Counter increments correctly below the cap.
  - Saturates correctly at the cap on both `⊕` and `⊗`.
  - `approximate-semiring?` returns `#t` for this, `#f` for others.
  - Multiplication overflow before clamp is detected (no wraparound).

### Phase 4 — graph-library integration

- `make-graph-analysis` accepts these semirings unchanged (no API change required — they're just semirings).
- Benchmark: rerun the machine-package counting query (539 nodes, 12 back-edges) under each of the three approximate variants. Expected: all three terminate in seconds rather than hours.
- Document the choice matrix in the `(wile algebra graph)` library docs:
  - "Exact counts, may be slow or non-terminating on cyclic graphs": `counting-semiring`
  - "Bounded magnitude, sortable ranking": `approximate-counting-semiring CAP`
  - "Exact mod P, useful for is-zero queries": `modular-counting-semiring P`
  - "Relative-magnitude ranking, unbounded range": `log-counting-semiring`

### Phase 5 — docs + PR

- Update `(wile algebra semiring)` library description to mention the new constructors.
- Add a section to algebra docs explaining the four-way trade-off.
- Open PR, dual review.

## Risks

- **R-1 — `approximate-semiring?` consumers.** If downstream code (e.g., a hypothetical `matrix-closure` that relies on distributivity) doesn't check `approximate-semiring?`, it will silently produce wrong answers when handed the approximate variant. Mitigation: document `approximate-semiring?` prominently; encourage callers to check it; consider adding a runtime warning in matrix-closure paths.
- **R-2 — Float64 precision boundaries in `log-counting-semiring`.** Two counts that differ by less than ~1 in log-space (i.e., within a factor of `e`) may be indistinguishable. For applications where small relative differences matter, this is a real loss. Documented in the trade-off table; not a blocker for the typical "rank top-K hotspots" use case.
- **R-3 — Modular collisions.** A true count divisible by P returns 0 from the modular semiring, indistinguishable from "no walks exist." For P near 2^62, this is a 1-in-4-quintillion event per query — negligible for almost any application — but worth documenting. Callers who need certainty can run with two different primes and compare.
- **R-4 — Prime selection.** Choosing primes for the predefined constants requires care: P must satisfy `P × P < 2^63` to avoid overflow in `×` before the mod. Mersenne primes are convenient but not all fit this bound. Phase 1 needs an explicit prime-selection step documented in the plan.

## Acceptance criteria

- All three new constructors export from `(wile algebra semiring)`.
- `approximate-semiring?` predicate exists and behaves correctly.
- Semiring axiom tests pass for modular and log variants (within float epsilon for log).
- Saturating-clamp test passes; distributivity-violation test demonstrates the documented failure mode (proves the non-semiring nature is real, not assumed).
- Benchmark: the 539-node machine-package counting query (currently 3-hour bignum hang) terminates in under 1 second using `log-counting-semiring`.
- `make lint && make covercheck && make ci` all green.

## Out of scope

- Worklist Bellman-Ford optimization (sibling plan `2026-05-24-graph-worklist-bellman-ford.md`).
- SCC condensation primitive (separate plan, not yet written — would let exact counting work on cyclic graphs by quotienting out the cycles).
- Tarjan / Kosaraju SCC implementations (separate).
- Other approximate semirings (probabilistic, Viterbi-with-backpointers, expectation semiring) — feasible follow-ups, not required for v1.
- Bignum performance work in Wile's numeric tower (Karatsuba, in-place arithmetic) — these would help the *exact* counting case but are orthogonal to providing approximate alternatives.

## References

- Mohri (2002) *Semiring Frameworks and Algorithms for Shortest-Distance Problems* — establishes the formal semiring framework and k-closedness condition that the approximate variants navigate.
- Goodman (1999) *Semiring Parsing* — application of log-space and Viterbi semirings to stochastic context-free grammars; canonical reference for log-sum-exp arithmetic in this style.
- `feedback-counting-semiring-on-cycles.md` — incident memory.
- `2026-05-24-graph-worklist-bellman-ford.md` — sibling plan on graph-library convergence-detection speedup.
- `2026-04-17-algebra-foundations-directions.md` — algebra roadmap (consider adding a §5 entry for "overflow-aware counting semirings" in next revision).
