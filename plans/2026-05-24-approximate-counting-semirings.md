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

**Is a true commutative semiring.** Saturating arithmetic on `[0, CAP]` preserves every semiring axiom — associativity, commutativity, distributivity, identities, and the strict zero-annihilation of `⊗`. The cap acts as an absorbing top element for nonzero multipliers (analogous to ∞ in the complete-natural-number semiring `(ℕ ∪ {∞}, +, ×)`); the structure is isomorphic to the quotient of that semiring by the equivalence `x ≥ CAP ↦ ⊤`. Absorbing-top constructions are standard in semiring theory (cf. tropical `(max, +)` on `ℝ ∪ {-∞, +∞}`).

Distributivity proof sketch: for any a, b, c ∈ [0, CAP], both `a ⊗ (b ⊕ c)` and `(a ⊗ b) ⊕ (a ⊗ c)` reduce to `min(a × (b + c), CAP) = min(a × b + a × c, CAP)`. The clamping commutes with the outer `min` because the cap absorbs anything ≥ CAP from either side. Worked example: with CAP = 10, a = b = c = 6 (the case originally claimed as a counterexample), both sides evaluate to `min(60, 10) = 10` and `min(min(36,10) + min(36,10), 10) = min(20, 10) = 10` — equal, as required.

**Information-bounded, not algebraically-defective.** Two distinct true counts that both exceed CAP saturate to the same value (CAP) and become indistinguishable in the carrier. This is a *loss of information*, not a violation of any semiring axiom. The library docs MUST flag this clearly: values at CAP convey "≥ CAP," not an exact count. Composition past the cap degrades: once two operands both equal CAP, every subsequent operation produces CAP, collapsing magnitude information for the rest of the computation chain.

Useful for: ranking, threshold queries ("any count above K?"), instrumentation. Default CAP suggestion: 2^53 (largest exactly-representable as float64, in case callers convert).

#### (2) `modular-counting-semiring P`

Carrier: int64, values reduced mod P after each operation. Operations:

- `⊕(a, b) = (a + b) mod P`
- `⊗(a, b) = (a × b) mod P`
- `semiring-zero` = 0
- `semiring-one` = 1

**Is a true semiring** (modular arithmetic preserves all semiring axioms).

**Loses exact count, preserves modular structure.** If true count `c ≡ 0 (mod P)`, modular result is 0 — false-zero collision probability ~1/P for *random / unstructured* inputs. Can be non-negligible for adversarial or structured inputs where true counts happen to be multiples of P by construction.

**Useful for** queries where the modular value is interpreted as a *hash, witness, or parity*, NOT as an approximate count:

- **Graph fingerprinting** — walk-count multisets `W^ℓ(G)` mod P as isomorphism witnesses. If fingerprints differ, graphs are definitely non-isomorphic. Used in Weisfeiler-Leman variants and color-refinement heuristics.
- **Parity counting (mod 2)** — bipartiteness via cycle-parity, perfect-matching parity via Edmonds/Tutte matrix; ⊕P-complete decision problems.
- **Schwartz-Zippel polynomial identity testing** — verify combinatorial identities by evaluating both sides mod a random prime P; equality mod P implies equality with high probability.
- **Rabin-Karp style structural hashing** — sub-graph indexing, content-addressable graph storage; modular fingerprint = O(1) equality test with ~1/P collision rate.
- **Karp-style randomized algorithms** — perfect matching detection via Tutte determinant mod P, polynomial-time RP algorithms whose decision form factors through modular arithmetic.

If you want "approximately N walks" with a meaningful magnitude, use the saturating or log-space variants instead — modular values are not approximate counts.

##### Numeric safety: dispatch path selection

Multiplication is the binding constraint for staying in int64:

```
(P - 1)² ≤ 2^63 - 1   ⟹   P ≤ √(2^63) ≈ 3.04 × 10^9   ⟹   P < 2^31 (with safety margin)
```

Addition is much looser: `2(P - 1) ≤ 2^63 - 1` allows `P ≤ 2^62`.

The semiring picks one of two dispatch paths at construction time:

| P range | Path | Per-op cost |
|---------|------|-------------|
| `P < 2^31` | **Pure int64.** `(a + b) % P`, `(a * b) % P`. | Constant-time, zero allocation. *Ideal case.* |
| `2^31 ≤ P ≤ 2^62` | **Bignum fallback for ×.** `+` stays pure int64. `×` uses `big.Int.Mul` + `big.Int.Rem` against a precomputed `bigP`. | ~3 allocations per `×`; constant-time `+`. *Acceptable but undesirable.* |
| `P > 2^62` | **Rejected at construction.** Even `+` could overflow int64. |

The bignum-fallback path is the natural client of the sibling `2026-05-24-bignum-allocation-reduction.md`'s in-place API — reuse one scratch `*big.Int` for the product and the precomputed `bigP` instead of allocating per call (drops 3 allocations to 0 per `×`). A no-allocation alternative using `math/bits.Mul64` (returns hi:lo 128-bit product) with manual Barrett-style reduction is a further future improvement, deferred to keep this plan focused.

##### Construction-time validation

When `(modular-counting-semiring P)` is called, validate before returning:

| Check | Action on failure |
|-------|-------------------|
| `P` is an exact integer ≥ 2 | Error: `werr.WrapForeignErrorf(werr.ErrInvalidArgument, "modular-counting-semiring: modulus must be an exact integer ≥ 2, got %v", P)` |
| `P ≤ 2^62` (addition-safe) | Error: modulus exceeds int64 addition-safety threshold; modular counting unavailable. |
| `P < 2^31` (multiplication-safe) | Select **pure-int64** closures. |
| `2^31 ≤ P ≤ 2^62` | Select **bignum-fallback** closures. Emit a debug-level log noting the slower path. |
| `P` prime *(advisory only)* | Not enforced. Use-case dependent: Schwartz-Zippel and Tutte-matrix applications require primality; fingerprinting / parity just need P large (or = 2). |

Construction-time dispatch wires the right closures into the `<semiring>` record; runtime operations never re-check.

##### Per-op implementations

```go
// Pure int64 path (P < 2^31).
func (s *modularSemiringInt64) plus(a, b int64) int64 {
    return (a + b) % s.P
}
func (s *modularSemiringInt64) times(a, b int64) int64 {
    return (a * b) % s.P
}

// Bignum-fallback path (2^31 ≤ P ≤ 2^62).
func (s *modularSemiringBig) plus(a, b int64) int64 {
    return (a + b) % s.P    // still safe in int64
}
func (s *modularSemiringBig) times(a, b int64) int64 {
    prod := new(big.Int).Mul(big.NewInt(a), big.NewInt(b))
    prod.Rem(prod, s.bigP)  // Rem, not Mod — values are already non-negative
    return prod.Int64()      // result < P ≤ 2^62, always fits in int64
}
```

Use `Rem`, not `Mod`, in the fallback: operands are guaranteed in `[0, P-1]` by the canonicalization step at the semiring boundary, so the sign-normalization `Mod` performs is unnecessary work.

##### Input normalization

External integer inputs (edge weights, initial values, user-supplied operands) may be negative or `≥ P`. A boundary primitive coerces to `[0, P-1]`:

```go
func (s *modularSemiring) canonicalize(a int64) int64 {
    r := a % s.P
    if r < 0 {
        r += s.P
    }
    return r
}
```

Called once per external input. Internal `plus`/`times` preserve the `[0, P-1]` invariant, so dispatch closures never re-canonicalize.

##### Named primes

```scheme
mersenne-31   ; 2^31 - 1 = 2,147,483,647     — pure-int64 dispatch,    ~1 / 2×10^9  collision rate
mersenne-61   ; 2^61 - 1 ≈ 2.3 × 10^18       — bignum-fallback dispatch, ~1 / 2×10^18 collision rate
```

The two named constants give callers an explicit speed/collision trade-off. Most applications should default to `mersenne-31`; use `mersenne-61` only when collision probability dominates the cost calculation (e.g., adversarial-input fingerprinting where 1/2×10⁹ is too high). Fermat primes are *not* included — most are composite (only F₀…F₄ are known prime), and none below 2^31 are useful primes outside the Mersenne set.

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
  -> semiring        ; true commutative semiring; bounded carrier

;; Modular: explicit modulus, predefined large primes for convenience
(modular-counting-semiring modulus)
  -> semiring

;; Log-space: no parameters; float64 range is fixed
(log-counting-semiring)
  -> semiring

;; Named primes (see §Design / Modular / Named primes for trade-offs)
mersenne-31    ; 2^31 - 1 = 2,147,483,647       — pure-int64 dispatch
mersenne-61    ; 2^61 - 1 ≈ 2.3 × 10^18         — bignum-fallback dispatch
```

### Type predicate

Add `bounded-carrier-semiring?` predicate. Returns `#t` for `saturating-counting-semiring` instances (carrier saturates at CAP — values past CAP are indistinguishable, magnitude information is lost), `#f` for `modular-counting-semiring` (carrier is exactly `Z/PZ`; values are well-defined modular fingerprints, not approximations) and `log-counting-semiring` (carrier covers the float64 magnitude range; bounded precision, unbounded magnitude).

The predicate is a *semantic warning*, not an algebraic flag: all three approximate variants are true semirings. It marks semirings whose carrier has a *saturation point past which information is irrecoverable*, so downstream consumers (e.g., `make-graph-analysis`) can warn callers that bounded-carrier results past the saturation point will be uninformative.

Note: an earlier draft of this plan named the predicate `approximate-semiring?` and justified it by claiming saturating-counting violated distributivity. The algebraic justification was incorrect — saturating-counting IS a true commutative semiring (see §Design / (1)). The rename reflects the actual semantic distinction: bounded vs. unbounded carrier, not valid vs. invalid algebra.

## Open design questions

- **Q-1:** [Resolved — premise was incorrect.] Originally asked whether `saturating-counting-semiring` should be called `-semiring` if it's not a strict semiring. The premise rested on the (mistaken) claim that distributivity fails under saturation. It does not — saturating-counting IS a true commutative semiring (see §Design / (1) and the distributivity proof sketch there). The `-semiring` suffix is correct; the predicate is renamed to `bounded-carrier-semiring?` to mark the actual semantic distinction (information loss past CAP, not algebraic defect).

- **Q-2:** Default modulus for `modular-counting-semiring` if called with no arg? **Default:** require an explicit modulus; no default. Lets callers pick the right size for their false-zero tolerance.

- **Q-3:** Should `log-counting-semiring` use natural log or log base 2? **Default:** natural log. Standard in stochastic-CFG / HMM literature. log2 would be marginally faster for very large counts (no `log(2)` factor) but the difference is irrelevant on modern FPUs.

- **Q-4:** Per-operation underflow handling in `log-counting-semiring` — when `min - max` is very negative, `exp(min - max)` underflows to 0 and `log(1+0) = 0`, giving `log-sum-exp(a, b) = max(a, b)`. This is mathematically correct ("a count of 10^300 plus a count of 10^200 is indistinguishably 10^300") but worth documenting. **Default:** rely on IEEE-754 underflow-to-zero behavior; no explicit guard.

## Implementation plan

### Phase 1 — modular semiring (lowest risk, true semiring)

- Add `modular-counting-semiring P` constructor to `(wile algebra semiring)` with construction-time validation per the table in §Design / Modular / Construction-time validation.
- Implement both dispatch paths (`modularSemiringInt64` for P < 2^31, `modularSemiringBig` for 2^31 ≤ P ≤ 2^62). Selection at construction time, no per-op re-check.
- Add named prime constants: `mersenne-31` (2^31 - 1, pure-int64) and `mersenne-61` (2^61 - 1, bignum-fallback).
- Add `canonicalize` boundary primitive for external-input normalization.
- Tests:
  - Semiring axioms hold (associativity, commutativity, distributivity, identity) for sample values mod a small prime where they can be checked exhaustively.
  - Counter increments correctly under sequences of `⊕` and `⊗`.
  - Pure-int64 path: multiplication of values just under sqrt(P) doesn't overflow before mod (P < 2^31).
  - Bignum-fallback path: multiplication of values just under P doesn't overflow (P near 2^62); result correctly reduced and fits in int64.
  - Construction-time validation rejects: non-integer P, P < 2, P > 2^62.
  - Construction-time dispatch selection: P = 2^31 - 1 → pure-int64, P = 2^31 → bignum-fallback, P = 2^61 - 1 → bignum-fallback.
  - Canonicalization: negative inputs and inputs ≥ P normalize correctly to `[0, P-1]`.

### Phase 2 — log-space semiring (true semiring, float carrier)

- Add `log-counting-semiring` constructor.
- Implementation of `log-sum-exp` with numerical-stability guard (subtract max before exp).
- Tests:
  - Semiring axioms hold approximately (within float epsilon).
  - Log-sum of large counts doesn't overflow (the whole point).
  - Underflow case (one value much smaller than the other) returns the larger correctly.

### Phase 3 — saturating semiring (true semiring; bounded carrier)

- Add `saturating-counting-semiring CAP` constructor.
- Add `bounded-carrier-semiring?` predicate.
- Tests:
  - Counter increments correctly below the cap.
  - Saturates correctly at the cap on both `⊕` and `⊗`.
  - `bounded-carrier-semiring?` returns `#t` for this, `#f` for modular and log variants.
  - Multiplication overflow before clamp is detected (no int64 wraparound).
  - Semiring axioms hold (associativity, commutativity, distributivity, identities, zero-annihilation) on sample values that include post-saturation cases — explicitly including `a = b = c = CAP/2 + 1` (the case originally listed as a distributivity counterexample) to lock in that both sides evaluate to CAP.

### Phase 4 — graph-library integration

- `make-graph-analysis` accepts these semirings unchanged (no API change required — they're just semirings).
- Benchmark: rerun the machine-package counting query (539 nodes, 12 back-edges) under each of the three approximate variants. Expected: all three terminate in seconds rather than hours.
- Document the choice matrix in the `(wile algebra graph)` library docs:
  - "Exact counts, may be slow or non-terminating on cyclic graphs": `counting-semiring`
  - "Bounded magnitude, sortable ranking; saturates past CAP": `saturating-counting-semiring CAP`
  - "Modular hash / parity / fingerprint; not a count": `modular-counting-semiring P`
  - "Relative-magnitude ranking, unbounded range, bounded precision": `log-counting-semiring`

### Phase 5 — docs + PR

- Update `(wile algebra semiring)` library description to mention the new constructors.
- Add a section to algebra docs explaining the four-way trade-off.
- Open PR, dual review.

## Risks

- **R-1 — `bounded-carrier-semiring?` consumers.** Downstream operations that expect *unbounded-carrier* semantics (e.g., a hypothetical `matrix-closure` whose user wants exact counts that might exceed CAP) will silently saturate to CAP when handed a bounded-carrier semiring, producing a value that conveys "≥ CAP" rather than the true count. Distributivity and the other semiring axioms still hold; the *information* the user wanted is what's lost. Mitigation: document `bounded-carrier-semiring?` prominently; encourage callers to check it; consider adding a runtime warning in downstream paths where saturation would degrade output quality.
- **R-2 — Float64 precision boundaries in `log-counting-semiring`.** Two counts that differ by less than ~1 in log-space (i.e., within a factor of `e`) may be indistinguishable. For applications where small relative differences matter, this is a real loss. Documented in the trade-off table; not a blocker for the typical "rank top-K hotspots" use case.
- **R-3 — Modular collisions.** A true count divisible by P returns 0 from the modular semiring, indistinguishable from "no walks exist." For P near 2^62, this is a 1-in-4-quintillion event per query — negligible for almost any application — but worth documenting. Callers who need certainty can run with two different primes and compare.
- **R-4 — Prime selection.** Resolved by the dispatch-path design in §Design / Numeric safety: P < 2^31 uses pure int64; 2^31 ≤ P ≤ 2^62 uses bignum fallback; P > 2^62 is rejected at construction. The plan now provides `mersenne-31` (pure-int64) and `mersenne-61` (bignum-fallback) as the two named choices, each documented with its dispatch path and collision rate. Residual risk: users passing arbitrary primes might pick values in the bignum-fallback range without realizing the performance implication — mitigated by the debug-level log on bignum-fallback construction.

## Acceptance criteria

- All three new constructors export from `(wile algebra semiring)`.
- `bounded-carrier-semiring?` predicate exists and behaves correctly (#t for saturating, #f for modular and log).
- Semiring axiom tests pass for **all three** variants — modular (exact), log (within float epsilon), and saturating (including post-saturation cases such as `a = b = c = CAP/2 + 1`).
- Saturating-clamp test passes (values past CAP correctly saturate; multiplication overflow before clamp is detected without int64 wraparound).
- Benchmark: the 539-node machine-package counting query (currently 3-hour bignum hang) terminates in under 1 second using `log-counting-semiring`.
- `make lint && make covercheck && make ci` all green.

## Out of scope

- Worklist Bellman-Ford optimization (sibling plan `2026-05-24-graph-worklist-bellman-ford.md`).
- SCC condensation primitive — shipped in `plans/2026-05-26-scc-condensation.md` (`algebra/graph/scc.go` + `CountPathsCyclic`). Complementary, not exclusive: condensation gives exact counts with entry-count semantics on non-trivial SCCs; the approximate carriers in this plan give bounded-precision counts with original semantics. A caller chooses which trade-off fits.
- Tarjan / Kosaraju SCC implementations (separate).
- Other approximate semirings (probabilistic, Viterbi-with-backpointers, expectation semiring) — feasible follow-ups, not required for v1.
- Bignum performance work in Wile's numeric tower (allocation reduction, in-place arithmetic) — sibling plan `2026-05-24-bignum-allocation-reduction.md`. Helps the *exact* counting case, orthogonal to providing approximate alternatives here. (Karatsuba was originally listed; verified during audit that `math/big` already provides it.)

## References

- Mohri (2002) *Semiring Frameworks and Algorithms for Shortest-Distance Problems* — establishes the formal semiring framework and k-closedness condition that the approximate variants navigate.
- Goodman (1999) *Semiring Parsing* — application of log-space and Viterbi semirings to stochastic context-free grammars; canonical reference for log-sum-exp arithmetic in this style.
- `feedback-counting-semiring-on-cycles.md` — incident memory.
- `2026-05-24-graph-worklist-bellman-ford.md` — sibling plan on graph-library convergence-detection speedup.
- `2026-04-17-algebra-foundations-directions.md` — algebra roadmap (consider adding a §5 entry for "overflow-aware counting semirings" in next revision).
