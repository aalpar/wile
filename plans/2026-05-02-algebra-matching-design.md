## `(wile algebra matching)` — Two-sided matching design

**Status:** Design locked (2026-05-02); ready for phased implementation. Q1=Yes, Q2=Lazy, Q3=Defer, Q4=Pair (all defaults confirmed).
**Date:** 2026-05-02
**Predecessor:** `plans/2026-04-17-algebra-foundations-directions.md` §4.6, §5.7
**Task-level plan:** `plans/2026-05-02-algebra-matching-impl.md`
**Dependencies shipped:** `plans/2026-04-21-matrix-path-d-impl.md` (§5.1 matrix), `plans/2026-04-22-lattice-birkhoff-impl.md` (§5.5 distributive lattice + Birkhoff), `plans/2026-04-21-incidence-algebra-impl.md` (§5.2 incidence algebra), `plans/2026-04-22-combinatorial-graph-impl.md` (§5.6 — `graph-maximum-bipartite-matching` for sanity comparison)
**Position in roadmap:** Tier B per the 2026-04-22 priority decision (wile-goast-first → matching-second → §5.7 lower priority). All Tier A prerequisites shipped.

---

## Context

Tier A of the algebra roadmap (§5.1 matrix, §5.2 incidence, §5.3 unification, §5.4 group actions, §5.5 distributive lattice + Birkhoff, §5.6 combinatorial graph, §2.2 free Boolean algebra) is complete. The lattice machinery shipped in §5.5 was explicitly motivated by §4.6 of the directions doc: Conway (1976) proved that the set of stable matchings under any preference profile forms a *distributive lattice* under the natural proposer-utility order, and Birkhoff's representation theorem identifies its join-irreducibles as **rotations** in the matching literature. Tier B's matching library is the load-test of that foundation — the place where `birkhoff-representation`, `lattice->locally-finite-poset`, and `fixpoint` get a non-trivial consumer.

This plan ships `(wile algebra matching)`: Roth-Sotomayor two-sided matching primitives organized around the three-layer optimization-constraint-optimization decomposition from §4.6. The library covers Gale-Shapley deferred acceptance, the Hungarian algorithm (the §4.2 named primitive surfacing tropical-permanent assignment as a returnable assignment, not just a cost), Irving's rotation poset, and Conway-lattice traversal for selection inside the stable set. Many-to-one (hospital/intern) is included; many-to-many (Kelso-Crawford) is deferred because it depends on §5.7 matroids (Tier C, not yet shipped).

The two §4.2 sub-items collapse into this library naturally: **§4.2 Hungarian primitive** ships as `tropical-assignment`; **§4.2 Maximum common subgraph** does *not* — it overlaps `(wile algebra combinatorial-graph)` and is tracked separately (closer to the iso/embedding work already shipped there).

## Scope

### In scope

- `(wile algebra matching)` library with three layers per §4.6:
  - **Stability layer** — Gale-Shapley deferred acceptance (one-to-one), proposer-optimal and receiver-optimal extremes
  - **Selection layer** — Conway-lattice machinery via Birkhoff: rotation enumeration, downset traversal, egalitarian / sex-equal / minimum-regret selectors
  - **Assignment layer** — Hungarian algorithm O(n³), returns the assignment plus its tropical cost
- `<preference-profile>`, `<bipartite-matching>`, `<rotation>` record types
- Hospital/intern (many-to-one with quotas) via Roth's college-admissions reduction
- Comprehensive test suite (~50 tests) covering all public exports plus invariants (stability, optimality, Conway-lattice axioms)
- Re-export from `(wile algebra)` aggregator
- User-facing docs section in `docs/algebra/reference.md`

### Out of scope (deferred)

- **Many-to-many (Kelso-Crawford with substitutes)** — depends on §5.7 matroids. Re-open after matroid library ships.
- **Roommates problem (Irving 1985)** — non-bipartite stable matching, may have no stable solution. See Q3.
- **Hospital/intern with couples** — NP-hard in general; tracked separately if a consumer surfaces.
- **Maximum common subgraph (§4.2)** — sibling to `(wile algebra combinatorial-graph)`'s iso machinery, not matching. Separate plan.
- **LP-based matching / assignment polytope** — needs LP infrastructure that wile does not have.
- **Online / streaming matching** — different algorithmic regime; no consumer.
- **Minimum-regret stable matching as an LP** — NP-hard via egalitarian; we ship a brute-force interior selector for small inputs only and document the complexity wall.

## Locked design decisions (formerly Q1–Q4)

User confirmation 2026-05-02: all four questions answered to default.

| # | Decision | Locked answer | Rationale |
|---|---|---|---|
| Q1 | Many-to-one in v1? | **Yes — include hospital/intern.** | Roth's reduction is mechanical; consumers of stable matching usually want quotas; deferring forces a v2 right away. |
| Q2 | Conway lattice eager or lazy? | **Lazy.** | The lattice can be exponential in `n` (≤ Catalan); eager construction would surprise users. Iterator + `stable-matching-lattice` accessor lets consumers materialize on demand. |
| Q3 | Include Roommates (Irving 1985)? | **Defer.** | No workspace consumer; failure mode (returning `#f` for "no stable matching") is a different return-shape from the bipartite functions. Tracked in Future extensions. |
| Q4 | Hungarian return shape? | **Pair `(matching . cost)`.** | Composes with `car`/`cdr`/`assoc` in the common case; matches `divmod` precedent in `(wile algebra polynomial)`. |

## Resolved design decisions

| # | Decision | Rationale |
|---|---|---|
| R1 | Library name: `(wile algebra matching)` (per directions doc), single library | Two-sided matching primitives share enough vocabulary (preference profile, blocking pair, stable set) that splitting into `stable-matching` + `assignment` would force callers to import both for typical work. Hungarian belongs in the same module because it answers "which assignment minimizes total cost" — a sister question to "which assignment is stable." |
| R2 | Preference representation: ordered list `(p1 p2 p3 ...)` with `prefers-strictly?` derived predicate, optionally augmented by a ranking-function for very-large preference spaces | List form matches the mathematical convention (Gusfield-Irving, Roth-Sotomayor textbooks); rank lookup is O(n) per query but n is small in practice. Ranking-function escape hatch for callers with computed preferences (e.g., similarity scores in source-code matching). |
| R3 | Setoid-carried agent equality on both sides | Consistent with §5.4 (`<group>`), §5.5 (`<lattice>`), §5.6 (`<graph>`) tier-lifting convention. Default `(default-setoid)` wraps R7RS `equal?`. |
| R4 | Out-of-scope: many-to-many (Kelso-Crawford) | Substitutes condition requires matroid intersection. Deferred to post-§5.7 matroid library. Add as a Phase-7 follow-up plan. |
| R5 | Hungarian over arbitrary semiring (in spirit of §5.1), implemented for tropical (min-plus) only in v1 | Hungarian is O(n³) only over tropical / max-plus where the negation step has algebraic meaning. Generalizing to arbitrary semirings either reduces to the O(n!·n) `semiring-matrix-permanent` already in `(wile algebra matrix)` (no win) or requires the semiring to admit "negation up to a unit" — unusual. v1 ships tropical only; the matrix permanent already handles the general semiring case. |
| R6 | Phase-3-before-Phase-5 (Hungarian before rotations) | Hungarian is self-contained and has the broadest external use. Ship it early; layer the rotation/lattice machinery on top of the proven Gale-Shapley substrate. |
| R7 | All public exports get docstrings with **Parameters / Returns / Category / Keywords** | LLM-reliability convention per `memory/keywords-motivation.md`. Non-negotiable. |
| R8 | Reuse `(wile algebra lattice)` `birkhoff-reconstruction` rather than re-implementing | The whole point of §5.5 was to make this available. If `birkhoff-reconstruction` proves awkward at the matching boundary, file a follow-up against §5.5 — don't fork the algorithm. |

**Anchoring goals** (from `memory/feedback-algebra-design-goals.md`): broadest application > robust > consistent > performance > brevity. R1, R3, R5, R8 each defer to consistency with shipped algebra libraries over local optimization.

## Layering

New file pair `stdlib/lib/wile/algebra/matching.{scm,sld}`. Imports:

```
(wile algebra matching)
  ├── (scheme base)
  ├── (srfi 1)
  ├── (wile algebra setoid)    ; default-setoid, setoid-equiv?, setoid-member?, validate-opts-keys, assert-procedure
  ├── (wile algebra order)     ; <partial-order> for the proposer-utility order on matchings
  ├── (wile algebra incidence) ; <locally-finite-poset> for the rotation poset
  └── (wile algebra lattice)   ; birkhoff-reconstruction, lattice-leq?, fixpoint
```

No reverse imports. The aggregator `(wile algebra)` re-exports `matching` at Phase 6.

`(wile algebra matrix)` is **not** imported. Hungarian is implemented directly because the matrix library's `semiring-matrix-permanent` returns the optimal *cost* but not the assignment achieving it — and adding that to matrix would conflate "linear algebra" with "combinatorial-optimization output shape." If a future consumer wants the matrix-typed input, an adapter is one line in user code.

## Exports

```scheme
;; Preference profiles
make-preference-profile  preference-profile?
preference-profile-agents              ; → list of agents on this side
preference-profile-ranks-of            ; agent → preference list
preference-profile-prefers-strictly?   ; agent × x × y → boolean
preference-profile-rank-of             ; agent × candidate → integer (1 = top)

;; Matchings (the bipartite kind)
make-bipartite-matching  bipartite-matching?
bipartite-matching-pairs               ; → alist ((proposer . receiver) ...)
bipartite-matching-partner             ; matching × agent → agent | #f
bipartite-matching-unmatched           ; matching × side → list of unmatched agents
bipartite-matching-equal?              ; setoid-respecting equality

;; Stability checks
blocking-pairs                         ; matching × prop-prefs × recv-prefs → list of (p . r)
stable?                                ; matching × prop-prefs × recv-prefs → boolean

;; Gale-Shapley deferred acceptance
gale-shapley                           ; prop-prefs × recv-prefs → bipartite-matching (proposer-optimal)
gale-shapley/receiver-optimal          ; prop-prefs × recv-prefs → bipartite-matching

;; Many-to-one (hospital/intern) — gated on Q1
hospital-intern-match                  ; intern-prefs × hospital-prefs × hospital-quotas → matching
                                       ;   → alist ((hospital . (intern ...)) ...)

;; Hungarian (tropical assignment, §4.2 named primitive)
tropical-assignment                    ; cost-fn × proposers × receivers → (matching . cost)
                                       ;   cost-fn : (proposer × receiver) → number ∪ +inf.0

;; Conway lattice — gated on Q2 (lazy default)
rotations                              ; prop-prefs × recv-prefs → list of <rotation>
make-rotation  rotation?
rotation-cycle                         ; → list of (proposer . receiver) pairs in cyclic order
apply-rotation                         ; matching × rotation → matching
stable-matching-lattice                ; prop-prefs × recv-prefs → <lattice>
                                       ;   carrier = stable matchings, leq = proposer-utility order
egalitarian-stable-matching            ; prop-prefs × recv-prefs → bipartite-matching
                                       ;   minimum-sum-rank; brute force over enumerated stable set
sex-equal-stable-matching              ; prop-prefs × recv-prefs → bipartite-matching
                                       ;   minimizes |sum-rank-prop − sum-rank-recv|

;; Validators
validate-preference-profile            ; profile × candidate-set → #t | violation list
validate-bipartite-matching            ; matching × prop-side × recv-side → #t | violation list

;; Field binders
with-preference-profile  with-bipartite-matching
```

Empty list / `#f` for "no such object" (no blocking pairs, no partner). Errors via `error` reserved for caller misuse: malformed preferences, agent referenced in preferences but not in agent-set, hospital quota ≤ 0.

## Representation

### `<preference-profile>`

```scheme
(define-record-type <preference-profile>
  (make-preference-profile* agents ranks-of setoid)
  preference-profile?
  (agents preference-profile-agents)               ; list of agents on this side
  (ranks-of preference-profile-ranks-of)           ; procedure: agent → preference list
  (setoid preference-profile-setoid))              ; for agent equality

(define (make-preference-profile agents ranks-of . opts)
  "..."
  (assert-procedure "make-preference-profile" ranks-of)
  (validate-opts-keys "make-preference-profile" opts '(setoid))
  (let ((setoid (assv-or opts 'setoid (default-setoid))))
    (make-preference-profile* agents ranks-of setoid)))
```

`ranks-of` as a procedure (not a baked alist) lets callers compute preferences on the fly — important for source-code-matching consumers who score similarity per query. `preference-profile-rank-of` materializes the rank lookup with a small per-call cost; consumers wanting batched rank queries can memoize externally.

### `<bipartite-matching>`

```scheme
(define-record-type <bipartite-matching>
  (make-bipartite-matching* pairs prop-setoid recv-setoid)
  bipartite-matching?
  (pairs bipartite-matching-pairs)                 ; alist ((proposer . receiver) ...)
  (prop-setoid bipartite-matching-prop-setoid)
  (recv-setoid bipartite-matching-recv-setoid))
```

Alist matches the rest of the algebra library's representation discipline (`<substitution>` in unification, `setoid-assoc` everywhere). Both setoids stored so equality checks against another `<bipartite-matching>` can use the right comparison on each side.

### `<rotation>`

```scheme
(define-record-type <rotation>
  (make-rotation* cycle)
  rotation?
  (cycle rotation-cycle))                          ; list of (proposer . receiver) pairs in cyclic order
```

A rotation in Irving's sense: an even-length cycle `(p₀, r₀, p₁, r₁, …, p_{k-1}, r_{k-1})` where applying it to a stable matching `M` produces a stable matching `M'` differing by exactly the swaps `(pᵢ ↦ rᵢ₊₁)`. The rotation poset (Gusfield-Irving 1989) is the locally-finite poset whose downsets are in bijection with stable matchings — Birkhoff's representation in action.

## Algorithms

### Gale-Shapley deferred acceptance (Phase 2)

Classical proposer-side algorithm, O(n²). Implementation as a `fixpoint` over the Conway-utility lattice on partial matchings.

**Fixpoint operator** `f`:

1. If every proposer is matched or has exhausted their preference list, return current matching (fixed point).
2. Otherwise: select an unmatched proposer `p` with non-empty remaining preferences.
3. `p` proposes to its top remaining receiver `r`.
4. If `r` is unmatched, tentatively match `(p, r)`.
5. If `r` is matched to `p'` and prefers `p` to `p'`, replace `p'` with `p`; `p'` becomes unmatched.
6. Otherwise, `p` is rejected; advance `p`'s preference cursor.
7. Return updated state.

**Termination**: every step either matches a previously-unmatched proposer or advances a preference cursor; both are bounded by `n²`. The lattice's `lattice-equal?` detects the fixed point.

**Receiver-optimal variant** swaps proposer/receiver roles. Conway's theorem guarantees these are the lattice top and bottom respectively.

**Stability proof obligation**: a property test asserts that `gale-shapley` output passes `stable?` on random inputs (50 random preference profiles per test).

### Hungarian algorithm (Phase 4)

**Input**: cost function `(proposer × receiver) → number ∪ +inf.0`, agent lists.
**Output**: `(matching . cost)` per R4 default for Q4.

**Connection to Shapley-Shubik assignment game.** The TODO entry's "assignment game core (Shapley-Shubik)" *is* the Kuhn-Munkres dual potentials. At Hungarian termination, the row/column potentials `(uᵢ, vⱼ)` satisfy `uᵢ + vⱼ ≤ C[i,j]` with equality on assigned pairs — exactly the LP dual of the assignment problem, exactly the Shapley-Shubik core allocation that sustains the matching as a competitive equilibrium with transferable utility. v1 returns only the matching and its cost; the potentials are computed internally and discarded. A follow-up `tropical-assignment/with-potentials` variant returning `(matching cost row-potentials col-potentials)` is the natural extension if a consumer wants the core allocation. Tracked in Future extensions.

Kuhn-Munkres O(n³) implementation. Standard textbook approach (Cormen et al. ch. 27 or Burkard-Dell'Amico-Martello *Assignment Problems*):

1. Build cost matrix `C[i,j]` from the cost function. `+inf.0` means "this pair is forbidden."
2. Subtract row minima, then column minima.
3. Cover all zeros with the minimum number of lines (König-Egerváry).
4. If lines = n, an optimal assignment exists in the zero positions — find it via DFS.
5. Otherwise, find the smallest uncovered entry `δ`, subtract from uncovered, add to twice-covered, return to step 3.

**Boundary behavior**:

- Unequal-size sides: pad with synthetic agents at `+inf.0` cost; result excludes synthetic pairs.
- All-`+inf.0` row or column: returns the partial matching with `cost = +inf.0`. Caller checks.

**Sanity test**: random 5×5 cost matrices compared against brute-force `semiring-matrix-permanent` over the tropical semiring (which gives the *cost* but not the assignment) — both must agree on cost.

### Hospital/intern (Phase 3, gated on Q1)

Roth's college-admissions reduction (1985): inflate each hospital `h` with quota `q_h` into `q_h` copies in a one-to-one Gale-Shapley instance, where each copy has the same preference list as `h` and each intern's preference list expands `h` to `(h-copy-1, h-copy-2, …, h-copy-{q_h})` in any consistent order. Apply Gale-Shapley to the inflated instance, then collapse copies back into per-hospital lists.

Returns `((hospital . (intern …)) …)`. Interns matched to no hospital are absent from the alist; an explicit `unmatched-interns` accessor helps consumers.

**Note**: only the *intern-proposing* version is hospital-intern-strategyproof for interns. The default is intern-proposing.

### Rotations and Conway lattice (Phase 5, gated on Q2)

**Rotation enumeration** (Gusfield-Irving 1989, ch. 3):

1. Run Gale-Shapley to get the proposer-optimal matching `M_top`.
2. Build the *reduced preference table*: for each proposer `p` matched to `r = M_top(p)`, delete from `p`'s list everyone *worse* than `r`; symmetrically for receivers.
3. Iteratively detect rotations: cycles in the "second-choice" graph on the reduced table. Each rotation, when applied, advances some proposers down their lists in lockstep.
4. The set of rotations forms a poset under "rotation `ρ_a` precedes `ρ_b` iff `ρ_a` must be applied before `ρ_b` is exposed."

**Lattice construction** — `stable-matching-lattice`:

```scheme
(define (stable-matching-lattice prop-prefs recv-prefs)
  (let* ((rho-list (rotations prop-prefs recv-prefs))
         (rho-poset (rotation-poset rho-list))
         ;; Birkhoff: distributive lattice ≅ downset lattice of join-irreducibles
         (D-lattice (birkhoff-reconstruction rho-poset)))
    ;; Re-label the downset lattice's elements as bipartite matchings via apply-rotation*
    (lattice-relabel D-lattice
                     (lambda (downset)
                       (fold apply-rotation
                             (gale-shapley prop-prefs recv-prefs)
                             downset)))))
```

This is the load-test of §5.5 Birkhoff. If the relabeling step turns out to be awkward (e.g., `lattice-relabel` doesn't exist), the *first* response is to file an enhancement against `(wile algebra lattice)` rather than locally re-implement downset traversal.

**Eager-vs-lazy** (Q2): the default builds rotations + the rotation poset eagerly, but defers `birkhoff-reconstruction` until `stable-matching-lattice` is called. Egalitarian/sex-equal selectors enumerate the stable set on demand and short-circuit when an optimum is found.

### Egalitarian and sex-equal selectors (Phase 5)

Both NP-hard in general (Iwama-Manlove 1999 for sex-equal). v1 ships brute force over the enumerated stable set — correct for small inputs, with documented complexity wall (~10 agents per side as a soft cap). A future plan can ship Feder's 2/3-approximation if a consumer needs scale.

## Test plan (~50 tests, `test/wile/algebra-matching-test.scm`)

| Layer | Count | Coverage |
|---|---:|---|
| Preference profiles | 6 | construct + validate; rank lookup; `prefers-strictly?`; out-of-set candidate; tied preferences (rejected with explicit error in v1); empty preference list |
| Bipartite matchings | 5 | construct; partner lookup hit/miss; unmatched on each side; setoid-respecting equality |
| Stability checks | 4 | `blocking-pairs` empty / non-empty; `stable?` on Gale-Shapley output; instability detection on hand-crafted unstable matching |
| Gale-Shapley | 8 | textbook 4×4 from Gusfield-Irving §1.2; proposer- vs receiver-optimal asymmetry; unequal sides; everyone-prefers-the-same-receiver; cyclic preferences; randomized property test (50 inputs, all stable) |
| Hospital/intern *(if Q1=Yes)* | 6 | textbook hospital-intern from Roth-Sotomayor §5.5; quota = 1 reduces to Gale-Shapley; quota > intern count; intern-optimality; one hospital with quota 0 (excluded); randomized property test |
| Hungarian | 8 | 2×2 by hand; 4×4 vs brute-force tropical permanent (cost agreement); square vs rectangular; all-equal costs (any matching optimal); forbidden pairs (`+inf.0`); zero costs; identity-permutation-optimal triangular matrix; randomized 5×5 |
| Rotations / Conway lattice *(if Q2=Lazy default)* | 7 | rotation enumeration on Gusfield-Irving §3.4 example; rotation poset Hasse diagram matches reference; downset count = stable matching count; egalitarian on textbook example; sex-equal on textbook example; lattice top = proposer-optimal, bottom = receiver-optimal |
| Birkhoff integration | 4 | `stable-matching-lattice` is `validate-distributive-lattice` clean; rotation poset = `lattice->locally-finite-poset` on the stable-matching lattice (round-trip identity on the join-irreducibles); free-distributive-lattice instance check |
| Edge cases | 4 | both sides empty; one side empty; one preference list empty; complete indifference rejected with clear error |

Table-driven where shapes repeat. Every Gale-Shapley test additionally asserts `stable?` on the output. Every rotation test additionally asserts that applying-and-unapplying yields the original matching.

**Test count target is aspirational.** Per the AC-matching post-ship lesson (29 actual vs 47 planned), padding with near-duplicates is worse than honest coverage. If realized count converges lower, document the gap.

## Commit strategy — 6 phases

Each phase lands green on CI (`make lint && make covercheck && make ci`) before the next begins. Progressive commits per `memory/feedback_commit_cadence.md`.

| Phase | Deliverable | Lib LOC | Test LOC |
|---|---|---:|---:|
| 1 | Scaffolding: `<preference-profile>`, `<bipartite-matching>`, validators, `with-X` macros, blocking-pair detection, `stable?` | ~180 | ~120 |
| 2 | Gale-Shapley deferred acceptance (proposer + receiver variants); textbook regression tests | ~120 | ~100 |
| 3 | Hospital/intern via Roth reduction *(gated on Q1; skip if Q1=Defer)* | ~120 | ~80 |
| 4 | Hungarian algorithm O(n³); `tropical-assignment` export; sanity vs `semiring-matrix-permanent` | ~180 | ~110 |
| 5 | Rotation enumeration; rotation poset; `stable-matching-lattice` via `birkhoff-reconstruction`; egalitarian + sex-equal selectors *(gated on Q2; lazy variant if Q2=Lazy default)* | ~220 | ~130 |
| 6 | Aggregator re-export; `docs/algebra/reference.md` section; library docstring with theorems brought into scope (Gale-Shapley 1962, Conway 1976, Roth 1985); `TODO.md` mark-done | ~30 | ~30 |
| **Total (Q1=Yes, Q2=Lazy)** | | **~850** | **~570** |
| **Total (Q1=Defer, Q2=Lazy)** | | **~730** | **~490** |

Total ~1,420 LOC at the high end. Larger than the directions-doc estimate (~600–800 for §5.4 was the closest reference) because this library spans three layers and ships Hungarian as a sister module.

**Phase-3-before-Phase-4 is intentional only if Q1=Yes.** If Q1=Defer, Phase 3 is skipped and Hungarian moves to Phase 3.

## Definition of done

- All tests pass; randomized property tests use a fixed seed for reproducibility
- `make lint && make covercheck && make ci` clean
- Every public export has a docstring with **Parameters / Returns / Category / Keywords**
- `(wile algebra)` aggregator re-exports `matching`
- `algebra_umbrella_drift_test.go` passes (re-export coverage check)
- Library header comment names the theorems brought into scope:
  - Gale-Shapley (1962): the proposer-side deferred-acceptance algorithm produces a stable matching
  - Conway (1976): the set of stable matchings forms a distributive lattice under the proposer-utility order
  - Birkhoff (1937, applied via §5.5): every finite distributive lattice is the downset lattice of its join-irreducibles
  - Roth (1985): hospital-intern as a one-to-one reduction with synthetic positions
  - Iwama-Manlove (1999): sex-equal stable matching is NP-hard in general (documents the brute-force complexity wall)
- `docs/algebra/reference.md` has a `(wile algebra matching)` section
- `TODO.md` Tier B `(wile algebra matching)` entry marked `[x]` with pointer to closing commit; §4.2 Hungarian primitive entry marked `[x]` with pointer to `tropical-assignment` export
- Follow-up plan stub filed: `plans/YYYY-MM-DD-algebra-matching-many-to-many.md` for Kelso-Crawford after §5.7 matroids

## Future extensions (deferred)

- **Many-to-many (Kelso-Crawford with substitutes)** — gated on §5.7 matroids
- **Shapley-Shubik core allocation as a Hungarian return value** — `tropical-assignment/with-potentials` returning `(matching cost row-potentials col-potentials)`. Internal computation already happens; just needs the return-shape plumbing
- **Roommates problem (Irving 1985)** — gated on Q3 user input
- **Hospital-intern with couples** — NP-hard; gated on consumer demand
- **Tied preferences and weak stability** — current v1 rejects ties with an explicit error; lifting requires distinguishing weak / strong / super stability (Manlove 2002 taxonomy)
- **Egalitarian via Feder's 2/3-approximation** — gated on a consumer needing scale beyond brute force
- **Maximum common subgraph** — separate plan; closer to `(wile algebra combinatorial-graph)` iso work
- **Online / streaming matching** — different algorithmic regime
- **Gale-Shapley as a `fixpoint` benchmark** — interesting integration test for `(wile algebra lattice)`'s fixpoint primitive on a non-trivial monotone operator; deferred to a benchmarks PR after lib ships

## Dependencies on shipped algebra

| Used | From | Why |
|---|---|---|
| `default-setoid`, `setoid-equiv?`, `setoid-member?`, `setoid-assoc`, `assert-procedure`, `validate-opts-keys`, `assv-or`, `make-violation-reporter`, `assert-validation` | `(wile algebra setoid)` | Conventions per `stdlib/lib/wile/algebra/CLAUDE.md` |
| `<partial-order>` | `(wile algebra order)` | Proposer-utility order on stable matchings |
| `make-locally-finite-poset`, `lf-poset-leq?`, `lf-poset-elements` | `(wile algebra incidence)` | The rotation poset is locally finite |
| `birkhoff-reconstruction`, `validate-distributive-lattice`, `fixpoint`, `lattice-leq?`, `lattice-equal?` | `(wile algebra lattice)` | Conway lattice; Gale-Shapley as fixpoint |

If any required helper turns out to be missing or awkwardly shaped, file the enhancement against the upstream library before working around it locally — that preserves the algebra library's coherence (per principle 3 of the directions doc Part 8).

---

## Appendix A — The three-layer structure as code

Per §4.6 of the directions doc, stable matching decomposes as:

| Layer | This library | Substrate |
|---|---|---|
| Local optimization | `gale-shapley`, `gale-shapley/receiver-optimal`, `tropical-assignment` | Per-agent ordinal preferences; Hungarian's per-row/column minima |
| Stability constraint | `stable?`, `blocking-pairs`, `validate-bipartite-matching` | Pure boolean; no optimization happens here |
| Global selection | `stable-matching-lattice`, `egalitarian-stable-matching`, `sex-equal-stable-matching`, `rotations` | `(wile algebra lattice)` Birkhoff machinery |

Reading the export list left-to-right walks the three layers. New selectors (Rawlsian, minimum-regret) plug in at the global-selection layer without touching the stability layer.

## Appendix B — References

- Gale, D. & Shapley, L. (1962). "College Admissions and the Stability of Marriage." *American Mathematical Monthly* 69(1), 9–15.
- Conway, J. H. (1976). Quoted in Knuth, *Mariages stables et leurs relations avec d'autres problèmes combinatoires*. (Distributive-lattice theorem on stable matchings.)
- Roth, A. E. (1985). "The college admissions problem is not equivalent to the marriage problem." *Journal of Economic Theory* 36, 277–288.
- Roth, A. E. & Sotomayor, M. A. O. (1990). *Two-Sided Matching: A Study in Game-Theoretic Modeling and Analysis*. Cambridge University Press.
- Gusfield, D. & Irving, R. W. (1989). *The Stable Marriage Problem: Structure and Algorithms*. MIT Press. (Definitive reference for rotations.)
- Irving, R. W. (1985). "An efficient algorithm for the 'stable roommates' problem." *Journal of Algorithms* 6, 577–595.
- Iwama, K., Miyazaki, S., Morita, Y., & Manlove, D. (1999). "Stable marriage with incomplete lists and ties." (NP-hardness of sex-equal stable matching.)
- Kuhn, H. W. (1955). "The Hungarian Method for the assignment problem." *Naval Research Logistics Quarterly* 2, 83–97.
- Munkres, J. (1957). "Algorithms for the Assignment and Transportation Problems." *Journal of SIAM* 5, 32–38.
- Burkard, R. E., Dell'Amico, M., & Martello, S. (2009). *Assignment Problems*. SIAM. (Modern reference.)
- Birkhoff, G. (1937). "Rings of sets." *Duke Mathematical Journal* 3, 443–454. (The representation theorem §5.5 ships.)

---

## Pre-implementation checklist

Before any code lands:

- [ ] User confirms Q1, Q2, Q3, Q4 (or accepts defaults explicitly)
- [ ] User confirms the scope boundary (Kelso-Crawford and MCS deferred)
- [ ] Branch `feat/algebra-matching` created from `master` (verify base)
- [ ] Plan committed as commit 1
- [ ] Companion `-impl.md` written with phase-by-phase task breakdown (6–8 steps per phase, TDD shape)
