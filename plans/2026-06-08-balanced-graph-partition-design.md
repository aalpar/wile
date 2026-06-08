# Balanced Graph Partition for `(wile algebra ...)` — Design

**Date:** 2026-06-08
**Status:** Design approved — Phase 1 objective = **Kernighan-Lin / FM** (decided 2026-06-08, §5)
**Scope:** Option "b" of the wile-goast `recommend_split` remediation — add a *real*
balanced-cut primitive to Wile's algebra library so wile-goast can import it instead of
calling a heuristic mislabeled "min-cut".

---

## 1. Problem

`wile-goast`'s `recommend_split` is advertised (MCP description, and `lib/wile/goast/split.scm:332`
docstring) as **"IDF-weighted FCA + min-cut package split"**. It is not.

Reading `split.scm` directly:

- `find-split` (`split.scm:231–275`) never builds a graph. Its "cut" is the set of functions
  landing in **both** chosen FCA concept extents plus the attribute-affinity ties
  (`assign-remainder`, `split.scm:216–229`). `cut-ratio = |cut| / |functions|`.
- `best-split-pair` (`split.scm:198–214`) selects by **balance**: it maximizes
  `min(|e1|, |e2|)`, breaking ties on coverage.

So the implemented objective is *"two large, balanced, lattice-incomparable concepts, with
leftovers assigned by attribute overlap."* There is no graph, no edge weights, no flow network,
and no cut in the graph-theoretic sense. The "min-cut" label is aspirational.

Option "b" (the real fix, vs. option "a" = just rename the docstring): add a genuine
weighted-graph partition primitive to the algebra library, and rewire `split.scm` to build an
affinity graph and call it.

---

## 2. What objective does the splitter actually want?

This is the load-bearing question, and it eliminates an entire family of algorithms before we
compare candidates.

The splitter wants to recommend *module boundaries*: cleave a package into two **internally
cohesive, weakly-coupled** groups. Two properties matter:

1. **Low cross-group coupling** — few/light edges severed.
2. **Non-degenerate sizes** — a recommendation that peels off a single function is useless;
   the current code hard-codes a balance term precisely to avoid this.

These two together are a **balanced cut** (or its smarter cousin, **normalized cut**), *not* a
global minimum cut and *not* an s–t minimum cut. This distinction drives §4 and §5.

### 2.1 The affinity graph (prerequisite for any cut)

No graph exists today; any cut algorithm needs one. Proposed construction, built in
`wile-goast` (the consumer), reusing the existing IDF machinery:

- **Nodes** = functions in the package.
- **Edge weight** `w(f, g)` = coupling between `f` and `g`, derived from the IDF-filtered import
  signatures already computed in `split.scm` (`import-signatures`, `compute-idf`,
  `filter-noise`). Candidate: IDF-weighted shared-attribute affinity
  `w(f,g) = Σ_{a ∈ attrs(f) ∩ attrs(g)} idf(a)` — two functions that share *rare* dependencies
  are strongly coupled; sharing ubiquitous ones (already filtered) counts little.

The FCA concept lattice does **not** disappear — it can still gate *whether* a split is worth
proposing (eigengap / concept structure as a confidence signal) — but the partition itself runs
on the weighted affinity graph.

The library primitive (§6) is generic over any weighted graph; the affinity construction above
is wile-goast's business, not the algebra library's.

---

## 3. Library home (minor, but pin it now)

The user framed this as "add min-cut to `(wile algebra graph)`." Note that `(wile algebra graph)`
(`graph.sld`) is the **semiring path-analytics** surface (`graph-query`, `graph-analysis-sccs`,
Bellman-Ford parameterized by semiring). The **weighted combinatorial graph type** — `make-graph`,
`graph-vertices`, `graph-edges`, and crucially `graph-bipartition` — lives in
`(wile algebra combinatorial-graph)`.

A balanced-partition operator is a sibling of `graph-bipartition` and operates on the
combinatorial graph type, so its natural home is **`(wile algebra combinatorial-graph)`**, not
`(wile algebra graph)`. Open question OQ-1 (§9) confirms this with the user.

---

## 4. Rejected algorithm family: global / s–t minimum cut

The doc was explicitly asked to justify *why Ford-Fulkerson, Dinic, and Karger were not selected.*
All three solve a minimum-cut problem — but the wrong one for §2's objective.

### 4.1 Ford-Fulkerson — rejected

Ford-Fulkerson (and Edmonds-Karp) computes maximum flow, and by max-flow/min-cut duality, the
minimum **s–t** cut. Two disqualifiers:

- **No natural source/sink.** Package-splitting has no `s` and `t`. Recovering a *global* min-cut
  from s–t machinery requires `n−1` max-flow computations (a Gomory-Hu tree) — gratuitous.
- **Wrong objective anyway.** Even the global min-cut it would compute is *unbalanced* (see §4.4).

### 4.2 Dinic — rejected

Dinic is an asymptotically faster max-flow algorithm (`O(V²E)`, better on unit-capacity graphs).
It is the *same* s–t framing as Ford-Fulkerson with a better engine. The framing problem (no s/t)
and the objective problem (unbalanced) are identical; the speed advantage addresses neither.
Selecting Dinic would be optimizing the constant factor of the wrong computation.

### 4.3 Karger / Karger-Stein — rejected

Karger's contraction algorithm computes a *global* min-cut by randomized edge contraction
(`O(V²)` per trial, `O(V² log V)` for Karger-Stein, with high-probability correctness over many
trials). Two independent disqualifiers:

- **Non-deterministic.** A static-analysis tool must produce **reproducible** findings; the same
  package must yield the same recommendation across runs and across the belief-DSL's caching.
  A Monte-Carlo cut that varies run-to-run is disqualifying on its own. (Cross-ref the project's
  determinism stance: findings are cached and diffed.)
- **Wrong objective.** Like all global-min-cut methods, it ignores balance (§4.4).

### 4.4 …and global min-cut itself (Stoer-Wagner) — also rejected

Even the *deterministic* global-min-cut algorithm, **Stoer-Wagner** (`O(VE + V² log V)`), is
rejected — and this is the crucial correction to the project's earlier offhand "Stoer-Wagner is
the natural fit" remark.

**The minimum cut of a graph is a known-bad clustering objective.** The minimum-weight cut almost
always isolates a *single* weakly-connected vertex, because cut weight grows with the number of
severed edges and a singleton severs the fewest. This is the textbook motivation for *normalized*
and *ratio* cut (Shi-Malik 2000; Hagen-Kahng 1992). Dropping Stoer-Wagner into `recommend_split`
would mostly recommend "extract this one function," and a downstream balance gate would reject the
majority of its own outputs — replacing an honest heuristic with a dishonest one.

**Conclusion of §4:** every minimum-cut variant — s–t (FF, Dinic), randomized global (Karger), and
deterministic global (Stoer-Wagner) — optimizes either an inapplicable (s–t) or degenerate
(unbalanced) objective. The selected algorithm must target a **balanced** or **normalized** cut.

---

## 5. Candidate algorithms (OPEN DECISION)

Three candidates target the right objective. They are not strictly mutually exclusive — option C
is A built on top of B.

### Comparison

| Dimension | A. Kernighan-Lin / FM | B. Shi-Malik (normalized cut) | C. Hybrid (spectral seed → KL/FM refine) |
|---|---|---|---|
| Objective | min cut s.t. `|A|≈|B|` (balance as **hard constraint**, FM = tolerance) | min `Ncut = cut/vol(A) + cut/vol(B)` (balance as **volume penalty** in objective) | normalized-cut objective, then local polish |
| Search | Local search from a seed; local minima; seed-sensitive | Global (eigenvector of whole graph); seed-free | Global seed + local refinement |
| Unequal natural clusters (e.g. 60/12) | Poor — forces ≈equal halves | **Good** — volume tolerates unequal-but-cohesive | Good |
| Native solution form | Discrete labels directly | Real **eigenvector** (1-D embedding); partition is a *derived* rounding | Eigenvector → discrete → refined labels |
| Extra signal | Hard labels only | Ordering + ambiguity (distance from threshold); **eigengap = confidence** | Same as B, plus tighter cut |
| Determinism | Deterministic given seed; restarts reintroduce nondeterminism | Deterministic up to eigensolver (caveat: near-degenerate eigenvalues) | Inherits both caveats |
| k-way | 2-way only (recursive for k) | Natural (k eigenvectors + k-means) | Natural |
| Dependency | **None** — pure Scheme | **Symmetric eigensolver** (gonum `mat.EigenSym`); see `plans/2026-04-18-gonum-integration-directions.md` | Eigensolver + Scheme refine |
| Scale (this consumer: ≤~hundreds of nodes) | Trivial | Trivial | Trivial |
| Scale (general lib use: 10³–10⁵ call-graph nodes) | FM near-linear/pass; quality needs multilevel coarsening | Sparse Lanczos wins — **but Wile's gonum path is dense `O(n³)`**, so the textbook scaling advantage is not deliverable today | Dense `O(n³)` ceiling until a sparse solver exists |

### Answering the four questions that prompted this section

- **Benefits of Shi-Malik over KL:** global (seed-free) solution; handles unequal-but-cohesive
  clusters that KL's hard balance constraint mangles; yields a continuous embedding with a free
  confidence signal (eigengap) and ambiguity detection; extends naturally to k-way.
- **Is the solution form the same?** *Final* form yes — both emit a 2-way vertex partition that
  drops into `find-split`'s `group-a`/`group-b`/`cut` contract. *Native* form no — KL's native
  output is the discrete labeling; Shi-Malik's is an eigenvector that is *rounded* into a
  partition, so it carries strictly more information.
- **Can Shi-Malik solve much larger graphs?** In theory yes (sparse iterative eigensolvers reach
  10⁵–10⁷ nodes); in practice **not via Wile's available tooling** (gonum dense `EigenSym` is
  `O(n³)`), and **irrelevant to the splitter** (which fails fast at `max-attributes = 30`). Honest
  framing: the scaling advantage is real in the literature and moot for this consumer.
- **Are the semantics different?** Yes. KL = "two roughly equal halves, fewest cross-edges,"
  unnormalized cut weight (so the existing `cut-ratio` 0.15/0.30 bands stay scale-sensitive).
  Shi-Malik = "two cohesive communities sized by volume," normalized cut (comparable across
  package sizes; eigengap replaces hand-tuned bands). They produce **different recommendations in
  kind**, not just different cuts.

### Recommendation

**A (Kernighan-Lin / Fiduccia-Mattheyses) as the first library primitive, architected so a
spectral seed can slot in later (→ C).**

Rationale:
- **No new dependency.** CLAUDE.md: prefer the standard library over new dependencies. gonum is a
  heavy external dependency, and its integration is currently only *directions*, not done. The
  existing algebra-graph kernels (`algebra/graph/scc.go`, `monotone.go`) are pure-Scheme-plus-
  optional-Go-fastpath with zero external deps; A preserves that.
- **Kernighan-Lin pair-swaps** preserve the seed's A/B ratio, so an unequal split (e.g. 60/12) is
  obtained by *seeding* that ratio and letting KL minimize the cut at it — no eigensolver needed.
  (Implementation note 2026-06-08: single-vertex FM moves were tried first and *deadlock* from a
  balanced even-`n` seed under a tight tolerance — see impl-plan Q-2 — so the engine is true KL
  pair-swaps, which hold sizes invariant and never freeze.)
- **Deterministic** by fixing the seed (e.g., seed from the best FCA-incomparable pair the current
  code already computes — a free, meaningful, reproducible initial bipartition).
- **Right scale.** At the splitter's tens-to-hundreds of nodes, A is instant and B's scaling edge
  is unrealizable anyway.

Shi-Malik's **semantics are genuinely better-matched** to "find module boundaries," and the
honest counter-argument is that A's hard balance is the *wrong* shape — FM's tolerance mitigates
but does not eliminate it. Therefore B is the better *eventual* objective, and C is the SOTA
endgame (METIS-style: spectral/multilevel seed + KL/FM refine). The phased plan (§7) builds A in a
way that does not preclude B/C, and gates B on the separate gonum eigensolver work.

**DECIDED 2026-06-08:** Phase 1 commits to **A (Kernighan-Lin)** per the rationale above — pure
Scheme, no new dependency, deterministic seed. Implemented with **KL pair-swaps** (not single-vertex
FM, which deadlocks — impl-plan Q-2); KL holds the seed ratio, so `balance` bounds the seed.
Shi-Malik (B) is deferred to Phase 3 gated on the gonum eigensolver; hybrid (C) is the eventual
target. **Phase 1 is implemented and tested on branch `feat/graph-partition`.**

---

## 6. Proposed API

Generic over the `(wile algebra combinatorial-graph)` weighted graph type (OQ-1). Honest naming —
do **not** repeat the "min-cut" mislabel:

```scheme
;; (wile algebra combinatorial-graph)
;; Partition a weighted undirected graph into two groups minimizing cut weight
;; subject to a balance criterion. Method selects the objective/algorithm.
(graph-partition graph
                 'method 'kernighan-lin       ; | 'normalized-cut  (future, §7 phase 3)
                 'balance 0.25                ; imbalance tolerance |A|-|B|/|V| (KL/FM);
                                              ;   exact range/feasibility per impl-plan Q-1
                 'weight  (lambda (edge-data) ...) ; weight accessor over edge-data
                                              ;   (graph-edges yields (u v edge-data));
                                              ;   default (lambda (_) 1) = unit weight per edge
                 'seed    initial-bipartition) ; optional; default = balanced ⌈n/2⌉ seed
;; =>  alist:  (group-a . (...))  (group-b . (...))
;;             (cut-weight . <number>)        ; severed edge weight (objective value)
;;             (sizes . (NA . NB))            ; raw group sizes (consumer derives any balance metric)
;;             (normalized-cut . <inexact>)   ; cut-weight / total-edge-weight; a COST, lower better;
;;                                            ;   0.0 if no edges. (Phase 3 normalized-cut: Ncut/eigengap)
```

> Field/arity names reconciled with the impl plan during 2026-06-08 review: weight is a 1-arg
> accessor over `edge-data` (not `(u v)`); the result reports `sizes` (was `balance`, which
> collided with the input tolerance) and `normalized-cut` (was `quality`, a misnomer for a cost).

Dispatch on the `'method` parameter (consistent with the project's BLAS-style
parameter-structure dispatch preference; cf. `make-graph-analysis` sub-paths). Lower kernels
(`kl-pass`, `fm-gain-buckets`) remain directly callable for testing. Correctness is monotone in
optimization: a slower exact bisection and the FM heuristic share the cut-weight contract.

**Go fast-path (optional, mirrors `scc.go`/`monotone.go`):** if FM's bucketed gain computation is
a hot path at general-library scale, add `algebra/graph/partition.go` exposing an FFI primitive,
with the pure-Scheme inner loop as fallback when the extension is absent. Not required for the
splitter scale.

### Consumer rewire (wile-goast `split.scm`)

- Add `build-affinity-graph` (§2.1) from the existing filtered signatures.
- Replace `find-split`'s incomparable-pair body with: build affinity graph → seed from best FCA
  incomparable pair → `(graph-partition g 'method 'kernighan-lin 'balance ...)`.
- Map `cut-weight`/`quality` into `compute-confidence`; replace the scale-sensitive 0.15/0.30
  `cut-ratio` bands with a normalized quality threshold.
- **Fix the docstrings** (`split.scm:232,332`) to state the actual algorithm — this also
  discharges option "a" as a side effect.

---

## 7. Phasing

- **Phase 1 (this plan, Wile):** `graph-partition` with `'method 'kernighan-lin` (+ FM tolerance),
  pure Scheme, in `(wile algebra combinatorial-graph)`. Tests + bibliography + docs.
- **Phase 2 (wile-goast):** affinity-graph construction + `find-split` rewire + confidence
  recalibration + docstring fix. (Separate impl plan in the `wile-goast` repo's `plans/`.)
- **Phase 3 (future, gated on gonum eigensolver):** add `'method 'normalized-cut` (Shi-Malik) and
  `'method 'hybrid` (spectral seed → FM refine). Requires the work in
  `plans/2026-04-18-gonum-integration-directions.md`.

---

## 8. Testing

- **KL/FM correctness:** known small graphs with a planted balanced cut (two cliques joined by `k`
  light edges) — assert the planted cut is recovered; assert cut-weight equals the analytic value.
- **Degeneracy guard:** a star graph — assert `graph-partition` does **not** peel the single hub
  (the property that disqualified global min-cut); contrast with a min-cut oracle in the test to
  document the difference.
- **Balance tolerance:** a 60/12 planted split — assert FM-with-tolerance recovers 60/12 and that
  strict-balance KL does *not* (regression-documenting the §5 trade-off).
- **Determinism:** same graph + same seed ⇒ identical partition across runs.
- **Consumer integration (`integration/`):** a real two-concern package fixture ⇒ expected groups
  and a HIGH/MEDIUM confidence; a cohesive package ⇒ COHESIVE.

> **User-authored test (per CLAUDE.md learning-mode convention):** the *core property* test —
> "min-cut isolates the hub but balanced-partition does not" — is the one that validates the entire
> design rationale of §4.4. Scaffold it, leave the assertion as a TODO for the user.

---

## 9. Open questions

- **OQ-1 (home):** `(wile algebra combinatorial-graph)` (recommended, sibling of
  `graph-bipartition`) vs. the user's literal `(wile algebra graph)` (semiring path-analytics).
- **OQ-2 (objective):** ~~confirm the §5 recommendation~~ **RESOLVED 2026-06-08 → KL/FM first.**
- **OQ-3 (edge weight):** IDF-weighted shared-attribute affinity (§2.1) vs. raw co-reference count
  vs. Jaccard on import signatures — affects only the consumer, deferrable to Phase 2.

---

## 10. Bibliography additions (BIBLIOGRAPHY.md — currently has none of these)

- Kernighan & Lin 1970, *An Efficient Heuristic Procedure for Partitioning Graphs* — `combinatorial-graph.scm` (`graph-partition`, KL).
- Fiduccia & Mattheyses 1982, *A Linear-Time Heuristic for Improving Network Partitions* — FM refinement.
- Shi & Malik 2000, *Normalized Cuts and Image Segmentation* — Phase 3 normalized cut.
- Hagen & Kahng 1992, *New Spectral Methods for Ratio Cut Partitioning* — ratio-cut / spectral background.
- Stoer & Wagner 1997, *A Simple Min-Cut Algorithm* — cited in §4.4 as the *rejected* global min-cut.
- (Rejected, cite in §4 rationale only:) Ford & Fulkerson 1956; Dinitz 1970; Karger & Stein 1996.
