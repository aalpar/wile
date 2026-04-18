# gonum Integration — Directions for wile & wile-goast

**Status:** Design-level directions document — no implementation proposed yet

**Scope:** Audit of what gonum (`gonum.org/v1/gonum`) could contribute to wile and wile-goast *today*, grounded in what the two projects already implement. Identifies genuine capability gaps vs. work already covered by `golang.org/x/tools`. Output is a prioritized set of integration directions, not a single-feature plan.

**Relation to existing plans:**
- Complementary to `plans/2026-04-17-algebra-foundations-directions.md`: that plan's §5.1 proposes `(wile algebra matrix)` as a *semiring-parameterized* library (tropical, Boolean, counting). gonum's `mat` package is *field-valued* (ℝ/ℂ) dense/sparse linear algebra. The two have no implementation overlap — semirings lack subtraction and division, so eigendecomposition, inversion, and Gaussian elimination require `gonum/mat` or equivalent, not semiring matrices.
- Independent of the algebra work in practice: the highest-leverage gonum integrations (`gonum/graph/community`, `gonum/graph/topo`, `gonum/graph/network`, `gonum/stat`) have no prerequisite on any algebra-library direction.
- Distinct from `plans/2026-04-16-recurrence-categories-design.md:5` (which flags matrix ops as a blocker for recurrence benchmarks): that need is still a semiring matrix library. gonum does not resolve it.

---

## Part 1 — Already Covered by `golang.org/x/tools`

The key finding that shapes this plan: **`x/tools` already does the language-aware graph construction that the naïve "use gonum for graphs" framing assumes is missing.** gonum's value is downstream of this, not a replacement for it.

### CFG and dominators

- `goastcfg/prim_cfg.go:23` imports `golang.org/x/tools/go/ssa`.
- Dominator trees come free from `ssa.Block.Idom()` (internally Cooper-Harvey-Kennedy, same asymptotic class as Lengauer-Tarjan).
- `goastcfg/prim_cfg.go:289` (`PrimGoCFGDominators`) just extracts the precomputed `idom` field — no custom computation.

**Consequence:** `gonum/graph/flow.DominatorsSLT` would duplicate existing capability. Not a useful integration.

### Callgraph construction

- `goastcg/prim_callgraph.go:22-26` imports `golang.org/x/tools/go/callgraph` with static / cha / rta / vta algorithms.
- These are language-aware: they understand interfaces, method sets, dynamic dispatch. gonum/graph has no notion of Go types.

**Consequence:** gonum cannot *construct* callgraphs. It can only *analyze* callgraphs built elsewhere.

### The architectural split

| Layer | Who owns it |
|---|---|
| Language-aware construction (SSA, callgraph with interface dispatch) | `x/tools` |
| Language-agnostic analytics (SCC, community detection, centrality, all-pairs paths, matrix algebra) | gonum — **currently absent in both projects** |

The right architecture is "build with `x/tools`, analyze with gonum," connected by a thin adapter translating `x/tools/go/callgraph.Graph` → `gonum/graph.Directed`. gonum's graph types are interface-based, so the adapter is straightforward.

---

## Part 2 — Genuine Gaps

Verified absent from both codebases: no references to Louvain, TarjanSCC, FloydWarshall, modularity, betweenness, eigenvector centrality, or PageRank in production code. The only hits are documentation mentions.

### 2.1 Strongly-connected components on the callgraph

**Missing:** cycle detection in the callgraph — mutual recursion, accidentally-cyclic package calls.

**Role:** Natural belief-DSL predicate (`(in-call-cycle? f)`, `(callgraph-sccs)`). Lint use: flag unintended cyclic call clusters that resist testing and reasoning.

**Prerequisite already present:** `go-callgraph` primitive family.

**gonum package:** `gonum/graph/topo.TarjanSCC` — pure Go, `O(V+E)`.

**Estimated effort:** ~50 LOC adapter + 30 LOC primitive wrapper.

### 2.2 Community detection for module-boundary recommendations

**Missing:** graph-theoretic clustering of the callgraph to propose module/package boundaries.

**Role:** wile-goast already has `function-boundary-recommendations` (per `wile-goast/memory/2026-04-10-function-boundary-recommendations-{design,impl}.md`) built on FCA-based clustering. FCA maximizes attribute-extent Galois connections; Louvain maximizes modularity. These are **different objectives**, not competing approximations of the same one.

Running both and comparing is diagnostically valuable: agreement → boundary is algorithm-independent (strong signal); disagreement → boundary is ambiguous and human-judgment-dependent (also useful information).

**gonum package:** `gonum/graph/community` — Louvain algorithm, pure Go.

**Estimated effort:** ~80 LOC adapter + primitive wrapper. Larger if we want weighted edges (call-frequency-weighted modularity).

### 2.3 Centrality measures

**Missing:** principled "function importance" ranking on the callgraph.

**Role:** Replaces ad-hoc heuristics. Specific applications:
- **Betweenness centrality** identifies structural bottlenecks — functions through which many call paths flow. High-betweenness functions are high-risk refactoring sites and good cut points for mocking.
- **Eigenvector centrality / PageRank** identifies functions reached by many important callers. Refactoring-priority and test-prioritization signal.
- **Closeness centrality** identifies functions with short average reach — API surface candidates.

**gonum package:** `gonum/graph/network` — `Betweenness`, `BetweennessWeighted`, `PageRank`, `HITS`, `Closeness`, `Farness`. All pure Go.

**Estimated effort:** ~100 LOC (multiple primitives, but each small).

### 2.4 Batch reachability

**Missing:** all-pairs transitive closure on the callgraph.

**Role:** Belief queries of the form "does any f ∈ F transitively call any g ∈ G?" currently need per-pair traversal. All-pairs Floyd-Warshall computes the full reachability matrix once in `O(V³)`, turning subsequent reachability queries into O(1) table lookups.

**gonum package:** `gonum/graph/path.FloydWarshall`. Pure Go.

**Estimated effort:** ~60 LOC.

**Note:** This is the same computation `plans/2026-04-17-algebra-foundations-directions.md:288` §5.1 would provide via Boolean-semiring matrix closure. gonum delivers it via field-valued shortest-path code. Either path works; the algebra-library approach is more structurally coherent but larger in scope.

### 2.5 Benchmark statistical analysis for wile

**Missing:** rigorous statistics on benchmark runs. Neither `go.mod` includes `gonum` or `benchstat`.

**Role:** wile has ~30 benchmarks (16 Gabriel + Larceny R7RS + Schelog + miniKanren). Current comparison appears ad-hoc. Principled statistics give:

- **Welch's t-test / Mann-Whitney U** for significance testing between runs (catches real regressions, ignores noise)
- **Confidence intervals on geomean** (not just point estimates — know when two runs are statistically indistinguishable)
- **CUSUM or EWMA regression detection** across commits (flag the exact commit that introduces a slowdown)
- **Outlier flagging** (detect GC-stall runs that skew means)

**gonum package:** `gonum/stat` — pure Go, no CGo.

**Estimated effort:** ~100 LOC benchmark-analysis script. Independent of wile's Scheme layer.

---

## Part 3 — Not Applicable Today

Directions where gonum could contribute but has no current consumer:

- **`gonum/mat`** — dense/sparse field-valued linear algebra. Relevant only if spectral work (Laplacian eigenvalues for CFG fingerprinting, spectral clustering) or other matrix-algebra needs enter scope. No present gap is blocked on it.
- **`gonum/optimize`** — numerical optimization. No current use case in either project.
- **`gonum/integrate`, `gonum/diff`, `gonum/interp`** — numerical calculus. No applicable analysis.
- **`gonum/spatial`** — geometric algorithms. No applicable consumer.

Revisit if specific needs emerge — particularly matrix algebra if the spectra question (CFG fingerprinting via Laplacian eigenvalue distance) becomes a funded direction.

---

## Part 4 — Constraints

### Pure Go

Wile's hard constraint is pure Go, no CGo. Verified:

- `gonum/graph`, `gonum/graph/topo`, `gonum/graph/community`, `gonum/graph/network`, `gonum/graph/path` — pure Go.
- `gonum/stat`, `gonum/stat/distuv` — pure Go.
- `gonum/mat` — pure Go by default; optional CGo BLAS via `gonum/blas/netlib` or `gonum/blas/cblas`, which the packages above **do not import**.

gonum's default BLAS is pure Go (`gonum/internal/asm` uses Go assembly for SIMD acceleration, not CGo). The directions in §§2.1–2.5 compile without CGo.

### Dependency surface

gonum is a single module (`gonum.org/v1/gonum`) with no external dependencies beyond the standard library. One `go.mod` entry per project.

### Version stability

gonum is at `v0.15.x` as of this writing, but its API has been stable for years; the `v0` designation reflects project policy, not instability. No known breaking-change risk in the packages identified above.

---

## Part 5 — Prioritized Directions

Ordered by leverage-to-code ratio.

### 5.1 Priority 1 — Graph analytics layer in wile-goast

**Target:** New `wile-goast/goastgraph/` package exposing generic graph analytics over `x/tools/go/callgraph.Graph` (and, where applicable, over the SSA CFG from `goastcfg`).

**Exports (proposed):**

    go-callgraph-scc                     ;; → list of SCCs (each a list of nodes)
    go-callgraph-in-cycle? fn            ;; → #t if fn is in a non-trivial SCC
    go-callgraph-reachability-matrix    ;; → all-pairs reachability table
    go-callgraph-reaches? from to       ;; → #t via precomputed matrix
    go-callgraph-communities [algo]     ;; → list of communities (Louvain default)
    go-callgraph-centrality measure     ;; → alist of (fn . score) for 'betweenness, 'pagerank, 'closeness

**Unlocks:**
- Cyclic-dependency lint (§2.1)
- Second-opinion module boundaries complementing FCA (§2.2)
- Refactoring-priority and test-prioritization metrics (§2.3)
- Batch belief evaluation of reachability queries (§2.4)

**Dependencies:** `x/tools/go/callgraph` (already present), gonum (new).

**Estimated effort:** ~300–500 LOC total across §§2.1–2.4. Adapter is the main shared cost; individual primitives are small.

**Leverage:** **Highest.** Multiple distinct use cases, all ship from one dependency addition, all have concrete consumers in belief DSL and module-decomposition work.

### 5.2 Priority 2 — Benchmark statistical analysis for wile

**Target:** `wile/scripts/bench-stats/` or `wile/internal/benchanalysis/` — a stand-alone Go tool consuming benchmark JSON output.

**Exports (proposed tool flags):**

    bench-stats --significance old.json new.json     ;; Welch's t / Mann-Whitney U
    bench-stats --geomean-ci results.json            ;; confidence intervals on geomean
    bench-stats --regression-detect history/*.json   ;; CUSUM / EWMA across commits
    bench-stats --outliers run.json                  ;; flag suspect runs

**Unlocks:**
- Rigorous "is this change a regression?" answer during PR review
- Historical regression attribution (which commit introduced the slowdown)
- Confidence that geomean differences are real, not noise

**Dependencies:** `gonum/stat`.

**Estimated effort:** ~100–150 LOC. Independent of wile's Scheme layer, no primitives to expose.

**Leverage:** High — small code, immediate daily-workflow impact for a project with 30+ benchmarks.

### 5.3 Lower-priority additions

- **Spatial / geometric algorithms** — no current consumer.
- **Numerical optimization** — no current consumer; could become relevant if Wile gains a constraint-solving extension, but speculative.
- **`gonum/mat` for spectral graph fingerprinting** — see `private/eigenvalues.md` / earlier session discussion. A second integration track, gated on a concrete wile-goast consumer.

---

## Part 6 — The Sharpest Single Win

If only one direction were funded, it would be **§5.1 (graph analytics layer in wile-goast)**. Rationale:

1. **Four distinct capability gaps close with one dependency addition.** SCC, communities, centrality, and all-pairs reachability share the same adapter — the marginal cost of each beyond the first is low.
2. **Existing consumers.** Belief DSL (cycle detection), `function-boundary-recommendations` (community detection as second opinion), refactoring-priority analysis (centrality). Not speculative.
3. **No blocking dependencies.** No algebra-library work required. Can ship now.
4. **Compatible with Wile's constraints.** Pure Go, single `go.mod` entry, no CGo.
5. **Complementary, not competitive, to FCA-based work.** Louvain and FCA optimize different objectives; running both gives a diagnostic signal FCA alone cannot.

Second-sharpest: **§5.2 (benchmark statistics)**. Smaller, more contained, completely independent of §5.1. Could ship in parallel.

---

## Part 7 — Non-Goals

Directions *not* recommended for near-term investment:

- **Replace `x/tools/go/ssa` or `x/tools/go/callgraph` with gonum equivalents.** gonum doesn't know Go semantics; this would be a large regression in analysis quality.
- **Replace the proposed `(wile algebra matrix)` library with `gonum/mat`.** Different algebraic settings (semiring vs. field); no implementation overlap.
- **Expose `gonum/mat` to Scheme via `wile`.** No current consumer. Wile's numeric tower already handles exact rationals and bignums; bridging to `gonum/mat` (float64-only, no exactness) would need careful boundary design and has no demand signal.
- **CGo-accelerated BLAS.** Pure-Go gonum is fast enough for analysis workloads; CGo violates Wile's constraints and gains nothing for the use cases in §5.1–5.2.

---

## Part 8 — Cross-Cutting Principles

Principles to uphold as directions land:

1. **Language-aware construction, language-agnostic analytics.** Keep `x/tools` responsible for understanding Go; keep gonum responsible for understanding graphs. The adapter layer is thin by design.
2. **Prefer second-opinion composition over replacement.** Louvain does not replace FCA; it complements it. Disagreement between two principled algorithms is diagnostic information, not a bug.
3. **Matrix algebra is not one library.** Semiring matrices (algebra-library-directions §5.1) and field matrices (gonum/mat) are distinct. Name them separately when both eventually exist.
4. **Statistics before anecdote.** For a project with ~30 benchmarks, "this looks faster" is not evidence. §5.2 exists to make benchmark claims falsifiable.

---

## Appendix A — Consumer Map

Which wile / wile-goast code would benefit from each proposed direction:

| Direction | Consumer |
|---|---|
| §2.1 Tarjan SCC | Belief DSL cycle predicates; `goastcg/` callgraph lint |
| §2.2 Louvain communities | `function-boundary-recommendations` (second-opinion source); module-decomposition work |
| §2.3 Centrality | Refactoring-priority metrics; test-prioritization; API-surface analysis |
| §2.4 Floyd-Warshall reachability | Belief-DSL batch reachability queries; dataflow precomputation |
| §2.5 gonum/stat | wile's benchmark harness; regression detection in CI |

## Appendix B — Verification Notes

Claims grounded in current code state (2026-04-18):

- `goastcfg/` uses `x/tools/go/ssa`: `goastcfg/prim_cfg.go:23`.
- Dominator extraction is pass-through, not computation: `goastcfg/prim_cfg.go:289–325`.
- `goastcg/` uses `x/tools/go/callgraph` with static/cha/rta/vta: `goastcg/prim_callgraph.go:22–26`.
- Neither project has gonum or benchstat in `go.mod`: verified in `wile/go.mod` and `wile-goast/go.mod`.
- No production references to Louvain, TarjanSCC, FloydWarshall, modularity, betweenness, PageRank, or centrality in either codebase: verified via grep. Only hits are documentation mentions.
- `function-boundary-recommendations` exists in `wile-goast/memory/` as design + impl plan files.

---

## Appendix C — References

Louvain, V. D. Blondel et al. (2008). *Fast unfolding of communities in large networks.* J. Stat. Mech.
Tarjan, R. E. (1972). *Depth-first search and linear graph algorithms.* SIAM J. Comput.
Freeman, L. C. (1977). *A set of measures of centrality based on betweenness.* Sociometry.
Page, L., Brin, S., Motwani, R., Winograd, T. (1999). *The PageRank citation ranking.*
gonum project: `https://pkg.go.dev/gonum.org/v1/gonum`.
`x/tools/go/callgraph`: `https://pkg.go.dev/golang.org/x/tools/go/callgraph`.
