# SCC Condensation for Counting on Cyclic Graphs

**Status:** Implemented on `feat/bigint-counting-monotone-kernel` (PR #757).

**Scope:** New Go kernel `algebra/graph/scc.go` providing strongly-connected-component computation and condensation. New entry point `CountPathsCyclic` that composes condensation with the existing `CountPathsInDAG` monotone kernel. Scheme adapter in `stdlib/lib/wile/algebra/graph.scm` so cyclic counting-semiring queries are routed through condensation automatically instead of returning `nil` or diverging. No change to the public Scheme API — `graph-query` / `graph-query-all` continue to return the same alist shape; the values they carry are now well-defined on cyclic input.

**Repository:** `aalpar/wile`. Files: `algebra/graph/scc.go` (new), `algebra/graph/scc_test.go` (new), `algebra/graph/monotone.go` (extended), `stdlib/lib/wile/algebra/graph.scm` (adapter), `test/wile/algebra-graph-test.scm` (tests).

## Motivation

The monotone kernel (sibling plan `2026-05-24-bignum-allocation-reduction.md`, Sub-path 4A) is DAG-only by design — `CountPathsInDAG` returns `nil` on any cycle reachable from the source (`algebra/graph/monotone.go:107-109`). On real call graphs this is the common case, not the exception: mutual recursion and direct recursion are routine, and the 3-hour incident on the `machine` package (539 nodes, 12 back-edges — `feedback-counting-semiring-on-cycles.md`) showed that even a small number of back-edges makes the question intractable.

The honest framing of the counting semiring on cycles is that *no finite answer exists* — the path set is infinite, so `Σ` over it diverges in `(ℕ, +, ×, 0, 1)`. There are three principled responses:

1. **Reject the question.** k-closedness gate (separate plan, not yet written).
2. **Change the carrier so the answer is bounded.** Approximate-counting semirings — sibling plan `2026-05-24-approximate-counting-semirings.md`.
3. **Change the graph so the question is well-posed.** SCC condensation — *this plan*.

Condensation works because the only reason counting diverges on a cyclic graph is the cycles themselves. Every directed graph's SCC quotient is a DAG (this is the textbook result — Cormen et al. *Introduction to Algorithms* §22.5). Paths *between* SCCs are well-defined and finite; paths *within* an SCC are infinite (or finite-by-self-avoiding, depending on how the user wants to count them — see Open Q-2). Condensation lets us answer the well-defined sub-question exactly and report the within-SCC undefined sub-question honestly.

For wile-goast's call-edge counting use case (`wile-goast/goast/path_algebra_test.go:151-182`), this is the structural fix that makes the existing API usable on real Go call graphs.

## Relationship to existing plans

| Plan | Relationship to this plan |
|------|---|
| `2026-05-24-bignum-allocation-reduction.md` | **Prerequisite.** Ships Sub-path 4A — the `CountPathsInDAG` monotone kernel that this plan composes condensation with. `CountPathsCyclic` is a thin wrapper, not a replacement. |
| `2026-05-24-approximate-counting-semirings.md` | **Alternative, not exclusive.** Approximate carriers and SCC condensation address different layers (algebraic vs. structural — see the five-layer table at `2026-05-24-bignum-allocation-reduction.md:19-25`). A user with a cyclic call graph could use *either* approach. Condensation gives exact counts on the well-defined sub-question; approximate carriers give bounded-precision counts on the original question. |
| `2026-05-24-graph-worklist-bellman-ford.md` | **Orthogonal.** Worklist B-F speeds up convergent queries on any graph (boolean reachability, tropical shortest path). It does not help cyclic counting because counting on cycles never converges. This plan is the missing piece worklist B-F's out-of-scope §:159 points at. |
| `2026-04-18-gonum-integration-directions.md` | **Adjacent.** §2.1 proposes `gonum/graph/topo.TarjanSCC` to unlock four gonum capabilities (SCC, community detection, centrality, all-pairs paths). Same primitive, different motivation: gonum-directions wants SCC for belief-DSL predicates (`(in-call-cycle? f)`, `(callgraph-sccs)`); this plan wants SCC purely for counting-condensation. If the gonum-integration track ships first, this plan should consume gonum's `TarjanSCC` rather than hand-rolling. See Open Q-1. |

## Background — what's already in place

Verified by reading `algebra/graph/monotone.go` and `stdlib/lib/wile/algebra/graph.scm`:

| Component | Status | Notes |
|---|---|---|
| `Edge` struct + adjacency representation | ✓ | `algebra/graph/monotone.go:28-30` — `Edge{U, V int}`. Multi-edges allowed and meaningful (each contributes a distinct path). |
| Iterative DFS with white/gray/black coloring | ✓ | `monotone.go:75-105`. Already detects back-edges; throws away the witness. |
| Topological-order propagation | ✓ | `monotone.go:122-130`. Pattern-3A inner loop. |
| Cycle detection | ✓ | Returns `nil` on first back-edge. Witness discarded. |
| SCC computation | ✗ | This plan. |
| Condensed-graph construction | ✗ | This plan. |
| `CountPathsCyclic` wrapper | ✗ | This plan. |
| Scheme adapter for bigint carrier | ✗ | Pending Phase 4 of `2026-05-24-bignum-allocation-reduction.md`. This plan wires the cyclic branch. |

## Design

### Layer 1 — SCC computation (`algebra/graph/scc.go`)

```go
// SCCResult describes the strongly-connected-component decomposition of a
// directed graph. Components are numbered 0..NumSCCs-1 in reverse
// topological order of the condensation: SCC 0 has no incoming inter-SCC
// edges (a "root" in the condensation); SCC NumSCCs-1 has no outgoing
// inter-SCC edges (a "leaf").
//
// (Reverse topological order is convenient because the caller's source
// node lives in SCC[source], which is then the "root" for path counting
// in the condensed DAG.)
type SCCResult struct {
    // SCC[v] is the component ID of node v. 0 <= SCC[v] < NumSCCs.
    SCC []int

    // NumSCCs is the number of distinct components.
    NumSCCs int

    // NonTrivial[c] is true iff component c contains a cycle (either
    // multiple nodes, or a single node with a self-loop). A trivial
    // SCC is a single node with no self-loop — the within-SCC count
    // is exactly 1 (the empty path). Non-trivial SCCs have infinite
    // within-SCC counts.
    NonTrivial []bool
}

// ComputeSCC computes the strongly-connected components of a directed
// graph. Returns nil if numNodes <= 0 or any edge references an
// out-of-range node.
func ComputeSCC(numNodes int, edges []Edge) *SCCResult { ... }
```

**Algorithm: Pearce's path-based variant** of Tarjan's algorithm (Pearce 2005). One DFS pass, O(V+E), single integer per node for state instead of Tarjan's two (`index` and `lowlink`). Marginally simpler code than classical Tarjan; comparable performance. See Open Q-1 for the choice rationale.

**Iterative DFS, not recursive.** Real call graphs can be deeper than Go's default stack. Mirrors the iterative DFS already used in `CountPathsInDAG` (`monotone.go:75-105`).

**Component IDs in reverse-topological order.** The condensed DAG has the property that for every edge `(c → d)` in the condensation, `c < d` numerically. Lets `CondenseSCC`'s output flow directly into `CountPathsInDAG` (which expects a forward-numbered DAG) without an extra renumbering pass.

### Layer 2 — Condensation (`algebra/graph/scc.go`)

```go
// CondenseSCC reduces a directed graph to its DAG of strongly-connected
// components. Returns:
//   - scc: SCCResult from ComputeSCC (same numbering)
//   - condensed: edges in the condensed graph. For each original edge
//     (u, v) where SCC[u] != SCC[v], emit (SCC[u], SCC[v]).
//
// Self-loops in the condensation (edges where SCC[u] == SCC[v]) are
// dropped — by construction the condensed graph is acyclic.
//
// Multi-edges in the condensation ARE preserved. If two distinct
// original edges (u1, v1) and (u2, v2) both satisfy SCC[u1]==SCC[u2]==c
// and SCC[v1]==SCC[v2]==d (c != d), both emit (c, d) — they
// contribute two distinct inter-SCC paths and the count must reflect
// that.
//
// CondenseSCC returns nil if ComputeSCC does (input validation failure).
func CondenseSCC(numNodes int, edges []Edge) (*SCCResult, []Edge) { ... }
```

**Multi-edge preservation.** This is the subtle correctness point. If the caller's source is in SCC `s` and there are two original edges from SCC `s` to SCC `d`, those are two distinct inter-SCC traversals — the path count from `s` to `d` is 2, not 1. `CondenseSCC` emits both edges; `CountPathsInDAG` will sum them naturally.

Open Q-3 covers whether multi-edge aggregation should instead happen via *weighted* condensed edges (sum the weights at condensation time, emit one edge with weight = count). That would be a different API and a different inner loop. Default chosen here: keep multi-edges, no weights, reuse Sub-path 4A's unit-weight inner loop verbatim.

### Layer 3 — Cyclic counting entry point (`algebra/graph/monotone.go`)

```go
// CyclicCountResult is the result of CountPathsCyclic. Unlike
// CountPathsInDAG which returns []*big.Int indexed by node, this
// returns counts indexed by SCC plus the SCC map so callers can
// project back to per-node answers.
type CyclicCountResult struct {
    // SCC[v] is the component containing node v.
    SCC []int

    // CountsBySCC[c] is the number of distinct paths from
    // SCC[source] to SCC c in the condensed DAG. Defined as:
    //   - For trivial SCCs (NonTrivial[c] == false), the count
    //     gives the exact number of paths from source's SCC to
    //     this SCC in the original graph.
    //   - For non-trivial SCCs (NonTrivial[c] == true), the count
    //     gives the number of distinct *entry points* into the
    //     SCC — paths from source's SCC that reach c via some
    //     entry. Within-SCC paths are infinite and not counted.
    CountsBySCC []*big.Int

    // NonTrivial[c] is true iff SCC c contains a cycle. Callers
    // SHOULD propagate this to users so they understand the
    // semantic shift for those nodes.
    NonTrivial []bool
}

// CountPathsCyclic computes path counts on an arbitrary directed
// graph by SCC-condensing it and running the monotone kernel on the
// resulting DAG. Returns counts per SCC, not per node.
//
// For acyclic input, this is equivalent to CountPathsInDAG but with
// extra overhead (one SCC pass). Callers that know their input is a
// DAG should prefer CountPathsInDAG directly.
func CountPathsCyclic(numNodes int, edges []Edge, source int) *CyclicCountResult { ... }
```

**Layered implementation:**
1. `scc, condensed := CondenseSCC(numNodes, edges)` — O(V+E).
2. `counts := CountPathsInDAG(scc.NumSCCs, condensed, scc.SCC[source])` — guaranteed non-nil because the condensation is acyclic by construction.
3. Pack into `CyclicCountResult`.

The kernel itself (`CountPathsInDAG`) is unchanged. The cyclic case is an *outer adapter* that pre-processes the input until the kernel's precondition (acyclic) is met. This is the right factoring: the kernel stays small and obviously correct; the cyclic glue is testable independently.

### Layer 4 — Cycle witness on `CountPathsInDAG` (optional micro-change)

The DFS in `CountPathsInDAG` already detects back-edges; it just discards the witness. For callers who want to inspect *where* a cycle lives (debugging, error messages) without running the SCC pass, return the witness:

```go
// Existing signature, extended:
func CountPathsInDAG(numNodes int, edges []Edge, source int) ([]*big.Int, *CycleWitness)

type CycleWitness struct {
    BackEdge Edge   // the (u, v) that closed the cycle
    Path     []int  // gray-state stack at detection: [source, ..., u]
                   // (so Path[0] == source, Path[-1] == u)
}
```

On acyclic input the witness is `nil`. The existing test suite needs to be updated to take two return values; the second is ignored where the test only cares about counts.

This is a small breaking change to an internal API. Acceptable on v1.x zero-consumer status; the only call site is the about-to-be-written Scheme adapter. See Open Q-4.

### Layer 5 — Scheme adapter (`stdlib/lib/wile/algebra/graph.scm`)

The branch already routes acyclic graphs through topological order and cyclic graphs through the worklist (`stdlib/lib/wile/algebra/graph.scm:36-71` in the current diff). For bigint-carrier semirings, the dispatch becomes:

```
                          carrier slot on semiring?
                                    │
                  ┌─────────────────┴─────────────────┐
              'big-int                          something else
                  │                                   │
        topological-order-from                Existing Scheme
                  │                          dispatch (boolean,
        ┌─────────┴─────────┐                tropical, etc.)
       DAG                 cyclic
        │                    │
   CountPathsInDAG    CountPathsCyclic
   (Go monotone        (Go: condense
   kernel)             then count)
```

New Scheme primitives:
- `(count-paths-in-dag node-count edges source-idx)` — wraps `CountPathsInDAG`.
- `(count-paths-cyclic node-count edges source-idx)` — wraps `CountPathsCyclic`. Returns three values: counts vector, SCC vector, non-trivial-SCC vector.

The Scheme side does the name↔index mapping (Scheme node identifiers are arbitrary `equal?`-comparable values; the kernel needs integers). The result is projected back to a named alist per the existing `graph-query-all` contract.

**Semantic surface on non-trivial SCCs (Open Q-2).** Default: report the count as the number of distinct entry-points into the SCC, and attach a metadata value (e.g., a symbol `'cyclic-scc-entry-count` or a record) so the caller knows the semantic shift. Alternative considered and rejected: return the symbol `'infinity` or `+inf.0` — looks principled but breaks numeric callers, and "infinity" isn't quite right either (the user often wants the entry-count, not the infinity).

## Open design questions

- **Q-1: SCC algorithm choice.** **Closed: hand-rolled Pearce path-based.** ~100 LOC pure Go, single field per node, O(V+E). Resolved 2026-05-26. Rationale: gonum-integration-directions is "funding-gated roadmap" and untouched — waiting would block indefinitely. Pearce's single-field-per-node design is marginally simpler than classical Tarjan with no algorithmic disadvantage. Implementation kept behind a stable `ComputeSCC` interface so a future gonum swap is internal.

- **Q-2: Within-SCC count semantics.** **Closed: entry-count + metadata flag.** Resolved 2026-05-26. `(graph-query ga node)` where `node` lives in a non-trivial SCC returns the count of distinct paths from source to *some* entry of that SCC, tagged with `'cyclic-scc-entry-count` metadata so callers can detect the semantic shift. Rationale: "how many paths reach this SCC" is almost always what the caller actually wanted; the within-SCC freedom is what makes the question infinite, and that freedom is generally not interesting once the SCC containing the target is identified. Numeric type preserved; downstream numeric processing unaffected.

- **Q-3: Condensed-edge representation.**
  - **(a) Multi-edges preserved.** Each original inter-SCC edge contributes a row in the condensed edge list. Kernel sums them naturally. **Default.**
  - **(b) Weighted edges aggregated.** Sum the per-(c, d) edge count at condensation time, emit one edge with weight = count. Saves work in the kernel inner loop at the cost of needing a weighted variant.

  Default chosen because (a) reuses `CountPathsInDAG` verbatim and the wasted work is bounded by E. If profiling shows the inner loop matters more than condensation, (b) becomes attractive — but that's a follow-up.

- **Q-4: Cycle witness on `CountPathsInDAG`.**
  - **(a) Change the signature** to return `([]*big.Int, *CycleWitness)`. Touches all current callers (tests).
  - **(b) Add a parallel function** `CountPathsInDAGWithWitness` returning the extended result.
  - **(c) Skip it.** The Scheme adapter only needs to know cycle exists / doesn't — it'll run `ComputeSCC` anyway when it does.

  Default: **(c)** for v1; revisit if a debugging use case appears. Saves the signature churn.

- **Q-5: Condensation caching across queries.** `make-graph-analysis` already caches per-source results (`stdlib/lib/wile/algebra/graph.scm` — `lazily compute single-source distances on first query per source`). The SCC decomposition is *source-independent* — same SCC structure for every source. Cache it once at `make-graph-analysis` time?
  - **Default: yes.** Compute SCC once when the analysis is constructed; reuse across all queries. Cheap (O(V+E) once vs. O(V+E) per source), simple to wire (lazy field on the `<graph-analysis>` record).

## Implementation plan

### Phase 0 — fold the existing untracked kernel into a commit

`algebra/graph/monotone.go` and `algebra/graph/monotone_test.go` are currently untracked (visible in `git status`). They are the implementation of Sub-path 4A from `2026-05-24-bignum-allocation-reduction.md`. Commit them on this branch (`feat/bigint-counting-monotone-kernel`) before adding SCC work — keeps the diff for this plan focused on condensation, not on the kernel itself.

### Phase 1 — SCC primitive

- Add `algebra/graph/scc.go` with `ComputeSCC`.
- Pearce path-based algorithm, iterative DFS (matches `CountPathsInDAG`'s shape).
- Add `algebra/graph/scc_test.go` with fixtures:
  - Single node, no edges → 1 trivial SCC.
  - Linear chain → V trivial SCCs in topological order.
  - Single cycle → 1 non-trivial SCC.
  - Two disconnected cycles → 2 non-trivial SCCs.
  - Self-loop on single node → 1 non-trivial SCC (NonTrivial[0] == true).
  - Diamond DAG → 4 trivial SCCs.
  - Bowtie (two cycles joined at one node) → 1 non-trivial SCC containing all nodes.
  - Reverse-topological ordering: assert SCC IDs are consistent (every condensed edge `c → d` satisfies `c < d`).
- Coverage gate: ≥90% line coverage on `scc.go`.

### Phase 2 — Condensation

- Add `CondenseSCC` to `scc.go`. Trivial given `ComputeSCC`.
- Test fixtures:
  - Acyclic input → condensed edge set equals original edges (modulo renumbering).
  - Single cycle → empty condensed edge set.
  - Bowtie → empty condensed edge set (all nodes in one SCC).
  - Multi-edge preservation: two original edges from c to d → two condensed edges from SCC(c) to SCC(d).
  - SCC self-loops dropped: an original edge from a node to itself within an SCC → no condensed edge.

### Phase 3 — Cyclic counting wrapper

- Add `CountPathsCyclic` to `monotone.go`.
- Wire `ComputeSCC` → `CondenseSCC` → `CountPathsInDAG`.
- Add tests covering:
  - Acyclic input → counts match `CountPathsInDAG` per-SCC.
  - Single cycle → all nodes in SCC 0, count 1.
  - Cycle + tail (`A → B → C → A` plus `A → D`) → SCC0 (cycle) entry count 1, SCC containing D entry count 1.
  - The motivating case from the bignum plan's Example 1 (acyclic) → expected counts match.
  - Real-shape mini call graph with mutual recursion and parallel call sites.

### Phase 4 — Scheme adapter

**Phase 4a — FFI surface (shipped):**

- New extension `extensions/algebragraph/` registers `count-paths-in-dag` and `count-paths-cyclic` primitives wrapping the Go kernels. Per-domain extension following the `charsets`/`process`/etc. pattern.
- Wired into `KitchenSink` profile only (no `Console`/`Small` membership per design Q-3 resolution).
- 11 Go-side tests in `prim_count_paths_test.go` + 9 Scheme integration tests in `test/wile/algebragraph-test.scm`.
- Auto-generated `(wile algebragraph)` library exposes the primitives — wile-goast can `import` and call them directly today.

**Phase 4b — `graph.scm` dispatch wiring (deferred):**

- Update `stdlib/lib/wile/algebra/graph.scm`'s dispatch so bigint-carrier semirings route through these primitives transparently. Project the Go result back to the existing alist shape on `graph-query` / `graph-query-all`. Attach `'cyclic-scc-entry-count` metadata on entries that fall in non-trivial SCCs (per Open Q-2 default).
- **Gating dependency:** the `(semiring-carrier S)` accessor and `bigint-counting-semiring` constructor that signal fast-path eligibility live in `memory/2026-05-24-bignum-allocation-reduction.md` Phase 3 (also not yet shipped). Without those, no semiring carries the metadata that would distinguish fast-path-eligible callers from existing pure-Scheme callers. Wiring the dispatch before the carrier slot lands would require either a parallel ad-hoc detection mechanism (technical debt) or a backward-incompatible change to `make-graph-analysis` (worse).
- Soft-dispatch mechanism: at library load time, probe `(guard (e (#t #f)) (begin (count-paths-cyclic 1 '() 0) #t))` to detect whether the algebragraph extension is loaded. Combined with the carrier check, this lets the library gracefully fall through to Scheme when either piece is missing.
- Tests in `test/wile/algebra-graph-test.scm` (new):
  - All existing tests pass unchanged.
  - New tests for cyclic counting with the bigint-counting-semiring.
  - Metadata propagation test (the entry for a node in a non-trivial SCC carries the flag).

### Phase 5 — wile-goast smoke test

- Run wile-goast's path-algebra counting query on the `machine` package call graph (the 3-hour incident workload).
- Expected: terminates in well under a second; returns finite counts for all nodes; nodes in non-trivial SCCs flagged.
- Compare against the (failed) baseline from the incident memory.

### Phase 6 — docs + PR

- Update `stdlib/lib/wile/algebra/CLAUDE.md` to document the cyclic-input semantics.
- Add a section to `docs/algebra/` (or wherever the algebra docs live — verify during implementation) explaining the condensation choice.
- Cross-link from the three sibling plans:
  - `2026-05-24-bignum-allocation-reduction.md` line 296 — update "future plan" reference to point here.
  - `2026-05-24-approximate-counting-semirings.md` line 286 — same.
  - `2026-05-24-graph-worklist-bellman-ford.md` line 159 — same.
- Open PR, dual review (Copilot + `/crosscheck`).

## Risks

- **R-1 — Within-SCC semantics confuse users.** The default (Open Q-2 (a): entry-count + metadata) is honest but subtle. Users who expect "number of paths from main to f" and get "number of entry points to the SCC containing f" may be surprised. Mitigation: clear docstring on `graph-query` / `graph-query-all`, explicit example in `docs/algebra/`, propagate the metadata visibly through `graph-analysis-fast-path-kind` (or equivalent introspection).
- **R-2 — Pearce vs Tarjan correctness divergence.** Pearce's path-based variant is correct (proven in the 2005 paper) but less widely-implemented than classical Tarjan. Mitigation: test against a Tarjan reference implementation on randomized fixtures during Phase 1; pin behavior with comprehensive unit tests.
- **R-3 — Multi-edge preservation hides cost.** Per Open Q-3, multi-edges in the condensed DAG are preserved verbatim — the kernel sees them as parallel edges. On dense graphs with many inter-SCC edges this can blow up the condensed edge count. Mitigation: if the condensed edge count substantially exceeds the original (shouldn't happen except in pathological inputs), Q-3 (b) becomes attractive. Measure during Phase 5.
- **R-4 — gonum dependency drift.** If `2026-04-18-gonum-integration-directions.md` ships during this work, we should reuse gonum's `TarjanSCC` rather than hand-rolled Pearce. Mitigation: keep the SCC implementation behind a stable `ComputeSCC` signature so the swap is internal.
- **R-5 — Source-mapping correctness.** The caller passes an integer source; we map it to `SCC[source]` and run the kernel on the condensed graph. The result must be projected back to the original node space. Off-by-one or mis-renumbering is a real risk. Mitigation: thorough Phase 3 test coverage on small fixtures where the answer can be verified by hand.

## Acceptance criteria

- All existing `(wile algebra graph)` tests pass unchanged.
- New Go tests for `ComputeSCC` and `CondenseSCC` achieve ≥90% line coverage on `algebra/graph/scc.go`.
- New Scheme tests for cyclic counting via `bigint-counting-semiring` pass.
- wile-goast `path-query-all` on the 539-node `machine` package terminates in under 1 second (vs. the 3-hour hang baseline). Results are well-defined and inspect-able.
- `make lint && make covercheck && make ci` all green.

## Out of scope

- **Approximate counting semirings** (sibling plan `2026-05-24-approximate-counting-semirings.md`). Complementary, not exclusive. A user with a cyclic graph can use either condensation (exact counts, entry-point semantics on non-trivial SCCs) or approximate carriers (bounded-precision counts, original semantics).
- **Worklist Bellman-Ford** (sibling plan `2026-05-24-graph-worklist-bellman-ford.md`). Orthogonal — speeds up convergent queries; does not help cyclic counting.
- **k-closedness gate** (separate plan, not yet written). A stricter posture would reject `make-graph-analysis` on cyclic input with a non-k-closed semiring, rather than silently condensing. The two are compatible — the gate could be opt-in (`(strict-cyclicity-check . #t)` on `<graph-analysis>`) and condensation would be the default.
- **gonum-based SCC** (covered in `2026-04-18-gonum-integration-directions.md` §2.1). If gonum integration ships independently, `ComputeSCC` becomes a thin wrapper around `gonum/graph/topo.TarjanSCC` — mechanical refactor, not a redesign.
- **General-purpose graph utility library.** This plan adds *only* the primitives needed for condensation-based counting. SCC predicates for the belief DSL, Louvain community detection, and centrality measures are explicitly the scope of `2026-04-18-gonum-integration-directions.md`, not this plan.
- **Carrier abstraction for non-bigint semirings.** This plan ships condensation that composes with the bigint monotone kernel. Per `2026-05-24-bignum-allocation-reduction.md`'s plan §"What 4A actually delivers", future Σ-semiring carriers (modular, log-float, saturating) get their own kernels. Condensation is carrier-agnostic — when a second kernel exists, `CountPathsCyclic` will be generalized to dispatch on carrier.

## References

- Tarjan, R. E. (1972). *Depth-first search and linear graph algorithms.* SIAM J. Comput. 1(2): 146-160.
- Pearce, D. J. (2005). *An Improved Algorithm for Finding the Strongly Connected Components of a Directed Graph.* Technical report, Victoria University of Wellington.
- Cormen et al., *Introduction to Algorithms*, 3rd ed., §22.5 — SCC and condensation; the textbook treatment of the DAG-of-SCCs result.
- `memory/2026-05-24-bignum-allocation-reduction.md` — ships the monotone DAG kernel this plan composes with.
- `memory/2026-05-24-approximate-counting-semirings.md` — alternative response to the cyclic-counting problem at the algebraic layer.
- `plans/2026-05-24-graph-worklist-bellman-ford.md` — orthogonal convergence-detection work for non-counting carriers.
- `plans/2026-04-18-gonum-integration-directions.md` — adjacent SCC use case (belief DSL) and possible future implementation source.
- `feedback-counting-semiring-on-cycles.md` — incident memory (3-hour hang on `machine` package counting query).
