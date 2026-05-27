# Graph Cyclic-Counting Dispatch — Scheme-Side Polish

**Status:** Draft. Implementation not yet started.

**Scope:** Wire the deferred Phase 4b of `2026-05-26-scc-condensation.md`. The `count-paths-cyclic` Go kernel and the `count-paths-in-dag` kernel both ship today; the bigint-carrier dispatch in `stdlib/lib/wile/algebra/graph.scm` routes acyclic input to the latter but **errors out** on cyclic input. This plan replaces that error path with dispatch to the cyclic kernel, exposes the SCC decomposition through a small side-query API, and adds the in-degree primitives that call-graph consumers (notably wile-goast) need but `(wile algebra combinatorial-graph)` does not yet export.

**Repository:** `aalpar/wile`. Cross-repo consumer switch in `aalpar/wile-goast`.

**Files touched (estimated):**

| File | Change |
|------|--------|
| `stdlib/lib/wile/algebra/graph.scm` | New `compute-via-count-paths-cyclic`; dispatch rewire; new public API; SCC cache slot. |
| `stdlib/lib/wile/algebra/graph.sld` | Export the new public API. |
| `stdlib/lib/wile/algebra/combinatorial-graph.scm` | New `graph-in-degree`, `graph-predecessors`, `graph-reverse`. |
| `stdlib/lib/wile/algebra/combinatorial-graph.sld` | Export the new primitives. |
| `test/wile/algebra-graph-test.scm` | Cyclic-counting + SCC-cache + side-query coverage. |
| `test/wile/algebra-combinatorial-graph-test.scm` | In-degree / reverse coverage. |
| `wile-goast/goast/path_algebra_test.go` | Switch to `bigint-counting-semiring`; add a cyclic fixture. **Cross-project — separate PR in wile-goast.** |

## Motivation

After PRs #757 (SCC condensation kernel) and #758 (bigint Phase 3 — carrier accessor + `bigint-counting-semiring`), the Scheme adapter is the missing link. Today:

```
(make-graph-analysis (bigint-counting-semiring) cyclic-adj #f)
  → analysis with fast-path-kind = 'unit-weight-counting
(graph-query-all ga source)
  → error: "graph-query: bigint-counting-semiring on a cyclic graph ..."
```

The error message points the user at `(import (wile algebragraph))` and direct use of `(count-paths-cyclic ...)`. That works, but it leaks the kernel through the abstraction the algebra layer was built to provide. A user who chose `bigint-counting-semiring` for the carrier-opt fast path now has to reach below the abstraction to handle the cycle case — exactly the boundary the SCC condensation plan was meant to eliminate.

The wile-goast case is concrete: `wile-goast/goast/path_algebra_test.go:151-182` constructs a counting-semiring `path-analysis` over a Go call graph. Mutual recursion is routine in real packages (the 539-node `machine` package had 12 back-edges, per `memory/feedback-counting-semiring-on-cycles.md`); without this dispatch, the test only works on artificially-acyclic fixtures.

## Relationship to existing plans

| Plan | Relationship |
|------|---|
| `2026-05-26-scc-condensation.md` | **Parent.** This plan implements its Phase 4b verbatim. Open Q-2 (within-SCC semantics) and Open Q-5 (SCC caching) are both resolved here. |
| `2026-05-24-bignum-allocation-reduction.md` | Prerequisite. Phase 3 (carrier accessor + `bigint-counting-semiring`) shipped in PR #758, unblocking this work. |
| `2026-05-24-approximate-counting-semirings.md` | Orthogonal. Approximate carriers (saturating / modular / log) converge under Bellman-Ford and don't need this dispatch. |
| `WORKSPACE-ROADMAP.md` | Tracking. Cross-project switch (item 8 below) goes through the wile-goast queue. |

## Background — what's already in place

Verified by reading the files cited:

| Component | Status | Notes |
|---|---|---|
| `algebra/graph/scc.go` — `ComputeSCC` + `CondenseSCC` | ✓ | Pearce path-based, reverse-topological SCC numbering. |
| `algebra/graph/monotone.go` — `CountPathsCyclic` | ✓ | Returns `CyclicCountResult{SCC, CountsBySCC, NonTrivial}`. |
| Scheme primitive `count-paths-cyclic` | ✓ | `extensions/algebragraph/register.go:54`. KitchenSink-only. Returns three vectors: `(scc-vec counts-by-scc-vec non-trivial-vec)`. |
| Scheme primitive `count-paths-in-dag` | ✓ | `extensions/algebragraph/register.go:44`. Returns vector of counts or `#f` on cycle. |
| `bigint-counting-semiring` constructor + `(semiring-carrier S)` accessor | ✓ | `stdlib/lib/wile/algebra/semiring.scm:100`. |
| `semiring-cycle-safe?` predicate | ✓ | Commit `93182b96`. |
| `make-graph-analysis` carrier dispatch (acyclic path) | ✓ | `graph.scm:68-79` attaches `'unit-weight-counting` fast-path-kind. |
| Worklist 2·V·E safety cap | ✓ | `graph.scm:372` — fires loudly on non-convergent semirings. |
| Cycle-detection in `topological-order-from` | ✓ | Returns `(values order cyclic?)` — `graph.scm:253-298`. |
| Cyclic dispatch (`'big-int` carrier + cyclic input) | ✗ | **This plan.** Currently errors at `graph.scm:204-219`. |
| `<graph-analysis>` SCC cache slot | ✗ | This plan. Source-independent. |
| `graph-node-in-cycle?` / `graph-cyclic-nodes` side-query API | ✗ | This plan. |
| `graph-in-degree` / `graph-predecessors` / `graph-reverse` | ✗ | This plan. `combinatorial-graph.scm:197` exports out-degree only. |

## Design

### Layer 1 — SCC cache slot on `<graph-analysis>`

Add a mutable slot to the record (currently 5 fields at `graph.scm:20-27`):

```scheme
(define-record-type <graph-analysis>
  (make-graph-analysis* semiring adjacency weight-fn cache fast-path-kind scc)
  graph-analysis?
  (semiring         ga-semiring)
  (adjacency        ga-adjacency)
  (weight-fn        ga-weight-fn)
  (cache            ga-cache       set-ga-cache!)
  (fast-path-kind   ga-fast-path-kind)
  (scc              ga-scc         set-ga-scc!))  ;; NEW: #f | <scc-record>
```

The `scc` slot stores a freshly-defined `<graph-scc>` record bundling the three vectors returned by `count-paths-cyclic`:

```scheme
(define-record-type <graph-scc>
  (make-graph-scc* scc-vec non-trivial-vec name->idx idx->name)
  graph-scc?
  (scc-vec          gscc-scc-vec)         ;; node-idx → scc-id
  (non-trivial-vec  gscc-non-trivial-vec) ;; scc-id   → bool
  (name->idx        gscc-name->idx)       ;; hashtable
  (idx->name        gscc-idx->name))      ;; vector
```

The name/index maps are shared between the cyclic and (when this lands) DAG kernels — they're the same interning state. Computing SCC once at the first cyclic query saves the interning re-pass on every subsequent source. Q-5 in the parent plan defaults to this.

### Layer 2 — `compute-via-count-paths-cyclic` adapter

Mirror `compute-via-count-paths-in-dag` (`graph.scm:147-241`). The structural difference is the 3-value return:

```
                     count-paths-cyclic
                              │
            ┌─────────────────┼─────────────────┐
            ▼                 ▼                 ▼
        scc-vec        counts-by-scc       non-trivial-vec
       (node→scc)      (scc→bigint)        (scc→bool)
            │                 │                 │
            └─────────────────┼─────────────────┘
                              ▼
                project to alist:
                  for each interned name n at idx i:
                    let s = scc-vec[i]
                    let c = counts-by-scc[s]
                    if c > 0: emit (n . c)
```

The non-trivial vector is not consumed at projection time — it's stashed in the `<graph-scc>` record for `graph-node-in-cycle?` to consult later. (Open Q-2 is resolved in favour of "side query"; the alist is shape-stable.)

Caching strategy:
- On first cyclic query against `ga`, run the interning pass + kernel call + projection, populate `ga-scc`.
- On subsequent cyclic queries for *different* sources against the same `ga`, the interning hashtable and SCC vector are unchanged. Only re-run the projection step, skipping the kernel — the per-source count alist is already trivially derivable from the cached `scc-vec` + a per-source `counts-by-scc`.

Wait: `counts-by-scc` *is* source-dependent (paths from source's SCC to each other SCC). The SCC decomposition itself isn't. So the cache splits:
- `ga-scc` (source-independent): SCC structure, name interning, non-trivial flags.
- `ga-cache` (per-source, already exists): the alist returned to the caller.

The kernel call has to re-run per source, because `count-paths-cyclic` takes a source index. That's fine — the cost was always going to be O(V + E) per source. The cache wins live on (a) skipping the interning re-walk and (b) sharing the SCC vector with `graph-node-in-cycle?`.

### Layer 3 — Dispatch rewire in `compute-single-source`

Current logic at `graph.scm:108-118`:

```scheme
(case (ga-fast-path-kind ga)
  ((unit-weight-counting)
   (compute-via-count-paths-in-dag ga source))
  (else
   (call-with-values
     (lambda () (topological-order-from ga source))
     (lambda (order cyclic?)
       (if cyclic?
           (compute-via-worklist ga source)
           (compute-via-topological-order ga source order))))))
```

The `'unit-weight-counting` branch currently calls `count-paths-in-dag` and lets the kernel decide via `#f` return whether the input is cyclic — at which point `compute-via-count-paths-in-dag` raises. The new structure pre-detects via `topological-order-from` (which is already free in the non-fast-path branch) and dispatches up-front:

```scheme
(case (ga-fast-path-kind ga)
  ((unit-weight-counting)
   ;; Pre-detect cyclicity to avoid the kernel's #f-return path.
   ;; topological-order-from already does this work for the non-fast-path
   ;; branch; reusing it here is symmetric.
   (call-with-values
     (lambda () (topological-order-from ga source))
     (lambda (_order cyclic?)
       (if cyclic?
           (compute-via-count-paths-cyclic ga source)
           (compute-via-count-paths-in-dag ga source)))))
  (else
   (call-with-values
     (lambda () (topological-order-from ga source))
     (lambda (order cyclic?)
       (if cyclic?
           (compute-via-worklist ga source)
           (compute-via-topological-order ga source order))))))
```

Cost: one extra `topological-order-from` per first-cyclic-query against a `ga`. After the first call, the SCC cache on `ga-scc` lets us short-circuit — if `ga-scc` is populated, we know the graph is cyclic and skip the topological pre-detect. Optimization deferred to follow-up unless benchmarks show it matters; `topological-order-from` is O(V+E) and the kernel is the same complexity, so the constant factor is bounded.

The error site at `graph.scm:204-219` (inside `compute-via-count-paths-in-dag` when the kernel returns `#f`) is **kept as a defensive invariant violation**: with pre-detection, the kernel should never see a cyclic input from this dispatch path. If it does, that's a bug — the existing error message becomes "internal invariant violation, please file a bug" wording, parallel to the existing `else` branch at `graph.scm:235-241`.

### Layer 4 — Public API additions in `graph.scm`

Three new exports, all read-through to the cached `<graph-scc>`:

```scheme
(define (graph-analysis-sccs ga)
  "Force computation of the SCC decomposition for GA and return the
<graph-scc> record. Used internally by graph-node-in-cycle? and
graph-cyclic-nodes; exposed for callers who want direct SCC introspection.
For analyses constructed over carriers that don't trigger the cyclic
fast path, this still computes SCCs on demand using the same kernel."
  ...)

(define (graph-node-in-cycle? ga node)
  "Return #t iff NODE lies in a non-trivial SCC of GA's adjacency.
Forces SCC computation on first call per GA. Returns #f for nodes
not in GA's adjacency (consistent with graph-query's permissive
out-of-graph semantics)."
  ...)

(define (graph-cyclic-nodes ga)
  "Return the list of node names that lie in non-trivial SCCs of GA's
adjacency. Same as filtering (graph-vertices) through
graph-node-in-cycle? but reads the cached SCC vector directly."
  ...)
```

`graph-analysis-sccs` doesn't take a source argument because the SCC decomposition is source-independent. Callers that need per-source path counts use `graph-query` / `graph-query-all`; callers that need cycle membership use this.

### Layer 5 — In-degree and reverse adjacency in `combinatorial-graph.scm`

Three new exports, structural — no semiring involvement:

```scheme
(define (graph-in-degree G v) ...)            ;; number of edges (_, v)
(define (graph-predecessors G v) ...)         ;; ((u . edge-data) ...) for edges (u, v)
(define (graph-reverse G) ...)                ;; new <graph> with edges reversed
```

`graph-reverse` is the structural primitive; `graph-in-degree` and `graph-predecessors` are convenience accessors on top of it. Implementation:

- `graph-reverse` walks `(graph-edges G)` once, builds an inverted adjacency, returns a fresh `<graph>` with the same directed?/multi?/self-loops?/setoid as the original.
- `graph-in-degree` and `graph-predecessors` lazily call `graph-reverse` and cache the result on a new `<graph>` slot (`reverse-cached`), parallel to how the analysis layer caches SCC.

This is intentionally one extra slot on `<graph>`, not a separate `<graph-with-reverse>` record. Reverse adjacency is a structural property of the graph, not of a particular analysis.

## Open design questions

**Q-1 — Where does `graph-reverse` build its inverted adjacency?**

  - **(a) Eagerly at `graph-reverse` time** — same shape as original, costs O(V+E) once. **Default.**
  - **(b) Lazily, edge-by-edge as `graph-predecessors` is called** — only pays for vertices actually queried. Saves work in sparse-query patterns but complicates the cache invalidation story.

  Default chosen because `<graph>` is immutable; once the reverse is built it's stable for the lifetime of the value. Per-vertex lazy is premature optimization.

**Q-2 — `graph-reverse` on undirected graphs?**

  - **(a) Return `G` unchanged** (undirected = its own reverse).
  - **(b) Error.** Undirected callers shouldn't be asking.

  Default: **(a)**. Cheaper for generic callers that don't want to branch on `(graph-directed? G)`. Documents the no-op explicitly in the docstring.

**Q-3 — Cache invalidation for SCC slot.**

The `<graph-analysis>` record is constructed from an immutable adjacency alist (passed by value to `make-graph-analysis`). The SCC cache is therefore safe to populate-and-keep forever; no invalidation path is needed. Same for the `<graph>` reverse cache. No design question, surfaced here to document the assumption.

## Implementation plan

### Phase 1 — In-degree / reverse adjacency

Independent of the SCC work; can ship first to unblock wile-goast's structural queries.

- Add `graph-in-degree`, `graph-predecessors`, `graph-reverse` to `combinatorial-graph.scm`.
- Extend `<graph>` with a `reverse-cached` slot (`#f` initially, populated lazily).
- Tests in `test/wile/algebra-combinatorial-graph-test.scm`:
  - In-degree on `K_3` (every vertex = 2 in undirected, 2 in directed-symmetric).
  - In-degree on a directed chain `a → b → c` (a:0, b:1, c:1).
  - Predecessors on a fan-in graph (multiple edges into one vertex).
  - Self-loop counts once in in-degree (consistent with `graph-degree`'s loop convention).
  - `graph-reverse` of a directed chain yields the reverse chain.
  - `graph-reverse` of an undirected graph returns `G` (Q-2 default).
  - Cache: two consecutive `graph-in-degree` calls don't re-run `graph-reverse` (test by mutating a sentinel inside a stub `graph-reverse` and asserting one call).

### Phase 2 — SCC cache + `compute-via-count-paths-cyclic`

- Extend `<graph-analysis>` with `scc` slot.
- Add `<graph-scc>` record.
- Implement `compute-via-count-paths-cyclic` paralleling `compute-via-count-paths-in-dag`.
- Cache population: first cyclic query allocates `<graph-scc>` and stores on `ga-scc`; subsequent cyclic queries reuse interning maps + SCC vector but re-run the kernel for the new source's `counts-by-scc`.

### Phase 3 — Dispatch rewire + public API

- Rewrite `compute-single-source` to pre-detect cyclicity for `'unit-weight-counting` and route accordingly.
- Recast the kernel-`#f`-return error site as an internal-invariant message.
- Add `graph-analysis-sccs`, `graph-node-in-cycle?`, `graph-cyclic-nodes`.
- Export from `graph.sld`.
- Update `graph-analysis-fast-path-kind` docstring to mention `'cyclic-counting-via-scc` (or unify under `'unit-weight-counting` if the kind symbol is meant to describe the carrier opt-in, not the per-query dispatch).

### Phase 4 — Tests

- Cyclic counting via `bigint-counting-semiring`:
  - Single self-loop: source's SCC count = 1.
  - Bowtie (two cycles sharing a vertex): one non-trivial SCC; all nodes flagged.
  - Cycle + tail (`A → B → C → A` plus `A → D`): two SCCs; D's count = 1.
  - The motivating mutual-recursion fixture (small simulated call graph: `f → g → f` plus `f → h`).
- Side query:
  - `graph-node-in-cycle?` returns `#t`/`#f` consistent with the SCC fixture.
  - `graph-cyclic-nodes` returns the expected node set.
- Cache:
  - Two consecutive queries against the same source share the per-source alist (already covered by existing cache tests, re-assert under cyclic-counting).
  - Two queries against *different* sources share the same `<graph-scc>` record (`eq?` assertion on `(graph-analysis-sccs ga)`).
- Defensive invariant:
  - Synthetic test: directly call `compute-via-count-paths-in-dag` with a cyclic graph (bypassing dispatch). Assert the rewritten "internal invariant violation" error fires.

### Phase 5 — wile-goast smoke + switch (cross-project)

Tracked through `WORKSPACE-ROADMAP.md`. Separate PR in `aalpar/wile-goast`:

- Switch `wile-goast/goast/path_algebra_test.go:151-182` from `(counting-semiring)` to `(bigint-counting-semiring)` — verifies the opt-in pathway.
- Add a mutual-recursion fixture (~10 nodes, 2 back-edges) and assert (a) termination, (b) finite counts on all nodes, (c) `graph-cyclic-nodes` returns the expected SCC members.
- Optionally: run the path query against the `machine` package call graph (the original 3-hour-hang incident workload from `memory/feedback-counting-semiring-on-cycles.md`) and assert sub-second termination. This is the acceptance test for "the polish actually unblocks the consumer."

### Phase 6 — Docs + PR

- Update `stdlib/lib/wile/algebra/CLAUDE.md` carrier-opt table to add `'cyclic-counting-via-scc` if Phase 3 adopts the separate kind, or to note that `'unit-weight-counting` now covers both cases.
- Add a paragraph to `docs/algebra/reference.md` explaining the side-query API and pointing at `graph-cyclic-nodes` for the cycle-distinction use case.
- Cross-link from `2026-05-26-scc-condensation.md` Phase 4b — flip its status from "deferred" to "shipped, see this plan."
- Open PR, dual review (Copilot + `/crosscheck`).

## Risks

- **R-1 — Pre-detect double-work on hot dispatch.** Adding `topological-order-from` to the bigint fast-path adds an O(V+E) pre-pass on every query (not just the first). On large analyses with many sources, this could be measurable. Mitigation: benchmark in Phase 4 on a 1000-node fixture; if the regression exceeds 5%, cache the `cyclic?` flag on `<graph-analysis>` (source-independent like SCC) and skip the pre-pass after the first source.
- **R-2 — SCC cache for non-fast-path semirings.** `graph-analysis-sccs` lazily computes SCC even when the analysis was constructed over a non-bigint carrier (boolean, tropical) where the existing dispatch never needs it. The cost is bounded — one O(V+E) call when the caller asks — but it pulls in the `(wile algebragraph)` extension dependency even for boolean callers. Mitigation: gate `graph-analysis-sccs` on `%fast-path-available?` and raise a clear error pointing at the KitchenSink profile when the extension isn't loaded.
- **R-3 — In-degree slot widening on `<graph>`.** Adding the `reverse-cached` field changes the record-type definition. Old `make-graph` callers go through a wrapper that fills `#f` for the new slot, so it's not a public-API break, but the record-type printer output changes. Acceptable on v1.x zero-consumer status.
- **R-4 — Side-query API naming collision.** `graph-node-in-cycle?` reads close to `graph-bipartite?` (already in `combinatorial-graph.scm:476`) and `graph-connected-components` — both whole-graph predicates. The per-node nature of `graph-node-in-cycle?` should be clear from the second argument, but if naming feedback says otherwise, alternatives: `graph-node-cyclic?`, `cyclic-scc-member?`. Decide on PR review.
- **R-5 — Cross-repo sequencing.** wile-goast's switch can't merge until this PR's KitchenSink dispatch is on `master`. Standard `WORKSPACE-ROADMAP.md` discipline: land wile-side first, open wile-goast PR consuming it, merge after `go.work` re-pin.

## Acceptance criteria

- All existing `(wile algebra graph)` and `(wile algebra combinatorial-graph)` tests pass unchanged.
- New cyclic-counting tests pass against `bigint-counting-semiring`.
- The existing error message at `graph.scm:204-219` is no longer reachable through `graph-query` / `graph-query-all` on cyclic input with `'big-int` carrier.
- `graph-node-in-cycle?` and `graph-cyclic-nodes` agree with hand-computed SCCs on the bowtie / diamond+self-loop / mutual-recursion fixtures.
- `make lint && make covercheck && make ci` all green.
- (Cross-project) wile-goast `path_algebra_test.go` runs against `bigint-counting-semiring` on a cyclic call-graph fixture and terminates with finite counts.

## Out of scope

- **Weighted bignum dispatch** (sub-path 4B of `2026-05-24-bignum-allocation-reduction.md`). Big-int + non-`#f` weight-fn still falls through to the generic Scheme inner loop.
- **Approximate carriers on cycles.** `saturating-counting-semiring` already converges under Bellman-Ford because its CAP is absorbing. No SCC dispatch needed; covered by `2026-05-24-approximate-counting-semirings.md`.
- **Modular / log carriers on cycles.** Each would need its own Go kernel; the cyclic-counting dispatch here is unit-weight-bigint-specific by design.
- **`count-paths-cyclic` exposure to non-KitchenSink profiles.** The extension stays opt-in; `make-graph-analysis` falls back to the worklist (which then hits the 2·V·E cap) when the kernel isn't loaded.
- **Reverse-graph SCC.** SCCs are direction-sensitive (a strongly-connected component in `G` is generally not one in `reverse(G)` — wait, actually it *is*, because strong-connectivity is symmetric in direction). Implication: `(graph-cyclic-nodes (graph-reverse G))` equals `(graph-cyclic-nodes G)` set-theoretically. Not interesting enough to surface specially; documented as a property in tests but no separate API.

## References

- `plans/2026-05-26-scc-condensation.md` — parent plan; Phase 4b deferred there is implemented here.
- `plans/2026-05-24-bignum-allocation-reduction.md` — Phase 3 (shipped) provides the carrier accessor and `bigint-counting-semiring` constructor this dispatch keys on.
- `plans/2026-05-24-approximate-counting-semirings.md` — alternative response at the algebraic layer (orthogonal to this work).
- `plans/WORKSPACE-ROADMAP.md` — cross-project sequencing for the wile-goast switch.
- `memory/feedback-counting-semiring-on-cycles.md` — the original incident motivating SCC condensation.
- `memory/feedback-blas-style-dispatch.md` — BLAS-style dispatch convention (correctness monotone in optimization) — this plan follows the same pattern.
- Cormen et al., *Introduction to Algorithms*, 3rd ed., §22.5 — SCC and condensation; the textbook treatment of the DAG-of-SCCs result.
- Pearce, D. J. (2005). *An Improved Algorithm for Finding the Strongly Connected Components of a Directed Graph.* Victoria University of Wellington — the SCC algorithm in `algebra/graph/scc.go`.
