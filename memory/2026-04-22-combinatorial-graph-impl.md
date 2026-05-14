# §5.6 Combinatorial Graph — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

> **Status:** **Ready to execute.** All design decisions resolved (user confirmation 2026-04-22): Q1 = scope C (full §5.6 — iso + spanning-tree + chromatic + Tutte + bipartite matching), Q3 = full backtracking iso (individualization-refinement, nauty-lite; guaranteed yes/no, no `'unknown` return).

---

## Inherited design context (must read before Phase 1)

Four prior-work streams constrain this plan before its own Q&A begins. Do not re-open these:

**From the master directions doc (`plans/2026-04-17-algebra-foundations-directions.md` §5.6 + Part 8):**

- **Library name is new, not a rename.** `(wile algebra graph)` (semiring-Bellman-Ford) stays. §5.6 ships as `(wile algebra combinatorial-graph)` alongside it. Part 8 cross-cutting principle #4: "Don't rename existing libraries." The naming asymmetry is a deliberate cost paid for zero consumer breakage.
- **Named consumers drive scope.** Every v1 export must map to an identified wile-goast construction currently handling the concept ad-hoc: `unify.scm` (CFG-level clone detection → graph iso), register allocation (chromatic polynomial), scheduling (matching). §5.7 lower-priority items (matroid intersection for scheduling, Connes–Kreimer for AST) are **not** v1 consumers; no speculative API surface.
- **Effort envelope: 600–800 LOC.** Foundation doc §5.6 estimate. Compare against shipped siblings: `lattice.scm` 841, `matrix.scm` 1302, `group.scm` 604, `unification.scm` 633. Target lands mid-pack. Over-budget = scope-cut, not scope-absorb.

**From §5.4 (`plans/2026-04-22-group-actions-burnside-impl.md` Revision Note):**

- **Setoid-carried vertex equality.** `<graph>` carries a `setoid` field defaulting to `default-setoid`. Convenience `(graph-vertex-equiv? G u v)` delegates to `(setoid-equiv? (graph-setoid G))`. Vertex identity is setoid-defined, not `equal?`-derived; this matters for iso (color refinement uses vertex equality to dedupe signatures) and for consumers who model vertices as structural objects.
- **Finiteness tiers.** Three tiers: tier-1 (vertices + edges enumerated — required for iso, chromatic, Tutte, spanning-tree count, matching), tier-2 (finitely-generated-via-seed-vertex + neighbor function — BFS closure; same lazy-enumeration pattern as §5.4's BFS-from-generators), tier-3 (opaque, adjacency-lookup only — works with `graph-neighbors`, `graph-edge?`, but not with any combinatorial-invariant computation). Tier-2 is useful here (large CFGs, call graphs) and cheaper to implement than for lattices.
- **Extend-by-composition, not extend-in-place.** Unlike §5.4/§5.5 which extended shipped records, §5.6 creates a new `<graph>` record. `graph.sld`'s `<graph-analysis>` is a *computation context* (semiring + cache), not a graph object; extending it with topology primitives would conflate abstractions. See Q2 rationale.

**From §5.3's post-ship notes (`plans/2026-04-21-ac-matching-design.md:269–322`):**

- **Public-vs-internal discipline upfront.** `graph-vertices`, `graph-edges`, `graph-neighbors`, `graph-degree`, `graph-edge?`, `graph-adjacency-matrix` are public from day one. Internal helpers (canonical-label dispatcher, partition-refinement loop) stay internal and are tested black-box through `graph-isomorphic?` / `graph-canonical-form`. No "exposed for testing" hatches.
- **Avoid speculative cross-library integration.** `spanning-tree-count` via Kirchhoff's theorem *could* use `(wile algebra matrix)` for the Laplacian minor determinant — but matrix is field-valued, not just semiring, and the determinant coupling is exactly the kind of aesthetic-but-empirically-fragile dependency §5.3's matrix-permanent prune attempt taught us to avoid. v1 uses direct enumeration + counting (O(V!) Cayley's formula for K_n is fine for small cases; deletion-contraction recursion for general graphs). Matrix coupling is a v2 question if and when a benchmark demands it.
- **Canonical counterexamples as test fixtures, not afterthoughts.** Ship with canonical graph fixtures from day one: K_n (complete), C_n (cycle), P_n (path), Petersen graph (10 vertices, 15 edges — canonical test for iso algorithms since it's vertex-transitive and 3-regular), K_{n,m} (complete bipartite). Petersen self-iso returning `#t` is the backtracking-correctness canary (1-WL refinement alone would leave Petersen as an indistinguishable pair; the `#t` answer proves individualization-refinement is wired).
- **Plan specs as design intent, not implementation truth.** Subagents executing this plan should prefer the stated behavior and invariants over any drift in code sketches.

**From §5.5's free-distributive-lattice feasibility cap (`plans/2026-04-22-lattice-birkhoff-impl.md` Q15):**

- **Super-exponential growth requires a hard cap + diagnostic.** Chromatic polynomial via deletion-contraction is O(1.618^(V+E)) (golden ratio, Tutte-Birkhoff); spanning-tree enumeration is O(V^(V-2)) on K_n by Cayley. v1 caps at `(V + E) ≤ 20` for deletion-contraction; raises with a diagnostic citing the asymptotic and pointing to `graph-spanning-tree-count` (Kirchhoff, polynomial) for spanning counts. Same shape as §5.5's `dedekind(6) = 7828354` diagnostic.

**Goal:** Ship `(wile algebra combinatorial-graph)` — a graph-as-combinatorial-object library distinct from `(wile algebra graph)`. Per master plan §5.6: "New `(wile algebra combinatorial-graph)` — distinct from `graph.sld`" and Appendix A consumer map: `unify.scm` (CFG-level extension) and clone detection.

**Priority:** wile-goast-first (Tier A per 2026-04-22 ordering in `TODO.md`). Primary consumers are `unify.scm`'s `ssa-diff` (currently aligned tree diff only; CFG-level unification is graph-iso-shaped) and future register-interference analysis (chromatic polynomial).

**Architecture:** New library. `<graph>` record with setoid-carried vertex equality, adjacency-list primary representation (alist-of-alists: same shape as existing `graph.sld` for migration ergonomics), tier-1 / tier-2 / tier-3 finiteness discipline from §5.4. Pure Scheme, no Go primitives. No dependency on `(wile algebra matrix)` in v1 (see Q10).

**Tech Stack:** R7RS Scheme record types, `(srfi 1)` list ops, `(scheme hash-table)` for color-refinement signature deduplication. Imports on `combinatorial-graph.sld`: `(wile algebra setoid)` (for `default-setoid`, `setoid-equiv?`).

**References:**

- `plans/2026-04-17-algebra-foundations-directions.md` §5.6 and Appendix A — motivation and consumer map
- `plans/2026-04-22-lattice-birkhoff-impl.md` — template (§5.5) for extend-by-composition, setoid, finiteness tier, preset fixtures
- `plans/2026-04-22-group-actions-burnside-impl.md` — template (§5.4) for BFS-from-generators (tier-2 graphs reuse this)
- Diestel, R. *Graph Theory* (5th ed., 2017) — canonical graph-theory reference for order/size, bridge/loop, connected components
- Godsil, C. & Royle, G. *Algebraic Graph Theory* (2001) — canonical reference for algebraic invariants, Petersen graph properties
- Weisfeiler, B. & Leman, A. (1968) *The reduction of a graph to canonical form and the algebra which appears therein.* — 1-dimensional color refinement
- Cai, J.-Y., Fürer, M. & Immerman, N. (1992) *An optimal lower bound on the number of variables for graph identification.* — k-dimensional WL hierarchy; establishes 1-WL as the first level
- Corneil, D. G. & Gotlieb, C. C. (1970) *An efficient algorithm for graph isomorphism.* — equitable partition refinement
- McKay, B. & Piperno, A. (2014) *Practical graph isomorphism, II.* — nauty; terminology pin for "stable / discrete / equitable" partitions (§2.2) and "canonical labeling via lex-smallest leaf" (§3.3)
- Junttila, T. & Kaski, P. (2007) *Engineering an efficient canonical labeling tool for large and sparse graphs.* — bliss
- Read, R. C. (1968) *An introduction to chromatic polynomials.* — deletion-contraction
- Tutte, W. T. (1954) *A contribution to the theory of chromatic and flow polynomials.* — Tutte polynomial, bridge/loop recursion, `c(G)` component count
- Kirchhoff, G. (1847) — Matrix-tree theorem
- Hopcroft, J. & Karp, R. (1973) *An n^(5/2) algorithm for maximum matchings in bipartite graphs.*
- Schwenk, A. J. (1973) *Almost all trees are cospectral.* — cospectral non-isomorphic tree pair fixture
- Sedláček, J. (1970) — spanning-tree count of Petersen graph = 2000

---

## Vocabulary and citations

Terms used throughout this plan. Citations pinned so the design intent is unambiguous.

### Graph-theoretic terms

| Term | Definition | Citation | Plan usage |
|------|------------|----------|------------|
| **Order** of G | \|V(G)\| (vertex count) | Diestel §1.1 | Accessor: `graph-order` (see Q14) |
| **Size** of G | \|E(G)\| (edge count) | Diestel §1.1 | Accessor: `graph-size` (see Q14) |
| **Loop** | Edge `(v, v)` — both endpoints identical | Diestel §1.1 | Library flag: `self-loops?` (concrete synonym chosen for option-key clarity; "loop" and "self-loop" are interchangeable in prose) |
| **Bridge** | Edge whose removal increases the number of connected components | Diestel §1.9 | Phase 6 Tutte recursion |
| **Connected component** | Maximal connected subgraph | Diestel §1.1 | `c(G)` in Tutte↔chromatic identity = number of connected components (Tutte 1954 §1) |
| **Multi-edge** | Two or more distinct edges with the same endpoint pair | Diestel §1.1 | Library flag: `multi?`; storage shape pinned in Q6 |

### Partition-theoretic terms (iso algorithm)

| Term | Definition | Citation | Plan usage |
|------|------------|----------|------------|
| **Cell** (of a partition) | Equivalence class — one element of the partition | McKay-Piperno §2.2 | Synonym: "color class" (1-WL speak). Used interchangeably. |
| **Stable partition** | Partition on which 1-WL refinement makes no further progress (fixed point of the refinement operator) | McKay-Piperno §2.2; Corneil-Gotlieb 1970 | Layer-1 terminates in a stable partition |
| **Equitable partition** | Partition where for every pair of cells (C, D), every vertex of C has the same number of neighbors in D | McKay-Piperno §2.2 | Equivalent to "stable under 1-WL"; we use "stable" in prose |
| **Discrete partition** | Every cell has cardinality 1 (singletons) | McKay-Piperno §2.2 | Signals that the partition is a canonical labeling |
| **Non-trivial cell** | Cell with cardinality ≥ 2 | McKay-Piperno §2.2 | Layer 2 individualizes a vertex in a non-trivial cell |
| **Canonical labeling / canonical form** | Function `C(G)` such that `G ≅ H ⟺ C(G) = C(H)` | McKay-Piperno §1.1 | `graph-canonical-form` return |
| **Lex-smallest leaf canonical** | Over all leaves of the search tree (discrete partitions reachable via individualization), the canonical form of the graph is the lexicographically smallest relabeling | McKay-Piperno §3.3 | Q3 Layer 2 convention (shortened to "first canonical" colloquially; the *literal* meaning is lex-smallest, not leftmost-traversed) |

### Isomorphism algorithm labels

| Label | Expansion | Citation |
|-------|-----------|----------|
| **1-WL** | 1-dimensional Weisfeiler-Leman | Weisfeiler & Leman (1968); Cai-Fürer-Immerman (1992) established the k-WL hierarchy with 1-WL as the first level |
| **Individualization-refinement** | Select a vertex v from a non-trivial cell, give it a unique color (individualize), re-run 1-WL refinement, recurse | McKay-Piperno §3.1 |
| **Target cell** | Cell from which the individualization vertex is chosen | McKay-Piperno §3.2 |
| **Target-cell selector** | Heuristic choosing *which* non-trivial cell to individualize next | McKay-Piperno §3.2 (nauty uses sophisticated selectors; v1 uses smallest-cell-then-smallest-vertex) |
| **Leaf** | Terminal node in the search tree — a discrete partition | McKay-Piperno §3.1 |

### Polynomial invariants

| Term | Definition | Citation |
|------|------------|----------|
| **Chromatic polynomial** `χ(G, x)` | Number of proper k-colorings of G, as a polynomial in k | Read 1968 |
| **Tutte polynomial** `T(G; x, y)` | Bivariate graph invariant generalizing chromatic, flow, and reliability polynomials | Tutte 1954 |
| **Deletion-contraction recursion** | `T(G) = T(G − e) + T(G / e)` for non-bridge non-loop e; `T(G) = x · T(G / e)` if e is a bridge; `T(G) = y · T(G − e)` if e is a loop | Tutte 1954 |
| **Chromatic from Tutte** | `χ(G, x) = (-1)^(V − c(G)) · x^c(G) · T(G; 1 − x, 0)` where `c(G)` is the number of connected components | Tutte 1954 §9 |

---

## Prior art and design lineage

### Systems we are deliberately imitating

| System | What we inherit | Primary citations |
|--------|-----------------|-------------------|
| **SageMath** `sage.graphs.graph.Graph` | Adjacency-list-primary representation with O(1) neighbor lookup. `Graph.is_isomorphic(other)` public API returning `#t`/`#f`; `Graph.canonical_label()` returning a canonicalized copy. `Graph.chromatic_polynomial()` as a polynomial object (not a function). Splitting directed / undirected into one type with a flag, not two types. | [Sage Graph docs](https://doc.sagemath.org/html/en/reference/graphs/sage/graphs/graph.html) |
| **NetworkX** `networkx.Graph` / `DiGraph` | `G.neighbors(v)`, `G.degree(v)`, `G.nodes`, `G.edges` as first-class queries. `nx.is_isomorphic(G, H)` via VF2 (backtracking with invariants). Accepting both hashable-vertex and arbitrary-vertex-via-equality. | [NetworkX docs](https://networkx.org/documentation/stable/reference/) |
| **nauty / bliss** | **Color refinement (1-WL) as the iso-test foundation**, augmented with individualization-refinement backtracking when refinement terminates without a discrete partition. Refinement is complete for almost-all graphs and fails only on regular graphs; the backtracking layer handles the residue. v1 ships both layers — 1-WL as the refinement primitive and simplified individualization-refinement as the completeness layer. nauty-level optimization (sophisticated target-cell selection, automorphism-group caching) is deferred to v2. | McKay & Piperno (2014) |
| **§5.4's BFS-from-generators** | Tier-2 graphs (finitely-generated-via-seed + neighbor function) lazily enumerate via BFS, same machinery as group element enumeration. Vertex set may be infinite-but-BFS-bounded in principle; in practice we require finite BFS closure for any combinatorial computation. | `plans/2026-04-22-group-actions-burnside-impl.md` |

### Systems we deliberately do *not* imitate

| System / pattern | Why we diverge |
|------------------|----------------|
| **NetworkX's separate `Graph` / `DiGraph` / `MultiGraph` / `MultiDiGraph` type hierarchy** | Four types for two orthogonal flags (directed, multi-edge) is a combinatorial explosion of API surface. We use one `<graph>` record with `(directed? . #t/#f)` and `(multi? . #t/#f)` fields. Matches Sage's unified approach. |
| **Sage's full nauty integration** | nauty is a C library with ~30 years of optimization tricks; we ship pure Scheme. v1 iso is correct (1-WL + individualization-refinement backtracking) but not competitive with nauty on adversarial inputs. Production-grade nauty-lite optimization is a v2 investment if a consumer hits the wall. |
| **Blossom-algorithm general matching in v1** | Edmonds' blossom algorithm for general-graph maximum matching is ~300 LOC of its own and largely disjoint from other v1 machinery. Bipartite-only matching (Hopcroft-Karp) covers the primary consumer (assignment-shaped matching between code entities); general matching is deferred to v2 if a consumer surfaces. |
| **Polynomial return from `chromatic-polynomial`** | Sage returns a polynomial object. We don't have a univariate polynomial type in `(wile algebra polynomial)` parameterized over the integers that plays nicely with graph invariant computation. v1 `graph-chromatic-polynomial G` returns a list of coefficients `(a_0 a_1 ... a_V)` indexed by degree. v2 can wrap into `polynomial-ring` once the interop is measured. |
| **Graph-iso-by-canonical-form-only API** | Returning only a canonical form forces consumers who want the yes/no answer to compare two canonical forms themselves. We export both: `graph-isomorphic? G H` and `graph-canonical-form G`. |

### Convergence check

If a future refactor or extension would:

- Replace individualization-refinement backtracking with a structural sublattice check analog (there isn't one — graph iso has no Birkhoff-theorem-style reduction),
- Silently accept infinite graphs and return a probabilistic iso answer,
- Couple `<graph>` construction to `<graph-analysis>` in `graph.sld`,
- Ship four separate `Graph/DiGraph/MultiGraph/MultiDiGraph` record types,

then it is diverging from the direction validated by Sage / NetworkX / nauty / bliss. Stop and verify motivation rather than proceed.

---

## Resolved design decisions

### Q1: Scope cap — **C (full foundation)** — resolved 2026-04-22

Ships the complete directions-doc §5.6 export proposal in one pass:

- Core queries (Phase 1–2): `make-graph`, accessors, BFS/DFS, connected-components, bipartite test
- Isomorphism (Phase 3): `graph-isomorphic?`, `graph-canonical-form` — **full backtracking per Q3**
- Spanning-tree count (Phase 4): `graph-spanning-tree-count`
- Chromatic polynomial (Phase 5): `graph-chromatic-polynomial`
- Tutte polynomial (Phase 6): `graph-tutte-polynomial`
- Bipartite matching (Phase 7): `graph-maximum-bipartite-matching`
- Umbrella + docs + closeout (Phase 8)

LOC target: ~850 (Phase 3 backtracking iso adds ~130 LOC over the 1-WL-only path; total sits between `group.scm` 604 and `matrix.scm` 1302, mid-pack). Over-budget policy: scope-cut at phase boundaries, not scope-absorb.

**Scopes rejected:**

- **A — Minimum viable (iso only).** Ships only Phase 1–3. Leaves spanning/chromatic/Tutte/matching in a future v1.5, creating the "partial library that needs a v1.5" pattern the directions doc warned against. Would also mean the polynomial inline-helpers and the chromatic-from-Tutte consistency check never land.
- **B — Core + counts.** Ships through Phase 4 + Phase 7 (spanning-tree + bipartite matching, skipping polynomials). Same "partial library" critique; polynomials are the part of §5.6 most connected to the register-interference consumer in the foundations doc.
- **D — C + automorphism group.** Requires subgroup-from-generators machinery that §5.4 did not ship; genuine new permutation-group infrastructure for a v2 consumer that hasn't surfaced. Out of v1.

### Q2: Library location — **new library, not extension of `graph.sld`**

Per master plan §5.6: "distinct from `graph.sld`." The shipped `<graph-analysis>` is a computation context (semiring + cache), not a graph topology. Extending it would force every graph-analysis consumer to carry topology state they don't use, and every combinatorial-graph consumer to carry a semiring they don't use. Separation is cleaner.

Umbrella `algebra.sld` gains a `;; Combinatorial graphs` block alongside the existing `;; Graph analysis` block (the current `graph.sld` exports). No rename of `graph.sld`.

### Q3: Iso algorithm — **full backtracking (individualization-refinement, nauty-lite)** — resolved 2026-04-22

`graph-isomorphic? G H` returns `#t` or `#f` — no `'unknown` path. Algorithm has two layers:

**Layer 1: 1-WL color refinement** (Weisfeiler-Leman 1968). Each vertex's initial color = degree (or more generally `(degree, self-loop-count, ...)` if multi/self-loop flags set). Iteratively refine by `(current-color, sorted multiset of neighbor colors)` until the partition is stable. Complexity O((V+E) log V).

- If refinement is **discrete** (every color class has size 1): the partition is a canonical labeling; compare canonical forms of G and H directly for the answer. This is the fast path and catches all non-regular graphs.
- If refinement terminates with **non-trivial color classes**: proceed to Layer 2.

**Layer 2: Individualization-refinement backtracking** (McKay & Piperno 2014, simplified). For each non-trivial color class C:

1. Pick a target vertex v ∈ C (canonical choice: smallest-indexed — deterministic).
2. For each candidate vertex w in the corresponding color class of H:
   - Individualize v → new color, individualize w → same new color.
   - Re-run refinement.
   - Recurse on the refined partitions.
3. Return `#t` on first success; `#f` if all branches exhaust.

Pruning: (a) color-class cardinality mismatch between G and H at any level = early fail; (b) canonical-form comparison at discrete-partition leaves = direct check; (c) **lex-smallest-leaf pruning** (McKay-Piperno §3.3): across all leaves of the search tree, the canonical form is the lexicographically smallest relabeling; track the best-seen leaf and prune sibling branches whose partial canonical already exceeds it.

**Complexity:** O((V+E) log V) on almost-all graphs (refinement discretizes on the first pass). Exponential worst case on highly-symmetric graphs (K_n, Kneser, Paley); the automorphism-group-size branching inherent to the problem. Petersen-sized graphs (V = 10) terminate in well under a second regardless.

**Why not 1-WL-only:**

- Honest yes/no answer is more useful to `unify.scm` and future consumers than tri-state. Consumers don't have to invent a policy for `'unknown`.
- Matches Sage (`is_isomorphic`) and NetworkX (`is_isomorphic`) return shapes; one fewer mental model for users moving between ecosystems.
- Pure-Scheme complexity cost (~130 LOC over 1-WL-only) is acceptable inside the scope-C budget.

**Why not nauty-full:**

- nauty's full algorithm has ~30 years of optimization tricks (target-cell selectors, invariant-refinement heuristics, automorphism-group caching for cross-tree pruning). Reimplementing all of that is a separate library-sized effort.
- The simplified backtracking above is complete; nauty-level performance is a v2 optimization if a consumer needs it.

**API:** `graph-isomorphic? G H` returns `#t` or `#f`. `graph-canonical-form G` returns the canonicalized adjacency alist (lex-smallest-leaf canonical per Layer 2, McKay-Piperno §3.3). No `'unknown`, no `graph-isomorphic-certain?`.

### Q4: Graph representation — **adjacency alist, matching `graph.sld` shape**

`<graph>` primary representation: adjacency alist `((vertex . ((neighbor . edge-data) ...)) ...)`. Same shape as `<graph-analysis>` in `graph.sld`, deliberately, so consumers can construct once and use either library.

Edge-data slot is arbitrary payload (label, weight, multiplicity marker). `#f` = no payload.

**Why not adjacency-matrix-primary:** graphs in code analysis are typically sparse (V×V matrix is O(V²) space; adjacency list is O(V+E)). Matrix view is available via `graph-adjacency-matrix G` which constructs an `(wile algebra matrix)` on demand if and only if §5.1 matrix is available as an import. Lazy coupling, not eager dependency.

**Why not separate vertex-set + edge-list records:** third representation, more API surface, no operational benefit. Adjacency-alist serves both "iterate neighbors of v" (the common case) and "iterate all edges" (via `graph-edges G`) in the same data structure.

### Q5: Directed vs undirected — **one `<graph>` record with `directed?` flag**

Matches Sage. `(make-graph adjacency ...)` takes optional `(directed? . #t/#f)` in an options alist; default is `#f` (undirected). Undirected graphs store each edge once with an invariant that the adjacency is symmetric (checked in `validate-graph`, enforced in `make-graph` by symmetrizing if `(symmetrize? . #t)` is set).

Directed iso, directed matching, directed spanning-tree count are specializations of the undirected versions; the flag propagates through every algorithm as a case split, not a type bifurcation.

### Q6: Multi-edges and self-loops — **allowed, not default**

Options alist: `(multi? . #f)` default (disallows parallel edges), `(self-loops? . #t)` default (allows self-loops — graph-theory term "loop" per Diestel §1.1 — since they're common in CFG self-transitions).

**Storage shape depends on `multi?`:**

- `multi? = #f` (simple graph): adjacency alist is `((vertex . ((neighbor . edge-data) ...)) ...)`. Each neighbor appears at most once per vertex; `(assoc neighbor neighbor-list)` yields a unique entry or `#f`.
- `multi? = #t` (multigraph): adjacency alist keeps one entry per edge instance — the same `neighbor` symbol may appear multiple times in the inner list, each with its own `edge-data`. `(graph-neighbors G v)` returns the multi-set (duplicates preserved); `(filter (lambda (p) (equal? (car p) neighbor)) neighbor-list)` yields all edges from v to that neighbor.

Rationale: multigraphs fundamentally need multi-set storage to distinguish parallel edges (which matter for Tutte polynomial and have distinct edge-data payloads). Wrapping the inner cdr in a sublist (`(neighbor . (data1 data2 ...))`) was considered but rejected — it requires different traversal code than the simple-graph case, breaking the shared `graph.sld` representation.

**Loop handling (per Tutte 1954 bridge/loop recursion):**

- A loop `(v, v)` contributes 2 to `graph-degree v` (canonical: each endpoint of an edge contributes 1 to the incident vertex's degree; a loop has both endpoints at v).
- For iso signature: a loop contributes one element to the refinement signature multiset (self-reference, not a neighbor edge).
- For Tutte polynomial: loops are the `y` factor in the recursion.

Chromatic polynomial collapses multi-edges: `χ(G) = χ(G − multi-edges)` since parallel edges impose the same proper-coloring constraint. Implementation de-duplicates before running the chromatic deletion-contraction; Tutte does not.

Each algorithm states its multi-edge and loop handling explicitly in its docstring.

### Q7: Vertex equality — **setoid-carried, per §5.4 pattern**

`<graph>` carries a `setoid` field defaulting to `default-setoid`. `(graph-vertex-equiv? G u v)` delegates. Color refinement, edge lookup, and iso use this for all vertex equality operations. Consumer obligation: vertices must be distinguishable under the setoid (violation is a precondition error surfaced by `validate-graph`).

### Q8: Finiteness tier — **tier-1 required for combinatorial invariants; tier-2 supported for traversal**

Three-tier pattern from §5.4:

- **Tier-1** (vertices + edges enumerated): `graph-vertices`, `graph-edges`, `graph-order`, `graph-size` populated. Required by `graph-isomorphic?`, `graph-canonical-form`, `graph-chromatic-polynomial`, `graph-tutte-polynomial`, `graph-spanning-tree-count`, `graph-maximum-bipartite-matching`.
- **Tier-2** (finitely-generated via seed + neighbor function): `graph-bfs`, `graph-dfs`, `graph-connected-components` work via lazy enumeration (§5.4's BFS-closure pattern). Combinatorial invariants raise a precondition error citing `(cons 'elements L)` (§5.5-consistent) or calling `enumerate-finite-graph` as the fix.
- **Tier-3** (opaque adjacency-lookup only): `graph-neighbors`, `graph-edge?`, `graph-degree` work. Anything beyond raises.

### Q9: Size caps on exponential algorithms — **per-algorithm metric and threshold**

Each deletion-contraction invariant has its own cap because the recursion branches on different objects (edges for `τ`, edges-and-vertices for `χ` when fast paths miss):

| Algorithm | Metric | Cap | Asymptotic | Citation |
|-----------|--------|-----|------------|----------|
| `graph-spanning-tree-count` (deletion-contraction fallback) | \|E\| | 20 | O(2^E) (deletion + contraction each peel one edge) | Tutte 1954; Godsil-Royle §13 |
| `graph-chromatic-polynomial` (deletion-contraction fallback) | \|V\| + \|E\| | 20 | O(1.618^(V+E)) (golden ratio; Read 1968) | Read 1968 |
| `graph-tutte-polynomial` (deletion-contraction) | \|V\| + \|E\| | 20 | O(1.618^(V+E)) (same recursion as chromatic) | Tutte 1954 |

Both exceedances raise a diagnostic matching the group.scm / §5.5 convention — a flat list of `(violation-type arg1 arg2 ...)`:

    '(graph-spanning-tree-count-too-large size-actual size-threshold)
    '(graph-chromatic-polynomial-too-large order+size-actual order+size-threshold)
    '(graph-tutte-polynomial-too-large order+size-actual order+size-threshold)

Fast paths (closed-form expressions for K_n, C_n, P_n, trees, empty graphs) bypass the cap — passing K_100 to `graph-chromatic-polynomial` returns `x(x−1)(x−2)...(x−99)` without recursion. The cap only guards the general-case deletion-contraction fallback.

v2 Kirchhoff-via-matrix route (Q10) would lift the spanning-tree cap to polynomial in V; v1 diagnostic on exceedance mentions this.

### Q10: Matrix-library coupling — **none in v1 for correctness paths; optional for `graph-adjacency-matrix` view**

Kirchhoff's matrix-tree theorem says `spanning-tree-count(G) = any minor of Laplacian(G)`. Computing the determinant of a Laplacian minor via `(wile algebra matrix)` is aesthetically clean but:

1. Matrix library is field-valued (integer-coefficient Laplacian determinants are fine; but the dependency pulls field-arithmetic into a combinatorial library).
2. §5.3's matrix-permanent prune attempt (10.6× regression) warned against speculative matrix coupling.
3. Alternative: direct enumeration (O(V^(V-2)) on K_V by Cayley; fine for small) + deletion-contraction-recursion `τ(G) = τ(G - e) + τ(G / e)` for general graphs (O(2^E) worst case; matches chromatic-polynomial's size cap).

v1 uses direct enumeration + deletion-contraction. `graph-adjacency-matrix G` returns a `(wile algebra matrix)` **if and only if** §5.1 matrix is imported (check via `(cond-expand)` or runtime check). Kirchhoff-via-matrix is a v2 opt-in.

### Q11: Bipartite matching algorithm — **Hopcroft-Karp (O(E√V))**

Augmenting-path BFS; classical polynomial algorithm. Input: `graph-bipartition G` returns `(part-a part-b)` or raises if non-bipartite. `graph-maximum-bipartite-matching G` returns an alist `((u . v) ...)`.

Non-bipartite maximum matching (Edmonds' blossom, O(V·E·α(E,V))) is deferred to v2. Diagnostic on non-bipartite input cites `graph-maximum-matching` as the v2 export that would handle it.

### Q12: Presets — **Petersen, K_n, C_n, P_n, K_{n,m}, empty-graph-on-n**

Ship six canonical fixtures:

| Preset | Vertices | Edges | Iso-test role | Notes |
|--------|----------|-------|---------------|-------|
| `(complete-graph n)` | n | n(n-1)/2 | Trivially iso under any vertex permutation; chromatic = n! · falling factorial | Canonical 1-WL-complete case |
| `(cycle-graph n)` | n | n | Vertex-transitive; 1-WL incomplete for n ≥ 4 without individualization (all vertices same signature) | Canonical regular-graph test |
| `(path-graph n)` | n | n-1 | Symmetric only under reflection; 1-WL complete | Canonical tree case |
| `(complete-bipartite-graph m n)` | m+n | m·n | Bipartite; matching = min(m,n) | Tests `graph-bipartition` |
| `(petersen-graph)` | 10 | 15 | Vertex-transitive, 3-regular; **1-WL refinement does not discretize**; backtracking-correctness canary | Canonical iso-algorithm test case (Godsil & Royle §3) |
| `(empty-graph n)` | n | 0 | Baseline; chromatic = (x)_n (falling factorial) | Edge cases |

**Why these:** K_n and C_n span the "iso-is-trivial" and "iso-is-hard-on-regular" extremes; K_{m,n} is the matching test; Petersen exercises the backtracking layer (1-WL refinement alone leaves Petersen undiscretized; `#t` on `(petersen-graph) iso (petersen-graph)` proves individualization-refinement is correctly wired).

### Q13: Validation and error diagnostics — **per §5.3 Q discipline**

`validate-graph G` checks: adjacency is symmetric (for undirected), self-loops respected per `self-loops?` flag, multi-edges respected per `multi?` flag, vertex set = keys of adjacency, setoid-distinguishable vertices. Returns `#t` or a diagnostic list `((violation-type args...) ...)`.

Precondition errors in operations (tier mismatch, non-bipartite for bipartite algorithm, size-cap exceeded) raise with structured diagnostics citing the fix.

### Q14: Accessor naming for \|V\| and \|E\| — **`graph-order` / `graph-size`** (canonical graph-theory)

Diestel §1.1 pins: the **order** of G is \|V(G)\|; the **size** of G is \|E(G)\|. Godsil-Royle, Bondy-Murty, and every subsequent textbook follow this. Accessor names:

- `graph-order G` → integer (or `#f` for tier-3); the number of vertices.
- `graph-size G` → integer (or `#f` for tier-3); the number of edges.

**Deliberate cross-family asymmetry with §5.4 / §5.5.** `group-order` means \|G\| (elements), `lattice-cardinality` means \|L\| (elements) — both of which are the graph-theory "order" concept. Graph theory has a *second* size concept (edges) that groups and lattices don't, which is why `-order` is reused for vertex count and `-size` is introduced for edge count. Following graph-theory literature trumps cross-family naming symmetry here, same reasoning §5.5 Q14 applied for `lattice-cardinality` over `lattice-order`.

**Why not `graph-v-count` / `graph-e-count` (earlier draft):** mechanically clear but non-canonical. Readers moving between Sage (`G.order()`, `G.size()`), NetworkX (`G.order()`, `G.size()`), and literature never see `v-count` / `e-count`. One fewer mental model.

**Why not `graph-cardinality`:** ambiguous — does it mean V or E? Graph theory already settled this with distinct words.

### Q15: Predicates and assertions — **match shipped sibling libs exactly**

Export the predicate and assertion siblings established by `group.sld` / `lattice.sld`:

- `finite-graph? G` — predicate (tier-1). True iff the graph has enumerated vertices AND enumerated edges AND both counts are finite. Parallel to `finite-group?`, `finite-lattice?`.
- `finitely-generated-graph? G` — predicate (tier-2). True iff the graph exposes a seed vertex and neighbor function but no enumerated vertex set. Parallel to `finitely-generated-group?`.
- `assert-graph G samples` — raising variant of `validate-graph`. group.sld exports `assert-group`; lattice.sld skipped `assert-lattice`. We follow group's precedent since graph validation has more failure modes (symmetry, multi/self-loop flag consistency, setoid distinguishability) where a raising API is useful to consumers.
- `enumerate-finite-graph G` — promotes tier-2 to tier-1 via BFS closure from the seed vertex. Parallel to `enumerate-finite-group`. Idempotent on already-tier-1 graphs.

### Q16: Tier-2 options alist keys — **`seed` and `neighbor-fn`**

For tier-2 graphs, the options alist on `make-graph` accepts:

- `(seed . v)` — starting vertex for BFS enumeration.
- `(neighbor-fn . proc)` — procedure `v → ((neighbor . edge-data) ...)` yielding v's out-neighbors.
- `(max-size . N)` — bound on `enumerate-finite-graph` closure, raises if exceeded. Parallel to group's `max-size` key.

Graph analog of group's `(generators . LIST)`. "Seed + neighbor-fn" chosen over "generators" because graph literature doesn't use "generator" for vertices; seed+neighbor matches how CFGs are actually constructed (entry block + successor function).

### Q17: Automorphism group integration with §5.4 — **v2**

`graph-automorphism-group G` returning a `<group>` built from `(wile algebra group)` is categorically correct — the automorphism group is a subgroup of the symmetric group on the vertex set, fixed by the permutations that preserve adjacency. But:

1. Requires permutation-group presentation machinery (§5.4 shipped `symmetric-group` but not `subgroup-from-generators`).
2. nauty's group-computation pass is where the algorithm complexity concentrates; in pure Scheme this is a separate 200 LOC investment.
3. No v1 consumer.

v2 addition.

---

## Umbrella surface

`algebra.sld` gains (roughly, indented for the scope-C recommendation):

    ;; Combinatorial graphs — core
    make-graph graph?
    graph-vertices graph-edges graph-neighbors graph-degree
    graph-edge? graph-vertex-equiv? graph-setoid
    graph-order graph-size graph-directed? graph-multi? graph-self-loops?
    graph-adjacency-matrix       ;; lazy; §5.1 matrix if available
    ;; Combinatorial graphs — tier predicates and promotion (per §5.4 precedent)
    finite-graph? finitely-generated-graph?
    enumerate-finite-graph
    ;; Combinatorial graphs — validation
    validate-graph assert-graph with-graph
    ;; Combinatorial graphs — traversal
    graph-bfs graph-dfs graph-connected-components
    graph-bipartite? graph-bipartition
    ;; Combinatorial graphs — isomorphism
    graph-isomorphic? graph-canonical-form
    ;; Combinatorial graphs — invariants
    graph-spanning-tree-count
    graph-chromatic-polynomial graph-tutte-polynomial
    ;; Combinatorial graphs — matching
    graph-maximum-bipartite-matching
    ;; Combinatorial graphs — presets
    complete-graph cycle-graph path-graph
    complete-bipartite-graph petersen-graph empty-graph

(Scope-C resolved; A/B/D rejected per Q1. This is the v1 surface.)

---

## Phases (scope C, 8 phases — 7 implementation + 1 closeout)

Each phase is one commit. Commit-cadence per `feedback_commit_cadence.md`: progressive commits once Phase 1 is authorized.

### Phase 1 — Scaffolding (~60 LOC)

- [ ] Create `stdlib/lib/wile/algebra/combinatorial-graph.sld` with header + empty export block.
- [ ] Create `stdlib/lib/wile/algebra/combinatorial-graph.scm` with `<graph>` record type. Fields: `adjacency`, `directed?`, `multi?`, `self-loops?`, `setoid`, cached `order` (= \|V\|), cached `size` (= \|E\|), tier-2 slots `seed` and `neighbor-fn` (both `#f` for tier-1).
- [ ] `make-graph adjacency opts` constructor. Options-alist keys (validated per `%validate-opts-keys` convention from group.scm): `directed?`, `multi?`, `self-loops?`, `setoid`, `symmetrize?`, `seed`, `neighbor-fn`, `max-size`.
- [ ] Accessors: `graph-vertices`, `graph-edges`, `graph-neighbors`, `graph-degree`, `graph-edge?`, `graph-directed?`, `graph-multi?`, `graph-self-loops?`, `graph-order`, `graph-size`, `graph-setoid`, `graph-vertex-equiv?`.
- [ ] Tier predicates: `finite-graph?`, `finitely-generated-graph?` (match §5.4 `finite-group?` / `finitely-generated-group?` discipline).
- [ ] Promotion: `enumerate-finite-graph G` (idempotent; BFS closure from `seed` via `neighbor-fn`; respects `max-size`).
- [ ] `validate-graph G samples` axiom check — returns `#t` or a list of `(violation-type arg ...)` entries per group.scm convention. Checks: symmetry on undirected, self-loops respected per flag, multi-edges respected per flag, vertex set = keys of adjacency, setoid-distinguishable vertices.
- [ ] `assert-graph G samples` raising variant (group.scm precedent).
- [ ] `with-graph` binder analogous to `with-lattice` / `with-group`.
- [ ] Tests: construction, accessors, tier-predicate dispatch (tier-1 vs tier-2 vs tier-3), validate on correct and malformed inputs, assert raises on failure, `enumerate-finite-graph` idempotency, options-alist key validation raises on unknown key.

**Commit message:** `feat(algebra/combinatorial-graph): scaffold <graph> record + accessors`

### Phase 2 — Traversal and bipartiteness (~80 LOC)

- [ ] `graph-bfs G source` returns visit order (list of vertices).
- [ ] `graph-dfs G source` returns visit order.
- [ ] `graph-connected-components G` returns list of vertex lists.
- [ ] `graph-bipartite? G` via two-coloring BFS; returns `#t`/`#f`.
- [ ] `graph-bipartition G` returns `(part-a part-b)` or raises.
- [ ] Tests: traversal on K_n / C_n / P_n / disconnected graphs; bipartition on K_{m,n} and odd cycles.

**Commit message:** `feat(algebra/combinatorial-graph): BFS/DFS + bipartite check`

### Phase 3 — Canonical form + full backtracking iso (~250 LOC)

Two sub-commits are acceptable here (the refinement primitive and the backtracking layer are natural seams). Single commit also fine if the Layer-1 helper is small and Layer 2 composes cleanly.

**Layer 1 — 1-WL color refinement (helper, not terminal):**

Terminology per McKay-Piperno §2.2 (pinned in the Vocabulary section above): a **cell** is one element of the partition; **stable** = 1-WL fixed point = coarsest equitable partition; **discrete** = every cell has cardinality 1; **non-trivial cell** = cardinality ≥ 2.

- [ ] Partition data structure: `<partition>` record wrapping a list of cells (each cell = list of vertices), kept sorted by cell cardinality then by smallest vertex for deterministic output.
- [ ] `refine-partition G P`: iteratively refine by `(current-color, sorted multiset of neighbor colors)` until **stable**. Initial color = `(degree, in-degree-if-directed, self-loop-count)`. O((V+E) log V) via hashtable-backed signature bucketing.
- [ ] `discrete-partition? P`: predicate — every cell has cardinality 1.
- [ ] `partition-canonical-adjacency G P`: given a discrete partition, relabel vertices by their cell position (0-indexed) and emit the canonical adjacency alist.

**Layer 2 — Individualization-refinement backtracking:**

- [ ] `graph-canonical-form G`: run Layer 1 refinement; if discrete, emit via `partition-canonical-adjacency`. Otherwise pick smallest-indexed vertex in the smallest non-trivial cell (deterministic target-cell selector; McKay-Piperno §3.2), individualize each member in turn, recurse. Collect all leaf canonicals; return the lexicographically-smallest leaf canonical (McKay-Piperno §3.3). Branch-and-bound: prune sibling branches whose partial canonical already exceeds the best-seen-so-far (lex-smallest-leaf pruning).
- [ ] `graph-isomorphic? G H`: returns `#t` if `(equal? (graph-canonical-form G) (graph-canonical-form H))`. Short-circuit: early `#f` if `(graph-order G) ≠ (graph-order H)`, `(graph-size G) ≠ (graph-size H)`, or degree sequences differ. Further short-circuit: early `#f` if refined color-cell cardinalities differ between G and H.
- [ ] Presets: `complete-graph`, `cycle-graph`, `path-graph`, `complete-bipartite-graph`, `empty-graph`, `petersen-graph`.

**Tests (Phase 3):**

- [ ] Non-regular, fast path: `P_n iso P_n` under random relabeling → `#t`; `P_5 iso P_6` → `#f` (different V).
- [ ] Regular, backtracking required: `C_5 iso C_5` under rotation → `#t`; `C_5 iso C_6` → `#f`.
- [ ] Vertex-transitive: `K_n iso K_n` under random permutation → `#t` for n ≤ 6 (exponential in automorphism group size; K_7 skipped to keep test runtime in check).
- [ ] **Petersen regression canary:** `(graph-isomorphic? (petersen-graph) (petersen-graph))` → `#t`. The 1-WL-only draft would have returned `'unknown` here; this test locks in the backtracking correctness.
- [ ] **Cospectral non-isomorphic canary:** two known cospectral-but-non-iso graphs (Schwenk pair on 8 vertices is standard) → `#f`. Catches the degenerate case where 1-WL refinement indistinguishes but the graphs are genuinely non-iso.
- [ ] Negative: `K_4 iso C_4` → `#f`; `K_{3,3} iso K_{2,4}` → `#f`; `empty-graph 5 iso cycle-graph 5` → `#f`.
- [ ] Edge cases: `empty-graph 0 iso empty-graph 0` → `#t`; `empty-graph 1 iso empty-graph 1` → `#t`.
- [ ] Deterministic canonical form: `(graph-canonical-form G)` is equal under `equal?` for two differently-ordered adjacency alists of the same graph.

**Commit message(s):** `feat(algebra/combinatorial-graph): 1-WL color refinement primitive` (if split) then `feat(algebra/combinatorial-graph): individualization-refinement iso backtracking`. Or single: `feat(algebra/combinatorial-graph): backtracking graph isomorphism via individualization-refinement`.

### Phase 4 — Spanning-tree count (~80 LOC)

- [ ] `graph-spanning-tree-count G`: deletion-contraction recursion `τ(G) = τ(G - e) + τ(G / e)` for general graphs, size-capped at E ≤ 20. Closed-form fast paths: `τ(K_n) = n^(n-2)` (Cayley), `τ(C_n) = n`, `τ(P_n) = 1`, `τ(tree) = 1`.
- [ ] Helper: `graph-delete-edge G e`, `graph-contract-edge G e`.
- [ ] Size-cap diagnostic citing `E ≤ 20` and v2 Kirchhoff matrix route.
- [ ] Tests: K_3 = 3, K_4 = 16, K_5 = 125, C_5 = 5, Petersen = 2000 (known value).

**Commit message:** `feat(algebra/combinatorial-graph): spanning-tree count via deletion-contraction + fast paths`

### Phase 5 — Chromatic polynomial (~100 LOC)

- [ ] `graph-chromatic-polynomial G`: deletion-contraction `χ(G, x) = χ(G - e, x) - χ(G / e, x)`. Return coefficient list `(a_0 a_1 ... a_V)` indexed by degree (so `a_0` is constant term, always 0 for V ≥ 1). Fast paths: `χ(K_n, x) = x(x-1)...(x-n+1)`, `χ(empty-graph n, x) = x^n`, `χ(tree T on n vertices, x) = x(x-1)^(n-1)`, `χ(C_n, x) = (x-1)^n + (-1)^n (x-1)`.
- [ ] Size-cap diagnostic citing `V + E ≤ 20`.
- [ ] Polynomial arithmetic helpers: add-poly, subtract-poly, multiply-poly (inline; ~30 LOC). Not a dependency on `(wile algebra polynomial)` — v1 keeps this self-contained to avoid the speculative-cross-library-integration trap.
- [ ] Tests: chromatic(K_3) = [0, 2, -3, 1] (x(x-1)(x-2) = x³ - 3x² + 2x); chromatic(C_4) = [0, -3, 6, -4, 1] ((x-1)^4 + (x-1) = x^4 - 4x^3 + 6x^2 - 3x; generalized from χ(C_n, x) = (x-1)^n + (-1)^n (x-1) per Read 1968); chromatic(empty-graph 3) = [0, 0, 0, 1] (x^3); chromatic(tree on 4) = [0, -1, 3, -3, 1] (x(x-1)^3); chromatic(Petersen) verified against published value (Biggs 1993).

**Commit message:** `feat(algebra/combinatorial-graph): chromatic polynomial via deletion-contraction + fast paths`

### Phase 6 — Tutte polynomial (~80 LOC)

- [ ] `graph-tutte-polynomial G`: deletion-contraction with the Tutte recursion `T(G; x, y) = T(G - e; x, y) + T(G / e; x, y)` for non-bridge non-loop `e`; `T(G; x, y) = x · T(G / e; x, y)` if `e` is a bridge; `T(G; x, y) = y · T(G - e; x, y)` if `e` is a loop. Return bivariate coefficient matrix `((a_{i,j}))` or 2D list.
- [ ] Bridge detection: edge `e` is a bridge if removing it increases component count. O((V+E)·E) per call (DFS per edge); acceptable at size cap.
- [ ] Derived: `χ(G, x) = (-1)^(V - c(G)) · x^c(G) · T(G; 1-x, 0)` — add as a consistency test, not a computation path.
- [ ] Tests: Tutte(K_3) known, Tutte(K_4) known, chromatic-from-Tutte matches direct chromatic for ≥ 3 fixtures.

**Commit message:** `feat(algebra/combinatorial-graph): Tutte polynomial via deletion-contraction`

### Phase 7 — Bipartite matching (~100 LOC)

- [ ] `graph-maximum-bipartite-matching G`: Hopcroft-Karp O(E√V). Requires `graph-bipartite? G` first; raises non-bipartite-for-bipartite-matching if false. Returns alist `((u . v) ...)` of matched pairs.
- [ ] Helpers: BFS layer construction, DFS augmenting-path search, layered-graph traversal.
- [ ] Tests: K_{3,3} matching size 3, K_{2,4} matching size 2, empty bipartite = 0, odd cycle raises.

**Commit message:** `feat(algebra/combinatorial-graph): Hopcroft-Karp bipartite matching`

### Phase 8 — Umbrella, docs, closeout (~30 LOC + doc)

- [ ] Add `;; Combinatorial graphs` block to `stdlib/lib/wile/algebra.sld` umbrella with all exports.
- [ ] Add entry to `TODO.md` marking §5.6 shipped.
- [ ] Add plans/CLAUDE.md entry under Algebra Libraries / completed.
- [ ] Write `docs/reference/combinatorial-graph.md` (short; structure per `docs/reference/incidence.md` precedent): what-certifies-what, consumer map to wile-goast, iso algorithm summary (1-WL refinement + individualization-refinement backtracking; complete; exponential worst case on highly-symmetric inputs), size-cap diagnostics.
- [ ] Run `make lint && make covercheck`; fix any findings.
- [ ] Crosscheck dispatch (`/crosscheck` with current changeset) for convention drift.

**Commit message:** `docs(algebra/combinatorial-graph): umbrella re-export + TODO closeout + reference doc`

---

## Test plan

Target: ~80 tests (scales with scope C). Follow `lattice-birkhoff` and `unification` test style (chibi-test).

### Fixture tests (Phase 1–3)
- Construction correctness: adjacency → accessors roundtrip
- Directed / undirected / multi / self-loop flag respect
- Setoid-carried equality: vertices-under-numeric-setoid distinguished from vertices-under-eqv-setoid for equivalent integers via different constructors

### Traversal tests (Phase 2)
- BFS/DFS order on K_n, C_n, P_n, disconnected graph
- Connected components on disjoint union of K_3 and C_4
- Bipartiteness: K_{m,n} yes, C_n yes-iff-n-even, K_n no-for-n≥3

### Isomorphism tests (Phase 3)
- Positive (non-regular, fast path): relabel K_{m,n}, P_n via random permutation → `#t`
- Positive (regular, backtracking exercised): relabel C_n, K_n (n ≤ 6), Petersen → `#t`
- Negative: K_n vs K_{n+1}; K_4 vs C_4; K_{3,3} vs K_{2,4}; empty-graph-5 vs cycle-graph-5
- **Backtracking correctness canary — Petersen self-iso returns `#t`.** 1-WL alone would have returned `'unknown` on this pair; the `#t` return is the regression signal that backtracking is wired.
- **Cospectral non-iso canary — Schwenk pair** on 8 vertices → `#f`. Catches the case where refinement doesn't distinguish but graphs are genuinely non-isomorphic.
- Edge cases: empty graph iso to empty graph; single-vertex iso
- Determinism: `(graph-canonical-form G)` invariant under adjacency-alist ordering

### Count tests (Phases 4–7)
- Spanning trees (Cayley / Sedláček): `τ(K_3) = 3`, `τ(K_4) = 16`, `τ(K_5) = 125`, `τ(C_5) = 5`, `τ(P_5) = 1`, `τ(Petersen) = 2000` (Sedláček 1970)
- Chromatic (Read 1968): `χ(K_3, x) = x(x-1)(x-2)`, `χ(C_4, x) = (x-1)^4 + (x-1) = x^4 - 4x^3 + 6x^2 - 3x`, `χ(T, x) = x(x-1)^(n-1)` for trees on n vertices, `χ(empty-n, x) = x^n`
- Chromatic-from-Tutte consistency (Tutte 1954 §9): `χ(G, x) = (-1)^(V - c(G)) · x^c(G) · T(G; 1-x, 0)` verified for K_3, K_4, C_4
- Bipartite matching: K_{3,3} = 3, K_{2,4} = 2, bipartite path P_n = ⌊n/2⌋

### Precondition / diagnostic tests (all phases)
- Tier-3 graph raises on `graph-canonical-form` with diagnostic citing `'elements` (§5.5-consistent options-alist key) or `enumerate-finite-graph` as the fix
- Odd cycle raises on `graph-bipartition` with diagnostic `(not-bipartite odd-cycle-witness ...)`
- \|E\| > 20 raises on `graph-spanning-tree-count` (general-case fallback only — K_n / C_n / P_n / tree fast paths skip the cap)
- \|V\| + \|E\| > 20 raises on `graph-chromatic-polynomial` and `graph-tutte-polynomial` (general-case fallback only)
- Unknown options-alist key raises via `%validate-opts-keys` (group.scm convention)
- `assert-graph` raises on malformed adjacency, `validate-graph` returns violation list
- Non-symmetric adjacency on undirected graph raises in `validate-graph` unless `symmetrize?` option is set

---

## Non-goals (explicit)

- **Edmonds' blossom / general-graph maximum matching.** v2 if a consumer surfaces.
- **Graph automorphism group.** v2 (Q17).
- **nauty-level iso optimization** (sophisticated target-cell selection, automorphism-group caching across branches, multi-level invariant refinement). v1 ships the simplified individualization-refinement backtracking — correct but not competitive with nauty on adversarial inputs. v2 if perf on highly-symmetric inputs proves inadequate.
- **Kirchhoff via `(wile algebra matrix)` determinant.** v2 opt-in (Q10).
- **Planarity testing, graph embedding, topological graph theory.** Out of v1 scope; not in foundations §5.6.
- **Graph minors, tree decomposition, treewidth.** Distinct research track.
- **Spectral graph theory (Laplacian eigenvalues).** Out of v1 scope; depends on mature numeric linear algebra which §5.1 is not.
- **Random-graph generators (Erdős–Rényi, Barabási–Albert).** Out of scope; benchmark-only if needed, not a library export.

---

## Relation to adjacent libraries

| Library | Relation |
|---------|----------|
| `(wile algebra graph)` | Disjoint. Both use adjacency-alist representation (same shape — consumers can build once, use either library for its respective computation). No cross-library symbol collision. |
| `(wile algebra matrix)` | Optional: `graph-adjacency-matrix G` constructs a `(wile algebra matrix)` on demand. No eager dependency. v2 may add Kirchhoff-via-determinant as opt-in. |
| `(wile algebra group)` | Disjoint in v1. v2 `graph-automorphism-group G` would return a `<group>` built from §5.4 machinery. |
| `(wile algebra order)` / `incidence` | Disjoint. Connected-components partial order could be a v2 `graph-component-order G` returning a `<partial-order>`, but no v1 consumer. |
| `(wile algebra lattice)` | Disjoint in v1. Partition lattice of graph vertex partitions is a theoretical v2 connection; no consumer. |
| `(wile algebra fca)` | Disjoint. v2: graph bipartite concept lattice via incidence-relation adjacency → FCA concept lattice. No v1 consumer. |

---

## Appendix — wile-goast consumer map (v1)

| Export | Named wile-goast consumer | Current handling |
|--------|---------------------------|------------------|
| `graph-isomorphic?` / `graph-canonical-form` | `unify.scm` CFG-level clone detection (currently tree-aligned only) | Not currently handled — this is the new capability |
| `graph-bfs` / `graph-dfs` | `goastcg/` call-graph traversal | Ad-hoc traversal in Go |
| `graph-connected-components` | `goastcg/` component analysis | Ad-hoc |
| `graph-bipartite?` / `graph-maximum-bipartite-matching` | Cross-version symbol matching (per foundations §4.2) | Not currently handled |
| `graph-chromatic-polynomial` | Register-interference analysis (per foundations §2.3) | Not currently handled |
| `graph-spanning-tree-count` | No v1 wile-goast consumer; named here for completeness of §5.6 export proposal | — |
| `graph-tutte-polynomial` | No v1 wile-goast consumer; prerequisite for network-reliability applications if later surfaced | — |

Chromatic, Tutte, and spanning-tree-count ship without immediate named consumers; they are part of the directions-doc §5.6 scope C proposal. If user selects scope A or B, these drop.

---

## Open questions (can be decided during implementation, not blocking Phase 1)

1. **Plan file naming convention** — impl plan only, or split into design.md + impl.md pair like §5.3 / §5.5? Prior shipped algebra plans are split when the Q&A is substantial; single-file when design decisions are inherited. Current draft is single-file. Split is not required — most Q&A inherits from §5.4/§5.5 — but can be done during Phase 8 closeout if the shipped file feels overstuffed.
2. **Petersen graph preset LOC budget.** Hard-coding the 10 vertices and 15 edges is ~15 LOC; constructing via Kneser-graph definition (`vertices = 2-subsets of [5]`, `edges = disjoint pairs`) is ~25 LOC and generalizes to `kneser-graph n k`. Default: hard-coded Petersen; `kneser-graph` as a v2 extension if consumers want the family.
3. **Phase 3 commit granularity** — one commit (backtracking iso with 1-WL refinement helper) or two (refinement primitive, then backtracking layer)? Single is fine if the refinement primitive is <80 LOC; split is fine if hitting >120. Decide at Phase-3 writing time.
