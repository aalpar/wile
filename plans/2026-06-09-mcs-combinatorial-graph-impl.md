# Maximum Common Subgraph for `(wile algebra combinatorial-graph)` — Implementation Plan

**Status:** IMPLEMENTED 2026-06-09 (working tree, not yet committed). All phases
landed in `combinatorial-graph.scm` + `.sld` + test; 353-assertion suite green
(exit 0), including the §6 admissibility regression. The §6 "user-authored"
pieces (`%mcs-upper-bound` + the admissibility test) were completed on request
rather than left as TODOs. Not committed — awaiting instruction.
**Parent design:** `plans/2026-04-17-algebra-foundations-directions.md` §4.2 ("Maximum
common subgraph → unification detection") + §5.6 (combinatorial-graph; §4.2's
capstone, the one un-built "strong fit" from Part 3.1).
**TODO entry:** `TODO.md:132` — "§4.2 Maximum common subgraph [Algebra, Matching]".
**Lands in:** `stdlib/lib/wile/algebra/combinatorial-graph.scm` (+ `.sld`, + test).
No new library, no new dependency. The substrate (`<graph>`, setoid accessors,
Hopcroft-Karp matcher) is already shipped.

---

## 1. What this delivers

A new export:

    graph-maximum-common-subgraph G H . opts
      → correspondence: alist ((g-vertex . h-vertex) ...)

The correspondence is a maximum **connected induced** common subgraph (MCCIS):
the largest injective vertex mapping `M : V(G) ⊇ D → V(H)` such that

1. **Induced-preserving:** for every pair of mapped vertices `(a→b)`, `(a'→b')`
   in `M`, `edge?(G,a,a') ⟺ edge?(H,b,b')`. *Both* adjacency and non-adjacency
   must agree — this is induced subgraph isomorphism (§3.4 "graph iso (CFG)"),
   not partial/edge subgraph.
2. **Connected (default):** the mapped domain `D` induces a connected subgraph
   of `G`. Relaxable via `(disconnected? . #t)`.
3. **Maximum:** `|M|` is as large as possible (cardinality objective). A weighted
   objective is an explicit v2 extension (see §7).

Return shape mirrors `graph-maximum-bipartite-matching` (`combinatorial-graph.scm:1665`):
an alist of pairs. Consistency with the sibling matching primitive is deliberate —
a caller already handling one handles the other. The empty correspondence `'()`
is the trivially-common subgraph; any compatible single pair beats it.

### Design decisions (resolved)

| Axis | Decision | Rationale |
|------|----------|-----------|
| Induced vs. partial | **Induced** | §3.4 frames CFG clone detection as graph iso = induced subgraph iso. |
| Connected vs. disconnected | **Connected default**, `disconnected?` opt | Code clones are connected fragments; smaller search; McGregor/McCreesh clone-detection variant. |
| Objective | **Cardinality** (`|M|`), `compatible?` opt for node filtering | Standard MCS; weighting is additive (v2). |
| Exact vs. heuristic | **Exact** B&B | §4.2: "branch-and-bound with assignment relaxation". Tier-1 graphs (per-function ASTs/CFGs) are small. Mirrors `graph-isomorphic?` being "complete — always a definite answer" with exponential worst case. |
| Directed support | **Yes** | Induced check is direction-aware via `graph-edge?`; the bound's bipartite compatibility graph is constructed undirected regardless of G/H directedness, so the matcher's directed-raise (`:1668`) never triggers. |
| Multi-edges | **Ignored in v1** (treated simple) | Documented limit; matches the matcher's v1 simplifications. |

---

## 2. Algorithm (McGregor 1982, branch-and-bound; bound per §4.2)

Search a tree of partial mappings. Each node holds:
- `M` — the partial correspondence built so far (alist of pairs).
- `incumbent` — best complete-on-this-path mapping seen globally (mutable, the B&B "best").

**Branching (connected):** seed the search once per candidate start pair
`(g0, h0)`. Thereafter only extend by a G-vertex on the *frontier* — adjacent to
the already-mapped domain — paired with a consistent unmapped H-vertex. The
frontier restriction is what enforces connectivity and prunes the tree hard.
(`disconnected?` drops the frontier restriction: any unmapped G-vertex may extend.)

**Feasibility (`%mcs-consistent?`):** a candidate extension `(a→b)` is admissible
iff for every existing `(a'→b')` in `M`: `graph-edge?(G,a,a') = graph-edge?(H,b,b')`
in both directions (directed) or symmetrically (undirected), and self-loop
agreement `graph-edge?(G,a,a) = graph-edge?(H,b,b)`.

**Bound (`%mcs-upper-bound`) — the §4.2 "assignment relaxation":** at a node with
mapping `M`, an upper bound on the *total* achievable size is
`|M| + (max additional pairs)`. The additional pairs are bounded by a maximum
bipartite matching between the still-unmapped, still-compatible G-vertices and
H-vertices: build a bipartite `<graph>` whose left = remaining G-vertices,
right = remaining H-vertices, edge `(u,v)` iff `u` and `v` could still be matched
(`compatible?` holds and degree-feasible), then the bound contribution is
`(length (graph-maximum-bipartite-matching compat-graph))`. **Prune** the node
when `|M| + bound ≤ |incumbent|` — it cannot beat the best.

> **Admissibility is the load-bearing property.** The bound MUST never
> under-estimate the true achievable size. If it does, B&B prunes a branch that
> contained the real optimum and the answer is silently wrong. A maximum matching
> is a valid relaxation because any feasible extension is an injective assignment,
> and a matching is the largest injective assignment ignoring the induced
> constraints — so it can only over-count, never under-count. This is why the
> bound is a *matching* (relaxation), not the exact recursive answer.

Complexity: exponential worst case (MCS is NP-hard, no 1-WL escape hatch — unlike
`graph-isomorphic?`, the graphs are deliberately different sizes with no
order/size/degree short-circuit). The matching bound is what makes it tractable
on small graphs; without it the tree is `O(|V_G|! )`-ish.

---

## 3. Internal layout (where it sits in the file)

Insert after the matching section (after `combinatorial-graph.scm:~1783`, before
the presets at `:1786`). Section banner mirroring the existing style:

    ;;; ====================================================================
    ;;; Maximum common connected induced subgraph (MCCIS) via McGregor (1982)
    ;;; branch-and-bound with a bipartite-matching (assignment) relaxation
    ;;; bound (foundations doc §4.2).
    ;;; ====================================================================

Private helpers (`%` prefix per `algebra/CLAUDE.md` "Private helpers"):
- `%mcs-consistent? G H M a b` — induced-feasibility of extending `M` by `(a→b)`.
- `%mcs-frontier G M` — unmapped G-vertices adjacent to `dom(M)` (connectivity).
- `%mcs-compat-graph G H rem-g rem-h compatible?` — builds the bipartite `<graph>`.
- `%mcs-upper-bound G H M rem-g rem-h compatible?` — **user-authored** (§6); the bound.
- `%mcs-search ...` — the recursive B&B driver, closes over a mutable `incumbent`.

Reuse, do not re-roll: `graph-neighbors` (`:192`), `graph-edge?` (`:338`),
`graph-vertices` (`:180`), `graph-setoid`/`setoid-equiv?`/`setoid-member?`
(`algebra/setoid`), `graph-maximum-bipartite-matching` (`:1665`),
`complete-bipartite-graph`-style construction via `make-graph` (`:121`).

---

## 4. Phases (TDD; one commit per phase per `feedback_commit_cadence.md`)

### Phase 0 — Plan + scaffold
- Commit 1 = this plan file (records starting design, per `plans/CLAUDE.md`).
- Add `graph-maximum-common-subgraph` to `.sld` export block (new `;; Common subgraph`
  group after `;; Matching`, `combinatorial-graph.sld:25`).
- Stub the proc raising "not implemented"; add failing test skeleton.
- `make build` green (stub compiles).

### Phase 1 — Induced consistency + brute-force oracle
- Implement `%mcs-consistent?` and a *naive exhaustive* disconnected search
  (no bound, no connectivity) returning a maximum induced correspondence.
- This slow-but-correct version is the **oracle** Phase 3 validates against.
- Tests: `MCCIS(G,G) = |V(G)|` for K3/C4/P4; `MCCIS(K3,P3) = 2` (K3's only
  3-subset is a triangle, P3 is a path — not induced-isomorphic).

### Phase 2 — Connectivity constraint
- Add `%mcs-frontier`; default search seeds per start-pair and extends only along
  the frontier. `(disconnected? . #t)` routes to the Phase-1 search.
- Tests: two-disjoint-edges fixture — `MCCIS = 2` (one edge), but
  `MCCIS … (disconnected? . #t) = 4` (both edges). Pins the semantic difference.

### Phase 3 — Branch-and-bound with the assignment bound  ← user-authored core (§6)
- Implement `%mcs-compat-graph`; wire `%mcs-upper-bound` (USER writes this) and the
  prune `|M| + bound ≤ |incumbent|` into `%mcs-search`.
- **Key correctness test (USER writes this, §6):** for every fixture, the B&B
  result size equals the Phase-1 brute-force result size. This is the
  admissibility regression — if the bound ever under-estimates, this test fails.
- Add a `visited-nodes` counter assertion showing pruning fires (B&B visits
  strictly fewer nodes than brute force on C4×P4).

### Phase 4 — Options, validation, edge cases, docstring
- `compatible?` opt (default `(lambda (u v) #t)`); `validate-opts-keys "graph-maximum-common-subgraph" opts '(disconnected? compatible?)`.
- Edge cases: empty graph → `'()`; no compatible pair → `'()`; single shared
  vertex; self-loop agreement; multi-edge documented-ignored.
- Full structured docstring (Parameters/Returns/Category/Keywords/Examples/See also),
  matching `graph-maximum-bipartite-matching`'s docstring shape.
- Error shape: `(list 'fix "...")` trailing entry per `lattice.scm` precedent.

### Phase 5 — Docs + green build
- `docs/algebra/reference.md` entry (bespoke headings OK per `TODO.md:214`).
- Fixture table with hand-verified values in the PR body.
- `make lint && make covercheck && make ci` all green.

---

## 5. Fixtures (all hand-verifiable — no external oracle needed)

| G | H | MCCIS | Disconnected MCIS | Why |
|---|---|-------|-------------------|-----|
| K3 | K3 | 3 | 3 | identical |
| P4 (path) | P4 | 4 | 4 | identical |
| C4 (cycle) | P4 | 3 | 3 | 3 consecutive C4 vertices induce P3 ⊆ P4; C4 has no induced P4 |
| K3 | P3 | 2 | 2 | triangle vs path: only a shared edge |
| 2·K2 (two edges) | 2·K2 | 2 | 4 | connected caps at one edge; disconnected gets both |

The C4×P4 pair is the pruning-fires fixture (Phase 3 node-count assertion). The
2·K2 pair is the connected/disconnected discriminator (Phase 2).

---

## 6. User-authored pieces (the design-bearing 10 lines)

Two pieces are scaffolded but left as `TODO` for you to write — they are where the
correctness of the whole feature actually lives:

**(a) `%mcs-upper-bound` (Phase 3).** The assignment-relaxation bound. Scaffold:

    (define (%mcs-upper-bound G H M rem-g rem-h compatible?)
      ;; Return an admissible upper bound on the MAX additional pairs
      ;; beyond M that any extension could achieve.
      ;; TODO(you): build the bipartite compatibility graph over rem-g × rem-h
      ;;   (edge iff compatible? holds) and return its maximum-matching size.
      ;;   Reuse %mcs-compat-graph + graph-maximum-bipartite-matching.
      ;;   INVARIANT: must never return less than the true achievable count,
      ;;   or B&B prunes a real optimum. Prove to yourself it over-counts.
      (error "TODO: implement the assignment-relaxation bound"))

Why you: this is the single decision that makes the algorithm both correct
(admissibility) and fast (tightness). A looser bound (`min(|rem-g|,|rem-h|)`) is
also admissible but prunes less — the matching bound is the §4.2-specified choice,
and feeling the admissibility argument is the point.

**(b) The admissibility regression test (Phase 3).** Reference at the top of the
test file, leave the assertion as a TODO:

    ;; TODO(you): for EVERY fixture in §5, assert
    ;;   (= (length (graph-maximum-common-subgraph G H))
    ;;      (length (brute-force-mccis G H)))
    ;; This is the design-property test: it fails the instant the bound
    ;; under-estimates. Everything else can pass while this catches a
    ;; silently-wrong prune.

---

## 7. Explicitly out of scope (v2 / re-open when a consumer surfaces)

- **Weighted objective** (maximize Σ similarity vs. cardinality) — the `compatible?`
  opt is the hook; a `weight` opt + best-by-weight incumbent is the extension.
- **Multi-edge multiplicity** in the induced check.
- **Approximate/anytime** mode (return best-so-far under a node/time budget) —
  mirrors the `set-timeout` pattern; add when graphs exceed exact-feasible size.
- **The `unify.scm` (wile-goast) consumer wiring** — that is wile-goast-side work
  (`TODO.md:147` family), not this library. This plan ships the *capability*; the
  CFG-level-unification layer that consumes it (§3.1: "currently unreachable")
  is downstream.

---

## 8. Verification checklist (per `plans/CLAUDE.md` completion workflow)

- [ ] Branch `feat/algebra-mcs` from `master`; plan = commit 1.
- [ ] Phases 1–5, one commit each, tests green per phase.
- [ ] `make lint && make covercheck && make ci` green locally.
- [ ] Master remote CI green before PR (`gh run list --branch master`).
- [ ] Self-review (Copilot-hat): docstring/code drift, the admissibility invariant
      stated at the bound site, return-shape matches `graph-maximum-bipartite-matching`.
- [ ] PR body cites the §5 fixture table with hand-derived values.
- [ ] Do NOT merge without explicit instruction.
