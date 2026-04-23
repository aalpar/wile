# Algebra Library — Foundations & Development Directions

**Status:** Foundations document — design-level, no implementation proposed yet

**Scope:** Audit of `(wile algebra ...)` libraries against the landscape of algebraic combinatorics; identification of structural gaps; mapping of those gaps onto (1) source code analysis applications (motivated by wile-goast's existing work) and (2) matching / global optimization applications. Output is a prioritized set of development directions, not a single-feature plan.

**Relation to existing plans:**
- Completes what `2026-03-25-algebra-library-design.md`, `2026-04-09-orthogonal-algebra-types.md`, and `2026-04-10-symbolic-algebra-design.md` *shipped*.
- Independently confirms the matrix-algebra gap flagged in `2026-04-16-recurrence-categories-design.md:5` (`matrix_ops blocked on (wile algebra matrix)`).
- Distinct from implementation-tracking plans: this document argues *which* directions are worth funding, not *how* to build any one of them.

---

## Part 1 — Current Coverage

### Shipped libraries

19 `.sld`/`.scm` pairs in `stdlib/lib/wile/algebra/`:

| Library | Lines | Core export | Category |
|---|---:|---|---|
| `setoid` | 81 | `make-setoid` with explicit equivalence | Foundation |
| `order` | 73 | `make-partial-order`, `po-leq?`, `po-monotone?` | Foundation |
| `lattice` | 220 | flat/powerset/product/map lattices, `fixpoint`, `fixpoint/widen` | Foundation |
| `closure` | 124 | closure operators, `closure->closed-lattice` | Foundation |
| `galois` | 47 | Galois connections, `gc-sound?` | Foundation |
| `monoid` | 61 | `make-monoid`, `monoid-fold`, `monoid-power` | Structure |
| `group` | 67 | `make-group`, integer + modular | Structure |
| `semiring` | 125 | boolean / tropical / counting | Structure |
| `ring` | 184 | integer, modular, rational field | Structure |
| `heyting` | 151 | bounded distributive + implication | Structure |
| `boolean` | 154 | complemented distributive lattice | Structure |
| `differential` | 177 | Leibniz-rule-respecting derivation | Structure |
| `interval` | 121 | infinity-aware interval arithmetic | Domain |
| `pareto` | 94 | `dominates?`, `pareto-frontier` | Domain |
| `graph` | 94 | semiring-Bellman-Ford (single-source) | Application |
| `fca` | 273 | concept lattice via NextClosure | Application |
| `category` | 95 | basic compose / identity / endomorphism-monoid | Advanced |
| `rewrite` | 301 | axiom-based term rewriting | Advanced |
| `symbolic` | 611 | theory projections, recursive normalizer, trace | Advanced |

**Total: 3053 lines.** The `symbolic` + `rewrite` pair (912 lines combined) is the heaviest investment and the most differentiated from typical algebra libraries. The foundation layer (setoid → order → lattice → closure → galois) is tight and coherent.

### Structural observations

1. **The lattice/order/closure/Galois/FCA block is genuinely rare** to see implemented together in a single library. It underpins the goast FCA work and the `dataflow.scm` abstract-interpretation framework.
2. **Symbolic rewriting is sophisticated but scoped to *directed* rules** — commutativity is handled by sorting operands (`ssa-rule-commutative`), associativity by one-directional rewrite. No matching modulo theories.
3. **`graph.sld` is misleadingly named** — it is a single-source Bellman-Ford wrapper over a semiring, not graph theory. It does not expose graphs as combinatorial objects.
4. **`category.sld` is very thin** — `compose`, `identity`, `endomorphism-monoid`. No functors, no natural transformations, no adjunctions (despite `galois.sld` being an adjunction).
5. **`group.sld` has only abelian examples** — integer addition, modular arithmetic. No symmetric group, no permutation representation, no group actions.

---

## Part 2 — Gaps in Algebraic Combinatorics

Organized by distance from existing code (Tier 1 = closest, Tier 3 = furthest).

### Tier 1 — Adjacent to existing code

#### 2.1 Incidence algebras & Möbius functions on posets

**Missing:** `mobius-function`, `zeta-function`, `incidence-algebra` on top of `(wile algebra order)`.

**Role:** Central tool of enumerative combinatorics (Rota 1964). Given a poset `P` with order relation ≤, the incidence algebra is functions `f: {(x,y) : x ≤ y} → R` under convolution. The Möbius function `μ(x,y)` is the multiplicative inverse of the zeta function, and the classical inversion formula is:

    g(x) = Σ_{y ≤ x} f(y)    ⇔    f(x) = Σ_{y ≤ x} μ(y,x) · g(y)

**Prerequisite already present:** `order.scm:1-73` exposes `po-leq?`.

**Estimated effort:** ~150 lines. Needs memoization of μ and a ring parameter (wile has rings).

#### 2.2 Free structures

**Status — partial:** Free Boolean algebra on atoms shipped via extraction from wile-goast's `boolean-simplify.scm` — see `plans/2026-04-22-wile-goast-algebra-extraction-design.md`. Entry points `symbolic-boolean-normalize` / `symbolic-boolean-equivalent?` in `(wile algebra symbolic)`. Free-distributive-lattice shipped via §5.5 (as the `free-distributive-lattice` preset in `(wile algebra lattice)`). Free-monoid and free-group remain unshipped; re-open as a follow-up TODO when a consumer surfaces.

**Missing:** `free-monoid`, `free-group`, `free-lattice`, `free-distributive-lattice`.

**Role:** Every universal-algebra construction has a free object — the "maximum possible" structure before quotienting by observed equations. Without free lattices, Dedekind's problem (counting elements of FD(n)) is unreachable. The FCA concept lattice is a quotient of a free structure; naming the free object clarifies what FCA is quotienting.

**Prerequisite already present:** `monoid.scm:1-61`, `lattice.scm:1-220`.

**Estimated effort:** Free monoid ~30 lines. Free distributive lattice via Birkhoff is harder (~200 lines) but leverages §2.5.

#### 2.3 Graph theory as combinatorial object

**Missing:** graph type, chromatic polynomial, Tutte polynomial, spanning trees, matchings, graph automorphisms, graph isomorphism.

**Role:** `graph.sld` *advertises* graph theory but delivers only semiring path computation. True combinatorial graph theory is absent. This blocks: clone detection at the CFG level (graph iso), register-interference analysis (chromatic polynomial), and scheduling (matching).

**Prerequisite already present:** `semiring.scm:1-125`, `graph.scm:1-94` (for path analytics). A separate `(wile algebra combinatorial-graph)` library would coexist.

**Estimated effort:** Substantial. ~500-800 lines for a credible subset.

#### 2.4 Group actions & orbit counting

**Missing:** Symmetric group, cyclic group construction, orbit/stabilizer, Burnside's lemma, cycle index, Pólya enumeration.

**Role:** `group.sld` is a definitional skeleton (`make-group`, `group-op`, etc.) with only additive abelian instances. The whole enumeration-under-symmetry branch is absent. SSA canonicalization in wile-goast *is* a quotient by the register-renaming group but doesn't name itself as such.

**Prerequisite already present:** `group.scm:1-67` has the data definition.

**Estimated effort:** Symmetric group + orbit/stabilizer ~100 lines. Cycle index + Burnside ~150 more.

#### 2.5 Distributive/modular lattice recognition + Birkhoff representation

**Missing:** `distributive?`, `modular?`, `join-irreducibles`, Birkhoff's representation theorem (finite distributive lattice ↔ poset of join-irreducibles).

**Role:** The bridge between order theory and lattice theory. Crucial for dataflow analysis: distributive domains guarantee MOP = MFP (Kildall-Kam-Ullman), meaning the fixpoint solution is *exact*, not merely sound. Without this, `dataflow.scm` in wile-goast cannot distinguish exact from conservative analyses.

**Prerequisite already present:** `lattice.scm:1-220`.

**Estimated effort:** ~100 lines for recognition; Birkhoff ~150 more.

### Tier 2 — Substantial territory

#### 2.6 Integer partitions & Young's lattice

Missing: `partitions-of`, conjugate partition, dominance order, Young's lattice as a poset. Natural addition given `order.sld`.

#### 2.7 Matroids

Missing entirely. Rank function, circuits, matroid duality, Tutte polynomial, matroid intersection algorithm. The cleanest combinatorial structure living on a lattice (the lattice of flats).

#### 2.8 Symmetric functions & Young tableaux

Missing entirely. Schur functions, RSK correspondence, Littlewood-Richardson. Where representation theory meets combinatorics.

#### 2.9 Combinatorial species (Joyal)

Missing entirely. Generating functions, species composition, OGF/EGF. No counting-as-algebra layer despite `counting-semiring` being present.

#### 2.10 Polynomial rings as first-class

`ring.sld` has only `integer-ring` and `modular-ring` (`ring.scm:1-184`). `differential.scm` manipulates polynomials internally but exposes no generic `polynomial-ring` constructor. q-analogs, Hilbert series, and polynomial dataflow domains all require this.

### Tier 3 — Deeper category-theoretic territory

#### 2.11 Category theory beyond compose/identity

Missing: functors, natural transformations, limits/colimits, adjunctions as first-class. `galois.sld` is a special case of adjunction that isn't generalized.

#### 2.12 Hopf algebras / coalgebras

Missing. Modern algebraic combinatorics (symmetric functions as Hopf algebra, Connes-Kreimer rooted trees, Malvenuto-Reutenauer) lives here.

#### 2.13 Simplicial complexes / order complexes

Missing. Order complex of a poset is how Möbius functions are computed topologically — connects to §2.1.

---

## Part 3 — Applications to Source Code Analysis

This section motivates the gaps *operationally*, with reference to wile-goast's existing code. The project-local document `../wile-goast/plans/2026-04-17-algebra-foundations-directions.md` *does not exist yet* but would be the natural consumer. Context is in wile-goast's `CLAUDE.md`.

### 3.1 Strong operational fits

**Möbius inversion → direct-vs-transitive disentanglement.** wile-goast has four posets: dominator trees (`goastcfg/`), subtype lattices (`go-interface-implementors`), call-graph reachability (`goastcg/`), import DAGs. Möbius inversion translates between direct-edge and transitive-closure information *declaratively*. The belief DSL's `(contains-call ...)` predicates stack multiplicatively; normalizing their overlaps is inclusion-exclusion, which is Möbius on the subset lattice. Currently handled ad-hoc.

**SSA canonicalization as group quotient.** `wile-goast/goastssa/prim_canonicalize.go` (`go-ssa-canonicalize`) alpha-renames registers — this is *orbit-representative selection* under the register-renaming group action. `unify.scm`'s `ssa-diff` then compares representatives. `ssa-rule-commutative` in `wile-goast/cmd/wile-goast/lib/wile/goast/ssa-normalize.scm` is another instance — the `S_2` action on binop operands. These are not separate techniques; they are two uses of one construction.

**Distributive lattice recognition → MOP = MFP theorem.** `dataflow.scm` computes MFP via `run-analysis`. Without `distributive?`, there is no way to certify which analyses are exact. The domains in `domains.scm` (reaching-defs, liveness, constant-prop) have different distributivity properties — constant propagation is notably *not* distributive, which is why it loses precision at joins. This gap has real operational consequences for documenting analysis precision.

**Graph isomorphism for CFG/AST clone detection.** `unify.scm`'s tree diff works only on aligned (root-to-root) structures. CFG-level clone detection — same control flow, different names/ordering — needs graph isomorphism. The project's "simplification through unification" thesis is graph-iso-shaped at the CFG layer, and that layer is currently unreachable.

**Category theory / adjunctions beyond Galois.** Abstract interpretation is saturated with adjunctions (Cousot & Cousot 1977). wile has `galois.sld` handling one connection. Composing abstract domains, lifting analyses, and analyses-as-natural-transformations are functorial constructions. `fca-algebra.scm` in wile-goast already shows the pattern: concept lattices *as* algebraic lattices, one construction reused in multiple places.

### 3.2 Moderate operational fits

**RSK / LIS → diff algorithms.** `unify.scm`'s tree diff uses structural alignment; sequence diff (statement lists, parameter lists) reduces to LCS, which reduces to LIS, which connects to Robinson-Schensted-Knuth. Small but concrete gain.

**Matroids → register allocation, instruction scheduling.** Live-range intervals form an interval matroid. Register allocation framed as matroid intersection (rather than graph coloring) is a well-known alternative; unblocks if matroids are added.

**Connes-Kreimer rooted-tree Hopf algebra → AST algebraic structure.** ASTs *are* rooted trees. Connes-Kreimer's coproduct *cuts subtrees*, which is exactly the primitive operation of `ast-transform` and `ast-splice` (in wile-goast's `utils.scm`). Recognizing this could formalize rewrite-rule composition.

**Polynomial rings → polynomial abstract domains.** Abstract interpretation with polynomial invariants needs polynomial-ring arithmetic. `domains.scm` tops out at intervals and sign; a polynomial domain would plug into existing `symbolic.scm` machinery.

### 3.3 Weak/speculative fits

Integer partitions, symmetric functions, species, simplicial complexes: interesting but no immediate application in wile-goast's current scope.

### 3.4 Named-vs-unnamed special cases

Several wile-goast constructions are unnamed special cases of gaps above:

| Current code (file) | General theory | What's lost by not naming it |
|---|---|---|
| `goastssa/prim_canonicalize.go` | Group quotient by register-renaming action | Can't count equivalence classes; can't compose symmetries |
| `ssa-normalize.scm` `ssa-rule-commutative` | `S_2` action on binops | Same construction, different file; doesn't generalize to n-ary commutative ops |
| `dataflow.scm` `run-analysis` (MFP) | MOP vs MFP (distributivity) | No free correctness certificate |
| `boolean-simplify.scm` normalization | Free Boolean algebra on atoms | Works in practice, no statement of what's quotiented |
| `unify.scm` structural diff | Graph iso (CFG) / tree edit distance (AST) | CFG-level unification unreachable |

---

## Part 4 — Matching Problems & Global Optimization

### 4.1 The unifying observation

Many "global optimization over discrete structures" problems reduce to *linear* algebra once the semiring is chosen (Gondran-Minoux; Baccelli et al., *Synchronization and Linearity*):

| Problem | Semiring | Operation |
|---|---|---|
| Shortest path | Tropical (min, +) | Matrix power |
| Longest / critical path | Max-plus | Matrix power |
| Reachability / transitive closure | Boolean (∨, ∧) | Matrix power |
| Path counting | Counting (+, ×) | Matrix power |
| Minimum-cost assignment | Tropical permanent | Permanent (not determinant) |
| Viterbi (max-likelihood path) | Max-product | Matrix power |
| Sum-product (marginals) | (+, ×) on probabilities | Matrix power |

**wile already has the semiring layer** (`semiring.sld`: boolean, tropical, counting). **wile already has one consumer** (`graph.sld`: single-source Bellman-Ford). **What is missing is the matrix algebra layer** — semiring-parameterized matrix multiplication, matrix power, permanent. One abstraction unifies shortest paths, assignment, reachability, Viterbi, and path counting.

### 4.2 Strong fits for matching in source code

**AC-matching / E-unification → `rewrite.scm`, `symbolic.scm`.** `rewrite.scm:72-116` confirmed: commutativity is handled by sorting (canonicalization), associativity by directional rewrite (`rewrite.scm:99-268`). No matching *modulo* AC theory. AC-matching is bipartite assignment between pattern variables and subject operands — matching as combinatorial optimization. Algorithm is NP-hard in general; polynomial for fixed arity. Would enable belief patterns that match modulo algebraic equivalence, not just syntactic shape.

**Maximum common subgraph → unification detection.** `unify.scm` is aligned diff. True code clone detection needs max common subgraph iso — bipartite matching between candidate node pairs, weighted by local similarity (branch-and-bound with assignment relaxation).

**Tropical permanent / Hungarian → cross-version symbol matching.** Matching symbols across two commits for blame, refactor tracking, or semantic diff is bipartite assignment weighted by similarity. Hungarian algorithm in O(n³). `unify.scm` + `fca.scm` already provide similarity scoring; a `tropical-assignment` primitive would plug directly in.

### 4.3 Moderate fits

**Matroid intersection** (Edmonds, polynomial time) generalizes bipartite matching. Scheduling (dependency matroid ∩ resource matroid), register allocation, call-site batching.

**Submodular optimization.** Matroid rank is submodular. Program slicing, test-suite selection ("cover N behaviors with minimum tests"), import minimization are submodular-maximization-under-cardinality problems. Lovász extension bridges discrete submodularity and continuous convex optimization.

**Matching polytope + LP duality.** Max matching = min vertex cover (König, bipartite). Edmonds' odd-set inequalities for general matching. Pure polyhedral combinatorics: matching as linear programming with integrality guaranteed by algebraic structure. wile has no LP infrastructure, but the *algebraic* duality statements are combinatorial and could be stated without an LP solver.

### 4.4 Deeper territory

**Holographic algorithms / Pfaffians (Valiant).** Perfect matching count in planar graphs = Pfaffian of the Tutte matrix, polynomial-time via determinants over a polynomial ring. One of the most striking algebra-unlocks-optimization results.

**Spectral graph matching.** Graph similarity via Laplacian eigenvalues. Umeyama's algorithm for approximate iso. Relevant where exact graph iso is too strict.

**Tropical algebraic geometry.** Newer territory. Tropical polytopes, tropical linear algebra contain shortest-path and assignment problems as instances. Most ambitious connection, least ready.

### 4.5 The AC-matching point, expanded

This is the most operationally relevant matching gap because it connects directly to symbolic rewriting:

- `symbolic.scm:1-611` does recursive normalization via directed rewrite rules.
- Commutativity: sort operands (`ssa-rule-commutative`) — canonicalization trick.
- Associativity: per-rule via `theory-associative-ops` — directional.
- Same variable appearing twice in a pattern (non-linear patterns): not supported.
- Matching modulo a theory (e.g., `f(x, g(y))` against `f(g(a), b)` where `f` is commutative): not supported; requires AC-unification.

**Rewriting modulo a theory = matching in the quotient algebra.** wile's current approach works around the absence of AC-matching via `discover-equivalences` (in `symbolic.scm`), which enumerates normal forms — exponential worst-case. AC-matching would replace the exponential enumeration with polynomial bipartite assignment. For n-ary associative-commutative operators with k pattern variables and n subject operands, the match count equals a partition-enumeration quantity — connecting back to §2.6 (integer partitions).

### 4.6 Stable matching — a three-layer optimization-constraint-optimization

Stable matching (Gale-Shapley 1962) is often described as "two levels of optimization" — local agent preferences and global system outcome. This framing is *almost* right but the middle layer is a **constraint**, not an optimization. The precise decomposition has three layers:

1. **Local optimization (per agent, ordinal).** Each participant has a preference ranking and seeks the best partner they can get. Each agent maximizes position on their own list.

2. **Stability is a constraint, not an objective.** A matching either is stable (no blocking pair: no `(m, w)` where both prefer each other to their current partners) or it isn't. "More stable" has no meaning. Framing stability as optimization obscures this — it is a feasibility condition.

3. **Global optimization inside the stable set.** This is where the second optimization genuinely lives. Conway (1976) proved that **the set of stable matchings forms a distributive lattice** under the natural order "every proposer does at least as well." Different global objectives pick different points:

   - Proposer-optimal (top of lattice) — what Gale-Shapley's proposer-side algorithm finds
   - Receiver-optimal (bottom)
   - Egalitarian / minimum-sum-rank (interior)
   - Rawlsian / minimax-regret (interior)
   - Sex-equal, minimum-regret, etc.

So the clean pairing is **local optimization + selection over a distributive lattice**, with a stability constraint mediating between them. "Bilevel optimization" is the nearest existing term in the optimization literature, but stable matching is not pure bilevel — the middle layer is a fixpoint condition, not a nested optimization.

**Algebraic consequences.** This three-layer structure connects stable matching directly to wile's foundation algebras:

- **Gale-Shapley is a lattice fixpoint algorithm.** Unmatched proposers propose, receivers tentatively accept, iterate to convergence. This is exactly the `fixpoint` primitive in `lattice.scm:1-220`. The algorithm's correctness is the statement "this monotone operator's least fixed point is the proposer-optimal stable matching."

- **Conway's distributive-lattice theorem enables Birkhoff's representation.** By Birkhoff (direction §5.5 of this plan), every finite distributive lattice is isomorphic to the downset lattice of its join-irreducibles. For stable matchings, those join-irreducibles are called **rotations** in the matching literature — they enumerate the minimal "swap moves" one side can make while preserving stability. This is one of the cleanest concrete instances of Birkhoff's representation theorem in applied combinatorics.

- **Tractability follows lattice compatibility.** Proposer-optimal selection is `O(n²)` via Gale-Shapley. Egalitarian stable matching (minimum-sum-rank) is NP-hard in general. The difference: objectives that respect the lattice order are tractable via lattice traversal; objectives that cut across the lattice require combinatorial optimization over a structure that Conway's lattice constrains but does not trivialize.

**Relevance to source code analysis.** Stable matching is not a core code-analysis problem, but it is a **template** for any multi-agent coordination problem with preferences and stability constraints. Concrete candidate uses:

- **Refactoring recommendation with conflicting constraints.** When multiple refactorings could apply to the same code location, stable matching between "refactorings" and "code sites" produces a coherent recommendation set with no blocking pair (no refactoring-site pair that mutually prefer each other over current assignments).
- **Test-to-code matching under coverage preferences.** Matching tests to suspected fault locations with preferences on both sides.
- **Module assignment in package splitting.** §5.5's Birkhoff machinery applies directly: the stable-matching lattice for a module-to-package assignment is distributive, and package-splitting heuristics can traverse it.

**The generalized lesson.** Whenever a problem has the shape "local preferences + a no-blocking-pair style stability + a global criterion," the natural algebra is distributive-lattice optimization, and the natural algorithm is lattice fixpoint. Recognizing this shape lets projects reuse Gale-Shapley-style algorithms instead of reinventing ad-hoc greedy heuristics.

---

## Part 5 — Prioritized Development Directions

Ordered by leverage-to-code ratio.

### 5.1 Priority 1 — Semiring matrix algebra

**Target:** `(wile algebra matrix)` — semiring-parameterized matrix operations.

**Exports (proposed):**

    make-semiring-matrix semiring-matrix?
    semiring-matrix-ref semiring-matrix-shape
    semiring-matrix-add semiring-matrix-mul
    semiring-matrix-power semiring-matrix-closure
    semiring-matrix-permanent
    semiring-matrix->sparse sparse->semiring-matrix

**Unlocks (single library):**

- All-pairs shortest paths (tropical matrix power)
- Transitive closure / reachability (Boolean matrix closure)
- Path counting (counting-semiring matrix power)
- Viterbi (max-product matrix power)
- Minimum-cost assignment (tropical permanent)
- Kleene closure / regex semantics (regular-language semiring)

**Dependencies already shipped:** `semiring.sld`.

**Independent corroboration:** Already flagged as a blocker in `2026-04-16-recurrence-categories-design.md:5` ("matrix_ops blocked on `(wile algebra matrix)` library"). The recurrence-benchmark work needs this; so does any serious optimization application; so does graph-theoretic analysis in wile-goast.

**Estimated effort:** ~300-500 lines dense vs. sparse variants. Moderate.

**Leverage:** **Highest.** One abstraction, 6+ immediate consumers, already independently requested.

### 5.2 Priority 2 — Möbius / incidence algebra on posets

**Target:** Extension of `(wile algebra order)` or new `(wile algebra incidence)`.

**Exports (proposed):**

    make-incidence-algebra incidence-algebra?
    zeta-function mobius-function
    incidence-convolve incidence-invert
    mobius-inversion

**Unlocks:**

- Declarative direct-vs-transitive translation on any poset
- Inclusion-exclusion on Boolean lattices (unified handling of belief-predicate overlaps)
- Chain-counting and rank-function computations
- Algebraic specification of what dominator-tree, subtype-lattice, call-reachability queries are doing

**Dependencies:** `order.sld`, `ring.sld` (μ takes values in a ring).

**Estimated effort:** ~150 lines. Small.

**Leverage:** High — four distinct posets in wile-goast that would benefit immediately.

### 5.3 Priority 3 — AC-matching / E-unification

**Target:** Extension of `(wile algebra rewrite)` or companion `(wile algebra unification)`.

**Exports (proposed):**

    ac-match pattern subject theory        ;; → list of substitutions
    ac-unify t1 t2 theory                   ;; → mgu or #f
    e-match pattern subject theory          ;; general E-matching
    nonlinear-match pattern subject         ;; repeated pattern vars

**Unlocks:**

- Pattern-based beliefs that match modulo algebraic equivalence
- Replacement of exponential `discover-equivalences` with polynomial AC-match
- SSA-rewriting that doesn't depend on canonicalization tricks
- Foundation for equality saturation (e-graphs) if ever wanted

**Dependencies:** `rewrite.sld`, `symbolic.sld`, matrix algebra from §5.1 (bipartite assignment).

**Estimated effort:** ~400-600 lines. Substantial. AC-matching is well-studied; algorithms exist (Eker, Kirchner).

**Leverage:** High — directly extends the heaviest-invested part of the library (symbolic/rewrite) and replaces an exponential workaround with a polynomial algorithm.

### 5.4 Priority 4 — Group actions & Burnside

**Target:** Extension of `(wile algebra group)`.

**Exports (proposed):**

    symmetric-group cyclic-group dihedral-group
    group-action orbit stabilizer
    burnside-count cycle-index
    polya-enumerate

**Unlocks:**

- Explicit naming of SSA canonicalization as orbit-selection
- Unification of `ssa-rule-commutative` and SSA canonicalization as one technique
- Enumeration of code structures modulo symmetry
- Foundation for AC-matching (symmetric group on operand list)

**Dependencies:** `group.sld`.

**Estimated effort:** ~250 lines.

**Leverage:** Medium-high — cleans up several existing wile-goast constructions and is a prerequisite for some forms of AC-matching.

### 5.5 Priority 5 — Distributive/modular lattice recognition + Birkhoff

**Target:** Extension of `(wile algebra lattice)`.

**Exports (proposed):**

    distributive? modular?
    join-irreducibles meet-irreducibles
    birkhoff-representation       ;; distributive lattice → poset of join-irreducibles
    birkhoff-reconstruction       ;; poset → distributive lattice of downsets

**Unlocks:**

- MOP = MFP certification for distributive dataflow domains
- Documentation of which analyses in `domains.scm` are exact vs conservative
- Connection between FCA concept lattices and underlying attribute posets
- Free distributive lattice construction (via Birkhoff)
- **Stable-matching lattices** (Conway 1976) and rotation-based traversal — see §4.6

**Dependencies:** `lattice.sld`, `order.sld`.

**Estimated effort:** ~250 lines (recognition + Birkhoff both ways).

**Leverage:** Medium — operationally matters for analysis correctness claims; also unlocks the §4.6 stable-matching template for any multi-agent coordination problem with preferences and stability constraints.

### 5.6 Priority 6 — Graph theory as combinatorial object

**Target:** New `(wile algebra combinatorial-graph)` — distinct from `graph.sld`.

**Exports (proposed):**

    make-graph graph? graph-vertices graph-edges
    graph-automorphism-group graph-isomorphic?
    chromatic-polynomial tutte-polynomial
    spanning-trees maximum-matching

**Unlocks:**

- CFG-level clone detection
- Register interference analysis (chromatic polynomial)
- Scheduling via matching

**Dependencies:** None that aren't already shipped.

**Estimated effort:** Substantial. ~600-800 lines for a credible subset. Graph iso alone is hard to do well.

**Leverage:** Medium — substantial code investment, but opens a new capability class.

### 5.7 Lower-priority additions

- **Matroids.** `(wile algebra matroid)`. ~300 lines. Unlocks matroid intersection for scheduling.
- **Polynomial rings as first-class.** Extension of `ring.sld`. ~200 lines. Enables polynomial abstract domains.
- **Integer partitions & Young's lattice.** `(wile algebra partition)`. ~150 lines.
- **Category theory extensions.** Functors, natural transformations, general adjunctions. ~400 lines. Enables compositional analysis combinators.
- **Connes-Kreimer Hopf algebra on rooted trees.** ~300 lines. Formalizes AST rewrite composition.
- **Symmetric functions / RSK.** Research-tier. ~500 lines.
- **Submodular optimization.** Greedy approximation framework. ~200 lines.

---

## Part 6 — The Sharpest Single Win

If only one direction were funded, it would be **§5.1 (semiring matrix algebra)**. Rationale:

1. **Already blocked on.** `2026-04-16-recurrence-categories-design.md:5` flags it. This is the second independent request.
2. **Smallest new concept surface.** Matrix multiplication over a ring generalizes cleanly to a semiring — one type, one operation.
3. **Highest consumer count.** 6+ distinct algorithms collapse to "matrix power over semiring S."
4. **Unblocks higher-priority items.** AC-matching (§5.3) uses bipartite assignment = tropical permanent. Graph-theoretic work (§5.6) leans on matrix representations.
5. **Completes `graph.sld` honestly.** The current `graph.sld` is a vectorized special case waiting for its matrix sibling.

Second-sharpest: **§5.2 (Möbius / incidence algebra)**. Smallest absolute effort, four immediate wile-goast consumers, fills the most conspicuous gap in the foundation layer.

---

## Part 7 — Non-Goals

Directions *not* recommended for near-term investment:

- **Symmetric functions, species, tableaux, RSK** beyond LIS for diff — no operational consumer in sight.
- **Tropical algebraic geometry** — research-tier, no consumer.
- **Simplicial complexes & persistent homology** — exotic for code analysis; revisit if a TDA-of-code use case emerges.
- **Vector spaces / linear algebra as algebraic objects** — wile's numeric tower already handles practical linear algebra; no need for an algebraic-structure-level abstraction unless a specific consumer appears.
- **Holographic algorithms** — striking theory, no prospective consumer.

---

## Part 8 — Cross-Cutting Principles

Principles to uphold as directions land:

1. **Name the quotient.** When a construction selects an orbit representative, say so. SSA canonicalization, `ssa-rule-commutative`, Boolean normalization are all quotient selections.
2. **Prefer semiring parametrization.** Matrix power, graph reachability, and pattern counting are the *same algorithm* over different semirings. Parameterize aggressively.
3. **Preserve the Galois-connection spine.** The library's coherence comes from `setoid → order → lattice → closure → Galois → FCA`. New constructions should plug into this spine, not bypass it.
4. **Don't rename existing libraries.** `graph.sld` is misleading but renaming breaks consumers; add `(wile algebra combinatorial-graph)` alongside, and let the names clarify via documentation.
5. **Document what each new library certifies.** Distributive-lattice recognition doesn't just add a predicate; it certifies MOP=MFP for dataflow. Möbius inversion doesn't just compute μ; it formalizes direct-vs-transitive relationships. Each library should state the theorems it brings into scope.

---

## Appendix A — Consumer Map

Which wile-goast code would benefit from each proposed direction:

| Direction | wile-goast consumer |
|---|---|
| §5.1 Semiring matrix | `graph.sld` (would generalize), `callgraph` reachability, `dataflow` product-lattice iteration |
| §5.2 Möbius / incidence | dominator trees, subtype lattice, call reachability, import DAG |
| §5.3 AC-matching | `symbolic.scm`, `ssa-normalize.scm`, belief DSL patterns |
| §5.4 Group actions | `prim_canonicalize.go`, `ssa-rule-commutative`, `boolean-simplify.scm` |
| §5.5 Distributive lattices | `dataflow.scm` `run-analysis`, `domains.scm` precision claims; stable-matching lattice traversal (§4.6) for refactoring/test/package coordination |
| §5.6 Combinatorial graphs | `unify.scm` (CFG-level extension), clone detection |
| Matroids | Register allocation analysis (if added) |
| Polynomial rings | Polynomial abstract domains |
| Category extensions | `fca-algebra.scm`, domain composition |

---

## Appendix B — References

Rota, G.-C. (1964). *On the foundations of combinatorial theory I: Theory of Möbius functions.*
Birkhoff, G. (1940). *Lattice Theory.* (Birkhoff representation, distributive lattices.)
Ganter, B. & Wille, R. (1999). *Formal Concept Analysis.* (Already consumed by `fca.sld`.)
Gondran, M. & Minoux, M. (2008). *Graphs, Dioids and Semirings.*
Baccelli, F. et al. (1992). *Synchronization and Linearity: An Algebra for Discrete Event Systems.*
Cousot, P. & Cousot, R. (1977). *Abstract interpretation.* (Galois-connection foundations.)
Edmonds, J. (1965). *Paths, trees, and flowers.* (Matching polytope, matroid intersection.)
Valiant, L. (2008). *Holographic algorithms.*
Joyal, A. (1981). *Une théorie combinatoire des séries formelles.* (Combinatorial species.)
Connes, A. & Kreimer, D. (1998). *Hopf algebras, renormalization, and noncommutative geometry.* (Rooted-tree Hopf algebra.)
Kildall, G. (1973); Kam, J. & Ullman, J. (1977). (MOP = MFP for distributive dataflow.)
Gale, D. & Shapley, L. (1962). *College admissions and the stability of marriage.* (Stable matching, deferred-acceptance algorithm.)
Conway, J. H. (1976). Noted in Knuth, *Marriages stables* (1976). (Distributive lattice structure of stable matchings.)
Irving, R., Leather, P. & Gusfield, D. (1987). *An efficient algorithm for the "optimal" stable marriage.* (Rotations as join-irreducibles; lattice traversal algorithms.)
