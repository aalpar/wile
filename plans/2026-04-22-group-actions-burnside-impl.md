# §5.4 Group Actions & Burnside — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

---

## Revision Note (2026-04-22)

This plan was revised after an initial draft drifted from the master plan (`plans/2026-04-17-algebra-foundations-directions.md` §5.4). Four design decisions (D1–D4) were ratified during review; the revisions below touch the top-level sections (Architecture, Q1, Q2, Exports, File Structure, `<group>` representation, Task 1.1) but not every downstream code sketch. Per §5.3's post-ship lesson ("Plan specs as design intent; code sketches as hints"), subagents should treat the decisions below as authoritative and **adapt task-level code sketches that still reflect the earlier draft**.

### D1 — Extend `(wile algebra group)` in place (not a new `(wile algebra group-action)` library)

The master plan's target is "Extension of `(wile algebra group)`" with dependency on `group.sld`. All new machinery ships from the existing library. The shipped 3-arg `(make-group op identity inverse)` constructor, the `group-op` / `group-inverse` / `group-identity` accessors, and `group->monoid` / `validate-group` / `with-group` are preserved. The umbrella `algebra.sld` already re-exports the group library; its `;; Groups` block (currently lines 58–62) grows with new exports rather than being duplicated.

### D2 — Literature nomenclature for the group operation and inverse

Field and accessor names follow the shipped library and the canonical literature (GAP, OSCAR, SageMath, Magma, Rotman):

- **Operation**: `op` / `group-op` (shipped), not `compose` / `group-compose` (category-theory register; correct only for permutation groups, misleading for generic groups).
- **Inverse**: `inverse` / `group-inverse` (shipped; matches the noun form used universally in group-theory texts), not `invert` / `group-invert` (verb form).

All occurrences of `compose`/`invert` in this plan's identifiers have been mechanically replaced. Any remaining `compose`/`invert` identifier in task-level sketches is a draft artifact — use `op`/`inverse`.

### D3 — Equality via `<setoid>`, not a bare predicate

The `<group>` record carries a `<setoid>` (from `(wile algebra setoid)`), not a raw `equal?` predicate. This matches the family-wide pattern already used by `validate-partial-order/setoid`. Accessor `group-setoid` returns the setoid; convenience helper `(group-equal? G a b)` applies the setoid's equivalence. Callers that need the raw predicate lift it via `(setoid-equiv? (group-setoid G))`. The `make-group` options alist accepts `(setoid . S)`; absent, it defaults to `default-setoid` (which wraps R7RS `equal?`).

Consequence: any task-level code sketch that uses `(group-equal? G)` as a 1-arg predicate-getter is out of date — rewrite as `(setoid-equiv? (group-setoid G))`. Any `(make-group ...)` call still using the draft's 4-positional `(element? identity inverse op ...)` shape must be rewritten to the 3-positional `(op identity inverse . opts)` shape with `element?` moved into the options alist as `(cons 'element? p)`.

### D4 — §5.5 (lattice/Birkhoff) will inherit this plan's finiteness tiering and output `<locally-finite-poset>`

Out of scope for this plan but noted here so §5.4 decisions that constrain §5.5 are visible:

- The three-tier finiteness pattern (tier-1 enumerated / tier-2 finitely generated / tier-3 opaque) established by §5.4 on `<group>` will be mirrored on `<lattice>` in §5.5 (`finite-lattice?`, `lattice-elements`, optional `lattice-generators` for lattices presented by generators-and-relations).
- `birkhoff-representation` in §5.5 will return a `<locally-finite-poset>` (from `(wile algebra incidence)`), not a `<partial-order>` — finite distributive lattices give finite posets of join-irreducibles where interval enumeration is meaningful and Möbius-on-the-result is directly computable.
- `(wile algebra group)` and `(wile algebra lattice)` should remain equality-compatible via shared `<setoid>` when a consumer constructs both over the same underlying set.

### Closure-getter internal accessors

The existing library already has internal field accessors `group-op-fn` and `group-inverse-fn` (returning closures) alongside public applied-form accessors `group-op G a b` and `group-inverse G a`. Internal plan code that needs the raw closure uses the `-fn` variants; external API callers use the applied forms. `group-op-fn` and `group-inverse-fn` are not exported.

### Rename map (applied throughout)

| Old (draft) | New (revised) | Why |
|-------------|---------------|-----|
| `(wile algebra group-action)` library | `(wile algebra group)` extended in place | D1 |
| `group-compose`, `compose` field | `group-op`, `op` field | D2 |
| `group-invert`, `invert` field | `group-inverse`, `inverse` field | D2 |
| `(group-equal? G)` returning a predicate | `(setoid-equiv? (group-setoid G))` | D3 |
| `group-equal?` as 1-arg accessor | `group-equal? G a b` as 3-arg applied convenience | D3 |
| `equal?` field on `<group>` | `setoid` field on `<group>` | D3 |
| `(make-group element? identity inverse op ...)` | `(make-group op identity inverse (cons 'element? p) ...)` | D1 + D3 |
| `(group-op G)` 1-arg closure getter (internal) | `(group-op-fn G)` | mechanical |
| `(group-inverse G)` 1-arg closure getter (internal) | `(group-inverse-fn G)` | mechanical |

Subagents executing this plan should grep for the "Old" column before writing any code that references the `<group>` record — any hit is a draft artifact that must be translated.

---

**Goal:** Extend `(wile algebra group)` — a Scheme library for abstract groups and group actions — with orbit computation, stabilizers, fixed points, Burnside's lemma, and canonical-representative selection for quotient-by-group-action operations. Per `plans/2026-04-17-algebra-foundations-directions.md` §5.4 ("**Target:** Extension of `(wile algebra group)`" and "**Dependencies:** `group.sld`"), this plan modifies the existing library in place rather than creating a new one.

**Priority:** wile-goast-first (per 2026-04-22 roadmap priority). Three currently-unnamed special cases in wile-goast become named instances of `<group-action>`: register-renaming (`goastssa/prim_canonicalize.go`), binop commutativity (`ssa-normalize.scm` `ssa-rule-commutative`), and any future S_n normalization on n-ary commutative operators.

**Architecture:** Mirror the abstract-algebra pattern established by `(wile algebra ring)` and `(wile algebra incidence)`. The shipped `<group>` record at `stdlib/lib/wile/algebra/group.scm:6-11` is extended with optional fields for finite enumeration and generating sets, preserving the existing 3-arg `(make-group op identity inverse)` constructor so that `ring.scm:44` and `test/wile/algebra-group-test.scm` keep working unchanged. A new `<group-action>` record pairs a group with an action function on a set. Equality is carried via a `<setoid>` (from `(wile algebra setoid)`) rather than a raw predicate, matching the family-wide parameterization pattern already used by `validate-partial-order/setoid`. Finite groups optionally carry an `elements` enumeration; Burnside and orbit operations require that enumeration. Pure Scheme, no Go primitives.

**Tech Stack:** R7RS Scheme record types (`define-record-type`), `(srfi 1)` list ops, `(scheme hash-table)` for equal?-keyed orbit deduplication.

**References:**
- `plans/2026-04-17-algebra-foundations-directions.md` §5.4 — motivation and consumer map
- `plans/2026-04-21-incidence-algebra-impl.md` — template for record-based algebra library
- `plans/2026-04-18-polynomial-library.md` — template for ring-parameterized library
- Armstrong, *Groups and Symmetry* (1988) — reference text for Burnside's lemma, orbit-stabilizer theorem
- Rotman, *An Introduction to the Theory of Groups* (1995) — general group theory reference

---

## Prior art and design lineage

The design choices in this plan (opaque `<group>` and `<group-action>` records with generators as primary representation; BFS-primary orbit enumeration; inverse symmetrization; enumeration-order tie-breaking; deferred symbolic-representation layer for v2) explicitly align with the dominant convention in mature computer algebra systems. This section exists so future work on the library does not drift from the reference points that validated the current shape. If a proposal would diverge from all three of OSCAR / GAP / Magma on a pattern that's common across them, treat it as a red flag requiring justification, not local cleverness.

### Systems we are deliberately imitating

| System | What we inherit from it | Primary citations |
|--------|------------------------|-------------------|
| **OSCAR** (Julia-based CAS, integrates GAP + Singular + polymake + Antic) | Explicit **lazy G-set** concept ("we obtain the orbit as a lazy G-set; actual computations only happen when needed"). **Generic groups** — user supplies arbitrary objects + multiplication function, represented opaquely — is the pattern behind our Q2 resolution. **fp-groups** and **pc-groups** (confluent rewriting systems) set the v2 trajectory for symbolic representation. | Decker, Eder, Fieker, Horn, Joswig. *The Computer Algebra System OSCAR: Algorithms and Examples.* Springer, 2024. Horn et al. ["Group theory in OSCAR"](https://arxiv.org/abs/2404.05871), arXiv:2404.05871, 2024. [Linz 2025 presentation](https://lgoe.li/public/2025-Linz-DMV-OEMG.pdf) (direct "lazy G-set" language). |
| **GAP** (Groups, Algorithms, Programming — the reference CAS for computational group theory) | **BFS-from-generators** as the primary orbit algorithm (Hulpke's algorithm signature — "generating set + comparison + image function" — matches our Q4 inputs exactly). **Opaque group types** (`FreeGroup`, `FpGroup`, `PermutationGroup`, `PcGroup`) — user never sees representation; operations work uniformly through the generator protocol. **BSGS** (base and strong generating sets) and **Schreier trees** are v2 optimizations we defer but whose conceptual foundation is present here. | [GAP Reference Manual, Chapter 43: Permutation Groups](https://docs.gap-system.org/doc/ref/chap43.html). Hulpke, ["Computing with group orbits"](https://www.math.colostate.edu/~hulpke/talks/polyhedralpost.pdf), Montréal 2006. Hulpke, [*Abstract Algebra in GAP*](https://www.math.colostate.edu/~hulpke/CGT/howtogap.pdf). |
| **Magma** (commercial CAS, University of Sydney) | **Word groups** — every `G` has a free group `W` on its generators plus a homomorphism `W → G`; elements represented symbolically. **Straight-line programs (SLP)** — DAG representation of compositions, evaluated on demand. **Probabilistic orbit size estimation** for cases where enumeration is infeasible (`MaxSize`/`NumberCoincidences` parameters return bounds + estimate, or refuse). This is the explicit "opaque until enumeration is justified" pattern. | [Magma Handbook — Presentations](http://magma.maths.usyd.edu.au/magma/handbook/text/672). [Magma Handbook — Matrix Group Actions](https://magma.maths.usyd.edu.au/magma/handbook/text/694). Cannon et al., [*The Magma Algebra System I: The User Language*](https://www.williamstein.org/people/cannon/magma-the-language.pdf), J. Symbolic Computation 24 (1997), 235–265. |
| **libsemigroups** (C++ specialty library) | **Froidure-Pin algorithm** — BFS over left and right Cayley graph from generators; enumerates a finite semigroup lazily, tracks a word for each element, recomputes on demand. Validates that BFS-from-generators is the right primary strategy even in specialty settings. | Froidure & Pin, "Algorithms for computing finite semigroups," in *Foundations of Computational Mathematics* (1997), Springer. |
| **Knuth-Bendix completion** (term rewriting) | Trajectory for v2 symbolic representation: infinite groups like `subgroup-generated Z '(3)` = 3Z need confluent rewriting to give finite canonical normal forms. The "shorter-first critical-pair processing" heuristic is itself BFS, reinforcing the strategy convergence. | Knuth & Bendix, "Simple word problems in universal algebras," in *Computational Problems in Abstract Algebra* (1970), Pergamon. [Wikipedia: Knuth–Bendix completion](https://en.wikipedia.org/wiki/Knuth%E2%80%93Bendix_completion_algorithm). |

### Systems we deliberately do *not* imitate

| System / pattern | Why we diverge |
|------------------|----------------|
| **Schreier-Sims / BSGS directly in v1** | GAP and Magma both use these for large permutation groups; we defer to v2. Rationale: our v1 consumers (S_2 on binops, small cyclic groups for tests) do not need the compact representation; scaffolding cost exceeds benefit. Documented as v2 in Future Extensions. |
| **Full fp-group / confluent rewriting in v1** | OSCAR's pc-groups and GAP's `FpGroup` ship rewriting machinery for symbolic normal forms. We defer to v2. Rationale: requires Knuth-Bendix completion infrastructure; v1 consumers (shippable finite and elementary infinite groups) don't need it. |
| **Probabilistic orbit size estimation** | Magma's approach for orbits too large to enumerate. We defer to v2. Rationale: v1 consumers have small orbits; silent non-termination is preferred over probabilistic answers in a correctness-first library. |
| **Pure "require total order" tie-breaking** (GAP's convention) | GAP treats ties in the caller's comparator as caller bugs. We follow Simulink/Sage and supply an implementation tie-breaker (Q6). Rationale: our consumers include lattice-valued and projection-based orderings where partial orders are common; failing loudly on ties would be brittle. |

### Convergence check

If a future refactor or extension would:
- Replace the `generators` field with something implicit / inferred,
- Replace BFS-from-generators with "iterate-all" as the primary strategy,
- Add a Schreier-Sims-shaped layer without keeping the generator-set protocol intact,
- Introduce a "canonical representative" operation that raises on ties instead of breaking them,

then it is diverging from three independent mature systems converging on the same design. That's not necessarily wrong but is a signal to stop and verify the motivation rather than proceed.

---

## Resolved design decisions (confirmed 2026-04-22)

Captured from user Q&A; further Q&A in progress for Q4, Q5, Q7, Q8 below.

### Q1: Library location — **extend `(wile algebra group)` in place** (resolved by master plan)

Per `plans/2026-04-17-algebra-foundations-directions.md` §5.4: "**Target:** Extension of `(wile algebra group)`" and "**Dependencies:** `group.sld`". The master plan treats this as an additive change to the shipped library, not a new library. This revision follows that direction.

Consequences:

- No new `(wile algebra group)` library. All new machinery (record extensions, `<group-action>`, presets, orbit/stabilizer/Burnside) ships from the existing `(wile algebra group)`.
- The shipped 3-arg `(make-group op identity inverse)` constructor at `group.scm:13` is preserved as the public entry point. New optional fields (`element?`, `setoid`, `order`, `elements`, `generators`) are supplied via an options alist tail, keeping `ring.scm:44`'s `(make-group (ring-plus-fn R) (ring-zero R) (ring-negate-fn R))` call and the existing test suite untouched.
- Existing exports (`group-op`, `group-identity`, `group-inverse`, `group->monoid`, `validate-group`, `with-group`) are preserved. Nothing in the shipped API is removed or renamed.
- The umbrella `algebra.sld` already re-exports the group library at lines 58–62; that block grows with new exports rather than being replaced.

**Why not a separate `(wile algebra group)` library?** The master plan treats groups, actions, orbits, and Burnside as one coherent unit ("group theory", §5.4 title: "Group actions & Burnside"). Splitting into two libraries would force every consumer that wants Burnside counting to import both, with no clean separation of concerns — `<group-action>` immediately reaches back into `<group>` for identity, op, inverse, and generators. OSCAR, GAP, SageMath, and Magma all expose group actions from the same namespace as the group type; splitting would diverge from literature convention without offset benefit.

### Q2: Element representation — **abstract / opaque** (resolved: option a)

User semantic model: *opaque type instances are arguments to their respective operations; decomposition operations break them into other sub-types; composition operations turn them into other opaque forms.*

Applied to this library:

- `<group>` and `<group-action>` are opaque record types; consumers don't inspect their fields directly.
- **Decomposition operations** (produce sub-types): `group-identity` → element; `group-elements` → list of elements; `group-order` → integer; `group-setoid` → `<setoid>`; `group-action-group` → `<group>`.
- **Composition operations** (produce new opaque forms): `make-group`, `subgroup-generated`, `product-group` → new `<group>`; `make-group-action`, `product-action`, `conjugation-action` → new `<group-action>`.
- **Element-valued operations** (take and return element types, not the opaque group): `group-op`, `group-inverse`, `group-equal?`, `group-action-apply`. These close over the opaque type but operate on its elements. `group-equal?` is a 3-arg convenience that delegates to `(setoid-equiv? (group-setoid G))` — callers who need the raw predicate can lift it via `(setoid-equiv? (group-setoid G))` directly.

Elements themselves (integers for Z_n, vectors for S_n, pairs for product groups) remain plain Scheme values — the opacity boundary is at the `<group>` / `<group-action>` record, not at the element layer.

### Q3: Permutation representation for S_n — **vector** (resolved: option a)

`perm[i]` = image of i. Identity is `#(0 1 ... n-1)`. Composition `(p∘q)[i] = p[q[i]]`.

**Citation:** Matches the array-indexed image convention used in mature CAS: GAP's permutation datatype stores `(i, i^perm)` pairs internally with O(1) image lookup (see GAP Reference Manual [Chapter 42 — Permutations](https://docs.gap-system.org/doc/ref/chap42.html), "A permutation is a bijection of the positive integers ... internally Wile stores the image of each point"). SageMath's `sage.combinat.permutation.Permutation` exposes permutations as 1-indexed lists with `[i]` giving the image; vectors are the 0-indexed array equivalent. Magma likewise represents permutations internally as image arrays. Choosing vector over list in Wile optimizes for the O(1) `(p∘q)[i] = p[q[i]]` composition hot path while preserving the same external conceptual model.

### Q4: Orbit enumeration strategy — **hybrid, BFS-primary with iterate-all fallback** (resolved: option c)

`<group>` carries optional `generators` and optional `elements`. `orbit` picks strategy at call time:

- **BFS from generators** (primary): used when `(group-generators G)` is non-`#f`. Supports infinite-but-finitely-generated groups (e.g., Z acting on Z/nZ) where the orbit is finite. Matches GAP/SageMath convention.
- **Iterate-all** (fallback): used when `generators` is `#f` but `elements` is present. Compatible with finite groups constructed without explicitly supplying generators.
- **Error** if neither is available.

**Rationale:** The user's Z/R question exposed that `group-elements`-only enumeration conflates two distinct preconditions — "G is finite" vs "orbit(x) is finite." BFS separates them. R is correctly left at element-level ops only (uncountable, not finitely generated as an abstract abelian group — no finite generating set has full span; no algorithmic enumeration possible regardless of strategy). Z, free groups, Z_n × Z become usable at the algorithmic layer whenever their orbits on the target set are finite.

**Prior art convergence:** GAP, Magma, and OSCAR all use BFS-from-generators as the primary orbit algorithm, and all three treat groups as opaque types with generating sets as the canonical representation. OSCAR explicitly calls this the "lazy G-set" pattern. Our adoption is not an independent invention; it is deliberate alignment with three mature CAS. See Prior Art section above.

**Implementation detail — inverse symmetrization:** BFS explores `y → {s·y : s ∈ gens}` only, which is the semigroup closure under `gens`. For finite-group action on finite orbits this equals the orbit; for infinite-group action it may not (Z with gens=`(1)` on Z traverses only non-negative integers). The library internally symmetrizes: `gens* = gens ∪ {inverse(g) : g ∈ gens}` with dedup via `group-equal?`. BFS uses `gens*`. Cost: one up-front `inverse` per generator. Users pass natural (non-symmetric) generating sets; the library handles inverses.

**Preset generator lists:**

| Preset | Generators | Rationale |
|--------|-----------|-----------|
| `trivial-group` | `'()` | Identity alone; empty generating set |
| `cyclic-group n` | `'(1)` | 1 generates Z_n |
| `symmetric-group n` | `(list transposition-0-1 n-cycle)` | Standard generating pair for S_n |
| `product-group G1 G2 ... Gn` | For each factor i: `(list id1 ... id[i-1] g id[i+1] ... idn)` for each `g ∈ gens(Gi)`. Per Q8 resolution: proper n-lists, not cons pairs. Implementation via internal `inject-at-index` helper (Phase 2 Task 2.5). | Variadic; flat-list element shape |
| `subgroup-generated G gens` | `gens` | As passed |

### Q5: Burnside preconditions — **finite-group? + caller list-X + `enumerate-finite-group` helper** (resolved: option b)

**Rule:** `burnside-count action X-elements` has two preconditions:
1. `(finite-group? (group-action-group action))` must return `#t` — i.e., `(group-elements G)` is populated.
2. `X-elements` is a finite Scheme list (caller's contract; library does not verify finiteness of the list itself).

On violation: raise a precondition error naming the failed precondition. When `finite-group?` fails, the error message cites `enumerate-finite-group` as the recovery path — users who have a finitely-generated-but-not-enumerated group can promote it first.

**New helper** `enumerate-finite-group G [#:max-size N]` — promotes a tier-2 `<group>` (generators only, no `elements`) to tier-1 (`elements` populated) via BFS closure from generators (symmetrized under inversion). Optional `max-size` safety cap raises if the enumeration discovers more than N elements; without the cap, BFS on an infinite group loops forever. Callers are responsible for knowing their group is finite; the library does not try to decide it.

**Rationale:**
- Burnside's formula `|X/G| = (1/|G|) Σ|X^g|` intrinsically requires both G and X finite. Not a defensible place to generalize.
- Separating Burnside's check from the enumeration-promotion utility matches OSCAR's and GAP's convention: `Size(G)` and related operations trigger enumeration when needed; user doesn't hand-roll it. Aligns with the Prior Art section's "lazy until operation requires materialization" pattern.
- `max-size` cap protects against "thought it was finite, wasn't" accidents — the primary risk mode for `enumerate-finite-group`. Optional rather than mandatory because users who know their group is small don't want to guess the cap.
- Stream / procedure X-input protocol (option c) deferred: no v1 consumer needs it; materialization is cheap for v1 Burnside use cases (necklace counting, small orbit spaces).

### Q8: Product group / product action API — **variadic, flat proper list representation** (resolved: option C)

**Rule:** `product-group` and `product-action` are variadic; elements of the product are proper Scheme lists of length n (where n is the number of factors). No `product-group-pair` binary helper — the variadic form is the single entry point.

```scheme
(product-group)                  ; ⇒ (trivial-group)
(product-group G1)               ; ⇒ G1  (identity on n=1)
(product-group G1 G2)            ; elements: (a b)     — proper 2-list
(product-group G1 G2 G3)         ; elements: (a b c)   — proper 3-list
```

Equality on elements defaults to `equal?`, which compares proper lists structurally. Component access via `(list-ref elt i)` or `(car elt)` / `(cadr elt)` / `(caddr elt)`.

**Why not pairs:** cons-pair nesting (`fold-right cons`) produces improper/dotted lists for n ≥ 3 (`(a b . c)`, `(a b c . d)`), which violate Scheme list conventions — `length`, `map`, `for-each` error on them. Pairs are reserved in Scheme idiom for single `(car . cdr)` relationships (syntax-rules literals, variadic arguments), not for fixed-arity tuples.

**Why not vectors (option D):** Vectors would give O(1) component access, but wile's v1 consumers (small n; structural pattern-matching on components) benefit more from proper-list idiom than from indexed access. If a future consumer needs O(1) access across many components, `product-group/vector` can be added without breaking the list-based API.

**Prior-art convergence:** GAP's `DirectProduct`, SageMath's `DirectProduct`, Magma's `DirectProduct`, and OSCAR's `direct_product` are all variadic with flat-tuple element representation. None use nested pairs. See Prior Art section.

**Generator construction under flat list:** For `(product-group G1 ... Gn)`, generators are `{inject(g, i) : i ∈ [0, n), g ∈ gens(Gi)}`, where `inject(g, i)` builds an n-list with `g` at index i and identities elsewhere. The v1 plan includes an internal `inject-at-index` helper.

### Q7: Burnside output verification — **divisibility check in `burnside-count`; explicit `verify-action` helper deferred to v2** (resolved: option b + documented deferrals)

**Rule:** `burnside-count` computes `q = sum / |G|` and verifies `|G|` divides `sum` (i.e., `q · |G| = sum`). On failure, raises with a message naming the likely cause: the caller's `act` function is not a genuine group action, or the group's `op` is inconsistent, or X is not closed under the action. O(1) cost beyond the sum computation.

**Rationale:** The orbit-stabilizer theorem guarantees `Σ_{g ∈ G} |X^g| = |G| · |X/G|` for any genuine group action — the sum is always divisible by `|G|`. Non-divisibility is a free, precise signal of malformation. Returning a raw quotient (option a) would yield a silently-wrong integer for malformed actions, with no diagnostic.

**Prior-art convergence:** Computer algebra systems validate group actions at the point of first observable inconsistency rather than via an up-front axiom check:

- **GAP** — orbit algorithms detect malformation during BSGS (base and strong generating set) construction; a violated Schreier lemma invariant raises immediately. See GAP Reference Manual [Chapter 43 (Permutation Groups)](https://docs.gap-system.org/doc/ref/chap43.html), sections on stabilizer chain construction. User-facing `Orbit(G, x, action)` inherits validation through the generic "ascending orbit" algorithm's consistency invariants.
- **SageMath** — `sage.groups.perm_gps.permgroup.PermutationGroup.orbit()` raises `TypeError` when an action yields a non-group-element image; `OrbitalStructure` in the MajoranaAlgebras GAP package ([docs](https://docs.gap-system.org/pkg/majoranaalgebras/doc/chap6.html)) similarly errors on malformed seed data.
- **Magma** — documented behavior in the [Matrix Group Actions handbook chapter](https://magma.maths.usyd.edu.au/magma/handbook/text/694): "If `y` is not in any `G`-orbit of the natural `G`-set, an error is raised." Silent incorrect results are treated as defects.

Wile's divisibility check is the equivalent "first observable inconsistency" validator for Burnside-shaped computations: the orbit-stabilizer identity's failure is the first cheap observable when an action misbehaves. Aligning with this convention keeps malformation *loud* rather than silent, which is the correctness-preserving default across the CAS reference points.

**Docstring language for `burnside-count`:** "This is a partial sanity check. It catches the generic malformation symptom (non-divisibility) but does not prove the action is valid — a malformed action may coincidentally produce a divisible sum. Users needing thorough verification should call `verify-action` (v2)."

**What was considered and deferred:**

- **Option (c) — probabilistic action-axiom spot-check.** Verify `act(op(g, h), x) = act(g, act(h, x))` on k randomly-sampled `(g, h)` pairs across X. Cost: O(k · |X|) per `burnside-count` call. Catches malformations that happen to produce divisible sums. Rejected for v1 as per-call overhead: v1 consumers have small `|X|`, so divisibility failure is sufficient and (c)'s cost would dominate. More useful as an **explicit, opt-in helper** users call once when they want confidence, rather than as a silent tax on every Burnside call.

- **`verify-action` (v2 helper).** Explicit procedure the user calls when they want thorough verification. Proposed signature:
  ```scheme
  (verify-action action X-elements [#:sample-size K] [#:full? #f])
  ```
  Checks:
  1. Identity: `act(e, x) = x` for all `x ∈ X-elements`.
  2. Closure: `act(g, x) ∈ X-elements` for all `g ∈ group-elements G` and `x ∈ X-elements`.
  3. Compatibility: `act(op(g, h), x) = act(g, act(h, x))` for either (a) `K` random pairs when `full?` is `#f` (default), or (b) all `|G|² · |X|` triples when `full?` is `#t`.

  Returns `#t` on success; raises a structured error naming the first violation. Complementary to `burnside-count`: `burnside-count` is cheap and catches most malformations at point of use; `verify-action` is expensive and proves the action valid when called.

  Tracked in Future Extensions section. No per-call overhead in `burnside-count`; users opt in explicitly via `verify-action` calls before they trust a new action construction.

### Q6: `orbit-representative` comparator — **required caller-supplied `<?` with implementation tie-breaker** (resolved)

**Rule:** `orbit-representative action x less?` requires a caller-supplied `<?` procedure. When `less?(a,b)` and `less?(b,a)` are both `#f` but `a` and `b` are `equal?`-distinct (the ordering is not strictly total on the orbit — e.g., lattice partial order, projection-based ordering), the implementation breaks ties using **order of discovery during orbit enumeration** (first-seen wins). Under the Q4-resolved hybrid strategy, this is BFS discovery order when generators are present (the typical case for preset groups) and group-elements iteration order otherwise. Falls out naturally from the `fold min` implementation; no extra machinery needed.

**Rationale:** Positions this library in the same convention as Simulink (4-tier fallback ending in block-creation order as "deterministic but implementation-dependent"), SageMath's index-stringification idiom ("indices stringified so all objects are comparable in a deterministic way"), and orbit-search in deterministic planning. Opposite convention to GAP, which demands strictly total orders from callers and treats ties as caller bugs — brittle for lattice-valued orderings which are common in our consumer domain (dataflow analysis, symbolic rewriting).

**Tie-breaker value:** Per user clarification, each element acquires a tie-breaker value implicitly — its position in the orbit enumeration sequence. This is a system implementation detail, not a mathematical property of the group action, and is explicitly labeled as such in the docstring.

**Surveyed comparables (2026-04-22):**

| System | Convention | Citation |
|--------|-----------|----------|
| Simulink (MathWorks) | First-come-first-served / block creation order as final tie-breaker | [Control and Display Execution Order](https://www.mathworks.com/help/simulink/ug/controlling-and-displaying-the-sorted-order.html) |
| GAP | Requires total order; lex-min; no tie-breaker layer | `CanonicalRightCosetElement`, `MinimalElementCosetStabChain` (GAP Reference Manual Ch. 43); Hulpke, *Constructing Transitive Permutation Groups* |
| SageMath `IntegerVectorsModPermutationGroup` | Lex-max under caller-supplied total order | [Sage docs](https://doc.sagemath.org/html/en/reference/combinat/sage/combinat/integer_vectors_mod_permgroup.html) |
| SageMath general | Stringify indices to manufacture total orders | Sage modules documentation |
| Jackrabbit Oak | "Unspecified but stable" per-instance (internal; less informative) | [Node State Model](https://jackrabbit.apache.org/oak/docs/architecture/nodestate.html) |

Wile's choice sits in the Simulink camp: require `<?`, fall back to enumeration-order when `<?` is not total. Document the fallback explicitly in `orbit-representative`'s docstring. See Prior Art section above for the broader system-design alignment.

---

## Context

### Consumers (from `plans/2026-04-17-algebra-foundations-directions.md` §3.4)

| wile-goast site | Currently | After shipping |
|-----------------|-----------|----------------|
| `goastssa/prim_canonicalize.go` (`go-ssa-canonicalize`) | Go code alpha-renames SSA registers without naming the group | Named as the S_n register-renaming action; Scheme callers can reason about it as a quotient |
| `ssa-normalize.scm` `ssa-rule-commutative` | Sorts two operands | Rewritten as `orbit-representative` under the S_2 action on binop operands |
| Future n-ary commutative rewrite rules | Would need ad-hoc sort each time | Single `orbit-representative` call parameterized by action |

Shipping this library removes wile-goast's need to come back to wile for group-theoretic primitives when it encounters further symmetry problems (e.g., algebraic simplification modulo a symmetry, counting distinct CFG shapes modulo graph automorphism, canonicalizing under S_n for n-ary commutative operators).

### What this does *not* ship

Distinctions worth flagging so future agents don't attempt scope creep:

- **Large permutation groups (Schreier-Sims machinery).** v1 assumes enumerable groups. Register-renaming for realistic SSA (|registers| ≫ 8) cannot use `orbit-representative` via enumeration; that consumer keeps its existing canonicalization-by-selection implementation. The library names what it's doing but doesn't replace it. Schreier-Sims, strong generating sets, coset enumeration are v2.
- **Group presentations by generators and relations.** v1 takes groups as concrete structures (identity, inverse, op). Presentations (`⟨a, b | a² = b² = (ab)³ = 1⟩`) are v2.
- **Character theory / representations.** Not a near-term consumer.
- **Subgroup lattice, normal subgroups, quotient groups.** Only `subgroup-generated` (BFS closure from a generating set) ships in v1; the general lattice structure is v2.

---

## Scope

### In scope (v1)

- `<group>` abstract type with identity, inverse, composition, element predicate, equality
- `<group-action>` abstract type pairing a group with an action on a set
- Enumerative algorithms: `orbit`, `stabilizer`, `fixed-points`, `orbit-representative`
- Burnside's lemma: `burnside-count` for finite group action on finite set
- `subgroup-generated` — BFS closure from a generating set
- Preset groups: `trivial-group`, `cyclic-group` (Z_n), `symmetric-group` (S_n for small n), `product-group`
- Preset actions: `trivial-action`, `permutation-action`, `regular-action`, `conjugation-action`, `product-action`
- Umbrella re-export via `stdlib/lib/wile/algebra.sld`

### Out of scope (deferred to v2 / never)

- Schreier-Sims, strong generating sets, coset enumeration
- Group presentations (generators + relations)
- Character theory / group representations
- Full subgroup lattice
- Homomorphisms, quotient groups
- Dihedral, alternating, or general matrix groups as presets
- Callback-based `orbit-representative` for non-enumerable groups

---

## Open design questions (user-review requested before Phase 1)

**All design questions (Q1–Q8) resolved.** See "Resolved design decisions" above. Plan is ready to execute.

---

## Exports

Total: 7 existing (preserved) + ~28 new = ~35 exports from `(wile algebra group)`.

```scheme
;; (wile algebra group) — exports (existing, preserved verbatim)
(make-group group?
 group-op group-identity group-inverse
 group->monoid
 validate-group with-group

 ;; NEW — extended introspection on <group>
 group-element? group-setoid group-equal?
 group-order group-elements group-generators
 finite-group? finitely-generated-group?
 subgroup-generated subgroup? enumerate-finite-group

 ;; NEW — <group-action> record
 make-group-action group-action? group-action-group
 group-action-apply group-action-set-element?

 ;; NEW — orbit / stabilizer / Burnside
 orbit orbit-representative stabilizer fixed-points
 burnside-count

 ;; NEW — preset groups
 trivial-group cyclic-group symmetric-group product-group

 ;; NEW — preset actions
 trivial-action permutation-action regular-action
 conjugation-action product-action)
```

**Existing exports preserved (no rename, no signature break):**

| Export | Shipped signature | v1 revision |
|--------|-------------------|-------------|
| `make-group` | `(op identity inverse)` — 3 positional | Extended to `(op identity inverse . opts)` — options alist appended; 3-arg call unchanged |
| `group-op` | `(G a b)` | unchanged |
| `group-identity` | `(G)` | unchanged |
| `group-inverse` | `(G a)` | unchanged |
| `group->monoid` | `(G)` | unchanged |
| `validate-group` | `(G samples)` | unchanged |
| `with-group` | `(G (op id inv) body ...)` | unchanged |

---

## File structure

**Modified files (no new library files):**

| Path | Change |
|------|--------|
| `stdlib/lib/wile/algebra/group.sld` | Extend `export` block with new identifiers; add `(import (wile algebra setoid))` alongside existing `(import (wile algebra monoid))` |
| `stdlib/lib/wile/algebra/group.scm` | Extend `<group>` record with optional fields; add `<group-action>` record; add presets, algorithms, orbit/stabilizer/Burnside machinery |
| `stdlib/lib/wile/algebra.sld` | Extend existing `;; Groups` export block (lines 58–62) with new names — no new `(import (wile algebra X))` line required |
| `test/wile/algebra-group-test.scm` | Add new `test-group` blocks alongside existing five; existing tests remain unchanged as regression check |
| `TODO.md` | Mark §5.4 entry as shipped; add Done entry |
| `plans/CLAUDE.md` | Move entry from Open Tier A → Completed |

---

## Representation

### `<group>` record — extended in place

The shipped record at `group.scm:6-11` has three fields: `op-fn`, `identity`, `inverse-fn`. The revision adds five optional fields (`element?`, `setoid`, `order`, `elements`, `generators`), all defaulting to `#f` except `setoid` which defaults to `default-setoid` (from `(wile algebra setoid)`, wrapping R7RS `equal?`). The private constructor grows to 8 fields; the public `make-group` remains a 3-arg function (with options alist tail) so that every existing caller — `ring.scm:44`, `test/wile/algebra-group-test.scm`, and any embedder code — continues to work without change.

```scheme
(define-record-type <group>
  (%make-group op-fn identity inverse-fn
               element? setoid order elements generators)
  group?
  (op-fn       group-op-fn)           ;; unchanged — internal accessor
  (identity    group-identity)        ;; unchanged — public accessor
  (inverse-fn  group-inverse-fn)      ;; unchanged — internal accessor
  ;; NEW — optional metadata
  (element?    group-element?)        ;; predicate or #f
  (setoid      group-setoid)          ;; <setoid>; defaults to default-setoid
  (order       group-order)           ;; exact integer or #f (infinite / unknown)
  (elements    group-elements)        ;; list or #f (not enumerated)
  (generators  group-generators))     ;; list or #f (not finitely generated)
```

Public constructor: existing 3-arg signature preserved; options alist appended:

```scheme
(define (make-group op identity inverse . opts)
  "Construct a group from binary operation OP, IDENTITY, and INVERSE function.
Optional trailing alist entries: (element? . P), (setoid . S), (order . N),
(elements . LIST), (generators . LIST). Absent options default to #f except
SETOID which defaults to DEFAULT-SETOID (wraps R7RS equal?)."
  (let ((elt?  (assv-or opts 'element?   #f))
        (setd  (assv-or opts 'setoid     default-setoid))
        (ord   (assv-or opts 'order      #f))
        (elts  (assv-or opts 'elements   #f))
        (gens  (assv-or opts 'generators #f)))
    (%make-group op identity inverse elt? setd ord elts gens)))

(define (assv-or opts key fallback)
  (let ((p (assv key opts)))
    (if p (cdr p) fallback)))
```

**Equality — setoid-delegated** (per D3, consistent with `validate-partial-order/setoid` and the family-wide setoid pattern):

```scheme
(define (group-equal? G a b)
  "Apply G's equality predicate to A and B. Delegates to (setoid-equiv? (group-setoid G))."
  ((setoid-equiv? (group-setoid G)) a b))
```

Callers who need the raw predicate lift it once: `(setoid-equiv? (group-setoid G))`.

**Finiteness predicates:**

```scheme
(define (finite-group? G)
  (and (group-order G) (group-elements G) #t))

(define (finitely-generated-group? G)
  (and (group-generators G) #t))
```

**Construction examples:**

```scheme
;; Backward-compatible 3-arg form — shipped API, unchanged behavior.
(make-group + 0 -)                               ; Z under + (no finiteness metadata)

;; Finite group with explicit enumeration and numeric setoid.
(make-group (lambda (a b) (modulo (+ a b) 3)) 0 (lambda (k) (modulo (- 3 k) 3))
            '(element? . integer?)
            `(setoid . ,numeric-setoid)          ; from (wile algebra setoid)
            '(order . 3)
            '(elements . (0 1 2))
            '(generators . (1)))

;; Infinite cyclic — Z under addition. Only BFS-from-generators works at the algorithmic layer.
(make-group + 0 -
            '(element? . integer?)
            `(setoid . ,numeric-setoid)
            '(generators . (1)))

;; Uncountable — R under addition. Element-level ops only; algorithmic layer rejects this group.
(make-group + 0 -
            '(element? . real?)
            `(setoid . ,numeric-setoid))         ; no elements, no generators — by design
```

**Why setoid, not bare predicate?** (D3 rationale)

The family already parameterizes equality via `<setoid>`: `default-setoid`, `numeric-setoid`, `string-setoid`, `eqv-setoid`, plus the `validate-partial-order/setoid` extension pattern. Carrying a bare `equal?` procedure on `<group>` would fork the family's convention: lattices, orders, and setoids would use `<setoid>`; groups alone would use a raw predicate. Consumers building cross-structure objects (e.g., a ring's additive group inheriting the ring's element equality) would need an ad-hoc bridge. Delegation via `(group-setoid G)` lets a `<group>` share an equality with the `<partial-order>`, `<lattice>`, or `<ring>` it sits inside.

### `<group-action>` record

```scheme
(define-record-type <group-action>
  (%make-group-action group set-element? act)
  group-action?
  (group        group-action-group)
  (set-element? group-action-set-element?)
  (act          group-action-apply))
```

---

## Algorithms

### `orbit` — BFS-primary with iterate-all fallback (per Q4)

Prefer BFS from `(group-generators G)` symmetrized under inversion; fall back to iterate-all over `(group-elements G)` when generators are absent.

```scheme
(define (orbit action x)
  (let* ((G    (group-action-group action))
         (act  (group-action-apply action))
         (gens (group-generators G))
         (elts (group-elements G))
         (seen (make-hashtable equal? equal-hash))
         (out  '()))
    (cond
      ;; Primary: BFS from x using (gens ∪ {inverse(g) : g ∈ gens})
      (gens
       (let* ((inv   (group-inverse-fn G))
              (gens* (symmetrize-generators gens inv (setoid-equiv? (group-setoid G)))))
         (hashtable-set! seen x #t)
         (set! out (list x))
         (let bfs ((frontier (list x)))
           (cond
             ((null? frontier) (reverse out))
             (else
              (let ((y (car frontier)))
                (let next ((gs gens*) (frontier+ (cdr frontier)))
                  (cond
                    ((null? gs) (bfs frontier+))
                    (else
                     (let ((z (act (car gs) y)))
                       (cond
                         ((hashtable-ref seen z #f)
                          (next (cdr gs) frontier+))
                         (else
                          (hashtable-set! seen z #t)
                          (set! out (cons z out))
                          (next (cdr gs) (cons z frontier+))))))))))))))
      ;; Fallback: iterate all group elements
      (elts
       (for-each
        (lambda (g)
          (let ((y (act g x)))
            (unless (hashtable-ref seen y #f)
              (hashtable-set! seen y #t)
              (set! out (cons y out)))))
        elts)
       (reverse out))
      ;; Error: neither enumerable nor finitely generated
      (else
       (error "orbit: group has neither generators nor element enumeration"
              'group G)))))

;; Internal: return (gens ∪ {inverse(g) : g ∈ gens}) deduplicated by eq?
(define (symmetrize-generators gens inverse eq?)
  (let ((seen (make-hashtable equal? equal-hash))
        (acc  '()))
    (for-each
      (lambda (g)
        (unless (hashtable-ref seen g #f)
          (hashtable-set! seen g #t)
          (set! acc (cons g acc)))
        (let ((g^-1 (inverse g)))
          (unless (hashtable-ref seen g^-1 #f)
            (hashtable-set! seen g^-1 #t)
            (set! acc (cons g^-1 acc)))))
      gens)
    (reverse acc)))
```

**Termination:** BFS terminates iff `orbit(x)` is finite. For infinite groups (Z) acting on infinite sets with infinite orbits (Z acting on Z by shift), BFS runs forever. v1 does not impose a maximum-orbit-size safety cap; document that callers are responsible for knowing their orbit is finite. A future `orbit/bounded` variant with a cap is a reasonable v2 addition if a consumer surfaces.

### `stabilizer` — all g with g·x = x

```scheme
(define (stabilizer action x)
  (let ((G       (group-action-group action))
        (act     (group-action-apply action))
        (set=?   equal?))
    (filter (lambda (g) (set=? (act g x) x))
            (group-elements G))))
```

### `fixed-points` — for g, all x ∈ X with g·x = x

```scheme
(define (fixed-points action g X-elements)
  (let ((act   (group-action-apply action))
        (set=? equal?))
    (filter (lambda (x) (set=? (act g x) x)) X-elements)))
```

### `orbit-representative` — min element under user-supplied `<?` with enumeration-order tie-breaker

```scheme
(define (orbit-representative action x less?)
  (let ((o (orbit action x)))
    (if (null? o)
        (error "orbit-representative: empty orbit (impossible)")
        (fold (lambda (y best) (if (less? y best) y best))
              (car o) (cdr o)))))
```

Tie-breaker semantics (per Q6 resolution): when `less?(y, best)` and `less?(best, y)` are both `#f` on `equal?`-distinct elements, `best` is kept — i.e., the earlier-enumerated element wins. Under Q4's hybrid strategy, enumeration order = BFS discovery order when `(group-generators G)` is present; = `group-elements` iteration order when the fallback fires. Deterministic within a given `<group>` instance and `orbit` invocation; implementation-dependent across versions and across groups constructed with different generator sets. Callers requiring cross-implementation stability must pass a strictly total `<?`.

The surveyed convention (Simulink, SageMath index-stringification) supports this choice: when a caller-supplied ordering has equivalence classes, fall back to a stable system detail (creation order / enumeration order) rather than failing. Document in the docstring; do not expose the tie-breaker as a user-configurable parameter in v1.

### `burnside-count` — |X/G| = (1/|G|) Σ |X^g|

```scheme
(define (burnside-count action X-elements)
  (let* ((G  (group-action-group action))
         (|G| (group-order G)))
    (unless (finite-group? G)
      (error (string-append
               "burnside-count: group is not finite (no elements enumeration). "
               "If the group is finitely generated and you believe it is finite, "
               "use (enumerate-finite-group G) to promote it first.")
             'group G))
    (let* ((sum (fold (lambda (g acc)
                        (+ acc (length (fixed-points action g X-elements))))
                      0
                      (group-elements G)))
           (q (quotient sum |G|)))
      (unless (= (* q |G|) sum)
        (error "burnside-count: sum not divisible by |G| — group action is malformed"
               'sum sum '|G| |G|))
      q)))
```

### `enumerate-finite-group` — promote tier-2 to tier-1 via BFS closure

Per Q5 resolution. Promotes a `<group>` carrying `generators` (only) to a new `<group>` carrying both `generators` and `elements` + `order`. BFS closure from the identity using `(gens ∪ {inverse(g) : g ∈ gens})`. Loops forever if the group is actually infinite; optional `max-size` cap raises.

```scheme
(define (enumerate-finite-group G . opts)
  (cond
    ((finite-group? G) G)                   ; already enumerated; return as-is
    ((not (finitely-generated-group? G))
     (error "enumerate-finite-group: group has no generators; cannot enumerate"
            'group G))
    (else
     (let* ((max-size (assv-ref opts 'max-size))     ;; #f = no cap
            (gens*    (symmetrize-generators (group-generators G)
                                             (group-inverse-fn G)
                                             (setoid-equiv? (group-setoid G))))
            (op  (group-op-fn G))
            (id       (group-identity G))
            (seen     (make-hashtable equal? equal-hash))
            (count    1))
       (hashtable-set! seen id #t)
       (let bfs ((frontier (list id)))
         (cond
           ((null? frontier)
            (let ((elts (hashtable-keys seen)))
              (make-group op id (group-inverse-fn G)
                          (cons 'element? (group-element? G))
                          (cons 'setoid (group-setoid G))
                          (cons 'order (length elts))
                          (cons 'elements elts)
                          (cons 'generators (group-generators G)))))
           (else
            (let ((g (car frontier)))
              (let inner ((gs gens*) (front+ (cdr frontier)))
                (cond
                  ((null? gs) (bfs front+))
                  (else
                   (let ((h (op g (car gs))))
                     (cond
                       ((hashtable-ref seen h #f) (inner (cdr gs) front+))
                       (else
                        (hashtable-set! seen h #t)
                        (set! count (+ count 1))
                        (when (and max-size (> count max-size))
                          (error "enumerate-finite-group: exceeded max-size; group may be infinite"
                                 'max-size max-size '|elements-so-far| count))
                        (inner (cdr gs) (cons h front+)))))))))))))))))
```

Note: `symmetrize-generators` is the same internal helper used by `orbit` (see above); both share it.

### `subgroup-generated` — BFS closure

```scheme
(define (subgroup-generated G generators)
  (let ((op      (group-op-fn G))
        (inverse (group-inverse-fn G))
        (seen    (make-hashtable (setoid-equiv? (group-setoid G)))))
    (hashtable-set! seen (group-identity G) #t)
    (let loop ((frontier (cons (group-identity G) generators)))
      (if (null? frontier)
          (let ((elts (hashtable-keys seen)))
            (make-group op (group-identity G) inverse
                        (cons 'element?   (group-element? G))
                        (cons 'setoid     (group-setoid G))
                        (cons 'order      (length elts))
                        (cons 'elements   elts)
                        (cons 'generators generators)))
          (let ((g (car frontier)))
            (let more ((gs generators) (new '()))
              (if (null? gs)
                  (loop (append (cdr frontier) new))
                  (let ((gh (op g (car gs))))
                    (if (hashtable-ref seen gh #f)
                        (more (cdr gs) new)
                        (begin (hashtable-set! seen gh #t)
                               (more (cdr gs) (cons gh new))))))))))))
```

Output subgroup carries the caller-supplied generators, making it well-suited for BFS-based orbit computation downstream. Note the subgroup is still finite (elements computed eagerly); infinite subgroups (e.g., `subgroup-generated Z '(3)` = 3Z) are v2 — would need BFS termination strategy (size cap or bounded-traversal variant).

### Preset: `cyclic-group n` → Z_n

```scheme
(define (cyclic-group n)
  (unless (and (integer? n) (positive? n))
    (error "cyclic-group: n must be a positive integer"))
  (make-group
    (lambda (a b) (modulo (+ a b) n))                   ; op (positional)
    0                                                   ; identity (positional)
    (lambda (k) (modulo (- n k) n))                     ; inverse (positional)
    (cons 'element? (lambda (k) (and (integer? k) (<= 0 k) (< k n))))
    (cons 'setoid numeric-setoid)
    (cons 'order n)
    (cons 'elements (iota n))
    (cons 'generators '(1))))
```

### Preset: `symmetric-group n` → S_n

Permutations as vectors of length n; identity is `#(0 1 ... n-1)`; composition is `(p∘q)[i] = p[q[i]]`. Standard generating set: transposition `(0 1)` and n-cycle `(0 1 2 ... n-1)`. Enumeration uses lexicographic generation via an internal `all-permutations` helper.

```scheme
(define (symmetric-group n)
  (let* ((id      (list->vector (iota n)))
         (trans01 (let ((v (list->vector (iota n))))
                    (vector-set! v 0 1) (vector-set! v 1 0) v))
         (n-cycle (list->vector
                    (append (cdr (iota n)) (list 0))))   ; #(1 2 ... n-1 0)
         (all     (all-permutations n))
         (valid?  (lambda (v) (and (vector? v) (= (vector-length v) n)
                                    (permutation-vector? v)))))
    (make-group
      vector-permutation-op                             ; op (positional)
      id                                                ; identity (positional)
      vector-permutation-inverse                        ; inverse (positional)
      (cons 'element? valid?)
      (cons 'setoid default-setoid)                     ; vectors compare with equal?
      (cons 'order (factorial n))
      (cons 'elements all)
      (cons 'generators
            (cond
              ((<= n 1) '())
              ((= n 2)  (list trans01))
              (else     (list trans01 n-cycle)))))))
```

`all-permutations`, `vector-permutation-inverse`, `vector-permutation-op`, `factorial`, and `permutation-vector?` are internal helpers specified with full code in Phase 2 Task 2.3.

---

## Test plan

Target: `test/wile/algebra-group-test.scm`. chibi-test style. Estimated ~35 tests across 8 suites.

1. **Group construction & accessors** (~5 tests) — `make-group`, predicates, identity/inverse/op roundtrips on Z_3.
2. **Finite-group enumeration** (~3 tests) — `group-elements` on cyclic and symmetric, `finite-group?` predicate.
3. **Subgroup closure** (~4 tests) — `subgroup-generated` on Z_6 from {2}, verify order = 3; S_3 from transposition (1 2) + 3-cycle (1 2 3), verify order = 6.
4. **Group action construction** (~3 tests) — `make-group-action` + `group-action-apply` roundtrip on the trivial, permutation, and regular actions.
5. **Orbit, stabilizer, fixed-points** (~6 tests) — orbit-stabilizer theorem `|orbit|·|stab| = |G|`, on S_3 acting on {1,2,3}; fixed-points of rotation in Z_4 on 2-colorings of a 4-cycle.
6. **orbit-representative** (~4 tests) — S_2 on ordered pairs returns the sorted pair; S_3 on 3-tuples returns the lexicographically smallest permutation.
7. **Burnside's lemma** (~5 tests) — **necklace counting**: 2-colorings of 4-bead cycle under Z_4 rotations = 6; S_3 acting on pairs = 2 orbits; Burnside malformation detector raises on a non-action.
8. **Product group/action** (~3 tests) — `(product-group Z_2 Z_3)` has order 6; product action orbit structure.
9. **Preset actions** (~4 tests) — conjugation on S_3 (3 conjugacy classes: identity, transpositions, 3-cycles); regular action is transitive.

---

## Commit strategy (phased)

Each phase ends with `make lint && make covercheck` clean and all tests passing.

| Phase | Scope | Lib LOC | Test LOC | Commit message template |
|-------|-------|---------|----------|-------------------------|
| **1** | Library skeleton; `<group>` record; make-group / accessors | ~40 | ~20 | `feat(algebra/group): add <group> record and constructors` |
| **2** | Preset groups: trivial, cyclic, vector-permutation helpers, symmetric, product + `cartesian-product` + `inject-at-index` internals | ~140 | ~90 | 5 commits, one per preset/helper group (see Phase 2 tasks) |
| **3** | `finite-group?`, `group-elements`; `subgroup-generated` BFS; `enumerate-finite-group` helper with `max-size` cap (Q5); `symmetrize-generators` internal helper | ~70 | ~55 | `feat(algebra/group): add finite-group enumeration, subgroup closure, and enumerate-finite-group helper` |
| **4** | `<group-action>` record; `group-action-apply`; trivial-action | ~30 | ~20 | `feat(algebra/group): add <group-action> record and trivial-action` |
| **5** | Orbit (BFS-primary), stabilizer, fixed-points; Z-on-Z/12Z test | ~60 | ~40 | `feat(algebra/group): add orbit, stabilizer, fixed-points` |
| **6** | `orbit-representative`; `burnside-count` with divisibility check | ~40 | ~35 | `feat(algebra/group): add orbit-representative and Burnside count` |
| **7** | Preset actions: permutation, regular, conjugation, product-action | ~60 | ~30 | `feat(algebra/group): add preset actions (permutation, regular, conjugation, product)` |
| **8** | Umbrella re-export; docstrings; TODO.md update; plans/CLAUDE.md move | ~15 | 0 | 3 commits (re-export, docs, closeout) |

**Total:** ~455 lib LOC + ~290 test LOC = ~745 LOC, ~45 tests.

Target total: ~375 LOC lib + ~215 LOC test = ~590 LOC, ~35 tests.

---

## Task breakdown (TDD)

### Phase 1 — Library skeleton and `<group>` record

#### Task 1.1: Create library file with failing smoke test

**Files:**
- Create: `test/wile/algebra-group-test.scm`
- Create: `stdlib/lib/wile/algebra/group.sld`
- Create: `stdlib/lib/wile/algebra/group.scm`

- [ ] **Step 1: Write failing test**

```scheme
;; test/wile/algebra-group-test.scm
(import (scheme base) (chibi test) (wile algebra group))

(test-begin "algebra-group")

(test-group "construction — extended <group> with optional metadata"
  (let ((Z3 (make-group
              (lambda (a b) (modulo (+ a b) 3))    ; op
              0                                    ; identity
              (lambda (k) (modulo (- 3 k) 3))      ; inverse
              '(element? . integer?)
              `(setoid . ,numeric-setoid)
              '(order . 3)
              '(elements . (0 1 2))
              '(generators . (1)))))
    (test-assert "group?" (group? Z3))
    (test "identity is 0" 0 (group-identity Z3))
    (test "op 1+2 = 0 (mod 3)" 0 (group-op Z3 1 2))
    (test "inverse 1 = 2 (mod 3)" 2 (group-inverse Z3 1))
    (test-assert "element? recognizes 0" ((group-element? Z3) 0))
    (test-assert "group-equal? via setoid" (group-equal? Z3 0 0))
    (test "order" 3 (group-order Z3))
    (test-assert "finite-group?" (finite-group? Z3))))

;; Regression check — shipped 3-arg make-group still works (ring.scm:44 path)
(test-group "backward compatibility — 3-arg make-group"
  (let ((Z (make-group + 0 -)))
    (test-assert "group?" (group? Z))
    (test "identity" 0 (group-identity Z))
    (test "op" 5 (group-op Z 2 3))
    (test "inverse" -3 (group-inverse Z 3))
    (test-assert "no elements by default" (not (group-elements Z)))
    (test-assert "no generators by default" (not (group-generators Z)))
    (test-assert "default setoid present" (group-setoid Z))))

(test-end)
```

- [ ] **Step 2: Run test; expect failure** (new accessors `group-element?`, `group-setoid`, `group-order`, etc. not yet defined)

```bash
./dist/darwin/arm64/wile --run test/wile/algebra-group-test.scm
```
Expected: error on the first new accessor name (e.g., `group-element?: unbound identifier`).

- [ ] **Step 3: Extend library — modify existing `group.sld` and `group.scm` in place.**

`stdlib/lib/wile/algebra/group.sld` — add import and extend export list:

```scheme
(define-library (wile algebra group)
  (description "Groups: abstract algebraic structure, actions, orbits, Burnside counting.")
  (export make-group group?
          group-op group-identity group-inverse
          group->monoid
          validate-group with-group
          ;; NEW in §5.4 — extended introspection
          group-element? group-setoid group-equal?
          group-order group-elements group-generators
          finite-group? finitely-generated-group?
          subgroup-generated subgroup? enumerate-finite-group
          ;; NEW in §5.4 — actions
          make-group-action group-action? group-action-group
          group-action-apply group-action-set-element?
          orbit orbit-representative stabilizer fixed-points
          burnside-count
          ;; NEW in §5.4 — presets
          trivial-group cyclic-group symmetric-group product-group
          trivial-action permutation-action regular-action
          conjugation-action product-action)
  (import (scheme base)
          (wile algebra monoid)
          (wile algebra setoid))
  (include "group.scm"))
```

`stdlib/lib/wile/algebra/group.scm` — extend the existing `<group>` record definition. Replace the current lines 6–15:

```scheme
;; BEFORE (shipped):
;;   (define-record-type <group>
;;     (make-group* op-fn identity inverse-fn)
;;     group?
;;     (op-fn      group-op-fn)
;;     (identity   group-identity)
;;     (inverse-fn group-inverse-fn))
;;
;;   (define (make-group op identity inverse)
;;     (make-group* op identity inverse))

;; AFTER (extended):
(define-record-type <group>
  (%make-group op-fn identity inverse-fn
               element? setoid order elements generators)
  group?
  (op-fn       group-op-fn)
  (identity    group-identity)
  (inverse-fn  group-inverse-fn)
  (element?    group-element?)
  (setoid      group-setoid)
  (order       group-order)
  (elements    group-elements)
  (generators  group-generators))

(define (make-group op identity inverse . opts)
  "Construct a group from binary operation OP, IDENTITY, and INVERSE function.
Optional trailing alist entries: (element? . P), (setoid . S), (order . N),
(elements . LIST), (generators . LIST). Absent options default to #f except
SETOID which defaults to DEFAULT-SETOID (wraps R7RS equal?)."
  (%make-group op identity inverse
               (assv-or opts 'element?   #f)
               (assv-or opts 'setoid     default-setoid)
               (assv-or opts 'order      #f)
               (assv-or opts 'elements   #f)
               (assv-or opts 'generators #f)))

(define (assv-or opts key fallback)
  (let ((p (assv key opts)))
    (if p (cdr p) fallback)))

(define (group-equal? G a b)
  ((setoid-equiv? (group-setoid G)) a b))

(define (finite-group? G)
  (and (group-order G) (group-elements G) #t))

(define (finitely-generated-group? G)
  (and (group-generators G) #t))
```

Existing `group-op`, `group-inverse`, `group->monoid`, `validate-group`, `with-group` remain untouched.

- [ ] **Step 4: Run test; expect pass**

```bash
./dist/darwin/arm64/wile --run test/wile/algebra-group-test.scm
```
Expected: PASS — both the new extended-`<group>` test group and the backward-compatibility test group.

- [ ] **Step 5: Commit**

```bash
git add stdlib/lib/wile/algebra/group.sld \
        stdlib/lib/wile/algebra/group.scm \
        test/wile/algebra-group-test.scm
git commit -m "feat(algebra/group): extend <group> record with optional metadata fields"
```

### Phase 2 — Preset groups and internal helpers

Preset groups move earlier than in the original draft so subsequent phases' tests can reference them without forward references. All presets depend only on `<group>` / `make-group` from Phase 1. Vector-permutation helpers ship alongside their consumer (`symmetric-group`); product-group helpers ship with `product-group`.

#### Task 2.1: `trivial-group`

**Files:** `stdlib/lib/wile/algebra/group.scm`, `test/wile/algebra-group-test.scm`

- [ ] **Step 1: Write failing test**

```scheme
(test-group "trivial-group"
  (let ((T (trivial-group)))
    (test "order" 1 (group-order T))
    (test-assert "is finite" (finite-group? T))
    (test "identity combines with itself"
          (group-identity T)
          (group-op T (group-identity T) (group-identity T)))
    (test "generators = '()" '() (group-generators T))))
```

- [ ] **Step 2: Run; expect failure** (`trivial-group` undefined)

- [ ] **Step 3: Implement**

Add `trivial-group` to `group.sld` exports. In `group.scm`:

```scheme
(define the-trivial-group
  (make-group
    (lambda (a b) 'e)              ; op (positional)
    'e                              ; identity (positional)
    (lambda (g) 'e)                 ; inverse (positional)
    (cons 'element? (lambda (x) (eq? x 'e)))
    (cons 'setoid eqv-setoid)       ; single element 'e — eqv? is sufficient
    (cons 'order 1)
    (cons 'elements '(e))
    (cons 'generators '())))

(define (trivial-group) the-trivial-group)
```

- [ ] **Step 4: Run; expect pass**

- [ ] **Step 5: Commit**

```bash
git commit -m "feat(algebra/group): add trivial-group preset"
```

#### Task 2.2: `cyclic-group`

**Files:** `stdlib/lib/wile/algebra/group.scm`, `test/wile/algebra-group-test.scm`

- [ ] **Step 1: Write failing test**

```scheme
(test-group "cyclic-group"
  (let ((Z5 (cyclic-group 5)))
    (test "order" 5 (group-order Z5))
    (test "2 + 4 = 1 (mod 5)" 1 (group-op Z5 2 4))
    (test "inverse 2" 3 (group-inverse Z5 2))
    (test "generators = (1)" '(1) (group-generators Z5))
    (test-assert "finitely-generated?" (finitely-generated-group? Z5))
    (test-assert "is finite" (finite-group? Z5))
    (test "elements = (0 1 2 3 4)" '(0 1 2 3 4) (group-elements Z5))))

(test-group "cyclic-group/validation"
  (test-error (cyclic-group 0))
  (test-error (cyclic-group -1))
  (test-error (cyclic-group 'not-an-integer)))
```

- [ ] **Step 2: Run; expect failure** (`cyclic-group` undefined)

- [ ] **Step 3: Implement**

Add `cyclic-group` to `group.sld` exports. In `group.scm`:

```scheme
(define (cyclic-group n)
  (unless (and (integer? n) (positive? n))
    (error "cyclic-group: n must be a positive integer" n))
  (make-group
    (lambda (a b) (modulo (+ a b) n))                   ; op (positional)
    0                                                   ; identity (positional)
    (lambda (k) (modulo (- n k) n))                     ; inverse (positional)
    (cons 'element? (lambda (k) (and (integer? k) (<= 0 k) (< k n))))
    (cons 'setoid numeric-setoid)
    (cons 'order n)
    (cons 'elements (iota n))
    (cons 'generators '(1))))
```

- [ ] **Step 4: Run; expect pass**

- [ ] **Step 5: Commit**

```bash
git commit -m "feat(algebra/group): add cyclic-group preset"
```

#### Task 2.3: Vector permutation helpers — `factorial`, `permutation-vector?`, `vector-permutation-op`, `vector-permutation-inverse`, `all-permutations`

**Files:** `stdlib/lib/wile/algebra/group.scm`, `test/wile/algebra-group-test.scm`

Internal helpers for the symmetric-group preset. Not exported. Per Q3 resolution: permutations represented as vectors of length n where `perm[i]` = image of i.

- [ ] **Step 1: Write failing tests**

```scheme
(test-group "factorial"
  (test "0! = 1" 1 (factorial 0))
  (test "4! = 24" 24 (factorial 4))
  (test "6! = 720" 720 (factorial 6)))

(test-group "permutation-vector?"
  (test-assert "#(0 1 2) is valid" (permutation-vector? #(0 1 2)))
  (test-assert "#(2 0 1) is valid" (permutation-vector? #(2 0 1)))
  (test-assert "#(0 0 1) is invalid (repeat)" (not (permutation-vector? #(0 0 1))))
  (test-assert "#(0 1 3) is invalid (out of range)" (not (permutation-vector? #(0 1 3))))
  (test-assert "'(0 1 2) is invalid (not vector)" (not (permutation-vector? '(0 1 2)))))

(test-group "vector-permutation-op"
  ;; p = #(1 0 2), q = #(0 2 1): (p∘q)[i] = p[q[i]]
  ;; q[0]=0, q[1]=2, q[2]=1
  ;; p[q[0]] = p[0] = 1
  ;; p[q[1]] = p[2] = 2
  ;; p[q[2]] = p[1] = 0
  (test "op"
        #(1 2 0)
        (vector-permutation-op #(1 0 2) #(0 2 1))))

(test-group "vector-permutation-inverse"
  ;; p = #(1 2 0): p maps 0→1, 1→2, 2→0
  ;; inverse maps 1→0, 2→1, 0→2, i.e. 0→2, 1→0, 2→1
  (test "inverse #(1 2 0)" #(2 0 1) (vector-permutation-inverse #(1 2 0)))
  (test-assert "p · p^-1 = identity"
               (let ((p #(2 0 1)))
                 (equal? (list->vector (iota 3))
                         (vector-permutation-op p (vector-permutation-inverse p))))))

(test-group "all-permutations"
  (test "0! permutations of empty" '(#()) (all-permutations 0))
  (test "1! = 1 permutation of 1 element" '(#(0)) (all-permutations 1))
  (test "2! = 2 permutations of 2 elements"
        '(#(0 1) #(1 0))
        (all-permutations 2))
  (test "3! = 6 permutations of 3 elements"
        6
        (length (all-permutations 3)))
  (test-assert "all 3-permutations are valid"
               (every permutation-vector? (all-permutations 3))))
```

- [ ] **Step 2: Run; expect failures** (helpers undefined)

- [ ] **Step 3: Implement**

In `group.scm` (not in .sld exports — internal only):

```scheme
(define (factorial n)
  (if (<= n 1) 1 (* n (factorial (- n 1)))))

(define (permutation-vector? v)
  (and (vector? v)
       (let* ((n    (vector-length v))
              (seen (make-vector n #f)))
         (let check ((i 0))
           (cond
             ((= i n) #t)
             (else
              (let ((x (vector-ref v i)))
                (cond
                  ((not (integer? x)) #f)
                  ((or (< x 0) (>= x n)) #f)
                  ((vector-ref seen x) #f)
                  (else
                   (vector-set! seen x #t)
                   (check (+ i 1)))))))))))

(define (vector-permutation-op p q)
  ;; (p∘q)[i] = p[q[i]]
  (let* ((n (vector-length p))
         (r (make-vector n)))
    (let loop ((i 0))
      (cond
        ((= i n) r)
        (else
         (vector-set! r i (vector-ref p (vector-ref q i)))
         (loop (+ i 1)))))))

(define (vector-permutation-inverse p)
  ;; r[p[i]] = i
  (let* ((n (vector-length p))
         (r (make-vector n)))
    (let loop ((i 0))
      (cond
        ((= i n) r)
        (else
         (vector-set! r (vector-ref p i) i)
         (loop (+ i 1)))))))

(define (all-permutations n)
  ;; Lexicographic enumeration of all n! permutations as vectors.
  (cond
    ((= n 0) '(#()))
    (else
     (let permute ((perms (list (list->vector (iota n)))))
       ;; Return immediately — perms already contains all via build-up below.
       ;; Alternative: Heap's algorithm. Use a simpler recursive permute:
       (letrec ((permute-from
                 (lambda (prefix remaining)
                   (cond
                     ((null? remaining)
                      (list (list->vector (reverse prefix))))
                     (else
                      (apply append
                             (map (lambda (x)
                                    (permute-from
                                      (cons x prefix)
                                      (remove (lambda (y) (= x y)) remaining)))
                                  remaining)))))))
         (permute-from '() (iota n)))))))
```

Note: `remove` is from `(srfi 1)`; ensure the `.sld` imports `(srfi 1)`.

- [ ] **Step 4: Run; expect pass**

- [ ] **Step 5: Commit**

```bash
git commit -m "feat(algebra/group): add internal vector-permutation helpers"
```

#### Task 2.4: `symmetric-group`

**Files:** `stdlib/lib/wile/algebra/group.scm`, `test/wile/algebra-group-test.scm`

- [ ] **Step 1: Write failing tests**

```scheme
(test-group "symmetric-group"
  (let ((S3 (symmetric-group 3)))
    (test "|S_3| = 6" 6 (group-order S3))
    (test-assert "identity is #(0 1 2)"
                 (equal? #(0 1 2) (group-identity S3)))
    (test-assert "op is non-abelian on S_3"
                 (not (equal? (group-op S3 #(1 0 2) #(0 2 1))
                              (group-op S3 #(0 2 1) #(1 0 2)))))
    (test-assert "elements are all valid permutations"
                 (every permutation-vector? (group-elements S3)))
    (test "|S_3| elements distinct"
          6 (length (delete-duplicates (group-elements S3) equal?)))))

(test-group "symmetric-group/small"
  (test "|S_1| = 1" 1 (group-order (symmetric-group 1)))
  (test "|S_2| = 2" 2 (group-order (symmetric-group 2)))
  (test "generators of S_1 are empty" '() (group-generators (symmetric-group 1)))
  (test "generators of S_2 have one element (swap)"
        1 (length (group-generators (symmetric-group 2)))))
```

- [ ] **Step 2: Run; expect failure**

- [ ] **Step 3: Implement**

Add `symmetric-group` to `group.sld` exports. In `group.scm`:

```scheme
(define (symmetric-group n)
  (unless (and (integer? n) (>= n 0))
    (error "symmetric-group: n must be a non-negative integer" n))
  (let* ((id      (list->vector (iota n)))
         (trans01 (cond
                    ((< n 2) #f)
                    (else (let ((v (list->vector (iota n))))
                            (vector-set! v 0 1)
                            (vector-set! v 1 0)
                            v))))
         (n-cycle (cond
                    ((< n 2) #f)
                    (else (list->vector (append (cdr (iota n)) (list 0))))))
         (all     (all-permutations n))
         (valid?  (lambda (v) (and (vector? v) (= (vector-length v) n)
                                    (permutation-vector? v))))
         (gens    (cond
                    ((<= n 1) '())
                    ((= n 2)  (list trans01))
                    (else     (list trans01 n-cycle)))))
    (make-group
      vector-permutation-op                             ; op (positional)
      id                                                ; identity (positional)
      vector-permutation-inverse                        ; inverse (positional)
      (cons 'element? valid?)
      (cons 'setoid default-setoid)                     ; vector equal? is structural
      (cons 'order (factorial n))
      (cons 'elements all)
      (cons 'generators gens))))
```

- [ ] **Step 4: Run; expect pass**

- [ ] **Step 5: Commit**

```bash
git commit -m "feat(algebra/group): add symmetric-group preset"
```

#### Task 2.5: `product-group` + `cartesian-product` and `inject-at-index` helpers

**Files:** `stdlib/lib/wile/algebra/group.scm`, `test/wile/algebra-group-test.scm`

Per Q8 resolution: variadic API; elements are proper Scheme lists of length n. Internal helpers `cartesian-product` (builds element enumeration) and `inject-at-index` (builds generator tuples). Neither helper is exported.

- [ ] **Step 1: Write failing tests**

```scheme
(test-group "cartesian-product"
  (test "empty → singleton of empty" '(()) (cartesian-product '()))
  (test "single list" '((1) (2) (3)) (cartesian-product '((1 2 3))))
  (test "two lists, all pairs"
        '((a 1) (a 2) (b 1) (b 2))
        (cartesian-product '((a b) (1 2)))))

(test-group "inject-at-index"
  (test "inject at 0 in (0 0 0)" '(9 0 0) (inject-at-index 9 0 '(0 0 0)))
  (test "inject at 1 in (x y z)" '(x 9 z) (inject-at-index 9 1 '(x y z)))
  (test "inject at 2 in (a b c)" '(a b 9) (inject-at-index 9 2 '(a b c))))

(test-group "product-group"
  (let ((Z2xZ3 (product-group (cyclic-group 2) (cyclic-group 3))))
    (test "order = |Z_2|·|Z_3| = 6" 6 (group-order Z2xZ3))
    (test-assert "identity is (0 0)"
                 (equal? '(0 0) (group-identity Z2xZ3)))
    (test "op (1 2) · (0 1) = (1 0)"
          '(1 0)
          (group-op Z2xZ3 '(1 2) '(0 1)))
    (test-assert "elements are proper lists of length 2"
                 (every (lambda (e) (and (list? e) (= (length e) 2)))
                        (group-elements Z2xZ3)))))

(test-group "product-group/edge-cases"
  (test-eq "n=0 gives trivial-group" (trivial-group) (product-group))
  (let ((Z3 (cyclic-group 3)))
    (test-eq "n=1 returns argument unchanged" Z3 (product-group Z3)))
  (let ((triple (product-group (cyclic-group 2)
                               (cyclic-group 3)
                               (cyclic-group 5))))
    (test "n=3 order = 30" 30 (group-order triple))
    (test-assert "n=3 identity is (0 0 0)"
                 (equal? '(0 0 0) (group-identity triple)))
    (test "n=3 generators span all three components"
          3 (length (group-generators triple)))))
```

- [ ] **Step 2: Run; expect failure**

- [ ] **Step 3: Implement**

Add `product-group` to `group.sld` exports. Helpers stay internal.

```scheme
;; Internal: return list of length n with g at index i, identity[j] elsewhere
(define (inject-at-index g i identities)
  (let loop ((j 0) (ids identities) (acc '()))
    (cond
      ((null? ids) (reverse acc))
      ((= j i)    (loop (+ j 1) (cdr ids) (cons g acc)))
      (else       (loop (+ j 1) (cdr ids) (cons (car ids) acc))))))

;; Internal: cartesian product of a list of lists → list of proper lists
(define (cartesian-product lists)
  (cond
    ((null? lists) '(()))
    (else
     (let ((tails (cartesian-product (cdr lists))))
       (apply append
              (map (lambda (x) (map (lambda (t) (cons x t)) tails))
                   (car lists)))))))

(define (product-group . groups)
  (cond
    ((null? groups) (trivial-group))
    ((null? (cdr groups)) (car groups))
    (else
     (let* ((n          (length groups))
            (identities (map group-identity groups))
            (element?   (lambda (elt)
                          (and (list? elt)
                               (= (length elt) n)
                               (every (lambda (G e) ((group-element? G) e))
                                      groups elt))))
            (inverse     (lambda (elt)
                          (map (lambda (G e) ((group-inverse-fn G) e))
                               groups elt)))
            (op    (lambda (a b)
                          (map (lambda (G e1 e2) ((group-op-fn G) e1 e2))
                               groups a b)))
            (orders     (map group-order groups))
            (all-elts   (map group-elements groups))
            (all-gens   (map group-generators groups))
            (order      (and (every (lambda (o) o) orders) (apply * orders)))
            (elements   (and (every (lambda (e) e) all-elts)
                             (cartesian-product all-elts)))
            (generators (and (every (lambda (g) g) all-gens)
                             (apply append
                                    (map (lambda (i gens-i)
                                           (map (lambda (g)
                                                  (inject-at-index g i identities))
                                                gens-i))
                                         (iota n)
                                         all-gens)))))
       (make-group op                               ; op (positional)
                   identities                        ; identity is the n-list of component identities
                   inverse                           ; inverse (positional)
                   (cons 'element? element?)
                   (cons 'setoid default-setoid)     ; list equal? is structural
                   (cons 'order order)
                   (cons 'elements elements)
                   (cons 'generators generators))))))
```

- [ ] **Step 4: Run; expect pass**

- [ ] **Step 5: Commit**

```bash
git commit -m "feat(algebra/group): add product-group preset (variadic, flat-list elements) with internal helpers"
```

---

### Phase 3 — Finite groups and subgroups

#### Task 3.1: `finite-group?` + `group-elements` tests

- [ ] **Step 1: Write failing tests**

```scheme
(test-group "finite-group"
  (let ((Z3 (make-group
              (lambda (a b) (modulo (+ a b) 3))            ; op
              0                                             ; identity
              (lambda (k) (modulo (- 3 k) 3))               ; inverse
              (cons 'element? (lambda (k) (and (integer? k) (<= 0 k) (< k 3))))
              (cons 'setoid numeric-setoid)
              '(order . 3)
              '(elements . (0 1 2)))))
    (test-assert "finite-group?" (finite-group? Z3))
    (test "group-order" 3 (group-order Z3))
    (test "group-elements" '(0 1 2) (group-elements Z3))))
```

- [ ] **Step 2: Run; expect pass** (already implemented in Phase 1 — consolidation)

#### Task 3.2: `subgroup-generated` via BFS closure

- [ ] **Step 1: Write failing tests**

```scheme
(test-group "subgroup-generated"
  (let ((Z6 (make-group
              (lambda (a b) (modulo (+ a b) 6))            ; op
              0                                             ; identity
              (lambda (k) (modulo (- 6 k) 6))               ; inverse
              (cons 'element? (lambda (k) (and (integer? k) (<= 0 k) (< k 6))))
              (cons 'setoid numeric-setoid)
              '(order . 6)
              '(elements . (0 1 2 3 4 5)))))
    (let ((H (subgroup-generated Z6 '(2))))
      (test "⟨2⟩ in Z_6 has order 3" 3 (group-order H))
      (test-assert "⟨2⟩ contains 0" (memv 0 (group-elements H)))
      (test-assert "⟨2⟩ contains 2" (memv 2 (group-elements H)))
      (test-assert "⟨2⟩ contains 4" (memv 4 (group-elements H))))))
```

- [ ] **Step 2: Run; expect failure** (`subgroup-generated` undefined)

- [ ] **Step 3: Implement**

Add `subgroup-generated subgroup?` to `group.sld` exports. Add to `group.scm`:

```scheme
(define (subgroup-generated G generators)
  (let ((op      (group-op-fn G))
        (inverse (group-inverse-fn G))
        (seen    (make-hashtable (setoid-equiv? (group-setoid G)))))
    (hashtable-set! seen (group-identity G) #t)
    (for-each (lambda (g) (hashtable-set! seen g #t)) generators)
    (let loop ((frontier (cons (group-identity G) generators)))
      (cond
        ((null? frontier)
         (let ((elts (hashtable-keys seen)))
           (make-group op (group-identity G) inverse
                       (cons 'element?   (group-element? G))
                       (cons 'setoid     (group-setoid G))
                       (cons 'order      (length elts))
                       (cons 'elements   elts)
                       (cons 'generators generators))))
        (else
         (let more ((gs generators) (frontier (cdr frontier)))
           (if (null? gs)
               (loop frontier)
               (let ((gh (op (car frontier) (car gs))))
                 (cond
                   ((hashtable-ref seen gh #f)
                    (more (cdr gs) frontier))
                   (else
                    (hashtable-set! seen gh #t)
                    (more (cdr gs) (cons gh frontier))))))))))))

(define (subgroup? H G)
  (and (finite-group? H) (finite-group? G)
       (every (group-element? G) (group-elements H))
       (let ((c-H (group-op-fn H))
             (c-G (group-op-fn G))
             (eq  (setoid-equiv? (group-setoid G))))
         (every (lambda (a)
                  (every (lambda (b) (eq (c-H a b) (c-G a b)))
                         (group-elements H)))
                (group-elements H)))))
```

- [ ] **Step 4: Run; expect pass**

- [ ] **Step 5: Commit**

```bash
git commit -m "feat(algebra/group): add finite-group enumeration and subgroup closure"
```

#### Task 3.3: `enumerate-finite-group` — promote tier-2 to tier-1

Per Q5 resolution. Promotes a `<group>` carrying `generators` only to a new `<group>` carrying `elements` via BFS closure. Shares the `symmetrize-generators` helper with `orbit` (Phase 5).

**Files:** `stdlib/lib/wile/algebra/group.scm`, `test/wile/algebra-group-test.scm`

- [ ] **Step 1: Write failing tests**

```scheme
(test-group "enumerate-finite-group"
  ;; Construct Z_6 via generators only (no elements)
  (let ((Z6-gens (make-group
                   (lambda (a b) (modulo (+ a b) 6))            ; op
                   0                                             ; identity
                   (lambda (k) (modulo (- 6 k) 6))               ; inverse
                   (cons 'element? (lambda (k) (and (integer? k) (<= 0 k) (< k 6))))
                   (cons 'setoid numeric-setoid)
                   '(generators . (1)))))
    (test-assert "not finite-group? before promotion"
                 (not (finite-group? Z6-gens)))
    (test-assert "is finitely-generated-group?"
                 (finitely-generated-group? Z6-gens))
    (let ((Z6 (enumerate-finite-group Z6-gens)))
      (test-assert "finite-group? after promotion" (finite-group? Z6))
      (test "order = 6" 6 (group-order Z6))
      (test-assert "elements contain 0..5"
                   (every (lambda (k) (memv k (group-elements Z6))) (iota 6))))))

(test-group "enumerate-finite-group/already-enumerated"
  ;; Idempotent: passing an already-enumerated group returns it unchanged
  (let* ((Z5 (cyclic-group 5))
         (Z5* (enumerate-finite-group Z5)))
    (test-eq "idempotent on finite group" Z5 Z5*)))

(test-group "enumerate-finite-group/max-size-cap"
  ;; Safety cap raises when exceeded
  (let ((Z100-gens (make-group
                     (lambda (a b) (modulo (+ a b) 100))          ; op
                     0                                             ; identity
                     (lambda (k) (modulo (- 100 k) 100))           ; inverse
                     (cons 'element? (lambda (k) (and (integer? k) (<= 0 k) (< k 100))))
                     (cons 'setoid numeric-setoid)
                     '(generators . (1)))))
    (test-error (enumerate-finite-group Z100-gens '(max-size . 10)))))

(test-group "enumerate-finite-group/no-generators"
  ;; Error when group has neither elements nor generators
  (let ((R (make-group + 0 -                                     ; real-line under addition
                       (cons 'element? real?)
                       (cons 'setoid numeric-setoid))))          ; no elements, no generators
    (test-error (enumerate-finite-group R))))
```

- [ ] **Step 2: Run; expect failure** (helper undefined)

- [ ] **Step 3: Implement**

Add `enumerate-finite-group` to `group.sld` exports.

Add the implementation from the Algorithms section above. Commit `symmetrize-generators` as an internal helper alongside `enumerate-finite-group` in this task; `orbit` in Phase 5 reuses it without duplication.

```scheme
;; Internal: return (gens ∪ {inverse(g) : g ∈ gens}) deduplicated under the
;; caller's EQ? (typically (setoid-equiv? (group-setoid G))).
;; Used by enumerate-finite-group (Phase 3 Task 3.3) and orbit (Phase 5 Task 5.1).
(define (symmetrize-generators gens inverse eq?)
  (let ((seen (make-hashtable eq?))
        (acc  '()))
    (for-each
      (lambda (g)
        (unless (hashtable-ref seen g #f)
          (hashtable-set! seen g #t)
          (set! acc (cons g acc)))
        (let ((g^-1 (inverse g)))
          (unless (hashtable-ref seen g^-1 #f)
            (hashtable-set! seen g^-1 #t)
            (set! acc (cons g^-1 acc)))))
      gens)
    (reverse acc)))
```

- [ ] **Step 4: Run; expect pass**

- [ ] **Step 5: Commit**

```bash
git commit -m "feat(algebra/group): add enumerate-finite-group helper with max-size cap"
```

### Phase 4 — `<group-action>` record

#### Task 4.1: Record type and trivial action

- [ ] **Step 1: Write failing tests**

```scheme
(test-group "group-action/trivial"
  (let* ((Z3 (cyclic-group 3))   ; from Phase 2 Task 2.2
         (A  (trivial-action Z3 integer?)))
    (test-assert "group-action?" (group-action? A))
    (test "trivial action fixes x" 42 ((group-action-apply A) 1 42))
    (test-eq "group-action-group" Z3 (group-action-group A))))
```

Phase 4 lands `trivial-action`. `cyclic-group` ships in Phase 2 (earlier), so the test can reference it directly — no inline construction needed.

- [ ] **Step 2: Run; expect failure**

- [ ] **Step 3: Implement**

Add to `group.sld` exports: `make-group-action group-action? group-action-group group-action-apply group-action-set-element? trivial-action`.

Add to `group.scm`:

```scheme
(define-record-type <group-action>
  (%make-group-action group set-element? act)
  group-action?
  (group        group-action-group)
  (set-element? group-action-set-element?)
  (act          group-action-apply))

(define (make-group-action G set-element? act)
  (unless (group? G)
    (error "make-group-action: expected <group>" G))
  (%make-group-action G set-element? act))

(define (trivial-action G set-element?)
  (make-group-action G set-element? (lambda (g x) x)))
```

- [ ] **Step 4: Run; expect pass**

- [ ] **Step 5: Commit**

```bash
git commit -m "feat(algebra/group): add <group-action> record and trivial-action"
```

### Phase 5 — Orbit, stabilizer, fixed-points

#### Task 5.1: `orbit` via enumeration

- [ ] **Step 1: Write failing tests**

```scheme
(test-group "orbit"
  ;; S_2 acting on {0, 1} — transitive
  (let* ((S2 (symmetric-group 2))   ; from Phase 2 Task 2.4
         (A  (permutation-action S2 2)))
    (let ((o (orbit A 0)))
      (test "S_2 on {0,1} orbit of 0" 2 (length o))
      (test-assert "contains 0" (member 0 o))
      (test-assert "contains 1" (member 1 o)))))
```

- [ ] **Step 2: Run; expect failure**

- [ ] **Step 3: Implement**

Add `orbit stabilizer fixed-points` to `group.sld` exports.

```scheme
(define (orbit action x)
  (let* ((G   (group-action-group action))
         (act (group-action-apply action))
         (eq  (setoid-equiv? (group-setoid G)))         ; element equality on X defaults to equal?
         (elts (group-elements G))
         (seen (make-hashtable equal? equal-hash))
         (out  '()))
    (unless elts
      (error "orbit: group is not finite"))
    (for-each
      (lambda (g)
        (let ((y (act g x)))
          (unless (hashtable-ref seen y #f)
            (hashtable-set! seen y #t)
            (set! out (cons y out)))))
      elts)
    (reverse out)))

(define (stabilizer action x)
  (let ((G   (group-action-group action))
        (act (group-action-apply action)))
    (filter (lambda (g) (equal? (act g x) x))
            (group-elements G))))

(define (fixed-points action g X-elements)
  (let ((act (group-action-apply action)))
    (filter (lambda (x) (equal? (act g x) x)) X-elements)))
```

- [ ] **Step 4: Run; expect pass**

- [ ] **Step 5: Commit**

```bash
git commit -m "feat(algebra/group): add orbit, stabilizer, fixed-points"
```

#### Task 5.2: Orbit-stabilizer theorem test

- [ ] **Step 1: Write test**

```scheme
(test-group "orbit-stabilizer theorem"
  (let* ((S3 (symmetric-group 3))
         (A  (permutation-action S3 3)))
    ;; For transitive action on {0,1,2}: |orbit(0)| · |stab(0)| = |S_3| = 6
    (let ((o (orbit A 0))
          (s (stabilizer A 0)))
      (test "|orbit(0)| · |stab(0)| = |G|"
            (group-order S3)
            (* (length o) (length s))))))
```

- [ ] **Step 2: Run; expect pass** (no new code needed if Phases 4.1 + 6 are done)

- [ ] **Step 3: Commit**

```bash
git commit -m "test(algebra/group): verify orbit-stabilizer theorem on S_3"
```

#### Task 5.3: Infinite group on finite set — Z on Z/12Z

**Files:** `test/wile/algebra-group-test.scm`

Verifies the hybrid BFS strategy (Q4 resolution): Z is infinite (no `elements`), but its orbit on Z/12Z is finite. Iterate-all would fail; BFS must succeed.

- [ ] **Step 1: Write test**

```scheme
(test-group "orbit/infinite-group-finite-orbit"
  (let* ((Z (make-group + 0 -                                   ; Z under addition
                        (cons 'element? integer?)
                        (cons 'setoid numeric-setoid)
                        '(generators . (1))))
         (Z/12Z? (lambda (x) (and (integer? x) (<= 0 x) (< x 12))))
         (A (make-group-action Z Z/12Z?
                               (lambda (k x) (modulo (+ x k) 12)))))
    (test-assert "Z is not a finite group" (not (finite-group? Z)))
    (test-assert "Z is finitely generated" (finitely-generated-group? Z))
    (let ((o (orbit A 0)))
      (test "orbit size = 12" 12 (length o))
      (test-assert "orbit contains 0..11"
                   (every (lambda (k) (member k o)) (iota 12))))))
```

- [ ] **Step 2: Run; expect pass** (no new code; tests the BFS strategy from Phase 5 Task 5.1)

- [ ] **Step 3: Commit**

```bash
git commit -m "test(algebra/group): verify BFS handles Z on Z/12Z (infinite group, finite orbit)"
```

### Phase 6 — `orbit-representative` and Burnside

#### Task 6.1: `orbit-representative`

- [ ] **Step 1: Write failing tests**

```scheme
(test-group "orbit-representative"
  ;; S_2 acting on 2-tuples — canonical form is the sorted pair
  (let* ((S2 (symmetric-group 2))
         (A  (make-group-action
               S2
               (lambda (p) (and (pair? p) (pair? (cdr p))))
               (lambda (perm pair)
                 ;; perm is vector #(0 1) or #(1 0)
                 (if (= (vector-ref perm 0) 0)
                     pair
                     (cons (cdr pair) (car pair)))))))
    (let ((pair<? (lambda (p q)
                    (or (< (car p) (car q))
                        (and (= (car p) (car q)) (< (cdr p) (cdr q)))))))
      (test "canonical (3 . 1) = (1 . 3)"
            '(1 . 3)
            (orbit-representative A '(3 . 1) pair<?))
      (test "canonical (1 . 3) = (1 . 3)"
            '(1 . 3)
            (orbit-representative A '(1 . 3) pair<?)))))
```

- [ ] **Step 2: Run; expect failure**

- [ ] **Step 3: Implement**

Add `orbit-representative` to `group.sld` exports.

```scheme
;; Returns the minimum element of the orbit of x under `action` using the
;; caller-supplied `less?` procedure. When `less?` is not strictly total on
;; the orbit (e.g., a lattice partial order or projection-based ordering),
;; ties are broken by order of discovery in `(orbit action x)` — a
;; deterministic but implementation-dependent fallback. Callers requiring
;; cross-implementation stability must supply a strictly total `<?`.
(define (orbit-representative action x less?)
  (let ((o (orbit action x)))
    (fold (lambda (y best) (if (less? y best) y best))
          (car o) (cdr o))))
```

- [ ] **Step 4: Add test for tie-breaker determinism**

```scheme
(test-group "orbit-representative/tie-breaker"
  ;; less? compares only by car — ties on pairs with same car are resolved
  ;; by enumeration order. With Z_4 acting cyclically on 4 pairs whose
  ;; cars all equal 0, the orbit-representative must return the first
  ;; element encountered in the orbit (deterministic within invocation).
  (let* ((Z4 (cyclic-group 4))
         (A  (make-group-action
               Z4
               pair?
               (lambda (k p) (cons (car p) (modulo (+ (cdr p) k) 4)))))
         (car<? (lambda (a b) (< (car a) (car b)))))
    ;; All orbit elements have car = 0; less? reports no winner.
    ;; Fold keeps the first-seen; test that successive calls return the same.
    (let ((r1 (orbit-representative A '(0 . 0) car<?))
          (r2 (orbit-representative A '(0 . 0) car<?)))
      (test-assert "tie-breaker deterministic across calls" (equal? r1 r2)))))
```

- [ ] **Step 5: Run; expect pass**

- [ ] **Step 6: Commit**

```bash
git commit -m "feat(algebra/group): add orbit-representative with documented tie-breaker"
```

#### Task 6.2: `burnside-count`

- [ ] **Step 1: Write failing tests (necklace counting)**

```scheme
(test-group "burnside-count"
  ;; Classic Burnside: 2-colorings of a 4-bead cycle under Z_4 rotations
  ;; Expected: 6 distinct necklaces (0000, 0001, 0011, 0101, 0111, 1111)
  (let* ((Z4 (cyclic-group 4))
         (colorings
           (let build ((n 4) (acc '(())))
             (if (= n 0)
                 acc
                 (build (- n 1)
                        (append (map (lambda (c) (cons 0 c)) acc)
                                (map (lambda (c) (cons 1 c)) acc))))))
         (rotate-by
           (lambda (k c)
             (let ((n (length c)))
               (let loop ((i 0) (c c))
                 (if (= i k) c
                     (loop (+ i 1) (append (cdr c) (list (car c)))))))))
         (A (make-group-action Z4 list? rotate-by)))
    (test "|2-colorings of 4-cycle / Z_4|" 6
          (burnside-count A colorings))))
```

- [ ] **Step 2: Run; expect failure**

- [ ] **Step 3: Implement**

Add `burnside-count` to `group.sld` exports.

```scheme
(define (burnside-count action X-elements)
  (let* ((G  (group-action-group action))
         (n  (group-order G)))
    (unless (finite-group? G)
      (error (string-append
               "burnside-count: group is not finite (no elements enumeration). "
               "If the group is finitely generated and you believe it is finite, "
               "use (enumerate-finite-group G) to promote it first.")
             'group G))
    (let ((sum (fold (lambda (g acc)
                       (+ acc (length (fixed-points action g X-elements))))
                     0
                     (group-elements G))))
      (let ((q (quotient sum n)))
        (unless (= (* q n) sum)
          (error "burnside-count: sum not divisible by |G|; group action malformed"
                 'sum sum '|G| n))
        q))))
```

- [ ] **Step 4: Run; expect pass**

- [ ] **Step 5: Add malformation-detection test**

```scheme
(test-group "burnside malformation"
  (let* ((Z2 (cyclic-group 2))
         (A-bad (make-group-action
                  Z2
                  integer?
                  ;; NOT an action: violates g·(g·x) = x
                  (lambda (g x) (if (= g 0) x (+ x 1))))))
    (test-error (burnside-count A-bad '(0 1 2 3)))))
```

- [ ] **Step 6: Commit**

```bash
git commit -m "feat(algebra/group): add Burnside's lemma with malformation check"
```

<!-- Old Phase 6 deleted; content moved to new Phase 2 (preset groups) earlier in the document, with Tier A fixes (full code for cyclic-group, explicit vector-helper task, separate product-helper tasks). -->

### Phase 7 — Preset actions

#### Task 7.1: `permutation-action`

```scheme
(define (permutation-action Sn n)
  (make-group-action
    Sn
    (lambda (x) (and (integer? x) (<= 0 x) (< x n)))
    (lambda (perm x) (vector-ref perm x))))
```

Test: orbit of 0 under S_n is all of {0..n-1} (transitivity). Stabilizer of 0 is S_{n-1}.

#### Task 7.2: `regular-action`

```scheme
(define (regular-action G)
  (make-group-action G (group-element? G) (group-op-fn G)))
```

Test: orbit of any element is all of G (regular action is transitive). Stabilizer of any element is trivial.

#### Task 7.3: `conjugation-action`

```scheme
(define (conjugation-action G)
  (let ((op (group-op-fn G))
        (inverse  (group-inverse-fn G)))
    (make-group-action G (group-element? G)
                       (lambda (g x) (op (op g x) (inverse g))))))
```

Test: on S_3, orbits under conjugation are the conjugacy classes — {identity}, {3 transpositions}, {2 three-cycles}; total 3 classes, verifiable via Burnside.

#### Task 7.4: `product-action`

Flat-list representation per Q8 resolution: action elements are proper lists of length n; component-wise application.

```scheme
(define (product-action . actions)
  (cond
    ((null? actions)
     (trivial-action (trivial-group) (lambda (x) (eq? x 'unit))))
    ((null? (cdr actions)) (car actions))
    (else
     (let* ((G           (apply product-group
                                 (map group-action-group actions)))
            (acts        (map group-action-apply actions))
            (set-elts    (map group-action-set-element? actions))
            (n           (length actions))
            (set-elt?    (lambda (elt)
                           (and (list? elt)
                                (= (length elt) n)
                                (every (lambda (se e) (se e))
                                       set-elts elt))))
            (act         (lambda (g-list elt-list)
                           (map (lambda (a g e) (a g e))
                                acts g-list elt-list))))
       (make-group-action G set-elt? act)))))
```

Test:

```scheme
(test-group "product-action"
  (let* ((Z2 (cyclic-group 2))
         (Z3 (cyclic-group 3))
         (A2 (permutation-action Z2 2))   ; if Z_2 treated as S_2; or use regular-action
         (A3 (permutation-action Z3 3))
         (A  (product-action A2 A3)))
    (test-assert "product-action? is #t" (group-action? A))
    (test "|product group| = 6" 6 (group-order (group-action-group A)))
    (let ((result ((group-action-apply A) '(1 2) '(0 1))))
      (test-assert "result is a proper 2-list"
                   (and (list? result) (= (length result) 2))))))
```

Commit each as a separate step with `feat(algebra/group): add <name>` messages.

### Phase 8 — Integration

#### Task 8.1: Umbrella re-export

- [ ] **Step 1: Modify** `stdlib/lib/wile/algebra.sld`:

Add `(wile algebra group)` to the `import` / `export` block.

- [ ] **Step 2: Test that unqualified import works**

```scheme
(import (wile algebra))
(cyclic-group 3)   ; should resolve
```

- [ ] **Step 3: Commit**

```bash
git commit -m "feat(algebra): re-export group through umbrella library"
```

#### Task 8.2: Documentation + docstrings

- [ ] Add docstrings to every exported primitive in `group.scm` following the `(wile algebra incidence)` style (Parameters / Returns / Category / Example).
- [ ] Add `Category: Algebra — groups` to enable topic browsing via `,topic`.
- [ ] Commit: `docs(algebra/group): add docstrings and topic category`.

#### Task 8.3: TODO.md + plans/CLAUDE.md update

- [ ] Mark `§5.4 Group actions & Burnside` in TODO.md as `[x]`; append PR reference.
- [ ] Add Done entry in TODO.md Features section mirroring the incidence / unification entries.
- [ ] Move this plan file entry in `plans/CLAUDE.md` from the Open (Tier A) table to the Completed (Algebra Libraries) table.
- [ ] Commit: `docs(todo,plans): mark §5.4 group actions shipped`.

---

## Definition of done

- [ ] `make build && ./dist/darwin/arm64/wile --run test/wile/algebra-group-test.scm` passes all ~35 tests.
- [ ] `make lint` clean.
- [ ] `make covercheck` clean.
- [ ] `(import (wile algebra)) ` transitively exposes group + group-action primitives.
- [ ] `,apropos group` in REPL surfaces the new primitives.
- [ ] `,topic` shows `Algebra — groups` category with all new exports.
- [ ] TODO.md §5.4 entry marked `[x]`; Done entry added.
- [ ] `plans/CLAUDE.md` index entry moved to Completed.
- [ ] PR description cross-references consumers from `plans/2026-04-17-algebra-foundations-directions.md` Appendix A (wile-goast `prim_canonicalize.go`, `ssa-rule-commutative`) and flags the follow-up plan for migrating `ssa-rule-commutative` to use `orbit-representative` (separate wile-goast PR).

---

## Post-ship follow-ups (separate plans)

- [ ] **wile-goast `ssa-rule-commutative` migration** — rewrite to use `(wile algebra group)`'s `orbit-representative` under an S_2 binop action. Low-risk mechanical refactor; ~30 LOC; in wile-goast repo, not wile. File as wile-goast plan.
- [ ] **Documentation cross-reference** — add to `docs/reference/algebra.md` (or equivalent) noting that `prim_canonicalize.go` implements the register-renaming action from this library. No code change; just naming what exists.

---

## Future extensions (deferred to v2)

- **`verify-action` — explicit, opt-in action verifier (per Q7 deferral).** Users call it once when they want confidence the action is genuinely a group action. Complementary to `burnside-count`'s cheap divisibility check (which stays in v1). Proposed signature:

  ```scheme
  (verify-action action X-elements [options])
  ;; Options alist:
  ;;   (sample-size . K)  ; default: 100. Random (g, h) pairs to spot-check.
  ;;   (full?       . #t) ; default: #f. Exhaustive check over |G|² · |X| triples.
  ;;   (raise?      . #t) ; default: #t. #f returns pair (#t . '()) on success
  ;;                       ;               or (#f . violation-details) on failure.
  ```

  Three checks performed:
  1. **Identity axiom:** `act(group-identity G, x) = x` for all `x ∈ X-elements`.
  2. **Closure:** `act(g, x) ∈ X-elements` for all `g ∈ group-elements G`, `x ∈ X-elements`.
  3. **Compatibility axiom:** `act(op(g, h), x) = act(g, act(h, x))`. Checked on `K` random triples when `full?` is `#f`; exhaustively when `full?` is `#t`.

  Cost: `O(|X|)` for check 1; `O(|G| · |X|)` for check 2; `O(K · |X|)` or `O(|G|² · |X|)` for check 3. Caller chooses trade-off via options.

  Why opt-in rather than tax every `burnside-count` call: v1 consumers have small `|X|` and use well-tested preset groups (Z_n, S_n); action-axiom bugs would surface via `burnside-count`'s divisibility check without incurring `verify-action`'s cost. When a caller constructs a bespoke action via `make-group-action`, they invoke `verify-action` once during development and remove the call once confident.

  **Reason we did not ship as v1 option:** the divisibility check (Q7-b) covers the generic malformation symptom for free; per-call probabilistic verification (Q7-c) would double or triple `burnside-count`'s cost without meaningful benefit on v1 consumer sizes. Explicit verification is strictly more general and composable.

- **Schreier-Sims machinery** — strong generating sets, log-space coset representatives; unlocks orbit-representative for large S_n (register renaming at realistic scale).
- **Callback-based `orbit-representative`** — for groups where enumeration is infeasible, accept a user-supplied "select representative from input" function; library verifies orbit-invariance on a sampled test battery.
- **Group presentations** — ⟨generators | relations⟩ constructor with Todd-Coxeter coset enumeration for finite presentations.
- **Subgroup lattice, normal subgroups, quotient groups** — full structural poset, Noether isomorphism theorems.
- **Homomorphisms** — `<group-homomorphism>` record; kernel/image computation; `is-isomorphism?` check.
- **Dihedral, alternating groups** — additional presets once a consumer appears.
- **Group representations / character theory** — research-tier.
- **`orbit/bounded`** — orbit enumeration with a max-size safety cap; raises when exceeded rather than looping forever on infinite-orbit cases. Analogous to `enumerate-finite-group`'s optional `max-size` but for orbits.

---

## Risks & cross-cutting concerns

1. **Scope creep into Schreier-Sims.** `orbit-representative` on large groups *will* be requested. Mitigation: explicit v2 deferral in docstring and exports commentary; consumer (wile-goast register renaming) continues using its existing canonical form.

2. **`equal?` vs custom element equality.** The `<group>` record carries `equal?` but orbit/stabilizer/fixed-points use `equal?` on the action's set X, not the group's element equality. If X has non-standard equality (e.g., ssa-terms modulo α-equivalence), consumers must canonicalize before passing to orbit operations. Document this clearly; a `group-action-set-equal?` field could be added later without breaking callers.

3. **`make-hashtable` availability.** Wile's hashtable API — verify via `mcp__wile__doc` that `make-hashtable`, `hashtable-ref`, `hashtable-set!`, `hashtable-keys`, `equal-hash` exist and have the expected signatures. If any are missing, use `(scheme hash-table)` SRFI-69 surface or assoc-list fallback.

4. **Test performance of S_n enumeration.** `symmetric-group 8` has 40320 elements; Burnside tests over S_8 would iterate 40320 group elements × |X| fixed-point checks. Keep Burnside tests to S_3, S_4 (|S_4| = 24). Note in docstring: "practical for n ≤ 6; n ≥ 7 requires Schreier-Sims (v2)."

5. **`product-group` / `product-action` element representation.** Per Q8 resolution, elements are proper Scheme lists of length n. Tests must verify `list?` and `length` rather than `pair?`. Composition and application operate component-wise via `map` over the groups/actions list. The earlier cons-pair nesting approach was rejected (produced improper/dotted lists for n ≥ 3).

---

## Self-review checklist

- [x] Every exported symbol listed in § "Exports" has a Task that implements it.
- [x] Every Task lists exact file paths.
- [x] Every Task has TDD steps (test first, run-fail, implement, run-pass, commit).
- [x] No placeholder "add appropriate X" or "handle edge cases" steps — all step bodies have concrete code.
- [x] Type names consistent across tasks (`<group>`, `<group-action>`; `orbit-representative` not `orbit-rep` or `orbit-canonical`).
- [x] Consumer story (wile-goast `ssa-rule-commutative`) explicit in Post-ship follow-ups.
- [x] Open design questions listed separately from Resolved; Q1, Q2, Q3, Q6 resolved; Q4, Q5, Q7, Q8 remain open for user Q&A.
