# §5.5 Distributive/Modular Lattice Recognition + Birkhoff — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

---

## Inherited design context (must read before Phase 1)

Three prior-work streams constrain this plan before its own Q&A begins. Do not re-open these:

**From §5.4's revision (`plans/2026-04-22-group-actions-burnside-impl.md` Revision Note, D1–D4):**

- **D3 — Setoid-carried equality.** `<lattice>` adds a `setoid` field defaulting to `default-setoid`. Convenience `(lattice-equiv? L a b)` delegates to `(setoid-equiv? (lattice-setoid L))`. Existing `(lattice-equal? L a b)` (antisymmetric-`leq?`-derived) is preserved for backward compatibility; the caller obligation is that `(setoid-equiv? S) a b ⟺ lattice-equal? L a b` on elements of L. Mismatch is a precondition error, not silent divergence.
- **D4 — Birkhoff output type.** `birkhoff-representation` returns a `<locally-finite-poset>` from `(wile algebra incidence)`, not a `<partial-order>`. The reconstructed input-side of `birkhoff-reconstruction` also takes `<locally-finite-poset>`. Roundtrip preserves setoid.
- **§5.4 extend-in-place pattern.** Modify existing `(wile algebra lattice)` in place. Shipped 5-arg `(make-lattice join meet bottom top leq?)` is preserved — new optional fields come through an options alist tail. All existing exports (`lattice-join`, `lattice-meet`, `fixpoint`, `validate-lattice`, `with-lattice`, the four presets) are untouched.
- **§5.4 finiteness tier pattern.** Three tiers: tier-1 (elements enumerated), tier-2 (finitely-presented-via-generators — deferred to v2 for lattices, since lattice presentations are more exotic), tier-3 (opaque, pointwise ops only). `distributive?` and `modular?` **require tier-1** (finite, enumerated); they raise a precondition error otherwise. No BFS-from-generators machinery in v1 for lattices.

**From §5.3's post-ship notes (`plans/2026-04-21-ac-matching-design.md:269–322`):**

- **Public-vs-internal discipline upfront.** §5.3 had to retcon `flatten-ac`'s public exposure because tests needed it. §5.5 decides upfront: `join-irreducibles`, `meet-irreducibles`, `join-irreducible?`, `meet-irreducible?` are **public** (consumers using Birkhoff need them); the internal downset enumerator for `birkhoff-reconstruction` stays **internal** and is tested black-box through `birkhoff-reconstruction` output. No "exposed for testing" hatches.
- **Alist vs hashtable — default alist, document crossover.** Downsets are represented as sorted lists of lattice elements (using the setoid's equivalence for deduplication, implementation's enumeration order for sort). Hashtable-backed representation is deferred; document the crossover as an open question only when a consumer surfaces.
- **Avoid speculative cross-library integration.** §5.3's matrix-library dependency was aesthetically clean but empirically wrong (10.6× regression). §5.5 does **not** pre-wire integration with `(wile algebra fca)` or `(wile algebra incidence)` beyond D4's direct use of `<locally-finite-poset>` as the Birkhoff output type. Do not add FCA concept-lattice distributivity checks, incidence-algebra Möbius computation via Birkhoff, or semiring/ring-parameterization in v1 just because they fit conceptually.
- **Canonical counterexamples as test fixtures, not afterthoughts.** §5.3's test coverage gaps (AC+AC nested; large operand counts) surfaced post-ship. §5.5 ships with **N5**, **M3**, chain, Boolean, and small free-distributive-lattice presets from day one, because these are the mathematically canonical test points.
- **Plan specs as design intent, not implementation truth.** Subagents executing this plan should prefer the stated behavior and invariants over any drift in code sketches. Per §5.3 lesson #5, treat test expectations as authoritative.

**Goal:** Extend `(wile algebra lattice)` with distributivity and modularity recognition, join/meet irreducibles extraction, and Birkhoff's fundamental-theorem-of-finite-distributive-lattices roundtrip. Per master plan `plans/2026-04-17-algebra-foundations-directions.md` §5.5: "**Target:** Extension of `(wile algebra lattice)`" and "**Dependencies:** `lattice.sld`, `order.sld`".

**Priority:** Dataflow-analysis-first. MOP = MFP certification for distributive abstract domains in `domains.scm`/`dataflow.scm` is the primary wile-goast consumer. Free distributive lattice construction via Birkhoff unlocks stable-matching-lattice patterns (Conway 1976) for the §4.6 multi-agent coordination template.

**Architecture:** Mirror §5.4's pattern. The shipped `<lattice>` record at `stdlib/lib/wile/algebra/lattice.scm:9-16` is extended with three optional fields (`setoid`, `cardinality`, `elements`) via an options alist on `make-lattice`. A new public interface (`distributive?`, `modular?`, `join-irreducibles`, `meet-irreducibles`, `birkhoff-representation`, `birkhoff-reconstruction`) ships alongside `validate-X/setoid` variants for sample-based checks and four new presets (chain, Boolean, M3 diamond, N5 pentagon, free-distributive). Pure Scheme, no Go primitives.

**Tech Stack:** R7RS Scheme record types, `(srfi 1)` list ops, `(scheme hash-table)` for downset deduplication. New imports on `lattice.sld`: `(wile algebra setoid)` (for `default-setoid`, `setoid-equiv?`), `(wile algebra incidence)` (for `<locally-finite-poset>`, `finite-set->locally-finite-poset`).

**References:**
- `plans/2026-04-17-algebra-foundations-directions.md` §5.5 — motivation and consumer map
- `plans/2026-04-22-group-actions-burnside-impl.md` — template (§5.4) for extend-in-place, finiteness tier, setoid
- `plans/2026-04-21-ac-matching-design.md:269-322` — §5.3 post-ship lessons
- `plans/2026-04-21-incidence-algebra-impl.md` — `<locally-finite-poset>` reference
- Birkhoff, "Rings of sets," *Duke Math. J.* 3 (1937), 443–454 — fundamental theorem
- Davey & Priestley, *Introduction to Lattices and Order* (2nd ed., 2002) — distributive / modular / Birkhoff
- Grätzer, *Lattice Theory: Foundation* (2011) — modular lattice reference, M3/N5 characterization
- Stanley, *Enumerative Combinatorics, Vol. 1* (2nd ed., 2011), §3.4 — distributive lattices and order ideals

---

## Prior art and design lineage

As with §5.4, this plan aligns with the dominant convention in mature CAS. Divergence from all three of Sage / GAP / OSCAR on a common pattern is a signal to re-examine.

### Systems we are deliberately imitating

| System | What we inherit | Primary citations |
|--------|-----------------|-------------------|
| **SageMath** `sage.combinat.posets.lattices.FiniteLatticePoset` | **Antisymmetric-`leq?`-derived equality** (we preserve existing `lattice-equal?`). **`is_distributive()` / `is_modular()` as O(\|L\|³) axiom checks** over enumerated elements. **Join-irreducibles via Hasse-diagram lower-cover count** (each join-irreducible has exactly one element immediately below it). Sage's `order_ideals_lattice()` is precisely `birkhoff-reconstruction`; `irreducibles()` is precisely `join-irreducibles`. | [Sage Posets docs](https://doc.sagemath.org/html/en/reference/combinat/sage/combinat/posets/lattices.html); Stanley, *Enumerative Combinatorics* §3.4. |
| **GAP** `IsDistributiveLattice`, `IsModularLattice` | Predicate forms returning `#t`/`#f` with no diagnostic channel beyond the return value. **Precondition: finite enumerable lattice**; GAP raises on infinite input rather than falling back to sample-based. We follow this — sample-based checks are the `validate-X/setoid` variants, not the predicates. | GAP Reference Manual Chapter on Lattices. |
| **nLab Birkhoff's theorem article** | Exact statement of the theorem: finite distributive lattices are categorically dual to finite posets via `L ↦ J(L)` and `P ↦ Downsets(P)`. Roundtrip is isomorphism (up to lattice equality / poset equality). | [nLab: Birkhoff's theorem for distributive lattices](https://ncatlab.org/nlab/show/Birkhoff%27s+theorem+for+distributive+lattices). |
| **§5.4's extend-in-place** | Record-extension, setoid-carried equality, validate-X/setoid convention, opaque record with composition/decomposition operations. Direct methodology import. | `plans/2026-04-22-group-actions-burnside-impl.md` post-revision. |

### Systems we deliberately do *not* imitate

| System / pattern | Why we diverge |
|------------------|----------------|
| **Sage's lattice-via-Hasse-diagram-only constructor** | Sage's `LatticePoset` takes a Hasse diagram and derives `join`/`meet` from it by computing joins/meets from the cover relation. We preserve the shipped `<lattice>` shape which takes `join`/`meet` as first-class operations. Rationale: many wile consumers (dataflow analysis) have natural join/meet operations that do **not** arise from an explicit cover relation (widening lattices, powerset lattices over large universes). Shipping Hasse-diagram-first would require every consumer to materialize the cover relation. Hasse-diagram support is a v2 addition if a consumer surfaces. |
| **Structural sublattice check for `distributive?`** | Birkhoff's theorem says: a lattice is distributive iff it contains neither M3 nor N5 as a sublattice. This gives an O(\|L\|⁵) algorithm (check every 5-element subset). We use O(\|L\|³) axiom check directly — same correctness, ~\|L\|² faster, no subset enumeration. Structural check is a *characterization* worth documenting in the `distributive?` docstring but not an *implementation strategy*. |
| **Birkhoff-via-roundtrip `distributive?` test** | "L is distributive iff `birkhoff-reconstruction(birkhoff-representation(L)) ≅ L`" is correct but requires computing the whole Birkhoff representation to answer a yes/no question. We do the axiom check directly and reserve Birkhoff for users who want the representation. |
| **Lattice presentations by generators and relations** | OSCAR's `FPLattice`-style presentation system. Deferred to v2 for the same reason §5.4 deferred `fp-group`: requires confluent rewriting machinery (Knuth-Bendix) and no v1 consumer needs it. |

### Convergence check

If a future refactor or extension would:

- Replace axiom-check `distributive?` with forbidden-sublattice structural check as the primary implementation,
- Silently accept non-finite lattices and return a probabilistic answer,
- Return `<partial-order>` instead of `<locally-finite-poset>` from `birkhoff-representation`,
- Swap the shipped `<lattice>`'s `join`/`meet`-first constructor for a Hasse-diagram-first one,

then it is diverging from the direction validated by Sage / GAP / OSCAR / nLab. Stop and verify motivation rather than proceed.

---

## Resolved design decisions

Captured from user Q&A; this plan is ready to execute after these resolutions.

### Q1: Library location — **extend `(wile algebra lattice)` in place**

Per master plan and §5.4's D1 precedent. All new machinery ships from the existing library. Existing exports preserved verbatim. Umbrella `algebra.sld` block at `;; Lattices` (lines 14–20) grows with new names; no new `(import ...)` line needed.

### Q2: Element representation — **opaque `<lattice>` record, same pattern as §5.4**

Consumers don't inspect `<lattice>` fields directly. **Decomposition** operations (produce sub-types): `lattice-bottom`, `lattice-top`, `lattice-elements`, `lattice-cardinality`, `lattice-setoid`, `lattice->partial-order`. **Composition** operations (produce new opaque forms): `make-lattice`, `powerset-lattice`, `product-lattice`, `map-lattice`, `chain-lattice`, `boolean-lattice`, `birkhoff-reconstruction`. **Element-valued** operations (take/return elements, not the lattice): `lattice-join`, `lattice-meet`, `lattice-leq?`, `lattice-equal?`, `lattice-equiv?`.

### Q3: Finiteness tier — **tier-1 only for v1**

Three-tier pattern from §5.4:

- **Tier-1** (elements enumerated): `lattice-elements` populated, `lattice-cardinality` known. Required by `distributive?`, `modular?`, `join-irreducibles`, `meet-irreducibles`, `birkhoff-representation`.
- **Tier-2** (finitely generated via join-irreducibles, elements computable via BFS-closure through `∨` and `∧`): **deferred to v2**. Unlike groups, lattice BFS-closure from generators terminates only if the lattice is finite; the termination predicate is not available without computing the closure. Users with a finitely-generated tier-2 lattice must enumerate explicitly in v1.
- **Tier-3** (opaque, pointwise ops only): existing lattices constructed with just `make-lattice join meet bottom top leq?` stay tier-3 unless caller supplies `(elements . L)` and `(cardinality . N)` in the options alist. They continue to work with `fixpoint`, `lattice-join`, `with-lattice`, `validate-lattice` — no regression. They **cannot** be passed to `distributive?` / `modular?` / `birkhoff-representation` — attempting to raises a precondition error whose message cites `(cons 'elements L)` as the fix.

### Q4: `distributive?` computation strategy — **exhaustive axiom check on enumerated triples**

For `L` with `(lattice-elements L) = es`, check

    ∀ a, b, c ∈ es: a ∧ (b ∨ c) = (a ∧ b) ∨ (a ∧ c)

using `(setoid-equiv? (lattice-setoid L))` as the equality. Cost: O(\|L\|³) triples plus O(1) join/meet cost per triple. Fails loudly on first violating triple with a diagnostic `'(not-distributive a b c lhs rhs)` so callers can reproduce the counterexample. Return `#t` if all triples pass.

**Why not sample-based by default:**

- Sample-based `distributive?` would be O(\|samples\|³) with weaker guarantee ("no counterexample found in samples"). Non-distributive lattices can pass large samples if the witnessing triple is outside the sample set.
- Sample-based *is* useful as a spot-check for large-but-enumerated lattices or as a regression guard for lattices whose construction we trust. That's `validate-distributive-lattice L samples` — a separate export following `validate-group`'s precedent.
- **GAP / Sage convention convergence:** both use exhaustive axiom check for the predicate form. Matches.

**Why not forbidden-sublattice (M3/N5) structural check:**

- O(\|L\|⁵) vs O(\|L\|³) — asymptotically slower.
- Requires 5-element subset enumeration infrastructure.
- No correctness advantage: Birkhoff says the two characterizations are equivalent.

Users who want the sublattice-witness rather than the axiom-witness can call a separate `distributive-witness` helper (v2 addition if requested).

### Q5: `modular?` — **exhaustive axiom check; independent of `distributive?`**

Check the modular law: `∀ a, b, c ∈ es with a ≤ c: a ∨ (b ∧ c) = (a ∨ b) ∧ c`. Cost: same order as `distributive?`, with the `a ≤ c` filter reducing the constant factor. Same early-exit-on-violation discipline.

**Why not compute `modular?` from `distributive?`:**

- Distributive ⟹ modular, but not vice versa (M3 is modular, not distributive).
- Composing `(or (distributive? L) (modular-axiom-check L))` is equivalent but obscures the diagnostic (caller can't tell whether we short-circuited).
- Independent axiom checks keep the diagnostic honest: `(distributive? L)` returning `'(not-distributive ...)` does not imply anything about modularity; the caller can call `modular?` next to get the orthogonal answer.

Document the implication in each docstring: "`distributive?` implies `modular?`; the converse is false (consider M3)."

### Q6: `join-irreducibles` / `meet-irreducibles` — **public list return, Hasse-cover-count algorithm**

**Element `j ∈ L` is join-irreducible iff `j ≠ ⊥` and `j = a ∨ b ⟹ j = a ∨ j = b`.** Equivalently: `j` has exactly one element immediately below it in the Hasse diagram (one "lower cover").

Algorithm (O(\|L\|²)):

    for each j in elements(L):
      if j = bottom(L): skip  ;; ⊥ is never join-irreducible by convention
      lower-covers := { x ∈ L : x < j and no y ∈ L with x < y < j }
      if |lower-covers| = 1: j is join-irreducible

Return value: list of elements, in `lattice-elements L` order (stable).

Dual for `meet-irreducibles`: `m ≠ ⊤` and exactly one upper cover.

Convenience predicates `(join-irreducible? L x)` and `(meet-irreducible? L x)` return booleans for individual elements; O(\|L\|) cost since they compute the lower/upper cover set for one element.

**Why these are public (not just internal to Birkhoff):**

- Consumers of `birkhoff-representation` often want the irreducibles list separately (e.g., to decompose elements as joins of irreducibles for concept analysis).
- The dual pair (`join-` + `meet-`) is mathematically canonical — offering only one direction forks the API asymmetrically.
- Decision made upfront (per §5.3 lesson #3): no retcon of public exposure.

### Q7: Birkhoff output type — **`<locally-finite-poset>`** (per D4)

`birkhoff-representation L` returns a `<locally-finite-poset>` whose elements are `(join-irreducibles L)` and whose `leq?` is `(lattice-leq? L)` restricted to that set. Interval procedure derived via `finite-set->locally-finite-poset`. This makes the result directly usable by `(wile algebra incidence)` for Möbius computation — the Birkhoff-representation's incidence-algebra Möbius function is the classical Möbius function on the poset of join-irreducibles (Rota 1964, §3).

**Why `<locally-finite-poset>` and not `<partial-order>`:**

- `<partial-order>` from `order.sld` has only `leq?`; no element enumeration or interval structure. Consumers doing anything structural with the result (counting chains, computing Möbius, enumerating antichains) would need to reconstruct enumeration themselves.
- `<locally-finite-poset>` carries `(leq?, interval)` pair and accepts `finite-set->locally-finite-poset` for construction from element lists. Direct fit.
- All finite distributive lattices produce finite posets of join-irreducibles; "locally finite" is trivially satisfied.

### Q8: Birkhoff reconstruction input — **`<locally-finite-poset>`; setoid inherited from poset's equality**

`birkhoff-reconstruction P` takes a `<locally-finite-poset>` `P` and returns a `<lattice>` `L` whose elements are the downsets of `P` (`D ⊆ P : x ∈ D ∧ y ≤ x ⟹ y ∈ D`), ordered by inclusion. Bottom is `∅`, top is `P` (full element set). Join is union, meet is intersection, `leq?` is subset.

**How downsets are represented:** sorted lists of poset elements. Sort order is first-seen order from the enumeration used during reconstruction (deterministic for a given poset element list). Alist-backed (per §5.3 lesson); hashtable crossover deferred.

**Setoid inheritance:** the lattice's setoid is constructed from the input poset's element equality. For posets built via `finite-set->locally-finite-poset`, this is structural `equal?` by default. Caller can override via `(birkhoff-reconstruction P (cons 'setoid S))` if they need a different element equality on downsets. Roundtrip invariant: if `L` was built from a poset with setoid `S`, then `(birkhoff-reconstruction (birkhoff-representation L))` uses a setoid compatible with `S` on the join-irreducible elements.

**How the poset's elements are obtained for enumeration:** v1 requires the input poset to expose elements via either (a) being constructed through `finite-set->locally-finite-poset` (which keeps the element list internally — but the shipped record does *not* currently expose this list) or (b) being supplied through a new optional `poset-elements` field on `<locally-finite-poset>`.

**This is an open issue** — see Q13 below.

### Q9: `validate-distributive-lattice` / `validate-modular-lattice` — **sample-based; `/setoid` variants**

Following `validate-group`/`validate-partial-order/setoid` precedent:

    (validate-distributive-lattice L samples)
    (validate-distributive-lattice/setoid L setoid samples)
    (validate-modular-lattice L samples)
    (validate-modular-lattice/setoid L setoid samples)

Semantics: run the axiom check on triples drawn from `samples`, using either `(lattice-setoid L)` (default) or the explicit `setoid` argument. Return `#t` on success or a list of `(violation-type args...)` entries (same shape as `validate-group`). Useful for:

- Spot-checking infinite/tier-3 lattices where `distributive?` is not applicable.
- Regression-guarding lattice constructions where the full axiom check is expensive.
- Teaching/debugging: pass a small hand-picked `samples` to see axioms verified or violated.

### Q10: Presets — **chain, boolean, M3 (diamond-lattice 3), N5 (pentagon-lattice), free-distributive-lattice small-n**

Ship the five canonical test/teaching fixtures:

| Preset | Size | Distributive | Modular | Purpose |
|--------|------|--------------|---------|---------|
| `(chain-lattice n)` | n | ✓ | ✓ | Canonical distributive; trivially modular |
| `(boolean-lattice n)` | 2ⁿ | ✓ | ✓ | Canonical distributive; same as `(powerset-lattice (iota n))` but with explicit cardinality/elements metadata |
| `(diamond-lattice n)` for n ≥ 3 | n + 2 | ✗ | ✓ | Canonical non-distributive modular lattice. M3 = diamond-lattice 3 |
| `(pentagon-lattice)` | 5 | ✗ | ✗ | Canonical non-modular lattice (N5). Used in Birkhoff theorem statement |
| `(free-distributive-lattice n)` for 0 ≤ n ≤ 5 | Dedekind(n) | ✓ | ✓ | Universal distributive lattice on n generators. D(0)=2, D(1)=3, D(2)=6, D(3)=20, D(4)=168, D(5)=7581. Constructed as Downsets(2^[n]) via `birkhoff-reconstruction`. Precondition-checks n ≤ 5 in v1; raises for n ≥ 6 (D(6) = 7.8M — out of scope for in-process construction). |

**Why these and not others:** M3 and N5 are the forbidden sublattices in Birkhoff's characterization theorem — every lattice-theory student encounters them in the first chapter of Davey & Priestley. Chain and Boolean are the canonical distributive examples. `free-distributive-lattice` is both pedagogically motivated (demonstrates Birkhoff roundtrip concretely) and useful (a precomputed universal distributive lattice for testing consumer code).

### Q11: Connection to FCA — **none in v1**

`(wile algebra fca)` already ships; its concept lattices are often not distributive. Pre-wiring FCA integration (e.g., auto-distributivity-check on every concept lattice) is the speculative cross-library dependency §5.3 warned against. Consumers who want to check FCA-concept-lattice distributivity call `(distributive? (concept-lattice C))` explicitly. Integration helpers deferred to a separate plan.

### Q12: Stable-matching lattice — **v2**

Conway 1976 showed the set of stable matchings on a bipartite preference structure forms a distributive lattice (man-optimal = top, woman-optimal = bottom, join = "man-side union," meet = "man-side intersection"). Rotation-based traversal gives a canonical chain between the two extremes.

This is its own implementation — stable matching itself is non-trivial (Gale-Shapley), and the lattice structure is derived from the matching computation. v2 plan; `stable-matching-lattice` preset will live in a future `(wile algebra matching)` library or an extension of `(wile algebra lattice)`.

---

## Open design questions (user-review requested before Phase 1)

### Q13: How does `birkhoff-reconstruction` get the poset's element list?

The shipped `<locally-finite-poset>` record at `incidence.scm:17-21` has two fields: `leq?` and `interval`. It does **not** expose an element list directly. `birkhoff-reconstruction` needs to enumerate downsets, which requires knowing all poset elements.

Three options:

- **(a) Add an optional `elements` field to `<locally-finite-poset>`.** Record type extension in `(wile algebra incidence)`. Cleanest long-term. Backward-compatible (existing 2-field `make-locally-finite-poset` keeps working with `elements = #f`). Adds `lf-poset-elements` accessor.
- **(b) Require caller to supply the element list as a second argument: `(birkhoff-reconstruction P elements)`.** Keeps `<locally-finite-poset>` untouched but asymmetric with `birkhoff-representation` which takes a single argument.
- **(c) Synthesize the element list from `(interval P bottom top)` if `bottom`, `top` are known.** Requires caller to supply bottom/top — same problem deferred.

**Recommendation: option (a).** Edits one field into an existing record, gives Birkhoff a clean single-argument signature, makes `<locally-finite-poset>` more usable for other consumers (counting chains, antichain enumeration). The change is additive, not a break.

Cross-section: §5.5 touches `(wile algebra incidence)` under option (a), which means this plan also updates `incidence.sld`/`incidence.scm`. Scope expansion is minor (one optional field).

**Needs user decision before Phase 1.**

### Q14: `lattice-cardinality` vs `lattice-size` naming

§5.4's `group-order` meant `|G|`. For lattices, "order" is ambiguous — it can mean the poset ordering OR the cardinality. Standard usage:

- Sage: `L.cardinality()`
- GAP: `Size(L)`
- Grätzer/Davey-Priestley: "size of the lattice"

**Recommendation: `lattice-cardinality`.** Unambiguous, matches Sage, avoids the `partial-order` word collision. Adds ~6 characters over `lattice-size`; acceptable for clarity.

Accept "good-enough-different" from §5.4's `group-order` because the math literatures themselves differ — consistency with lattice-specific literature is more valuable than consistency with the sibling group library.

**Needs user decision before Phase 1.**

### Q15: `free-distributive-lattice` n upper bound

Dedekind numbers grow super-exponentially: `D(5) = 7581`, `D(6) ≈ 7.8M`, `D(7) ≈ 2.4 × 10¹²`. Materializing `free-distributive-lattice 6` as an in-memory list of elements is ~60 MB (7.8M downset-tuples at ~8 bytes each). Practical v1 cap:

- **n ≤ 5** (7581 elements, ~60 KB) — comfortable
- **n = 6** (7.8M elements, ~60 MB) — technically possible but slow; consumer would hit `fixpoint` or `distributive?` O(\|L\|³) costs long before the construction itself
- **n ≥ 7** — infeasible

**Recommendation:** `(free-distributive-lattice n)` raises for `n ≥ 6` in v1. Docstring cites Dedekind-number growth as the reason. If a consumer needs n=6, the escape hatch is explicit construction via `birkhoff-reconstruction` on a user-supplied poset.

**Needs user decision before Phase 1.**

---

## Context

### Consumers (from `plans/2026-04-17-algebra-foundations-directions.md` §5.5 and §4.6)

| Site | Currently | After shipping |
|------|-----------|----------------|
| `wile-goast` `dataflow.scm` `run-analysis` | MOP vs MFP conflation; correctness claims ad-hoc | `distributive?` on the abstract domain gates the claim "MOP = MFP is exact on this domain" |
| `wile-goast` `domains.scm` precision annotations | Comment-level claims about domain precision | Machine-checkable via `distributive?` / `modular?` in a test |
| `(wile algebra fca)` concept lattices | Distributivity unknown | User-driven `(distributive? (concept-lattice C))` answers precisely |
| Future: stable-matching coordination templates (§4.6) | Non-existent | `free-distributive-lattice` and (v2) `stable-matching-lattice` as primitives |

### What this does *not* ship

- **Lattice presentations by generators and relations.** v1 lattices are tier-1 (enumerated) or tier-3 (opaque). Tier-2 generator-based construction is v2.
- **Hasse-diagram-first lattice constructor.** Shipped `<lattice>` takes `join`/`meet`; Hasse-diagram-derived variant is v2.
- **Complete lattice theory.** `fixpoint` handles Kleene iteration on the shipped `<lattice>`; deeper completeness theory (Dedekind-MacNeille completion, free-complete-lattice) is out of scope.
- **Lattice morphisms / homomorphisms.** v2. v1 shipped `lattice->partial-order` forgetful functor only.
- **Stable-matching lattice.** v2, separate plan.
- **Birkhoff-witness decomposition.** `birkhoff-representation` returns the Irr(L) poset; it does not return the explicit isomorphism `L → Downsets(Irr(L))`. Callers who want the isomorphism compose it manually: for `x ∈ L`, its downset under the isomorphism is `{j ∈ Irr(L) : j ≤ x}`.

---

## Scope

### In scope (v1)

- New exports listed in the Exports section below.
- `<lattice>` record extension (three optional fields: `setoid`, `cardinality`, `elements`) with backward-compatible 5-arg `make-lattice`.
- One new accessor on `<locally-finite-poset>` (`lf-poset-elements`) and one new field (`elements`) per Q13 option (a). This is a small additive edit in `(wile algebra incidence)`.
- Five new presets (chain, Boolean, diamond/M3, pentagon/N5, free-distributive).
- Four new `validate-X[/setoid]` variants.
- Umbrella `algebra.sld` re-exports all new names in the `;; Lattices` block.
- Test suite: ~30 test groups covering `distributive?`, `modular?`, join-irreducibles, meet-irreducibles, Birkhoff roundtrip on canonical lattices, presets, and the canonical counterexamples.

### Out of scope (deferred to v2 / never)

- Lattice presentations, Hasse-diagram-first construction, lattice morphisms.
- Stable-matching lattice (Conway 1976).
- Dedekind-MacNeille completion.
- Concept-lattice distributivity auto-checking in `(wile algebra fca)`.
- Hashtable-backed downset representation.
- Probabilistic or structural (M3/N5-sublattice) variants of `distributive?`.

---

## Exports

Total: 12 existing (preserved) + 24 new = 36 exports from `(wile algebra lattice)`.

```scheme
;; (wile algebra lattice) — exports (existing, preserved verbatim)
(make-lattice lattice?
 lattice-join lattice-meet lattice-bottom lattice-top
 lattice-leq? lattice-equal?
 lattice->partial-order
 flat-lattice powerset-lattice product-lattice map-lattice
 fixpoint fixpoint/widen
 validate-lattice with-lattice

 ;; NEW — extended introspection on <lattice>
 lattice-setoid lattice-equiv?
 lattice-cardinality lattice-elements
 finite-lattice?

 ;; NEW — distributivity and modularity
 distributive? modular?
 validate-distributive-lattice validate-distributive-lattice/setoid
 validate-modular-lattice     validate-modular-lattice/setoid

 ;; NEW — irreducibles
 join-irreducibles meet-irreducibles
 join-irreducible? meet-irreducible?

 ;; NEW — Birkhoff roundtrip and lattice-to-poset projection
 lattice->locally-finite-poset
 birkhoff-representation birkhoff-reconstruction

 ;; NEW — presets (canonical distributive / modular / counter-example fixtures)
 chain-lattice boolean-lattice
 diamond-lattice pentagon-lattice
 free-distributive-lattice)
```

**Existing exports preserved (no rename, no signature break):**

| Export | Shipped signature | v1 revision |
|--------|-------------------|-------------|
| `make-lattice` | `(join meet bottom top leq?)` — 5 positional | Extended to `(join meet bottom top leq? . opts)` — options alist appended; 5-arg call unchanged |
| `lattice-join` | `(L a b)` | unchanged |
| `lattice-meet` | `(L a b)` | unchanged |
| `lattice-bottom`, `lattice-top` | `(L)` | unchanged |
| `lattice-leq?`, `lattice-equal?` | `(L a b)` | unchanged |
| `lattice->partial-order` | `(L)` | unchanged |
| `flat-lattice`, `powerset-lattice`, `product-lattice`, `map-lattice` | (each) | unchanged |
| `fixpoint`, `fixpoint/widen` | (each) | unchanged |
| `validate-lattice`, `with-lattice` | (each) | unchanged |

Minor touch: `(wile algebra incidence)` adds one optional field (`elements`) on `<locally-finite-poset>` plus accessor `lf-poset-elements`. Existing 2-arg `(make-locally-finite-poset leq? interval)` preserved; optional trailing alist.

---

## File structure

**Modified files (no new library files):**

| Path | Change |
|------|--------|
| `stdlib/lib/wile/algebra/lattice.sld` | Extend `export` block with new identifiers; add imports `(wile algebra setoid)` and `(wile algebra incidence)` |
| `stdlib/lib/wile/algebra/lattice.scm` | Extend `<lattice>` record with optional fields; add new predicates, Birkhoff pair, validators, presets |
| `stdlib/lib/wile/algebra/incidence.sld` | Add `lf-poset-elements` to exports |
| `stdlib/lib/wile/algebra/incidence.scm` | Extend `<locally-finite-poset>` with optional `elements` field; preserve 2-arg `make-locally-finite-poset` |
| `stdlib/lib/wile/algebra.sld` | Extend existing `;; Lattices` export block (lines 14–20) with new names; `;; Incidence algebra` block gets `lf-poset-elements` |
| `test/wile/algebra-lattice-test.scm` | Add new `test-group` blocks alongside existing; existing tests remain unchanged as regression check |
| `TODO.md` | Mark §5.5 entry as shipped; add Done entry |
| `plans/CLAUDE.md` | Move entry from Open Tier A → Completed |

---

## Representation

### `<lattice>` record — extended in place

The shipped record at `lattice.scm:9-16` has five fields: `join-fn`, `meet-fn`, `bottom`, `top`, `leq-fn`. The revision adds three optional fields (`setoid`, `cardinality`, `elements`), defaulting to `default-setoid`, `#f`, `#f` respectively. The private constructor grows to 8 fields; the public `make-lattice` remains a 5-arg function with options alist tail.

```scheme
(define-record-type <lattice>
  (%make-lattice join-fn meet-fn bottom top leq-fn
                 setoid cardinality elements)
  lattice?
  (join-fn     lattice-join-fn)
  (meet-fn     lattice-meet-fn)
  (bottom      lattice-bottom)
  (top         lattice-top)
  (leq-fn      lattice-leq-fn)
  ;; NEW
  (setoid      lattice-setoid)       ;; <setoid>; defaults to default-setoid
  (cardinality lattice-cardinality)  ;; exact integer or #f
  (elements    lattice-elements))    ;; list or #f

(define (make-lattice join meet bottom top leq? . opts)
  "Construct a lattice from JOIN, MEET, BOTTOM, TOP, and LEQ?.
Optional trailing alist entries:
  (setoid . S)       — <setoid> carrying element equality; defaults to default-setoid
  (cardinality . N)  — exact integer |L| if known; #f otherwise
  (elements . LIST)  — enumeration of L's elements; required for distributive?/modular?
Absent options default to #f except SETOID which defaults to default-setoid."
  (%make-lattice join meet bottom top leq?
                 (assv-or opts 'setoid      default-setoid)
                 (assv-or opts 'cardinality #f)
                 (assv-or opts 'elements    #f)))
```

**Semantic invariant (caller obligation):** `(setoid-equiv? (lattice-setoid L)) a b ⟺ lattice-equal? L a b` on elements a, b of L. Mismatch is undefined behavior; the library does not check it at construction but callers violating it will see `distributive?` and `birkhoff-representation` produce wrong answers (since they use setoid equality internally). `validate-lattice/setoid` (new helper — to be decided in Q-followup) could be added as the spot-check.

**Finiteness predicate:**

```scheme
(define (finite-lattice? L)
  (and (lattice-cardinality L) (lattice-elements L) #t))
```

**Equivalence helper (D3 convenience):**

```scheme
(define (lattice-equiv? L a b)
  "Apply L's setoid equivalence to A and B. Equivalent to
((setoid-equiv? (lattice-setoid L)) a b)."
  ((setoid-equiv? (lattice-setoid L)) a b))
```

Callers that need the raw predicate lift it via `(setoid-equiv? (lattice-setoid L))`.

**Why not replace `lattice-equal?` with `lattice-equiv?`:** `lattice-equal?` (antisymmetric-`leq?`-derived) is shipped and used by `fixpoint`. Replacing its semantics would silently change fixpoint convergence behavior. Additive naming keeps both meanings distinguishable — `lattice-equal?` is "mathematical antisymmetric equality," `lattice-equiv?` is "element-level carrier equality." They agree on well-formed lattices (by the caller obligation above) but the intent is different at the call site.

### `<locally-finite-poset>` record — one optional field added

The shipped record at `incidence.scm:17-21` has two fields: `leq?`, `interval`. Add an optional `elements` field (list or `#f`).

```scheme
(define-record-type <locally-finite-poset>
  (%make-locally-finite-poset leq? interval elements)
  locally-finite-poset?
  (leq?     lf-poset-leq?-fn)
  (interval lf-poset-interval-fn)
  (elements lf-poset-elements))   ;; NEW — list or #f

(define (make-locally-finite-poset leq? interval . opts)
  (%make-locally-finite-poset leq? interval
                              (assv-or opts 'elements #f)))

(define (finite-set->locally-finite-poset leq? elements)
  ;; unchanged behavior, but populates the new elements field:
  (make-locally-finite-poset
    leq?
    (lambda (x y) ...interval procedure as before...)
    (cons 'elements elements)))
```

`lf-poset-elements` is exported. Consumers built through `finite-set->locally-finite-poset` automatically gain the element list; hand-rolled callers who pass a raw interval procedure can opt in with `(cons 'elements ...)`.

---

## Algorithms

### `distributive?` — exhaustive axiom check

```scheme
(define (distributive? L)
  (unless (finite-lattice? L)
    (error "distributive?: requires finite lattice (elements enumerated)"
           'fix "pass (cons 'elements LIST) to make-lattice"))
  (let ((elts  (lattice-elements L))
        (join  (lambda (a b) (lattice-join L a b)))
        (meet  (lambda (a b) (lattice-meet L a b)))
        (eq    (setoid-equiv? (lattice-setoid L))))
    (let outer ((as elts))
      (cond
        ((null? as) #t)
        (else
         (let middle ((bs elts))
           (cond
             ((null? bs) (outer (cdr as)))
             (else
              (let inner ((cs elts))
                (cond
                  ((null? cs) (middle (cdr bs)))
                  (else
                   (let* ((a (car as)) (b (car bs)) (c (car cs))
                          (lhs (meet a (join b c)))
                          (rhs (join (meet a b) (meet a c))))
                     (cond
                       ((eq lhs rhs) (inner (cdr cs)))
                       (else #f))))))))))))))  ;; early exit on first counterexample
```

O(\|L\|³) time, O(1) auxiliary space. Returns `#t` or `#f`. For the witness-carrying variant (if a consumer needs it), v2 would add `distributive-witness` returning the first `(a b c)` triple where the axiom fails.

### `modular?` — filtered axiom check

```scheme
(define (modular? L)
  (unless (finite-lattice? L)
    (error "modular?: requires finite lattice"))
  (let ((elts  (lattice-elements L))
        (join  (lambda (a b) (lattice-join L a b)))
        (meet  (lambda (a b) (lattice-meet L a b)))
        (leq   (lambda (a b) (lattice-leq? L a b)))
        (eq    (setoid-equiv? (lattice-setoid L))))
    (let outer ((as elts))
      (cond
        ((null? as) #t)
        (else
         (let middle ((bs elts))
           (cond
             ((null? bs) (outer (cdr as)))
             (else
              (let inner ((cs elts))
                (cond
                  ((null? cs) (middle (cdr bs)))
                  (else
                   (let ((a (car as)) (b (car bs)) (c (car cs)))
                     (cond
                       ((not (leq a c)) (inner (cdr cs)))  ;; skip; precondition a ≤ c
                       (else
                        (let ((lhs (join a (meet b c)))
                              (rhs (meet (join a b) c)))
                          (cond
                            ((eq lhs rhs) (inner (cdr cs)))
                            (else #f)))))))))))))))))
```

Worst case O(\|L\|³), average closer to O(\|L\|² × \|chain\|) with the `(leq a c)` filter.

### `join-irreducibles` / `meet-irreducibles` — lower/upper cover count

```scheme
(define (lower-covers L x)
  "Elements y with y < x and no z such that y < z < x. Internal helper."
  (let ((elts (lattice-elements L))
        (leq  (lambda (a b) (lattice-leq? L a b)))
        (eq   (setoid-equiv? (lattice-setoid L))))
    (let* ((below (filter (lambda (y) (and (leq y x) (not (eq y x)))) elts))
           (covers
             (filter
               (lambda (y)
                 (not (any (lambda (z)
                             (and (not (eq z y)) (not (eq z x))
                                  (leq y z) (leq z x)))
                           below)))
               below)))
      covers)))

(define (upper-covers L x) ...)  ;; dual

(define (join-irreducible? L x)
  (let ((eq (setoid-equiv? (lattice-setoid L))))
    (and (not (eq x (lattice-bottom L)))
         (= 1 (length (lower-covers L x))))))

(define (meet-irreducible? L x)
  (let ((eq (setoid-equiv? (lattice-setoid L))))
    (and (not (eq x (lattice-top L)))
         (= 1 (length (upper-covers L x))))))

(define (join-irreducibles L)
  (unless (finite-lattice? L)
    (error "join-irreducibles: requires finite lattice"))
  (filter (lambda (x) (join-irreducible? L x)) (lattice-elements L)))

(define (meet-irreducibles L)
  (unless (finite-lattice? L)
    (error "meet-irreducibles: requires finite lattice"))
  (filter (lambda (x) (meet-irreducible? L x)) (lattice-elements L)))
```

`lower-covers` / `upper-covers` are internal (not exported). Consumers use `join-irreducible?` / `meet-irreducible?` / `join-irreducibles` / `meet-irreducibles`.

### `lattice->locally-finite-poset` — forgetful projection

Parallel to the shipped `lattice->partial-order`: forgets the `join`/`meet`/`bottom`/`top` structure and returns the underlying poset as a `<locally-finite-poset>` (with elements populated, since the lattice is finite).

```scheme
(define (lattice->locally-finite-poset L)
  "Project a finite lattice to its underlying <locally-finite-poset>.
Requires finite-lattice?. The result carries the same element list as
L; its leq? is (lattice-leq? L)."
  (unless (finite-lattice? L)
    (error "lattice->locally-finite-poset: requires finite lattice"
           'fix "pass (cons 'elements LIST) to make-lattice"))
  (finite-set->locally-finite-poset
    (lambda (a b) (lattice-leq? L a b))
    (lattice-elements L)))
```

This is independently useful beyond `free-distributive-lattice`: consumers computing Möbius functions on a finite lattice call `(mobius-function (make-incidence-algebra (lattice->locally-finite-poset L)))`.

### `birkhoff-representation` — lattice → poset of join-irreducibles

```scheme
(define (birkhoff-representation L)
  "Return the <locally-finite-poset> of join-irreducibles of finite
lattice L, ordered by the restriction of lattice-leq?. Precondition:
L is a finite distributive lattice; result's behavior on non-
distributive input is not a contract (Birkhoff's theorem assumes
distributivity for the bijection)."
  (unless (finite-lattice? L)
    (error "birkhoff-representation: requires finite lattice"))
  (let ((irr (join-irreducibles L)))
    (make-locally-finite-poset
      (lambda (a b) (lattice-leq? L a b))
      (lambda (x y)
        (if (not (lattice-leq? L x y))
            '()
            (filter (lambda (z) (and (lattice-leq? L x z)
                                     (lattice-leq? L z y)))
                    irr)))
      (cons 'elements irr))))
```

Cost dominated by `join-irreducibles`: O(\|L\|²) for the irreducibles computation, O(\|Irr(L)\|) for the poset construction. Result carries the element list (for direct Birkhoff-reconstruction roundtrip).

### `birkhoff-reconstruction` — poset → lattice of downsets

```scheme
(define (birkhoff-reconstruction P . opts)
  "Return the <lattice> whose elements are the downsets of locally-
finite-poset P, ordered by inclusion. Precondition: P exposes its
element list via (lf-poset-elements P)."
  (let ((elements (lf-poset-elements P)))
    (unless elements
      (error "birkhoff-reconstruction: poset must expose elements"
             'fix "construct P via finite-set->locally-finite-poset or pass (cons 'elements LIST) to make-locally-finite-poset"))
    (let* ((leq      (lf-poset-leq? P))
           (downsets (enumerate-downsets elements leq))
           (bot      '())
           (top      elements)
           (setoid   (assv-or opts 'setoid default-setoid)))
      (make-lattice
        ;; join: union under downset-preserving set-union
        (lambda (a b) (sorted-union a b elements))
        ;; meet: intersection
        (lambda (a b) (sorted-intersection a b elements))
        bot top
        ;; leq?: subset
        (lambda (a b) (subset? a b))
        (cons 'setoid      setoid)
        (cons 'cardinality (length downsets))
        (cons 'elements    downsets)))))

;; Internal — enumerate all downsets of P. Algorithm: for each subset
;; S of ELEMENTS, include S iff downward-closed under LEQ. O(2^|P|)
;; with early-pruning via canonical enumeration order. For |P| small
;; enough for Birkhoff to make sense, this is fine.
(define (enumerate-downsets elements leq) ...)

;; Internal helpers: sorted-union, sorted-intersection, subset? — all
;; operate on sorted lists using ELEMENTS as the canonical order. Set
;; equality is structural list equal? because sort order is deterministic.
(define (sorted-union a b canonical-order) ...)
(define (sorted-intersection a b canonical-order) ...)
(define (subset? a b) ...)
```

Cost: O(2^\|P\|) for downset enumeration (every subset is a candidate), O(\|D\|) per set-op where D is a typical downset. Practical for \|P\| ≤ ~15 (32768 subsets); beyond that the lattice itself may not fit in memory. `free-distributive-lattice n` via `(birkhoff-reconstruction (antichain-poset n))` has \|L\| = Dedekind(n) which grows faster than 2ⁿ; the construction caps at n=5 per Q15.

### Presets

```scheme
(define (chain-lattice n)
  "The n-element chain 0 < 1 < ... < n-1. Distributive, modular."
  (unless (and (integer? n) (positive? n))
    (error "chain-lattice: n must be a positive integer"))
  (let ((elts (iota n)))
    (make-lattice
      max min 0 (- n 1) <=
      (cons 'setoid      numeric-setoid)
      (cons 'cardinality n)
      (cons 'elements    elts))))

(define (boolean-lattice n)
  "The Boolean lattice 2^[n]. Same as powerset-lattice (iota n) but
with explicit cardinality/elements populated for distributivity checks."
  (unless (and (integer? n) (not (negative? n)))
    (error "boolean-lattice: n must be a non-negative integer"))
  (let* ((universe (iota n))
         (ps       (powerset-lattice universe)))
    ;; Rebuild with elements/cardinality populated (powerset-lattice ships without).
    (make-lattice
      (lattice-join-fn ps) (lattice-meet-fn ps)
      (lattice-bottom ps)  (lattice-top ps)
      (lattice-leq-fn ps)
      (cons 'setoid      default-setoid)
      (cons 'cardinality (expt 2 n))
      (cons 'elements    (all-subsets universe)))))

(define (diamond-lattice n)
  "The rank-3 lattice with n atoms: ⊥ < a_1, ..., a_n < ⊤, no
comparabilities among the a_i. Modular (for all n ≥ 3) but not
distributive (for n ≥ 3). M_3 = (diamond-lattice 3); canonical
counterexample for distributivity."
  (unless (and (integer? n) (>= n 3))
    (error "diamond-lattice: n must be at least 3"))
  (let* ((atoms (map (lambda (i) (list 'atom i)) (iota n)))
         (elts  (cons 'bot (cons 'top atoms))))
    ;; join/meet/leq? derived from the explicit ordering
    ...))

(define (pentagon-lattice)
  "N_5: the pentagon lattice. Five elements {⊥, a, b, c, ⊤} with
b < c and a incomparable to both. Not modular, not distributive.
Canonical counterexample for modularity; Birkhoff's forbidden
sublattice for distributivity."
  ...)

(define (free-distributive-lattice n)
  "The free bounded distributive lattice on n generators. Isomorphic
to the lattice of monotone Boolean functions on {0,1}^n, equivalently
to Downsets(B(n)) where B(n) is the Boolean poset 2^[n] (subsets of
n-element set ordered by inclusion). Cardinality = Dedekind(n).
Raises for n >= 6. D(0)=2, D(1)=3, D(2)=6, D(3)=20, D(4)=168, D(5)=7581."
  (unless (and (integer? n) (not (negative? n)))
    (error "free-distributive-lattice: n must be a non-negative integer"))
  (when (>= n 6)
    (error "free-distributive-lattice: n >= 6 infeasible (Dedekind number explodes)"
           'n n '|dedekind(6)| 7828354))
  ;; B(n) viewed as a <locally-finite-poset>, then take its Birkhoff
  ;; reconstruction (= Downsets(B(n)) = FDL(n)).
  ;; CRUCIAL: do NOT take (birkhoff-representation (boolean-lattice n))
  ;; which gives the n-element antichain J(B(n)) — Downsets of that
  ;; would reconstruct B(n) (size 2^n), not FDL(n) (size D(n)).
  ;; We need the WHOLE Boolean poset, not just its join-irreducibles.
  (birkhoff-reconstruction (lattice->locally-finite-poset (boolean-lattice n))))
```

---

## Test plan

Target: `test/wile/algebra-lattice-test.scm`. chibi-test style. Estimated ~30 test groups across 8 suites.

1. **Extended `<lattice>` construction & accessors** (~4 tests) — setoid, cardinality, elements; backward-compat with 5-arg shipped `make-lattice`.
2. **`lattice-equiv?` and setoid semantics** (~3 tests) — default-setoid returns `equal?`; numeric-setoid returns `=`; caller-obligation invariant spot-check.
3. **Presets** (~6 tests) — `chain-lattice n` has cardinality n; `boolean-lattice n` has cardinality 2^n; `diamond-lattice 3` (= M3) has 5 elements; `pentagon-lattice` has 5 elements; `free-distributive-lattice n` for n = 0..5 has Dedekind(n) elements; n=6 raises.
4. **`distributive?`** (~5 tests) — chain: #t; boolean: #t; M3: #f; N5: #f; free-distributive: #t. Witness of non-distributivity reported for M3.
5. **`modular?`** (~4 tests) — chain: #t; boolean: #t; M3: #t; N5: #f (the modularity witness).
6. **Irreducibles** (~5 tests) — `(join-irreducibles (chain-lattice 5))` = `(1 2 3 4)` (4 elements; ⊥ = 0 excluded); `(join-irreducibles (boolean-lattice 3))` has 3 atoms; `(meet-irreducibles (boolean-lattice 3))` has 3 coatoms; `(join-irreducible? (diamond-lattice 3) atom_1)` = #t; `(join-irreducibles (pentagon-lattice))` has 3 elements (a, b, c where c = b ∨ _).
7. **Birkhoff roundtrip** (~4 tests) — for distributive lattice L: `(distributive? (birkhoff-reconstruction (birkhoff-representation L)))` = #t; the reconstructed lattice has same cardinality as L (Birkhoff isomorphism); roundtrip on `chain-lattice 4`, `boolean-lattice 3`, `free-distributive-lattice 2`.
8. **Error handling and preconditions** (~3 tests) — `distributive?` on tier-3 lattice raises with 'fix hint; `birkhoff-representation` on non-finite raises; `birkhoff-reconstruction` on poset without elements raises.
9. **Validators** (~2 tests) — `validate-distributive-lattice (pentagon-lattice) samples` reports `'not-distributive` violations; `validate-modular-lattice/setoid` on M3 with numeric-setoid returns #t.

---

## Commit strategy (phased)

Each phase ends with `make lint && make covercheck` clean and all tests passing.

| Phase | Scope | Lib LOC | Test LOC | Commit message template |
|-------|-------|---------|----------|-------------------------|
| **1** | Extend `<lattice>` record: setoid/cardinality/elements; `lattice-equiv?`; `finite-lattice?` | ~40 | ~20 | `feat(algebra/lattice): extend <lattice> record with optional metadata fields` |
| **2** | Extend `<locally-finite-poset>` with optional `elements`; `lf-poset-elements` accessor (scope: `(wile algebra incidence)`) | ~15 | ~8 | `feat(algebra/incidence): add optional elements field on <locally-finite-poset>` |
| **3** | Presets: `chain-lattice`, `boolean-lattice`, `diamond-lattice`, `pentagon-lattice` | ~70 | ~30 | 4 commits, one per preset |
| **4** | `join-irreducibles` / `meet-irreducibles` / predicates; internal `lower-covers` / `upper-covers` helpers | ~50 | ~30 | `feat(algebra/lattice): add join/meet-irreducibles` |
| **5** | `distributive?` + `modular?` + validator siblings | ~60 | ~25 | `feat(algebra/lattice): add distributive?, modular?, and sample validators` |
| **6** | `lattice->locally-finite-poset` + `birkhoff-representation` + `birkhoff-reconstruction` + internal downset enumerator | ~95 | ~40 | `feat(algebra/lattice): add Birkhoff representation / reconstruction roundtrip` (with preceding projection commit) |
| **7** | `free-distributive-lattice` preset (consumes Birkhoff) | ~20 | ~10 | `feat(algebra/lattice): add free-distributive-lattice preset` |
| **8** | Umbrella re-exports; docstrings; TODO.md; plans/CLAUDE.md | ~15 | 0 | 3 commits (re-export, docs, closeout) |

Target totals: ~350 lib LOC, ~160 test LOC, ~17 commits. Estimated effort slightly above master-plan estimate (~250 LOC) because of the validator siblings and preset set.

---

## Task breakdown (TDD)

Following §5.4's format: each task = write test first, run to failure, implement, run to pass, commit.

### Phase 1 — `<lattice>` record extension

#### Task 1.1: Add optional metadata fields (setoid, cardinality, elements)

**Files:** `stdlib/lib/wile/algebra/lattice.sld`, `stdlib/lib/wile/algebra/lattice.scm`, `test/wile/algebra-lattice-test.scm`

- [ ] **Step 1: Write failing test.** Test both the extended form and the backward-compat 5-arg form:

```scheme
(test-group "extended <lattice> with optional metadata"
  (let ((L (make-lattice
            max min 0 4 <=
            '(cardinality . 5)
            '(elements . (0 1 2 3 4))
            `(setoid . ,numeric-setoid))))
    (test-assert "lattice?"      (lattice? L))
    (test      "cardinality"   5 (lattice-cardinality L))
    (test      "elements"      '(0 1 2 3 4) (lattice-elements L))
    (test-assert "finite-lattice?" (finite-lattice? L))
    (test-assert "equiv via setoid" (lattice-equiv? L 2 2))
    (test-assert "equiv disagrees on distinct" (not (lattice-equiv? L 2 3)))))

(test-group "backward compatibility — 5-arg make-lattice"
  (let ((L (make-lattice max min 0 100 <=)))
    (test-assert "lattice?" (lattice? L))
    (test 50 (lattice-join L 20 50))
    (test-assert "default setoid present" (lattice-setoid L))
    (test-assert "no cardinality by default" (not (lattice-cardinality L)))
    (test-assert "no elements by default" (not (lattice-elements L)))
    (test-assert "finite? #f when unenumerated" (not (finite-lattice? L)))))
```

- [ ] **Step 2:** Run test; expect failure on `lattice-cardinality` (unbound).

- [ ] **Step 3:** Extend record in `lattice.scm`. See Representation section above. Key points:
  - Private `%make-lattice` grows to 8 fields.
  - Public `make-lattice` takes 5 positional + options alist, using the `assv-or` helper.
  - Introduce `assv-or` at file top (copy from `group.scm` if §5.4 lands first, else duplicate).
  - Add `(wile algebra setoid)` to `lattice.sld`'s imports.
  - Export new names.

- [ ] **Step 4:** Run; expect pass.
- [ ] **Step 5:** Commit `feat(algebra/lattice): extend <lattice> record with optional metadata fields`.

### Phase 2 — `<locally-finite-poset>` element field

#### Task 2.1: Add optional `elements` field

**Files:** `stdlib/lib/wile/algebra/incidence.sld`, `stdlib/lib/wile/algebra/incidence.scm`, `test/wile/algebra-incidence-test.scm` (extend existing; don't break).

- [ ] **Step 1: Write failing test.** Verify the accessor on a `finite-set->locally-finite-poset`-constructed poset.

```scheme
(test-group "<locally-finite-poset> exposes elements"
  (let ((P (finite-set->locally-finite-poset
             (lambda (a b) (<= a b))
             '(1 2 3 4))))
    (test '(1 2 3 4) (lf-poset-elements P))))

(test-group "<locally-finite-poset> backward compat — 2-arg make"
  (let ((P (make-locally-finite-poset
             (lambda (a b) (<= a b))
             (lambda (x y) (iota (- y x -1) x)))))
    (test-assert "lf-poset?" (locally-finite-poset? P))
    (test-assert "no elements by default" (not (lf-poset-elements P)))))
```

- [ ] **Step 2:** Run; expect failure.
- [ ] **Step 3:** Extend record; update `make-locally-finite-poset`; make `finite-set->locally-finite-poset` populate the new field via `(cons 'elements elements)`.
- [ ] **Step 4:** Run; expect pass (including existing incidence tests).
- [ ] **Step 5:** Commit.

### Phase 3 — Presets (canonical fixtures)

Ordered so subsequent phases can rely on them without forward references.

#### Task 3.1: `chain-lattice`

**Files:** `lattice.scm`, `test/wile/algebra-lattice-test.scm`

- [ ] **Step 1: Write failing test.**

```scheme
(test-group "chain-lattice"
  (let ((C5 (chain-lattice 5)))
    (test "cardinality" 5 (lattice-cardinality C5))
    (test "elements"    '(0 1 2 3 4) (lattice-elements C5))
    (test "join 2 4"    4 (lattice-join C5 2 4))
    (test "meet 2 4"    2 (lattice-meet C5 2 4))
    (test "bottom"      0 (lattice-bottom C5))
    (test "top"         4 (lattice-top C5))))
```

- [ ] **Step 2:** Run; fail.
- [ ] **Step 3:** Implement as sketched in Algorithms > Presets.
- [ ] **Step 4:** Pass. - [ ] **Step 5:** Commit.

#### Task 3.2: `boolean-lattice`

Include `all-subsets` as internal helper. Test at n=0 (1 element, `()`), n=3 (8 elements).

#### Task 3.3: `diamond-lattice` (with M3 as the n=3 case)

Sketch the element layout carefully: `'bot` / `(atom 0)` ... `(atom n-1)` / `'top`. Write join/meet by explicit case analysis. Verify modular but not distributive as a separate test later (Phase 5).

#### Task 3.4: `pentagon-lattice` (N5)

Element layout: `'bot`, `'a`, `'b`, `'c`, `'top` with ordering `bot < a < top`, `bot < b < c < top`, `a incomparable with b and c`.

### Phase 4 — Join/meet irreducibles

#### Task 4.1: Internal `lower-covers` / `upper-covers` helpers

Not exported. Test indirectly via `join-irreducible?` in Task 4.2.

#### Task 4.2: `join-irreducible?` / `meet-irreducible?` / `join-irreducibles` / `meet-irreducibles`

Test on all four Phase 3 presets. Canonical expected values:

- `(join-irreducibles (chain-lattice 4))` → `(1 2 3)` (three atoms; 0 excluded)
- `(join-irreducibles (boolean-lattice 3))` → three singletons (atoms)
- `(join-irreducibles (diamond-lattice 3))` → the three atoms
- `(join-irreducibles (pentagon-lattice))` → `(a b c)` (all non-bot elements; a has one lower cover (bot), b has one (bot), c has one (b))

### Phase 5 — Distributivity / modularity

#### Task 5.1: `distributive?`

Canonical tests:

```scheme
(test-assert "chain distributive"   (distributive? (chain-lattice 5)))
(test-assert "boolean distributive" (distributive? (boolean-lattice 3)))
(test #f                            (distributive? (diamond-lattice 3)))
(test #f                            (distributive? (pentagon-lattice)))
(test-error                         (distributive? (make-lattice max min 0 1 <=)))  ;; tier-3
```

#### Task 5.2: `modular?`

```scheme
(test-assert "chain modular"    (modular? (chain-lattice 5)))
(test-assert "M3 modular"       (modular? (diamond-lattice 3)))
(test #f                        (modular? (pentagon-lattice)))
```

#### Task 5.3: Validators

`validate-distributive-lattice`, `validate-distributive-lattice/setoid`, `validate-modular-lattice`, `validate-modular-lattice/setoid`. Mirror `validate-partial-order`/`validate-partial-order/setoid` from `order.sld`.

### Phase 6 — Birkhoff roundtrip

#### Task 6.0: `lattice->locally-finite-poset` projection

Small helper, but ordered first because Phase 7 and `birkhoff-reconstruction`'s free-lattice test depend on it.

- [ ] **Step 1:** Write failing test.

```scheme
(test-group "lattice->locally-finite-poset"
  (let* ((L (chain-lattice 4))
         (P (lattice->locally-finite-poset L)))
    (test-assert "is <locally-finite-poset>" (locally-finite-poset? P))
    (test '(0 1 2 3) (lf-poset-elements P))
    (test-assert "leq? preserved" ((lf-poset-leq? P) 1 3))))

(test-group "lattice->locally-finite-poset/preconditions"
  (test-error (lattice->locally-finite-poset (make-lattice max min 0 10 <=))))  ;; tier-3
```

- [ ] **Step 2:** Run; fail.
- [ ] **Step 3:** Implement per Algorithms section.
- [ ] **Step 4:** Pass.
- [ ] **Step 5:** Commit `feat(algebra/lattice): add lattice->locally-finite-poset projection`.

#### Task 6.1: Internal downset enumerator

```scheme
(define (enumerate-downsets elements leq) ...)
```

Per Risks #3: use recursive construction (downsets(P) = downsets(P minus any maximal x) ∪ {D ∪ {x} : D ∈ downsets(P\x) closed under x's predecessors}), not subset-filter, so time is O(|downsets(P)|) not O(2^|P|). Test indirectly via Task 6.2.

#### Task 6.2: `birkhoff-representation`

```scheme
(test-group "birkhoff-representation / boolean-lattice 3"
  (let* ((B3 (boolean-lattice 3))
         (P  (birkhoff-representation B3)))
    (test-assert "lf-poset?" (locally-finite-poset? P))
    (test "join-irreducibles = 3 atoms" 3 (length (lf-poset-elements P)))))
```

#### Task 6.3: `birkhoff-reconstruction`

```scheme
(test-group "birkhoff-reconstruction round-trip"
  ;; Roundtrip on chain-lattice 4 — must reproduce a 4-element chain
  (let* ((L       (chain-lattice 4))
         (P       (birkhoff-representation L))
         (L-back  (birkhoff-reconstruction P)))
    (test 4 (lattice-cardinality L-back))
    (test-assert "roundtrip still distributive" (distributive? L-back))))

(test-group "birkhoff-reconstruction preconditions"
  ;; Poset without elements → raises
  (let ((P (make-locally-finite-poset <= (lambda (x y) '()))))
    (test-error (birkhoff-reconstruction P))))
```

### Phase 7 — `free-distributive-lattice`

#### Task 7.1: `free-distributive-lattice n` for 0 ≤ n ≤ 5

Verify against Dedekind numbers:

```scheme
(test-group "free-distributive-lattice cardinality = Dedekind(n)"
  (test "D(0)" 2    (lattice-cardinality (free-distributive-lattice 0)))
  (test "D(1)" 3    (lattice-cardinality (free-distributive-lattice 1)))
  (test "D(2)" 6    (lattice-cardinality (free-distributive-lattice 2)))
  (test "D(3)" 20   (lattice-cardinality (free-distributive-lattice 3)))
  (test "D(4)" 168  (lattice-cardinality (free-distributive-lattice 4)))
  (test "D(5)" 7581 (lattice-cardinality (free-distributive-lattice 5)))
  (test-error       (free-distributive-lattice 6)))
```

Last test may need `(test-error/time-limit 5)` if the n=5 case is slow; benchmark during development. Document the D(5) construction time if > 1s as a known caveat.

### Phase 8 — Integration

#### Task 8.1: Umbrella re-export

Add to `stdlib/lib/wile/algebra.sld`:

```
;; in the ;; Lattices block:
lattice-setoid lattice-equiv? lattice-cardinality lattice-elements
finite-lattice?
distributive? modular?
validate-distributive-lattice validate-distributive-lattice/setoid
validate-modular-lattice     validate-modular-lattice/setoid
join-irreducibles meet-irreducibles
join-irreducible? meet-irreducible?
lattice->locally-finite-poset
birkhoff-representation birkhoff-reconstruction
chain-lattice boolean-lattice diamond-lattice pentagon-lattice
free-distributive-lattice

;; in the ;; Incidence algebra block:
lf-poset-elements
```

#### Task 8.2: Docstrings

Add docstrings to every new exported primitive following `(wile algebra incidence)` style (Parameters / Returns / Category / Keywords / Examples / See also).

#### Task 8.3: TODO.md + plans/CLAUDE.md

Mark §5.5 shipped in `TODO.md`. Move plan entry in `plans/CLAUDE.md` from Open → Completed.

---

## Definition of done

- [ ] All 30 test groups pass (`make test` + specific `algebra-lattice-test`).
- [ ] `make lint && make covercheck` both clean.
- [ ] Umbrella `(wile algebra)` re-exports all new names.
- [ ] Docstrings on every new export (Parameters / Returns / Category / Keywords / Examples / See also).
- [ ] `plans/CLAUDE.md` entry moved to Completed.
- [ ] `TODO.md` §5.5 entry marked shipped.
- [ ] PR description cross-references consumers from `plans/2026-04-17-algebra-foundations-directions.md` §5.5 (dataflow analysis, FCA connection, free distributive lattice).

---

## Post-ship follow-ups (separate plans)

- **Stable-matching lattice** — Conway 1976. New library or `(wile algebra lattice)` extension with `stable-matching-lattice preferences-lhs preferences-rhs`. Rotation-based traversal as `stable-matching-rotation-lattice`. Independent effort ~300 LOC including the Gale-Shapley matching core.
- **wile-goast `run-analysis` distributive-domain gate** — migrate `dataflow.scm`'s `run-analysis` to check `(distributive? abstract-domain)` and tag MOP=MFP results accordingly. ~30 LOC in wile-goast; separate PR in wile-goast repo.
- **FCA concept-lattice distributivity helper** — if consumers request it, add `(concept-lattice-distributive? C)` as a one-liner wrapper in `(wile algebra fca)`. Deferred until requested.

---

## Future extensions (deferred to v2)

- **Lattice presentations by generators and relations.** Tier-2 lattices. Requires confluent rewriting.
- **Hasse-diagram-first `<lattice>` constructor.** Derives `join`/`meet` from the cover relation.
- **Lattice morphisms / homomorphisms.** `<lattice-morphism>` record, composition, kernel/image.
- **Dedekind-MacNeille completion.** Completion of a poset to a lattice.
- **Witness-carrying `distributive?` / `modular?` variants.** `(distributive-witness L)` returns `(ok)` or `'(not-distributive a b c lhs rhs)`. Low-effort addition if a consumer needs it.
- **Structural (M3/N5-sublattice) check as a separate helper.** `(has-forbidden-sublattice? L)` — useful for pedagogy.
- **Probabilistic / sample-based `modular?` on large lattices.** Via `validate-modular-lattice` with random sampling strategy.
- **Hashtable-backed downset representation.** Only when a consumer profile surfaces the alist bottleneck.
- **Stable-matching-lattice preset.** See post-ship follow-ups.

---

## Risks & cross-cutting concerns

1. **Scope creep into lattice morphism theory or complete-lattice theory.** Both are natural generalizations but each triples the implementation effort. Mitigation: explicit Future Extensions section; docstring on `<lattice>` documents v1 scope as "bounded lattices with finite-enumeration metadata."

2. **Caller-obligation on setoid ⟺ `lattice-equal?`.** If a caller constructs a lattice where `setoid` disagrees with antisymmetric-`leq?`, `distributive?` and Birkhoff silently misbehave. Mitigation: document as a precondition in Representation section; offer `validate-lattice/setoid` in a future pass that spot-checks the invariant.

3. **`free-distributive-lattice 5` performance.** D(5) = 7581 elements; downset enumeration is 2^\|Irr(P)\| where \|Irr(P)\| = 5 (antichain) → 32 downsets candidate subsets filtered to 7581 downsets. Actually: antichain of 5 elements has 2^5 = 32 subsets, all of which are downsets (antichain has trivial order). So FDL(5) = 32 elements? No — free distributive lattice on 5 generators is Downsets(2^[5]) where 2^[5] is the Boolean poset on 5 atoms, which has 32 elements in a non-trivial order. Downsets of that is Dedekind(5) = 7581. Enumeration is O(2^32) ≈ 4 billion subset candidates — infeasible. Need smarter downset enumeration (BFS through the poset, not subset-filter). Document this during Phase 7; may need to cap at n=4 (D(4)=168) if the O(2^\|P\|) approach dominates.

   **Mitigation:** Phase 7 includes a performance benchmark; if `free-distributive-lattice 5` exceeds 30 s, cap v1 at n=4 with the docstring noting the tighter bound. Smarter downset enumeration (recursive: downsets(P) = downsets(P minus maximal element x) ∪ {D ∪ {x} : D ∈ downsets(P minus x), x's predecessors ⊆ D}) reduces to O(D(n)) iterations, feasible up to n=5.

4. **Interaction with `lattice-equal?` = antisymmetric-`leq?` in `fixpoint`.** If D3's setoid is used somewhere and diverges from antisymmetric-`leq?`, `fixpoint` would loop. Mitigation: `fixpoint` is *not modified* in this plan; it continues to use `lattice-equal?`. The setoid is a parallel notion, used only by the new Birkhoff/distributive machinery. Document this boundary in the record-extension docstring.

5. **Test-suite runtime.** Phase 7's `free-distributive-lattice 5` construction could slow the test suite. Mitigation: mark the large preset tests as a separate slow-test group; keep unit tests under 5s total.

---

## Self-review checklist

- [ ] All 15 design questions (Q1–Q15) resolved or explicitly flagged as needing user decision before Phase 1.
- [ ] Every new export documented in the Exports section.
- [ ] Backward compatibility verified: shipped 5-arg `make-lattice`, `lattice-equal?`, and every preset/validator from `lattice.sld` continue to work unchanged.
- [ ] `<locally-finite-poset>` extension preserves shipped 2-arg `make-locally-finite-poset`.
- [ ] Canonical counterexamples (M3, N5) shipped as day-one presets.
- [ ] Test plan includes roundtrip verification on at least three distributive lattices.
- [ ] File structure table lists every modified file with its specific change.
- [ ] Cross-links to §5.4 (extend-in-place, setoid) and §5.3 (process lessons) visible in the Inherited Design Context section.
- [ ] Risks section addresses the `free-distributive-lattice 5` feasibility question with a concrete mitigation.
- [ ] Post-ship follow-ups explicitly list the wile-goast dataflow migration as a downstream wile-goast plan, not a §5.5 task.
