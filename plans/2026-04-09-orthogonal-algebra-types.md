# Orthogonal Algebra Types

**Date:** 2026-04-09
**Status:** Implemented
**Depends on:** 2026-03-25-algebra-library-design.md

## Context

The existing `(wile algebra)` library covers two independent axes:

- **Order-theoretic:** partial-order → lattice → Galois connection (+ fixpoint)
- **Algebraic:** monoid → semiring → ring → field; monoid → group → ring

Every structure is one-sorted (one carrier set). Every arrow between types is a
forgetful functor (drops operations, keeps the carrier). The term rewriting
module is a consumer of the algebra, not a new algebraic dimension.

This document identifies abstract algebra types that are **orthogonal** to those
already represented — structures whose axioms and operations are not derivable
from or reducible to the ones above, and that do not sit in either existing
chain as a refinement.

Two consumers drive the evaluation (same as the original design):

1. **wile-goast** — static analysis: lattices, fixpoints, transfer functions
2. **Standalone algebraic computation** — symbolic work, constraint systems

## Orthogonal Types

### 1. Module (R-Module) / Vector Space

**New operation:** scalar multiplication `R × M → M` where R is a ring and M is
an abelian group, satisfying:

```
r · (m₁ + m₂) = r · m₁ + r · m₂     (distributes over M-addition)
(r₁ + r₂) · m = r₁ · m + r₂ · m     (distributes over R-addition)
(r₁ × r₂) · m = r₁ · (r₂ · m)       (compatible with R-multiplication)
1_R · m = m                            (ring identity acts trivially)
```

**Why orthogonal:** Every existing structure is one-sorted. A module is the
first two-sorted structure — it requires two algebraic objects (a ring and an
abelian group) to interact via a bilinear action. You cannot encode "R acts on
M" within a single ring or group. A vector space is the special case where R is
a field.

**Consumer relevance:**

- **wile-goast:** Polyhedra abstract domains represent constraints as systems of
  linear inequalities over abstract values — these are vector spaces over the
  rationals. Affine transfer functions are module homomorphisms.
- **Standalone:** Linear algebra, symbolic matrix operations, polynomial modules.

**API design question:** The existing pattern stores all operations in a single
record. A module must reference its scalar ring. Two options:

1. Store the ring inside the module record (self-contained, but couples them)
2. Pass the ring externally (lighter record, but the caller must pair them)

Option 1 fits the `with-module` macro pattern better and mirrors how
`ring->additive-group` already embeds substructure.

**Sketch:**

```scheme
(make-module ring group scalar-multiply)  → <module>
(module-ring M)                           → <ring>
(module-group M)                          → <group>
(module-scale M r m)                      → element
(module->additive-group M)                → <group>
(validate-module M ring-samples module-samples) → #t | violations
(with-module M (scale add zero negate) body ...)
```

**Pre-built instances:**

- `(rational-vector-space n)` — Qⁿ as a module over rational-field
- `(polynomial-module R)` — R[x] as a module over ring R (if polynomials exist)

---

### 2. Heyting Algebra (Bounded Lattice + Relative Pseudo-complement)

**New operation:** implication `a → b`, defined as the largest `c` such that
`a ∧ c ≤ b`. Equivalently, `meet(a, -)` has a right adjoint for every `a`.

**Axioms (beyond lattice):**

```
a ∧ (a → b) ≤ b                         (modus ponens)
c ≤ (a → b)  iff  a ∧ c ≤ b             (adjunction / residuation)
```

**Why orthogonal:** The existing lattice has join and meet but no implication.
Implication is not derivable from join/meet — it requires each `meet(a, -)`
functor to have a right adjoint, which is an independent axiom. A Heyting
algebra is strictly richer than a bounded lattice but not a refinement of ring
or field (it has no additive inverse in general).

**Consumer relevance:**

- **wile-goast:** Constructive reasoning in type systems ("if S <: T then ..."),
  representing logical implications in abstract domains, computing weakest
  preconditions. Every finite distributive lattice is Heyting — so
  `powerset-lattice` and `map-lattice` already produce Heyting algebras, but
  the library cannot compute implication on them yet.
- **Standalone:** Intuitionistic propositional logic, Kripke semantics.

**Observation:** Boolean algebra (see below) is the special case where every
Heyting implication `a → ⊥` is a true complement. Implementing Heyting first
makes Boolean algebra a one-line refinement.

**Sketch:**

```scheme
(make-heyting-algebra join meet bottom top leq? implies) → <heyting-algebra>
(heyting-algebra? x)                     → boolean
(heyting-implies H a b)                  → element   ; a → b
(heyting-negate H a)                     → element   ; a → ⊥ (pseudo-complement)
(heyting->lattice H)                     → <lattice>

;; Constructors that lift existing lattices
(powerset-heyting universe)              → <heyting-algebra>
(map-heyting keys value-heyting)         → <heyting-algebra>

(validate-heyting-algebra H samples)     → #t | violations
  ;; checks: lattice laws + modus ponens + adjunction
(with-heyting H (join meet bottom top leq? implies) body ...)
```

---

### 3. Boolean Algebra (Complemented Distributive Lattice)

**New operation:** complement `¬a` where `a ∧ ¬a = ⊥` and `a ∨ ¬a = ⊤`.

**Axioms (beyond lattice):**

```
a ∧ ¬a = ⊥                              (non-contradiction)
a ∨ ¬a = ⊤                              (excluded middle)
a ∧ (b ∨ c) = (a ∧ b) ∨ (a ∧ c)        (distributivity)
```

**Why orthogonal:** Extends the lattice tower with an operation that the
algebraic tower has an analog of (ring negation) but that works completely
differently. A Boolean algebra is simultaneously:

- A complemented distributive lattice (order-theoretic view)
- A Heyting algebra where `a → ⊥` is a true complement (logical view)
- A ring of characteristic 2 where `a + b = a △ b` (algebraic view)

It bridges the two towers at a concrete point, but the complement operation is
required to make the connection. The existing `boolean-semiring` captures the
ring-of-characteristic-2 perspective but not the lattice-complement perspective.

**Consumer relevance:**

- **wile-goast:** Bit-vector analysis, must/may analysis pairs (definite =
  Boolean complement of possible), condition analysis, flag tracking. The
  `powerset-lattice` is already a Boolean algebra — the library just can't
  express complement on it.
- **Standalone:** Boolean satisfiability, digital circuit analysis, set algebra.

**Sketch:**

```scheme
(make-boolean-algebra join meet bottom top leq? complement) → <boolean-algebra>
(boolean-algebra? x)                     → boolean
(boolean-complement B a)                 → element
(boolean->heyting B)                     → <heyting-algebra>
(boolean->lattice B)                     → <lattice>

;; Ring-of-characteristic-2 view
(boolean->ring B)                        → <ring>
  ;; plus = symmetric-difference, times = meet, zero = ⊥, one = ⊤

;; Constructors
(powerset-boolean universe)              → <boolean-algebra>

(validate-boolean-algebra B samples)     → #t | violations
  ;; checks: lattice laws + complement + distributivity
(with-boolean B (join meet bottom top leq? complement) body ...)
```

---

### 4. Monoid Action (M-Set)

**New operation:** action `M × X → X` where M is a monoid and X is a set.

**Axioms:**

```
e · x = x                               (identity acts trivially)
(m₁ ⊕ m₂) · x = m₁ · (m₂ · x)         (action respects composition)
```

**Why orthogonal:** Like modules but without requiring any algebraic structure
on X. Modules require X to be an abelian group acted on by a ring; actions
require X to be a bare set acted on by a monoid. This is the minimal two-sorted
structure. Orthogonal to everything in the library because no existing type has
an external carrier that is not itself algebraically structured.

**Consumer relevance:**

- **wile-goast:** Abstract transfer functions are exactly this. A dataflow
  analysis has a monoid of state transformers (compose = ⊕, identity = e)
  acting on a lattice of abstract states. The library already has the monoid
  and the lattice separately; the action connects them. This would give
  wile-goast a way to represent, compose, and apply transfer functions as
  first-class algebraic objects.
- **Standalone:** Automata (monoid of input sequences acting on states),
  permutation groups acting on sets, any "transformer acts on state" pattern.

**Observation:** Module is a special case (X is an abelian group, M's monoid
comes from a ring). Implementing action first would let module build on it,
but the axiom sets are different enough that composition may not save much.

**Sketch:**

```scheme
(make-action monoid act)                 → <action>
(action? x)                             → boolean
(action-monoid A)                        → <monoid>
(action-act A m x)                       → element   ; m · x
(action-orbit A x ms)                    → list      ; sequence of m₁·x, m₂·m₁·x, ...

(validate-action A monoid-samples state-samples) → #t | violations
  ;; checks: identity action, composition compatibility
(with-action A (act monoid-op identity) body ...)
```

**Pre-built instances:**

- `(endomorphism-action)` — monoid of `(compose, identity)` acting by
  application. The "free" action.

## Relationship Diagram

```
                        ┌────────────────┐
                        │ Monoid Action  │
                        │  M × X → X    │
                        └───────┬────────┘
                                │ X = abelian group,
                                │ M from ring
                        ┌───────▼────────┐
                        │    Module      │
                        │  R × M → M    │
                        └────────────────┘

field → ring → semiring → monoid (additive)
          │           └──→ monoid (multiplicative)
          └→ group ──────→ monoid

              ┌──────────────────┐
              │ Boolean Algebra  │
              │ lattice+complement│
              └───────┬──────────┘
                      │ forget complement
              ┌───────▼──────────┐
              │ Heyting Algebra  │
              │ lattice+implies  │
              └───────┬──────────┘
                      │ forget implies
              ┌───────▼──────────┐
              │     Lattice      │
              └───────┬──────────┘
                      │ forget join/meet
              ┌───────▼──────────┐
              │  Partial Order   │
              └──────────────────┘
```

The four new types (bold in concept) extend the library along two new
dimensions:

1. **Two-sorted structures** (action, module) — elements of one type operated
   on by elements of another
2. **Logical connectives on lattices** (Heyting, Boolean) — implication and
   complement

Neither dimension is reachable by refining existing types.

## Priority Assessment

| Type             | wile-goast value | Standalone value | Impl. complexity |
|------------------|------------------|------------------|------------------|
| Monoid Action    | High (transfer)  | Medium           | Low              |
| Heyting Algebra  | High (types)     | Medium           | Low-Medium       |
| Boolean Algebra  | High (must/may)  | Medium           | Low (if Heyting) |
| Module           | Medium (poly.)   | High             | Medium           |

Suggested order: Heyting → Boolean → Action → Module. Heyting and Boolean are
small additions that enrich the existing lattice constructors. Action is
structurally novel but simple. Module is the most complex due to the two-sorted
API pattern being new to the library.

## Scope Decision

**Implement Heyting + Boolean only.** Monoid Action and Module are deferred.

| Type           | Verdict      | Rationale                                                |
|----------------|--------------|----------------------------------------------------------|
| Heyting        | **Yes**      | Enables WP reasoning on existing lattice constructors    |
| Boolean        | **Yes**      | Must/may duality on powerset-lattice                     |
| Monoid Action  | Deferred     | Needs interprocedural analysis to be load-bearing        |
| Module         | Deferred     | No numerical abstract domains exist in wile-goast        |

**Key observation:** `flat-lattice` with 3+ elements is NOT distributive
(verified: `a ∧ (b ∨ c) ≠ (a ∧ b) ∨ (a ∧ c)` when a, b, c are
incomparable). Therefore `flat-lattice` is NOT a Heyting algebra. Only
`powerset-lattice` and `map-lattice` (over a distributive value-lattice) get
lifted constructors.

## Resolved Questions

### Q1: Wrap or extend?

**Wrap.** Store a lattice inside the Heyting/Boolean record. This matches the
existing pattern — `<ring>` stores its own operation slots and offers
`ring->semiring` to project. R7RS `define-record-type` has no inheritance, so
wrapping is the only composable option.

The wrapping stores operation closures, not a lattice record. This keeps
Heyting/Boolean records flat (no indirection to access join/meet) and lets
`heyting->lattice` / `boolean->lattice` reconstruct a `<lattice>` from the
stored closures — exactly how `ring->semiring` works.

### Q2: Lattice constructor upgrades

**Add alongside, don't change.** `powerset-lattice` continues to return
`<lattice>`. New constructors `powerset-heyting` and `powerset-boolean` return
the richer types. Rationale:

- No existing code breaks
- Existing callers that only need lattice operations pay no conceptual cost
- The upgrade path is explicit: `(powerset-boolean U)` instead of
  `(powerset-lattice U)` when you need complement

`map-heyting` lifts a Heyting value-lattice to a Heyting map-lattice. No
`map-boolean` — the map-lattice complement requires enumerating the value
domain, which is only sound for finite value lattices. Callers can construct
one manually if they know their domain is finite.

### Q3: Boolean→Heyting derivation

`boolean->heyting` derives implies from complement:
`a → b = ¬a ∨ b`. This is a one-line derivation, not a stored field.

### Q4: Boolean→Ring derivation

`boolean->ring` constructs a ring of characteristic 2:
- plus = symmetric difference = `(a ∨ b) ∧ ¬(a ∧ b)`
- times = meet
- zero = ⊥, one = ⊤
- negate = identity (every element is its own additive inverse)

This bridges the lattice tower and the algebraic tower at a concrete point.

## Concrete API

### Heyting Algebra — `(wile algebra heyting)`

```scheme
;; Record: flat, stores closures + constants directly
(define-record-type <heyting-algebra>
  (make-heyting-algebra* join-fn meet-fn bottom top leq-fn implies-fn)
  heyting-algebra?
  (join-fn    heyting-join-fn)
  (meet-fn    heyting-meet-fn)
  (bottom     heyting-bottom)
  (top        heyting-top)
  (leq-fn     heyting-leq-fn)
  (implies-fn heyting-implies-fn))

;; Constructor
(make-heyting-algebra join meet bottom top leq? implies)
  → <heyting-algebra>

;; Operations
(heyting-join H a b)       → element
(heyting-meet H a b)       → element
(heyting-leq? H a b)       → boolean
(heyting-implies H a b)    → element   ; a → b
(heyting-negate H a)       → element   ; a → ⊥ (pseudo-complement, derived)

;; Projection
(heyting->lattice H)       → <lattice>

;; Constructors
(powerset-heyting universe)             → <heyting-algebra>
  ;; implies: (U \ a) ∪ b
(map-heyting keys value-heyting)        → <heyting-algebra>
  ;; implies: pointwise on values

;; Validation
(validate-heyting-algebra H samples)    → #t | violations
  ;; checks: lattice laws (delegates to validate-lattice)
  ;;       + modus ponens: a ∧ (a → b) ≤ b
  ;;       + adjunction: for all c in samples,
  ;;           c ≤ (a → b) iff a ∧ c ≤ b

;; Macro
(with-heyting H (join meet bottom top leq? implies) body ...)
```

### Boolean Algebra — `(wile algebra boolean)`

```scheme
;; Record: flat, stores closures + constants directly
(define-record-type <boolean-algebra>
  (make-boolean-algebra* join-fn meet-fn bottom top leq-fn complement-fn)
  boolean-algebra?
  (join-fn       boolean-join-fn)
  (meet-fn       boolean-meet-fn)
  (bottom        boolean-bottom)
  (top           boolean-top)
  (leq-fn        boolean-leq-fn)
  (complement-fn boolean-complement-fn))

;; Constructor
(make-boolean-algebra join meet bottom top leq? complement)
  → <boolean-algebra>

;; Operations
(boolean-join B a b)        → element
(boolean-meet B a b)        → element
(boolean-leq? B a b)        → boolean
(boolean-complement B a)    → element

;; Projections
(boolean->heyting B)        → <heyting-algebra>
  ;; implies derived: ¬a ∨ b
(boolean->lattice B)        → <lattice>
(boolean->ring B)           → <ring>
  ;; plus = symmetric-difference, times = meet, zero = ⊥, one = ⊤

;; Constructors
(powerset-boolean universe) → <boolean-algebra>
  ;; complement: set difference from universe

;; Validation
(validate-boolean-algebra B samples) → #t | violations
  ;; checks: lattice laws (delegates to validate-lattice)
  ;;       + complement:      a ∧ ¬a = ⊥, a ∨ ¬a = ⊤
  ;;       + distributivity:  a ∧ (b ∨ c) = (a ∧ b) ∨ (a ∧ c)

;; Macro
(with-boolean B (join meet bottom top leq? complement) body ...)
```

## Library Organization

New sub-libraries:

```
stdlib/lib/wile/algebra/heyting.sld   → (wile algebra heyting)
stdlib/lib/wile/algebra/heyting.scm
stdlib/lib/wile/algebra/boolean.sld   → (wile algebra boolean)
stdlib/lib/wile/algebra/boolean.scm
```

Dependency chain:

```
(wile algebra order)
  ↑
(wile algebra lattice)
  ↑
(wile algebra heyting)     imports: (scheme base), (wile algebra lattice)
  ↑
(wile algebra boolean)     imports: (scheme base), (wile algebra heyting),
                                    (wile algebra lattice),
                                    (wile algebra ring)
```

`boolean.scm` imports `(wile algebra ring)` for `boolean->ring`. This creates
a cross-tower dependency — the first in the library. It's justified because
Boolean algebra genuinely IS the bridge point between the two towers.

Umbrella `(wile algebra)` adds both to its imports and exports.

## Test Plan

New test files:

```
test/wile/algebra-heyting-test.scm
test/wile/algebra-boolean-test.scm
```

### Heyting tests

1. **Construction:** `heyting-algebra?` predicate, non-Heyting returns `#f`
2. **powerset-heyting:**
   - `heyting-implies` on concrete sets (e.g., `{a} → {a,b} = U`)
   - `heyting-negate` (`{a} → ⊥ = {b,c}` = complement)
   - Modus ponens: `a ∧ (a → b) ≤ b` for sample pairs
3. **map-heyting:** pointwise implication, bottom/top behavior
4. **heyting->lattice:** resulting lattice agrees with direct operations
5. **validate-heyting-algebra:** passes on `powerset-heyting`, catches a
   broken implies function
6. **with-heyting:** macro destructuring

### Boolean tests

1. **Construction:** `boolean-algebra?` predicate
2. **powerset-boolean:**
   - `boolean-complement` on concrete sets
   - Non-contradiction: `a ∧ ¬a = ⊥`
   - Excluded middle: `a ∨ ¬a = ⊤`
   - Involution: `¬¬a = a`
3. **boolean->heyting:** implies agrees with `¬a ∨ b`
4. **boolean->lattice:** projects correctly
5. **boolean->ring:** symmetric difference is commutative, associative;
   `meet` distributes over it; identity `a + a = ⊥`
6. **validate-boolean-algebra:** passes on `powerset-boolean`, catches
   non-distributive lattice or broken complement
7. **with-boolean:** macro destructuring

### Integration test additions

Add to `algebra-integration-test.scm`:

1. **Projection chain:** `boolean->heyting->lattice->partial-order`
2. **Boolean↔Ring bridge:** `boolean->ring` then `ring->semiring`, verify
   operations agree
3. **Powerset round-trip:** `powerset-boolean` complement + join recovers
   universe

## Implementation Phases

### Phase 1: Heyting Algebra

Files: `heyting.sld`, `heyting.scm`, `algebra-heyting-test.scm`

1. Record type and constructor
2. Core operations: `heyting-join`, `heyting-meet`, `heyting-leq?`,
   `heyting-implies`, `heyting-negate` (derived: `implies H a (heyting-bottom H)`)
3. `heyting->lattice` projection
4. `powerset-heyting` constructor — implies = `(union (set-diff universe a) b)`
5. `map-heyting` constructor — implies = pointwise
6. `validate-heyting-algebra` — delegates lattice laws + checks modus ponens
   and adjunction
7. `with-heyting` macro
8. Tests
9. Add to umbrella `(wile algebra)` exports and imports

### Phase 2: Boolean Algebra

Files: `boolean.sld`, `boolean.scm`, `algebra-boolean-test.scm`

1. Record type and constructor
2. Core operations: `boolean-join`, `boolean-meet`, `boolean-leq?`,
   `boolean-complement`
3. `boolean->heyting` — derives implies from complement: `(join (complement a) b)`
4. `boolean->lattice` projection
5. `boolean->ring` — symmetric difference, meet, identity negate
6. `powerset-boolean` constructor — complement = set difference from universe
7. `validate-boolean-algebra` — delegates lattice laws + checks complement
   and distributivity
8. `with-boolean` macro
9. Tests
10. Add to umbrella exports/imports

### Phase 3: Integration tests + cleanup

1. Add cross-tower tests to `algebra-integration-test.scm`
2. `make lint && make covercheck`
3. Docstrings on all public procedures (follow existing Guile-style pattern)

## Internal Helpers

Both `powerset-heyting` and `powerset-boolean` need `set-diff`. The existing
`powerset-lattice` already defines internal `union`, `intersect`, `subset?`.
Rather than duplicate, each constructor defines its own local set operations
(they're 3-4 lines each). No shared utility — consistent with how
`powerset-lattice` already works.

```scheme
;; set-diff: elements of a not in b
(define (set-diff a b)
  (cond ((null? a) '())
        ((member (car a) b) (set-diff (cdr a) b))
        (else (cons (car a) (set-diff (cdr a) b)))))
```

## Deferred Work

### Monoid Action

Deferred until wile-goast needs interprocedural analysis (function summaries
as composed transfer functions). The current worklist solver applies transfers
one block at a time and never composes them.

### Module / Vector Space

Deferred indefinitely. Requires numerical abstract domains (polyhedra,
intervals, octagons) that don't exist in wile-goast and aren't on the
roadmap.

### FCA concept lattice as Heyting algebra (wile-goast follow-up)

wile-goast's FCA library (`fca.scm`) hand-implements intent/extent as a
Galois connection and computes concept lattices via NextClosure. Every concept
lattice is a complete lattice, hence a Heyting algebra. The Galois connection
already provides the ingredients — the missing piece is the implication
operation.

**What it upgrades.** The current `cross-boundary-concepts` filter answers a
symmetric, binary question: "which field groupings span multiple struct
types?" Heyting implication adds a directional one: "does accessing field set
A imply accessing field set B?" The pseudo-complement (`heyting-negate`)
identifies functions that are structurally excluded from a coupling pattern —
**evidence against merging**.

Example: given functions that access `{Cache.Entries, Index.Keys}` together
and functions that access only one, the pseudo-complement of the cross-
boundary concept identifies the single-struct functions — the ones that prove
the coupling isn't universal and would break if the structs were merged. The
current boundary report doesn't surface this.

**Concept lattice operations.** Meet and join on concepts are computable
directly from `intent`, `extent`, and `fca-close` (all exist in `fca.scm`):

```
meet(C₁, C₂)  =  (A₁ ∩ A₂, close(B₁ ∪ B₂))
join(C₁, C₂)  =  (extent(close(B₁ ∩ B₂)), close(B₁ ∩ B₂))
```

Heyting implication `C₁ → C₂` = largest concept `C₃` where `C₁ ∧ C₃ ≤ C₂`,
computable by enumeration over the (finite, already-computed) concept lattice.

**Implementation shape.** A thin adapter in wile-goast's `fca.scm`, not in
wile itself:

```scheme
(define (concept-lattice->heyting ctx lat)
  (make-heyting-algebra
    (lambda (a b) (fca-concept-join ctx a b))
    (lambda (a b) (fca-concept-meet ctx a b))
    bottom-concept top-concept
    (lambda (a b) (subset? (concept-extent a) (concept-extent b)))
    (lambda (a b) (fca-concept-implies ctx lat a b))))
```

This depends on `(wile algebra heyting)` shipping first. No new Go primitives
needed — ~30 lines of Scheme on top of existing FCA infrastructure.

