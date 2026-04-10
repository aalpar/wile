# Orthogonal Algebra Types

**Date:** 2026-04-09
**Status:** Proposal
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

## Open Questions

1. **Should Heyting/Boolean extend lattice or wrap it?** Extending
   `<lattice>` to `<heyting-algebra>` with an extra slot is simpler but
   breaks the existing record hierarchy (R7RS records don't support
   inheritance). Wrapping (store a lattice field inside the Heyting record)
   matches the existing pattern — ring stores its operations separately and
   offers `ring->semiring` to project. Wrapping is the likely answer.

2. **Module API: embed ring or pass externally?** See discussion in Module
   section above.

3. **Should action carry a predicate for X?** The existing structures don't
   type-check their carrier sets (monoid-op accepts anything). Consistency
   says: no predicate, just trust the caller. But actions are the first type
   where the two carriers could be confused.

4. **Lattice constructor upgrades.** `powerset-lattice` already produces a
   Boolean algebra. Should the existing constructor be left alone and
   `powerset-boolean` added alongside, or should `powerset-lattice` return a
   `<boolean-algebra>` (which projects to lattice)? The former is less
   disruptive; the latter is more correct.
