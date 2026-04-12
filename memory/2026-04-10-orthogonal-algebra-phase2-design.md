# Orthogonal Algebra Types — Phase 2

**Date:** 2026-04-10
**Status:** Implemented
**Depends on:** 2026-04-09-orthogonal-algebra-types.md (Phase 1: Heyting + Boolean)

## Context

Phase 1 added Heyting and Boolean algebras along the "logical connectives on
lattices" axis. This phase adds four types along new axes that are genuinely
orthogonal to the existing library — none is a refinement of or derivable from
existing structures.

## New Types

### 1. Setoid (Explicit Equivalence Relation)

**Axis:** Foundational — below partial order in the concept hierarchy.

**New operation:** `equiv? : A x A -> Bool` satisfying reflexivity, symmetry,
transitivity.

**Why orthogonal:** Every existing structure implicitly assumes Scheme's
`equal?`. A Setoid makes equality an explicit, parameterized operation. This
is not derivable from any existing structure. Lattices derive equality from
`leq?`, but that's lattice-specific; a Setoid works over bare sets.

**Gap it fills:** `validate-partial-order` cannot check antisymmetry because
it has no equality predicate. A new `validate-partial-order/setoid` function
uses the Setoid's `equiv?` to close this gap.

**API:**

```scheme
(define-record-type <setoid>
  (make-setoid* equiv-fn)
  setoid?
  (equiv-fn setoid-equiv-fn))

(make-setoid equiv?)                        -> <setoid>
(setoid-equiv? S a b)                       -> boolean

;; Pre-built
(default-setoid)                            -> <setoid>   ; equal?
(numeric-setoid)                            -> <setoid>   ; =
(string-setoid)                             -> <setoid>   ; string=?
(eqv-setoid)                                -> <setoid>   ; eqv?

;; Derived
(setoid-equivalence-class S elem samples)   -> list

;; Validation
(validate-setoid S samples)                 -> #t | violations
  ;; reflexivity, symmetry, transitivity

;; Interaction with existing types
(validate-partial-order/setoid po setoid samples)  -> #t | violations
  ;; existing checks + antisymmetry: a <= b /\ b <= a => equiv?(a, b)

(with-setoid S (equiv?) body ...)
```

**Library:** `(wile algebra setoid)`
**Imports:** `(scheme base)`

### 2. Category (Morphisms + Composition)

**Axis:** Category-theoretic — morphism composition as first-class algebra.

**New operations:** `compose : Morph x Morph -> Morph`, `identity : Obj -> Morph`,
with associativity and identity laws.

**Why orthogonal:** The existing forgetful functors (boolean->heyting->lattice)
are informal Scheme functions. A Category formalizes morphism composition as a
first-class algebraic object with testable laws. No existing structure captures
"things that compose associatively with identities per object."

**Design note:** Monoids are endomorphism categories with one object. The
projection `category->endomorphism-monoid` makes this explicit.

**Composition convention:** `(category-compose C f g)` = f . g (mathematical
convention, matches Scheme's `compose` — apply g first, then f).

**API:**

```scheme
(define-record-type <category>
  (make-category* compose-fn identity-fn equiv-fn)
  category?
  (compose-fn  category-compose-fn)
  (identity-fn category-identity-fn)
  (equiv-fn    category-equiv-fn))

(make-category compose identity equiv?)     -> <category>
(category-compose C f g)                    -> morphism
(category-identity C obj)                   -> morphism
(category-equiv? C f g)                     -> boolean

;; Projection
(category->endomorphism-monoid C obj)       -> <monoid>

;; Pre-built
(procedure-category)                        -> <category>

;; Validation
(validate-category C morphism-triples identity-morphisms)  -> #t | violations
  ;; morphism-triples: ((f g h) ...) for associativity
  ;; identity-morphisms: ((id f dom-id) ...) for identity laws

(with-category C (compose identity equiv?) body ...)
```

**Library:** `(wile algebra category)`
**Imports:** `(scheme base)`, `(wile algebra monoid)`

### 3. Closure Operator

**Axis:** Topological — fixed-point structure on lattices.

**New operation:** `close : L -> L` satisfying extensiveness, monotonicity,
idempotency.

**Why orthogonal:** A closure operator on a lattice is not a lattice
homomorphism, not an algebraic operation, and not derivable from join/meet.
It is an independent structure that selects fixed points ("closed elements")
which themselves form a lattice.

**API:**

```scheme
(define-record-type <closure-operator>
  (make-closure-operator* close-fn lattice)
  closure-operator?
  (close-fn closure-close-fn)
  (lattice   closure-lattice))

(make-closure-operator close lattice)       -> <closure-operator>
(closure-close C a)                         -> element
(closure-closed? C a)                       -> boolean
(closure-lattice C)                         -> <lattice>

;; Derived
(closed-elements C samples)                 -> list

;; Projection
(closure->closed-lattice C samples)         -> <lattice>
  ;; meet = cl(lattice-meet), join = lattice-join

;; Pre-built
(downward-closure-operator po universe)     -> <closure-operator>
  ;; on powerset-lattice: downward-close under po

;; Validation
(validate-closure-operator C samples)       -> #t | violations
  ;; extensive, monotone, idempotent

(with-closure C (close lattice) body ...)
```

**Library:** `(wile algebra closure)`
**Imports:** `(scheme base)`, `(wile algebra lattice)`

### 4. Differential Ring (Derivation on a Ring)

**Axis:** Calculus on algebraic structures — endomorphism with Leibniz rule.

**New operation:** `deriv : R -> R` satisfying additivity and the Leibniz rule
`D(a*b) = D(a)*b + a*D(b)`.

**Why orthogonal:** Adds an endomorphism on Ring with a specific interaction
law (Leibniz). Not derivable from ring operations — it's additional structure
on top. No existing type captures "operation that respects both addition and
multiplication in this specific asymmetric way."

**Showcase: dual numbers.** The dual-number ring R[e]/(e^2=0) gives forward-mode
automatic differentiation: elements are `(a . b)` representing `a + b*e`,
and `deriv` extracts the e-coefficient.

**API:**

```scheme
(define-record-type <differential-ring>
  (make-differential-ring* ring deriv-fn)
  differential-ring?
  (ring     differential-ring-ring)
  (deriv-fn differential-ring-deriv-fn))

(make-differential-ring ring deriv)         -> <differential-ring>
(differential-deriv D a)                    -> element
(differential-ring-ring D)                  -> <ring>

;; Derived
(differential-nth-deriv D n a)              -> element
(differential-constant? D a)                -> boolean

;; Projection
(differential-ring->ring D)                 -> <ring>

;; Pre-built
(dual-number-ring)                          -> <differential-ring>
  ;; R[e]/(e^2=0), forward-mode AD

(polynomial-derivation R)                   -> <differential-ring>
  ;; polynomials over R, formal derivative

;; Validation
(validate-differential-ring D samples)      -> #t | violations
  ;; delegates ring laws + additivity + Leibniz

(with-differential D (plus times zero one negate deriv) body ...)
```

**Library:** `(wile algebra differential)`
**Imports:** `(scheme base)`, `(wile algebra ring)`

## Implementation Order

Setoid -> Category -> Closure -> Differential

Rationale:
1. Setoid is foundational, fills a known gap, zero dependencies on new types
2. Category is self-contained, introduces new dimension
3. Closure depends on lattice (existing), nothing new
4. Differential depends on ring (existing), most application-specific

## Relationship Diagram

```
                    +------------------+
                    |     Setoid       |  NEW — foundational
                    |  equiv? : A^2->B |
                    +--------+---------+
                             | enables antisymmetry
                    +--------v---------+
                    |  Partial Order   |  existing
                    +--------+---------+
                             |
                    +--------v---------+     +------------------+
                    |     Lattice      |     |    Category      |  NEW — orthogonal
                    +--+-----+---------+     | compose, id      |
                       |     |               +------------------+
            +----------+     +----------+
            |                           |
  +---------v----------+    +-----------v---------+
  |  Closure Operator  |    |  Heyting / Boolean  |  existing
  |  close : L -> L    |    +---------------------+
  +--------------------+
       NEW — topological

  Ring ----------------------------------------> Differential Ring
  (existing)     adds deriv : R -> R              NEW — calculus
```

## Test Strategy

Each type gets:
1. Unit test file: `test/wile/algebra-{name}-test.scm`
2. Validation tests (pass on valid, catch broken instances)
3. Pre-built instance tests
4. Projection/interaction tests
5. Integration tests in `algebra-integration-test.scm`

Cross-type integration tests:
- Setoid + partial-order: antisymmetry check
- Category -> endomorphism-monoid -> validate-monoid
- Closure -> closed-lattice -> validate-lattice
- Differential -> validate-ring on underlying ring
- Dual-number AD: compute known derivatives, verify

## Deferred

- `map-setoid`: lifting setoid over key-value maps (no consumer)
- `functor`: requires two categories (add when category has consumers)
- `natural-transformation`: requires functors
- `interior-operator`: dual of closure operator (add if needed)
