# Algebra Library Design

**Date:** 2026-03-25
**Status:** Draft
**Version target:** TBD

## Goal

A general-purpose algebraic structures library for Wile. Provides partial
orders, lattices, monoids, semirings, groups, rings, fields, Galois
connections, and fixpoint computation as composable R7RS records.

Two consumers drive the design:

1. **wile-goast** — static analysis combinators built on lattices, fixpoints,
   and semirings (see wile-goast TODO Track C)
2. **Standalone algebraic computation** — rings, fields, polynomial
   manipulation, symbolic work

## Design Decisions

**Representation: R7RS records.** Each algebraic structure is a
`define-record-type` with operation slots. Records give type predicates
(`lattice?`, `monoid?`), immutability, and named accessors.

**Library organization: flat re-export with sub-libraries.** `(wile algebra)`
re-exports everything. Sub-libraries (`(wile algebra lattice)`,
`(wile algebra monoid)`, etc.) allow selective import.

**Calling convention: explicit structure + `with-` macros.** Operations take
the structure as first argument: `(lattice-join L a b)`. For sustained use,
`with-` macros destructure the operations into bare bindings:

```scheme
(with-lattice L (join meet bottom top leq?)
  (join (join a b) c))
```

**Axiom validation: separate procedures.** Constructors accept functions
without validation. Separate `validate-*` procedures spot-check algebraic
laws against user-supplied samples:

```scheme
(validate-lattice L sample-elements)  ; → #t or list of violations
```

## Library Map

```
(wile algebra)           re-exports all sub-libraries
(wile algebra order)     partial orders
(wile algebra lattice)   lattices, constructors, fixpoint
(wile algebra monoid)    monoids
(wile algebra semiring)  semirings + pre-built instances
(wile algebra group)     groups
(wile algebra ring)      rings, fields
(wile algebra galois)    Galois connections
```

## Hierarchy

Algebraic structures form a projection graph. Each arrow forgets operations
and retains a subset. No inheritance — explicit conversion functions.

```
field ──→ ring ──→ semiring ──→ additive-monoid
            │                └→ multiplicative-monoid
            └→ additive-group ──→ monoid

lattice ──→ partial-order
```

## API

### Partial Orders — `(wile algebra order)`

```scheme
;; Construction
(make-partial-order leq?)            → <partial-order>
(partial-order? x)                   → boolean

;; Operations
(po-leq? po a b)                    → boolean
(po-comparable? po a b)             → boolean  ; a≤b ∨ b≤a

;; Validation
(po-monotone? po f a b)             → boolean  ; a≤b ⟹ f(a)≤f(b)
(validate-partial-order po samples) → #t | violations
  ;; checks: reflexivity, antisymmetry, transitivity
```

### Lattices — `(wile algebra lattice)`

```scheme
;; Construction
(make-lattice join meet bottom top leq?)  → <lattice>
(lattice? x)                              → boolean

;; Operations
(lattice-join L a b)               → element
(lattice-meet L a b)               → element
(lattice-bottom L)                 → element
(lattice-top L)                    → element
(lattice-leq? L a b)              → boolean

;; Projection
(lattice->partial-order L)         → <partial-order>

;; Constructors (build common lattices)
(flat-lattice elements equal?)     → <lattice>   ; ⊥ < each element < ⊤
(powerset-lattice universe)        → <lattice>   ; (P(U), ⊆, ∪, ∩, ∅, U)
(product-lattice L ...)            → <lattice>   ; pointwise on lists
(map-lattice keys value-lattice)   → <lattice>   ; alist: keys → L, pointwise

;; Fixpoint
(fixpoint L f x)                   → element     ; Kleene: f^n(x) until stable
(fixpoint L f x fuel)              → element | #f; bounded iteration
(fixpoint/widen L f x widen)       → element     ; with widening operator
  ;; fixpoint uses case-lambda for the 3/4-arg dispatch

;; Validation
(validate-lattice L samples)       → #t | violations
  ;; checks: commutativity, associativity, absorption,
  ;;         identity (bottom/top), idempotence

;; Macro
(with-lattice L (join meet bottom top leq?) body ...)
  ;; expands to let binding each name to the record accessor
```

### Monoids — `(wile algebra monoid)`

```scheme
;; Construction
(make-monoid op identity)          → <monoid>
(monoid? x)                        → boolean

;; Operations
(monoid-op M a b)                  → element
(monoid-identity M)                → element
(monoid-fold M lst)                → element     ; fold-left from identity
(monoid-power M a n)               → element     ; repeated application

;; Validation
(validate-monoid M samples)        → #t | violations
  ;; checks: associativity, left/right identity

;; Macro
(with-monoid M (op identity) body ...)
```

### Semirings — `(wile algebra semiring)`

```scheme
;; Construction
(make-semiring plus times zero one) → <semiring>
(semiring? x)                       → boolean

;; Operations
(semiring-plus S a b)               → element
(semiring-times S a b)              → element
(semiring-zero S)                   → element
(semiring-one S)                    → element

;; Projection
(semiring->additive-monoid S)       → <monoid>   ; (plus, zero)
(semiring->multiplicative-monoid S) → <monoid>   ; (times, one)

;; Pre-built instances
(boolean-semiring)                 → <semiring>  ; (∨, ∧, #f, #t)
(tropical-semiring)                → <semiring>  ; (min, +, +inf, 0)
(counting-semiring)                → <semiring>  ; (+, *, 0, 1)

;; Validation
(validate-semiring S samples)      → #t | violations
  ;; checks: both monoid laws, distributivity, zero annihilation

;; Macro
(with-semiring S (plus times zero one) body ...)
```

### Groups — `(wile algebra group)`

```scheme
;; Construction
(make-group op identity inverse)   → <group>
(group? x)                         → boolean

;; Operations
(group-inverse G a)                → element
(group-op G a b)                   → element
(group-identity G)                 → element

;; Projection
(group->monoid G)                  → <monoid>

;; Validation
(validate-group G samples)         → #t | violations
  ;; checks: monoid laws + left/right inverse

;; Macro
(with-group G (op identity inverse) body ...)
```

### Rings and Fields — `(wile algebra ring)`

```scheme
;; Rings
(make-ring plus times zero one negate)  → <ring>
(ring? x)                              → boolean
(ring-plus R a b)                      → element
(ring-times R a b)                     → element
(ring-zero R)                          → element
(ring-one R)                           → element
(ring-negate R a)                      → element
(ring-minus R a b)                     → element  ; a + negate(b)

;; Projection
(ring->semiring R)                     → <semiring>
(ring->additive-group R)               → <group>

;; Validation
(validate-ring R samples)              → #t | violations
  ;; checks: additive group, multiplicative monoid,
  ;;         distributivity

;; Fields
(make-field plus times zero one negate reciprocal) → <field>
(field? x)                             → boolean
(field-reciprocal F a)                 → element
(field-divide F a b)                   → element  ; a * reciprocal(b)

;; Projection
(field->ring F)                        → <ring>

;; Validation
(validate-field F samples)             → #t | violations
  ;; checks: ring laws + multiplicative inverse (nonzero elements)

;; Macros
(with-ring R (plus times zero one negate) body ...)
(with-field F (plus times zero one negate reciprocal) body ...)
```

### Galois Connections — `(wile algebra galois)`

```scheme
;; Construction
(make-galois-connection alpha gamma
  concrete-po abstract-lattice)        → <galois-connection>
(galois-connection? x)                 → boolean

;; Operations
(gc-alpha GC concrete-val)             → abstract-val
(gc-gamma GC abstract-val)             → concrete-val
(gc-concrete-po GC)                    → <partial-order>
(gc-abstract-lattice GC)               → <lattice>

;; Validation
(gc-sound? GC concrete-samples abstract-samples) → #t | violations
  ;; checks: ∀c. c ≤ γ(α(c))  (soundness / extensive lower adjoint)
  ;; checks: ∀a. α(γ(a)) ≤ a  (reductive upper adjoint)
```

## Implementation Order

Structures have real dependencies. The critical path:

```
Phase 1: partial orders
Phase 2: lattices (depends on partial orders)
Phase 3: fixpoint (depends on lattices)
Phase 4: monoids  (independent — can parallel with 2-3)
Phase 5: semirings (depends on monoids)
Phase 6: groups (depends on monoids)
Phase 7: rings, fields (depends on semirings + groups)
Phase 8: Galois connections (depends on lattices + partial orders)
```

Phases 1-3 unblock wile-goast Track C. Phases 4-7 serve both consumers.
Phase 8 gates future abstract interpretation work.

## Resolved Questions

1. **Widening operators.** `fixpoint/widen` accepts a user-supplied widening
   function. The algebra library ships no widenings. Widenings are
   domain-specific (interval widening, k-limiting, threshold sets) — they
   belong with the concrete lattice that needs them, not in a generic library.
   wile-goast ships its own widenings alongside domain-specific lattice
   constructors.

2. **Lattice element equality.** Derive from `leq?`: `a = b ⟺ a ≤ b ∧ b ≤ a`.
   Costs two comparisons per convergence check. No explicit `equal?` slot —
   keeps the `make-lattice` constructor at five arguments and avoids a sixth
   function that must agree with `leq?`. Consumers with cheap equality can
   wrap `leq?` to short-circuit: `(lambda (a b) (or (eq? a b) (real-leq? a b)))`.

3. **Pre-built numeric instances.** Ship them. `(integer-ring)`, `(rational-field)`,
   `(modular-ring n)` are 3 lines each wrapping `+`, `*`, `-` from `(scheme base)`.
   No real coupling concern. Useful for testing the library's own validation
   procedures and as documentation-by-example.

## References

- Nielson, Nielson, Hankin. *Principles of Program Analysis*. Springer, 1999.
  Chapters 1-4: lattice theory, fixpoint theory, abstract domains.
- Tarjan. "A Unified Approach to Path Problems." *JACM*, 1981.
  Semiring framework for graph path problems.
- Cousot, Cousot. "Abstract Interpretation: A Unified Lattice Model for
  Static Analysis of Programs." *POPL*, 1977. Galois connections as the
  foundation of abstract interpretation.
