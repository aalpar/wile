# (wile algebra) -- Algebraic Structures and Symbolic Rewriting

## Overview

The `(wile algebra)` library provides algebraic structures as composable
R7RS records, equational rewriting driven by axiom objects, and symbolic
normalization with transformation tracing. It covers setoids, partial
orders, lattices, closure operators, Heyting and Boolean algebras, monoids,
categories, semirings, groups, rings, differential rings, fields, and
Galois connections. Import `(wile algebra)` for all exports, or individual
sub-libraries like `(wile algebra monoid)` or `(wile algebra rewrite)` for
a narrower scope.

Here is what using it looks like:

```scheme
(import (wile algebra))

(define str-monoid (make-monoid string-append ""))
(monoid-fold str-monoid '("hello" " " "world"))  ;=> "hello world"
(validate-monoid str-monoid '("" "a" "bc"))       ;=> #t
```

Three lines: construct a structure from closures, compute with it, verify
its laws hold. The rest of the library follows the same pattern at every
level of abstraction.

## Design Philosophy

Structures are R7RS records whose fields are operation closures. A monoid
stores a binary operation and an identity element; a ring stores plus,
times, zero, one, and negate. There is no class hierarchy and no
inheritance. Composition happens explicitly: `ring->semiring` extracts the
semiring inside a ring by building a new semiring record from the ring's
plus, times, zero, and one. These forgetful projections mirror the
category-theoretic notion of a forgetful functor -- they discard operations
the target structure does not need.

Every structure serves three orthogonal roles. *Operationally*, its
closures compute: `(ring-plus Z 3 4)` returns 7. *Equationally*, the
structure's axioms drive symbolic rewriting: `ring->theory` projects a ring
into a theory whose axioms (additive identity, multiplicative absorbing
element, commutativity, etc.) normalize S-expression terms.
*Explanatorily*, named axioms carry human-readable names and general-form
strings, and the recursive normalizer records every rewrite step in a
trace, so the path from input to normal form is auditable.

This separation means you can compute with a ring, derive a rewriting
theory from the same ring, simplify symbolic expressions using that theory,
and inspect exactly which axioms fired and in what order -- all from one
record definition.

## Structure Hierarchy

The library organizes into three layers: a lattice-theoretic foundation,
an algebraic tower, and a rewriting/symbolic layer. Arrows indicate
forgetful projections (the target forgets some structure).

```
Foundation
----------
Setoid
  |
Partial Order  <--  Lattice  -->  Closure Operator
                      |                  |
                      |           closed-lattice
                      |
                   Heyting
                      |
                   Boolean  -->  Ring (char 2, via symmetric difference)

Algebra
-------
Monoid  <--  Group
  ^            ^
  |            |
  |       additive-group
  |            |
Semiring  <--  Ring  <--  Field
  |              |
  |        Differential Ring
  |
  +--  additive-monoid
  +--  multiplicative-monoid

Category  -->  Monoid (endomorphism monoid at a fixed object)

Rewriting
---------
Axiom objects  +  Term protocol  -->  Normalizer (single-step)
Named axiom  +  Theory  -->  Recursive normalizer (to fixed point, with trace)
Structure-to-theory projections:
  monoid->theory, group->theory, semiring->theory, ring->theory,
  field->theory, lattice->theory, heyting->theory, boolean->theory
```

Each arrow discards exactly one capability. `field->ring` forgets the
reciprocal. `ring->semiring` forgets negation. `boolean->heyting` forgets
complement. `heyting->lattice` forgets implication. `group->monoid` forgets
inverse. This means any algorithm written against a semiring works on
rings and fields too -- just project first.

Galois connections bridge two partial orders with an adjoint pair of
monotone maps (alpha, gamma). They do not participate in the forgetful
projection chain but connect concrete and abstract domains.

## Patterns

**Validation.** Every structure type has a `validate-X` procedure that
spot-checks algebraic laws against sample elements. Pass a structure and a
list of sample values; it returns `#t` if all laws hold or a list of
violation descriptions if any fail. This is not a proof -- it is a
property-based sanity check. Use it during development to catch mistakes in
custom structure definitions.

**Destructuring macros.** Each structure type has a `with-X` syntax macro
that binds its operations to local names. `(with-ring Z (plus times zero
one negate) ...)` lets you write `(times (plus a b) (plus a (negate b)))`
instead of `(ring-times Z (ring-plus Z a b) (ring-plus Z a (ring-negate Z
b)))`. The names are yours to choose.

**Forgetful projections.** Functions like `ring->semiring`, `group->monoid`,
`boolean->heyting->lattice` extract simpler structures from richer ones.
They build new records from the relevant fields of the source. Code that
only needs a monoid can accept one projected from a group, ring, or
semiring -- no adapter needed.

**Predicate-based matching.** Axiom constructors like `make-identity-axiom`
and `make-absorbing-axiom` take a predicate, not a value. The identity
axiom for addition takes `(lambda (x) (eq? x 'zero))`, not the symbol
`zero` itself. This lets axioms match structural identity without requiring
`equal?` on arbitrary terms.

**`#f` for no-match.** A single-step normalizer built by `make-normalizer`
returns the rewritten term when a rule fires and `#f` when no rule applies.
This makes it easy to compose normalizers or loop until a fixed point: keep
applying until the result is `#f`.

## Learning Path

The examples build on each other. Work through them in order.

1. **[`examples/algebra/getting-started.scm`](../examples/algebra/getting-started.scm)** --
   Monoids from scratch. Covers `make-monoid`, `monoid-fold`, `monoid-power`,
   `validate-monoid`, and `with-monoid`. Demonstrates that monoids work on
   strings, not just numbers.

2. **[`examples/algebra/structures.scm`](../examples/algebra/structures.scm)** --
   Lattices, rings, fields, Boolean algebras, and forgetful projections.
   Shows the two-step chain `boolean->heyting->lattice` and the cross-tower
   projection `boolean->ring`.

3. **[`examples/algebra/rewriting.scm`](../examples/algebra/rewriting.scm)** --
   Term protocols and all seven axiom types: identity, commutativity,
   absorbing, idempotence, involution, absorption, associativity. Builds
   composed normalizers from multiple axioms.

4. **[`examples/algebra/symbolic.scm`](../examples/algebra/symbolic.scm)** --
   Named axioms, theories, theory combinators (`filter`, `exclude`,
   `prioritize`, `merge`), the recursive normalizer, transformation traces
   via `format-trace`, structure-to-theory projections, and fuel exhaustion.

5. **[`examples/algebra/boolean-simplifier.scm`](../examples/algebra/boolean-simplifier.scm)** --
   End-to-end workflow: build a Boolean algebra, derive its theory, simplify
   expressions, and compare what Boolean algebras can simplify versus what
   Heyting algebras cannot (double negation elimination).

6. **[`examples/algebra/equivalence-discovery.scm`](../examples/algebra/equivalence-discovery.scm)** --
   `discover-equivalences` explores distinct normal forms across sub-theories.
   Shows how different axiom subsets produce different results, and what
   "equivalence depends on which laws you assume" means concretely.

## See Also

- `docs/ALGEBRA_REFERENCE.md` -- Complete API reference for all structures,
  projections, rewriting, and symbolic operations.
- `BIBLIOGRAPHY.md` -- Academic references (abstract algebra, lattice
  theory, term rewriting).
- `test/wile/algebra-*.scm` -- Test files covering each sub-library.
  These serve as additional usage examples beyond the guided examples above.
