# Algebra Library Reference

Complete API reference for `(wile algebra)` and its sub-libraries.

Import the umbrella library for everything:

```scheme
(import (wile algebra))
```

Or import individual sub-libraries for a subset (e.g., `(import (wile algebra setoid))`).

---

## Setoid -- `(wile algebra setoid)`

Sets with explicit equivalence relations.

### Constructors

- `(make-setoid equiv-fn)` -- create a setoid from an equivalence predicate (procedure of two arguments returning boolean)
- `(default-setoid)` -- setoid using `equal?`
- `(numeric-setoid)` -- setoid using `=` (numeric equality)
- `(string-setoid)` -- setoid using `string=?`
- `(eqv-setoid)` -- setoid using `eqv?`

### Predicates

- `(setoid? x)` -- test whether x is a setoid

### Operations

- `(setoid-equiv? S a b)` -- test whether a and b are equivalent under setoid S
- `(setoid-equivalence-class S x samples)` -- return elements of samples equivalent to x under S

### Validation

- `(validate-setoid S samples)` -- spot-check reflexivity, symmetry, and transitivity on samples; returns `#t` or a list of `(violation-type element ...)` entries

### Destructuring

- `(with-setoid S (equiv?) body ...)` -- bind `equiv?` to a two-argument procedure that tests equivalence under S

---

## Partial Order -- `(wile algebra order)`

Partial orders: reflexive, antisymmetric, transitive relations.

### Constructors

- `(make-partial-order leq?)` -- create a partial order from a binary leq? predicate

### Predicates

- `(partial-order? x)` -- test whether x is a partial order

### Operations

- `(po-leq? po a b)` -- test whether a <= b under partial order po
- `(po-comparable? po a b)` -- test whether a and b are comparable (either a <= b or b <= a)
- `(po-monotone? po f a b)` -- test whether function f preserves ordering of a and b under po; returns `#t` vacuously when a is not <= b

### Validation

- `(validate-partial-order po samples)` -- spot-check reflexivity and transitivity (not antisymmetry -- no equality predicate available); returns `#t` or a list of violations
- `(validate-partial-order/setoid po setoid samples)` -- extends `validate-partial-order` by also checking antisymmetry using setoid for equality

---

## Lattice -- `(wile algebra lattice)`

Bounded lattices with join, meet, and fixpoint computation.

### Constructors

- `(make-lattice join meet bottom top leq?)` -- create a lattice from five components: join and meet are binary procedures, bottom and top are elements, leq? is a binary predicate
- `(flat-lattice elements equal?)` -- flat lattice where all elements are incomparable between bottom and top; uses `'flat-bottom` and `'flat-top` as sentinel values
- `(powerset-lattice universe)` -- lattice of subsets of universe (lists); join is union, meet is intersection, leq? is subset
- `(product-lattice L1 L2 ...)` -- pointwise product of lattices; elements are lists of the same length
- `(map-lattice keys value-lattice)` -- lattice of alists mapping keys to elements of value-lattice; operations are pointwise

### Predicates

- `(lattice? x)` -- test whether x is a lattice

### Operations

- `(lattice-join L a b)` -- least upper bound of a and b
- `(lattice-meet L a b)` -- greatest lower bound of a and b
- `(lattice-bottom L)` -- the bottom element
- `(lattice-top L)` -- the top element
- `(lattice-leq? L a b)` -- test whether a <= b in the lattice

### Fixpoint

- `(fixpoint L f x)` -- Kleene iteration: compute least fixpoint of f starting from x; f must be monotone, L must have no infinite ascending chains
- `(fixpoint L f x fuel)` -- same but limited to fuel steps; returns `#f` if fixpoint not reached
- `(fixpoint/widen L f x widen)` -- fixpoint with widening operator for lattices with infinite ascending chains; widen takes (current, next) and must produce a finite ascending chain

### Projections

- `(lattice->partial-order L)` -- extract the partial order (leq?) from L

### Validation

- `(validate-lattice L samples)` -- spot-check commutativity, absorption, idempotence, and identity laws; returns `#t` or a list of violations

### Destructuring

- `(with-lattice L (join meet bottom top leq?) body ...)` -- bind five names to the lattice operations

---

## Closure Operator -- `(wile algebra closure)`

Extensive, monotone, idempotent functions on lattices.

### Constructors

- `(make-closure-operator close lattice)` -- create a closure operator from a unary close procedure and its underlying lattice
- `(downward-closure-operator po universe)` -- closure on the powerset lattice of universe; adds all elements below existing elements according to po

### Predicates

- `(closure-operator? x)` -- test whether x is a closure operator

### Operations

- `(closure-close C a)` -- apply closure: returns cl(a), always >= a
- `(closure-closed? C a)` -- test whether a is a fixed point: cl(a) = a
- `(closure-lattice C)` -- access the underlying lattice
- `(closed-elements C samples)` -- filter samples to those that are fixed points of C

### Projections

- `(closure->closed-lattice C samples)` -- construct the lattice of closed elements; join applies closure after lattice join, meet is inherited

### Validation

- `(validate-closure-operator C samples)` -- spot-check extensiveness, monotonicity, and idempotence; returns `#t` or a list of violations

### Destructuring

- `(with-closure C (close lattice) body ...)` -- bind close (unary procedure) and lattice

---

## Heyting Algebra -- `(wile algebra heyting)`

Bounded distributive lattices with relative pseudo-complement (implication).

### Constructors

- `(make-heyting-algebra join meet bottom top leq? implies)` -- create from six components; implies computes the largest c such that a /\ c <= b
- `(powerset-heyting universe)` -- Heyting algebra of subsets; implication is complement(a) union b
- `(map-heyting keys value-heyting)` -- pointwise Heyting algebra over alists; implication computed per-key

### Predicates

- `(heyting-algebra? x)` -- test whether x is a Heyting algebra

### Operations

- `(heyting-join H a b)` -- least upper bound
- `(heyting-meet H a b)` -- greatest lower bound
- `(heyting-bottom H)` -- bottom element
- `(heyting-top H)` -- top element
- `(heyting-leq? H a b)` -- test a <= b
- `(heyting-implies H a b)` -- relative pseudo-complement: largest c such that a /\ c <= b
- `(heyting-negate H a)` -- pseudo-complement: a -> bottom (largest c such that a /\ c = bottom)

### Projections

- `(heyting->lattice H)` -- extract the underlying lattice, forgetting implication

### Validation

- `(validate-heyting-algebra H samples)` -- spot-check lattice laws, modus ponens, and adjunction; returns `#t` or a list of violations

### Destructuring

- `(with-heyting H (join meet bottom top leq? implies) body ...)` -- bind six names

---

## Boolean Algebra -- `(wile algebra boolean)`

Complemented distributive lattices.

### Constructors

- `(make-boolean-algebra join meet bottom top leq? complement)` -- create from six components; complement satisfies a /\ ~a = bottom and a \/ ~a = top
- `(powerset-boolean universe)` -- Boolean algebra of subsets; complement is set difference from universe

### Predicates

- `(boolean-algebra? x)` -- test whether x is a Boolean algebra

### Operations

- `(boolean-join B a b)` -- least upper bound
- `(boolean-meet B a b)` -- greatest lower bound
- `(boolean-bottom B)` -- bottom element
- `(boolean-top B)` -- top element
- `(boolean-leq? B a b)` -- test a <= b
- `(boolean-complement B a)` -- complement: the unique element satisfying a /\ ~a = bottom and a \/ ~a = top

### Projections

- `(boolean->heyting B)` -- project to Heyting algebra; implication derived as ~a \/ b
- `(boolean->lattice B)` -- extract underlying lattice, forgetting complement
- `(boolean->ring B)` -- project to a ring of characteristic 2; plus is symmetric difference, times is meet

### Validation

- `(validate-boolean-algebra B samples)` -- spot-check lattice laws, complement laws, and distributivity; returns `#t` or a list of violations

### Destructuring

- `(with-boolean B (join meet bottom top leq? complement) body ...)` -- bind six names

---

## Monoid -- `(wile algebra monoid)`

Sets with an associative binary operation and identity element.

### Constructors

- `(make-monoid op identity)` -- create from a binary operation and identity element

### Predicates

- `(monoid? x)` -- test whether x is a monoid

### Operations

- `(monoid-op M a b)` -- apply the binary operation
- `(monoid-identity M)` -- access the identity element
- `(monoid-fold M lst)` -- left fold of lst starting from identity
- `(monoid-power M a n)` -- combine a with itself n times; returns identity when n <= 0

### Validation

- `(validate-monoid M samples)` -- spot-check left/right identity and associativity; returns `#t` or a list of violations

### Destructuring

- `(with-monoid M (op identity) body ...)` -- bind op (binary procedure) and identity (value)

---

## Category -- `(wile algebra category)`

Morphism composition with identity and associativity.

### Constructors

- `(make-category compose identity equiv?)` -- create from three procedures; compose takes (f, g) and returns f . g (mathematical convention: apply g first, then f); identity takes an object and returns its identity morphism; equiv? tests morphism equality

### Predicates

- `(category? x)` -- test whether x is a category

### Operations

- `(category-compose C f g)` -- compose morphisms: f . g (apply g first, then f)
- `(category-identity C obj)` -- identity morphism on obj
- `(category-equiv? C f g)` -- test whether morphisms f and g are equivalent

### Built-in Instances

- `(procedure-category)` -- category of Scheme procedures; composition is function composition, identity is the identity function, equivalence uses `equal?`

### Projections

- `(category->endomorphism-monoid C obj)` -- monoid of endomorphisms on obj (morphisms from obj to itself)

### Validation

- `(validate-category C morphism-triples identity-morphisms)` -- spot-check associativity and identity laws; morphism-triples is a list of (f g h) for associativity testing; identity-morphisms is a list of (obj f) pairs for identity law testing; returns `#t` or a list of violations

### Destructuring

- `(with-category C (compose identity equiv?) body ...)` -- bind three names

---

## Semiring -- `(wile algebra semiring)`

Two monoidal operations where times distributes over plus.

### Constructors

- `(make-semiring plus times zero one)` -- create from two binary operations and their identity elements

### Predicates

- `(semiring? x)` -- test whether x is a semiring

### Operations

- `(semiring-plus S a b)` -- additive operation
- `(semiring-times S a b)` -- multiplicative operation
- `(semiring-zero S)` -- additive identity (zero)
- `(semiring-one S)` -- multiplicative identity (one)

### Built-in Instances

- `(boolean-semiring)` -- plus is `or`, times is `and`, zero is `#f`, one is `#t`
- `(tropical-semiring)` -- plus is `min`, times is `+`, zero is `tropical-inf`, one is `0`; useful for shortest-path problems
- `(counting-semiring)` -- plus is `+`, times is `*`, zero is `0`, one is `1`

### Projections

- `(semiring->additive-monoid S)` -- extract the (plus, zero) monoid
- `(semiring->multiplicative-monoid S)` -- extract the (times, one) monoid

### Validation

- `(validate-semiring S samples)` -- spot-check identity, annihilation, commutativity, and distributivity; returns `#t` or a list of violations

### Destructuring

- `(with-semiring S (plus times zero one) body ...)` -- bind four names

---

## Group -- `(wile algebra group)`

Monoids with inverses.

### Constructors

- `(make-group op identity inverse)` -- create from a binary operation, identity element, and unary inverse function

### Predicates

- `(group? x)` -- test whether x is a group

### Operations

- `(group-op G a b)` -- apply the binary operation
- `(group-identity G)` -- access the identity element
- `(group-inverse G a)` -- compute the inverse of a

### Projections

- `(group->monoid G)` -- extract the underlying monoid, forgetting inverse

### Validation

- `(validate-group G samples)` -- spot-check identity, inverse, and associativity; returns `#t` or a list of violations

### Destructuring

- `(with-group G (op identity inverse) body ...)` -- bind three names

---

## Ring -- `(wile algebra ring)`

Semirings where addition forms an abelian group.

### Constructors

- `(make-ring plus times zero one negate)` -- create from two binary operations, their identities, and a unary negation

### Predicates

- `(ring? x)` -- test whether x is a ring

### Operations

- `(ring-plus R a b)` -- addition
- `(ring-times R a b)` -- multiplication
- `(ring-zero R)` -- additive identity
- `(ring-one R)` -- multiplicative identity
- `(ring-negate R a)` -- additive inverse
- `(ring-minus R a b)` -- subtraction: a + negate(b)

### Built-in Instances

- `(integer-ring)` -- standard integer arithmetic
- `(modular-ring n)` -- integers modulo n; elements are in [0, n)

### Projections

- `(ring->semiring R)` -- forget negation
- `(ring->additive-group R)` -- extract the (plus, zero, negate) group

### Validation

- `(validate-ring R samples)` -- spot-check identity, inverse, and distributivity; returns `#t` or a list of violations

### Destructuring

- `(with-ring R (plus times zero one negate) body ...)` -- bind five names

---

## Differential Ring -- `(wile algebra differential)`

Rings equipped with a derivation satisfying the Leibniz rule.

### Constructors

- `(make-differential-ring R deriv)` -- create from a ring R and a unary derivation procedure satisfying D(a+b) = D(a)+D(b) and D(a*b) = D(a)*b + a*D(b)

### Predicates

- `(differential-ring? x)` -- test whether x is a differential ring

### Operations

- `(differential-deriv D a)` -- apply the derivation to a
- `(differential-ring-ring D)` -- access the underlying ring
- `(differential-nth-deriv D n a)` -- apply derivation n times; D^0 is identity
- `(differential-constant? D a)` -- test whether D(a) equals zero of the underlying ring

### Built-in Instances

- `(dual-number-ring)` -- differential ring of dual numbers over integers; elements are pairs (a . b) representing a + b*epsilon where epsilon^2 = 0; derivation D(a,b) = (0,b)
- `(polynomial-derivation R)` -- differential ring of polynomials over ring R; elements are coefficient lists in ascending power order; derivation is formal derivative

### Projections

- `(differential-ring->ring D)` -- extract the underlying ring (same as `differential-ring-ring`)

### Validation

- `(validate-differential-ring D samples)` -- spot-check ring laws, additivity, and Leibniz rule; returns `#t` or a list of violations

### Destructuring

- `(with-differential D (plus times zero one negate deriv) body ...)` -- bind six names (five from the underlying ring plus deriv)

---

## Field -- `(wile algebra field)`

Rings where every nonzero element has a multiplicative inverse.

Note: Field types are defined in `(wile algebra ring)` and re-exported by the umbrella library.

### Constructors

- `(make-field plus times zero one negate reciprocal)` -- create from ring operations plus a unary reciprocal; reciprocal need not be defined for zero

### Predicates

- `(field? x)` -- test whether x is a field

### Operations

- `(field-plus F a b)` -- addition
- `(field-times F a b)` -- multiplication
- `(field-zero F)` -- additive identity
- `(field-one F)` -- multiplicative identity
- `(field-negate F a)` -- additive inverse
- `(field-reciprocal F a)` -- multiplicative inverse; a must be nonzero
- `(field-divide F a b)` -- division: a * reciprocal(b); b must be nonzero

### Built-in Instances

- `(rational-field)` -- exact rational arithmetic

### Projections

- `(field->ring F)` -- forget reciprocal

### Validation

- `(validate-field F samples)` -- spot-check ring laws plus multiplicative inverse for nonzero samples; returns `#t` or a list of violations

### Destructuring

- `(with-field F (plus times zero one negate reciprocal) body ...)` -- bind six names

---

## Galois Connection -- `(wile algebra galois)`

Adjunctions between a concrete partial order and an abstract lattice.

### Constructors

- `(make-galois-connection alpha gamma concrete-po abstract-lattice)` -- create from abstraction function alpha, concretization function gamma, a partial order on the concrete domain, and a lattice on the abstract domain

### Predicates

- `(galois-connection? x)` -- test whether x is a Galois connection

### Operations

- `(gc-alpha GC concrete-val)` -- abstract a concrete value
- `(gc-gamma GC abstract-val)` -- concretize an abstract value
- `(gc-concrete-po GC)` -- access the concrete partial order
- `(gc-abstract-lattice GC)` -- access the abstract lattice

### Validation

- `(gc-sound? GC concrete-samples abstract-samples)` -- spot-check extensiveness (c <= gamma(alpha(c))) and reductiveness (alpha(gamma(a)) <= a); returns `#t` or a list of violations

---

## Rewrite -- `(wile algebra rewrite)`

Axiom-driven term rewriting via abstract term protocols.

### Term Protocol

- `(make-term-protocol compound-term? get-operator get-operands make-term compare)` -- create a protocol; compound-term? tests if a value has substructure; get-operator and get-operands extract parts; make-term rebuilds a term with new operands preserving metadata; compare orders atoms for commutativity normalization
- `(term-protocol? x)` -- test whether x is a term protocol
- `(term-compound? proto x)` -- test whether x is a compound term
- `(term-get-operator proto term)` -- extract operator from compound term
- `(term-get-operands proto term)` -- extract operand list from compound term
- `(term-make-term proto term new-args)` -- rebuild term with new operands
- `(term-compare proto a b)` -- test whether a sorts before b

### Axiom Constructors

- `(make-identity-axiom op element)` -- identity law: op(x, e) = x; op is an operator symbol, element is a predicate (value -> boolean) identifying the identity element
- `(make-commutativity-axiom op)` -- commutativity: op(x, y) = op(y, x), normalized by term ordering
- `(make-absorbing-axiom op element)` -- absorbing element: op(x, z) = z; element is a predicate identifying the absorbing element
- `(make-idempotence-axiom op)` -- idempotence: op(x, x) = x
- `(make-involution-axiom op)` -- involution: op(op(x)) = x (unary operator)
- `(make-absorption-axiom op-outer op-inner)` -- absorption: op-outer(a, op-inner(a, b)) = a
- `(make-associativity-axiom op)` -- associativity: op(op(a, b), c) = op(a, op(b, c)); right-associates

### Axiom Predicates

- `(identity-axiom? x)` -- test for identity axiom
- `(commutativity-axiom? x)` -- test for commutativity axiom
- `(absorbing-axiom? x)` -- test for absorbing axiom
- `(idempotence-axiom? x)` -- test for idempotence axiom
- `(involution-axiom? x)` -- test for involution axiom
- `(absorption-axiom? x)` -- test for absorption axiom
- `(associativity-axiom? x)` -- test for associativity axiom
- `(directional-axiom? x)` -- test whether x is directional (currently only associativity)
- `(axiom? x)` -- test whether x is any recognized axiom type

### Rule Compilation

- `(axiom->rules axiom proto)` -- compile an axiom into a list of rewrite-rule procedures using term protocol proto; each rule is a procedure (term -> value-or-no-match)

### Normalizer

- `(make-normalizer theory proto)` -- compile a list of axioms into a single normalizer (term -> value-or-#f); tries each rule in order, returns first match or `#f`

### Sentinel

- `(no-match? x)` -- test whether x is the internal no-match sentinel; for use by rule dispatch internals

---

## Symbolic -- `(wile algebra symbolic)`

Named axioms, theories, theory combinators, recursive normalization, and transformation tracing.

### Named Axioms

- `(make-named-axiom name general-form axiom)` -- create a named axiom; name is a string for trace labeling, general-form is a string describing the law (e.g., "+(a, 0) = a"), axiom is a rewrite axiom record
- `(named-axiom? x)` -- test whether x is a named axiom
- `(named-axiom-name na)` -- access the string name
- `(named-axiom-general-form na)` -- access the general-form string
- `(named-axiom-axiom na)` -- access the underlying axiom record

### Theories

- `(make-theory axioms associative-ops)` -- create a theory; axioms is a list of named-axiom records, associative-ops is a list of operator symbols that are associative
- `(theory? x)` -- test whether x is a theory
- `(theory-axioms th)` -- access the list of named axioms
- `(theory-associative-ops th)` -- access the list of associative operator symbols

### Theory Combinators

- `(theory-filter theory names)` -- return a new theory with only axioms whose names appear in names (a list of strings)
- `(theory-exclude theory names)` -- return a new theory without axioms whose names appear in names
- `(theory-prioritize theory names)` -- return a new theory with named axioms moved to the front (controls rule application order)
- `(theory-merge theory1 theory2)` -- concatenate two theories' axiom and associative-ops lists; no deduplication

### Rewrite Steps

- `(make-rewrite-step rule-name general-form before after)` -- create a trace entry recording one rewrite step
- `(rewrite-step? x)` -- test whether x is a rewrite step
- `(step-rule-name step)` -- access the rule name string
- `(step-general-form step)` -- access the general-form string
- `(step-before step)` -- access the term before rewriting
- `(step-after step)` -- access the term after rewriting
- `(fuel-exhausted-step? step)` -- test whether step is a fuel-exhaustion marker (iteration limit reached)

### Term Protocol

- `(sexp-term-protocol compare)` -- term protocol for S-expression terms: compound terms are pairs `(op arg ...)`, atoms are leaves; compare orders atoms for commutativity normalization and must handle all atom types in terms

### Recursive Normalizer

- `(make-recursive-normalizer theory proto)` -- create a normalizer that recursively normalizes subterms to a fixed point; default fuel is 100; returns a procedure `(term -> (values result trace))` where trace is a list of rewrite-step records
- `(make-recursive-normalizer theory proto fuel)` -- same with explicit fuel limit

### Theory Projections

- `(monoid->theory M op-symbol)` -- 2 axioms: identity, associativity; identity predicate uses `equal?` against M's identity element
- `(group->theory G op-symbol inv-symbol)` -- 3 axioms: identity, associativity (from monoid), inverse involution
- `(semiring->theory S plus-sym times-sym)` -- 6 axioms: identity and associativity for both ops, additive commutativity, multiplicative absorbing element
- `(ring->theory R plus-sym times-sym neg-sym)` -- 7 axioms: 6 semiring axioms plus negation involution
- `(field->theory F plus-sym times-sym neg-sym recip-sym)` -- 8 axioms: 7 ring axioms plus reciprocal involution
- `(lattice->theory L join-sym meet-sym)` -- 10 axioms: identity, commutativity, idempotence, absorption, and associativity for both join and meet
- `(heyting->theory H join-sym meet-sym)` -- 10 axioms: same as lattice->theory via the underlying lattice; implication is not included
- `(boolean->theory B join-sym meet-sym comp-sym)` -- 11 axioms: 10 lattice axioms plus complement involution

### Equivalence Discovery

- `(discover-equivalences theory proto term)` -- find distinct normal forms by running term through the full theory and each non-directional single-axiom sub-theory; default fuel 100; returns a list of `(normal-form . trace)` pairs deduplicated by `equal?`
- `(discover-equivalences theory proto term fuel)` -- same with explicit fuel limit

### Formatting

- `(format-trace trace)` -- format a list of rewrite steps as human-readable strings; each string shows rule name, general form, before, and after; fuel-exhaustion steps are prefixed with `[fuel exhausted]`

---

## Cross-Reference: Sub-library to Import Path

| Section | Import Path |
|---------|-------------|
| Setoid | `(wile algebra setoid)` |
| Partial Order | `(wile algebra order)` |
| Lattice | `(wile algebra lattice)` |
| Closure Operator | `(wile algebra closure)` |
| Heyting Algebra | `(wile algebra heyting)` |
| Boolean Algebra | `(wile algebra boolean)` |
| Monoid | `(wile algebra monoid)` |
| Category | `(wile algebra category)` |
| Semiring | `(wile algebra semiring)` |
| Group | `(wile algebra group)` |
| Ring | `(wile algebra ring)` |
| Differential Ring | `(wile algebra differential)` |
| Field | `(wile algebra ring)` (field types are defined here) |
| Galois Connection | `(wile algebra galois)` |
| Rewrite | `(wile algebra rewrite)` |
| Symbolic | `(wile algebra symbolic)` |

## Symbols Not Re-exported by Umbrella

The following symbols are exported by their sub-libraries but **not** by `(wile algebra)`:

- `tropical-inf` from `(wile algebra semiring)` -- the infinity sentinel for the tropical semiring; import the sub-library directly to access it
