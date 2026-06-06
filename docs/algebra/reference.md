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
- `(make-lattice join meet bottom top leq? . opts)` -- with optional trailing metadata: `(cons 'setoid S)`, `(cons 'cardinality N)`, `(cons 'elements LIST)`
- `(flat-lattice elements equal?)` -- flat lattice where all elements are incomparable between bottom and top; uses `'flat-bottom` and `'flat-top` as sentinel values
- `(powerset-lattice universe)` -- lattice of subsets of universe (lists); join is union, meet is intersection, leq? is subset
- `(product-lattice L1 L2 ...)` -- pointwise product of lattices; elements are lists of the same length
- `(map-lattice keys value-lattice)` -- lattice of alists mapping keys to elements of value-lattice; operations are pointwise

### Presets

- `(chain-lattice n)` -- the n-element total order 0 < 1 < ... < n-1; distributive, modular
- `(two-point-lattice)` -- the truth-value lattice on `{#f, #t}`: bottom `#f`, top `#t`, join `or`, meet `and`, leq? implication; distributive. Distinct from `(boolean-lattice 1)`: its carrier is the two booleans themselves, not subset-valued elements -- the lattice used by reachability-style analyses
- `(boolean-lattice n)` -- 2^[n]: subsets of an n-element universe ordered by inclusion; distributive
- `(diamond-lattice n)` -- M_n: bottom, n incomparable atoms, top; modular, not distributive
- `(pentagon-lattice)` -- N_5: the standard witness of non-modularity; neither distributive nor modular
- `(free-distributive-lattice n)` -- the free bounded distributive lattice on n generators; cardinality is the Dedekind number D(n); raises for n >= 6

### Predicates

- `(lattice? x)` -- test whether x is a lattice
- `(finite-lattice? L)` -- true iff L carries an elements enumeration
- `(distributive? L)` -- exhaustive check: `a ∧ (b ∨ c) = (a ∧ b) ∨ (a ∧ c)` on all triples of elements
- `(modular? L)` -- exhaustive check of the modular law
- `(join-irreducible? L x)` / `(meet-irreducible? L x)` -- individual-element predicates

### Operations

- `(lattice-join L a b)` -- least upper bound of a and b
- `(lattice-meet L a b)` -- greatest lower bound of a and b
- `(lattice-bottom L)` -- the bottom element
- `(lattice-top L)` -- the top element
- `(lattice-leq? L a b)` -- test whether a <= b in the lattice
- `(lattice-equal? L a b)` -- lattice equality via setoid (metadata-carried)
- `(lattice-cardinality L)` -- carried cardinality, or `#f` when unset
- `(lattice-elements L)` -- carried element enumeration, or `#f`
- `(lattice-setoid L)` -- the carrier's setoid, or `#f`
- `(lattice-equiv? L)` -- the setoid's equivalence procedure, or `#f`

### Irreducibles

- `(join-irreducibles L)` -- the list of join-irreducible elements (not a join of strictly smaller elements)
- `(meet-irreducibles L)` -- symmetric, for meet

### Fixpoint

- `(fixpoint L f x)` -- Kleene iteration: compute least fixpoint of f starting from x; f must be monotone, L must have no infinite ascending chains
- `(fixpoint L f x fuel)` -- same but limited to fuel steps; returns `#f` if fixpoint not reached
- `(fixpoint/widen L f x widen)` -- fixpoint with widening operator for lattices with infinite ascending chains; widen takes (current, next) and must produce a finite ascending chain

### Birkhoff duality

- `(lattice->locally-finite-poset L)` -- project the lattice's leq? and element set to a `<locally-finite-poset>`
- `(birkhoff-representation L)` -- the locally-finite poset of join-irreducibles; requires finite distributive L; raises otherwise
- `(birkhoff-representation/unchecked L)` -- same without the distributivity gate; caller guarantees the precondition
- `(birkhoff-reconstruction P)` -- the lattice of downsets of P; `lattice-cardinality` matches the downset count; roundtrip `birkhoff-reconstruction ∘ birkhoff-representation` is an isomorphism when L is distributive

### Projections

- `(lattice->partial-order L)` -- extract the partial order (leq?) from L

### Validation

- `(validate-lattice L samples)` -- spot-check commutativity, absorption, idempotence, and identity laws; returns `#t` or a list of violations
- `(validate-distributive-lattice L samples)` / `(validate-distributive-lattice/setoid L samples)` -- sample-based distributivity check
- `(validate-modular-lattice L samples)` / `(validate-modular-lattice/setoid L samples)` -- sample-based modularity check

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
- `(semiring-carrier S)` -- the advisory carrier symbol attached at construction (e.g. `'big-int`, `'saturating`, `'modular`, `'log-float`, `'boolean`, `'tropical`), or `#f` when unset; lets consumer libraries dispatch to fast paths without inspecting the operations
- `(semiring-eq? S)` -- the carrier's equivalence procedure used for convergence tests (worklist fixpoint), or `equal?` when unset

### Built-in Instances

- `(boolean-semiring)` -- plus is `or`, times is `and`, zero is `#f`, one is `#t`; carrier `'boolean`, idempotent plus (cycle-safe)
- `(tropical-semiring)` -- plus is `min`, times is `+`, zero is `tropical-inf`, one is `0`; carrier `'tropical`, idempotent plus (cycle-safe); useful for shortest-path problems
- `(counting-semiring)` -- plus is `+`, times is `*`, zero is `0`, one is `1`; exact-integer arithmetic, auto-promotes to bignum

#### Counting variants

The counting semiring's exact arithmetic is intractable on cyclic graphs (the path set is infinite, so the sum diverges) and expensive on deep walks. These variants trade exactness for tractability; all four are *true semirings* (every semiring axiom holds). They declare a carrier symbol so `(wile algebra graph)` can dispatch on it. See `memory/2026-05-24-approximate-counting-semirings.md`.

- `(bigint-counting-semiring)` -- same arithmetic as `counting-semiring`, but carrier `'big-int` opts into the `count-paths-in-dag` Go kernel when consumed by `make-graph-analysis` under three conditions (carrier `'big-int`, unit weights, atomic node ids); advisory -- never changes results, only dispatch cost
- `(modular-counting-semiring P)` -- carrier is Z/PZ; plus and times are arithmetic mod `P` (an exact integer >= 2); carrier `'modular`. For fingerprints, parity, and Schwartz-Zippel identity testing -- NOT approximate counts (a true count divisible by `P` reads as 0). Not cycle-safe: Z/PZ has no absorbing top, so worklist iteration on cycles hits the safety cap. `mersenne-31` (`2^31 - 1`) and `mersenne-61` (`2^61 - 1`) are provided as named modulus choices
- `(log-counting-semiring)` -- carrier float64 in log-space; plus is log-sum-exp, times is `+`, zero `-inf.0`, one `0.0`; carrier `'log-float`. Preserves orders of magnitude past 2^53, loses exact counts. For magnitude-ranking on DAGs (Viterbi-like queries). Not cycle-safe (no absorbing element)
- `(saturating-counting-semiring cap)` -- carrier `[0, CAP]`; plus is `min(a+b, CAP)`, times is `min(a*b, CAP)`; `CAP` a positive exact integer; carrier `'saturating`. CAP is an absorbing top, so this is **the only counting variant that converges under worklist iteration on cyclic graphs**. Values at CAP mean ">= CAP," not an exact count. Suggested default cap `2^53`

#### Carrier introspection

- `(bounded-carrier-semiring? S)` -- `#t` iff `S`'s carrier saturates (currently only `saturating-counting-semiring`); a semantic warning that results past the saturation point are uninformative, not an algebraic defect
- `(semiring-cycle-safe? S)` -- `#t` iff worklist iteration over `S` is guaranteed to converge on cyclic adjacencies; closed-set lookup on the carrier symbol, true for `'saturating` (absorbing top), `'boolean`, and `'tropical` (idempotent plus), `#f` otherwise including unannotated semirings

### Projections

- `(semiring->additive-monoid S)` -- extract the (plus, zero) monoid
- `(semiring->multiplicative-monoid S)` -- extract the (times, one) monoid

### Validation

- `(validate-semiring S samples)` -- spot-check identity, annihilation, commutativity, and distributivity; returns `#t` or a list of violations

### Destructuring

- `(with-semiring S (plus times zero one) body ...)` -- bind four names

---

## Group -- `(wile algebra group)`

Monoids with inverses. The record type carries optional introspection metadata (setoid, order, elements, generators) so clients can distinguish finite from finitely-generated groups and enumerate elements when available.

### Constructors

- `(make-group op identity inverse)` -- create from a binary operation, identity element, and unary inverse function
- `(make-group op identity inverse . opts)` -- with optional trailing metadata: `(cons 'setoid S)`, `(cons 'element? PRED)`, `(cons 'order N)`, `(cons 'elements LIST)`, `(cons 'generators LIST)`

### Presets

- `(trivial-group)` -- the one-element group with element `'e`; cached (eq?-identical across calls)
- `(cyclic-group n)` -- Z/nZ under addition mod n; elements `0..n-1`; generators `(1)`
- `(symmetric-group n)` -- S_n under permutation composition; elements are length-n permutation vectors
- `(product-group . groups)` -- direct product; element lists of length |groups|
- `(subgroup-generated G generators)` / `(subgroup-generated G generators . opts)` -- smallest subgroup containing generators (BFS closure); `(cons 'max-size N)` caps the search
- `(enumerate-finite-group G)` / `(enumerate-finite-group G . opts)` -- promote a finitely-generated group to a finite one by enumerating elements via BFS closure; idempotent when G already has elements

### Group actions

- `(make-group-action G set-element? act)` -- build an action record; `set-element?` tests membership in the set being acted on; `act` is `(lambda (g x) g·x)`
- `(trivial-action G set-element?)` / `(permutation-action Sn n)` / `(regular-action G)` / `(conjugation-action G)` / `(product-action . actions)` -- preset actions
- `(group-action? x)` / `(group-action-group A)` / `(group-action-act A g x)` / `(group-action-act-fn A)` / `(group-action-set-element? A)` -- accessors

### Orbit, stabilizer, Burnside

- `(orbit action x)` -- list of points reachable from x (BFS via generators, or enumerate-all if generators absent)
- `(stabilizer action x)` -- list of group elements fixing x; requires G to carry `elements`
- `(fixed-points action g X-elements)` -- points in `X-elements` fixed by g
- `(orbit-representative action x less?)` -- LESS?-minimum element of orbit(x); ties broken by discovery order
- `(burnside-count action X-elements)` -- `|X/G| = (1/|G|) Σ_{g} |X^g|`; requires finite G; raises if sum not divisible by |G|

### Predicates

- `(group? x)` -- test whether x is a group
- `(finite-group? G)` -- true when G carries both order and elements
- `(finitely-generated-group? G)` -- true when G carries generators
- `(subgroup? H G)` -- H is a sub-structure of G (closed under op, contains identity, closed under inverse)
- `(group-element? G)` -- the carried element-membership predicate, or `#f`

### Operations

- `(group-op G a b)` -- apply the binary operation
- `(group-identity G)` -- access the identity element
- `(group-inverse G a)` -- compute the inverse of a
- `(group-order G)` / `(group-elements G)` / `(group-generators G)` / `(group-setoid G)` / `(group-equal? G a b)` -- metadata accessors (return `#f` when unset)

### Projections

- `(group->monoid G)` -- extract the underlying monoid, forgetting inverse

### Validation

- `(validate-group G samples)` -- spot-check identity, inverse, and associativity; returns `#t` or a list of violations
- `(assert-group ...)` -- retained for backward compatibility; prefer `assert-validation` on `validate-group`

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

## Field -- `(wile algebra ring)`

Rings where every nonzero element has a multiplicative inverse.

Field types are defined alongside ring types in `(wile algebra ring)` and re-exported by the umbrella library.

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

- `(make-normalizer axioms proto)` -- compile a list of axioms into a single normalizer (term -> value-or-#f); tries each rule in order, returns first match or `#f`

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

### Boolean normalization facade

- `(symbolic-boolean-normalize term)` -- apply the standard Boolean theory (`boolean->theory` on a 1-atom Boolean algebra, operators `and` / `or` / `not`); returns `(values result trace)`; covers commutativity, associativity, identity, idempotence, absorption, and complement-involution but not De Morgan or complement laws
- `(symbolic-boolean-equivalent? t1 t2)` -- true iff both terms normalize to the same form under the standard Boolean theory

---

## Matrix -- `(wile algebra matrix)`

Semiring-parameterized dense and sparse matrices. Arithmetic follows the coefficient semiring; the same matrix type covers Boolean (reachability), tropical (shortest path), counting (ordinary arithmetic), and user-defined semirings. See `plans/2026-04-21-matrix-path-d-impl.md` for the polymorphism design.

### Constructors

- `(make-semiring-matrix S rows cols)` -- rows x cols matrix over semiring S filled with `(semiring-zero S)`
- `(make-semiring-matrix S rows cols fill)` -- same with explicit fill value
- `(semiring-matrix-from-rows S rows-list)` -- construct from a list of equal-length rows; non-empty list required
- `(semiring-matrix-identity S n)` -- n x n identity matrix (diagonal = `semiring-one`; off-diagonal = `semiring-zero`)
- `(make-sparse-semiring-matrix S rows cols entries)` -- sparse matrix from an entries iterator
- `(semiring-matrix->sparse M)` -- convert dense to sparse representation
- `(sparse->semiring-matrix M)` -- convert sparse to dense representation

### Predicates

- `(semiring-matrix? x)` -- dense matrix predicate
- `(sparse-semiring-matrix? x)` -- sparse matrix predicate
- `(matrix? x)` -- polymorphic predicate (dense or sparse)
- `(matrix-rep-tag M)` -- representation tag symbol (`'dense` or `'sparse`; extensible)

### Accessors

- `(semiring-matrix-ref M r c)` / `(sparse-semiring-matrix-ref M r c)` / `(matrix-ref M r c)` -- element at (r, c)
- `(semiring-matrix-rows M)` / `(semiring-matrix-cols M)` -- dimensions
- `(matrix-rows M)` / `(matrix-cols M)` / `(matrix-shape M)` -- polymorphic dimensions
- `(semiring-matrix-semiring M)` / `(matrix-semiring M)` -- the coefficient semiring
- `(semiring-matrix->rows M)` -- convert to list-of-lists form
- `(matrix-for-each-entry M proc)` / `(matrix-fold-entries M seed proc)` -- iterator API

### Arithmetic

- `(semiring-matrix-add A B)` / `(matrix-add A B)` / `(matrix-add! A B)` -- addition; `!` form is in-place
- `(semiring-matrix-mul A B)` -- multiplication; inner dimensions must match
- `(semiring-matrix-power M n)` -- repeated multiplication; `M^0 = I`, `M^n = M * M^(n-1)`
- `(semiring-matrix-closure M)` -- Kleene-star closure (I + M + M^2 + ...) for Kleene-algebra semirings
- `(semiring-matrix-permanent M)` -- permanent (over a commutative semiring with subtraction not required)

### Destructuring

- `(with-semiring-matrix (add mul power closure ref) body ...)` -- positionally bind the five operations to local names; order is fixed (`add`, `mul`, `power`, `closure`, `ref`), not by name

---

## Polynomial -- `(wile algebra polynomial)`

Univariate polynomials over a coefficient ring. Ascending-order coefficient lists, normalized (no trailing zero). Arithmetic threads through the coefficient ring's ops, so the same polynomial type runs over Z, Q, Z/nZ, or any user-defined ring.

### Constructors

- `(make-poly R coeffs)` -- polynomial over ring R from coefficient list in ascending power order; trailing zeros stripped; empty list is the zero polynomial
- `(poly-zero R)` / `(poly-one R)` -- additive and multiplicative identities
- `(polynomial-ring R)` -- the ring of polynomials over R (elements are `<polynomial>` records)
- `(integer-polynomials)` / `(rational-polynomials)` -- convenience presets for `Z[x]` and `Q[x]`

### Predicates and accessors

- `(polynomial? x)` -- test whether x is a polynomial record
- `(poly-ring p)` -- coefficient ring
- `(poly-coeffs p)` -- ascending-order coefficient list
- `(poly-degree p)` -- degree (-1 for the zero polynomial, PARI/GP convention)
- `(poly-leading-coeff p)` -- leading (highest-power) coefficient; ring-zero for the zero polynomial

### Arithmetic

- `(poly-plus p q)` -- sum; both polynomials must share the ring
- `(poly-negate p)` -- additive inverse (negate each coefficient)
- `(poly-minus p q)` -- difference: `poly-plus p (poly-negate q)`
- `(poly-times p q)` -- product via schoolbook multiplication, O(n*m) coefficient operations
- `(poly-eval p x)` -- Horner evaluation at `x` using the coefficient ring's plus and times

### Calculus and division

- `(poly-derivative p)` -- formal derivative: shifted, coefficient-scaled
- `(poly-divmod p d F)` -- polynomial long division returning `(values q r)` such that `p = q*d + r` and `deg(r) < deg(d)`; requires F to be a field (for leading-coefficient inversion)
- `(poly-gcd p q F)` -- Euclidean GCD; returns a monic polynomial; requires F to be a field

### Destructuring

- `(with-polynomial R (plus times zero one negate) body ...)` -- bind ring-level polynomial operators

---

## Incidence Algebra -- `(wile algebra incidence)`

Locally-finite posets and their incidence algebras over a ring. Supports zeta and Möbius functions, convolution, and Möbius inversion (inclusion-exclusion). Classical setting for combinatorial identities on divisibility, subset, and subword posets.

### Constructors

- `(make-locally-finite-poset leq? interval)` -- construct a locally-finite poset from a `leq?` predicate and an `interval` procedure; `interval x y` returns the list of elements z with x ≤ z ≤ y
- `(make-locally-finite-poset leq? interval (cons 'elements LIST))` -- with optional enumeration (required by `birkhoff-reconstruction`)
- `(finite-set->locally-finite-poset leq? elements)` -- build a bounded poset from a predicate and an explicit element list; scans the list to derive intervals
- `(make-incidence-algebra poset)` -- incidence algebra over the integer ring by default
- `(make-incidence-algebra poset ring)` -- incidence algebra over an explicit ring

### Predicates and accessors

- `(locally-finite-poset? x)` / `(incidence-algebra? x)` -- predicates
- `(lf-poset-leq? P)` -- the leq? procedure
- `(lf-poset-interval P)` -- the interval-enumeration procedure
- `(lf-poset-elements P)` -- optional element enumeration (or `#f`)
- `(incidence-algebra-poset IA)` / `(incidence-algebra-ring IA)` -- underlying poset and ring

### Zeta, Möbius, convolution

- `(zeta-function IA)` -- returns a procedure `(zeta x y)` that is `ring-one` when x ≤ y and `ring-zero` otherwise
- `(mobius-function IA)` -- returns a procedure `(mu x y)` computing the Möbius function μ(x, y), memoized in the incidence algebra
- `(incidence-convolve IA f g)` -- convolution of two functions `f, g : P x P -> R`
- `(mobius-inversion IA g x lower-set)` -- apply Möbius inversion to recover f from g = Σ_{y ≤ x} f(y) on a given lower-set

---

## Interval -- `(wile algebra interval)`

Infinity-aware interval arithmetic. Intervals are `(lo . hi)` pairs where `lo` and `hi` may be numbers or the sentinels `'neg-inf` / `'pos-inf`. The interval lattice orders by containment. Useful as a concrete abstract domain for static analysis or as a conservative envelope for uncertain quantities.

### Lattice and representation

- `(interval-lattice)` -- the lattice over intervals represented as `(lo . hi)` pairs; bottom is the empty interval, top is `(neg-inf . pos-inf)`; order is reverse-inclusion (`(a . b) <= (c . d)` iff `c <= a` and `b <= d` under `inf<=`)

Intervals are ordinary pairs: build with `cons lo hi`; access with `car` and `cdr`. Use the infinity-aware comparisons below for predicates over the endpoints.

### Infinity-aware comparisons and arithmetic

- `(inf<= a b)` -- infinity-aware `<=`: `neg-inf <= anything`, `anything <= pos-inf`
- `(inf-min a b)` / `(inf-max a b)` -- infinity-aware min / max
- `(inf+ a b)` -- infinity-aware addition; `pos-inf + neg-inf = pos-inf` (conservative widening)
- `(inf- a b)` -- infinity-aware subtraction
- `(inf* a b)` -- infinity-aware multiplication

### Interval arithmetic

- `(interval-add a b)` -- sum; componentwise `inf+`. Returns `interval-bot` if either operand is `interval-bot` (absorbing)
- `(interval-sub a b)` -- difference. `interval-bot`-absorbing
- `(interval-mul a b)` -- product via four-corner multiplication (min and max of the four endpoint products). `interval-bot`-absorbing

### Abstraction, widening, and the Galois connection

- `(abstract-interval n)` -- abstract an integer `n` to the point interval `(n . n)`; the interval analog of `abstract-sign`
- `(interval-widen cur next)` -- widening operator (Cousot & Cousot 1977): keep a bound if stable, else jump to infinity (lower → `neg-inf`, upper → `pos-inf`); `interval-bot` is absorbed in either position. Forces ascending chains finite so fixpoint iteration over the infinite-height interval lattice terminates. Pass to `run-analysis` via `(widen interval-widen)`, or to `fixpoint/widen` directly
- `(interval-galois-connection)` -- the Galois connection between finite integer sets (containment order) and the interval lattice: `alpha(S) = [min S, max S]` (`interval-bot` for `()`), `gamma([a,b]) = {x : a <= x <= b}` (sentinel `'unbounded` for unbounded intervals). The soundness certificate for an interval dataflow result; passes `gc-sound?`

---

## Graph (abstract) -- `(wile algebra graph)`

Semiring-parameterized single-source graph analysis. Lazy Bellman-Ford-style traversal on adjacency alists with the semiring determining what "distance" means. Distinct from `(wile algebra combinatorial-graph)`, which handles isomorphism and enumeration.

### Constructors

- `(make-graph-analysis semiring adjacency weight-fn)` -- build a graph-analysis from a semiring, an adjacency alist, and a weight function; `#f` weight-fn means unit weights (each edge contributes `semiring-one`)

### Predicates and accessors

- `(graph-analysis? x)` -- predicate

### Queries

- `(graph-query ga source target)` -- return the semiring-value between source and target; `semiring-zero` when unreachable; lazily computes and caches single-source distances per source
- `(graph-query-all ga source)` -- return an alist `((node . value) ...)` for all reachable nodes

### Semiring choice on cyclic graphs

Counting on cyclic graphs has no finite answer in the strict counting semiring `(ℕ, +, ×, 0, 1)` — the path set is infinite, so `Σ` diverges. The library's response depends on the semiring carrier:

- **Boolean reachability, tropical shortest path:** idempotent operations. Worklist Bellman-Ford converges in finite time. Cycles handled correctly.
- **Counting semiring on a DAG:** topological-order single-pass propagation. Each edge relaxed exactly once. Counts exact (bignum-promoted as needed).
- **Counting semiring on a cyclic graph:** worklist Bellman-Ford does not terminate (over-counts on each re-pop). Use one of:
  - **SCC condensation** via `(import (wile algebragraph))` and `count-paths-cyclic` (see below). Gives exact counts per SCC, with "entry count" semantics on non-trivial SCCs (the path-count to the SCC, not within it).
  - **Approximate-counting semirings** (`saturating-counting-semiring`, `modular-counting-semiring`, `log-counting-semiring`) — bounded carriers that converge even on cycles. See `memory/2026-05-24-approximate-counting-semirings.md`.

## SCC Condensation Fast Path -- `(wile algebragraph)`

Auto-generated extension library exposing the Go-side graph kernels in `algebra/graph/`. Available under the `KitchenSink` profile or via explicit `WithExtension(algebragraph.Extension)`. Designed for callers who want exact path counts on arbitrary directed graphs, including cyclic ones.

Inputs use integer-indexed nodes; the caller is responsible for mapping symbolic names to indices.

### Primitives

- `(count-paths-in-dag num-nodes edges source)` — count paths in a DAG. `edges` is a list of `(u . v)` pairs of node indices. Returns a vector of exact-integer counts (length `num-nodes`) where `v[i]` is the number of distinct paths from `source` to node `i`. Returns `#f` if the input contains a cycle reachable from `source`. Internal monotone-add kernel; O(V + E).

- `(count-paths-cyclic num-nodes edges source)` — count paths via SCC condensation. Handles arbitrary directed graphs. Returns three values:
  1. SCC vector (length `num-nodes`) mapping each node to its strongly-connected component ID (low IDs are sources in the condensation; high IDs are sinks).
  2. Counts vector (length `num-sccs`) of distinct paths from the source's SCC to each SCC in the condensed DAG.
  3. NonTrivial-flag vector (length `num-sccs`) marking SCCs that contain a cycle.

  **Semantics on non-trivial SCCs:** within-SCC path counts are infinite (cycle). For nodes in non-trivial SCCs, the per-SCC count is the *entry count* — the number of distinct condensed-DAG paths from the source's SCC that reach this SCC via some entry point. Callers should propagate the NonTrivial flag so users understand the semantic shift.

### Scale

On a graph the size of the wile/machine package call graph (539 nodes, 623 edges, 12 back-edges — the workload that triggered the 3-hour incident documented in `memory/feedback-counting-semiring-on-cycles.md`), `count-paths-cyclic` completes in ~36 µs (Apple M4 Max). Empirically O(V + E) — the SCC pass, condensation pass, and monotone-add pass each touch every node and edge once.

---

## Combinatorial Graph -- `(wile algebra combinatorial-graph)`

Graphs as combinatorial objects. Isomorphism via 1-WL + individualization-refinement backtracking (McKay-Piperno 2014), deletion-contraction for chromatic/Tutte polynomials and spanning-tree count, Hopcroft-Karp bipartite matching. Distinct from `(wile algebra graph)`.

### Constructors

- `(make-graph adjacency)` -- adjacency given as `((v (w edge-data) ...) ...)`; see `plans/2026-04-22-combinatorial-graph-impl.md` for the record shape

### Presets

- `(complete-graph n)` -- K_n
- `(cycle-graph n)` -- C_n
- `(path-graph n)` -- P_n
- `(complete-bipartite-graph m n)` -- K_{m,n}
- `(empty-graph n)` -- n isolated vertices
- `(petersen-graph)` -- Petersen graph (3-regular, 10 vertices, 15 edges)

### Predicates and accessors

- `(graph? x)` / `(finite-graph? x)` / `(finitely-generated-graph? x)` -- tier predicates
- `(graph-order G)` -- vertex count
- `(graph-size G)` -- edge count
- `(graph-vertices G)` / `(graph-edges G)` / `(graph-neighbors G v)` / `(graph-degree G v)` -- structure accessors
- `(graph-edge? G u v)` / `(graph-has-vertex? G v)` -- membership predicates
- `(graph-vertex-equiv? G)` / `(graph-setoid G)` -- vertex-equality access
- `(graph-directed? G)` / `(graph-multi? G)` / `(graph-self-loops? G)` -- shape predicates

### Traversal and connectivity

- `(graph-bfs G source)` -- breadth-first order from source; raises if source is not a vertex
- `(graph-dfs G source)` -- depth-first preorder
- `(graph-connected-components G)` -- list of components
- `(graph-bipartite? G)` / `(graph-bipartition G)` -- bipartite test and 2-coloring

### Isomorphism

- `(graph-isomorphic? G H)` -- true iff G and H are isomorphic
- `(graph-canonical-form G)` -- canonical representation

### Invariants

- `(graph-spanning-tree-count G)` -- non-negative integer; closed forms for K_n, C_n, trees; deletion-contraction otherwise
- `(graph-chromatic-polynomial G)` -- coefficients in ascending order; closed forms for K_n, C_n, trees, empty graph
- `(graph-tutte-polynomial G)` -- list of rows, each a y-coefficient list for one x power

### Matching

- `(graph-maximum-bipartite-matching G)` -- Hopcroft-Karp matching as an alist of pairs; raises if not bipartite

### Validation

- `(validate-graph G)` / `(assert-graph G)` / `(with-graph G ...)` -- standard validation pattern

---

## Unification -- `(wile algebra unification)`

Syntactic and AC-modulo unification. Pattern variables as records, substitutions as alists. AC unification via Stickel's Diophantine reduction for pure-variable cases. The `diophantine-basis` primitive is exposed for Petri-net and integer-programming consumers.

### Pattern variables

- `(make-pattern-var name)` -- pattern-var record; identity is name-based
- `(pattern-var? x)` -- predicate
- `(pattern-var-name v)` -- name accessor
- `(parse-pattern expr)` -- walk expr, replacing each `?name` symbol with a pattern-var; repeated names intern to one record

### Substitutions

- `(make-substitution bindings)` -- substitution from an alist of `(var . term)` bindings
- `empty-substitution` -- the empty substitution (a constant, not a procedure)
- `(substitution? x)` / `(substitution-bindings sub)` -- predicate and accessor
- `(substitution-lookup sub var)` -- returns the bound term, or `#f`
- `(substitution-compose s1 s2)` -- sequential composition (apply s2, then s1)
- `(substitution-apply sub proto term)` -- walk term, replacing pattern-vars per sub

### Diophantine basis

- `(diophantine-basis a b)` -- minimal non-negative integer solutions of `a·u = b·v`; returns list of `(u . v)` pairs; finite by Dickson's lemma

### Matching and unification

- `(ac-match pattern subject theory proto)` -- match pattern against subject modulo AC operators in theory; returns list of substitutions (empty = no match)
- `(ac-unify t1 t2 theory proto)` -- two-sided AC unification; returns a CSU (complete set of unifiers) as a list of substitutions

### Internal helpers (exposed for tests)

- `(flatten-ac term op proto)` -- flatten nested applications of op into a list of operands

---

## Formal Concept Analysis -- `(wile algebra fca)`

Concept lattices via NextClosure (Ganter 1984). Extracts the maximal extent/intent pairs from a binary object-attribute relation. The resulting lattice has a natural algebra-level projection.

### Constructors

- `(make-context objects attributes incidence)` -- context from object list, attribute list, and an incidence predicate `(lambda (obj attr) boolean)`
- `(context-from-alist entries)` -- convenience: each entry is `(object attr ...)`

### Predicates and accessors

- `(fca-context? x)` -- predicate
- `(context-objects ctx)` / `(context-attributes ctx)` -- sorted string lists

### Galois derivation

- `(intent ctx object-set)` -- attributes common to every object in the set; empty set yields all attributes (vacuous)
- `(extent ctx attribute-set)` -- objects possessing every attribute in the set; empty set yields all objects

### Concepts and lattices

- `(concept-lattice ctx)` -- list of `(extent . intent)` concepts via NextClosure
- `(concept-extent c)` / `(concept-intent c)` -- accessors on a concept pair
- `(concept-lattice->algebra-lattice ctx concepts)` -- project the concept list to a `<lattice>`
- `(concept-relationship c1 c2)` -- ordering between two concepts

### Sorted-string primitives

- `(set-add elem sorted)` / `(set-intersect a b)` / `(set-union a b)` / `(set-member? elem sorted)` / `(set-subset? a b)` / `(set-before elem sorted)` / `(sort-strings lst)` -- canonical sorted-string-set operations

---

## Pareto -- `(wile algebra pareto)`

Multi-objective Pareto dominance and frontier computation. Mixed factor types (boolean, numeric) via `factor-leq?` / `factor-less?`. Use it when you have candidates scored on multiple axes and want the non-dominated set.

### Factor comparison

- `(factor-leq? a b)` -- non-strict factor comparison: booleans `#f ≤ #t`, numbers `<=`
- `(factor-less? a b)` -- strict version

### Dominance and frontier

- `(dominates? factors-x factors-y)` -- test Pareto dominance: X dominates Y iff X is factor-leq to Y on every key and factor-less on at least one
- `(pareto-frontier candidates factor-names)` -- candidates is `((id alist) ...)`; factor-names is documentation only; returns `((frontier id ...) (dominated (dominator . dominated-ids) ...))`

---

## Abstract Domain -- `(wile algebra abstract-domain)`

Pre-built abstract interpretation domains. Currently hosts the sign domain: a 5-element flat lattice with an abstraction function from integers and a sign arithmetic table for add/sub/mul.

### Sign lattice

- `(sign-lattice)` -- 5-element flat lattice `{flat-bottom, neg, zero, pos, flat-top}` with the three atoms incomparable
- `(abstract-sign n)` -- abstract a concrete integer to its sign: `'neg`, `'zero`, or `'pos`
- `(sign? s)` -- test whether s is a valid sign value
- `(sign-binop op a b)` -- apply sign operator; `op` is `'add`, `'sub`, or `'mul`; `flat-bottom` is strict (any operand bottom yields bottom); `mul` annihilates at zero even with `flat-top`; otherwise `flat-top` propagates
- `(sign-galois-connection)` -- the Galois connection between finite integer sets (containment order) and the sign lattice: `alpha(S)` is the join of the per-element signs; `gamma` maps `'zero` → `{0}`, `'neg`/`'pos`/`'flat-top` → the typed sentinels `'all-neg`/`'all-pos`/`'all-int`, `'flat-bottom` → `{}`. Passes `gc-sound?` on all five signs

---

## Dataflow -- `(wile algebra dataflow)`

Monotone framework (MFP) worklist dataflow solver with CFG-protocol abstraction. Lattice-parameterized forward/backward fixpoint analysis; the protocol separates the algorithm from the CFG representation.

### CFG protocol

- `(make-cfg-protocol blocks-of index-of preds-of succs-of)` -- build a CFG protocol from four closures
- `(cfg-protocol? x)` -- predicate
- `(cfg-blocks-of proto fn)` / `(cfg-index-of proto block)` / `(cfg-preds-of proto block)` / `(cfg-succs-of proto block)` -- wrapper accessors; prefer these over the raw `cfg-protocol-*-fn` accessors

### Initial-state wrapper

- `(init-state value)` -- tagged record for passing an initial lattice value into `run-analysis` (prevents ambiguity with flag symbols)
- `(init-state? x)` / `(init-state-value is)` -- predicate and accessor

### Block ordering

- `(reverse-postorder blocks protocol)` -- DFS-prepend-on-finish produces RPO directly; only blocks reachable from the first block appear

### Solver

- `(run-analysis direction lattice transfer fn protocol . args)` -- run MFP analysis; `direction` is `'forward` or `'backward`; `transfer` is `(lambda (block in-state) out-state)`; `args` may contain `(init-state x)`, `'check-monotone`, and/or `(widen OP)`; returns per-block result alist `((idx in out) ...)`
- `(widen op)` -- tagged wrapper for the optional widening operator passed to `run-analysis`. `op` is `(lambda (cur next) widened)`, applied at loop headers (back-edge targets) in place of raw join. Required for termination on infinite-height lattices (e.g. the interval lattice via `interval-widen`); absent ⇒ pure MFP (raw join everywhere, behavior unchanged). Accessors: `widen?`, `widen-op`

### Result accessors

- `(analysis-in result block-idx)` -- in-state for block, or `#f` if absent
- `(analysis-out result block-idx)` -- out-state for block, or `#f` if absent
- `(analysis-states result)` -- full per-block result alist

---

## Matching -- `(wile algebra matching)`

Two-sided matching primitives -- Roth-Sotomayor (1990). Three-layer structure per directions doc §4.6: local optimization (Gale-Shapley, Hungarian) + stability constraint + global selection (Conway distributive lattice via Birkhoff §5.5).

### Preference profiles

- `(make-preference-profile <agents> <ranks-of> [opts ...])` -- construct a preference profile; `<ranks-of>` is `agent → ordered list of preferred candidates`; opts: `(setoid . S)`
- `(preference-profile? <x>)` / `(preference-profile-agents <P>)` / `(preference-profile-ranks-of <P>)` / `(preference-profile-setoid <P>)` -- predicate and accessors
- `(preference-profile-rank-of <P> <agent> <candidate>)` -- 1-based rank, or `#f` if absent
- `(preference-profile-prefers-strictly? <P> <agent> <x> <y>)` -- strict preference predicate
- `(validate-preference-profile <P> <candidate-set>)` -- catches out-of-set candidates and tied preferences; returns `#t` or violation list
- `(with-preference-profile <P> (agents ranks-of) <body>...)` -- field binder

### Bipartite matchings

- `(make-bipartite-matching <pairs> [opts ...])` -- construct from alist of `(proposer . receiver)` pairs; opts: `(prop-setoid . S)`, `(recv-setoid . S)`
- `(bipartite-matching? <x>)` / `(bipartite-matching-pairs <M>)` / `(bipartite-matching-prop-setoid <M>)` / `(bipartite-matching-recv-setoid <M>)` -- predicate and accessors
- `(bipartite-matching-partner <M> <agent>)` -- symmetric partner lookup, or `#f` if unmatched
- `(bipartite-matching-unmatched <M> <side> <agents>)` -- agents from `<agents>` not appearing on the given side (`'proposer` or `'receiver`)
- `(bipartite-matching-equal? <M1> <M2>)` -- order-insensitive equality
- `(validate-bipartite-matching <M> <proposers> <receivers>)` -- catches duplicates and out-of-set agents
- `(with-bipartite-matching <M> (pairs) <body>...)` -- field binder

### Stability

- `(blocking-pairs <M> <prop-prefs> <recv-prefs>)` -- list of `(p . r)` pairs that violate stability (empty iff stable)
- `(stable? <M> <prop-prefs> <recv-prefs>)` -- stability predicate

### Algorithms

- `(gale-shapley <prop-prefs> <recv-prefs>)` -- proposer-optimal stable matching, O(n²) (Gale-Shapley 1962)
- `(gale-shapley/receiver-optimal <prop-prefs> <recv-prefs>)` -- receiver-optimal stable matching
- `(hospital-intern-match <intern-prefs> <hospital-prefs> <hospital-quotas>)` -- intern-optimal stable many-to-one matching via Roth's reduction; returns alist `((hospital . (intern ...)) ...)`
- `(tropical-assignment <cost-fn> <proposers> <receivers>)` → `(<bipartite-matching> . cost)` -- minimum-cost assignment via Kuhn-Munkres O(n³); use `+inf.0` to forbid pairs. On square instances returns a perfect assignment or raises on infeasibility; on unequal sides returns a partial matching of size `min(|proposers|, |receivers|)` with unmatched agents on the larger side derivable via set difference.

### Conway lattice (selection layer)

- `(make-rotation <cycle>)` / `(rotation? <x>)` / `(rotation-cycle <rho>)` -- rotation record (Gusfield-Irving 1989)
- `(apply-rotation <M> <rho>)` -- apply rotation to matching; each proposer shifts to next receiver in cycle
- `(rotations <prop-prefs> <recv-prefs>)` -- enumerate exposed rotations; these are the join-irreducibles of the Conway lattice
- `(stable-matching-lattice <prop-prefs> <recv-prefs>)` -- Conway distributive lattice of all stable matchings under proposer-utility order; brute-force, exponential in `|rotations|`
- `(egalitarian-stable-matching <prop-prefs> <recv-prefs>)` -- minimum sum-of-ranks across both sides; NP-hard in general (Iwama-Manlove 1999), brute force
- `(sex-equal-stable-matching <prop-prefs> <recv-prefs>)` -- minimum |Δ-sum-rank|; same NP-hard caveat

### References

- Gale-Shapley (1962). "College Admissions and the Stability of Marriage." *American Mathematical Monthly* 69(1).
- Conway (1976) via Knuth, *Mariages stables*. (Distributive-lattice theorem.)
- Roth (1985). "The college admissions problem is not equivalent to the marriage problem." *J. Economic Theory* 36.
- Roth & Sotomayor (1990). *Two-Sided Matching*. Cambridge.
- Gusfield & Irving (1989). *The Stable Marriage Problem*. MIT Press.
- Kuhn (1955) / Munkres (1957). Hungarian algorithm.
- Iwama-Manlove et al. (1999). NP-hardness of sex-equal stable matching.

---

## SAT -- `(wile algebra sat)`

A CDCL satisfiability solver (watched literals, 1-UIP conflict analysis, VSIDS, Luby restarts). All decision procedures return `#t`, `#f`, or `'unknown` -- the last only when an optional conflict budget is exhausted. The solver backs `boolean-decide-equivalent?` in this library and closes the De Morgan / complement-law / distributivity gaps that the purely-axiomatic `symbolic-boolean-equivalent?` in `(wile algebra symbolic)` cannot decide.

### CNF decision

A CNF formula is a list of clauses; each clause is a list of nonzero exact integers, where `n` is the positive literal of variable `n` and `-n` its negation. A clause is a disjunction; the formula is their conjunction.

- `(sat-cnf? <clauses> [<budget>])` -- decide CNF satisfiability; `<budget>` is the conflict budget (default 1000000, `#f` for unlimited). Returns `#t` / `#f` / `'unknown`
- `(sat-cnf-model)` -- the witness from the most recent `sat-cnf?` call as a vector indexed `1..N` (index 0 unused), or `#f` if none / last result was unsatisfiable

### S-expression decision

- `(sat? <formula> [<budget>])` -- decide satisfiability of a Boolean S-expression over `and`, `or`, `not`, `xor`, `iff`, `=>` with symbols as variables. Returns `#t` / `#f` / `'unknown`
- `(sat-model)` -- the witness from the most recent `sat?` call as an alist `((<sym> . #t/#f) ...)`, or `#f` if none

### Boolean reasoning

- `(boolean-decide-sat? <formula>)` -- SAT-backed satisfiability; equivalent to `(sat? <formula>)` at the default budget
- `(boolean-decide-equivalent? <a> <b>)` -- decide `A == B` by testing whether `~(A <-> B)` is unsatisfiable. Returns `#t` / `#f` / `'unknown`

### CNF encoding

- `(cnf->flat <clauses>)` -- convert a clause list to a flat vector with `0` terminators (e.g. `'((1 -2 3) (-1 4))` → `#(1 -2 3 0 -1 4 0)`)

### References

- Marques-Silva & Sakallah (1999). GRASP / 1-UIP conflict-driven clause learning.
- Moskewicz et al. (2001). Chaff -- watched literals, VSIDS.
- Biere et al., eds. (2009). *Handbook of Satisfiability*.

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
| Matrix | `(wile algebra matrix)` |
| Polynomial | `(wile algebra polynomial)` |
| Incidence Algebra | `(wile algebra incidence)` |
| Interval | `(wile algebra interval)` |
| Graph (abstract) | `(wile algebra graph)` |
| Combinatorial Graph | `(wile algebra combinatorial-graph)` |
| Unification | `(wile algebra unification)` |
| Formal Concept Analysis | `(wile algebra fca)` |
| Pareto | `(wile algebra pareto)` |
| Abstract Domain | `(wile algebra abstract-domain)` |
| Dataflow | `(wile algebra dataflow)` |
| Matching | `(wile algebra matching)` |
| SAT | `(wile algebra sat)` |

## Umbrella Re-exports

The umbrella `(wile algebra)` re-exports every public binding of its sub-libraries, including sentinels and sub-library-specific helpers such as `tropical-inf` (the tropical semiring's infinity sentinel), `mersenne-31` / `mersenne-61`, and the FCA sorted-string-set primitives. Importing the umbrella is sufficient to reach any symbol documented above; importing an individual sub-library only narrows the surface, it never exposes anything the umbrella hides.
