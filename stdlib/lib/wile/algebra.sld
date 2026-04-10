(define-library (wile algebra)
  (description "Algebraic structures: orders, lattices, Heyting/Boolean algebras, monoids, semirings, groups, rings, fields.")
  (export
    ;; Partial orders
    make-partial-order partial-order?
    po-leq? po-comparable? po-monotone?
    validate-partial-order
    ;; Lattices
    make-lattice lattice?
    lattice-join lattice-meet lattice-bottom lattice-top
    lattice-leq? lattice->partial-order
    flat-lattice powerset-lattice product-lattice map-lattice
    fixpoint fixpoint/widen
    validate-lattice with-lattice
    ;; Heyting algebras
    make-heyting-algebra heyting-algebra?
    heyting-join heyting-meet heyting-bottom heyting-top
    heyting-leq? heyting-implies heyting-negate
    heyting->lattice
    powerset-heyting map-heyting
    validate-heyting-algebra with-heyting
    ;; Boolean algebras
    make-boolean-algebra boolean-algebra?
    boolean-join boolean-meet boolean-bottom boolean-top
    boolean-leq? boolean-complement
    boolean->heyting boolean->lattice boolean->ring
    powerset-boolean
    validate-boolean-algebra with-boolean
    ;; Monoids
    make-monoid monoid?
    monoid-op monoid-identity monoid-fold monoid-power
    validate-monoid with-monoid
    ;; Semirings
    make-semiring semiring?
    semiring-plus semiring-times semiring-zero semiring-one
    semiring->additive-monoid semiring->multiplicative-monoid
    boolean-semiring tropical-semiring counting-semiring
    validate-semiring with-semiring
    ;; Groups
    make-group group?
    group-op group-identity group-inverse
    group->monoid
    validate-group with-group
    ;; Rings
    make-ring ring?
    ring-plus ring-times ring-zero ring-one
    ring-negate ring-minus
    ring->semiring ring->additive-group
    integer-ring modular-ring
    validate-ring with-ring
    ;; Fields
    make-field field?
    field-plus field-times field-zero field-one
    field-negate field-reciprocal field-divide
    field->ring
    rational-field
    validate-field with-field
    ;; Galois connections
    make-galois-connection galois-connection?
    gc-alpha gc-gamma
    gc-concrete-po gc-abstract-lattice
    gc-sound?
    ;; Rewriting
    make-term-protocol term-protocol?
    term-compound? term-get-operator term-get-operands term-make-term term-compare
    make-identity-axiom identity-axiom?
    make-commutativity-axiom commutativity-axiom?
    make-absorbing-axiom absorbing-axiom?
    make-idempotence-axiom idempotence-axiom?
    make-involution-axiom involution-axiom?
    axiom?
    make-normalizer)
  (import (wile algebra order)
          (wile algebra lattice)
          (wile algebra heyting)
          (wile algebra boolean)
          (wile algebra monoid)
          (wile algebra semiring)
          (wile algebra group)
          (wile algebra ring)
          (wile algebra galois)
          (wile algebra rewrite)))
