(define-library (wile algebra)
  (description "Algebraic structures, equational rewriting, and symbolic normalization. Structures: setoids, orders, lattices, closure operators, Heyting/Boolean algebras, monoids, categories, semirings, groups, rings, differential rings, fields. Rewriting: axiom-driven term normalization. Symbolic: theory projections, recursive normalization, transformation tracing.")
  (export
    ;; Setoids
    make-setoid setoid?
    setoid-equiv?
    default-setoid numeric-setoid string-setoid eqv-setoid
    setoid-equivalence-class
    validate-setoid with-setoid
    ;; Partial orders
    make-partial-order partial-order?
    po-leq? po-comparable? po-monotone?
    validate-partial-order validate-partial-order/setoid
    ;; Lattices
    make-lattice lattice?
    lattice-join lattice-meet lattice-bottom lattice-top
    lattice-leq? lattice->partial-order
    flat-lattice powerset-lattice product-lattice map-lattice
    fixpoint fixpoint/widen
    validate-lattice with-lattice
    ;; Closure operators
    make-closure-operator closure-operator?
    closure-close closure-closed? closure-lattice
    closed-elements
    closure->closed-lattice
    downward-closure-operator
    validate-closure-operator with-closure
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
    ;; Categories
    make-category category?
    category-compose category-identity category-equiv?
    category->endomorphism-monoid
    procedure-category
    validate-category with-category
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
    ;; Polynomials
    make-poly polynomial?
    poly-ring poly-coeffs
    poly-zero poly-one
    poly-degree poly-leading-coeff
    poly-plus poly-negate poly-minus
    poly-times
    poly-eval
    poly-derivative
    poly-divmod poly-gcd
    polynomial-ring
    integer-polynomials rational-polynomials
    with-polynomial
    ;; Differential rings
    make-differential-ring differential-ring?
    differential-deriv differential-ring-ring
    differential-nth-deriv differential-constant?
    differential-ring->ring
    dual-number-ring polynomial-derivation
    validate-differential-ring with-differential
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
    make-absorption-axiom absorption-axiom?
    make-associativity-axiom associativity-axiom?
    directional-axiom? axiom?
    axiom->rules
    no-match?
    make-normalizer
    ;; Symbolic algebra
    make-named-axiom named-axiom?
    named-axiom-name named-axiom-general-form named-axiom-axiom
    make-theory theory?
    theory-axioms theory-associative-ops
    theory-filter theory-exclude theory-prioritize theory-merge
    make-rewrite-step rewrite-step?
    step-rule-name step-general-form step-before step-after
    fuel-exhausted-step?
    sexp-term-protocol
    make-recursive-normalizer
    monoid->theory group->theory semiring->theory ring->theory
    field->theory lattice->theory heyting->theory boolean->theory
    discover-equivalences
    format-trace
    ;; Formal Concept Analysis
    make-context context-from-alist fca-context?
    context-objects context-attributes
    intent extent
    concept-lattice concept-extent concept-intent
    concept-lattice->algebra-lattice concept-relationship
    set-add set-intersect set-union set-subset? set-member? set-before
    sort-strings
    ;; Pareto dominance
    dominates? pareto-frontier factor-leq? factor-less?
    ;; Interval arithmetic
    interval-lattice
    interval-add interval-sub interval-mul
    inf<= inf-min inf-max inf+ inf- inf*
    ;; Graph algorithms
    make-graph-analysis graph-analysis?
    graph-query graph-query-all
    ;; Matrices
    make-semiring-matrix semiring-matrix?
    semiring-matrix-from-rows semiring-matrix->rows
    semiring-matrix-identity
    semiring-matrix-ref semiring-matrix-shape
    semiring-matrix-rows semiring-matrix-cols
    semiring-matrix-semiring
    semiring-matrix-add semiring-matrix-mul
    semiring-matrix-power semiring-matrix-closure
    semiring-matrix-permanent
    make-sparse-semiring-matrix sparse-semiring-matrix?
    sparse-semiring-matrix-ref
    sparse-semiring-matrix-rows sparse-semiring-matrix-cols
    sparse-semiring-matrix-semiring
    semiring-matrix->sparse sparse->semiring-matrix
    with-semiring-matrix
    ;; Path D polymorphic API
    matrix-rep-tag matrix-for-each-entry matrix-fold-entries
    matrix? matrix-ref matrix-rows matrix-cols
    matrix-shape matrix-semiring
    matrix-add matrix-add!
    matrix-mul matrix-mul!
    matrix-op-supported?)
  (import (wile algebra setoid)
          (wile algebra order)
          (wile algebra lattice)
          (wile algebra closure)
          (wile algebra heyting)
          (wile algebra boolean)
          (wile algebra monoid)
          (wile algebra category)
          (wile algebra semiring)
          (wile algebra group)
          (wile algebra ring)
          (wile algebra polynomial)
          (wile algebra differential)
          (wile algebra galois)
          (wile algebra rewrite)
          (wile algebra symbolic)
          (wile algebra fca)
          (wile algebra pareto)
          (wile algebra interval)
          (wile algebra graph)
          (wile algebra matrix)))
