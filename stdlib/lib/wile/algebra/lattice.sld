(define-library (wile algebra lattice)
  (description "Lattice types: flat, powerset, product, map lattices with join/meet.")
  (export make-lattice lattice?
          lattice-join lattice-meet lattice-bottom lattice-top
          lattice-leq? lattice-equal? lattice->partial-order
          flat-lattice powerset-lattice product-lattice map-lattice
          fixpoint fixpoint/widen
          validate-lattice
          with-lattice
          ;; §5.5 — extended introspection on <lattice>
          lattice-setoid lattice-equiv?
          lattice-cardinality lattice-elements
          finite-lattice?
          ;; §5.5 — canonical presets
          chain-lattice boolean-lattice
          diamond-lattice pentagon-lattice)
  (import (scheme base)
          (wile algebra order)
          (wile algebra setoid))
  (include "lattice.scm"))
