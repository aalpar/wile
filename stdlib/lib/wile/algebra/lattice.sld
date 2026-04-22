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
          diamond-lattice pentagon-lattice
          ;; §5.5 — irreducibles
          join-irreducibles meet-irreducibles
          join-irreducible? meet-irreducible?
          ;; §5.5 — distributivity and modularity
          distributive? modular?
          validate-distributive-lattice validate-distributive-lattice/setoid
          validate-modular-lattice     validate-modular-lattice/setoid)
  (import (scheme base)
          (srfi 1)
          (wile algebra order)
          (wile algebra setoid))
  (include "lattice.scm"))
