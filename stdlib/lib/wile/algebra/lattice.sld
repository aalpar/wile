(define-library (wile algebra lattice)
  (description "Lattice types: flat, powerset, product, map lattices with join/meet.")
  (export make-lattice lattice?
          lattice-join lattice-meet lattice-bottom lattice-top
          lattice-leq? lattice->partial-order
          flat-lattice powerset-lattice product-lattice map-lattice
          fixpoint fixpoint/widen
          validate-lattice
          with-lattice)
  (import (scheme base)
          (wile algebra order))
  (include "lattice.scm"))
