(define-library (wile algebra galois)
  (description "Galois connections between lattices.")
  (export make-galois-connection galois-connection?
          gc-alpha gc-gamma
          gc-concrete-po gc-abstract-lattice
          gc-sound?)
  (import (scheme base)
          (wile algebra setoid)
          (wile algebra order)
          (wile algebra lattice))
  (include "galois.scm"))
