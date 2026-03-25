(define-library (wile algebra galois)
  (export make-galois-connection galois-connection?
          gc-alpha gc-gamma
          gc-concrete-po gc-abstract-lattice
          gc-sound?)
  (import (scheme base)
          (wile algebra order)
          (wile algebra lattice))
  (include "galois.scm"))
