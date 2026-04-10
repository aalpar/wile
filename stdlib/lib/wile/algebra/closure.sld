(define-library (wile algebra closure)
  (description "Closure operators: extensive, monotone, idempotent functions on lattices.")
  (export make-closure-operator closure-operator?
          closure-close closure-closed? closure-lattice
          closed-elements
          closure->closed-lattice
          downward-closure-operator
          validate-closure-operator
          with-closure)
  (import (scheme base)
          (wile algebra lattice)
          (wile algebra order))
  (include "closure.scm"))
