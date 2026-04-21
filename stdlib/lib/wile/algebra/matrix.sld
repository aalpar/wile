(define-library (wile algebra matrix)
  (description "Semiring-parameterized matrix algebra with dense and sparse representations. Operations (add, multiply, power, Kleene closure, permanent) work over any semiring — Boolean for reachability, tropical for shortest paths, counting for path counting, max-plus for Viterbi.")
  (export make-semiring-matrix semiring-matrix?
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
          sparse-semiring-matrix-semiring sparse-semiring-matrix-entries
          semiring-matrix->sparse sparse->semiring-matrix
          with-semiring-matrix)
  (import (scheme base)
          (wile algebra semiring))
  (include "matrix.scm"))
