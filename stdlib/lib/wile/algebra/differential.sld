(define-library (wile algebra differential)
  (description "Differential rings: rings equipped with a derivation satisfying the Leibniz rule.")
  (export make-differential-ring differential-ring?
          differential-deriv differential-ring-ring
          differential-nth-deriv differential-constant?
          differential-ring->ring
          dual-number-ring polynomial-derivation
          validate-differential-ring
          with-differential)
  (import (scheme base)
          (wile algebra setoid)
          (wile algebra ring)
          (wile algebra polynomial))
  (include "differential.scm"))
