(define-library (wile algebra polynomial)
  (description "Univariate polynomials over a coefficient ring. Ascending-order coefficient lists, normalized (no trailing zero). Supports arithmetic, Horner evaluation, formal derivative, and (field-required) Euclidean divmod/gcd.")
  (export make-poly polynomial?
          poly-ring poly-coeffs
          poly-zero poly-one
          poly-degree poly-leading-coeff
          poly-plus poly-negate poly-minus
          poly-times)
  (import (scheme base)
          (wile algebra ring))
  (include "polynomial.scm"))
