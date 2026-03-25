(define-library (wile algebra ring)
  (export make-ring ring?
          ring-plus ring-times ring-zero ring-one
          ring-negate ring-minus
          ring->semiring ring->additive-group
          integer-ring modular-ring
          validate-ring
          with-ring
          make-field field?
          field-plus field-times field-zero field-one
          field-negate field-reciprocal field-divide
          field->ring
          rational-field
          validate-field
          with-field)
  (import (scheme base)
          (wile algebra monoid)
          (wile algebra semiring)
          (wile algebra group))
  (include "ring.scm"))
