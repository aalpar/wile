(define-library (wile algebra monoid)
  (description "Monoids with identity and associative binary operation.")
  (export make-monoid monoid?
          monoid-op monoid-identity
          monoid-fold monoid-power
          validate-monoid
          with-monoid)
  (import (scheme base))
  (include "monoid.scm"))
