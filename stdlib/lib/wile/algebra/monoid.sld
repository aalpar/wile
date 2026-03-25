(define-library (wile algebra monoid)
  (export make-monoid monoid?
          monoid-op monoid-identity
          monoid-fold monoid-power
          validate-monoid
          with-monoid)
  (import (scheme base))
  (include "monoid.scm"))
