(define-library (wile algebra semiring)
  (export make-semiring semiring?
          semiring-plus semiring-times semiring-zero semiring-one
          semiring->additive-monoid semiring->multiplicative-monoid
          boolean-semiring tropical-semiring counting-semiring
          validate-semiring
          with-semiring)
  (import (scheme base)
          (wile algebra monoid))
  (include "semiring.scm"))
