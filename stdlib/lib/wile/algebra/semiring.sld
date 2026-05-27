(define-library (wile algebra semiring)
  (description "Semirings: boolean, tropical, counting, and lifted variants.")
  (export make-semiring semiring?
          semiring-plus semiring-times semiring-zero semiring-one
          semiring-carrier semiring-eq?
          semiring->additive-monoid semiring->multiplicative-monoid
          boolean-semiring tropical-semiring tropical-inf tropical-eq?
          counting-semiring bigint-counting-semiring
          modular-counting-semiring mersenne-31 mersenne-61
          log-counting-semiring
          saturating-counting-semiring
          bounded-carrier-semiring?
          validate-semiring
          with-semiring)
  (import (scheme base)
          (wile algebra setoid)
          (wile algebra monoid))
  (include "semiring.scm"))
