(define-library (wile algebra order)
  (export make-partial-order partial-order?
          po-leq? po-comparable? po-monotone?
          validate-partial-order)
  (import (scheme base))
  (include "order.scm"))
