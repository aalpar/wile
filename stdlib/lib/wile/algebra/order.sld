(define-library (wile algebra order)
  (description "Partial and total orders with comparison operations.")
  (export make-partial-order partial-order?
          po-leq? po-comparable? po-monotone?
          validate-partial-order
          validate-partial-order/setoid)
  (import (scheme base)
          (wile algebra setoid))
  (include "order.scm"))
