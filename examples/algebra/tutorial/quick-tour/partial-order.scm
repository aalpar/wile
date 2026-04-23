;; quick-tour: (wile algebra order)
;;
;; A partial order is a reflexive, antisymmetric, transitive relation.
;; Strictly weaker than a total order -- incomparable pairs are allowed.
;; Reach for this when you need to phrase a constraint as "x is at least
;; as general / old / strong as y" without committing to a single axis.

(import (scheme base) (wile algebra order) (wile algebra setoid))
(include "../lib/check.scm")

;; -- Construction: divisibility order on integers --------------------

(define div-po
  (make-partial-order (lambda (a b) (zero? (modulo b a)))))

(check-true (partial-order? div-po)                  "divisibility is a partial order")

;; -- Queries ---------------------------------------------------------

(check-true  (po-leq? div-po 2 6)       "2 | 6")
(check-false (po-leq? div-po 3 10)      "3 does not divide 10")

(check-true  (po-comparable? div-po 4 8)  "4 and 8 are comparable (4 | 8)")
(check-false (po-comparable? div-po 4 6)  "4 and 6 are incomparable")

;; -- Monotonicity check ----------------------------------------------

(check-true  (po-monotone? div-po (lambda (n) (* n 3)) 2 4)
             "x * 3 preserves 2 | 4 ordering")

;; -- Validation (reflexivity + transitivity on samples) --------------

(check= (validate-partial-order div-po '(1 2 3 4 6 12))  #t
        "divisibility satisfies partial-order laws on divisors of 12")

;; -- Antisymmetry with a setoid --------------------------------------

(check= (validate-partial-order/setoid div-po (numeric-setoid) '(1 2 3 4 6 12))
        #t
        "antisymmetry passes under numeric equality")

(display "partial-order tour complete") (newline)
