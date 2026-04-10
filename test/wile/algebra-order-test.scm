;;; algebra-order-test.scm — Partial order tests

(import (scheme base)
        (chibi test)
        (wile algebra order)
        (wile algebra setoid))

(test-begin "partial-orders")

;; -- Construction and predicate --

(test-group "construction"
  (let ((po (make-partial-order <=)))
    (test #t (partial-order? po))
    (test #f (partial-order? 42))
    (test #f (partial-order? "not a po"))))

;; -- po-leq? --

(test-group "po-leq?"
  (let ((po (make-partial-order <=)))
    (test #t (po-leq? po 1 2))
    (test #t (po-leq? po 1 1))
    (test #f (po-leq? po 2 1))))

;; -- po-comparable? --

(test-group "po-comparable?"
  ;; divisibility partial order: a ≤ b iff a divides b
  (let ((div-po (make-partial-order
                  (lambda (a b) (zero? (modulo b a))))))
    (test #t (po-comparable? div-po 2 6))   ; 2|6
    (test #t (po-comparable? div-po 6 2))   ; 2|6 reversed
    (test #f (po-comparable? div-po 2 3)))) ; neither 2|3 nor 3|2

;; -- po-monotone? --

(test-group "po-monotone?"
  (let ((po (make-partial-order <=)))
    ;; doubling is monotone on ≤
    (test #t (po-monotone? po (lambda (x) (* x 2)) 1 3))
    ;; negation is NOT monotone on ≤ (1≤3 but -1 > -3)
    (test #f (po-monotone? po (lambda (x) (- x)) 1 3))))

;; -- validate-partial-order --

(test-group "validate-partial-order"
  ;; ≤ on integers is a valid partial order
  (test #t (validate-partial-order
             (make-partial-order <=)
             '(1 2 3 4 5)))
  ;; A broken "order" that isn't reflexive
  (let ((result (validate-partial-order
                  (make-partial-order <)  ; strict < is not reflexive
                  '(1 2 3))))
    (test #f (eq? #t result))  ; should return violations, not #t
    ;; Each violation should be (reflexivity x)
    (test 'reflexivity (caar result))))

(test-group "validate-partial-order/setoid"
  ;; Valid: numeric order with numeric equality
  (test #t (validate-partial-order/setoid
             (make-partial-order <=) numeric-setoid '(1 2 3)))
  ;; Invalid: <= is not antisymmetric under eqv? for 1 and 1.0
  ;; because (<= 1 1.0) and (<= 1.0 1) but (eqv? 1 1.0) is #f
  (let ((result (validate-partial-order/setoid
                  (make-partial-order <=) eqv-setoid '(1 1.0))))
    (test #f (eq? #t result))
    (test 'antisymmetry (caar result))))

(test-end)
(test-exit)
