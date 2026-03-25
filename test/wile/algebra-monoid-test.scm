;;; algebra-monoid-test.scm — Monoid tests

(import (scheme base)
        (chibi test)
        (wile algebra monoid))

(test-begin "monoids")

(define sum-monoid (make-monoid + 0))
(define product-monoid (make-monoid * 1))

(test-group "construction"
  (test #t (monoid? sum-monoid))
  (test #f (monoid? 42)))

(test-group "monoid-op"
  (test 5 (monoid-op sum-monoid 2 3))
  (test 6 (monoid-op product-monoid 2 3)))

(test-group "monoid-identity"
  (test 0 (monoid-identity sum-monoid))
  (test 1 (monoid-identity product-monoid)))

(test-group "monoid-fold"
  (test 10 (monoid-fold sum-monoid '(1 2 3 4)))
  (test 24 (monoid-fold product-monoid '(1 2 3 4)))
  ;; empty list -> identity
  (test 0 (monoid-fold sum-monoid '())))

(test-group "monoid-power"
  ;; 3 + 3 + 3 + 3 = 12
  (test 12 (monoid-power sum-monoid 3 4))
  ;; 2 * 2 * 2 = 8
  (test 8 (monoid-power product-monoid 2 3))
  ;; power 0 = identity
  (test 0 (monoid-power sum-monoid 99 0))
  (test 1 (monoid-power product-monoid 99 0)))

(test-group "with-monoid"
  (test 6 (with-monoid sum-monoid (op identity)
            (op (op identity 1) (op 2 3)))))

(test-group "validate-monoid"
  (test #t (validate-monoid sum-monoid '(0 1 2 3 5 10))))

(test-end)
(test-exit)
