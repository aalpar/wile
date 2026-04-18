;;; algebra-polynomial-test.scm — Polynomial library tests

(import (scheme base)
        (chibi test)
        (wile algebra ring)
        (wile algebra polynomial))

(test-begin "polynomials")

;; -- Construction & predicate --

(test-group "make-poly and predicate"
  (let ((R (integer-ring)))
    (test #t (polynomial? (make-poly R '(1 2 3))))
    (test #f (polynomial? '(1 2 3)))
    (test #f (polynomial? 42))
    ;; Zero polynomial
    (test #t (polynomial? (make-poly R '())))))

(test-group "normalization strips trailing zeros"
  (let ((R (integer-ring)))
    ;; 1 + 2x + 0 + 0 → 1 + 2x
    (test '(1 2) (poly-coeffs (make-poly R '(1 2 0 0))))
    ;; All zeros → empty (zero poly)
    (test '() (poly-coeffs (make-poly R '(0 0 0))))
    ;; Already normalized
    (test '(1 2 3) (poly-coeffs (make-poly R '(1 2 3))))
    ;; Empty stays empty
    (test '() (poly-coeffs (make-poly R '())))))

(test-group "poly-ring accessor"
  (let* ((R (integer-ring))
         (p (make-poly R '(1 2 3))))
    (test #t (ring? (poly-ring p)))))

(test-group "poly-zero and poly-one"
  (let ((R (integer-ring)))
    (test '()  (poly-coeffs (poly-zero R)))
    (test '(1) (poly-coeffs (poly-one R)))))

(test-group "poly-degree"
  (let ((R (integer-ring)))
    (test -1 (poly-degree (make-poly R '())))        ; zero poly: -1 by convention
    (test 0  (poly-degree (make-poly R '(5))))       ; constant
    (test 2  (poly-degree (make-poly R '(1 2 3))))   ; 1 + 2x + 3x^2
    (test 1  (poly-degree (make-poly R '(1 2 0)))))) ; normalized to (1 2)

(test-group "poly-leading-coeff"
  (let ((R (integer-ring)))
    (test 0 (poly-leading-coeff (make-poly R '())))   ; zero poly → ring-zero
    (test 5 (poly-leading-coeff (make-poly R '(5))))
    (test 3 (poly-leading-coeff (make-poly R '(1 2 3))))))

(test-group "poly-plus"
  (let ((R (integer-ring)))
    ;; (1 + 2x) + (3 + 4x + 5x^2) = 4 + 6x + 5x^2
    (test '(4 6 5) (poly-coeffs (poly-plus (make-poly R '(1 2))
                                           (make-poly R '(3 4 5)))))
    ;; Zero identity
    (test '(1 2 3) (poly-coeffs (poly-plus (poly-zero R)
                                           (make-poly R '(1 2 3)))))
    ;; Cancellation normalizes: (1 + x) + (-1 - x) = 0
    (test '() (poly-coeffs (poly-plus (make-poly R '(1 1))
                                      (make-poly R '(-1 -1)))))))

(test-group "poly-negate"
  (let ((R (integer-ring)))
    (test '(-1 -2 -3) (poly-coeffs (poly-negate (make-poly R '(1 2 3)))))
    (test '() (poly-coeffs (poly-negate (poly-zero R))))))

(test-group "poly-minus"
  (let ((R (integer-ring)))
    ;; (3 + 4x) - (1 + 2x) = 2 + 2x
    (test '(2 2) (poly-coeffs (poly-minus (make-poly R '(3 4))
                                          (make-poly R '(1 2)))))))

(test-end)
(test-exit)
