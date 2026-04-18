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

(test-end)
(test-exit)
