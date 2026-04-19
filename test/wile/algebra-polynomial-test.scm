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

(test-group "poly-times"
  (let ((R (integer-ring)))
    ;; (1 + x)(1 + x) = 1 + 2x + x^2
    (test '(1 2 1) (poly-coeffs (poly-times (make-poly R '(1 1))
                                            (make-poly R '(1 1)))))
    ;; (1 - x)(1 + x) = 1 - x^2
    (test '(1 0 -1) (poly-coeffs (poly-times (make-poly R '(1 -1))
                                             (make-poly R '(1 1)))))
    ;; Zero annihilates
    (test '() (poly-coeffs (poly-times (poly-zero R) (make-poly R '(1 2 3)))))
    ;; One is identity
    (test '(1 2 3) (poly-coeffs (poly-times (poly-one R) (make-poly R '(1 2 3)))))))

(test-group "poly-eval (Horner)"
  (let ((R (integer-ring)))
    ;; p(x) = 1 + 2x + 3x^2; p(0) = 1, p(1) = 6, p(2) = 17
    (let ((p (make-poly R '(1 2 3))))
      (test 1  (poly-eval p 0))
      (test 6  (poly-eval p 1))
      (test 17 (poly-eval p 2)))
    ;; Zero polynomial evaluates to ring-zero
    (test 0 (poly-eval (poly-zero R) 42))
    ;; Constant polynomial evaluates to its constant
    (test 5 (poly-eval (make-poly R '(5)) 99))))

(test-group "poly-derivative"
  (let ((R (integer-ring)))
    ;; D(1 + 2x + 3x^2) = 2 + 6x
    (test '(2 6) (poly-coeffs (poly-derivative (make-poly R '(1 2 3)))))
    ;; D(constant) = 0
    (test '() (poly-coeffs (poly-derivative (make-poly R '(42)))))
    ;; D(0) = 0
    (test '() (poly-coeffs (poly-derivative (poly-zero R))))
    ;; D(x) = 1
    (test '(1) (poly-coeffs (poly-derivative (make-poly R '(0 1)))))))

(test-group "poly-divmod (rational field)"
  (let* ((F (rational-field))
         (R (field->ring F)))
    ;; (x^2 - 1) / (x - 1) = (x + 1), remainder 0
    (let-values (((q r) (poly-divmod (make-poly R '(-1 0 1))
                                     (make-poly R '(-1 1))
                                     F)))
      (test '(1 1) (poly-coeffs q))
      (test '()    (poly-coeffs r)))
    ;; (x^2 + 1) / (x) = (x), remainder 1
    (let-values (((q r) (poly-divmod (make-poly R '(1 0 1))
                                     (make-poly R '(0 1))
                                     F)))
      (test '(0 1) (poly-coeffs q))
      (test '(1)   (poly-coeffs r)))
    ;; Dividing smaller by larger: quotient 0, remainder is dividend
    (let-values (((q r) (poly-divmod (make-poly R '(1 2))
                                     (make-poly R '(1 2 3))
                                     F)))
      (test '()    (poly-coeffs q))
      (test '(1 2) (poly-coeffs r)))))

(test-group "poly-gcd (rational field)"
  (let* ((F (rational-field))
         (R (field->ring F)))
    ;; gcd(x^2 - 1, x - 1) = x - 1 (monic)
    (test '(-1 1) (poly-coeffs (poly-gcd (make-poly R '(-1 0 1))
                                         (make-poly R '(-1 1))
                                         F)))
    ;; gcd(x^2 - 1, x^2 - 2x + 1) = x - 1 (monic)
    (test '(-1 1) (poly-coeffs (poly-gcd (make-poly R '(-1 0 1))
                                         (make-poly R '(1 -2 1))
                                         F)))
    ;; gcd(2 + 2x, 0) = 1 + x (monic-normalized from 2 + 2x)
    (test '(1 1) (poly-coeffs (poly-gcd (make-poly R '(2 2))
                                        (poly-zero R)
                                        F)))))

(test-group "polynomial-ring constructor"
  (let* ((R   (integer-ring))
         (PR  (polynomial-ring R))
         (p   (make-poly R '(1 2)))    ; 1 + 2x
         (q   (make-poly R '(3 4))))   ; 3 + 4x
    (test #t (ring? PR))
    ;; PR's operations should agree with poly-plus/poly-times
    (test '(4 6)   (poly-coeffs (ring-plus PR p q)))
    (test '(3 10 8) (poly-coeffs (ring-times PR p q)))
    (test '() (poly-coeffs (ring-zero PR)))
    (test '(1) (poly-coeffs (ring-one PR)))
    (test '(-1 -2) (poly-coeffs (ring-negate PR p)))))

(test-group "polynomial-ring enables recursion (bivariate via R[x][y])"
  (let* ((R   (integer-ring))
         (Rx  (polynomial-ring R))           ; Z[x]
         (Rxy (polynomial-ring Rx)))         ; Z[x][y]
    (test #t (ring? Rxy))
    ;; Element of Z[x][y] has poly-over-R coefficients
    (let ((x (make-poly R '(0 1)))            ; x
          (y-plus-x (make-poly Rx
                      (list (make-poly R '(0 1))  ; x
                            (make-poly R '(1))))))  ; y
      (test #t (polynomial? y-plus-x)))))

(test-group "with-polynomial macro"
  (let ((R (integer-ring)))
    (test '(2 4) (poly-coeffs
                   (with-polynomial R (plus times zero one negate)
                     (plus (make-poly R '(1 2)) (make-poly R '(1 2))))))))

(test-group "validate-polynomial-ring"
  ;; Over the rational field (a field is a ring), polynomial laws must hold
  ;; on a small sample of polynomials.
  (let* ((F  (rational-field))
         (R  (field->ring F))
         (PR (polynomial-ring R))
         (samples (list (make-poly R '())
                        (make-poly R '(1))
                        (make-poly R '(1 2))
                        (make-poly R '(0 1))
                        (make-poly R '(-1 0 1)))))
    (test #t (validate-ring PR samples))))

(test-group "pre-built integer-polynomials"
  (let ((PR (integer-polynomials)))
    (test #t (ring? PR))
    ;; Elements must be polynomials over integer-ring
    (let ((p (make-poly (integer-ring) '(1 2)))
          (q (make-poly (integer-ring) '(3 4))))
      (test '(4 6) (poly-coeffs (ring-plus PR p q))))))

(test-group "pre-built rational-polynomials"
  (let ((PR (rational-polynomials)))
    (test #t (ring? PR))
    (let ((p (make-poly (field->ring (rational-field)) '(1/2 1/3))))
      (test '(1 2/3) (poly-coeffs (ring-plus PR p p))))))

(test-end)
(test-exit)
