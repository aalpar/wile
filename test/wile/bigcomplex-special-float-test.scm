;;; bigcomplex-special-float-test.scm — +inf.0 / +nan.0 meeting a BigComplex
;;;
;;; Wile-only: the #m reader prefix is not R7RS. Mirrors
;;; TestSpecialFloatBigComplex_ImaginaryDigits in pkg/values/promotion_test.go at
;;; the Scheme layer. An Inf/NaN has no imaginary part to contribute, so an
;;; additive result must carry the BigComplex's imaginary component digit for
;;; digit; it used to come back rounded to float64.
;;;
;;; Every group runs under an explicit equal?: chibi `test`'s default comparator
;;; is approximate (1e-5), which a float64-rounded imaginary part satisfies.
;;;
;;; The multiplicative group pins today's answers so a change to them is
;;; deliberate: they are right while the BigComplex parts fit in float64 and
;;; wrong beyond (filed in todo/tier1-security-correctness.md).

(import (scheme base)
        (scheme inexact)
        (scheme complex)
        (chibi test))

(test-begin "bigcomplex-special-float")

(define-syntax with-exact-comparison
  (syntax-rules ()
    ((_ body ...)
     (parameterize ((current-test-comparator equal?))
       body ...))))

(define im #m9.87654321098765432109876543210987)
(define z (make-rectangular #m0 im))

(test-group "additive keeps the imaginary digits"
  (with-exact-comparison
    (test im (imag-part (+ z +inf.0)))
    (test im (imag-part (+ +inf.0 z)))
    (test im (imag-part (- z +inf.0)))
    (test (- im) (imag-part (- +inf.0 z)))
    (test im (imag-part (+ z +nan.0)))
    (test im (imag-part (- z +nan.0)))
    (test (- im) (imag-part (- +nan.0 z)))
    (test (/ #m1 #m0) (real-part (+ z +inf.0)))
    (test (/ #m-1 #m0) (real-part (- z +inf.0)))
    (test #t (nan? (real-part (+ z +nan.0))))
    ;; a finite real part past float64 range must not become -inf.0 on the way
    ;; (+inf.0 + -inf.0 is NaN; the true sum is +inf.0)
    (test (/ #m1 #m0) (real-part (+ (make-rectangular #m-1e400 #m1) +inf.0)))
    (test (/ #m1 #m0) (real-part (- +inf.0 (make-rectangular #m-1e400 #m1))))
    ;; but a real part that IS infinite is not clamped: +inf + -inf is NaN
    (test #t (nan? (real-part (+ (make-rectangular (/ #m1 #m0) #m1) -inf.0))))
    (test #t (nan? (real-part (- (make-rectangular (/ #m1 #m0) #m1) +inf.0))))))

;; Subtracting from a real Inf negates the imaginary part; it must not be
;; computed as 0.0 - d, which is +0.0 for d = +0.0 and loses the sign.
(test-group "signed zero"
  (with-exact-comparison
    (test #m-0.0 (imag-part (- +inf.0 (make-rectangular #m0 #m0.0))))
    (test #m-0.0 (imag-part (+ (make-rectangular #m0 #m-0.0) +inf.0)))))

;; An exact imaginary part rounds to float64 as it does on the finite path.
(test-group "exact imaginary part"
  (with-exact-comparison
    (test (imag-part (+ 1.0 1+1/3i)) (imag-part (+ +inf.0 1+1/3i)))
    (test (- (imag-part (+ 1.0 1+1/3i))) (imag-part (- +inf.0 1+1/3i)))
    (test (/ #m1 #m0) (imag-part (+ +inf.0 (make-rectangular 1 (expt 10 400)))))))

;; The BigFloat twin: a finite BigFloat past float64 range is not +-inf.
(test-group "out-of-range BigFloat partner"
  (with-exact-comparison
    (test +inf.0 (+ +inf.0 #m-1e400))
    (test +inf.0 (+ #m-1e400 +inf.0))
    (test +inf.0 (- +inf.0 #m1e400))
    (test -inf.0 (- #m1e400 +inf.0))
    (test +inf.0 (/ +inf.0 #m1e400))))

;; The multiplicative arms keep the complex128 route.
(test-group "multiplicative arms are unchanged"
  (with-exact-comparison
    (test "+nan.0+inf.0i" (number->string (* z +inf.0)))
    (test "+nan.0+nan.0i" (number->string (* z +nan.0)))
    (test "0.0+0.0i" (number->string (/ z +inf.0)))
    (test "+nan.0+nan.0i" (number->string (/ z +nan.0)))))

(test-end)
(test-exit)
