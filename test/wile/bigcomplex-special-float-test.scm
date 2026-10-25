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
;;; The last two groups (portable spellings, but they need exact complex and
;;; bignum contagion that does not overflow, so they are not in test/scheme)
;;; pin the same guard for BigInteger, Rational and exact-complex partners.
;;;
;;; The multiplicative groups pin real x complex and real / complex part-wise:
;;; an out-of-range part stays finite and non-zero, an EXACT zero part
;;; annihilates (0.0), and an INEXACT zero part gives NaN, as Chez does.

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
    (test +inf.0 (/ +inf.0 #m1e400))
    (test 0.0 (/ #m1e400 +inf.0))
    ;; an INFINITE BigFloat partner is not clamped: +inf + -inf is NaN
    (test #t (nan? (+ +inf.0 (/ #m-1 #m0))))))

;; z's real part is an INEXACT zero (#m0), so +inf.0 times it is NaN; Chez
;; agrees on (* +inf.0 0+9.87i) => +nan.0+inf.0i.
(test-group "multiplicative arms, inexact zero real part"
  (with-exact-comparison
    (test "+nan.0+inf.0i" (number->string (* z +inf.0)))
    (test "+nan.0+inf.0i" (number->string (* +inf.0 z)))
    (test "+nan.0+nan.0i" (number->string (* z +nan.0)))
    (test "0.0+0.0i" (number->string (/ z +inf.0)))
    (test "+nan.0-inf.0i" (number->string (/ +inf.0 z)))
    (test "+nan.0+nan.0i" (number->string (/ z +nan.0)))
    (test "+nan.0+nan.0i" (number->string (/ +inf.0 (make-rectangular #m0.0 #m0.0))))))

;; A BigFloat part past float64 range (either way) stays finite and non-zero.
;; Chez gives the same answers for the exact spellings (expt 10 400).
(test-group "multiplicative arms, out-of-range BigFloat parts"
  (with-exact-comparison
    (test "+inf.0+inf.0i" (number->string (* +inf.0 (make-rectangular #m1 #m1e400))))
    (test "+inf.0+inf.0i" (number->string (* (make-rectangular #m1 #m1e400) +inf.0)))
    (test "+inf.0+inf.0i" (number->string (* +inf.0 (make-rectangular #m1e-400 #m1))))
    (test "+inf.0-inf.0i" (number->string (/ +inf.0 (make-rectangular #m1 #m1e400))))
    (test "+inf.0-inf.0i" (number->string (/ +inf.0 (make-rectangular #m1e400 #m1e400))))
    (test "+inf.0" (number->string (* +inf.0 #m1e-400)))
    (test "-inf.0" (number->string (* +inf.0 #m-1e-400)))
    ;; an INFINITE part is not clamped: Inf / Inf is NaN
    (test "+nan.0+nan.0i" (number->string (/ +inf.0 (make-rectangular #m1 (/ #m1 #m0)))))
    (test "+inf.0+inf.0i" (number->string (* +inf.0 (make-rectangular #m1 (/ #m1 #m0)))))))

;; The exact partner must not be rounded to +-inf.0 or 0.0 before the special
;; value decides: that made (+ +inf.0 (- (expt 10 400))) Inf - Inf = NaN and
;; (* +inf.0 (/ 1 (expt 10 400))) Inf * 0 = NaN.
(test-group "special float with an out-of-range exact partner"
  (with-exact-comparison
    (test "+inf.0" (number->string (+ +inf.0 (- (expt 10 400)))))
    (test "-inf.0" (number->string (- (expt 10 400) +inf.0)))
    (test "+inf.0" (number->string (/ +inf.0 (expt 10 400))))
    (test "0.0" (number->string (/ (expt 10 400) +inf.0)))
    (test "+inf.0" (number->string (* +inf.0 (/ 1 (expt 10 400)))))
    (test "-inf.0" (number->string (* +inf.0 (/ -1 (expt 10 400)))))
    ;; controls: NaN still wins; a FINITE float partner stays lossy
    (test "+nan.0" (number->string (+ +nan.0 (expt 10 400))))
    (test "+inf.0" (number->string (+ 1.0 (expt 10 400))))))

;; A real Inf/NaN scales each part of a complex. An exact zero part annihilates
;; (0.0); an inexact zero part gives NaN.
(test-group "special float times or over an exact complex"
  (with-exact-comparison
    (test "0.0+inf.0i" (number->string (* +inf.0 +2i)))
    (test "0.0-inf.0i" (number->string (* -inf.0 +2i)))
    (test "0.0+nan.0i" (number->string (* +nan.0 +2i)))
    (test "0.0+inf.0i" (number->string (* +2i +inf.0)))
    (test "0.0-inf.0i" (number->string (/ +inf.0 +2i)))
    (test "0.0+inf.0i" (number->string (/ -inf.0 +2i)))
    (test "+inf.0+inf.0i" (number->string (* +inf.0 (make-rectangular 1 (expt 10 400)))))
    (test "+inf.0-inf.0i" (number->string (/ +inf.0 (make-rectangular 1 (expt 10 400)))))
    (test "+inf.0-inf.0i"
          (number->string (/ +inf.0 (make-rectangular (expt 10 400) (expt 10 400)))))
    (test "+inf.0-inf.0i"
          (number->string (/ +inf.0 (make-rectangular (/ 1 (expt 10 400)) 1))))
    (test "0.0+0.0i" (number->string (/ (make-rectangular 1 (expt 10 400)) +inf.0)))
    ;; controls: inexact zero parts and in-range parts
    (test "+nan.0+inf.0i" (number->string (* +inf.0 0+9.87i)))
    (test "+nan.0-inf.0i" (number->string (/ +inf.0 0+9.87i)))
    (test "+inf.0-inf.0i" (number->string (/ +inf.0 (make-rectangular 1 1e300))))
    (test "+inf.0+inf.0i" (number->string (* +inf.0 (make-rectangular 1 1e300))))))

(test-end)
(test-exit)
