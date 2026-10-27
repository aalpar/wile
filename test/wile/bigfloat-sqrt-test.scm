;;; bigfloat-sqrt-test.scm — sqrt of a BigFloat
;;;
;;; Wile-only: the #m reader prefix and the l exponent marker are not R7RS, so
;;; this lives here rather than in test/scheme/numbers-test.scm, which also runs
;;; under other implementations. Mirrors TestSqrtBigFloat in
;;; extensions/math/prim_transcendental_test.go at the Scheme layer.
;;;
;;; chibi `test` with its default comparator is approximate: an inexact expected
;;; value matches within current-test-epsilon (1e-5), which a float64 result
;;; satisfies. Every group here therefore runs under an explicit equal?, which
;;; goes through eqv? on numbers, so a row checks the kind (BigFloat, not
;;; Float), the sign of a zero, and every digit.

(import (scheme base)
        (scheme inexact)
        (scheme complex)
        (chibi test))

(test-begin "bigfloat-sqrt")

(define-syntax with-exact-comparison
  (syntax-rules ()
    ((_ body ...)
     (parameterize ((current-test-comparator equal?))
       body ...))))

(test-group "positive"
  (with-exact-comparison
    (test 1.4142135623730950488016887242096980785696718753769480731766797379907324784621l0
          (sqrt #m2))
    (test #m2 (sqrt #m4))
    (test #f (exact? (sqrt #m4)))
    (test #m0 (sqrt #m0))
    ;; a negative zero keeps its sign
    (test #m-0.0 (sqrt #m-0.0))))

(test-group "negative takes the branch cut"
  (with-exact-comparison
    (test #m0 (real-part (sqrt #m-4)))
    (test #m2 (imag-part (sqrt #m-4)))
    (test 1.7320508075688772935274463415058723669428052538103806280558069794519330169088l0
          (imag-part (sqrt #m-3)))))

;; A non-square BigInteger or Rational root is a BigFloat at the default 256
;; bits, whatever the operand's width, so it survives write/read. Neither arm
;; goes through float64, where 10^400 overflows. A BigFloat wider than 256 bits
;; cannot be read, so the operand-precision rows are Go's (TestSqrtBigFloat).
;; racket and petite agree on the leading digits.
(test-group "unbounded exact operands"
  (with-exact-comparison
    (test "5.7735026918962576450914878050195745564760175127012687601860232648397767230293l+199"
          (number->string (sqrt (/ (+ 1 (expt 10 400)) 3))))
    (test 1l-200 (sqrt (/ 1 (+ 1 (expt 10 400)))))
    (test #m0 (real-part (sqrt (- 1 (expt 10 400)))))
    (test (sqrt (- (expt 10 400) 1)) (imag-part (sqrt (- 1 (expt 10 400)))))
    (test (sqrt 2/3) (imag-part (sqrt -2/3)))
    (let ((r (sqrt (+ 3 (expt 10 401)))))
      (test r (string->number (number->string r))))))

(test-group "non-finite"
  (with-exact-comparison
    (test #t (nan? (sqrt (/ #m0 #m0))))
    ;; still a BigFloat NaN, not a Float one
    (test (/ #m0 #m0) (sqrt (/ #m0 #m0)))
    (test (/ #m1 #m0) (sqrt (/ #m1 #m0)))
    ;; -inf takes the branch cut to 0+inf*i, not -inf
    (test #m0 (real-part (sqrt (/ #m-1 #m0))))
    (test (/ #m1 #m0) (imag-part (sqrt (/ #m-1 #m0))))))

(test-end)
(test-exit)
