;;; numbers-test.scm - R7RS 6.2 Numbers: extended coverage
;;;
;;; Test cases extracted from Go test suite:
;;;   - extensions/math/prim_math_test.go
;;;   - registry/core/prim_arithmetic_test.go
;;;   - registry/core/prim_numeric_predicate_test.go
;;;
;;; Complements the canonical R7RS tests in integration/testdata/r7rs-tests.scm.
;;; Does not duplicate tests already present there.

(import (scheme base)
        (scheme inexact)
        (scheme complex)
        (chibi test))

(test-begin "numbers")

;; ── Transcendentals ─────────────────────────────────────────────────

(test-group "exp"
  (test #t (= (exp 0) 1.0))
  (test #t (< (abs (- (exp 1) 2.718281828459045)) 1e-10))
  (test #t (< (abs (- (exp -1) 0.36787944117144233)) 1e-10)))

(test-group "log"
  (test #t (< (abs (log 1)) 1e-10))
  (test #t (< (abs (- (log 2.718281828459045) 1.0)) 1e-10))
  (test #t (< (abs (- (log 8 2) 3.0)) 1e-10)))

(test-group "sin"
  (test #t (< (abs (sin 0)) 1e-10))
  (test #t (< (abs (- (sin 1.5707963267948966) 1.0)) 1e-10)))

(test-group "cos"
  (test #t (< (abs (- (cos 0) 1.0)) 1e-10))
  (test #t (< (abs (- (cos 3.141592653589793) -1.0)) 1e-10)))

(test-group "tan"
  (test #t (< (abs (tan 0)) 1e-10))
  (test #t (< (abs (- (tan 0.7853981633974483) 1.0)) 1e-10)))

(test-group "asin"
  (test #t (< (abs (asin 0)) 1e-10))
  (test #t (< (abs (- (asin 1) 1.5707963267948966)) 1e-10)))

(test-group "acos"
  (test #t (< (abs (acos 1)) 1e-10))
  (test #t (< (abs (- (acos 0) 1.5707963267948966)) 1e-10)))

(test-group "atan single arg"
  (test #t (< (abs (atan 0)) 1e-10))
  (test #t (< (abs (- (atan 1) 0.7853981633974483)) 1e-10)))

(test-group "atan two args"
  (test #t (< (abs (- (atan 1 1) 0.7853981633974483)) 1e-10))
  (test #t (< (abs (- (atan 1 0) 1.5707963267948966)) 1e-10)))

(test-group "sqrt"
  (test #t (< (abs (- (sqrt 4) 2.0)) 1e-10))
  (test #t (< (abs (sqrt 0)) 1e-10))
  (test #t (< (abs (- (sqrt 2.0) 1.4142135623730951)) 1e-10))
  ;; sqrt of negative reals produces complex
  (test #t (< (abs (real-part (sqrt -1))) 1e-10))
  (test #t (< (abs (- (imag-part (sqrt -1)) 1.0)) 1e-10))
  (test #t (< (abs (- (imag-part (sqrt -4)) 2.0)) 1e-10)))

;; ── Exponentiation ──────────────────────────────────────────────────

(test-group "expt"
  (test 1024 (expt 2 10))
  (test 1 (expt 5 0))
  (test 1 (expt 0 0))
  (test 1/2 (expt 2 -1))
  (test 9/4 (expt 3/2 2))
  (test #t (< (abs (- (expt 2.0 0.5) 1.4142135623730951)) 1e-10))
  (test 18446744073709551616 (expt 2 64)))

(test-group "square"
  (test 25 (square 5))
  (test 9 (square -3))
  (test 0 (square 0))
  (test 1/9 (square 1/3))
  (test #t (< (abs (- (square 1.5) 2.25)) 1e-10)))

;; ── Rounding ────────────────────────────────────────────────────────

(test-group "floor"
  (test 3 (floor 3))
  (test 3.0 (floor 3.7))
  (test -4.0 (floor -3.7))
  (test 3 (floor 7/2))
  (test #t (exact? (floor 7/2))))

(test-group "ceiling"
  (test 3 (ceiling 3))
  (test 4.0 (ceiling 3.2))
  (test -3.0 (ceiling -3.7))
  (test 4 (ceiling 7/2)))

(test-group "truncate"
  (test 3.0 (truncate 3.7))
  (test -3.0 (truncate -3.7))
  (test 3 (truncate 7/2)))

(test-group "round"
  ;; R7RS banker's rounding (round to even)
  (test 4.0 (round 3.5))
  (test 4.0 (round 4.5))
  (test 0.0 (round 0.5))
  (test 4.0 (round 3.7))
  (test 3.0 (round 3.2))
  (test -4.0 (round -3.5))
  (test 4 (round 7/2)))

(test-group "rounding inexactness preservation"
  (test #t (inexact? (floor 3.7))))

;; ── Integer division ────────────────────────────────────────────────

(test-group "floor/ multi-value"
  (test '(3 1)
    (call-with-values (lambda () (floor/ 10 3)) list))
  (test '(-4 2)
    (call-with-values (lambda () (floor/ -10 3)) list))
  (test '(-4 -2)
    (call-with-values (lambda () (floor/ 10 -3)) list))
  (test #t
    (equal? (call-with-values (lambda () (floor/ 10.0 3)) list)
            '(3.0 1.0))))

(test-group "floor-quotient"
  (test 3 (floor-quotient 10 3))
  (test -4 (floor-quotient -10 3))
  (test #t (exact? (floor-quotient 10 3))))

(test-group "floor-remainder"
  (test 1 (floor-remainder 10 3))
  (test 2 (floor-remainder -10 3))
  (test -2 (floor-remainder 10 -3)))

(test-group "truncate/ multi-value"
  (test '(3 1)
    (call-with-values (lambda () (truncate/ 10 3)) list))
  (test '(-3 -1)
    (call-with-values (lambda () (truncate/ -10 3)) list))
  (test '(-3 1)
    (call-with-values (lambda () (truncate/ 10 -3)) list)))

(test-group "truncate-quotient"
  (test 3 (truncate-quotient 10 3))
  (test -3 (truncate-quotient -10 3)))

(test-group "truncate-remainder"
  (test 1 (truncate-remainder 10 3))
  (test -1 (truncate-remainder -10 3)))

(test-group "integer division inexact paths"
  (test 3.0 (floor-quotient 10.0 3))
  (test 1.0 (floor-remainder 10.0 3))
  (test 3.0 (truncate-quotient 10.0 3))
  (test 1.0 (truncate-remainder 10.0 3)))

;; ── Numeric predicates (finite?, infinite?, nan?) ───────────────────

(test-group "finite?"
  (test #t (finite? 42))
  (test #t (finite? 3.14))
  (test #t (finite? 3/4))
  (test #f (finite? +inf.0))
  (test #f (finite? -inf.0))
  (test #f (finite? +nan.0)))

(test-group "infinite?"
  (test #t (infinite? +inf.0))
  (test #t (infinite? -inf.0))
  (test #f (infinite? 42))
  (test #f (infinite? +nan.0)))

(test-group "nan?"
  (test #t (nan? +nan.0))
  (test #f (nan? 42))
  (test #f (nan? +inf.0))
  (test #f (nan? 3.14)))

;; ── Numeric predicates (zero?, odd?, even?) extended ────────────────

(test-group "zero? extended"
  (test #t (zero? 0))
  (test #f (zero? 5))
  (test #f (zero? -5))
  (test #t (zero? 0.0))
  (test #f (zero? 3.14))
  (test #t (zero? 0/1))
  (test #f (zero? 1/2)))

(test-group "odd? extended"
  (test #t (odd? 1))
  (test #f (odd? 2))
  (test #f (odd? 0))
  (test #t (odd? -1))
  (test #f (odd? -2))
  (test #t (odd? 999999999))
  (test #f (odd? 1000000000))
  ;; inexact integer
  (test #t (odd? 3.0))
  (test #f (odd? 4.0)))

(test-group "even? extended"
  (test #t (even? 0))
  (test #t (even? 2))
  (test #f (even? 1))
  (test #t (even? -2))
  (test #f (even? -1))
  (test #t (even? 1000000000))
  (test #f (even? 999999999))
  ;; inexact integer
  (test #t (even? 4.0))
  (test #f (even? 3.0)))

(test-group "positive?/negative? with special values"
  (test #t (positive? +inf.0))
  (test #f (positive? -inf.0))
  (test #f (negative? +inf.0))
  (test #t (negative? -inf.0)))

;; ── Rational operations ─────────────────────────────────────────────

(test-group "numerator"
  (test 3 (numerator 3/5))
  (test 7 (numerator 7))
  (test -3 (numerator -3/5))
  (test 1.0 (numerator 0.5)))

(test-group "denominator"
  (test 5 (denominator 3/5))
  (test 1 (denominator 7))
  (test 2.0 (denominator 0.5)))

(test-group "rationalize"
  (test 1/3 (rationalize 3/10 1/10))
  (test 1/3 (rationalize 1/3 0))
  (test 0 (rationalize 0 1/10))
  (test #t (inexact? (rationalize 0.5 1/10)))
  (test #t (inexact? (rationalize 1/3 0.1)))
  (test -1 (rationalize -1 1/10)))

(test-group "exact-integer-sqrt"
  (test '(3 5)
    (call-with-values (lambda () (exact-integer-sqrt 14)) list))
  (test '(2 0)
    (call-with-values (lambda () (exact-integer-sqrt 4)) list))
  (test '(0 0)
    (call-with-values (lambda () (exact-integer-sqrt 0)) list))
  (test '(10 0)
    (call-with-values (lambda () (exact-integer-sqrt 100)) list)))

;; ── Complex operations ──────────────────────────────────────────────

(test-group "make-rectangular"
  (test 3.0 (real-part (make-rectangular 3.0 4.0)))
  (test 4.0 (imag-part (make-rectangular 3.0 4.0)))
  (test 3 (make-rectangular 3 0)))

(test-group "make-polar"
  (test #t (< (abs (- (real-part (make-polar 1 0)) 1.0)) 1e-10))
  (test #t (< (abs (imag-part (make-polar 1 0))) 1e-10))
  (test #t (< (abs (- (magnitude (make-polar 5 1.0)) 5.0)) 1e-10)))

(test-group "real-part on reals"
  (test 5.0 (real-part 5))
  (test 3.14 (real-part 3.14)))

(test-group "imag-part on reals"
  (test 0.0 (imag-part 5))
  (test 0.0 (imag-part 3.14)))

(test-group "magnitude"
  (test #t (< (abs (- (magnitude (make-rectangular 3.0 4.0)) 5.0)) 1e-10))
  (test 5.0 (magnitude 5))
  (test 5.0 (magnitude -5))
  (test 0.0 (magnitude 0))
  ;; float
  (test 3.14 (magnitude 3.14))
  (test 3.14 (magnitude -3.14))
  ;; rational
  (test #t (< (abs (- (magnitude 3/4) 0.75)) 1e-10))
  (test #t (< (abs (- (magnitude -3/4) 0.75)) 1e-10))
  ;; biginteger
  (test #t (> (magnitude (expt 2 100)) 0))
  ;; exact integer complex
  (test #t (< (abs (- (magnitude (make-rectangular 3 4)) 5.0)) 1e-10)))

(test-group "angle"
  (test 0.0 (angle 1))
  (test #t (< (abs (- (angle -1) 3.141592653589793)) 1e-10))
  (test #t (< (abs (- (angle (make-rectangular 0.0 1.0)) 1.5707963267948966)) 1e-10))
  ;; float
  (test 0.0 (angle 3.14))
  (test #t (< (abs (- (angle -3.14) 3.141592653589793)) 1e-10))
  ;; rational
  (test 0.0 (angle 3/4))
  (test #t (< (abs (- (angle -3/4) 3.141592653589793)) 1e-10))
  ;; biginteger
  (test #t (>= (magnitude (angle (expt 2 100))) 0))
  (test #t (> (angle (- (expt 2 100))) 3.0))
  ;; exact integer complex
  (test #t (> (angle (make-rectangular 3 4)) 0)))

;; ── make-rectangular with exact types ───────────────────────────────

(test-group "make-rectangular exact types"
  (test 3 (real-part (make-rectangular 3 4)))
  (test 4 (imag-part (make-rectangular 3 4)))
  ;; biginteger parts
  (test #t (> (real-part (make-rectangular (expt 2 100) 1)) 0))
  ;; rational parts
  (test #t (< (abs (- (real-part (make-rectangular 3/4 1/2)) 0.75)) 1e-10)))

;; ── number->string ──────────────────────────────────────────────────

(test-group "number->string"
  (test "42" (number->string 42))
  (test "-42" (number->string -42))
  (test "0" (number->string 0))
  (test "ff" (number->string 255 16))
  (test "111" (number->string 7 2))
  (test "10" (number->string 8 8))
  (test "1.5" (number->string 1.5))
  (test "1.0" (number->string 1.0))
  (test "+inf.0" (number->string +inf.0))
  (test "-inf.0" (number->string -inf.0))
  (test "+nan.0" (number->string +nan.0))
  (test "3/5" (number->string 3/5))
  ;; complex and biginteger produce strings
  (test #t (string? (number->string (make-rectangular 3.0 4.0))))
  (test #t (string? (number->string (expt 2 100))))
  ;; scientific notation
  (test "5.0e-324" (number->string 5e-324)))

;; ── string->number ──────────────────────────────────────────────────

(test-group "string->number"
  (test 42 (string->number "42"))
  (test -42 (string->number "-42"))
  (test 1.5 (string->number "1.5"))
  (test 100.0 (string->number "1e2"))
  ;; radix argument
  (test 255 (string->number "ff" 16))
  (test 7 (string->number "111" 2))
  (test 8 (string->number "10" 8))
  ;; rational
  (test 3/5 (string->number "3/5"))
  ;; prefix directives
  (test 255 (string->number "#xff"))
  (test 7 (string->number "#b111"))
  (test 3/2 (string->number "#e1.5"))
  (test 42.0 (string->number "#i42"))
  (test 8 (string->number "#o10"))
  (test 42 (string->number "#d42"))
  (test #f (string->number "#z42"))
  ;; exactness conversions
  (test 42 (string->number "#e42"))
  (test 1 (string->number "#e1.0"))
  (test #t (inexact? (string->number "#i99999999999999999999999")))
  ;; invalid input
  (test #f (string->number "hello"))
  (test #f (string->number "")))

(test-group "string->number #i prefix"
  (test #t (< (abs (- (string->number "#i3/5") 0.6)) 1e-10))
  (test #t (inexact? (string->number "#i3/5")))
  (test 1.5 (string->number "#i1.5")))

;; ── BigInteger precision ────────────────────────────────────────────

(test-group "expt biginteger precision"
  (test #t (exact? (expt 2 1000)))
  (test #t (exact? (expt 2 100)))
  (test #t (exact? (expt 10 50)))
  (test 1024 (expt 2 10))
  (test 1000 (expt 10 3))
  ;; composition preserves exactness
  (test #t (exact? (expt (expt 2 500) 2)))
  (test #t (= (expt (expt 2 500) 2) (expt 2 1000)))
  ;; negative integer exponents produce exact rationals
  (test 1/2 (expt 2 -1))
  (test #t (exact? (expt 2 -1)))
  ;; fractional exponents produce inexact
  (test #t (inexact? (expt 2 0.5)))
  ;; large base stays exact
  (test #t (exact? (expt (expt 10 20) 2))))

(test-group "expt additional cases"
  ;; biginteger base, negative exponent produces rational
  (test #t (rational? (expt (expt 2 100) -1)))
  (test #t (> (expt (expt 2 100) -1) 0))
  ;; biginteger base, positive exponent stays exact
  (test #t (exact? (expt (expt 2 100) 3)))
  ;; rational base, negative exponent
  (test 2/3 (expt 3/2 -1))
  (test 3 (expt 1/3 -1))
  (test 3/2 (expt 2/3 -1)))

;; ── Rational to inexact precision ───────────────────────────────────

(test-group "rational to inexact precision"
  (test #t (> (inexact (/ (expt 2 100) 3)) 1e29))
  (test #t (> (inexact (/ (expt 10 50) 7)) 1e48))
  (test #t (< (abs (- (inexact (/ 1 3)) 0.333333)) 0.001))
  (test 0.5 (inexact (/ 1 2)))
  (test #t (inexact? (inexact (/ 1 3)))))

;; ── Complex sqrt branch cuts ────────────────────────────────────────

(test-group "complex sqrt branch cuts"
  ;; R7RS: sqrt of negative real with zero imaginary produces positive imaginary
  (test #t (> (imag-part (sqrt (make-rectangular -1.0 0.0))) 0))
  ;; sqrt of non-negative real complex
  (test #t (< (abs (- (real-part (sqrt (make-rectangular 4.0 0.0))) 2.0)) 1e-10))
  ;; general complex sqrt
  (test #t (> (real-part (sqrt (make-rectangular 0.0 1.0))) 0))
  ;; bigcomplex sqrt
  (test #t (> (magnitude (sqrt (make-rectangular -4 1))) 0)))

;; ── exact-integer-sqrt with bigintegers ─────────────────────────────

(test-group "exact-integer-sqrt biginteger"
  (test #t
    (equal? (call-with-values
              (lambda () (exact-integer-sqrt (expt 2 100)))
              list)
            (list (expt 2 50) 0)))
  (test #t
    (let-values (((s r) (exact-integer-sqrt (+ (expt 2 100) 1))))
      (= r 1))))

;; ── Error conditions ────────────────────────────────────────────────

(test-group "type errors"
  (test-error (exp "hello"))
  (test-error (sin "hello"))
  (test-error (sqrt "hello"))
  (test-error (expt "hello" 2))
  (test-error (expt 2 "hello"))
  (test-error (square "hello"))
  (test-error (floor "hello"))
  (test-error (ceiling "hello"))
  (test-error (finite? "hello"))
  (test-error (infinite? "hello"))
  (test-error (nan? "hello"))
  (test-error (numerator "hello"))
  (test-error (denominator "hello"))
  (test-error (real-part "hello"))
  (test-error (imag-part "hello"))
  (test-error (magnitude "hello"))
  (test-error (angle "hello"))
  (test-error (number->string "hello"))
  (test-error (string->number 42)))

(test-group "domain errors"
  (test-error (exact-integer-sqrt -1))
  (test-error (exact-integer-sqrt 1.5))
  (test-error (numerator +inf.0))
  (test-error (denominator +nan.0))
  ;; negative biginteger for exact-integer-sqrt
  (test-error (exact-integer-sqrt (- (expt 2 100)))))

(test-group "division by zero"
  (test-error (floor/ 10 0))
  (test-error (floor-quotient 10 0))
  (test-error (floor-remainder 10 0))
  (test-error (truncate/ 10 0))
  (test-error (truncate-quotient 10 0))
  (test-error (truncate-remainder 10 0)))

(test-group "invalid radix"
  (test-error (number->string 42 3)))

(test-group "make-rectangular complex args"
  (test-error (make-rectangular (make-rectangular 1.0 1.0) 0.0))
  (test-error (make-rectangular (make-rectangular 1 1) 0)))

(test-group "odd?/even? errors"
  (test-error (odd? 3.5))
  (test-error (odd? 1/2))
  (test-error (even? 3.5))
  (test-error (even? 1/2)))

;; ── Numeric comparison extended ─────────────────────────────────────

(test-group "numeric comparison with NaN"
  ;; NaN is never equal to anything
  (test #f (= +nan.0 +nan.0))
  (test #f (< 1 +nan.0))
  (test #f (< +nan.0 1))
  (test #f (> 1 +nan.0))
  (test #f (> +nan.0 1)))

(test-group "numeric comparison with infinity"
  (test #t (= +inf.0 +inf.0))
  (test #t (= -inf.0 -inf.0))
  (test #f (= +inf.0 -inf.0))
  (test #t (< -inf.0 0))
  (test #t (< 0 +inf.0))
  (test #t (< -inf.0 +inf.0)))

(test-group "comparison type errors"
  (test-error (= "hello" 1))
  (test-error (< "hello" 1))
  (test-error (> "hello" 1))
  (test-error (<= "hello" 1))
  (test-error (>= "hello" 1)))

(test-group "ordering rejects non-real complex"
  (test-error (< 1+1i 2))
  (test-error (> 1+1i 2))
  (test-error (<= 1+1i 2))
  (test-error (>= 1+1i 2))
  ;; = accepts complex
  (test #t (= 1+1i 1+1i)))

;; ── Overflow promotion ──────────────────────────────────────────────

(test-group "integer overflow promotion"
  (test #t (exact? (+ 9223372036854775807 1)))
  (test #t (integer? (+ 9223372036854775807 1)))
  (test #t (= (+ 9223372036854775807 1) 9223372036854775808))
  (test #t (= (- -9223372036854775808 1) -9223372036854775809))
  (test #t (= (* 9223372036854775807 2) 18446744073709551614)))

(test-end)
