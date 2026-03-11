;;; big-numbers.scm - Arbitrary precision arithmetic
;;;
;;; Demonstrates: BigInteger, BigFloat, precision beyond native types
;;; Wile-specific: Automatic promotion to arbitrary precision
;;;
;;; Usage: ./dist/wile --file examples/numeric-tower/big-numbers.scm

;; Wile automatically promotes to arbitrary precision when needed.
;; No overflow errors - numbers grow as large as memory allows.

(display "=== Arbitrary Precision Numbers ===\n")
(newline)

;; Large integers
(display "Large integers (BigInteger):\n")
(display "  2^100 = ")
(display (expt 2 100))
(display "\n")
(display "  10^50 = ")
(display (expt 10 50))
(display "\n")
(display "  100! (factorial) = ")

(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(display (factorial 100))
(display "\n\n")

;; No overflow
(display "No integer overflow:\n")
(display "  2^1000 (too large for any fixed-width int):\n  ")
(display (expt 2 1000))
(display "\n\n")

;; Fibonacci with large numbers
(display "Fibonacci numbers (exact, arbitrarily large):\n")

(define (fib n)
  (define (iter a b count)
    (if (= count 0)
        b
        (iter (+ a b) a (- count 1))))
  (iter 1 0 n))

(display "  fib(100) = ")
(display (fib 100))
(display "\n")
(display "  fib(200) = ")
(display (fib 200))
(display "\n")
(display "  fib(500) (104 digits):\n  ")
(display (fib 500))
(display "\n\n")

;; Large exact arithmetic
(display "Large exact arithmetic:\n")
(let ((a (expt 2 100))
      (b (expt 3 100)))
  (display "  2^100 + 3^100 = ")
  (display (+ a b))
  (display "\n")
  (display "  2^100 × 3^100 = 6^100 = ")
  (display (* a b))
  (display "\n"))
(newline)

;; Catalan numbers: C_n = (2n)! / ((n+1)! × n!)
(define (catalan n)
  (/ (factorial (* 2 n))
     (* (factorial (+ n 1)) (factorial n))))

(display "Catalan numbers (exact, arbitrarily large):\n")
(display "  C₁₀ = ")
(display (catalan 10))
(display "\n")
(display "  C₂₀ = ")
(display (catalan 20))
(display "\n")
(display "  C₅₀ = ")
(display (catalan 50))
(display "\n\n")

;; Prime testing with large numbers
(define (prime? n)
  ;; Trial division (simple but exact for large numbers)
  (define (divides? d n)
    (= (remainder n d) 0))
  (define (iter d)
    (cond
     ((> (* d d) n) #t)
     ((divides? d n) #f)
     (else (iter (+ d 2)))))
  (cond
   ((< n 2) #f)
   ((= n 2) #t)
   ((divides? 2 n) #f)
   (else (iter 3))))

(display "Prime testing with large numbers:\n")
(let ((p (- (expt 2 31) 1)))  ; Mersenne prime 2^31 - 1
  (display "  2^31 - 1 = ")
  (display p)
  (display "\n")
  (display "  Is prime? ")
  (display (prime? p))
  (display "\n"))
(newline)

;; Greatest common divisor with large numbers
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(display "GCD with large numbers:\n")
(let ((a (expt 2 100))
      (b (expt 2 50)))
  (display "  gcd(2^100, 2^50) = ")
  (display (gcd a b))
  (display " = 2^50\n"))

(let ((a (factorial 100))
      (b (factorial 99)))
  (display "  gcd(100!, 99!) = ")
  (display (gcd a b))
  (display " = 99!\n"))
(newline)

;; Exact powers
(display "Exact powers (no approximation):\n")
(display "  (expt 2 256) = ")
(display (expt 2 256))
(display "\n")
(display "  (expt 10 100) (googol) = ")
(display (expt 10 100))
(display "\n\n")

;; Large rationals
(display "Large exact rationals:\n")
(let ((r (/ (factorial 50) (expt 2 100))))
  (display "  50! / 2^100 = ")
  (display r)
  (display "\n")
  (display "  As decimal: ")
  (display (exact->inexact r))
  (display "\n"))
(newline)

;; Computing π using Machin's formula (exact up to a point)
;; π/4 = 4·arctan(1/5) - arctan(1/239)
;; arctan(x) = x - x³/3 + x⁵/5 - x⁷/7 + ...

(define (arctan-series x terms)
  ;; Taylor series for arctan
  (define (iter k acc)
    (if (> k terms)
        acc
        (let* ((power (* 2 k))
               (x-pow (expt x (+ power 1)))
               (sign (if (even? k) 1 -1))
               (term (/ (* sign x-pow) (+ power 1))))
          (iter (+ k 1) (+ acc term)))))
  (iter 0 0))

(display "Approximating π using exact arithmetic (Machin's formula):\n")
(let* ((atan-1/5 (arctan-series 1/5 20))
       (atan-1/239 (arctan-series 1/239 20))
       (pi/4 (- (* 4 atan-1/5) atan-1/239))
       (pi-approx (* 4 pi/4)))
  (display "  π ≈ ")
  (display pi-approx)
  (display " (exact rational)\n")
  (display "  As decimal: ")
  (display (exact->inexact pi-approx))
  (display "\n"))
(newline)

;; Numerical integration with exact arithmetic
(define (integrate-simpson f a b n)
  ;; Simpson's rule with exact arithmetic
  (let* ((h (/ (- b a) n))
         (sum-odds 0)
         (sum-evens 0))
    (do ((i 1 (+ i 1)))
        ((>= i n))
      (let ((x (+ a (* i h))))
        (if (odd? i)
            (set! sum-odds (+ sum-odds (f x)))
            (set! sum-evens (+ sum-evens (f x))))))
    (* (/ h 3)
       (+ (f a)
          (* 4 sum-odds)
          (* 2 sum-evens)
          (f b)))))

(display "Numerical integration with exact arithmetic:\n")
(display "  ∫₀¹ x² dx using Simpson's rule (10 intervals):\n")
(let ((result (integrate-simpson (lambda (x) (* x x)) 0 1 10)))
  (display "    Exact: ")
  (display result)
  (display "\n")
  (display "    Expected: 1/3 = ")
  (display 1/3)
  (display "\n")
  (display "    Match? ")
  (display (= result 1/3))
  (display "\n"))
