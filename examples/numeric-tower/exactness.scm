;;; exactness.scm - Exact vs inexact arithmetic
;;;
;;; Demonstrates: Exactness preservation, contagion, conversions
;;; Wile-specific: Full exactness tracking across the numeric tower
;;;
;;; Usage: ./dist/scheme --file examples/numeric-tower/exactness.scm

;; R7RS distinguishes exact and inexact numbers.
;; Exact: integers, rationals (preserves mathematical precision)
;; Inexact: floats, approximate values (limited precision)

(display "=== Exactness in Wile ===\n")
(newline)

;; Exact numbers
(display "Exact numbers:\n")
(display "  42 exact? ")
(display (exact? 42))
(display "\n")
(display "  1/3 exact? ")
(display (exact? 1/3))
(display "\n")
(display "  -17 exact? ")
(display (exact? -17))
(display "\n\n")

;; Inexact numbers
(display "Inexact numbers:\n")
(display "  3.14 exact? ")
(display (exact? 3.14))
(display "\n")
(display "  1e10 exact? ")
(display (exact? 1e10))
(display "\n")
(display "  #i1/3 exact? ")
(display (exact? #i1/3))
(display "\n\n")

;; Exactness preservation
(display "Exact operations preserve exactness:\n")
(display "  (+ 1 2) = ")
(display (+ 1 2))
(display " exact? ")
(display (exact? (+ 1 2)))
(display "\n")
(display "  (* 3 4) = ")
(display (* 3 4))
(display " exact? ")
(display (exact? (* 3 4)))
(display "\n")
(display "  (/ 1 3) = ")
(display (/ 1 3))
(display " exact? ")
(display (exact? (/ 1 3)))
(display "\n\n")

;; Exactness contagion (R7RS §6.2.2)
;; If any operand is inexact, the result is inexact
(display "Contagion: inexact 'infects' the result\n")
(display "  (+ 1 2.0) = ")
(display (+ 1 2.0))
(display " exact? ")
(display (exact? (+ 1 2.0)))
(display "\n")
(display "  (* 10 0.1) = ")
(display (* 10 0.1))
(display " exact? ")
(display (exact? (* 10 0.1)))
(display "\n")
(display "  (/ 1 3.0) = ")
(display (/ 1 3.0))
(display " exact? ")
(display (exact? (/ 1 3.0)))
(display "\n\n")

;; Explicit conversions
(display "Explicit conversions:\n")
(display "  (exact->inexact 1/3) = ")
(display (exact->inexact 1/3))
(display "\n")
(display "  (inexact->exact 0.75) = ")
(display (inexact->exact 0.75))
(display "\n")
(display "  (inexact->exact 0.333) = ")
(display (inexact->exact 0.333))
(display " (closest rational)\n\n")

;; Why exactness matters: accumulation of error
(define (inexact-sum n)
  ;; Sum 0.1 n times (inexact)
  (define (iter i acc)
    (if (> i n)
        acc
        (iter (+ i 1) (+ acc 0.1))))
  (iter 1 0.0))

(define (exact-sum n)
  ;; Sum 1/10 n times (exact)
  (define (iter i acc)
    (if (> i n)
        acc
        (iter (+ i 1) (+ acc 1/10))))
  (iter 1 0))

(display "Accumulation of rounding errors:\n")
(display "  Inexact: (+ 0.1 0.1 ... 10 times) = ")
(display (inexact-sum 10))
(display "\n")
(display "  Exact:   (+ 1/10 1/10 ... 10 times) = ")
(display (exact-sum 10))
(display " = ")
(display (exact->inexact (exact-sum 10)))
(display "\n\n")

;; Practical example: financial calculations
;; Computing compound interest
(define (compound-interest-inexact principal rate years)
  ;; Using floats - accumulates rounding errors
  (define (iter n acc)
    (if (= n 0)
        acc
        (iter (- n 1) (* acc (+ 1.0 rate)))))
  (iter years principal))

(define (compound-interest-exact principal rate years)
  ;; Using exact rationals - no rounding errors
  (define (iter n acc)
    (if (= n 0)
        acc
        (iter (- n 1) (* acc (+ 1 rate)))))
  (iter years principal))

(display "Compound interest: $1000 at 5% for 10 years\n")
(let ((rate 1/20))  ; 5% = 1/20
  (display "  Inexact calculation: $")
  (display (compound-interest-inexact 1000.0 0.05 10))
  (display "\n")
  (display "  Exact calculation:   $")
  (display (exact->inexact (compound-interest-exact 1000 rate 10)))
  (display "\n")
  (display "  Exact (as rational): $")
  (display (compound-interest-exact 1000 rate 10))
  (display "\n"))
(newline)

;; When to use exact vs inexact
(display "Guidelines:\n")
(display "  Use EXACT when:\n")
(display "    - Precision is critical (financial, counting)\n")
(display "    - Need to represent fractions exactly\n")
(display "    - Symbolic computation\n")
(display "  Use INEXACT when:\n")
(display "    - Performance matters more than precision\n")
(display "    - Working with measured/approximate values\n")
(display "    - Transcendental functions (sqrt, sin, cos)\n")
