;;; mixed-arithmetic.scm - Cross-type numeric operations
;;;
;;; Demonstrates: R7RS numeric tower, type promotion, exactness contagion
;;; Wile-specific: Full tower with exact rationals, complex, arbitrary precision
;;;
;;; Usage: ./dist/scheme --file examples/numeric-tower/mixed-arithmetic.scm

;; R7RS requires seamless mixing of different numeric types.
;; Wile implements the full tower: Integer → Rational → Float → Complex
;; with automatic promotion and exactness tracking.

(display "=== Mixed-Type Arithmetic ===\n")
(newline)

;; Exact integer + exact integer = exact integer
(display "Exact + exact:\n")
(display "  (+ 1 2) = ")
(display (+ 1 2))
(display " (exact: ")
(display (exact? (+ 1 2)))
(display ")\n\n")

;; Exact integer + inexact float = inexact float (contagion)
(display "Exact + inexact:\n")
(display "  (+ 1 2.5) = ")
(display (+ 1 2.5))
(display " (exact: ")
(display (exact? (+ 1 2.5)))
(display ")\n\n")

;; Exact division produces exact rational
(display "Exact division:\n")
(display "  (/ 1 3) = ")
(display (/ 1 3))
(display " (exact: ")
(display (exact? (/ 1 3)))
(display ")\n")
(display "  (/ 22 7) = ")
(display (/ 22 7))
(display " (approximates π)\n\n")

;; Rational arithmetic preserves exactness
(display "Rational arithmetic:\n")
(display "  (+ 1/2 1/3) = ")
(display (+ 1/2 1/3))
(display "\n")
(display "  (* 2/3 3/4) = ")
(display (* 2/3 3/4))
(display "\n")
(display "  (/ (/ 1 2) (/ 1 3)) = ")
(display (/ (/ 1 2) (/ 1 3)))
(display "\n\n")

;; Complex numbers mix with reals
(display "Complex arithmetic:\n")
(display "  (+ 1 2+3i) = ")
(display (+ 1 2+3i))
(display "\n")
(display "  (* 2 1+1i) = ")
(display (* 2 1+1i))
(display "\n")
(display "  (magnitude 3+4i) = ")
(display (magnitude 3+4i))
(display "\n")
(display "  (angle 1+1i) = ")
(display (angle 1+1i))
(display " radians\n\n")

;; Exact complex numbers
(display "Exact complex:\n")
(display "  (+ 1/2 1/3i) = ")
(let ((z (make-rectangular 1/2 1/3)))
  (display z)
  (display " (exact: ")
  (display (exact? z))
  (display ")\n"))
(newline)

;; Type promotion chain
(display "Type promotion chain:\n")
(display "  Integer → Rational: (/ 1 2) = ")
(display (/ 1 2))
(display "\n")
(display "  Rational → Float:   (* 1/2 2.0) = ")
(display (* 1/2 2.0))
(display "\n")
(display "  Float → Complex:    (+ 1.5 2i) = ")
(display (+ 1.5 0+2i))
(display "\n\n")

;; Exactness contagion rules (R7RS §6.2.2)
(display "Exactness contagion:\n")
(display "  exact op exact = exact:     (+ 1 2) exact? ")
(display (exact? (+ 1 2)))
(display "\n")
(display "  exact op inexact = inexact: (+ 1 2.0) exact? ")
(display (exact? (+ 1 2.0)))
(display "\n")
(display "  inexact op exact = inexact: (+ 2.0 1) exact? ")
(display (exact? (+ 2.0 1)))
(display "\n")
(display "  inexact op inexact = inexact: (+ 2.0 3.0) exact? ")
(display (exact? (+ 2.0 3.0)))
(display "\n\n")

;; Practical example: solving quadratic equation
;; ax² + bx + c = 0
;; x = (-b ± sqrt(b² - 4ac)) / 2a
(define (quadratic a b c)
  (let* ((discriminant (- (* b b) (* 4 a c)))
         (sqrt-disc (sqrt discriminant))
         (denom (* 2 a)))
    (values (/ (+ (- b) sqrt-disc) denom)
            (/ (- (- b) sqrt-disc) denom))))

(display "Quadratic solver: x² - 5x + 6 = 0\n")
(call-with-values
 (lambda () (quadratic 1 -5 6))
 (lambda (x1 x2)
   (display "  x₁ = ")
   (display x1)
   (display "\n")
   (display "  x₂ = ")
   (display x2)
   (display "\n")))
(newline)

;; Complex roots when discriminant is negative
(display "Quadratic solver: x² + 1 = 0 (complex roots)\n")
(call-with-values
 (lambda () (quadratic 1 0 1))
 (lambda (x1 x2)
   (display "  x₁ = ")
   (display x1)
   (display "\n")
   (display "  x₂ = ")
   (display x2)
   (display "\n")))
(newline)

;; Demonstration: computing π using exact arithmetic
;; Leibniz formula: π/4 = 1 - 1/3 + 1/5 - 1/7 + ...
(define (pi-approximation terms)
  (define (iter k acc)
    (if (> k terms)
        acc
        (let ((sign (if (even? k) 1 -1))
              (denom (+ (* 2 k) 1)))
          (iter (+ k 1) (+ acc (/ sign denom))))))
  (* 4 (iter 0 0)))

(display "Computing π using exact rationals (Leibniz formula):\n")
(let ((approx-10 (pi-approximation 10))
      (approx-100 (pi-approximation 100)))
  (display "  10 terms:  ")
  (display approx-10)
  (display " (exact rational)\n")
  (display "  100 terms: ")
  (display approx-100)
  (display " (exact rational)\n")
  (display "  As float:  ")
  (display (exact->inexact approx-100))
  (display "\n"))
