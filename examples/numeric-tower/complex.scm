;;; complex.scm - Complex number mathematics
;;;
;;; Demonstrates: Complex arithmetic, polar/rectangular forms, complex functions
;;; Wile-specific: Exact complex numbers (e.g., 1/2+1/3i)
;;;
;;; Usage: ./dist/scheme --file examples/numeric-tower/complex.scm

;; Complex numbers extend the real line to the complex plane.
;; Wile supports both exact and inexact complex numbers.

(display "=== Complex Numbers in Wile ===\n")
(newline)

;; Creating complex numbers
(display "Creating complex numbers:\n")
(display "  3+4i = ")
(display 3+4i)
(display "\n")
(display "  1-2i = ")
(display 1-2i)
(display "\n")
(display "  0+5i = ")
(display 0+5i)
(display " (pure imaginary)\n")
(display "  (make-rectangular 3 4) = ")
(display (make-rectangular 3 4))
(display "\n")
(display "  (make-polar 5 0.927) = ")
(display (make-polar 5 0.927))
(display "\n\n")

;; Exact complex numbers
(display "Exact complex (rational components):\n")
(let ((z (make-rectangular 1/2 1/3)))
  (display "  (make-rectangular 1/2 1/3) = ")
  (display z)
  (display "\n")
  (display "  exact? ")
  (display (exact? z))
  (display "\n"))
(newline)

;; Extracting components
(display "Rectangular form (x + yi):\n")
(let ((z 3+4i))
  (display "  z = ")
  (display z)
  (display "\n")
  (display "  real-part: ")
  (display (real-part z))
  (display "\n")
  (display "  imag-part: ")
  (display (imag-part z))
  (display "\n"))
(newline)

;; Polar form
(display "Polar form (r∠θ):\n")
(let ((z 3+4i))
  (display "  z = ")
  (display z)
  (display "\n")
  (display "  magnitude: ")
  (display (magnitude z))
  (display "\n")
  (display "  angle (radians): ")
  (display (angle z))
  (display "\n")
  (display "  angle (degrees): ")
  (display (* (angle z) (/ 180 3.141592653589793)))
  (display "°\n"))
(newline)

;; Complex arithmetic
(display "Complex arithmetic:\n")
(display "  (+ 3+4i 1+2i) = ")
(display (+ 3+4i 1+2i))
(display "\n")
(display "  (- 5+7i 2+3i) = ")
(display (- 5+7i 2+3i))
(display "\n")
(display "  (* 1+1i 1+1i) = ")
(display (* 1+1i 1+1i))
(display "\n")
(display "  (/ 1+0i 0+1i) = ")
(display (/ 1+0i 0+1i))
(display "\n\n")

;; Complex conjugate
(display "Complex conjugate:\n")
(define (conjugate z)
  (make-rectangular (real-part z) (- (imag-part z))))

(let ((z 3+4i))
  (display "  z = ")
  (display z)
  (display "\n")
  (display "  conjugate(z) = ")
  (display (conjugate z))
  (display "\n")
  (display "  z × conjugate(z) = ")
  (display (* z (conjugate z)))
  (display " (real, = |z|²)\n"))
(newline)

;; Euler's formula: e^(iθ) = cos(θ) + i·sin(θ)
(display "Euler's formula: e^(iπ) + 1 = 0\n")
(let* ((pi 3.141592653589793)
       (e-to-i-pi (make-polar 1 pi)))
  (display "  e^(iπ) = ")
  (display e-to-i-pi)
  (display "\n")
  (display "  e^(iπ) + 1 = ")
  (display (+ e-to-i-pi 1))
  (display " (≈ 0, within rounding error)\n"))
(newline)

;; Roots of unity
(display "Cube roots of unity (solutions to z³ = 1):\n")
(define (nth-root-of-unity n k)
  ;; k-th root of unity: e^(2πik/n)
  (let ((theta (* 2 3.141592653589793 (/ k n))))
    (make-polar 1 theta)))

(do ((k 0 (+ k 1)))
    ((>= k 3))
  (display "  ω")
  (display k)
  (display " = ")
  (display (nth-root-of-unity 3 k))
  (display "\n"))
(newline)

;; Mandelbrot set membership test
(define (mandelbrot? c max-iter)
  ;; Test if c is in the Mandelbrot set
  ;; Iterate z_{n+1} = z_n² + c starting from z_0 = 0
  (define (iter z n)
    (cond
     ((>= n max-iter) #t)  ; Didn't escape
     ((> (magnitude z) 2) #f)  ; Escaped
     (else (iter (+ (* z z) c) (+ n 1)))))
  (iter 0 0))

(display "Mandelbrot set membership (max 50 iterations):\n")
(display "  0+0i in set? ")
(display (mandelbrot? 0+0i 50))
(display "\n")
(display "  -1+0i in set? ")
(display (mandelbrot? -1+0i 50))
(display "\n")
(display "  0.25+0i in set? ")
(display (mandelbrot? 0.25+0i 50))
(display "\n")
(display "  1+1i in set? ")
(display (mandelbrot? 1+1i 50))
(display "\n\n")

;; Quadratic formula with complex roots
(define (solve-quadratic a b c)
  ;; Solve ax² + bx + c = 0
  (let* ((discriminant (- (* b b) (* 4 a c)))
         (sqrt-disc (sqrt discriminant))
         (denom (* 2 a)))
    (values (/ (+ (- b) sqrt-disc) denom)
            (/ (- (- b) sqrt-disc) denom))))

(display "Quadratic formula:\n")
(display "  x² - 2x + 5 = 0\n")
(call-with-values
 (lambda () (solve-quadratic 1 -2 5))
 (lambda (x1 x2)
   (display "    x₁ = ")
   (display x1)
   (display "\n")
   (display "    x₂ = ")
   (display x2)
   (display "\n")))
(newline)

;; De Moivre's theorem: (cos θ + i sin θ)^n = cos(nθ) + i sin(nθ)
(display "De Moivre's theorem: (1+i)⁴\n")
(let* ((z 1+1i)
       (z4 (* z z z z))
       (r (magnitude z))
       (theta (angle z)))
  (display "  Direct: (1+i)⁴ = ")
  (display z4)
  (display "\n")
  (display "  Polar: r⁴∠(4θ) = ")
  (display (make-polar (expt r 4) (* 4 theta)))
  (display "\n"))
