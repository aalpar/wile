;;; rationals.scm - Rational number operations
;;;
;;; Demonstrates: Exact fractions, rational arithmetic, automatic simplification
;;; Wile-specific: Full rational support throughout the numeric tower
;;;
;;; Usage: ./dist/scheme --file examples/numeric-tower/rationals.scm

;; Wile represents rational numbers as exact fractions.
;; Rationals are automatically reduced to lowest terms.

(display "=== Rational Numbers in Wile ===\n")
(newline)

;; Creating rationals
(display "Creating rationals:\n")
(display "  (/ 1 2) = ")
(display (/ 1 2))
(display "\n")
(display "  (/ 22 7) = ")
(display (/ 22 7))
(display " (approximates π)\n")
(display "  (/ 355 113) = ")
(display (/ 355 113))
(display " (better π approximation)\n\n")

;; Automatic simplification
(display "Automatic simplification:\n")
(display "  (/ 6 8) = ")
(display (/ 6 8))
(display " (reduced to lowest terms)\n")
(display "  (/ 100 150) = ")
(display (/ 100 150))
(display "\n")
(display "  (/ 17 1) = ")
(display (/ 17 1))
(display " (becomes integer)\n\n")

;; Rational arithmetic
(display "Rational arithmetic:\n")
(display "  (+ 1/2 1/3) = ")
(display (+ 1/2 1/3))
(display "\n")
(display "  (- 3/4 1/2) = ")
(display (- 3/4 1/2))
(display "\n")
(display "  (* 2/3 3/4) = ")
(display (* 2/3 3/4))
(display "\n")
(display "  (/ 1/2 1/3) = ")
(display (/ 1/2 1/3))
(display "\n\n")

;; Mixed rational and integer
(display "Mixing rationals and integers:\n")
(display "  (+ 1 1/2) = ")
(display (+ 1 1/2))
(display "\n")
(display "  (* 3 2/5) = ")
(display (* 3 2/5))
(display "\n")
(display "  (/ 7 2) = ")
(display (/ 7 2))
(display "\n\n")

;; Extracting numerator and denominator
(display "Numerator and denominator:\n")
(let ((r 22/7))
  (display "  Rational: ")
  (display r)
  (display "\n")
  (display "  Numerator: ")
  (display (numerator r))
  (display "\n")
  (display "  Denominator: ")
  (display (denominator r))
  (display "\n"))
(newline)

;; Comparison
(display "Comparing rationals:\n")
(display "  (< 1/3 1/2) = ")
(display (< 1/3 1/2))
(display "\n")
(display "  (> 3/4 2/3) = ")
(display (> 3/4 2/3))
(display "\n")
(display "  (= 2/4 1/2) = ")
(display (= 2/4 1/2))
(display " (simplified forms match)\n\n")

;; Practical example: Egyptian fractions
;; Represent a fraction as a sum of unit fractions (1/n)
(define (greedy-egyptian num den)
  ;; Greedy algorithm for Egyptian fraction decomposition
  (define (iter n d result)
    (if (= n 0)
        (reverse result)
        (let ((unit-den (ceiling (/ d n))))
          (iter (- n (/ d unit-den))
                d
                (cons (/ 1 unit-den) result)))))
  (iter num den '()))

(display "Egyptian fraction decomposition:\n")
(display "  2/3 = ")
(display (greedy-egyptian 2 3))
(display "\n")
(display "  5/6 = ")
(display (greedy-egyptian 5 6))
(display "\n")
(display "  3/4 = ")
(display (greedy-egyptian 3 4))
(display "\n\n")

;; Practical example: Farey sequence
;; All reduced fractions between 0 and 1 with denominator ≤ n
;; Uses the Farey neighbor algorithm to generate in sorted order
(define (farey-sequence n)
  (define (build-sequence)
    (let loop ((a 0) (b 1)     ; Current fraction a/b
               (c 1) (d n)     ; Next fraction c/d (starts at 1/n)
               (result '()))
      (let ((new-result (cons (/ a b) result)))
        (if (and (= a 1) (= b 1))
            (reverse new-result)
            (let ((k (quotient (+ n b) d)))  ; Mediant coefficient
              (loop c d
                    (- (* k c) a)
                    (- (* k d) b)
                    new-result))))))
  (build-sequence))

(display "Farey sequence F₅ (all fractions with denominator ≤ 5):\n")
(display "  ")
(display (farey-sequence 5))
(display "\n\n")

;; Harmonic series (partial sums)
;; H_n = 1 + 1/2 + 1/3 + ... + 1/n
(define (harmonic n)
  (define (iter k acc)
    (if (> k n)
        acc
        (iter (+ k 1) (+ acc (/ 1 k)))))
  (iter 1 0))

(display "Harmonic series (exact):\n")
(display "  H₅  = 1 + 1/2 + 1/3 + 1/4 + 1/5 = ")
(display (harmonic 5))
(display "\n")
(display "  H₁₀ = ")
(display (harmonic 10))
(display "\n")
(display "  As decimal: ")
(display (exact->inexact (harmonic 10)))
(display "\n\n")

;; Converting to continued fractions
(define (continued-fraction num den max-terms)
  ;; Represent num/den as [a₀; a₁, a₂, ...]
  (define (iter n d terms)
    (if (or (= d 0) (>= (length terms) max-terms))
        (reverse terms)
        (let ((q (quotient n d))
              (r (remainder n d)))
          (iter d r (cons q terms)))))
  (iter num den '()))

(display "Continued fractions:\n")
(display "  22/7 = ")
(display (continued-fraction 22 7 10))
(display "\n")
(display "  355/113 = ")
(display (continued-fraction 355 113 10))
(display "\n")
(display "  100/37 = ")
(display (continued-fraction 100 37 10))
(display "\n")
