;; quick-tour: (wile algebra matrix)
;;
;; Semiring-parameterized matrices. The library treats the coefficient
;; structure as a semiring (plus/times/zero/one) so the same matrix
;; types flow through reachability (boolean), shortest path (tropical),
;; counting paths (counting), and ordinary number arithmetic. Reach for
;; this when you want a single matrix implementation that serves many
;; algebras.

(import (scheme base) (wile algebra matrix) (wile algebra semiring))
(include "../lib/check.scm")

;; -- Counting semiring matrices (ordinary natural-number arithmetic) --

(define N (counting-semiring))
(define A (semiring-matrix-from-rows N '((1 2) (3 4))))
(define B (semiring-matrix-from-rows N '((5 6) (7 8))))

(check-true (semiring-matrix? A)                     "A is a semiring-matrix")
(check= (semiring-matrix-rows A) 2                   "2 rows")
(check= (semiring-matrix-cols A) 2                   "2 cols")
(check= (semiring-matrix-ref A 0 1)  2               "A[0][1] = 2")

;; -- Addition and multiplication ----------------------------------

(check= (semiring-matrix->rows (semiring-matrix-add A B))
        '((6 8) (10 12))
        "matrix addition")

;; 2x2 matrix multiplication: (1*5 + 2*7)=19 ; (1*6+2*8)=22 ;
;;                            (3*5 + 4*7)=43 ; (3*6+4*8)=50
(check= (semiring-matrix->rows (semiring-matrix-mul A B))
        '((19 22) (43 50))
        "matrix multiplication")

;; -- Identity matrix is left- and right-identity -----------------

(define I2 (semiring-matrix-identity N 2))
(check= (semiring-matrix->rows (semiring-matrix-mul A I2))
        (semiring-matrix->rows A)
        "A * I = A")
(check= (semiring-matrix->rows (semiring-matrix-mul I2 A))
        (semiring-matrix->rows A)
        "I * A = A")

;; -- Power ---------------------------------------------------------

;; A^2 = A * A
(check= (semiring-matrix->rows (semiring-matrix-power A 2))
        (semiring-matrix->rows (semiring-matrix-mul A A))
        "A^2 = A * A")

;; -- Boolean semiring gives reachability matrix -------------------

(define Bsem (boolean-semiring))
;; Adjacency of the directed 3-cycle A -> B -> C -> A.
(define adj
  (semiring-matrix-from-rows Bsem
    '((#f #t #f)
      (#f #f #t)
      (#t #f #f))))

;; adj tells which vertices are reachable in exactly 1 step.
;; adj^3 tells which vertices are reachable in exactly 3 steps -- since
;; the 3-cycle has period 3, every vertex reaches itself in exactly 3
;; steps, and no other vertex. So adj^3 = identity.
(define reach3 (semiring-matrix-power adj 3))
(check-true (semiring-matrix? reach3)                "reachability power computed")
(check= (semiring-matrix->rows reach3)
        '((#t #f #f) (#f #t #f) (#f #f #t))
        "adj^3 on a 3-cycle is the identity matrix (period 3)")

(display "matrix tour complete") (newline)
