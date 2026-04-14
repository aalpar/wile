;;; getting-started.scm - First steps with algebraic structures
;;;
;;; Demonstrates: monoids, fold, power, validation, with-monoid
;;;
;;; Usage: ./dist/wile --file examples/algebra/getting-started.scm

(import (scheme base) (scheme write) (wile algebra))

;; ============================================================
;; 1. Creating monoids
;; ============================================================
;;
;; A monoid is a binary operation paired with an identity element.
;; The operation must be associative: (a op b) op c = a op (b op c)
;; The identity must satisfy: id op a = a op id = a

(display "=== Creating Monoids ===\n\n")

(define add-monoid (make-monoid + 0))
(define mul-monoid (make-monoid * 1))

(display "Addition monoid: 3 + 4 = ")
(display (monoid-op add-monoid 3 4))
(newline)

(display "Multiplication monoid: 6 * 7 = ")
(display (monoid-op mul-monoid 6 7))
(newline)

(display "Addition identity: ")
(display (monoid-identity add-monoid))
(newline)

(display "Multiplication identity: ")
(display (monoid-identity mul-monoid))
(newline)
(newline)

;; ============================================================
;; 2. Folding lists with monoid-fold
;; ============================================================
;;
;; monoid-fold reduces a list using the monoid's operation,
;; starting from the identity. An empty list returns the identity.

(display "=== Folding Lists ===\n\n")

(display "Sum of (1 2 3 4 5): ")
(display (monoid-fold add-monoid '(1 2 3 4 5)))
(newline)

(display "Product of (1 2 3 4 5): ")
(display (monoid-fold mul-monoid '(1 2 3 4 5)))
(newline)

;; Empty lists return the identity -- no special-casing needed
(display "Sum of (): ")
(display (monoid-fold add-monoid '()))
(newline)

(display "Product of (): ")
(display (monoid-fold mul-monoid '()))
(newline)
(newline)

;; ============================================================
;; 3. Repeated application with monoid-power
;; ============================================================
;;
;; monoid-power applies the operation n times:
;;   (monoid-power M a n) = a op a op ... op a  (n times)
;; starting from the identity. When n=0, returns identity.

(display "=== Repeated Application (monoid-power) ===\n\n")

(display "2^10 via multiplication: ")
(display (monoid-power mul-monoid 2 10))
(newline)

(display "5 * 4 via addition: ")
(display (monoid-power add-monoid 5 4))
(newline)

(display "2^0 (zero repetitions = identity): ")
(display (monoid-power mul-monoid 2 0))
(newline)
(newline)

;; ============================================================
;; 4. Validating monoid laws
;; ============================================================
;;
;; validate-monoid spot-checks the monoid laws (identity and
;; associativity) against sample elements. Returns #t if all
;; laws hold, or a list of violations if any fail.

(display "=== Validating Monoid Laws ===\n\n")

(display "Addition monoid valid? ")
(display (validate-monoid add-monoid '(0 1 -5 100)))
(newline)

(display "Multiplication monoid valid? ")
(display (validate-monoid mul-monoid '(1 2 3)))
(newline)

;; Subtraction is NOT associative: (a - b) - c != a - (b - c)
;; It also fails identity: 0 - a != a for a != 0
(define bad-monoid (make-monoid - 0))
(let ((result (validate-monoid bad-monoid '(1 2 3))))
  (display "Subtraction monoid valid? #f  (returned ")
  (display (length result))
  (display " violations)\n")
  ;; Show a few to illustrate what failed
  (display "  Sample violations: ")
  (display (list (car result) (cadr result) (caddr result)))
  (newline))
(newline)

;; ============================================================
;; 5. Destructuring with with-monoid
;; ============================================================
;;
;; with-monoid binds the operation and identity to local names,
;; giving cleaner syntax when working with a single monoid.

(display "=== Destructuring (with-monoid) ===\n\n")

(with-monoid mul-monoid (op e)
  (display "Destructured mul-monoid:\n")
  (display "  op 6 7 = ")
  (display (op 6 7))
  (newline)
  (display "  identity = ")
  (display e)
  (newline))

(with-monoid add-monoid (combine zero)
  ;; The names are yours to choose
  (display "  combine 10 20 = ")
  (display (combine 10 20))
  (newline)
  (display "  zero = ")
  (display zero)
  (newline))
(newline)

;; ============================================================
;; 6. String monoid -- not just numbers
;; ============================================================
;;
;; Any associative operation with an identity works. Strings
;; under concatenation form a monoid with "" as identity.

(display "=== String Monoid ===\n\n")

(define str-monoid (make-monoid string-append ""))

(display "Concatenate words: ")
(display (monoid-fold str-monoid '("hello" " " "world")))
(newline)

(display "Repeat \"ha\" 3 times: ")
(display (monoid-power str-monoid "ha" 3))
(newline)

(display "String monoid valid? ")
(display (validate-monoid str-monoid '("" "a" "bc" "def")))
(newline)
(newline)

;; ============================================================
;; Summary
;; ============================================================

(display "=== What We Covered ===\n")
(display "  make-monoid       Create a monoid from an operation and identity\n")
(display "  monoid-op         Apply the monoid's operation\n")
(display "  monoid-identity   Retrieve the identity element\n")
(display "  monoid-fold       Reduce a list (empty list = identity)\n")
(display "  monoid-power      Repeat an operation n times\n")
(display "  validate-monoid   Check that the monoid laws hold\n")
(display "  with-monoid       Destructure for cleaner syntax\n")
(newline)
(display "Next: examples/algebra/structures.scm — setoids, orders, lattices, and more.\n")
