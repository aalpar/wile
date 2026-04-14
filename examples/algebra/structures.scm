;;; structures.scm - Algebraic structures and their relationships
;;;
;;; Demonstrates: lattices, rings, fields, Boolean algebras, forgetful projections, with-ring
;;;
;;; Usage: ./dist/wile --file examples/algebra/structures.scm

(import (scheme base) (scheme write) (wile algebra))

;; ============================================================
;; 1. Lattices
;; ============================================================
;;
;; A lattice has join (least upper bound), meet (greatest lower
;; bound), a bottom element, a top element, and a partial order.

(display "=== Lattices ===\n\n")

;; Integer lattice: max/min over [0, 100]
(define int-lat (make-lattice max min 0 100 <=))

(display "Integer lattice over [0, 100]:\n")
(display "  join(30, 70) = ")
(display (lattice-join int-lat 30 70))
(newline)
(display "  meet(30, 70) = ")
(display (lattice-meet int-lat 30 70))
(newline)
(display "  bottom = ")
(display (lattice-bottom int-lat))
(newline)
(display "  top = ")
(display (lattice-top int-lat))
(newline)
(newline)

;; Powerset lattice: subsets of {a, b, c} ordered by inclusion
(define ps-lat (powerset-lattice '(a b c)))

(display "Powerset lattice over {a, b, c}:\n")
(display "  join({a,b}, {b,c}) = ")
(display (lattice-join ps-lat '(a b) '(b c)))
(display "  (union)\n")
(display "  meet({a,b}, {b,c}) = ")
(display (lattice-meet ps-lat '(a b) '(b c)))
(display "  (intersection)\n")
(display "  bottom = ")
(display (lattice-bottom ps-lat))
(display "  (empty set)\n")
(display "  top = ")
(display (lattice-top ps-lat))
(display "  (universe)\n")
(newline)

;; ============================================================
;; 2. Rings
;; ============================================================
;;
;; A ring adds multiplication and additive inverses to a monoid.
;; It has two operations (plus, times), two identities (zero, one),
;; and a negation operation.

(display "=== Rings ===\n\n")

;; Integer ring: standard arithmetic
(define Z (integer-ring))

(display "Integer ring Z:\n")
(display "  3 + 4 = ")
(display (ring-plus Z 3 4))
(newline)
(display "  6 * 7 = ")
(display (ring-times Z 6 7))
(newline)
(display "  negate(5) = ")
(display (ring-negate Z 5))
(newline)
(newline)

;; Modular ring: arithmetic mod 7
(define Z7 (modular-ring 7))

(display "Modular ring Z/7Z:\n")
(display "  5 + 4 = ")
(display (ring-plus Z7 5 4))
(display "  (mod 7)\n")
(display "  3 * 5 = ")
(display (ring-times Z7 3 5))
(display "  (mod 7)\n")
(display "  negate(3) = ")
(display (ring-negate Z7 3))
(display "  (since 3 + 4 = 0 mod 7)\n")
(newline)

;; ============================================================
;; 3. Fields
;; ============================================================
;;
;; A field is a ring where every nonzero element has a
;; multiplicative inverse. Division is always defined
;; (except by zero).

(display "=== Fields ===\n\n")

(define Q (rational-field))

(display "Rational field Q:\n")
(display "  1/3 + 1/4 = ")
(display (field-plus Q 1/3 1/4))
(newline)
(display "  2/3 * 3/4 = ")
(display (field-times Q 2/3 3/4))
(newline)
(display "  reciprocal(2/3) = ")
(display (field-reciprocal Q 2/3))
(newline)
(display "  (5/6) / (2/3) = ")
(display (field-divide Q 5/6 2/3))
(newline)
(newline)

;; ============================================================
;; 4. Boolean Algebras
;; ============================================================
;;
;; A Boolean algebra is a complemented distributive lattice.
;; Join is OR, meet is AND, complement is NOT. The powerset
;; of a set is the canonical example.

(display "=== Boolean Algebras ===\n\n")

(define B (powerset-boolean '(x y z)))

(display "Powerset Boolean algebra over {x, y, z}:\n")
(display "  join({x}, {y,z}) = ")
(display (boolean-join B '(x) '(y z)))
(display "  (OR: union)\n")
(display "  meet({x,y}, {y,z}) = ")
(display (boolean-meet B '(x y) '(y z)))
(display "  (AND: intersection)\n")
(display "  complement({x}) = ")
(display (boolean-complement B '(x)))
(display "  (NOT: set difference from universe)\n")
(newline)

;; ============================================================
;; 5. Forgetful Projections
;; ============================================================
;;
;; Every richer structure contains simpler ones inside it.
;; "Forgetful" projections extract these by dropping operations.
;; This is the category-theoretic notion of a forgetful functor.

(display "=== Forgetful Projections ===\n\n")

;; --- Ring -> Semiring (forget negation) ---

(display "Ring -> Semiring (forget negation):\n")
(define S (ring->semiring Z))
(display "  semiring-plus(3, 4) = ")
(display (semiring-plus S 3 4))
(newline)
(display "  semiring-times(6, 7) = ")
(display (semiring-times S 6 7))
(newline)
(newline)

;; --- Ring -> Additive Group -> Monoid (forget inverse, then fold) ---

(display "Ring -> Additive Group -> Monoid:\n")
(define G (ring->additive-group Z))
(define M (group->monoid G))
(display "  monoid-fold(+, [1..5]) = ")
(display (monoid-fold M '(1 2 3 4 5)))
(display "  (sum via extracted monoid)\n")
(newline)

;; --- Boolean -> Heyting -> Lattice (two-step forgetting) ---
;;
;; Boolean has complement. Heyting has implication but no
;; complement. Lattice has neither.

(display "Boolean -> Heyting -> Lattice:\n")
(define H (boolean->heyting B))
(display "  heyting-implies({x,y}, {y,z}) = ")
(display (heyting-implies H '(x y) '(y z)))
(display "  (largest c where {x,y} ^ c <= {y,z})\n")

(define L (heyting->lattice H))
(display "  lattice-join({x}, {y}) = ")
(display (lattice-join L '(x) '(y)))
(display "  (same join, implication forgotten)\n")
(newline)

;; --- Boolean -> Ring (symmetric difference) ---
;;
;; Every Boolean algebra is a ring of characteristic 2:
;; plus = symmetric difference, times = meet.

(display "Boolean -> Ring (characteristic 2):\n")
(define BR (boolean->ring B))
(display "  plus({x,y}, {y,z}) = ")
(display (ring-plus BR '(x y) '(y z)))
(display "  (symmetric difference)\n")
(display "  times({x,y}, {y,z}) = ")
(display (ring-times BR '(x y) '(y z)))
(display "  (intersection)\n")
(newline)

;; ============================================================
;; 6. Destructuring with with-ring
;; ============================================================
;;
;; with-ring binds a ring's operations to local names so you
;; can write algebraic expressions naturally.

(display "=== Destructuring (with-ring) ===\n\n")

(with-ring Z (plus times zero one negate)
  (let ((a 7) (b 3))
    (display "  With integer ring, a=7, b=3:\n")
    (display "  (a + b) * (a - b) = ")
    (display (times (plus a b) (plus a (negate b))))
    (display "  [difference of squares: 10 * 4 = 40]\n")))
(newline)
(newline)

;; ============================================================
;; Summary
;; ============================================================

(display "=== What We Covered ===\n")
(display "  Lattices          make-lattice, powerset-lattice, join/meet/bottom/top\n")
(display "  Rings             integer-ring, modular-ring, ring-plus/times/negate\n")
(display "  Fields            rational-field, field-plus/times/reciprocal/divide\n")
(display "  Boolean algebras  powerset-boolean, join/meet/complement\n")
(display "  Projections       ring->semiring, ring->additive-group, group->monoid\n")
(display "                    boolean->heyting, heyting->lattice, boolean->ring\n")
(display "  Destructuring     with-ring for natural algebraic syntax\n")
(newline)
(display "Next: examples/algebra/rewriting.scm — axiom-driven term rewriting.\n")
