;; ================================================================
;; Chapter 02 -- Structures: the algebraic tower
;;
;; What you will learn:
;;   - How the tower stacks: setoid -> lattice, monoid -> group -> ring -> field.
;;   - What a *forgetful projection* is and why the library has so many.
;;   - How Boolean algebras sit at the intersection of lattices and rings.
;;   - Why preset structures (integer-ring, rational-field, powerset-lattice)
;;     exist: they bake in the classical examples so you do not re-derive
;;     them every time.
;;
;; Prerequisites: chapter 01 (understanding monoid, validate-monoid).
;; Sub-libraries used:
;;   (wile algebra lattice), (wile algebra semiring), (wile algebra group),
;;   (wile algebra ring), (wile algebra differential), (wile algebra boolean),
;;   (wile algebra heyting).
;; ================================================================

(import (scheme base) (scheme write)
        (srfi 132)                ; sort (not in scheme base)
        (wile algebra monoid)
        (wile algebra lattice)
        (wile algebra semiring)
        (wile algebra group)
        (wile algebra ring)
        (wile algebra differential)
        (wile algebra boolean)
        (wile algebra heyting))
(include "../lib/check.scm")

(define (sym<? a b) (string<? (symbol->string a) (symbol->string b)))

;; ----------------------------------------------------------------
;; Part 1: Lattices -- join, meet, bottom, top.
;;
;; A lattice is a partial order with a least-upper-bound (join) and
;; greatest-lower-bound (meet) for every pair. A bounded lattice has
;; a global bottom and top. `powerset-lattice` is the textbook example:
;; subsets ordered by inclusion, join = union, meet = intersection.
;; ----------------------------------------------------------------

(define L (powerset-lattice '(a b c)))

(check-true  (lattice? L)                               "L is a lattice")
(check=      (lattice-bottom L)  '()                    "bottom is empty set")
(check-true  (member 'a (lattice-top L))                "top contains a")
(check-true  (member 'c (lattice-top L))                "top contains c")

(check= (sort sym<? (lattice-join L '(a) '(b)))
        '(a b)
        "union of {a} and {b}")
(check= (lattice-meet L '(a b) '(b c))
        '(b)
        "intersection of {a,b} and {b,c}")

(check-true  (lattice-leq? L '(a) '(a b))   "{a} subset of {a,b}")
(check-false (lattice-leq? L '(a b) '(a))   "{a,b} not subset of {a}")

;; ----------------------------------------------------------------
;; Part 2: Fixpoints.
;;
;; A monotone function on a lattice has a least fixpoint. `fixpoint`
;; iterates from the bottom upward until nothing changes. This is the
;; mechanism behind reachability analyses, static analyzers, and many
;; other fixed-point computations.
;;
;; Example: reachability in a directed graph. The state is a set of
;; reachable nodes; the step adds successors. Iterate until stable.
;; ----------------------------------------------------------------

(define edges '((a b) (b c) (c d) (c e) (d f) (x y)))

(define (successors-of node)
  (map cadr (filter (lambda (edge) (eq? (car edge) node)) edges)))

(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

(define R (powerset-lattice '(a b c d e f x y)))

(define (step-from-a seen)
  (let loop ((xs seen) (out seen))
    (if (null? xs) out
        (loop (cdr xs)
              (lattice-join R out (successors-of (car xs)))))))

(define reachable-from-a (fixpoint R step-from-a '(a)))
(check-true (member 'a reachable-from-a)    "a is reachable from a")
(check-true (member 'f reachable-from-a)    "f is reachable from a via b,c,d")
(check-false (member 'x reachable-from-a)   "x is in a separate component")

;; ----------------------------------------------------------------
;; Part 3: The additive/multiplicative tower.
;;
;; Semiring: (plus, times, 0, 1). Two monoids sharing a carrier.
;;   No negate. Booleans, natural numbers, tropical semiring all fit here.
;;
;; Ring: semiring + negate. Integers are the classical ring.
;;
;; Field: ring + reciprocal. Rationals, reals, complex numbers.
;;
;; Each level adds exactly one operation. Each level has a forgetful
;; projection to the level below it.
;; ----------------------------------------------------------------

(define Nsem (make-semiring + * 0 1))               ; natural-number semiring
(check= (semiring-plus Nsem 3 4)  7  "natural semiring addition")
(check= (semiring-times Nsem 6 7) 42 "natural semiring multiplication")
(check= (semiring-zero Nsem)       0  "semiring zero")
(check= (semiring-one  Nsem)       1  "semiring one")

(define Z (integer-ring))                           ; preset: integer ring
(check-true (ring? Z)                   "Z is a ring")
(check= (ring-plus   Z 3 4)   7         "Z addition")
(check= (ring-times  Z 6 7)  42         "Z multiplication")
(check= (ring-negate Z 5)    -5         "Z negation")
(check= (ring-minus  Z 10 3)  7         "Z subtraction = plus . negate")
(check= (ring-zero   Z)       0         "Z zero")
(check= (ring-one    Z)       1         "Z one")

(define Q (rational-field))                         ; preset: rational field
(check-true (field? Q)                           "Q is a field")
(check= (field-plus       Q 1/2 1/3) 5/6         "Q addition")
(check= (field-times      Q 2/3 3/4) 1/2         "Q multiplication")
(check= (field-reciprocal Q 3/5)     5/3         "Q reciprocal")
(check= (field-divide     Q 1 3)     1/3         "Q division")

;; ----------------------------------------------------------------
;; Part 4: Forgetful projections -- keeping what you need, discarding
;; what you do not.
;;
;; A ring is "a semiring plus negate." If you write an algorithm that
;; only needs addition, multiplication, 0, and 1, it should accept a
;; semiring. You can run it on a ring by projecting: `ring->semiring`
;; builds a new semiring record from the ring's four fields, discarding
;; negate.
;;
;; This mirrors the category-theoretic *forgetful functor* -- a map that
;; drops structure without changing the underlying set.
;; ----------------------------------------------------------------

(define Zsem (ring->semiring Z))
(check-true (semiring? Zsem)                      "Z projects to a semiring")
(check= (semiring-plus  Zsem 3 4) 7               "projected plus")
(check= (semiring-times Zsem 6 7) 42              "projected times")

(define Zadd (ring->additive-group Z))
(check-true (group? Zadd)                         "ring has additive group")
(check= (group-op       Zadd 3 4)  7              "additive group op is +")
(check= (group-identity Zadd)      0              "additive group id is 0")
(check= (group-inverse  Zadd 5)   -5              "additive group inverse is negate")

(define Zmono (semiring->additive-monoid Zsem))
(check-true (monoid? Zmono)                       "semiring gives additive monoid")
(check= (monoid-op       Zmono 3 4) 7             "monoid op = semiring plus")
(check= (monoid-identity Zmono)     0             "monoid id = semiring zero")

;; Field projects down to ring, and ring to semiring. The chain means
;; rational code can consume rings, ring code can consume semirings, etc.
(define Qring (field->ring Q))
(check-true (ring? Qring)                         "field projects to ring")
(check= (ring-times Qring 2/3 3/4) 1/2            "ring-from-field multiplication")

;; ----------------------------------------------------------------
;; Part 5: Differential rings -- rings with a derivation.
;;
;; A differential ring is a ring plus a unary "derivative" operation D
;; satisfying linearity and the Leibniz rule:
;;   D(a+b) = D(a) + D(b)
;;   D(a*b) = D(a)*b + a*D(b)
;;
;; `dual-number-ring` is the classical presentation of automatic
;; differentiation: pairs (a, b) representing a + b*epsilon with
;; epsilon^2 = 0. Derivative of (a, b) is (b, 0).
;; ----------------------------------------------------------------

(define D (dual-number-ring))
(check-true (differential-ring? D)                    "D is a differential ring")

;; Dual numbers (a . b) represent a + b*epsilon. The derivation extracts
;; the infinitesimal part: D(a + b*eps) = b*eps. In pair representation
;; that is (0 . b) -- real part zeroed, infinitesimal part preserved.
(check= (differential-deriv D '(5 . 2))  '(0 . 2)     "D(5 + 2*eps) = 2*eps")
(check= (differential-deriv D '(7 . 0))  '(0 . 0)     "D(constant) = 0")
(check-true  (differential-constant? D '(7 . 0))      "(7,0) is a constant")
(check-false (differential-constant? D '(7 . 3))      "(7,3) is not a constant")

;; ----------------------------------------------------------------
;; Part 6: Boolean algebras and their three projections.
;;
;; A Boolean algebra is simultaneously:
;;   - a bounded distributive lattice with complement, and
;;   - a ring of characteristic 2 under (xor, and).
;;
;; Three projections land in three different towers:
;;   boolean->heyting  : forget complement (keep implication)
;;   boolean->lattice  : forget complement and implication
;;   boolean->ring     : cross into the additive/multiplicative tower
;; ----------------------------------------------------------------

(define B (powerset-boolean '(a b c)))
(check-true (boolean-algebra? B)                           "B is a Boolean algebra")

(check= (sort sym<? (boolean-join B '(a) '(b)))
        '(a b)
        "B-join is union")
(check= (boolean-meet B '(a b) '(b c))
        '(b)
        "B-meet is intersection")
(check= (sort sym<? (boolean-complement B '(a)))
        '(b c)
        "complement of {a} in universe {a,b,c}")

;; Projection B -> Heyting: forget complement, but implication still exists.
(define BH (boolean->heyting B))
(check-true (heyting-algebra? BH)                          "B projects to Heyting")

;; Projection B -> lattice: forget implication too.
(define BL (boolean->lattice B))
(check-true (lattice? BL)                                  "B projects to lattice")
(check= (lattice-meet BL '(a b) '(b c))
        '(b)
        "lattice meet matches boolean meet")

;; Projection B -> ring: xor (= symmetric difference) as plus, and (= intersection)
;; as times. Characteristic 2: every element is its own additive inverse.
(define BR (boolean->ring B))
(check-true (ring? BR)                                     "B projects to ring")
(check= (ring-plus BR '(a) '(a))  '()
        "characteristic 2: x + x = 0 in Boolean ring")

;; ----------------------------------------------------------------
;; Part 7: validate-X spot checks across the tower.
;;
;; Every structure has a validate-X. They all return #t or a list of
;; violation descriptions, so the same pattern catches mistakes at every
;; level.
;; ----------------------------------------------------------------

(check= (validate-semiring Nsem '(0 1 2 3))            #t  "Nsem is a valid semiring")
(check= (validate-ring Z '(-2 -1 0 1 2))               #t  "Z is a valid ring")
(check= (validate-field Q '(1/2 1 2 3))                #t  "Q is a valid field")
(check= (validate-lattice L '(() (a) (b) (a b)))       #t  "L is a valid lattice")

;; ----------------------------------------------------------------
;; Part 8: with-X destructuring.
;;
;; Every structure has a with-X macro that binds its operations to
;; local names. The field lists differ between structures, but the
;; pattern is identical: open the structure once, use short names in
;; the body.
;; ----------------------------------------------------------------

(define poly-eval
  ;; Horner evaluation of x^2 + 2x + 1 at x using Z's plus and times.
  (with-ring Z (plus times zero one negate)
    (lambda (x)
      (plus (times x (plus x (times 2 one)))
            one))))

(check= (poly-eval 3) 16  "(3)^2 + 2*3 + 1 = 16 via with-ring")
(check= (poly-eval 0) 1   "(0)^2 + 2*0 + 1 = 1 via with-ring")

(display "chapter 02 complete") (newline)
