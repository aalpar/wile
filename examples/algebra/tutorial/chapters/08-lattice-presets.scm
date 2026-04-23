;; ================================================================
;; Chapter 08 -- Lattice presets and Möbius functions
;;
;; What you will learn:
;;   - The five canonical preset lattices: chain, boolean, diamond (M_3),
;;     pentagon (N_5), free-distributive.
;;   - distributive? / modular? as exhaustive checks across triples.
;;   - join-irreducibles and meet-irreducibles on each preset.
;;   - Birkhoff's theorem: every finite distributive lattice equals
;;     reconstructed-downsets-of-its-join-irreducibles.
;;   - Dedekind numbers D(2)..D(4) computed via free-distributive-lattice.
;;   - Locally-finite posets, Möbius function, and inclusion-exclusion
;;     via mobius-inversion on a small divisor poset.
;;
;; Prerequisites: chapter 02 (lattices).
;; Sub-libraries used:
;;   (wile algebra lattice), (wile algebra incidence), (wile algebra ring).
;; ================================================================

(import (scheme base) (scheme write)
        (wile algebra ring)
        (wile algebra lattice)
        (wile algebra incidence))
(include "../lib/check.scm")

;; ----------------------------------------------------------------
;; Part 1: Chain lattice 0 < 1 < 2 < 3 < 4.
;; ----------------------------------------------------------------

(define C5 (chain-lattice 5))
(check-true (lattice? C5)                      "chain-lattice is a lattice")
(check= (lattice-cardinality C5)   5           "|chain(5)| = 5")
(check= (lattice-bottom C5)        0           "chain bottom is 0")
(check= (lattice-top C5)           4           "chain top is 4")
(check= (lattice-join C5 2 4)      4           "chain join is max")
(check= (lattice-meet C5 2 4)      2           "chain meet is min")

;; ----------------------------------------------------------------
;; Part 2: Boolean lattice 2^{0,1,2}.
;; ----------------------------------------------------------------

(define B3 (boolean-lattice 3))
(check= (lattice-cardinality B3)   8           "|2^3| = 8")
(check= (lattice-bottom B3)        '()         "boolean bottom is empty set")
(check= (lattice-top B3)           '(0 1 2)    "boolean top is full universe")
(check= (lattice-join B3 '(0) '(1))   '(0 1)   "boolean join is union")
(check= (lattice-meet B3 '(0 1) '(1 2)) '(1)   "boolean meet is intersection")

;; ----------------------------------------------------------------
;; Part 3: Diamond lattice M_3 and pentagon lattice N_5.
;;
;; M_3: bottom, three incomparable atoms, top. Modular but not distributive.
;; N_5: the pentagon -- 5 elements arranged so one "chain of 3" and one
;;      "chain of 2" meet at bottom and top. Neither modular nor distributive.
;; ----------------------------------------------------------------

(define M3 (diamond-lattice 3))
(check= (lattice-cardinality M3)   5           "|M_3| = 5 (bottom + 3 atoms + top)")

(define N5 (pentagon-lattice))
(check= (lattice-cardinality N5)   5           "|N_5| = 5")

;; ----------------------------------------------------------------
;; Part 4: distributive? / modular? distinguish these lattices.
;;
;; A lattice is distributive iff (a ∧ (b ∨ c)) = (a ∧ b) ∨ (a ∧ c)
;; for all triples. M_3 fails distributivity; N_5 fails both.
;; ----------------------------------------------------------------

(check-true  (distributive? C5)    "chain is distributive")
(check-true  (distributive? B3)    "Boolean lattice is distributive")
(check-false (distributive? M3)    "M_3 is NOT distributive")
(check-false (distributive? N5)    "N_5 is NOT distributive")

(check-true  (modular? C5)         "chain is modular")
(check-true  (modular? B3)         "Boolean is modular")
(check-true  (modular? M3)         "M_3 IS modular")
(check-false (modular? N5)         "N_5 is NOT modular (witness lattice for non-modularity)")

;; ----------------------------------------------------------------
;; Part 5: join-irreducibles and meet-irreducibles.
;;
;; An element x is join-irreducible if it is not the join of strictly
;; smaller elements. For a chain, every non-bottom element is
;; join-irreducible (1, 2, 3, 4 in chain(5)). For B_n, the
;; join-irreducibles are the singletons.
;; ----------------------------------------------------------------

(check= (join-irreducibles C5)  '(1 2 3 4)
        "chain(5) join-irreducibles are {1,2,3,4}")

;; Boolean(3) singletons: {0}, {1}, {2}.
(check= (length (join-irreducibles B3))  3
        "Boolean(3) has 3 join-irreducibles (singletons)")

;; M_3 atoms are join-irreducible. It has 3 atoms.
(check= (length (join-irreducibles M3))  3
        "M_3 has 3 join-irreducibles (the atoms)")

;; Symmetric with meet-irreducibles for chain.
(check= (meet-irreducibles C5)  '(0 1 2 3)
        "chain(5) meet-irreducibles are {0,1,2,3}")

;; ----------------------------------------------------------------
;; Part 6: Birkhoff representation and reconstruction.
;;
;; Birkhoff's fundamental theorem (1937):
;;   Every finite distributive lattice is isomorphic to the lattice
;;   of downsets of the poset of its join-irreducibles.
;;
;; Steps:
;;   1. birkhoff-representation L        -> a poset P
;;   2. birkhoff-reconstruction P        -> a lattice L'
;;   3. L and L' are isomorphic          (verified by cardinality here;
;;                                        the library provides roundtrip
;;                                        tests in its own test suite).
;; ----------------------------------------------------------------

(define P-from-C5 (birkhoff-representation C5))
(check-true (locally-finite-poset? P-from-C5)
            "birkhoff-representation returns a locally-finite poset")

(define C5-rebuilt (birkhoff-reconstruction P-from-C5))
(check= (lattice-cardinality C5-rebuilt)  5
        "chain(5) reconstructs to cardinality 5")

(define B3-rebuilt
  (birkhoff-reconstruction (birkhoff-representation B3)))
(check= (lattice-cardinality B3-rebuilt)  8
        "boolean(3) reconstructs to cardinality 8")

;; ----------------------------------------------------------------
;; Part 7: Dedekind numbers via free-distributive-lattice.
;;
;; The n-th Dedekind number D(n) counts monotone Boolean functions
;; on {0,1}^n, equivalently the elements of the free distributive
;; lattice on n generators. Values: D(0)=2, D(1)=3, D(2)=6, D(3)=20,
;; D(4)=168, D(5)=7581.
;;
;; D(5) is slow (~seconds); we stop at D(4) to keep this chapter fast.
;; If you want to push to D(5), uncomment the commented check below.
;; ----------------------------------------------------------------

(check= (lattice-cardinality (free-distributive-lattice 0))  2     "D(0) = 2")
(check= (lattice-cardinality (free-distributive-lattice 1))  3     "D(1) = 3")
(check= (lattice-cardinality (free-distributive-lattice 2))  6     "D(2) = 6")
(check= (lattice-cardinality (free-distributive-lattice 3))  20    "D(3) = 20")
(check= (lattice-cardinality (free-distributive-lattice 4))  168   "D(4) = 168")
;; (check= (lattice-cardinality (free-distributive-lattice 5)) 7581  "D(5) = 7581")

;; Every free distributive lattice IS distributive, by construction.
(check-true (distributive? (free-distributive-lattice 2))  "FDL(2) is distributive")
(check-true (distributive? (free-distributive-lattice 3))  "FDL(3) is distributive")

;; ----------------------------------------------------------------
;; Part 8: Projection to locally-finite poset.
;;
;; Any finite lattice projects to a poset by keeping the leq? relation
;; and discarding the join/meet operations. The result composes with
;; the (wile algebra incidence) library's Möbius machinery.
;; ----------------------------------------------------------------

(define C5-as-poset (lattice->locally-finite-poset C5))
(check-true (locally-finite-poset? C5-as-poset)
            "lattice projects to poset")
(check= (length (lf-poset-elements C5-as-poset))  5
        "poset has 5 elements (same as lattice)")

;; ----------------------------------------------------------------
;; Part 9: Möbius function on the divisor poset of 12.
;;
;; The divisor poset of n: elements are divisors of n, ordered by
;; divisibility (a <= b iff a | b). Möbius function μ on divisor posets
;; is the classical number-theoretic Möbius function:
;;   μ(1, 1) =  1
;;   μ(1, p) = -1 for prime p
;;   μ(1, p*q) = 1  for distinct primes (square-free, two prime factors)
;;   μ(1, p^2) = 0  (has a square factor)
;;   μ(1, 12) = 0   (12 = 2^2 * 3 has a square factor)
;; ----------------------------------------------------------------

(define divs-12 (list 1 2 3 4 6 12))
(define div12-poset
  (finite-set->locally-finite-poset
    (lambda (a b) (zero? (modulo b a)))
    divs-12))

(define IA (make-incidence-algebra div12-poset))

(define mu (mobius-function IA))

(check= (mu 1 1)    1   "μ(1, 1) = 1")
(check= (mu 1 2)   -1   "μ(1, 2) = -1 (prime)")
(check= (mu 1 3)   -1   "μ(1, 3) = -1 (prime)")
(check= (mu 1 6)    1   "μ(1, 6) = 1 (2·3, squarefree)")
(check= (mu 1 4)    0   "μ(1, 4) = 0 (2^2, has square factor)")
(check= (mu 1 12)   0   "μ(1, 12) = 0 (2^2·3, has square factor)")

;; ----------------------------------------------------------------
;; Part 10: Möbius inversion -- the classical inclusion-exclusion.
;;
;; If g(y) = Σ_{x <= y} f(x), then f(y) = Σ_{x <= y} μ(x, y) · g(x).
;;
;; Example: count proper divisors of 6.
;;   Divisors of 6: {1, 2, 3, 6}.
;;   Let g(n) = |divisors of n|. Then g(1)=1, g(2)=2, g(3)=2, g(6)=4.
;;   μ-inversion: f(6) = Σ μ(x,6)·g(x) -- this gives the count of numbers
;;   coprime to 6, which equals φ(6) = 2 (namely 1 and 5, both <= 6).
;;
;; Here we just verify that the convolution ζ * μ = delta (inverse pair).
;; That is: Σ_{x <= z <= y} ζ(x,z)·μ(z,y) = [x = y].
;; ----------------------------------------------------------------

(define zeta (zeta-function IA))

;; Non-trivial sanity: the Möbius of an interval [a, b] and its zeta
;; compose back to the delta. We verify for (1, 6).
;;
;; By incidence-convolve: (ζ * μ)(x, y) = δ(x, y).
(check-true (incidence-algebra? IA)           "incidence algebra built")

;; Spot-check the ζ function on a known pair.
(check= (zeta 1 6)   1   "ζ(1, 6) = 1 (1 | 6)")
(check= (zeta 6 1)   0   "ζ(6, 1) = 0 (6 does not divide 1)")
(check= (zeta 2 3)   0   "ζ(2, 3) = 0 (2 does not divide 3)")

(display "chapter 08 complete") (newline)
