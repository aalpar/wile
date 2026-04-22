;;; algebra-incidence-test.scm — Incidence algebra / Möbius function tests

(import (scheme base)
        (chibi test)
        (wile algebra ring)
        (wile algebra incidence))

(test-begin "incidence")

;; ─── Group 1: Classical μ(n) on the divisor lattice ──────────────
;;
;; Rota's μ on the divisor lattice of n, evaluated at (1, n), matches
;; the number-theoretic Möbius function:
;;   μ(n) = 1             if n = 1
;;        = (-1)^k         if n is a product of k distinct primes
;;        = 0              if n has a squared prime factor

(test-group "divisor lattice μ(1,n) matches number-theoretic μ(n)"
  (let* ((divides? (lambda (a b) (zero? (modulo b a))))
         ;; Divisors of 1..12 — cover squarefree, prime powers, mixed.
         (P (finite-set->locally-finite-poset
              divides? '(1 2 3 4 5 6 7 8 9 10 11 12)))
         (IA (make-incidence-algebra P))
         (mu (mobius-function IA)))
    (test  1 (mu 1 1))   ; empty product
    (test -1 (mu 1 2))   ; prime
    (test -1 (mu 1 3))   ; prime
    (test  0 (mu 1 4))   ; 2²
    (test -1 (mu 1 5))   ; prime
    (test  1 (mu 1 6))   ; 2·3
    (test -1 (mu 1 7))   ; prime
    (test  0 (mu 1 8))   ; 2³
    (test  0 (mu 1 9))   ; 3²
    (test  1 (mu 1 10))  ; 2·5
    (test -1 (mu 1 11))  ; prime
    (test  0 (mu 1 12))))  ; 2²·3

;; ─── Group 2: Subset lattice — inclusion-exclusion ───────────────
;;
;; For subsets A ⊆ B of a ground set, μ(A,B) = (-1)^|B\A|.
;; This is what drives inclusion-exclusion.

(define (sorted-sublist? xs ys)
  ;; Every element of XS appears in YS (both are assumed sorted, distinct).
  (let loop ((xs xs) (ys ys))
    (cond
      ((null? xs) #t)
      ((null? ys) #f)
      ((equal? (car xs) (car ys)) (loop (cdr xs) (cdr ys)))
      (else (loop xs (cdr ys))))))

(test-group "subset lattice μ(A,B) = (-1)^|B\\A|"
  (let* ((subsets '(()
                    (a) (b) (c)
                    (a b) (a c) (b c)
                    (a b c)))
         (P (finite-set->locally-finite-poset sorted-sublist? subsets))
         (IA (make-incidence-algebra P))
         (mu (mobius-function IA)))
    ;; |B\A| = 0 → 1
    (test  1 (mu '()      '()))
    (test  1 (mu '(a)     '(a)))
    ;; |B\A| = 1 → -1
    (test -1 (mu '()      '(a)))
    (test -1 (mu '(a)     '(a b)))
    (test -1 (mu '(b c)   '(a b c)))
    ;; |B\A| = 2 → 1
    (test  1 (mu '()      '(a b)))
    (test  1 (mu '(a)     '(a b c)))
    ;; |B\A| = 3 → -1
    (test -1 (mu '()      '(a b c)))
    ;; Incomparable → 0
    (test  0 (mu '(a)     '(b)))
    (test  0 (mu '(a b)   '(b c)))))

;; ─── Group 3: Chain μ ────────────────────────────────────────────
;;
;; On a total order {1, 2, ..., n} with ≤:
;;   μ(i,j) = 1  if i = j
;;          = -1 if j = i+1
;;          = 0  otherwise (i < j but not immediate successor, or i > j)

(test-group "chain μ on total order"
  (let* ((P (finite-set->locally-finite-poset <= '(1 2 3 4 5)))
         (IA (make-incidence-algebra P))
         (mu (mobius-function IA)))
    ;; Diagonal
    (test  1 (mu 1 1))
    (test  1 (mu 3 3))
    ;; Immediate successors
    (test -1 (mu 1 2))
    (test -1 (mu 3 4))
    ;; Gap ≥ 2
    (test  0 (mu 1 3))
    (test  0 (mu 1 5))
    (test  0 (mu 2 5))
    ;; Reversed → 0
    (test  0 (mu 3 1))))

;; ─── Group 4: ζ behavior ─────────────────────────────────────────

(test-group "zeta function indicator"
  (let* ((P (finite-set->locally-finite-poset <= '(1 2 3)))
         (IA (make-incidence-algebra P))
         (zeta (zeta-function IA)))
    (test 1 (zeta 1 1))
    (test 1 (zeta 1 3))
    (test 1 (zeta 2 3))
    (test 0 (zeta 2 1))
    (test 0 (zeta 3 1))))

;; ─── Group 5: Convolution identity — ζ * μ = δ ───────────────────
;;
;; The defining property of μ: it is the convolutional inverse of ζ.
;; Their convolution is the Kronecker δ:
;;   (ζ*μ)(x,y) = 1 if x = y, else 0 (when x ≤ y).

(test-group "ζ * μ = δ (Kronecker) on divisor lattice of 12"
  (let* ((divides? (lambda (a b) (zero? (modulo b a))))
         (P (finite-set->locally-finite-poset
              divides? '(1 2 3 4 6 12)))
         (IA (make-incidence-algebra P))
         (zeta*mu (incidence-convolve IA (zeta-function IA)
                                         (mobius-function IA))))
    ;; Diagonal → 1
    (test 1 (zeta*mu 1 1))
    (test 1 (zeta*mu 6 6))
    (test 1 (zeta*mu 12 12))
    ;; Off-diagonal (x ≤ y but x ≠ y) → 0
    (test 0 (zeta*mu 1 2))
    (test 0 (zeta*mu 1 12))
    (test 0 (zeta*mu 2 6))
    ;; Incomparable → 0 (out of support)
    (test 0 (zeta*mu 2 3))))

;; ─── Group 6: Möbius inversion roundtrip ─────────────────────────
;;
;; If g(x) = Σ_{y ≤ x} f(y), then f(x) = Σ_{y ≤ x} μ(y,x) · g(y).
;; Roundtrip: construct g from f, invert, check f recovered.

(test-group "Möbius inversion recovers f from g on chain"
  (let* ((P (finite-set->locally-finite-poset <= '(1 2 3 4)))
         (IA (make-incidence-algebra P))
         (f (lambda (y) (* y y y)))        ; cubes
         (lower-set
           (lambda (x)
             (let loop ((ys '(1 2 3 4)) (acc '()))
               (cond
                 ((null? ys) (reverse acc))
                 ((<= (car ys) x) (loop (cdr ys) (cons (car ys) acc)))
                 (else (loop (cdr ys) acc))))))
         (g (lambda (x)
              (let loop ((ys (lower-set x)) (s 0))
                (if (null? ys) s (loop (cdr ys) (+ s (f (car ys)))))))))
    (test (f 1) (mobius-inversion IA g 1 (lower-set 1)))
    (test (f 2) (mobius-inversion IA g 2 (lower-set 2)))
    (test (f 3) (mobius-inversion IA g 3 (lower-set 3)))
    (test (f 4) (mobius-inversion IA g 4 (lower-set 4)))))

;; ─── Group 7: Ring parameter — μ over Z/7Z ───────────────────────

(test-group "μ over modular ring Z/7Z"
  (let* ((P (finite-set->locally-finite-poset <= '(1 2 3)))
         (R (modular-ring 7))
         (IA (make-incidence-algebra P R))
         (mu (mobius-function IA)))
    ;; μ values over Z/7Z: -1 becomes 6 mod 7.
    (test 1 (mu 1 1))
    (test 6 (mu 1 2))  ; -1 ≡ 6 (mod 7)
    (test 0 (mu 1 3)))) ; unchanged

;; ─── Group 8: Lazy memoization ───────────────────────────────────
;;
;; The outer μ-cache is keyed on x. After computing μ(1, 12), the
;; outer table contains at least one entry (for x=1). After further
;; computation from other starting points, more x's appear. This
;; test verifies cache population — not a timing claim.

(test-group "μ cache populates on demand"
  (let* ((divides? (lambda (a b) (zero? (modulo b a))))
         (P (finite-set->locally-finite-poset
              divides? '(1 2 3 4 6 12)))
         (IA (make-incidence-algebra P))
         (mu (mobius-function IA)))
    ;; Freshly constructed: cache is empty.
    (test 0 (length (incidence-algebra-mu-cache IA)))
    ;; First call populates the cache.
    (mu 1 12)
    (test #t (positive? (length (incidence-algebra-mu-cache IA))))
    ;; Repeating a fully-cached call does not grow the cache.
    (let ((before (length (incidence-algebra-mu-cache IA))))
      (mu 1 12)
      (test before (length (incidence-algebra-mu-cache IA))))
    ;; A genuinely new (x,y) does grow the cache.
    (let ((before (length (incidence-algebra-mu-cache IA))))
      (mu 3 12)
      (test #t (> (length (incidence-algebra-mu-cache IA)) before)))))

;; ─── Group 9: Edge cases ─────────────────────────────────────────

(test-group "edge cases"
  ;; One-element poset: the only interval is [x,x], μ(x,x) = 1.
  (let* ((P (finite-set->locally-finite-poset <= '(7)))
         (IA (make-incidence-algebra P))
         (mu (mobius-function IA)))
    (test 1 (mu 7 7)))
  ;; Incomparable pair → 0.
  (let* ((subsets '(() (a) (b)))
         (P (finite-set->locally-finite-poset sorted-sublist? subsets))
         (IA (make-incidence-algebra P))
         (mu (mobius-function IA)))
    (test 0 (mu '(a) '(b)))
    (test 0 (mu '(b) '(a))))
  ;; μ(x,x) on any atom is 1.
  (let* ((P (finite-set->locally-finite-poset <= '(1 2 3)))
         (IA (make-incidence-algebra P))
         (mu (mobius-function IA)))
    (test 1 (mu 2 2))))

;; ─── Group 10: Locally-finite without explicit element set ───────
;;
;; ℕ under divisibility is locally-finite but has an infinite universe.
;; An interval-proc using trial division correctly enumerates each
;; finite interval without the library ever seeing a bounded element set.

(test-group "locally-finite poset via direct interval-proc"
  (let* ((divides? (lambda (a b) (zero? (modulo b a))))
         ;; Interval [a,b] in (ℕ, |) = {d : a|d ∧ d|b} = multiples of a
         ;; that divide b. Finite since there are finitely many divisors
         ;; of b.
         (interval
           (lambda (a b)
             (if (not (divides? a b))
                 '()
                 (let loop ((d a) (acc '()))
                   (cond
                     ((> d b) (reverse acc))
                     ((divides? a d)
                      (if (divides? d b)
                          (loop (+ d a) (cons d acc))
                          (loop (+ d a) acc)))
                     (else (loop (+ d a) acc)))))))
         (P (make-locally-finite-poset divides? interval))
         (IA (make-incidence-algebra P))
         (mu (mobius-function IA)))
    ;; Classical Möbius values anchored at 1, verified without any
    ;; global element list.
    (test  1 (mu 1 1))
    (test -1 (mu 1 2))    ; single prime
    (test  0 (mu 1 4))    ; 2² — squared factor
    (test  1 (mu 1 6))    ; 2·3 — two distinct primes → (-1)²
    (test -1 (mu 1 30))   ; 2·3·5 — three distinct primes → (-1)³
    (test  0 (mu 1 12)))) ; 2²·3 — squared factor

(test-end)
(test-exit)
