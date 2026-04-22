;;; algebra-lattice-test.scm — Lattice tests

(import (scheme base)
        (chibi test)
        (srfi 1)
        (wile algebra order)
        (wile algebra setoid)
        (wile algebra lattice))

(test-begin "lattices")

;; -- A simple lattice: divisibility on {1,2,3,6} --
;; join = lcm, meet = gcd, bottom = 1, top = 6

(define div-lat
  (make-lattice
    (lambda (a b) (lcm a b))        ; join
    (lambda (a b) (gcd a b))        ; meet
    1                                ; bottom
    6                                ; top
    (lambda (a b) (zero? (modulo b a)))))  ; leq: a divides b

(test-group "construction"
  (test #t (lattice? div-lat))
  (test #f (lattice? 42)))

(test-group "lattice-join"
  (test 6  (lattice-join div-lat 2 3))
  (test 2  (lattice-join div-lat 1 2))
  (test 6  (lattice-join div-lat 2 6)))

(test-group "lattice-meet"
  (test 1  (lattice-meet div-lat 2 3))
  (test 2  (lattice-meet div-lat 2 6))
  (test 3  (lattice-meet div-lat 3 6)))

(test-group "lattice-bottom and lattice-top"
  (test 1 (lattice-bottom div-lat))
  (test 6 (lattice-top div-lat)))

(test-group "lattice-leq?"
  (test #t (lattice-leq? div-lat 1 6))
  (test #t (lattice-leq? div-lat 2 6))
  (test #f (lattice-leq? div-lat 6 2))
  (test #f (lattice-leq? div-lat 2 3)))

(test-group "lattice->partial-order"
  (let ((po (lattice->partial-order div-lat)))
    (test #t (partial-order? po))
    (test #t (po-leq? po 1 6))
    (test #f (po-leq? po 6 1))))

;; -- flat-lattice --

(test-group "flat-lattice"
  (let ((fl (flat-lattice '(a b c) eq?)))
    (test #t (lattice? fl))
    ;; bottom ≤ everything
    (test #t (lattice-leq? fl (lattice-bottom fl) 'a))
    ;; everything ≤ top
    (test #t (lattice-leq? fl 'a (lattice-top fl)))
    ;; elements are incomparable
    (test #f (lattice-leq? fl 'a 'b))
    ;; join of incomparable = top
    (test 'flat-top (lattice-join fl 'a 'b))
    ;; meet of incomparable = bottom
    (test 'flat-bottom (lattice-meet fl 'a 'b))
    ;; join with bottom = identity
    (test 'a (lattice-join fl (lattice-bottom fl) 'a))
    ;; join of same = same
    (test 'a (lattice-join fl 'a 'a))))

;; -- powerset-lattice --

(test-group "powerset-lattice"
  (let ((ps (powerset-lattice '(x y z))))
    (test #t (lattice? ps))
    ;; empty set is bottom
    (test '() (lattice-bottom ps))
    ;; universe is top
    (test '(x y z) (lattice-top ps))
    ;; subset ordering
    (test #t (lattice-leq? ps '() '(x y)))
    (test #t (lattice-leq? ps '(x) '(x y)))
    (test #f (lattice-leq? ps '(x y) '(x)))
    ;; join = union (order may vary, test membership)
    (let ((result (lattice-join ps '(x) '(y))))
      (test #t (and (member 'x result) (member 'y result) #t)))))

;; -- product-lattice --

(test-group "product-lattice"
  (let* ((fl (flat-lattice '(a b) eq?))
         (pl (product-lattice fl fl)))
    (test #t (lattice? pl))
    ;; bottom is (flat-bottom flat-bottom)
    (test (list 'flat-bottom 'flat-bottom) (lattice-bottom pl))
    ;; pointwise join
    (test (list 'a 'b)
      (lattice-join pl
        (list 'a 'flat-bottom)
        (list 'flat-bottom 'b)))))

;; -- fixpoint --

(test-group "fixpoint"
  ;; Fixpoint on powerset: start from empty, add 'x each step until {x y z}
  (let* ((ps (powerset-lattice '(x y z)))
         ;; transfer: add 'x, 'y, 'z one at a time based on what's there
         (f (lambda (s)
              (cond ((null? s) '(x))
                    ((and (member 'x s) (not (member 'y s)))
                     (cons 'y s))
                    ((and (member 'y s) (not (member 'z s)))
                     (cons 'z s))
                    (else s)))))
    (let ((result (fixpoint ps f '())))
      ;; Should reach {x y z}
      (test #t (and (member 'x result) (member 'y result)
                    (member 'z result) #t)))))

(test-group "fixpoint-bounded"
  ;; Same as above but with fuel=1, should return #f (not converged)
  (let* ((ps (powerset-lattice '(x y z)))
         (f (lambda (s)
              (cond ((null? s) '(x))
                    ((and (member 'x s) (not (member 'y s)))
                     (cons 'y s))
                    ((and (member 'y s) (not (member 'z s)))
                     (cons 'z s))
                    (else s)))))
    (test #f (fixpoint ps f '() 1))))

;; -- with-lattice macro --

(test-group "with-lattice"
  (test 6 (with-lattice div-lat (join meet bottom top leq?)
            (join (join bottom 2) 3))))

;; -- validate-lattice --

(test-group "validate-lattice"
  (test #t (validate-lattice div-lat '(1 2 3 6))))

;; ─── §5.5 — Phase 1: extended <lattice> metadata ─────────────────

(test-group "extended <lattice> with optional metadata"
  (let ((L (make-lattice
             max min 0 4 <=
             (cons 'cardinality 5)
             (cons 'elements '(0 1 2 3 4))
             (cons 'setoid (numeric-setoid)))))
    (test #t (lattice? L))
    (test 5 (lattice-cardinality L))
    (test '(0 1 2 3 4) (lattice-elements L))
    (test #t (finite-lattice? L))
    (test #t (lattice-equiv? L 2 2))
    (test #f (lattice-equiv? L 2 3))))

(test-group "backward compatibility — 5-arg make-lattice"
  (let ((L (make-lattice max min 0 100 <=)))
    (test #t (lattice? L))
    (test 50 (lattice-join L 20 50))
    (test #t (and (lattice-setoid L) #t))
    (test #f (lattice-cardinality L))
    (test #f (lattice-elements L))
    (test #f (finite-lattice? L))))

;; ─── §5.5 Phase 3: canonical presets ─────────────────────────────

(test-group "chain-lattice"
  (let ((C5 (chain-lattice 5)))
    (test 5 (lattice-cardinality C5))
    (test '(0 1 2 3 4) (lattice-elements C5))
    (test 4 (lattice-join C5 2 4))
    (test 2 (lattice-meet C5 2 4))
    (test 0 (lattice-bottom C5))
    (test 4 (lattice-top C5))
    (test #t (finite-lattice? C5))))

(test-group "chain-lattice — preconditions"
  (test-error (chain-lattice 0))
  (test-error (chain-lattice -1))
  (test-error (chain-lattice 'nope)))

(test-group "boolean-lattice"
  (let ((B0 (boolean-lattice 0))
        (B3 (boolean-lattice 3)))
    (test 1 (lattice-cardinality B0))
    (test 8 (lattice-cardinality B3))
    (test '() (lattice-bottom B3))
    (test #t (lattice-leq? B3 '(1) '(1 2)))
    (test #t (lattice-leq? B3 '() '(2)))
    ;; Membership in elements list: every 3-bit subset accounted for
    (test 8 (length (lattice-elements B3)))))

(test-group "boolean-lattice — preconditions"
  (test-error (boolean-lattice -1))
  (test-error (boolean-lattice 'nope)))

(test-group "diamond-lattice — M3 (n=3)"
  (let ((M3 (diamond-lattice 3)))
    (test 5 (lattice-cardinality M3))
    (test 'bot (lattice-bottom M3))
    (test 'top (lattice-top M3))
    (test 5 (length (lattice-elements M3)))
    ;; Atoms are incomparable to each other, top/bot comparable to all
    (test #t (lattice-leq? M3 'bot (list 'atom 0)))
    (test #t (lattice-leq? M3 (list 'atom 0) 'top))
    (test #f (lattice-leq? M3 (list 'atom 0) (list 'atom 1)))
    (test #f (lattice-leq? M3 (list 'atom 1) (list 'atom 0)))
    ;; Join/meet of distinct atoms → top/bot
    (test 'top (lattice-join M3 (list 'atom 0) (list 'atom 1)))
    (test 'bot (lattice-meet M3 (list 'atom 0) (list 'atom 1)))
    ;; Join of atom with itself = atom; meet atom ⋀ top = atom
    (test '(atom 0) (lattice-join M3 (list 'atom 0) (list 'atom 0)))
    (test '(atom 0) (lattice-meet M3 (list 'atom 0) 'top))))

(test-group "diamond-lattice — preconditions"
  (test-error (diamond-lattice 2))
  (test-error (diamond-lattice 'nope)))

(test-group "pentagon-lattice — N5"
  (let ((N5 (pentagon-lattice)))
    (test 5 (lattice-cardinality N5))
    (test 'bot (lattice-bottom N5))
    (test 'top (lattice-top N5))
    (test '(bot a b c top) (lattice-elements N5))
    ;; Ordering: bot < a < top; bot < b < c < top; a inc. with b, c.
    (test #t (lattice-leq? N5 'bot 'a))
    (test #t (lattice-leq? N5 'b 'c))
    (test #t (lattice-leq? N5 'c 'top))
    (test #f (lattice-leq? N5 'a 'b))
    (test #f (lattice-leq? N5 'a 'c))
    (test #f (lattice-leq? N5 'b 'a))
    (test #f (lattice-leq? N5 'c 'a))
    ;; Joins: a ⋁ b = a ⋁ c = top; b ⋁ c = c
    (test 'top (lattice-join N5 'a 'b))
    (test 'top (lattice-join N5 'a 'c))
    (test 'c   (lattice-join N5 'b 'c))
    ;; Meets: a ⋀ b = a ⋀ c = bot; b ⋀ c = b
    (test 'bot (lattice-meet N5 'a 'b))
    (test 'bot (lattice-meet N5 'a 'c))
    (test 'b   (lattice-meet N5 'b 'c))))

;; ─── §5.5 Phase 4: irreducibles ──────────────────────────────────

(test-group "join-irreducibles on chain"
  ;; Chain(n): elements 0..n-1; join-irreducibles = all except bot (0)
  (test '(1 2 3) (join-irreducibles (chain-lattice 4)))
  (test #t (join-irreducible? (chain-lattice 4) 1))
  (test #f (join-irreducible? (chain-lattice 4) 0)))

(test-group "join-irreducibles on boolean"
  ;; B(3): join-irreducibles are the singletons (atoms) — 3 of them
  (let* ((B3 (boolean-lattice 3))
         (ji (join-irreducibles B3)))
    (test 3 (length ji))
    ;; Each element is a singleton list
    (test #t (every (lambda (s) (= (length s) 1)) ji))))

(test-group "meet-irreducibles on boolean"
  ;; B(3): meet-irreducibles are the coatoms — 3 of them (2-element
  ;; subsets)
  (let* ((B3 (boolean-lattice 3))
         (mi (meet-irreducibles B3)))
    (test 3 (length mi))
    (test #t (every (lambda (s) (= (length s) 2)) mi))))

(test-group "join-irreducibles on diamond(3) — M3"
  ;; M3: join-irreducibles are the three atoms
  (let* ((M3 (diamond-lattice 3))
         (ji (join-irreducibles M3)))
    (test 3 (length ji))
    (test #t (every (lambda (x) (and (pair? x) (eq? (car x) 'atom))) ji))))

(test-group "join-irreducibles on pentagon"
  ;; N5: bot, a, b, c, top
  ;;   a: one lower cover (bot) → join-irreducible
  ;;   b: one lower cover (bot) → join-irreducible
  ;;   c: one lower cover (b) → join-irreducible
  ;;   top: two lower covers (a, c) → NOT join-irreducible
  (let* ((N5 (pentagon-lattice))
         (ji (join-irreducibles N5)))
    (test '(a b c) ji)))

(test-group "irreducibles preconditions"
  (test-error (join-irreducibles (make-lattice max min 0 10 <=)))
  (test-error (meet-irreducibles (make-lattice max min 0 10 <=))))

(test-end)
(test-exit)
