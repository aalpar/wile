;;; algebra-lattice-test.scm — Lattice tests

(import (scheme base)
        (chibi test)
        (srfi 1)
        (wile algebra order)
        (wile algebra setoid)
        (wile algebra incidence)
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

;; ─── §5.5 Phase 5: distributive? / modular? / validators ─────────

(test-group "distributive?"
  (test #t (distributive? (chain-lattice 5)))
  (test #t (distributive? (boolean-lattice 3)))
  (test #f (distributive? (diamond-lattice 3)))
  (test #f (distributive? (pentagon-lattice))))

(test-group "distributive? preconditions"
  (test-error (distributive? (make-lattice max min 0 10 <=))))

(test-group "modular?"
  (test #t (modular? (chain-lattice 5)))
  (test #t (modular? (boolean-lattice 3)))
  (test #t (modular? (diamond-lattice 3)))   ;; M3 is modular
  (test #f (modular? (pentagon-lattice))))   ;; N5 is not

(test-group "modular? preconditions"
  (test-error (modular? (make-lattice max min 0 10 <=))))

(test-group "validate-distributive-lattice on pentagon — finds violation"
  (let ((violations (validate-distributive-lattice
                     (pentagon-lattice)
                     '(bot a b c top))))
    (test #f (eq? #t violations))
    (test #t (and (list? violations) (positive? (length violations))))
    (test #t (every (lambda (v) (eq? (car v) 'not-distributive)) violations))))

(test-group "validate-distributive-lattice on chain — #t"
  (test #t (validate-distributive-lattice (chain-lattice 4) '(0 1 2 3))))

(test-group "validate-modular-lattice on pentagon — finds violation"
  (let ((violations (validate-modular-lattice
                     (pentagon-lattice)
                     '(bot a b c top))))
    (test #f (eq? #t violations))
    (test #t (every (lambda (v) (eq? (car v) 'not-modular)) violations))))

(test-group "validate-modular-lattice on M3 — #t"
  (let ((elts (lattice-elements (diamond-lattice 3))))
    (test #t (validate-modular-lattice (diamond-lattice 3) elts))))

(test-group "validate-*/setoid variant uses supplied setoid"
  ;; When lattice-setoid is default equal? but elements are numbers,
  ;; the /setoid variant with numeric-setoid gives the same answer.
  (test #t (validate-distributive-lattice/setoid
             (chain-lattice 4) (numeric-setoid) '(0 1 2 3)))
  (test #t (validate-modular-lattice/setoid
             (chain-lattice 4) (numeric-setoid) '(0 1 2 3))))

;; ─── §5.5 Phase 6: Birkhoff roundtrip ────────────────────────────

(test-group "lattice->locally-finite-poset"
  (let* ((L (chain-lattice 4))
         (P (lattice->locally-finite-poset L)))
    (test #t (locally-finite-poset? P))
    (test '(0 1 2 3) (lf-poset-elements P))
    (test #t ((lf-poset-leq? P) 1 3))))

(test-group "lattice->locally-finite-poset preconditions"
  (test-error (lattice->locally-finite-poset (make-lattice max min 0 10 <=))))

(test-group "birkhoff-representation / chain-lattice 4"
  ;; Chain: every non-bottom element is join-irreducible.
  (let* ((C4 (chain-lattice 4))
         (P  (birkhoff-representation C4)))
    (test #t (locally-finite-poset? P))
    (test '(1 2 3) (lf-poset-elements P))
    (test #t ((lf-poset-leq? P) 1 3))))

(test-group "birkhoff-representation / boolean-lattice 3"
  ;; B(3): join-irreducibles are the three 1-element subsets (atoms).
  (let* ((B3 (boolean-lattice 3))
         (P  (birkhoff-representation B3)))
    (test #t (locally-finite-poset? P))
    (test 3 (length (lf-poset-elements P)))))

(test-group "birkhoff-reconstruction round-trip / chain-lattice 4"
  (let* ((L      (chain-lattice 4))
         (P      (birkhoff-representation L))
         (L-back (birkhoff-reconstruction P)))
    (test 4 (lattice-cardinality L-back))
    (test #t (distributive? L-back))))

(test-group "birkhoff-reconstruction round-trip / boolean-lattice 2"
  (let* ((L      (boolean-lattice 2))
         (P      (birkhoff-representation L))
         (L-back (birkhoff-reconstruction P)))
    ;; B(2) has 4 elements = Downsets(2-element antichain) = 4
    (test 4 (lattice-cardinality L-back))
    (test #t (distributive? L-back))))

(test-group "birkhoff-reconstruction preconditions"
  ;; Poset without elements → raises
  (let ((P (make-locally-finite-poset <= (lambda (x y) '()))))
    (test-error (birkhoff-reconstruction P))))

;; ─── §5.5 Phase 7: free-distributive-lattice ─────────────────────

(test-group "free-distributive-lattice cardinality = Dedekind(n)"
  ;; Dedekind numbers D(0..5): 2, 3, 6, 20, 168, 7581.
  ;; D(5) is enabled; takes ~1.5s on reference hardware.
  (test 2    (lattice-cardinality (free-distributive-lattice 0)))
  (test 3    (lattice-cardinality (free-distributive-lattice 1)))
  (test 6    (lattice-cardinality (free-distributive-lattice 2)))
  (test 20   (lattice-cardinality (free-distributive-lattice 3)))
  (test 168  (lattice-cardinality (free-distributive-lattice 4)))
  (test 7581 (lattice-cardinality (free-distributive-lattice 5))))

(test-group "free-distributive-lattice is distributive"
  (test #t (distributive? (free-distributive-lattice 2)))
  (test #t (distributive? (free-distributive-lattice 3))))

(test-group "free-distributive-lattice preconditions"
  (test-error (free-distributive-lattice -1))
  (test-error (free-distributive-lattice 6)))

;; ─── §5.5 — crosscheck/Copilot follow-ups ────────────────────────

(test-group "make-lattice — rejects non-procedure mandatory args"
  (test-error (make-lattice 'not-a-proc min 0 10 <=))
  (test-error (make-lattice max 'not-a-proc 0 10 <=))
  (test-error (make-lattice max min 0 10 'not-a-proc)))

(test-group "make-lattice — rejects cardinality/elements mismatch"
  (test-error (make-lattice max min 0 4 <=
                            (cons 'cardinality 5)
                            (cons 'elements '(0 1 2)))))

(test-group "make-lattice — rejects unknown opts keys"
  (test-error (make-lattice max min 0 10 <= (cons 'nope 42))))

(test-group "make-locally-finite-poset — rejects unknown opts keys"
  (test-error (make-locally-finite-poset
                (lambda (a b) (<= a b))
                (lambda (x y) (iota (+ 1 (- y x)) x))
                (cons 'nope 42))))

(test-group "validate-distributive-lattice/setoid — setoid argument is load-bearing"
  ;; A degenerate setoid that identifies everything should make even
  ;; pentagon pass (no triples can fail when all values are "equal").
  (let ((everything-equal (make-setoid (lambda (a b) #t))))
    (test #t (validate-distributive-lattice/setoid
               (pentagon-lattice) everything-equal '(bot a b c top))))
  ;; A degenerate setoid that refuses equality should fail even on a
  ;; distributive lattice.
  (let ((nothing-equal (make-setoid (lambda (a b) #f))))
    (let ((violations (validate-distributive-lattice/setoid
                        (chain-lattice 3) nothing-equal '(0 1 2))))
      (test #f (eq? #t violations))
      (test #t (positive? (length violations))))))

(test-group "validate-modular-lattice/setoid — setoid argument is load-bearing"
  (let ((everything-equal (make-setoid (lambda (a b) #t))))
    (test #t (validate-modular-lattice/setoid
               (pentagon-lattice) everything-equal '(bot a b c top)))))

(test-group "validate-lattice on §5.5 presets — laws hold"
  (test #t (validate-lattice (chain-lattice 5) '(0 1 2 3 4)))
  (test #t (validate-lattice (boolean-lattice 3)
                              (lattice-elements (boolean-lattice 3))))
  (test #t (validate-lattice (diamond-lattice 3)
                              (lattice-elements (diamond-lattice 3))))
  (test #t (validate-lattice (pentagon-lattice) '(bot a b c top))))

(test-group "boolean-lattice — canonical-order subset representation"
  (let ((B3 (boolean-lattice 3)))
    ;; Join is order-insensitive on inputs AND produces canonical-order
    ;; output regardless of input ordering.
    (test (lattice-join B3 '(0 1) '())
          (lattice-join B3 '(1 0) '()))
    (test #t (and (member '(0 1) (lattice-elements B3)) #t))))

(test-group "birkhoff-representation on non-distributive — raises"
  ;; M3 and N5 are not distributive; the default form gates on
  ;; distributive? and refuses them.
  (test-error (birkhoff-representation (diamond-lattice 3)))
  (test-error (birkhoff-representation (pentagon-lattice))))

(test-group "birkhoff-representation/unchecked on non-distributive — well-formed poset"
  ;; The /unchecked escape hatch skips the distributivity gate. The
  ;; result is still a well-formed <locally-finite-poset> (the poset
  ;; of join-irreducibles), but Birkhoff's roundtrip won't reproduce
  ;; the input — that's a mathematical property of Birkhoff's theorem,
  ;; not a bug. Pin this documented behavior.
  (let ((P-M3 (birkhoff-representation/unchecked (diamond-lattice 3)))
        (P-N5 (birkhoff-representation/unchecked (pentagon-lattice))))
    (test #t (locally-finite-poset? P-M3))
    (test #t (locally-finite-poset? P-N5))
    (test 3 (length (lf-poset-elements P-M3)))
    (test 3 (length (lf-poset-elements P-N5)))))

(test-group "birkhoff-representation/unchecked agrees with gated form on distributive input"
  (let* ((L        (chain-lattice 4))
         (gated    (birkhoff-representation           L))
         (unchecked (birkhoff-representation/unchecked L)))
    (test (lf-poset-elements gated) (lf-poset-elements unchecked))))

(test-group "birkhoff-representation/unchecked preconditions"
  (test-error (birkhoff-representation/unchecked (make-lattice max min 0 10 <=))))

(test-group "trivial lattices — 1-element edge cases"
  ;; chain-lattice 1 is a single-element lattice where bot = top.
  (let ((C1 (chain-lattice 1)))
    (test 1 (lattice-cardinality C1))
    (test '(0) (lattice-elements C1))
    (test 0 (lattice-bottom C1))
    (test 0 (lattice-top C1))
    (test #t (distributive? C1))
    (test #t (modular? C1))
    (test '() (join-irreducibles C1))   ;; only element is bot; no irreducibles
    (test '() (meet-irreducibles C1))))

(test-group "trivial lattices — boolean-lattice 0"
  ;; B(0) is the one-element lattice {()}.
  (let ((B0 (boolean-lattice 0)))
    (test 1 (lattice-cardinality B0))
    (test '(()) (lattice-elements B0))
    (test #t (distributive? B0))
    (test #t (modular? B0))
    (test '() (join-irreducibles B0))))

(test-end)
(test-exit)
