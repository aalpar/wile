;;; (wile algebra lattice) — Lattices, constructors, and fixpoint
;;;
;;; A lattice is a partially ordered set where every pair has a join
;;; (least upper bound) and meet (greatest lower bound), plus bottom
;;; and top elements.

;; ─── Record type ─────────────────────────────
;;
;; The <lattice> record carries five mandatory fields (join-fn, meet-fn,
;; bottom, top, leq-fn) and three optional metadata fields (setoid,
;; cardinality, elements) used by §5.5 machinery (distributivity /
;; modularity checks, join/meet irreducibles, Birkhoff roundtrip).
;; Absent optional fields default to #f except setoid, which defaults to
;; (default-setoid) wrapping R7RS equal?.
;;
;; Cardinality field naming: "lattice-cardinality" (not "lattice-order"
;; or "lattice-size"). "Order" is ambiguous in lattice theory — it can
;; mean the poset ordering OR the cardinality. Sage uses .cardinality();
;; we match. Deliberate asymmetry with <group>'s group-order: the math
;; literatures themselves differ, and consistency with lattice-specific
;; literature outweighs cross-family symmetry.
;;
;; Semantic invariant (caller obligation): (setoid-equiv? setoid a b) ⟺
;; (lattice-equal? L a b) on elements of L. Mismatch is undefined
;; behavior; distributive? and birkhoff-representation use setoid
;; equality internally. fixpoint continues to use lattice-equal? (the
;; antisymmetric-leq?-derived notion), unchanged.

(define-record-type <lattice>
  (%make-lattice join-fn meet-fn bottom top leq-fn
                 setoid cardinality elements)
  lattice?
  (join-fn     lattice-join-fn)
  (meet-fn     lattice-meet-fn)
  (bottom      lattice-bottom)
  (top         lattice-top)
  (leq-fn      lattice-leq-fn)
  (setoid      lattice-setoid)
  (cardinality lattice-cardinality)
  (elements    lattice-elements))

(define (%assv-or opts key fallback)
  (let ((p (assv key opts)))
    (if p (cdr p) fallback)))

(define (%validate-opts-keys site opts known-keys)
  ;; Reject unrecognized opts keys so typos surface at construction
  ;; instead of silently returning the fallback.
  (for-each
    (lambda (pair)
      (unless (and (pair? pair) (memv (car pair) known-keys))
        (error (string-append site ": unknown option key") pair known-keys)))
    opts))

(define (make-lattice join meet bottom top leq? . opts)
  "Construct a lattice from JOIN, MEET, BOTTOM, TOP, and LEQ? predicate.\nJOIN computes the least upper bound of two elements, MEET computes\nthe greatest lower bound. BOTTOM is less than all elements, TOP is\ngreater than all elements. LEQ? tests the partial ordering.\n\nOptional trailing alist entries specify extended metadata:\n  (setoid . S)       — <setoid> carrying element equality (defaults to (default-setoid))\n  (cardinality . N)  — exact integer |L| if known; #f otherwise\n  (elements . LIST)  — enumeration of L's elements; required for distributive?/modular?\n\nExamples:\n  (let ((L (make-lattice max min 0 100 <=)))\n    (lattice-join L 3 7))  => 7\n  (let ((L (make-lattice max min 0 4 <=\n                         (cons 'elements '(0 1 2 3 4))\n                         (cons 'cardinality 5))))\n    (lattice-cardinality L))  => 5\n\nParameters:\n  join : procedure\n  meet : procedure\n  bottom : any\n  top : any\n  leq? : procedure\n  opts : alist\nReturns: any\nCategory: algebra\nKeywords: lattice, bounded, complete, algebraic structure, order theory\n\nSee also: `flat-lattice', `powerset-lattice', `validate-lattice'."
  (%validate-opts-keys "make-lattice" opts
                       '(setoid cardinality elements))
  (%make-lattice join meet bottom top leq?
                 (%assv-or opts 'setoid      (default-setoid))
                 (%assv-or opts 'cardinality #f)
                 (%assv-or opts 'elements    #f)))

(define (lattice-equiv? L a b)
  "Test A and B for equivalence under lattice L's setoid.\nApplies (lattice-setoid L)'s equivalence relation to A and B. This\ncomplements `lattice-equal?' (antisymmetric-leq?-derived) with the\nelement-level carrier equality used by §5.5's distributivity and\nBirkhoff machinery. Callers obligate (lattice-equal? L a b) ⟺\n(lattice-equiv? L a b) on elements of L.\n\nExamples:\n  (lattice-equiv? (make-lattice max min 0 100 <=) 5 5)  => #t\n\nParameters:\n  L : any\n  a : any\n  b : any\nReturns: boolean\nCategory: algebra\nKeywords: equivalence, setoid, carrier equality\n\nSee also: `lattice-equal?', `lattice-setoid'."
  (setoid-equiv? (lattice-setoid L) a b))

(define (finite-lattice? L)
  "Return #t if lattice L carries both a cardinality and an elements\nenumeration. Required by `distributive?`, `modular?`,\n`join-irreducibles`, `meet-irreducibles`, and\n`birkhoff-representation`.\n\nExamples:\n  (finite-lattice? (make-lattice max min 0 100 <=))  => #f\n\nParameters:\n  L : any\nReturns: boolean\nCategory: algebra\nKeywords: finite, enumerate, cardinality\n\nSee also: `lattice-cardinality', `lattice-elements'."
  (and (lattice-cardinality L) (lattice-elements L) #t))

;; ─── Core operations ─────────────────────────

(define (lattice-join L a b)
  "Compute the join (least upper bound) of A and B in lattice L.\nThe join is the smallest element that is greater than or equal\nto both A and B.\n\nExamples:\n  (let ((L (powerset-lattice '(1 2 3))))\n    (lattice-join L '(1) '(2 3)))  => (1 2 3)\n\nParameters:\n  L : any\n  a : any\n  b : any\nReturns: any\nCategory: algebra\nKeywords: join, supremum, lub, least upper bound, union, vee, sqcup, max, or"
  ((lattice-join-fn L) a b))

(define (lattice-meet L a b)
  "Compute the meet (greatest lower bound) of A and B in lattice L.\nThe meet is the largest element that is less than or equal to\nboth A and B.\n\nExamples:\n  (let ((L (powerset-lattice '(1 2 3))))\n    (lattice-meet L '(1 2) '(2 3)))  => (2)\n\nParameters:\n  L : any\n  a : any\n  b : any\nReturns: any\nCategory: algebra\nKeywords: meet, infimum, glb, greatest lower bound, intersection, wedge, sqcap, min, and"
  ((lattice-meet-fn L) a b))

(define (lattice-leq? L a b)
  "Test whether A is less than or equal to B in lattice L.\n\nExamples:\n  (let ((L (powerset-lattice '(1 2 3))))\n    (lattice-leq? L '(1) '(1 2)))  => #t\n  (let ((L (powerset-lattice '(1 2 3))))\n    (lattice-leq? L '(1 3) '(1 2)))  => #f\n\nParameters:\n  L : any\n  a : any\n  b : any\nReturns: boolean\nCategory: algebra\nKeywords: partial order, less than, leq, ordering, subset, subtype"
  ((lattice-leq-fn L) a b))

;; ─── Projection ──────────────────────────────

(define (lattice->partial-order L)
  "Extract the partial order from lattice L.\nThe resulting partial order uses L's leq? predicate.\n\nExamples:\n  (let* ((L (powerset-lattice '(1 2 3)))\n         (po (lattice->partial-order L)))\n    (po-leq? po '(1) '(1 2)))  => #t\n\nParameters:\n  L : any\nReturns: any\nCategory: algebra\nKeywords: forgetful functor, projection, underlying order, extract\n\nSee also: `lattice-leq?', `make-partial-order'."
  (make-partial-order (lattice-leq-fn L)))

;; ─── with-lattice macro ─────────────────────

(define-syntax with-lattice
  (syntax-rules ()
    ((with-lattice L (join meet bottom top leq?) body ...)
     (let ((tmp L))
       (let ((join   (lambda (a b) (lattice-join tmp a b)))
             (meet   (lambda (a b) (lattice-meet tmp a b)))
             (bottom (lattice-bottom tmp))
             (top    (lattice-top tmp))
             (leq?   (lambda (a b) (lattice-leq? tmp a b))))
         body ...)))))

;; ─── Lattice equality (derived from leq?) ───

(define (lattice-equal? L a b)
  "Test whether A and B are equal in lattice L.\nTwo elements are lattice-equal when each is less than or equal\nto the other (antisymmetry of the underlying partial order).\n\nExamples:\n  (let ((L (powerset-lattice '(1 2 3))))\n    (lattice-equal? L '(1 2) '(2 1)))  => #t\n  (let ((L (powerset-lattice '(1 2 3))))\n    (lattice-equal? L '(1) '(1 2)))    => #f\n\nParameters:\n  L : any\n  a : any\n  b : any\nReturns: boolean\nCategory: algebra\nKeywords: antisymmetry, equivalence, lattice equality, equal\n\nSee also: `lattice-leq?'."
  (and (lattice-leq? L a b)
       (lattice-leq? L b a)))

;; ─── Fixpoint ────────────────────────────────

(define fixpoint
  (case-lambda
    ((L f x)
     "Compute the least fixpoint of F starting from X in lattice L.\nIterates F(F(...F(X)...)) until the result stabilizes according\nto lattice-equal?. This is Kleene iteration; F must be monotone\nand L must have no infinite ascending chains for termination.\nWith four arguments, limits iteration to FUEL steps and returns\n#f if the fixpoint is not reached.\n\nExamples:\n  (fixpoint (powerset-lattice '(1 2 3))\n            (lambda (s) (if (member 2 s) s (cons 2 s)))\n            '())  => (2)\n\nParameters:\n  L : any\n  f : procedure\n  x : any\nReturns: any\nCategory: algebra\nKeywords: fixed point, lfp, Kleene iteration, abstract interpretation, dataflow, convergence\n\nSee also: `fixpoint/widen', `lattice-equal?'."
     (let loop ((current x))
       (let ((next (f current)))
         (if (lattice-equal? L current next)
             current
             (loop next)))))
    ((L f x fuel)
     (let loop ((current x) (remaining fuel))
       (if (<= remaining 0) #f
           (let ((next (f current)))
             (if (lattice-equal? L current next)
                 current
                 (loop next (- remaining 1)))))))))

(define (fixpoint/widen L f x widen)
  "Compute a fixpoint of F from X in lattice L using WIDEN to ensure termination.\nLike fixpoint, but applies WIDEN instead of raw join when the\nvalue increases. WIDEN takes (current, next) and must return an\nelement at least as large as their join, and every ascending\nchain under WIDEN must be finite. This guarantees termination\neven when L has infinite ascending chains.\n\nExamples:\n  (let ((L (make-lattice max min 0 100 <=)))\n    (fixpoint/widen L\n      (lambda (x) (+ x 1))\n      0\n      (lambda (cur next) 100)))  => 100\n\nParameters:\n  L : any\n  f : procedure\n  x : any\n  widen : procedure\nReturns: any\nCategory: algebra\nKeywords: widening, abstract interpretation, termination, infinite chains, acceleration\n\nSee also: `fixpoint', `lattice-leq?', `lattice-equal?'."
  (let loop ((current x))
    (let* ((next (f current))
           (widened (if (lattice-leq? L next current)
                       current        ; already stable
                       (widen current next))))
      (if (lattice-equal? L current widened)
          current
          (loop widened)))))

;; ─── Lattice constructors ────────────────────

(define (flat-lattice elements equal?)
  "Construct a flat lattice over ELEMENTS using EQUAL? for comparison.\nIn a flat lattice, all elements are incomparable to each other\nbut sit between a bottom element (less than everything) and a top\nelement (greater than everything). The lattice join of two unequal\nelements is top; their meet is bottom.\n\nExamples:\n  (let ((L (flat-lattice '(a b c) eq?)))\n    (lattice-join L 'a 'a))  => a\n  (let ((L (flat-lattice '(a b c) eq?)))\n    (lattice-join L 'a 'b))  => flat-top\n\nParameters:\n  elements : list\n  equal? : procedure\nReturns: any\nCategory: algebra\nKeywords: flat, discrete, constant propagation, abstract domain, incomparable\n\nSee also: `powerset-lattice', `product-lattice', `make-lattice'."
  (let ((bot 'flat-bottom)
        (top 'flat-top))
    (define (member? x)
      (let loop ((es elements))
        (cond ((null? es) #f)
              ((equal? x (car es)) #t)
              (else (loop (cdr es))))))
    (make-lattice
      ;; join
      (lambda (a b)
        (cond ((eq? a bot) b)
              ((eq? b bot) a)
              ((equal? a b) a)
              (else top)))
      ;; meet
      (lambda (a b)
        (cond ((eq? a top) b)
              ((eq? b top) a)
              ((equal? a b) a)
              (else bot)))
      bot top
      ;; leq?
      (lambda (a b)
        (cond ((eq? a bot) #t)
              ((eq? b top) #t)
              ((equal? a b) #t)
              (else #f))))))

(define (powerset-lattice universe)
  "Construct the powerset lattice over UNIVERSE.\nElements are lists representing subsets. Join is set union,\nmeet is set intersection, bottom is the empty set, top is\nUNIVERSE, and ordering is the subset relation. Membership\nis tested with equal?.\n\nExamples:\n  (let ((L (powerset-lattice '(1 2 3))))\n    (lattice-join L '(1) '(2 3)))  => (1 2 3)\n  (let ((L (powerset-lattice '(1 2 3))))\n    (lattice-meet L '(1 2) '(2 3)))  => (2)\n\nParameters:\n  universe : list\nReturns: any\nCategory: algebra\nKeywords: powerset, set, subset, union, intersection, power set, set lattice, collection\n\nSee also: `flat-lattice', `product-lattice', `map-lattice'."
  (define (subset? a b)
    (cond ((null? a) #t)
          ((member (car a) b) (subset? (cdr a) b))
          (else #f)))
  (define (union a b)
    (cond ((null? a) b)
          ((member (car a) b) (union (cdr a) b))
          (else (cons (car a) (union (cdr a) b)))))
  (define (intersect a b)
    (cond ((null? a) '())
          ((member (car a) b) (cons (car a) (intersect (cdr a) b)))
          (else (intersect (cdr a) b))))
  (make-lattice union intersect '() universe subset?))

(define (product-lattice . lattices)
  "Construct the product lattice from LATTICES applied pointwise.\nElements are lists of the same length as LATTICES. All operations\n(join, meet, leq?) apply component-wise to corresponding elements.\nBottom is the list of all component bottoms; top is the list of\nall component tops.\n\nExamples:\n  (let ((L (product-lattice (make-lattice max min 0 10 <=)\n                             (make-lattice max min 0 10 <=))))\n    (lattice-join L '(1 2) '(3 1)))  => (3 2)\n\nParameters:\n  lattices : list\nReturns: any\nCategory: algebra\nKeywords: product, cartesian, tuple, pointwise, componentwise, pair\n\nSee also: `map-lattice', `flat-lattice', `powerset-lattice'."
  (make-lattice
    ;; join: pointwise
    (lambda (a b) (map (lambda (L ai bi) (lattice-join L ai bi))
                       lattices a b))
    ;; meet: pointwise
    (lambda (a b) (map (lambda (L ai bi) (lattice-meet L ai bi))
                       lattices a b))
    ;; bottom
    (map lattice-bottom lattices)
    ;; top
    (map lattice-top lattices)
    ;; leq?: all components
    (lambda (a b)
      (let loop ((Ls lattices) (as a) (bs b))
        (cond ((null? Ls) #t)
              ((not (lattice-leq? (car Ls) (car as) (car bs))) #f)
              (else (loop (cdr Ls) (cdr as) (cdr bs))))))))

(define (map-lattice keys value-lattice)
  "Construct a lattice of alists mapping KEYS to elements of VALUE-LATTICE.\nAll operations apply pointwise: join, meet, and leq? operate\non corresponding values for each key. Missing keys are treated\nas VALUE-LATTICE's bottom element. Bottom is all keys mapped\nto bottom; top is all keys mapped to top.\n\nExamples:\n  (let ((L (map-lattice '(x y) (make-lattice max min 0 100 <=))))\n    (lattice-join L '((x . 1) (y . 5)) '((x . 3) (y . 2))))\n    => ((x . 3) (y . 5))\n\nParameters:\n  keys : list\n  value-lattice : any\nReturns: any\nCategory: algebra\nKeywords: map, dictionary, environment, pointwise, association, mapping, key-value\n\nSee also: `product-lattice', `powerset-lattice'."
  (let ((vbot (lattice-bottom value-lattice))
        (vtop (lattice-top value-lattice)))
    (define (lookup key alist)
      (let ((pair (assoc key alist)))
        (if pair (cdr pair) vbot)))
    (define (pointwise-binop op a b)
      (map (lambda (k) (cons k (op value-lattice (lookup k a) (lookup k b))))
           keys))
    (make-lattice
      (lambda (a b) (pointwise-binop lattice-join a b))
      (lambda (a b) (pointwise-binop lattice-meet a b))
      (map (lambda (k) (cons k vbot)) keys)
      (map (lambda (k) (cons k vtop)) keys)
      (lambda (a b)
        (let loop ((ks keys))
          (cond ((null? ks) #t)
                ((not (lattice-leq? value-lattice
                                    (lookup (car ks) a)
                                    (lookup (car ks) b)))
                 #f)
                (else (loop (cdr ks)))))))))

;; ─── Validation ──────────────────────────────

;; ─── §5.5 presets ────────────────────────────
;;
;; Five canonical fixtures: chain, boolean, diamond (M3 at n=3),
;; pentagon (N5), and free-distributive (shipped in Phase 7, depends
;; on Birkhoff). These are the same fixtures used throughout lattice
;; theory textbooks (Davey & Priestley, Grätzer) for discussing
;; distributivity / modularity — M3 and N5 are the forbidden
;; sublattices in Birkhoff's characterization theorem.

(define (chain-lattice n)
  "Construct the n-element chain 0 < 1 < ... < n-1.\nDistributive and modular (vacuously — every totally ordered set\nis distributive). Setoid is the numeric-setoid.\n\nExamples:\n  (lattice-elements (chain-lattice 4))  => (0 1 2 3)\n  (lattice-join (chain-lattice 5) 2 4)  => 4\n\nParameters:\n  n : exact positive integer\nReturns: lattice\nCategory: algebra\nKeywords: chain, total order, distributive, canonical lattice, teaching fixture\n\nSee also: `boolean-lattice', `diamond-lattice', `pentagon-lattice'."
  (unless (and (integer? n) (positive? n))
    (error "chain-lattice: n must be a positive integer" n))
  (let ((elts (let loop ((i 0) (acc '()))
                (if (>= i n) (reverse acc)
                    (loop (+ i 1) (cons i acc))))))
    (make-lattice
      max min 0 (- n 1) <=
      (cons 'setoid      (numeric-setoid))
      (cons 'cardinality n)
      (cons 'elements    elts))))

(define (%sort-by-canonical-order xs canonical)
  ;; Return xs reordered to match the appearance order of CANONICAL.
  ;; Used to produce canonical representations of subsets so that
  ;; equal? recognises them regardless of insertion order.
  (let loop ((cs canonical) (acc '()))
    (cond
      ((null? cs) (reverse acc))
      ((member (car cs) xs)
       (loop (cdr cs) (cons (car cs) acc)))
      (else
       (loop (cdr cs) acc)))))

(define (%all-subsets universe)
  ;; Enumerate all subsets of UNIVERSE, each in UNIVERSE's order.
  ;; Returns 2^|universe| subsets in the standard "binary counting"
  ;; order: () first, then subsets containing the first element, etc.
  (if (null? universe)
      '(())
      (let ((rest (%all-subsets (cdr universe)))
            (x    (car universe)))
        (append rest
                (map (lambda (s) (cons x s)) rest)))))

(define (boolean-lattice n)
  "Construct the Boolean lattice B(n) = 2^[n] of subsets of an n-element\nuniverse, ordered by inclusion.\n\nElements are canonical-order sublists of (0 1 ... n-1) (carried in\nlattice-elements). Join is set union, meet is set intersection, bottom\nis the empty set, top is the full universe. Distributive and modular.\n\nExamples:\n  (lattice-cardinality (boolean-lattice 3))  => 8\n  (lattice-bottom    (boolean-lattice 3))   => ()\n\nParameters:\n  n : exact non-negative integer\nReturns: lattice\nCategory: algebra\nKeywords: boolean, powerset, subset, distributive, canonical lattice\n\nSee also: `powerset-lattice', `chain-lattice'."
  (unless (and (integer? n) (not (negative? n)))
    (error "boolean-lattice: n must be a non-negative integer" n))
  (let* ((universe (let loop ((i 0) (acc '()))
                     (if (>= i n) (reverse acc)
                         (loop (+ i 1) (cons i acc)))))
         (elts     (%all-subsets universe)))
    (define (subset? a b)
      (cond ((null? a) #t)
            ((member (car a) b) (subset? (cdr a) b))
            (else #f)))
    (define (canon s) (%sort-by-canonical-order s universe))
    (define (union a b)
      (canon
        (let loop ((xs b) (acc a))
          (cond ((null? xs) acc)
                ((member (car xs) acc) (loop (cdr xs) acc))
                (else (loop (cdr xs) (cons (car xs) acc)))))))
    (define (intersect a b)
      (canon
        (let loop ((xs a) (acc '()))
          (cond ((null? xs) acc)
                ((member (car xs) b) (loop (cdr xs) (cons (car xs) acc)))
                (else (loop (cdr xs) acc))))))
    (make-lattice union intersect '() universe subset?
                  (cons 'cardinality (length elts))
                  (cons 'elements    elts))))

(define (%diamond-leq? n a b)
  ;; Ordering on diamond(n): bot < atom_i < top for every i; atoms
  ;; mutually incomparable. a == b is the reflexive case.
  (cond
    ((equal? a b) #t)
    ((eq? a 'bot) #t)
    ((eq? b 'top) #t)
    ((eq? a 'top) #f)
    ((eq? b 'bot) #f)
    ;; both are atoms, distinct
    (else #f)))

(define (%diamond-join n a b)
  (cond
    ((equal? a b) a)
    ((eq? a 'bot) b)
    ((eq? b 'bot) a)
    ((eq? a 'top) 'top)
    ((eq? b 'top) 'top)
    ;; two distinct atoms
    (else 'top)))

(define (%diamond-meet n a b)
  (cond
    ((equal? a b) a)
    ((eq? a 'top) b)
    ((eq? b 'top) a)
    ((eq? a 'bot) 'bot)
    ((eq? b 'bot) 'bot)
    ;; two distinct atoms
    (else 'bot)))

(define (diamond-lattice n)
  "Construct the rank-3 diamond lattice with N atoms: ⊥, atom_0, ...,\natom_{N-1}, ⊤, with no comparabilities among atoms.\n\nModular for every N ≥ 3; not distributive for N ≥ 3 (distinct atoms\na, b give a ⋀ (b ⋁ c) ≠ (a ⋀ b) ⋁ (a ⋀ c) for appropriate choices).\nM_3 = (diamond-lattice 3) is the canonical counterexample for\ndistributivity in Birkhoff's theorem.\n\nExamples:\n  (lattice-cardinality (diamond-lattice 3))  => 5\n  (lattice-join (diamond-lattice 3) '(atom 0) '(atom 1))  => top\n\nParameters:\n  n : exact integer ≥ 3\nReturns: lattice\nCategory: algebra\nKeywords: diamond, M3, modular, counterexample, forbidden sublattice, Birkhoff\n\nSee also: `pentagon-lattice', `chain-lattice'."
  (unless (and (integer? n) (>= n 3))
    (error "diamond-lattice: n must be an integer ≥ 3" n))
  (let* ((atoms (let loop ((i 0) (acc '()))
                  (if (>= i n) (reverse acc)
                      (loop (+ i 1) (cons (list 'atom i) acc)))))
         (elts  (cons 'bot (append atoms (list 'top)))))
    (make-lattice
      (lambda (a b) (%diamond-join n a b))
      (lambda (a b) (%diamond-meet n a b))
      'bot 'top
      (lambda (a b) (%diamond-leq? n a b))
      (cons 'cardinality (+ n 2))
      (cons 'elements    elts))))

(define (%pentagon-leq? a b)
  ;; N5 Hasse: bot < a < top; bot < b < c < top; a ⟂ b, a ⟂ c
  (cond
    ((equal? a b) #t)
    ((eq? a 'bot) #t)
    ((eq? b 'top) #t)
    ((and (eq? a 'b) (eq? b 'c)) #t)  ;; b ≤ c
    (else #f)))

(define (%pentagon-join a b)
  (cond
    ((equal? a b) a)
    ((eq? a 'bot) b)
    ((eq? b 'bot) a)
    ((eq? a 'top) 'top)
    ((eq? b 'top) 'top)
    ;; three atoms: a, b, c; b < c; a incomparable with b and c
    ((and (eq? a 'b) (eq? b 'c)) 'c)
    ((and (eq? a 'c) (eq? b 'b)) 'c)
    ;; any mix containing 'a with another non-bot/non-top → top
    ((or (eq? a 'a) (eq? b 'a)) 'top)
    (else 'top)))

(define (%pentagon-meet a b)
  (cond
    ((equal? a b) a)
    ((eq? a 'top) b)
    ((eq? b 'top) a)
    ((eq? a 'bot) 'bot)
    ((eq? b 'bot) 'bot)
    ;; b ⋀ c = b
    ((and (eq? a 'b) (eq? b 'c)) 'b)
    ((and (eq? a 'c) (eq? b 'b)) 'b)
    ;; any mix with 'a and (b or c) → bot
    ((or (eq? a 'a) (eq? b 'a)) 'bot)
    (else 'bot)))

(define (pentagon-lattice)
  "Construct the pentagon lattice N_5: {⊥, a, b, c, ⊤} with ordering\n⊥ < a < ⊤ and ⊥ < b < c < ⊤, where a is incomparable to both b and c.\n\nNeither modular nor distributive: N_5 is the Birkhoff-theorem\nforbidden sublattice whose presence characterises non-modular lattices.\n\nExamples:\n  (lattice-join (pentagon-lattice) 'b 'c)   => c\n  (lattice-meet (pentagon-lattice) 'a 'c)   => bot\n\nReturns: lattice\nCategory: algebra\nKeywords: pentagon, N5, non-modular, counterexample, forbidden sublattice, Birkhoff\n\nSee also: `diamond-lattice', `chain-lattice'."
  (make-lattice
    %pentagon-join
    %pentagon-meet
    'bot 'top
    %pentagon-leq?
    (cons 'cardinality 5)
    (cons 'elements '(bot a b c top))))

;; ─── §5.5 Irreducibles ───────────────────────
;;
;; An element j ∈ L is join-irreducible iff j ≠ ⊥ and j = a ⋁ b implies
;; j = a or j = b. Equivalently, j has exactly one lower cover in the
;; Hasse diagram. Dual: m is meet-irreducible iff m ≠ ⊤ and m has
;; exactly one upper cover.
;;
;; Algorithm (O(|L|²) per element): compute lower-covers(L, x) by
;; filtering { y : y < x, no z with y < z < x }; the cardinality-1 case
;; is the irreducibility witness.

(define (%lower-covers L x)
  ;; Elements y with y < x (strictly less, using lattice's setoid for
  ;; equality) and no z with y < z < x. Internal.
  (let* ((elts   (lattice-elements L))
         (eq     (lattice-setoid L))
         (below  (filter
                   (lambda (y) (and (lattice-leq? L y x)
                                    (not (setoid-equiv? eq y x))))
                   elts)))
    (filter
      (lambda (y)
        (not (any
               (lambda (z)
                 (and (not (setoid-equiv? eq z y))
                      (not (setoid-equiv? eq z x))
                      (lattice-leq? L y z)
                      (lattice-leq? L z x)))
               below)))
      below)))

(define (%upper-covers L x)
  (let* ((elts   (lattice-elements L))
         (eq     (lattice-setoid L))
         (above  (filter
                   (lambda (y) (and (lattice-leq? L x y)
                                    (not (setoid-equiv? eq y x))))
                   elts)))
    (filter
      (lambda (y)
        (not (any
               (lambda (z)
                 (and (not (setoid-equiv? eq z y))
                      (not (setoid-equiv? eq z x))
                      (lattice-leq? L x z)
                      (lattice-leq? L z y)))
               above)))
      above)))

(define (join-irreducible? L x)
  "Return #t if X is join-irreducible in lattice L.\nAn element j is join-irreducible iff j ≠ ⊥ and j has exactly one\nlower cover (i.e. exactly one element immediately below it in the\nHasse diagram). Requires a finite lattice.\n\nExamples:\n  (join-irreducible? (chain-lattice 4) 2)  => #t\n  (join-irreducible? (chain-lattice 4) 0)  => #f\n\nParameters:\n  L : lattice\n  x : any\nReturns: boolean\nCategory: algebra\nKeywords: join irreducible, atom, Hasse cover, Birkhoff, irreducibility\n\nSee also: `join-irreducibles', `meet-irreducible?'."
  (unless (finite-lattice? L)
    (error "join-irreducible?: requires finite lattice"
           'fix "pass (cons 'elements LIST) to make-lattice"))
  (and (not (setoid-equiv? (lattice-setoid L) x (lattice-bottom L)))
       (= 1 (length (%lower-covers L x)))))

(define (meet-irreducible? L x)
  "Return #t if X is meet-irreducible in lattice L.\nAn element m is meet-irreducible iff m ≠ ⊤ and m has exactly one\nupper cover. Dual of `join-irreducible?'. Requires a finite lattice.\n\nExamples:\n  (meet-irreducible? (chain-lattice 4) 2)  => #t\n  (meet-irreducible? (chain-lattice 4) 3)  => #f\n\nParameters:\n  L : lattice\n  x : any\nReturns: boolean\nCategory: algebra\nKeywords: meet irreducible, coatom, Hasse cover, irreducibility\n\nSee also: `meet-irreducibles', `join-irreducible?'."
  (unless (finite-lattice? L)
    (error "meet-irreducible?: requires finite lattice"
           'fix "pass (cons 'elements LIST) to make-lattice"))
  (and (not (setoid-equiv? (lattice-setoid L) x (lattice-top L)))
       (= 1 (length (%upper-covers L x)))))

(define (join-irreducibles L)
  "Return the list of join-irreducibles of finite lattice L, in\n(lattice-elements L) order.\n\nRequires a finite lattice. These are the elements that cannot be\nexpressed as a non-trivial join; equivalently, each has exactly one\nlower cover. Birkhoff's fundamental theorem of finite distributive\nlattices uses this set as the domain of the poset dual to L.\n\nExamples:\n  (join-irreducibles (chain-lattice 4))  => (1 2 3)\n  (length (join-irreducibles (boolean-lattice 3)))  => 3\n\nParameters:\n  L : lattice\nReturns: list\nCategory: algebra\nKeywords: join irreducibles, Birkhoff, atoms, irreducibility, distributive lattice\n\nSee also: `meet-irreducibles', `birkhoff-representation'."
  (unless (finite-lattice? L)
    (error "join-irreducibles: requires finite lattice"
           'fix "pass (cons 'elements LIST) to make-lattice"))
  (filter (lambda (x) (join-irreducible? L x)) (lattice-elements L)))

(define (meet-irreducibles L)
  "Return the list of meet-irreducibles of finite lattice L, in\n(lattice-elements L) order. Dual of `join-irreducibles'.\n\nExamples:\n  (length (meet-irreducibles (boolean-lattice 3)))  => 3\n\nParameters:\n  L : lattice\nReturns: list\nCategory: algebra\nKeywords: meet irreducibles, coatoms, irreducibility\n\nSee also: `join-irreducibles'."
  (unless (finite-lattice? L)
    (error "meet-irreducibles: requires finite lattice"
           'fix "pass (cons 'elements LIST) to make-lattice"))
  (filter (lambda (x) (meet-irreducible? L x)) (lattice-elements L)))

;; ─── Validation ──────────────────────────────

(define (validate-lattice L samples)
  "Spot-check that L satisfies the lattice laws on SAMPLES.\nTests join and meet commutativity, absorption, idempotence,\nand identity (bottom for join, top for meet) for all elements\nand pairs in SAMPLES. Returns #t if all laws hold, or a list\nof (violation-type element ...) entries describing failures.\n\nExamples:\n  (validate-lattice (flat-lattice '(a b c) eq?) '(a b c))  => #t\n\nParameters:\n  L : any\n  samples : list\nReturns: any\nCategory: algebra\nKeywords: commutativity, absorption, idempotence, identity, law checking, validation\n\nSee also: `make-lattice', `lattice-join', `lattice-meet'."
  (let ((violations '()))
    (define (fail! type . args)
      (set! violations (cons (cons type args) violations)))
    (for-each
      (lambda (a)
        (for-each
          (lambda (b)
            ;; Commutativity of join
            (unless (lattice-equal? L (lattice-join L a b) (lattice-join L b a))
              (fail! 'join-commutativity a b))
            ;; Commutativity of meet
            (unless (lattice-equal? L (lattice-meet L a b) (lattice-meet L b a))
              (fail! 'meet-commutativity a b))
            ;; Absorption: a ⊔ (a ⊓ b) = a
            (unless (lattice-equal? L (lattice-join L a (lattice-meet L a b)) a)
              (fail! 'absorption-join a b))
            ;; Absorption: a ⊓ (a ⊔ b) = a
            (unless (lattice-equal? L (lattice-meet L a (lattice-join L a b)) a)
              (fail! 'absorption-meet a b)))
          samples)
        ;; Idempotence
        (unless (lattice-equal? L (lattice-join L a a) a)
          (fail! 'join-idempotence a))
        (unless (lattice-equal? L (lattice-meet L a a) a)
          (fail! 'meet-idempotence a))
        ;; Identity: bottom is join identity
        (unless (lattice-equal? L (lattice-join L (lattice-bottom L) a) a)
          (fail! 'join-identity a))
        ;; Identity: top is meet identity
        (unless (lattice-equal? L (lattice-meet L (lattice-top L) a) a)
          (fail! 'meet-identity a)))
      samples)
    (if (null? violations) #t (reverse violations))))
