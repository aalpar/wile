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

(define (make-lattice join meet bottom top leq? . opts)
  "Construct a lattice from JOIN, MEET, BOTTOM, TOP, and LEQ? predicate.\nJOIN computes the least upper bound of two elements, MEET computes\nthe greatest lower bound. BOTTOM is less than all elements, TOP is\ngreater than all elements. LEQ? tests the partial ordering.\n\nOptional trailing alist entries specify extended metadata:\n  (setoid . S)       — <setoid> carrying element equality (defaults to (default-setoid))\n  (cardinality . N)  — exact integer |L| if known; #f otherwise\n  (elements . LIST)  — enumeration of L's elements; required for distributive?/modular?\n\nExamples:\n  (let ((L (make-lattice max min 0 100 <=)))\n    (lattice-join L 3 7))  => 7\n  (let ((L (make-lattice max min 0 4 <=\n                         (cons 'elements '(0 1 2 3 4))\n                         (cons 'cardinality 5))))\n    (lattice-cardinality L))  => 5\n\nParameters:\n  join : procedure\n  meet : procedure\n  bottom : any\n  top : any\n  leq? : procedure\n  opts : alist\nReturns: any\nCategory: algebra\nKeywords: lattice, bounded, complete, algebraic structure, order theory\n\nSee also: `flat-lattice', `powerset-lattice', `validate-lattice'."
  (unless (procedure? join)
    (error "make-lattice: join must be a procedure" join))
  (unless (procedure? meet)
    (error "make-lattice: meet must be a procedure" meet))
  (unless (procedure? leq?)
    (error "make-lattice: leq? must be a procedure" leq?))
  (validate-opts-keys "make-lattice" opts
                       '(setoid cardinality elements))
  (let ((card (assv-or opts 'cardinality #f))
        (elts (assv-or opts 'elements    #f)))
    (when (and card elts (not (= card (length elts))))
      (error "make-lattice: cardinality does not match (length elements)"
             'cardinality card 'length (length elts)))
    (%make-lattice join meet bottom top leq?
                   (assv-or opts 'setoid (default-setoid))
                   card
                   elts)))

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
     ;; Backstop cap (mirrors dataflow.scm's max-revisits=100000). The lattice
     ;; height is opaque here — f keeps ascending on an infinite-height lattice
     ;; (integers, intervals) and Kleene iteration never converges. Turn that
     ;; silent hang into a remedy-pointing error. Steps are O(1), so the cap
     ;; fires promptly. The 4-arg (fixpoint L f x FUEL) form below opts out.
     (let loop ((current x) (steps 0))
       (when (>= steps 100000)
         (error (string-append
                 "fixpoint: exceeded 100000 iterations without converging — the"
                 " lattice likely has infinite ascending chains (e.g. integers or"
                 " intervals) and f keeps ascending. Remedies: use (fixpoint/widen"
                 " L f x WIDEN) with a chain-finite widening operator, or the 4-arg"
                 " (fixpoint L f x FUEL) form which returns #f after FUEL steps")
                (list 'current current 'iterations steps)))
       (let ((next (f current)))
         (if (lattice-equal? L current next)
             current
             (loop next (+ steps 1))))))
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

(define (two-point-lattice)
  "Construct the two-point Boolean (truth-value) lattice on {#f, #t}:\nbottom=#f, top=#t, join=or, meet=and, leq?=implication. This is the\n#f/#t-carried truth lattice used by reachability-style analyses; it is\nDISTINCT from `boolean-lattice', which builds the powerset lattice 2^[n]\nof subset-valued elements.\n\nExamples:\n  (lattice-join (two-point-lattice) #f #t)  => #t\n  (lattice-meet (two-point-lattice) #t #t)  => #t\n  (lattice-bottom (two-point-lattice))      => #f\n\nReturns: lattice\nCategory: algebra\nKeywords: boolean, truth value, two-point, distributive, canonical lattice\n\nSee also: `boolean-lattice', `chain-lattice', `flat-lattice'."
  (make-lattice
    (lambda (a b) (or a b))            ; join  = logical or
    (lambda (a b) (and a b))           ; meet  = logical and
    #f                                  ; bottom
    #t                                  ; top
    (lambda (a b) (or (not a) b))       ; leq?  = implication
    (cons 'setoid      (default-setoid)) ; carrier equality on #f/#t
    (cons 'cardinality 2)
    (cons 'elements    '(#f #t))))

(define (boolean-lattice n)
  "Construct the Boolean lattice B(n) = 2^[n] of subsets of an n-element\nuniverse, ordered by inclusion.\n\nElements are canonical-order sublists of (0 1 ... n-1) (carried in\nlattice-elements). Join is set union, meet is set intersection, bottom\nis the empty set, top is the full universe. Distributive and modular.\n\nExamples:\n  (lattice-cardinality (boolean-lattice 3))  => 8\n  (lattice-bottom    (boolean-lattice 3))   => ()\n\nParameters:\n  n : exact non-negative integer\nReturns: lattice\nCategory: algebra\nKeywords: boolean, powerset, subset, distributive, canonical lattice\n\nSee also: `powerset-lattice', `chain-lattice'."
  (unless (and (integer? n) (not (negative? n)))
    (error "boolean-lattice: n must be a non-negative integer" n))
  (let* ((universe (let loop ((i 0) (acc '()))
                     (if (>= i n) (reverse acc)
                         (loop (+ i 1) (cons i acc)))))
         (elts     (%all-subsets universe)))
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
    (make-lattice union intersect '() universe %subset-of?
                  (cons 'setoid      (default-setoid))
                  (cons 'cardinality (length elts))
                  (cons 'elements    elts))))

(define (%diamond-leq? x y)
  ;; Ordering on diamond(n): bot < atom_i < top for every i; atoms
  ;; mutually incomparable. x == y is the reflexive case.
  (cond
    ((equal? x y) #t)
    ((eq? x 'bot) #t)
    ((eq? y 'top) #t)
    ((eq? x 'top) #f)
    ((eq? y 'bot) #f)
    ;; both are atoms, distinct
    (else #f)))

(define (%diamond-join x y)
  (cond
    ((equal? x y) x)
    ((eq? x 'bot) y)
    ((eq? y 'bot) x)
    ((eq? x 'top) 'top)
    ((eq? y 'top) 'top)
    ;; two distinct atoms
    (else 'top)))

(define (%diamond-meet x y)
  (cond
    ((equal? x y) x)
    ((eq? x 'top) y)
    ((eq? y 'top) x)
    ((eq? x 'bot) 'bot)
    ((eq? y 'bot) 'bot)
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
      %diamond-join
      %diamond-meet
      'bot 'top
      %diamond-leq?
      (cons 'setoid      (default-setoid))
      (cons 'cardinality (+ n 2))
      (cons 'elements    elts))))

;; Pentagon helpers — parameters are named x, y to avoid collision with
;; the element symbols 'a, 'b, 'c that appear as literals below.
(define (%pentagon-leq? x y)
  ;; N5 Hasse: bot < a < top; bot < b < c < top; a ⟂ b, a ⟂ c
  (cond
    ((equal? x y) #t)
    ((eq? x 'bot) #t)
    ((eq? y 'top) #t)
    ((and (eq? x 'b) (eq? y 'c)) #t)  ;; b ≤ c
    (else #f)))

(define (%pentagon-join x y)
  (cond
    ((equal? x y) x)
    ((eq? x 'bot) y)
    ((eq? y 'bot) x)
    ((eq? x 'top) 'top)
    ((eq? y 'top) 'top)
    ;; three atoms: 'a, 'b, 'c; b < c; a incomparable with b and c
    ((and (eq? x 'b) (eq? y 'c)) 'c)
    ((and (eq? x 'c) (eq? y 'b)) 'c)
    ;; any remaining mix (involves 'a and ('b or 'c)) → top
    (else 'top)))

(define (%pentagon-meet x y)
  (cond
    ((equal? x y) x)
    ((eq? x 'top) y)
    ((eq? y 'top) x)
    ((eq? x 'bot) 'bot)
    ((eq? y 'bot) 'bot)
    ;; b ⋀ c = b
    ((and (eq? x 'b) (eq? y 'c)) 'b)
    ((and (eq? x 'c) (eq? y 'b)) 'b)
    ;; any remaining mix (involves 'a and ('b or 'c)) → bot
    (else 'bot)))

(define (pentagon-lattice)
  "Construct the pentagon lattice N_5: {⊥, a, b, c, ⊤} with ordering\n⊥ < a < ⊤ and ⊥ < b < c < ⊤, where a is incomparable to both b and c.\n\nNeither modular nor distributive: N_5 is the Birkhoff-theorem\nforbidden sublattice whose presence characterises non-modular lattices.\n\nExamples:\n  (lattice-join (pentagon-lattice) 'b 'c)   => c\n  (lattice-meet (pentagon-lattice) 'a 'c)   => bot\n\nReturns: lattice\nCategory: algebra\nKeywords: pentagon, N5, non-modular, counterexample, forbidden sublattice, Birkhoff\n\nSee also: `diamond-lattice', `chain-lattice'."
  (make-lattice
    %pentagon-join
    %pentagon-meet
    'bot 'top
    %pentagon-leq?
    (cons 'setoid      (default-setoid))
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

;; ─── §5.5 Distributivity / modularity ────────
;;
;; `distributive?` tests ∀ a,b,c: a ⋀ (b ⋁ c) = (a ⋀ b) ⋁ (a ⋀ c)
;; `modular?`      tests ∀ a,b,c with a ≤ c: a ⋁ (b ⋀ c) = (a ⋁ b) ⋀ c
;;
;; Both do exhaustive axiom check on (lattice-elements L) using the
;; lattice's setoid for equality, O(|L|³). Early exit on first
;; violating triple. Matches GAP / Sage convention (axiom check, not
;; forbidden-sublattice structural check — same correctness, ~|L|²
;; faster, no subset enumeration).
;;
;; `distributive?` implies `modular?`; the converse is false
;; (M3 is modular, not distributive — (diamond-lattice 3) witnesses
;; this). Independent checks keep diagnostics honest: a #f from one
;; does not short-circuit the other.

(define (%distributive-triple-axiom? L a b c)
  (let ((eq  (lattice-setoid L))
        (lhs (lattice-meet L a (lattice-join L b c)))
        (rhs (lattice-join L (lattice-meet L a b)
                              (lattice-meet L a c))))
    (setoid-equiv? eq lhs rhs)))

(define (%modular-triple-axiom? L a b c)
  ;; Precondition: a ≤ c. Returns #t when the modular law holds on
  ;; (a, b, c).
  (let ((eq  (lattice-setoid L))
        (lhs (lattice-join L a (lattice-meet L b c)))
        (rhs (lattice-meet L (lattice-join L a b) c)))
    (setoid-equiv? eq lhs rhs)))

(define (distributive? L)
  "Return #t if finite lattice L satisfies the distributive law\na ⋀ (b ⋁ c) = (a ⋀ b) ⋁ (a ⋀ c) for all triples.\n\nExhaustive axiom check over (lattice-elements L), using\n(lattice-setoid L) for equality. Cost: O(|L|³). Returns #t on\nsuccess or #f on the first violating triple (early exit).\n\nRequires a finite lattice. distributive? implies modular?; the\nconverse is false — see M3 = (diamond-lattice 3) for a modular\nbut not distributive lattice.\n\nExamples:\n  (distributive? (chain-lattice 5))     => #t\n  (distributive? (boolean-lattice 3))   => #t\n  (distributive? (diamond-lattice 3))   => #f  (M3)\n  (distributive? (pentagon-lattice))    => #f  (N5)\n\nParameters:\n  L : lattice\nReturns: boolean\nCategory: algebra\nKeywords: distributive, lattice axiom, Birkhoff, distributivity, canonical lattice\n\nSee also: `modular?', `validate-distributive-lattice'."
  (unless (finite-lattice? L)
    (error "distributive?: requires finite lattice (elements enumerated)"
           'fix "pass (cons 'elements LIST) to make-lattice"))
  (let ((elts (lattice-elements L)))
    (let outer ((as elts))
      (cond
        ((null? as) #t)
        (else
         (let middle ((bs elts))
           (cond
             ((null? bs) (outer (cdr as)))
             (else
              (let inner ((cs elts))
                (cond
                  ((null? cs) (middle (cdr bs)))
                  ((%distributive-triple-axiom? L (car as) (car bs) (car cs))
                   (inner (cdr cs)))
                  (else #f)))))))))))

(define (modular? L)
  "Return #t if finite lattice L satisfies the modular law\na ⋁ (b ⋀ c) = (a ⋁ b) ⋀ c for every triple with a ≤ c.\n\nExhaustive axiom check on qualifying triples, O(|L|³) worst case\n(filter reduces the constant factor). Requires a finite lattice.\n\nEvery distributive lattice is modular; the pentagon (N_5) is the\ncanonical non-modular lattice (join/meet of a with b,c depends on\norder of operations). M_3 is modular but not distributive.\n\nExamples:\n  (modular? (chain-lattice 5))      => #t\n  (modular? (diamond-lattice 3))    => #t  (M3)\n  (modular? (pentagon-lattice))     => #f  (N5)\n\nParameters:\n  L : lattice\nReturns: boolean\nCategory: algebra\nKeywords: modular, lattice axiom, Dedekind, modularity, canonical lattice\n\nSee also: `distributive?', `validate-modular-lattice'."
  (unless (finite-lattice? L)
    (error "modular?: requires finite lattice (elements enumerated)"
           'fix "pass (cons 'elements LIST) to make-lattice"))
  (let ((elts (lattice-elements L)))
    (let outer ((as elts))
      (cond
        ((null? as) #t)
        (else
         (let middle ((bs elts))
           (cond
             ((null? bs) (outer (cdr as)))
             (else
              (let inner ((cs elts))
                (cond
                  ((null? cs) (middle (cdr bs)))
                  ((not (lattice-leq? L (car as) (car cs)))
                   (inner (cdr cs)))
                  ((%modular-triple-axiom? L (car as) (car bs) (car cs))
                   (inner (cdr cs)))
                  (else #f)))))))))))

;; Sample-based validators — same-shape return as validate-group:
;;   #t if no violations; list of (violation-type args...) entries
;;   otherwise. Each entry shape: (not-distributive a b c lhs rhs) or
;;   (not-modular a b c lhs rhs). Useful for:
;;     - spot-checking tier-3 lattices (where distributive? is not
;;       applicable because no element enumeration exists),
;;     - regression-guarding expensive lattices,
;;     - teaching / debugging.

(define (%validate-distributive-impl setoid L samples)
  ;; Check the distributive law a ⋀ (b ⋁ c) = (a ⋀ b) ⋁ (a ⋀ c) on
  ;; every triple from SAMPLES. Collect (not-distributive a b c lhs rhs)
  ;; entries; return #t on clean run.
  (let ((fail! (make-violation-reporter)))
    (for-each
      (lambda (a)
        (for-each
          (lambda (b)
            (for-each
              (lambda (c)
                (let ((lhs (lattice-meet L a (lattice-join L b c)))
                      (rhs (lattice-join L (lattice-meet L a b)
                                           (lattice-meet L a c))))
                  (unless (setoid-equiv? setoid lhs rhs)
                    (fail! 'not-distributive a b c lhs rhs))))
              samples))
          samples))
      samples)
    (fail!)))

(define (%validate-modular-impl setoid L samples)
  ;; Check the modular law a ⋁ (b ⋀ c) = (a ⋁ b) ⋀ c on triples with
  ;; a ≤ c (trivially true otherwise). Collect (not-modular a b c lhs rhs)
  ;; entries; return #t on clean run.
  (let ((fail! (make-violation-reporter)))
    (for-each
      (lambda (a)
        (for-each
          (lambda (b)
            (for-each
              (lambda (c)
                (when (lattice-leq? L a c)
                  (let ((lhs (lattice-join L a (lattice-meet L b c)))
                        (rhs (lattice-meet L (lattice-join L a b) c)))
                    (unless (setoid-equiv? setoid lhs rhs)
                      (fail! 'not-modular a b c lhs rhs)))))
              samples))
          samples))
      samples)
    (fail!)))

(define (validate-distributive-lattice L samples)
  "Spot-check lattice L's distributive axiom over every triple in SAMPLES.\nUses L's setoid for equality. Returns #t on success or a list of\n(not-distributive a b c lhs rhs) entries naming the first violations.\nSampling cost: O(|samples|³).\n\nExamples:\n  (validate-distributive-lattice (pentagon-lattice) '(bot a b c top))\n    => ((not-distributive a b c lhs rhs) ...)\n\nParameters:\n  L : lattice\n  samples : list\nReturns: any\nCategory: algebra\nKeywords: distributive, validation, axiom, spot check, samples\n\nSee also: `distributive?', `validate-distributive-lattice/setoid'."
  (%validate-distributive-impl (lattice-setoid L) L samples))

(define (validate-distributive-lattice/setoid L S samples)
  "Spot-check L's distributive axiom using SETOID S for equality.\nOtherwise identical to `validate-distributive-lattice'. Useful when\nthe lattice's carrier equality is non-default (e.g. numeric, string,\nor a custom quotient).\n\nExamples:\n  (validate-distributive-lattice/setoid\n    (chain-lattice 4) (numeric-setoid) '(0 1 2 3))  => #t\n\nParameters:\n  L : lattice\n  setoid : setoid\n  samples : list\nReturns: any\nCategory: algebra\nKeywords: distributive, validation, setoid, axiom, spot check\n\nSee also: `validate-distributive-lattice'."
  (%validate-distributive-impl S L samples))

(define (validate-modular-lattice L samples)
  "Spot-check lattice L's modular axiom over triples (a,b,c) with a ≤ c.\nUses L's setoid. Returns #t on success or a list of\n(not-modular a b c lhs rhs) entries.\n\nExamples:\n  (validate-modular-lattice (pentagon-lattice) '(bot a b c top))\n    => ((not-modular ...) ...)\n\nParameters:\n  L : lattice\n  samples : list\nReturns: any\nCategory: algebra\nKeywords: modular, validation, axiom, spot check, Dedekind\n\nSee also: `modular?', `validate-modular-lattice/setoid'."
  (%validate-modular-impl (lattice-setoid L) L samples))

(define (validate-modular-lattice/setoid L S samples)
  "Spot-check L's modular axiom using SETOID S for equality.\nOtherwise identical to `validate-modular-lattice'.\n\nParameters:\n  L : lattice\n  setoid : setoid\n  samples : list\nReturns: any\nCategory: algebra\nKeywords: modular, validation, setoid, Dedekind, axiom\n\nSee also: `validate-modular-lattice'."
  (%validate-modular-impl S L samples))

;; ─── §5.5 Birkhoff roundtrip ─────────────────
;;
;; Birkhoff (1937): every finite distributive lattice L is isomorphic
;; to the lattice of downsets of its poset of join-irreducibles
;; (Irr(L)), ordered by subset inclusion. Dually, every finite poset P
;; arises as Irr(L) of the lattice L = Downsets(P). We ship both
;; directions:
;;
;;   birkhoff-representation  : finite distributive L → Irr(L)
;;                              (as <locally-finite-poset>)
;;   birkhoff-reconstruction  : P → Downsets(P) as <lattice>
;;
;; Roundtrip preserves the isomorphism class (distributive lattice ↔
;; finite poset). Pre-shipped `lattice->locally-finite-poset` is a
;; forgetful projection independent of Birkhoff: takes a finite lattice
;; and returns its underlying <locally-finite-poset>, which lets
;; consumers call incidence-algebra machinery on any finite lattice.

(define (lattice->locally-finite-poset L)
  "Project a finite lattice to its underlying <locally-finite-poset>.\nRequires a finite lattice (elements enumerated); the result carries\nthe same element list as L and uses (lattice-leq? L) as its order.\n\nEnables Möbius-function computation on finite lattices: compose\n(make-incidence-algebra ...) with this projection.\n\nExamples:\n  (lf-poset-elements (lattice->locally-finite-poset (chain-lattice 4)))\n    => (0 1 2 3)\n\nParameters:\n  L : lattice\nReturns: locally-finite-poset\nCategory: algebra\nKeywords: forgetful functor, projection, underlying poset, lattice to poset\n\nSee also: `lattice->partial-order', `birkhoff-representation'."
  (unless (finite-lattice? L)
    (error "lattice->locally-finite-poset: requires finite lattice"
           'fix "pass (cons 'elements LIST) to make-lattice"))
  (finite-set->locally-finite-poset
    (lambda (a b) (lattice-leq? L a b))
    (lattice-elements L)))

(define (%build-birkhoff-irr-poset L)
  ;; Core body shared by birkhoff-representation and its /unchecked
  ;; sibling. Assumes caller has already validated finite-lattice?.
  (let ((irr (join-irreducibles L)))
    (make-locally-finite-poset
      (lambda (a b) (lattice-leq? L a b))
      (lambda (x y)
        ;; Interval [x,y] restricted to irreducibles: empty if ¬(x≤y),
        ;; else the irreducibles z with x ≤ z ≤ y.
        (if (not (lattice-leq? L x y))
            '()
            (filter (lambda (z) (and (lattice-leq? L x z)
                                     (lattice-leq? L z y)))
                    irr)))
      (cons 'elements irr))))

(define (birkhoff-representation L)
  "Return the <locally-finite-poset> of join-irreducibles of finite\ndistributive lattice L, ordered by the restriction of (lattice-leq? L).\n\nBirkhoff's fundamental theorem: every finite distributive lattice L\nis isomorphic to (birkhoff-reconstruction (birkhoff-representation L)).\n\nRequires a finite, distributive lattice. Raises a precondition error\notherwise. The distributivity check is O(|L|³) — negligible for the\ntypical consumer sizes (|L| ≤ ~100 for abstract domains / concept\nlattices / dataflow) but noticeable at FDL(4) (~2 s) and infeasible at\nFDL(5) (~days). Callers who have already validated distributivity can\nuse `birkhoff-representation/unchecked' to skip the gate.\n\nExamples:\n  (lf-poset-elements (birkhoff-representation (chain-lattice 4)))\n    => (1 2 3)\n  (birkhoff-representation (diamond-lattice 3))  ;; raises — M3 not distributive\n\nParameters:\n  L : lattice\nReturns: locally-finite-poset\nCategory: algebra\nKeywords: Birkhoff, representation, join irreducibles, distributive, duality\n\nSee also: `birkhoff-reconstruction', `birkhoff-representation/unchecked', `distributive?'."
  (unless (finite-lattice? L)
    (error "birkhoff-representation: requires finite lattice"
           'fix "pass (cons 'elements LIST) to make-lattice"))
  (unless (distributive? L)
    (error "birkhoff-representation: requires distributive lattice"
           'fix "check (distributive? L) first, or use birkhoff-representation/unchecked for pre-validated input"))
  (%build-birkhoff-irr-poset L))

(define (birkhoff-representation/unchecked L)
  "Return the <locally-finite-poset> of join-irreducibles of finite\nlattice L, without checking distributivity.\n\nBehaves identically to `birkhoff-representation' when L is\ndistributive. On non-distributive L the result is still a\nwell-formed <locally-finite-poset> (the poset of join-irreducibles\nunder the restricted lattice order), but the Birkhoff roundtrip\n(`birkhoff-reconstruction' of this result) will not reproduce L —\nthat is a consequence of Birkhoff's theorem applying only to\ndistributive lattices, not a bug.\n\nUse this when the caller has already validated distributivity, or\nwhen the cost of `distributive?' (O(|L|³)) is prohibitive for the\nlattice size. Prefer `birkhoff-representation' by default.\n\nRequires a finite lattice (elements enumerated).\n\nExamples:\n  ;; M3 is not distributive; /unchecked returns the poset of its 3 atoms.\n  (length (lf-poset-elements\n            (birkhoff-representation/unchecked (diamond-lattice 3))))  => 3\n\nParameters:\n  L : lattice\nReturns: locally-finite-poset\nCategory: algebra\nKeywords: Birkhoff, representation, unchecked, join irreducibles, escape hatch\n\nSee also: `birkhoff-representation', `distributive?'."
  (unless (finite-lattice? L)
    (error "birkhoff-representation/unchecked: requires finite lattice"
           'fix "pass (cons 'elements LIST) to make-lattice"))
  (%build-birkhoff-irr-poset L))

;; ─── Internal downset enumeration ────────────
;;
;; Smart recursive enumerator per plan §5.5 Q15 / Risk #3. Picks any
;; maximal element x of P, recursively enumerates downsets(P \ {x}),
;; and extends each downset D by x when D already contains all
;; strict-predecessors of x.
;;
;; Cost: O(|downsets(P)|), not O(2^|P|). Feasible for the
;; free-distributive-lattice 5 case (D(5) = 7581 downsets of B(5)'s 32
;; elements), which the naive subset-filter approach cannot handle.

(define (%maximal-element elements leq?)
  ;; Return any maximal element of ELEMENTS under LEQ?.
  ;; Maximal x: for all y in elements, ¬(x < y). Strict-less uses
  ;; (leq? x y) ∧ ¬(leq? y x); same as "x ≤ y and x ≠ y" under the
  ;; assumption that LEQ? is antisymmetric.
  (let loop ((xs elements) (best #f) (have-best? #f))
    (cond
      ((null? xs) best)
      ((not have-best?) (loop (cdr xs) (car xs) #t))
      ((and (leq? best (car xs)) (not (leq? (car xs) best)))
       ;; Found something strictly above best; update.
       (loop (cdr xs) (car xs) #t))
      (else (loop (cdr xs) best #t)))))

(define (%remove-element xs target)
  (filter (lambda (x) (not (equal? x target))) xs))

(define (%strict-predecessors elements x leq?)
  ;; Return elements y with y < x (strictly; leq? antisymmetric).
  (filter
    (lambda (y)
      (and (leq? y x) (not (leq? x y))))
    elements))

(define (%subset-of? xs ys)
  ;; Every element of XS appears in YS (equal?-membership).
  (let loop ((xs xs))
    (cond
      ((null? xs) #t)
      ((member (car xs) ys) (loop (cdr xs)))
      (else #f))))

(define (%enumerate-downsets elements leq?)
  ;; Recursive enumerator. Returns a list of downsets; each downset
  ;; is a list of elements in ELEMENTS-first-seen order (canonical).
  (cond
    ((null? elements) '(()))
    (else
     (let* ((x    (%maximal-element elements leq?))
            (rest (%remove-element elements x))
            (sub  (%enumerate-downsets rest leq?))
            (preds (%strict-predecessors rest x leq?)))
       (append
         sub
         (map (lambda (D)
                (%sort-by-canonical-order (cons x D) elements))
              (filter (lambda (D) (%subset-of? preds D)) sub)))))))

(define (%sorted-union a b canonical)
  ;; Union as canonical-ordered list, using CANONICAL for sort order.
  (%sort-by-canonical-order
    (let loop ((xs b) (acc a))
      (cond ((null? xs) acc)
            ((member (car xs) acc) (loop (cdr xs) acc))
            (else (loop (cdr xs) (cons (car xs) acc)))))
    canonical))

(define (%sorted-intersection a b canonical)
  (%sort-by-canonical-order
    (filter (lambda (x) (member x b)) a)
    canonical))

(define (birkhoff-reconstruction P)
  "Return the <lattice> whose elements are the downsets of\nlocally-finite poset P, ordered by inclusion.\n\nP must expose its element list via `lf-poset-elements' (constructed\nvia `finite-set->locally-finite-poset` or `make-locally-finite-poset`\nwith (cons 'elements LIST)).\n\nResult:\n  bottom:      '()   (empty downset)\n  top:         elements(P)\n  join:        sorted union of downsets\n  meet:        sorted intersection\n  leq?:        subset relation on downsets\n  setoid:      (default-setoid) wrapping equal? — consistent with the\n               equal?-based list-union/intersection internals\n\nThis is the reverse direction of Birkhoff's theorem: every finite\ndistributive lattice L is isomorphic to\n(birkhoff-reconstruction (birkhoff-representation L)).\n\nExamples:\n  (lattice-cardinality\n    (birkhoff-reconstruction\n      (birkhoff-representation (chain-lattice 4))))  => 4\n\nParameters:\n  P : locally-finite-poset\nReturns: lattice\nCategory: algebra\nKeywords: Birkhoff, reconstruction, downsets, order ideals, distributive, duality\n\nSee also: `birkhoff-representation', `lattice->locally-finite-poset'."
  (let ((elements (lf-poset-elements P)))
    (unless elements
      (error "birkhoff-reconstruction: poset must expose elements"
             'fix "construct P via finite-set->locally-finite-poset or pass (cons 'elements LIST) to make-locally-finite-poset"))
    (let* ((leq      (lf-poset-leq? P))
           (downsets (%enumerate-downsets elements leq)))
      (make-lattice
        (lambda (a b) (%sorted-union a b elements))
        (lambda (a b) (%sorted-intersection a b elements))
        '()          ;; bottom
        elements     ;; top
        %subset-of?
        (cons 'setoid      (default-setoid))
        (cons 'cardinality (length downsets))
        (cons 'elements    downsets)))))

(define (free-distributive-lattice n)
  "Construct the free bounded distributive lattice on N generators.\nIsomorphic to the lattice of monotone Boolean functions on {0,1}^N,\nequivalently to Downsets(B(n)) where B(n) is the Boolean lattice\n2^[n] viewed as a poset. Cardinality is the Dedekind number D(n):\n  D(0) = 2, D(1) = 3, D(2) = 6, D(3) = 20, D(4) = 168, D(5) = 7581.\n\nRaises for n ≥ 6; D(6) ≈ 7.8M elements is infeasible for in-process\nconstruction. Direct callers of `birkhoff-reconstruction' on a\nuser-supplied poset can opt into that cost.\n\nExamples:\n  (lattice-cardinality (free-distributive-lattice 2))  => 6\n  (lattice-cardinality (free-distributive-lattice 3))  => 20\n\nParameters:\n  n : exact non-negative integer, n ≤ 5\nReturns: lattice\nCategory: algebra\nKeywords: free distributive lattice, Dedekind number, Birkhoff, monotone boolean, FDL\n\nSee also: `birkhoff-reconstruction', `boolean-lattice'."
  (unless (and (integer? n) (not (negative? n)))
    (error "free-distributive-lattice: n must be a non-negative integer" n))
  (when (>= n 6)
    (error "free-distributive-lattice: n ≥ 6 infeasible (Dedekind number explodes)"
           'n n '|D(6)| 7828354))
  ;; FDL(n) = Downsets(B(n)), taking the WHOLE Boolean poset as the
  ;; reconstruction domain. Do NOT take
  ;; (birkhoff-representation (boolean-lattice n)) — that gives the
  ;; n-element antichain J(B(n)) whose downsets reconstruct B(n)
  ;; (cardinality 2^n), not FDL(n) (cardinality D(n)).
  (birkhoff-reconstruction (lattice->locally-finite-poset (boolean-lattice n))))

;; ─── Validation ──────────────────────────────

(define (validate-lattice L samples)
  "Spot-check that L satisfies the lattice laws on SAMPLES.\nTests join and meet commutativity, absorption, idempotence,\nand identity (bottom for join, top for meet) for all elements\nand pairs in SAMPLES. Returns #t if all laws hold, or a list\nof (violation-type element ...) entries describing failures.\n\nExamples:\n  (validate-lattice (flat-lattice '(a b c) eq?) '(a b c))  => #t\n\nParameters:\n  L : any\n  samples : list\nReturns: any\nCategory: algebra\nKeywords: commutativity, absorption, idempotence, identity, law checking, validation\n\nSee also: `make-lattice', `lattice-join', `lattice-meet'."
  (let ((fail! (make-violation-reporter)))
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
    (fail!)))
