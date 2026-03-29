;;; (wile algebra lattice) — Lattices, constructors, and fixpoint
;;;
;;; A lattice is a partially ordered set where every pair has a join
;;; (least upper bound) and meet (greatest lower bound), plus bottom
;;; and top elements.

;; ─── Record type ─────────────────────────────

(define-record-type <lattice>
  (make-lattice* join-fn meet-fn bottom top leq-fn)
  lattice?
  (join-fn lattice-join-fn)
  (meet-fn lattice-meet-fn)
  (bottom  lattice-bottom)
  (top     lattice-top)
  (leq-fn  lattice-leq-fn))

(define (make-lattice join meet bottom top leq?)
  "Construct a lattice from JOIN, MEET, BOTTOM, TOP, and LEQ? predicate.\nJOIN computes the least upper bound of two elements, MEET computes\nthe greatest lower bound. BOTTOM is less than all elements, TOP is\ngreater than all elements. LEQ? tests the partial ordering.\n\nExamples:\n  (let ((L (make-lattice max min 0 100 <=)))\n    (lattice-join L 3 7))  => 7\n  (let ((L (make-lattice max min 0 100 <=)))\n    (lattice-meet L 3 7))  => 3\n\nSee also: `flat-lattice', `powerset-lattice', `validate-lattice'."
  (make-lattice* join meet bottom top leq?))

;; ─── Core operations ─────────────────────────

(define (lattice-join L a b)
  "Compute the join (least upper bound) of A and B in lattice L.\nThe join is the smallest element that is greater than or equal\nto both A and B.\n\nExamples:\n  (let ((L (powerset-lattice '(1 2 3))))\n    (lattice-join L '(1) '(2 3)))  => (1 2 3)"
  ((lattice-join-fn L) a b))

(define (lattice-meet L a b)
  "Compute the meet (greatest lower bound) of A and B in lattice L.\nThe meet is the largest element that is less than or equal to\nboth A and B.\n\nExamples:\n  (let ((L (powerset-lattice '(1 2 3))))\n    (lattice-meet L '(1 2) '(2 3)))  => (2)"
  ((lattice-meet-fn L) a b))

(define (lattice-leq? L a b)
  "Test whether A is less than or equal to B in lattice L.\n\nExamples:\n  (let ((L (powerset-lattice '(1 2 3))))\n    (lattice-leq? L '(1) '(1 2)))  => #t\n  (let ((L (powerset-lattice '(1 2 3))))\n    (lattice-leq? L '(1 3) '(1 2)))  => #f"
  ((lattice-leq-fn L) a b))

;; ─── Projection ──────────────────────────────

(define (lattice->partial-order L)
  "Extract the partial order from lattice L.\nThe resulting partial order uses L's leq? predicate.\n\nExamples:\n  (let* ((L (powerset-lattice '(1 2 3)))\n         (po (lattice->partial-order L)))\n    (po-leq? po '(1) '(1 2)))  => #t\n\nSee also: `lattice-leq?', `make-partial-order'."
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
  "Test whether A and B are equal in lattice L.\nTwo elements are lattice-equal when each is less than or equal\nto the other (antisymmetry of the underlying partial order).\n\nExamples:\n  (let ((L (powerset-lattice '(1 2 3))))\n    (lattice-equal? L '(1 2) '(2 1)))  => #t\n  (let ((L (powerset-lattice '(1 2 3))))\n    (lattice-equal? L '(1) '(1 2)))    => #f\n\nSee also: `lattice-leq?'."
  (and (lattice-leq? L a b)
       (lattice-leq? L b a)))

;; ─── Fixpoint ────────────────────────────────

(define fixpoint
  (case-lambda
    ((L f x)
     "Compute the least fixpoint of F starting from X in lattice L.\nIterates F(F(...F(X)...)) until the result stabilizes according\nto lattice-equal?. This is Kleene iteration; F must be monotone\nand L must have no infinite ascending chains for termination.\nWith four arguments, limits iteration to FUEL steps and returns\n#f if the fixpoint is not reached.\n\nExamples:\n  (fixpoint (powerset-lattice '(1 2 3))\n            (lambda (s) (if (member 2 s) s (cons 2 s)))\n            '())  => (2)\n\nSee also: `fixpoint/widen', `lattice-equal?'."
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
  "Compute a fixpoint of F from X in lattice L using WIDEN to ensure termination.\nLike fixpoint, but applies WIDEN instead of raw join when the\nvalue increases. WIDEN takes (current, next) and must return an\nelement at least as large as their join, and every ascending\nchain under WIDEN must be finite. This guarantees termination\neven when L has infinite ascending chains.\n\nExamples:\n  (let ((L (make-lattice max min 0 100 <=)))\n    (fixpoint/widen L\n      (lambda (x) (+ x 1))\n      0\n      (lambda (cur next) 100)))  => 100\n\nSee also: `fixpoint', `lattice-leq?', `lattice-equal?'."
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
  "Construct a flat lattice over ELEMENTS using EQUAL? for comparison.\nIn a flat lattice, all elements are incomparable to each other\nbut sit between a bottom element (less than everything) and a top\nelement (greater than everything). The lattice join of two unequal\nelements is top; their meet is bottom.\n\nExamples:\n  (let ((L (flat-lattice '(a b c) eq?)))\n    (lattice-join L 'a 'a))  => a\n  (let ((L (flat-lattice '(a b c) eq?)))\n    (lattice-join L 'a 'b))  => flat-top\n\nSee also: `powerset-lattice', `product-lattice', `make-lattice'."
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
  "Construct the powerset lattice over UNIVERSE.\nElements are lists representing subsets. Join is set union,\nmeet is set intersection, bottom is the empty set, top is\nUNIVERSE, and ordering is the subset relation. Membership\nis tested with equal?.\n\nExamples:\n  (let ((L (powerset-lattice '(1 2 3))))\n    (lattice-join L '(1) '(2 3)))  => (1 2 3)\n  (let ((L (powerset-lattice '(1 2 3))))\n    (lattice-meet L '(1 2) '(2 3)))  => (2)\n\nSee also: `flat-lattice', `product-lattice', `map-lattice'."
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
  "Construct the product lattice from LATTICES applied pointwise.\nElements are lists of the same length as LATTICES. All operations\n(join, meet, leq?) apply component-wise to corresponding elements.\nBottom is the list of all component bottoms; top is the list of\nall component tops.\n\nExamples:\n  (let ((L (product-lattice (make-lattice max min 0 10 <=)\n                             (make-lattice max min 0 10 <=))))\n    (lattice-join L '(1 2) '(3 1)))  => (3 2)\n\nSee also: `map-lattice', `flat-lattice', `powerset-lattice'."
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
  "Construct a lattice of alists mapping KEYS to elements of VALUE-LATTICE.\nAll operations apply pointwise: join, meet, and leq? operate\non corresponding values for each key. Missing keys are treated\nas VALUE-LATTICE's bottom element. Bottom is all keys mapped\nto bottom; top is all keys mapped to top.\n\nExamples:\n  (let ((L (map-lattice '(x y) (make-lattice max min 0 100 <=))))\n    (lattice-join L '((x . 1) (y . 5)) '((x . 3) (y . 2))))\n    => ((x . 3) (y . 5))\n\nSee also: `product-lattice', `powerset-lattice'."
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

(define (validate-lattice L samples)
  "Spot-check that L satisfies the lattice laws on SAMPLES.\nTests join and meet commutativity, absorption, idempotence,\nand identity (bottom for join, top for meet) for all elements\nand pairs in SAMPLES. Returns #t if all laws hold, or a list\nof (violation-type element ...) entries describing failures.\n\nExamples:\n  (validate-lattice (flat-lattice '(a b c) eq?) '(a b c))  => #t\n\nSee also: `make-lattice', `lattice-join', `lattice-meet'."
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
