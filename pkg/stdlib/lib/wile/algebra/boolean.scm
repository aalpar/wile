;;; (wile algebra boolean) — Boolean algebras
;;;
;;; A Boolean algebra is a complemented distributive lattice: a bounded
;;; lattice where every element a has a complement ¬a satisfying
;;; a ∧ ¬a = ⊥ and a ∨ ¬a = ⊤, and where meet distributes over join.

;; ─── Record type ─────────────────────────────

(define-record-type <boolean-algebra>
  (make-boolean-algebra* join-fn meet-fn bottom top leq-fn complement-fn)
  boolean-algebra?
  (join-fn       boolean-join-fn)
  (meet-fn       boolean-meet-fn)
  (bottom        boolean-bottom)
  (top           boolean-top)
  (leq-fn        boolean-leq-fn)
  (complement-fn boolean-complement-fn))

(define (make-boolean-algebra join meet bottom top leq? complement)
  "Construct a Boolean algebra from lattice operations and COMPLEMENT.\nA Boolean algebra is a complemented distributive lattice where\nevery element a has a complement satisfying a ∧ ¬a = ⊥ and\na ∨ ¬a = ⊤, and meet distributes over join.\n\nExamples:\n  (let ((B (powerset-boolean '(x y z))))\n    (boolean-complement B '(x)))  => (y z)\n\nParameters:\n  join : procedure\n  meet : procedure\n  bottom : any\n  top : any\n  leq? : procedure\n  complement : procedure\nReturns: any\nCategory: algebra\nKeywords: Boolean, complement, distributive lattice, classical logic\n\nSee also: `powerset-boolean', `validate-boolean-algebra'."
  (assert-procedure "make-boolean-algebra" join)
  (assert-procedure "make-boolean-algebra" meet)
  (assert-procedure "make-boolean-algebra" leq?)
  (assert-procedure "make-boolean-algebra" complement)
  (make-boolean-algebra* join meet bottom top leq? complement))

;; ─── Core operations ─────────────────────────

(define (boolean-join B a b)
  "Compute the join (least upper bound) of A and B in Boolean algebra B.\n\nExamples:\n  (let ((B (powerset-boolean '(x y z))))\n    (boolean-join B '(x) '(y)))  => (x y)\n\nParameters:\n  B : any\n  a : any\n  b : any\nReturns: any\nCategory: algebra\nKeywords: join, union, or, disjunction, vee"
  ((boolean-join-fn B) a b))

(define (boolean-meet B a b)
  "Compute the meet (greatest lower bound) of A and B in Boolean algebra B.\n\nExamples:\n  (let ((B (powerset-boolean '(x y z))))\n    (boolean-meet B '(x y) '(y z)))  => (y)\n\nParameters:\n  B : any\n  a : any\n  b : any\nReturns: any\nCategory: algebra\nKeywords: meet, intersection, and, conjunction, wedge"
  ((boolean-meet-fn B) a b))

(define (boolean-leq? B a b)
  "Test whether A is less than or equal to B in Boolean algebra B.\n\nExamples:\n  (let ((B (powerset-boolean '(x y z))))\n    (boolean-leq? B '(x) '(x y)))  => #t\n\nParameters:\n  B : any\n  a : any\n  b : any\nReturns: boolean\nCategory: algebra\nKeywords: partial order, less than, leq, ordering, subset"
  ((boolean-leq-fn B) a b))

(define (boolean-complement B a)
  "Compute the complement of A in Boolean algebra B.\nReturns ¬a satisfying a ∧ ¬a = ⊥ and a ∨ ¬a = ⊤.\nIn a Boolean algebra, complement is an involution: ¬¬a = a.\n\nExamples:\n  (let ((B (powerset-boolean '(x y z))))\n    (boolean-complement B '(x)))  => (y z)\n  (let ((B (powerset-boolean '(x y z))))\n    (boolean-complement B '()))   => (x y z)\n\nParameters:\n  B : any\n  a : any\nReturns: any\nCategory: algebra\nKeywords: complement, negation, not, Boolean negation, set difference\n\nSee also: `boolean-meet', `boolean-join'."
  ((boolean-complement-fn B) a))

;; ─── Projections ─────────────────────────────

(define (boolean->heyting B)
  "Project Boolean algebra B to a Heyting algebra.\nImplication is derived from complement: a → b = ¬a ∨ b.\nIn a Boolean algebra, the pseudo-complement equals the\ntrue complement, so Heyting negation agrees with Boolean\ncomplement.\n\nExamples:\n  (let* ((B (powerset-boolean '(x y z)))\n         (H (boolean->heyting B)))\n    (heyting-negate H '(x)))  => (y z)\n\nParameters:\n  B : any\nReturns: any\nCategory: algebra\nKeywords: forgetful functor, projection, Heyting, implication derived\n\nSee also: `make-heyting-algebra', `boolean-complement'."
  (make-heyting-algebra
    (boolean-join-fn B) (boolean-meet-fn B)
    (boolean-bottom B) (boolean-top B)
    (boolean-leq-fn B)
    (lambda (a b)
      ((boolean-join-fn B) ((boolean-complement-fn B) a) b))))

(define (boolean->lattice B)
  "Project Boolean algebra B to its underlying lattice, forgetting complement.\nThe resulting lattice has the same join, meet, bottom, top, and leq?.\n\nExamples:\n  (let* ((B (powerset-boolean '(x y z)))\n         (L (boolean->lattice B)))\n    (lattice-join L '(x) '(y)))  => (x y)\n\nParameters:\n  B : any\nReturns: any\nCategory: algebra\nKeywords: forgetful functor, projection, underlying lattice, extract\n\nSee also: `make-lattice', `boolean->heyting'."
  (make-lattice (boolean-join-fn B) (boolean-meet-fn B)
                (boolean-bottom B) (boolean-top B)
                (boolean-leq-fn B)))

(define (boolean->ring B)
  "Project Boolean algebra B to a ring of characteristic 2.\nPlus is symmetric difference (a △ b = (a ∨ b) ∧ ¬(a ∧ b)),\ntimes is meet, zero is ⊥, one is ⊤, and negate is identity\n(every element is its own additive inverse since a △ a = ⊥).\nThis bridges the order-theoretic and algebraic towers.\n\nExamples:\n  (let* ((B (powerset-boolean '(x y z)))\n         (R (boolean->ring B)))\n    (ring-plus R '(x y) '(y z)))  => (x z)\n\nParameters:\n  B : any\nReturns: any\nCategory: algebra\nKeywords: characteristic 2, symmetric difference, Boolean ring, bridge\n\nSee also: `make-ring', `boolean->lattice'."
  (let ((join-fn (boolean-join-fn B))
        (meet-fn (boolean-meet-fn B))
        (comp-fn (boolean-complement-fn B)))
    (make-ring
      ;; plus = symmetric difference: (a ∨ b) ∧ ¬(a ∧ b)
      (lambda (a b)
        (meet-fn (join-fn a b) (comp-fn (meet-fn a b))))
      ;; times = meet
      meet-fn
      ;; zero = bottom, one = top
      (boolean-bottom B)
      (boolean-top B)
      ;; negate = identity (a △ a = ⊥, so a is its own inverse)
      (lambda (a) a))))

;; ─── with-boolean macro ──────────────────────

(define-syntax with-boolean
  (syntax-rules ()
    ((with-boolean B (join meet bottom top leq? complement) body ...)
     (let ((tmp B))
       (let ((join       (lambda (a b) (boolean-join tmp a b)))
             (meet       (lambda (a b) (boolean-meet tmp a b)))
             (bottom     (boolean-bottom tmp))
             (top        (boolean-top tmp))
             (leq?       (lambda (a b) (boolean-leq? tmp a b)))
             (complement (lambda (a) (boolean-complement tmp a))))
         body ...)))))

;; ─── Boolean equality (derived from leq?) ────

(define (boolean-equal? B a b)
  (and (boolean-leq? B a b)
       (boolean-leq? B b a)))

;; ─── Constructors ────────────────────────────

(define (powerset-boolean universe)
  "Construct the Boolean algebra of subsets of UNIVERSE.\nElements are lists representing subsets. Join is set union,\nmeet is set intersection, complement is set difference from\nUNIVERSE, bottom is the empty set, and top is UNIVERSE.\n\nExamples:\n  (let ((B (powerset-boolean '(x y z))))\n    (boolean-complement B '(x)))      => (y z)\n  (let ((B (powerset-boolean '(x y z))))\n    (boolean-join B '(x) '(y z)))     => (x y z)\n\nParameters:\n  universe : list\nReturns: any\nCategory: algebra\nKeywords: powerset, set, subset, power set, Boolean, set complement\n\nSee also: `powerset-lattice', `powerset-heyting'."
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
  (define (set-diff a b)
    (cond ((null? a) '())
          ((member (car a) b) (set-diff (cdr a) b))
          (else (cons (car a) (set-diff (cdr a) b)))))
  (make-boolean-algebra
    union intersect '() universe subset?
    (lambda (a) (set-diff universe a))))

;; ─── Validation ──────────────────────────────

(define (validate-boolean-algebra B samples)
  "Spot-check that B satisfies the Boolean algebra laws on SAMPLES.\nDelegates lattice law checks to validate-lattice, then tests\ncomplement laws (a ∧ ¬a = ⊥, a ∨ ¬a = ⊤) and distributivity\n(a ∧ (b ∨ c) = (a ∧ b) ∨ (a ∧ c)) for elements and triples\nin SAMPLES. Returns #t if all laws hold, or a list of\n(violation-type element ...) entries describing failures.\n\nExamples:\n  (validate-boolean-algebra\n    (powerset-boolean '(x y z)) '((x) (y) (x y)))  => #t\n\nParameters:\n  B : any\n  samples : list\nReturns: any\nCategory: algebra\nKeywords: complement, distributivity, excluded middle, law checking, validation\n\nSee also: `make-boolean-algebra', `validate-lattice'."
  (let ((fail! (make-violation-reporter))
        (bot (boolean-bottom B))
        (top (boolean-top B)))
    ;; Delegate lattice laws
    (let ((lattice-result (validate-lattice (boolean->lattice B) samples)))
      (unless (eq? #t lattice-result)
        (for-each (lambda (v) (apply fail! v)) lattice-result)))
    (for-each
      (lambda (a)
        ;; Complement: a ∧ ¬a = ⊥
        (let ((neg-a (boolean-complement B a)))
          (unless (boolean-equal? B (boolean-meet B a neg-a) bot)
            (fail! 'non-contradiction a))
          ;; Complement: a ∨ ¬a = ⊤
          (unless (boolean-equal? B (boolean-join B a neg-a) top)
            (fail! 'excluded-middle a)))
        ;; Distributivity: a ∧ (b ∨ c) = (a ∧ b) ∨ (a ∧ c)
        (for-each
          (lambda (b)
            (for-each
              (lambda (c)
                (let ((lhs (boolean-meet B a (boolean-join B b c)))
                      (rhs (boolean-join B (boolean-meet B a b)
                                           (boolean-meet B a c))))
                  (unless (boolean-equal? B lhs rhs)
                    (fail! 'distributivity a b c))))
              samples))
          samples))
      samples)
    (fail!)))
