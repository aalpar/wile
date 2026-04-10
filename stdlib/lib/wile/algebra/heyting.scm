;;; (wile algebra heyting) — Heyting algebras
;;;
;;; A Heyting algebra is a bounded distributive lattice equipped with a
;;; relative pseudo-complement (implication): for every a and b, there
;;; exists a largest c such that a ∧ c ≤ b.  That c is written a → b.

;; ─── Record type ─────────────────────────────

(define-record-type <heyting-algebra>
  (make-heyting-algebra* join-fn meet-fn bottom top leq-fn implies-fn)
  heyting-algebra?
  (join-fn    heyting-join-fn)
  (meet-fn    heyting-meet-fn)
  (bottom     heyting-bottom)
  (top        heyting-top)
  (leq-fn     heyting-leq-fn)
  (implies-fn heyting-implies-fn))

(define (make-heyting-algebra join meet bottom top leq? implies)
  "Construct a Heyting algebra from lattice operations and IMPLIES.\nA Heyting algebra is a bounded distributive lattice where every\npair (a, b) has a relative pseudo-complement: the largest c\nsuch that a ∧ c ≤ b. IMPLIES computes this c.\n\nExamples:\n  (heyting-algebra? (powerset-heyting '(x y z)))  => #t\n  (let ((H (powerset-heyting '(x y z))))\n    (heyting-leq? H '(x) '(x y)))  => #t\n\nParameters:\n  join : procedure\n  meet : procedure\n  bottom : any\n  top : any\n  leq? : procedure\n  implies : procedure\nReturns: any\nCategory: algebra\nKeywords: Heyting, implication, pseudo-complement, intuitionistic, distributive lattice\n\nSee also: `powerset-heyting', `validate-heyting-algebra'."
  (make-heyting-algebra* join meet bottom top leq? implies))

;; ─── Core operations ─────────────────────────

(define (heyting-join H a b)
  "Compute the join (least upper bound) of A and B in Heyting algebra H.\n\nExamples:\n  (let ((H (powerset-heyting '(x y z))))\n    (heyting-leq? H '(x) (heyting-join H '(x) '(y))))  => #t\n\nParameters:\n  H : any\n  a : any\n  b : any\nReturns: any\nCategory: algebra\nKeywords: join, supremum, lub, least upper bound, union, vee"
  ((heyting-join-fn H) a b))

(define (heyting-meet H a b)
  "Compute the meet (greatest lower bound) of A and B in Heyting algebra H.\n\nExamples:\n  (let ((H (powerset-heyting '(x y z))))\n    (heyting-meet H '(x y) '(y z)))  => (y)\n  (let ((H (powerset-heyting '(x y z))))\n    (heyting-meet H '(x) '(y)))      => ()\n\nParameters:\n  H : any\n  a : any\n  b : any\nReturns: any\nCategory: algebra\nKeywords: meet, infimum, glb, greatest lower bound, intersection, wedge"
  ((heyting-meet-fn H) a b))

(define (heyting-leq? H a b)
  "Test whether A is less than or equal to B in Heyting algebra H.\n\nExamples:\n  (let ((H (powerset-heyting '(x y z))))\n    (heyting-leq? H '(x) '(x y)))  => #t\n\nParameters:\n  H : any\n  a : any\n  b : any\nReturns: boolean\nCategory: algebra\nKeywords: partial order, less than, leq, ordering, subset"
  ((heyting-leq-fn H) a b))

(define (heyting-implies H a b)
  "Compute the Heyting implication A → B in Heyting algebra H.\nReturns the largest element c such that a ∧ c ≤ b.\nSatisfies the adjunction: c ≤ (a → b) iff a ∧ c ≤ b.\n\nExamples:\n  (let ((H (powerset-heyting '(x y z))))\n    (heyting-implies H '(x y z) '(x)))  => (x)\n  (let ((H (powerset-heyting '(x y z))))\n    (heyting-implies H '(x y z) '()))   => ()\n\nParameters:\n  H : any\n  a : any\n  b : any\nReturns: any\nCategory: algebra\nKeywords: implication, relative pseudo-complement, residuation, adjunction, arrow, entailment, conditional\n\nSee also: `heyting-negate', `heyting-meet'."
  ((heyting-implies-fn H) a b))

(define (heyting-negate H a)
  "Compute the pseudo-complement of A in Heyting algebra H.\nDefined as A → ⊥, the largest element c such that a ∧ c = ⊥.\nIn a Boolean algebra this is the true complement; in a general\nHeyting algebra, ¬¬a ≥ a but ¬¬a = a does not always hold.\n\nExamples:\n  (let ((H (powerset-heyting '(x y z))))\n    (heyting-negate H '(x)))  => (y z)\n\nParameters:\n  H : any\n  a : any\nReturns: any\nCategory: algebra\nKeywords: pseudo-complement, negation, not, complement, intuitionistic negation\n\nSee also: `heyting-implies'."
  (heyting-implies H a (heyting-bottom H)))

;; ─── Projection ──────────────────────────────

(define (heyting->lattice H)
  "Project Heyting algebra H to its underlying lattice, forgetting implication.\nThe resulting lattice has the same join, meet, bottom, top, and leq?.\n\nExamples:\n  (let* ((H (powerset-heyting '(x y z)))\n         (L (heyting->lattice H)))\n    (lattice-leq? L '(x) '(x y)))  => #t\n\nParameters:\n  H : any\nReturns: any\nCategory: algebra\nKeywords: forgetful functor, projection, underlying lattice, extract\n\nSee also: `make-lattice', `heyting-join'."
  (make-lattice (heyting-join-fn H) (heyting-meet-fn H)
                (heyting-bottom H) (heyting-top H)
                (heyting-leq-fn H)))

;; ─── with-heyting macro ──────────────────────

(define-syntax with-heyting
  (syntax-rules ()
    ((with-heyting H (join meet bottom top leq? implies) body ...)
     (let ((tmp H))
       (let ((join    (lambda (a b) (heyting-join tmp a b)))
             (meet    (lambda (a b) (heyting-meet tmp a b)))
             (bottom  (heyting-bottom tmp))
             (top     (heyting-top tmp))
             (leq?    (lambda (a b) (heyting-leq? tmp a b)))
             (implies (lambda (a b) (heyting-implies tmp a b))))
         body ...)))))

;; ─── Heyting equality (derived from leq?) ────

(define (heyting-equal? H a b)
  (and (heyting-leq? H a b)
       (heyting-leq? H b a)))

;; ─── Constructors ────────────────────────────

(define (powerset-heyting universe)
  "Construct the Heyting algebra of subsets of UNIVERSE.\nElements are lists representing subsets. Join is set union,\nmeet is set intersection, implication is (complement(a) ∪ b),\nbottom is the empty set, and top is UNIVERSE.\nThis is also a Boolean algebra, but the Heyting view provides\nimplication without requiring an explicit complement operation.\n\nExamples:\n  (let ((H (powerset-heyting '(x y z))))\n    (heyting-negate H '(x y)))         => (z)\n  (let ((H (powerset-heyting '(x y z))))\n    (heyting-negate H '(x)))           => (y z)\n\nParameters:\n  universe : list\nReturns: any\nCategory: algebra\nKeywords: powerset, set, subset, power set, set lattice, Heyting\n\nSee also: `powerset-lattice', `powerset-boolean'."
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
  (make-heyting-algebra
    union intersect '() universe subset?
    (lambda (a b) (union (set-diff universe a) b))))

(define (map-heyting keys value-heyting)
  "Construct a Heyting algebra of alists mapping KEYS to elements of VALUE-HEYTING.\nAll operations apply pointwise. Implication is computed per-key\nusing VALUE-HEYTING's implication.\n\nExamples:\n  (let* ((H (powerset-heyting '(1 2)))\n         (M (map-heyting '(x y) H)))\n    (heyting-implies M\n      (list (cons 'x '(1)) (cons 'y '(1 2)))\n      (list (cons 'x '(1 2)) (cons 'y '(1)))))\n    ;; => ((x . (1 2)) (y . (1)))\n\nParameters:\n  keys : list\n  value-heyting : any\nReturns: any\nCategory: algebra\nKeywords: map, dictionary, pointwise, association, key-value, Heyting\n\nSee also: `map-lattice', `powerset-heyting'."
  (let ((vbot (heyting-bottom value-heyting))
        (vtop (heyting-top value-heyting)))
    (define (lookup key alist)
      (let ((pair (assoc key alist)))
        (if pair (cdr pair) vbot)))
    (define (pointwise-binop op a b)
      (map (lambda (k) (cons k (op value-heyting (lookup k a) (lookup k b))))
           keys))
    (make-heyting-algebra
      (lambda (a b) (pointwise-binop heyting-join a b))
      (lambda (a b) (pointwise-binop heyting-meet a b))
      (map (lambda (k) (cons k vbot)) keys)
      (map (lambda (k) (cons k vtop)) keys)
      (lambda (a b)
        (let loop ((ks keys))
          (cond ((null? ks) #t)
                ((not (heyting-leq? value-heyting
                                    (lookup (car ks) a)
                                    (lookup (car ks) b)))
                 #f)
                (else (loop (cdr ks))))))
      (lambda (a b) (pointwise-binop heyting-implies a b)))))

;; ─── Validation ──────────────────────────────

(define (validate-heyting-algebra H samples)
  "Spot-check that H satisfies the Heyting algebra laws on SAMPLES.\nDelegates lattice law checks to validate-lattice, then tests\nmodus ponens (a ∧ (a → b) ≤ b) and the adjunction property\n(c ≤ (a → b) iff a ∧ c ≤ b) for all pairs and triples in\nSAMPLES. Returns #t if all laws hold, or a list of\n(violation-type element ...) entries describing failures.\n\nExamples:\n  (validate-heyting-algebra\n    (powerset-heyting '(x y z)) '((x) (y) (x y)))  => #t\n\nParameters:\n  H : any\n  samples : list\nReturns: any\nCategory: algebra\nKeywords: modus ponens, adjunction, residuation, law checking, validation\n\nSee also: `make-heyting-algebra', `validate-lattice'."
  (let ((violations '()))
    (define (fail! type . args)
      (set! violations (cons (cons type args) violations)))
    ;; Delegate lattice laws
    (let ((lattice-result (validate-lattice (heyting->lattice H) samples)))
      (when (not (eq? #t lattice-result))
        (set! violations (append lattice-result violations))))
    ;; Modus ponens: a ∧ (a → b) ≤ b
    (for-each
      (lambda (a)
        (for-each
          (lambda (b)
            (let ((imp (heyting-implies H a b)))
              (unless (heyting-leq? H (heyting-meet H a imp) b)
                (fail! 'modus-ponens a b))
              ;; Adjunction: for all c, c ≤ (a → b) iff a ∧ c ≤ b
              (for-each
                (lambda (c)
                  (let ((lhs (heyting-leq? H c imp))
                        (rhs (heyting-leq? H (heyting-meet H a c) b)))
                    (unless (eq? lhs rhs)
                      (fail! 'adjunction a b c))))
                samples)))
          samples))
      samples)
    (if (null? violations) #t (reverse violations))))
