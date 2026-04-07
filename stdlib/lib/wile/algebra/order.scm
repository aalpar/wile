;;; (wile algebra order) — Partial orders
;;;
;;; A partial order is a reflexive, antisymmetric, transitive relation.
;;; Represented as an R7RS record holding a single leq? predicate.

(define-record-type <partial-order>
  (make-partial-order leq?)
  partial-order?
  (leq? po-leq-fn))

(define (po-leq? po a b)
  "Test whether A is less than or equal to B under partial order PO.\nReturns #t if A precedes or equals B in the ordering, #f otherwise.\n\nExamples:\n  (let ((po (make-partial-order <=)))\n    (po-leq? po 2 3))  => #t\n  (let ((po (make-partial-order <=)))\n    (po-leq? po 3 2))  => #f\n\nParameters:\n  po : any\n  a : any\n  b : any\nReturns: boolean\nCategory: algebra"
  ((po-leq-fn po) a b))

(define (po-comparable? po a b)
  "Test whether A and B are comparable under partial order PO.\nTwo elements are comparable if one precedes the other. In a\npartial order, some pairs may be incomparable (neither A <= B\nnor B <= A holds).\n\nExamples:\n  (let ((po (make-partial-order <=)))\n    (po-comparable? po 2 3))  => #t\n  (let ((po (make-partial-order <=)))\n    (po-comparable? po 3 2))  => #t\n\nParameters:\n  po : any\n  a : any\n  b : any\nReturns: boolean\nCategory: algebra\n\nSee also: `po-leq?'."
  (or (po-leq? po a b)
      (po-leq? po b a)))

(define (po-monotone? po f a b)
  "Test whether function F preserves the ordering of A and B under PO.\nA function is monotone if A <= B implies F(A) <= F(B). Returns #t\nvacuously when A is not less than or equal to B, since the\nprecondition for the monotonicity check is not met.\n\nExamples:\n  (let ((po (make-partial-order <=)))\n    (po-monotone? po (lambda (x) (* x 2)) 1 3))  => #t\n  (let ((po (make-partial-order <=)))\n    (po-monotone? po (lambda (x) (- x)) 1 3))    => #f\n\nParameters:\n  po : any\n  f : procedure\n  a : any\n  b : any\nReturns: boolean\nCategory: algebra\n\nSee also: `po-leq?', `po-comparable?'."
  (if (po-leq? po a b)
      (po-leq? po (f a) (f b))
      #t))

(define (validate-partial-order po samples)
  "Spot-check that PO satisfies partial order laws on SAMPLES.\nTests reflexivity (A <= A) and transitivity (A <= B and B <= C\nimplies A <= C) for all elements and triples in SAMPLES.\nAntisymmetry is not checked because no equality predicate is\navailable. Returns #t if all tested laws hold, or a list of\n(violation-type element ...) entries describing failures.\n\nExamples:\n  (validate-partial-order (make-partial-order <=) '(1 2 3))  => #t\n\nParameters:\n  po : any\n  samples : list\nReturns: any\nCategory: algebra\n\nSee also: `make-partial-order', `po-leq?'."
  (let ((violations '()))
    (define (fail! type . args)
      (set! violations (cons (cons type args) violations)))
    ;; Reflexivity: a ≤ a
    (for-each
      (lambda (a) (unless (po-leq? po a a) (fail! 'reflexivity a)))
      samples)
    ;; Antisymmetry: a ≤ b ∧ b ≤ a ⟹ a = b
    ;; Without an equality predicate we cannot fully check this.
    ;; Transitivity: a ≤ b ∧ b ≤ c ⟹ a ≤ c
    (for-each
      (lambda (a)
        (for-each
          (lambda (b)
            (when (po-leq? po a b)
              (for-each
                (lambda (c)
                  (when (and (po-leq? po b c)
                             (not (po-leq? po a c)))
                    (fail! 'transitivity a b c)))
                samples)))
          samples))
      samples)
    (if (null? violations) #t (reverse violations))))
