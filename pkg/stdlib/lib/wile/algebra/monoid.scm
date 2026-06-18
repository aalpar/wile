;;; (wile algebra monoid) — Monoids
;;;
;;; A monoid is a set with an associative binary operation and an identity
;;; element: (S, ⊕, e) where a ⊕ (b ⊕ c) = (a ⊕ b) ⊕ c and e ⊕ a = a ⊕ e = a.

(define-record-type <monoid>
  (make-monoid* op identity)
  monoid?
  (op       monoid-op-fn)
  (identity monoid-identity))

(define (make-monoid op identity)
  "Construct a monoid from binary OP and IDENTITY element.\nOP must be associative (OP a (OP b c) = OP (OP a b) c) and IDENTITY\nmust be neutral (OP IDENTITY a = a and OP a IDENTITY = a). These laws\nare spot-checkable via `validate-monoid'.\n\nExamples:\n  (monoid-op (make-monoid + 0) 3 4)                       => 7\n  (monoid-op (make-monoid string-append \"\") \"a\" \"b\")  => \"ab\"\n\nParameters:\n  op : procedure\n  identity : any\nReturns: monoid\nCategory: algebra\nKeywords: monoid, associative, identity, algebraic structure\n\nSee also: `monoid-op', `monoid-identity', `validate-monoid'."
  (assert-procedure "make-monoid" op)
  (make-monoid* op identity))

(define (monoid-op M a b)
  "Apply monoid M's binary operation to A and B.\nA monoid operation is associative: combining A with the result\nof combining B and C gives the same result as combining the\nresult of A and B with C.\n\nExamples:\n  (monoid-op (make-monoid + 0) 3 4)          => 7\n  (monoid-op (make-monoid string-append \"\") \"a\" \"b\")  => \"ab\"\n\nParameters:\n  M : any\n  a : any\n  b : any\nReturns: any\nCategory: algebra\nKeywords: binary operation, combine, oplus, otimes, associative, append, concat\n\nSee also: `monoid-identity', `monoid-fold'."
  ((monoid-op-fn M) a b))

(define (monoid-fold M lst)
  "Fold LST under monoid M by combining elements left to right.\nStarts from M's identity element and accumulates by applying\nM's binary operation to each element in order. Returns the\nidentity when LST is empty.\n\nExamples:\n  (monoid-fold (make-monoid + 0) '(1 2 3))  => 6\n  (monoid-fold (make-monoid * 1) '())        => 1\n\nParameters:\n  M : any\n  lst : list\nReturns: any\nCategory: algebra\nKeywords: fold, reduce, accumulate, aggregate, sum, product, foldl\n\nSee also: `monoid-op', `monoid-power'."
  (let loop ((acc (monoid-identity M)) (xs lst))
    (if (null? xs) acc
        (loop (monoid-op M acc (car xs)) (cdr xs)))))

(define (monoid-power M a n)
  "Combine A with itself N times under monoid M.\nRepeated application of M's binary operation: the result is\nA combined N times starting from the identity. Returns the\nidentity when N is zero or negative. Runs in O(N) time.\n\nExamples:\n  (monoid-power (make-monoid + 0) 5 3)  => 15\n  (monoid-power (make-monoid * 1) 2 0)  => 1\n\nParameters:\n  M : any\n  a : any\n  n : integer\nReturns: any\nCategory: algebra\nKeywords: repeated, exponentiation, power, multiply, scale, iterated\n\nSee also: `monoid-op', `monoid-fold'."
  (let loop ((acc (monoid-identity M)) (remaining n))
    (if (<= remaining 0) acc
        (loop (monoid-op M acc a) (- remaining 1)))))

(define-syntax with-monoid
  (syntax-rules ()
    ((with-monoid M (op identity) body ...)
     (let ((tmp M))
       (let ((op       (lambda (a b) (monoid-op tmp a b)))
             (identity (monoid-identity tmp)))
         body ...)))))

(define (validate-monoid M samples)
  "Spot-check that M satisfies the monoid laws on SAMPLES.\nTests left identity, right identity, and associativity for all\nelements and triples in SAMPLES. Returns #t if all laws hold,\nor a list of (violation-type element ...) entries describing failures.\n\nExamples:\n  (validate-monoid (make-monoid + 0) '(1 2 3))  => #t\n\nParameters:\n  M : any\n  samples : list\nReturns: any\nCategory: algebra\nKeywords: associativity, identity element, neutral element, law checking, validation\n\nSee also: `make-monoid', `monoid-op', `monoid-identity'."
  (let ((fail! (make-violation-reporter))
        (e (monoid-identity M)))
    (for-each
      (lambda (a)
        ;; Left identity
        (unless (equal? (monoid-op M e a) a)
          (fail! 'left-identity a))
        ;; Right identity
        (unless (equal? (monoid-op M a e) a)
          (fail! 'right-identity a))
        ;; Associativity (with all pairs)
        (for-each
          (lambda (b)
            (for-each
              (lambda (c)
                (unless (equal? (monoid-op M (monoid-op M a b) c)
                                (monoid-op M a (monoid-op M b c)))
                  (fail! 'associativity a b c)))
              samples))
          samples))
      samples)
    (fail!)))
