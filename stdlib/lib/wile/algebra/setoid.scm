;;; (wile algebra setoid) — Setoids
;;;
;;; A setoid is a set equipped with an explicit equivalence relation:
;;; (S, ≡) where ≡ is reflexive (a ≡ a), symmetric (a ≡ b ⟹ b ≡ a),
;;; and transitive (a ≡ b ∧ b ≡ c ⟹ a ≡ c).

(define-record-type <setoid>
  (make-setoid equiv-fn)
  setoid?
  (equiv-fn setoid-equiv-fn))

(define (setoid-equiv? S a b)
  "Test whether A and B are equivalent under setoid S.\nApplies S's equivalence relation to A and B, returning #t\nif they are considered equivalent, #f otherwise.\n\nExamples:\n  (setoid-equiv? (default-setoid) '(1 2) '(1 2))  => #t\n  (setoid-equiv? (default-setoid) 1 2)             => #f\n\nParameters:\n  S : any\n  a : any\n  b : any\nReturns: boolean\nCategory: algebra\nKeywords: equivalence, equality, equiv, congruence, identification\n\nSee also: `make-setoid', `validate-setoid'."
  ((setoid-equiv-fn S) a b))

(define (default-setoid)
  "Construct a setoid using Scheme's equal? as the equivalence relation.\n\nExamples:\n  (setoid-equiv? (default-setoid) '(1 2) '(1 2))  => #t\n  (setoid-equiv? (default-setoid) 'a 'b)           => #f\n\nReturns: any\nCategory: algebra\nKeywords: equal, structural equality, default\n\nSee also: `numeric-setoid', `eqv-setoid'."
  (make-setoid equal?))

(define (numeric-setoid)
  "Construct a setoid using Scheme's = as the equivalence relation.\nOnly valid for numeric elements.\n\nExamples:\n  (setoid-equiv? (numeric-setoid) 1 1.0)    => #t\n  (setoid-equiv? (numeric-setoid) 1/2 0.5)  => #t\n\nReturns: any\nCategory: algebra\nKeywords: numeric, number, mathematical equality\n\nSee also: `default-setoid', `string-setoid'."
  (make-setoid =))

(define (string-setoid)
  "Construct a setoid using string=? as the equivalence relation.\nOnly valid for string elements.\n\nExamples:\n  (setoid-equiv? (string-setoid) \"abc\" \"abc\")  => #t\n  (setoid-equiv? (string-setoid) \"a\" \"b\")      => #f\n\nReturns: any\nCategory: algebra\nKeywords: string, text, string equality\n\nSee also: `default-setoid', `numeric-setoid'."
  (make-setoid string=?))

(define (eqv-setoid)
  "Construct a setoid using Scheme's eqv? as the equivalence relation.\nUses identity/value equality -- same object or same simple value.\n\nExamples:\n  (setoid-equiv? (eqv-setoid) 'a 'a)  => #t\n  (setoid-equiv? (eqv-setoid) 1 1)    => #t\n\nReturns: any\nCategory: algebra\nKeywords: eqv, identity, pointer equality, value equality\n\nSee also: `default-setoid'."
  (make-setoid eqv?))

(define (setoid-equivalence-class S x samples)
  "Return elements of SAMPLES equivalent to X under setoid S.\nFilters SAMPLES to those elements for which S's equivalence\nrelation holds with X. The result always includes X itself\nwhen X appears in SAMPLES.\n\nExamples:\n  (setoid-equivalence-class (numeric-setoid) 1 '(1 1.0 2 3))  => (1 1.0)\n  (setoid-equivalence-class (default-setoid) 5 '(1 2 3))      => ()\n\nParameters:\n  S : any\n  x : any\n  samples : list\nReturns: list\nCategory: algebra\nKeywords: equivalence class, partition, quotient, coset, fiber, preimage\n\nSee also: `setoid-equiv?', `make-setoid'."
  (let loop ((xs samples) (acc '()))
    (cond
      ((null? xs) (reverse acc))
      ((setoid-equiv? S x (car xs))
       (loop (cdr xs) (cons (car xs) acc)))
      (else (loop (cdr xs) acc)))))

(define (validate-setoid S samples)
  "Spot-check that S satisfies the setoid laws on SAMPLES.\nTests reflexivity (a ≡ a), symmetry (a ≡ b ⟹ b ≡ a), and\ntransitivity (a ≡ b ∧ b ≡ c ⟹ a ≡ c) for all elements and\ntriples in SAMPLES. Returns #t if all laws hold, or a list of\n(violation-type element ...) entries describing failures.\n\nExamples:\n  (validate-setoid (default-setoid) '(1 2 3))  => #t\n\nParameters:\n  S : any\n  samples : list\nReturns: any\nCategory: algebra\nKeywords: reflexivity, symmetry, transitivity, validation, law checking, equivalence\n\nSee also: `make-setoid', `setoid-equiv?'."
  (let ((violations '()))
    (define (fail! type . args)
      (set! violations (cons (cons type args) violations)))
    ;; Reflexivity: a ≡ a
    (for-each
      (lambda (a) (unless (setoid-equiv? S a a) (fail! 'reflexivity a)))
      samples)
    ;; Symmetry: a ≡ b ⟹ b ≡ a
    (for-each
      (lambda (a)
        (for-each
          (lambda (b)
            (when (and (setoid-equiv? S a b)
                       (not (setoid-equiv? S b a)))
              (fail! 'symmetry a b)))
          samples))
      samples)
    ;; Transitivity: a ≡ b ∧ b ≡ c ⟹ a ≡ c
    (for-each
      (lambda (a)
        (for-each
          (lambda (b)
            (when (setoid-equiv? S a b)
              (for-each
                (lambda (c)
                  (when (and (setoid-equiv? S b c)
                             (not (setoid-equiv? S a c)))
                    (fail! 'transitivity a b c)))
                samples)))
          samples))
      samples)
    (if (null? violations) #t (reverse violations))))

(define-syntax with-setoid
  (syntax-rules ()
    ((with-setoid S (equiv?) body ...)
     (let ((tmp S))
       (let ((equiv? (lambda (a b) (setoid-equiv? tmp a b))))
         body ...)))))
