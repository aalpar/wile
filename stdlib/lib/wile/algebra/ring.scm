;;; (wile algebra ring) — Rings and fields
;;;
;;; A ring (R, +, ×, 0, 1, -) is a semiring where (R, +, 0, -) is an
;;; abelian group. A field adds multiplicative inverses for nonzero elements.

;; ─── Rings ───────────────────────────────────

(define-record-type <ring>
  (make-ring* plus-fn times-fn zero one negate-fn)
  ring?
  (plus-fn   ring-plus-fn)
  (times-fn  ring-times-fn)
  (zero      ring-zero)
  (one       ring-one)
  (negate-fn ring-negate-fn))

(define (make-ring plus times zero one negate)
  "Construct a ring from PLUS, TIMES, ZERO, ONE, and NEGATE.\nThe additive structure (PLUS, ZERO, NEGATE) must form an abelian\ngroup, TIMES must be associative with ONE as identity, and TIMES\nmust distribute over PLUS from both sides.\n\nExamples:\n  (let ((R (make-ring + * 0 1 -)))\n    (ring-plus R 3 4))   => 7\n  (let ((R (make-ring + * 0 1 -)))\n    (ring-times R 3 4))  => 12\n\nParameters:\n  plus : procedure\n  times : procedure\n  zero : any\n  one : any\n  negate : procedure\nReturns: any\nCategory: algebra\nKeywords: ring, algebraic structure, additive group, distributive\n\nSee also: `ring->semiring', `ring->additive-group', `validate-ring'."
  (assert-procedure "make-ring" plus)
  (assert-procedure "make-ring" times)
  (assert-procedure "make-ring" negate)
  (make-ring* plus times zero one negate))

(define (ring-plus R a b)
  "Add A and B under ring R's additive operation.\n\nExamples:\n  (ring-plus (integer-ring) 3 4)  => 7\n\nParameters:\n  R : any\n  a : any\n  b : any\nReturns: any\nCategory: algebra\nKeywords: addition, add, sum, plus, ring addition"
  ((ring-plus-fn R) a b))

(define (ring-times R a b)
  "Multiply A and B under ring R's multiplicative operation.\n\nExamples:\n  (ring-times (integer-ring) 3 4)  => 12\n\nParameters:\n  R : any\n  a : any\n  b : any\nReturns: any\nCategory: algebra\nKeywords: multiplication, multiply, product, times, ring multiplication"
  ((ring-times-fn R) a b))

(define (ring-negate R a)
  "Return the additive inverse of A in ring R.\nThe additive inverse is the element that, when added to A,\nyields the ring's zero element.\n\nExamples:\n  (ring-negate (integer-ring) 5)   => -5\n  (ring-negate (modular-ring 7) 3) => 4\n\nParameters:\n  R : any\n  a : any\nReturns: any\nCategory: algebra\nKeywords: additive inverse, negation, minus, opposite, unary minus\n\nSee also: `ring-minus', `ring-plus'."
  ((ring-negate-fn R) a))

(define (ring-minus R a b)
  "Subtract B from A in ring R.\nComputed as A plus the additive inverse of B.\n\nExamples:\n  (ring-minus (integer-ring) 10 3)  => 7\n  (ring-minus (modular-ring 5) 1 3) => 3\n\nParameters:\n  R : any\n  a : any\n  b : any\nReturns: any\nCategory: algebra\nKeywords: subtraction, subtract, difference, minus\n\nSee also: `ring-negate', `ring-plus'."
  (ring-plus R a (ring-negate R b)))

(define (ring->semiring R)
  "Project ring R to its underlying semiring by forgetting negation.\nThe resulting semiring has the same PLUS, TIMES, ZERO, and ONE.\n\nExamples:\n  (let ((S (ring->semiring (integer-ring))))\n    (semiring-plus S 3 4))  => 7\n\nParameters:\n  R : any\nReturns: any\nCategory: algebra\nKeywords: forgetful functor, projection, forget negation\n\nSee also: `ring->additive-group', `make-semiring'."
  (make-semiring (ring-plus-fn R) (ring-times-fn R)
                 (ring-zero R) (ring-one R)))

(define (ring->additive-group R)
  "Extract the additive group (PLUS, ZERO, NEGATE) from ring R.\n\nExamples:\n  (let ((G (ring->additive-group (integer-ring))))\n    (group-inverse G 5))  => -5\n\nParameters:\n  R : any\nReturns: any\nCategory: algebra\nKeywords: additive group, forgetful functor, projection, extract\n\nSee also: `ring->semiring', `make-group'."
  (make-group (ring-plus-fn R) (ring-zero R) (ring-negate-fn R)))

(define-syntax with-ring
  (syntax-rules ()
    ((with-ring R (plus times zero one negate) body ...)
     (let ((tmp R))
       (let ((plus   (lambda (a b) (ring-plus tmp a b)))
             (times  (lambda (a b) (ring-times tmp a b)))
             (zero   (ring-zero tmp))
             (one    (ring-one tmp))
             (negate (lambda (a) (ring-negate tmp a))))
         body ...)))))

;; ─── Pre-built ring instances ────────────────

(define (integer-ring)
  "Construct the ring of integers with standard arithmetic.\nPLUS is +, TIMES is *, ZERO is 0, ONE is 1, NEGATE is -.\n\nExamples:\n  (ring-plus (integer-ring) 3 4)   => 7\n  (ring-times (integer-ring) 3 4)  => 12\n\nReturns: any\nCategory: algebra\nKeywords: integers, Z, standard arithmetic, whole numbers\n\nSee also: `modular-ring', `make-ring'."
  (make-ring + * 0 1 -))

(define (modular-ring n)
  "Construct the ring of integers modulo N.\nAll operations reduce results modulo N. Elements are integers\nin the range [0, N). For prime N this is also a field, but\nthis constructor does not provide a multiplicative inverse.\n\nExamples:\n  (let ((Z5 (modular-ring 5)))\n    (ring-plus Z5 3 4))   => 2\n  (let ((Z5 (modular-ring 5)))\n    (ring-times Z5 3 4))  => 2\n\nParameters:\n  n : integer\nReturns: any\nCategory: algebra\nKeywords: modular arithmetic, modulo, mod, Zn, cyclic, remainder, clock\n\nSee also: `integer-ring', `make-ring'."
  (make-ring
    (lambda (a b) (modulo (+ a b) n))
    (lambda (a b) (modulo (* a b) n))
    0 1
    (lambda (a) (modulo (- a) n))))

;; ─── Ring validation ─────────────────────────

(define (validate-ring R samples)
  "Spot-check that R satisfies the ring laws on SAMPLES.\nTests additive identity, multiplicative identity, additive\ninverse, and left distributivity for all elements and triples\nin SAMPLES. Returns #t if all laws hold, or a list of\n(violation-type element ...) entries describing failures.\n\nExamples:\n  (validate-ring (integer-ring) '(0 1 2 3))  => #t\n\nParameters:\n  R : any\n  samples : list\nReturns: any\nCategory: algebra\nKeywords: distributivity, inverse, identity, law checking, validation\n\nSee also: `make-ring', `validate-semiring', `validate-field'."
  (let ((fail! (make-violation-reporter))
        (z (ring-zero R))
        (o (ring-one R)))
    (for-each
      (lambda (a)
        ;; Additive identity
        (unless (equal? (ring-plus R z a) a)
          (fail! 'additive-left-identity a))
        ;; Multiplicative identity
        (unless (equal? (ring-times R o a) a)
          (fail! 'multiplicative-left-identity a))
        ;; Additive inverse
        (unless (equal? (ring-plus R a (ring-negate R a)) z)
          (fail! 'additive-inverse a))
        ;; Distributivity
        (for-each
          (lambda (b)
            (for-each
              (lambda (c)
                (unless (equal? (ring-times R a (ring-plus R b c))
                                (ring-plus R (ring-times R a b)
                                             (ring-times R a c)))
                  (fail! 'left-distributivity a b c)))
              samples))
          samples))
      samples)
    (fail!)))

;; ─── Fields ──────────────────────────────────

(define-record-type <field>
  (make-field* plus-fn times-fn zero one negate-fn reciprocal-fn)
  field?
  (plus-fn       field-plus-fn)
  (times-fn      field-times-fn)
  (zero          field-zero)
  (one           field-one)
  (negate-fn     field-negate-fn)
  (reciprocal-fn field-reciprocal-fn))

(define (make-field plus times zero one negate reciprocal)
  "Construct a field from PLUS, TIMES, ZERO, ONE, NEGATE, and RECIPROCAL.\nA field is a ring where every nonzero element has a multiplicative\ninverse given by RECIPROCAL. RECIPROCAL need not be defined for ZERO.\n\nExamples:\n  (let ((F (make-field + * 0 1 - (lambda (x) (/ 1 x)))))\n    (field-times F 3 (field-reciprocal F 3)))  => 1\n\nParameters:\n  plus : procedure\n  times : procedure\n  zero : any\n  one : any\n  negate : procedure\n  reciprocal : procedure\nReturns: any\nCategory: algebra\nKeywords: field, division, algebraic structure, multiplicative inverse\n\nSee also: `field->ring', `validate-field'."
  (assert-procedure "make-field" plus)
  (assert-procedure "make-field" times)
  (assert-procedure "make-field" negate)
  (assert-procedure "make-field" reciprocal)
  (make-field* plus times zero one negate reciprocal))

(define (field-plus F a b)
  "Add A and B under field F's additive operation.\n\nExamples:\n  (field-plus (rational-field) 1/3 1/6)  => 1/2\n\nParameters:\n  F : any\n  a : any\n  b : any\nReturns: any\nCategory: algebra\nKeywords: addition, add, sum, plus, field addition"
  ((field-plus-fn F) a b))

(define (field-times F a b)
  "Multiply A and B under field F's multiplicative operation.\n\nExamples:\n  (field-times (rational-field) 2/3 3/4)  => 1/2\n\nParameters:\n  F : any\n  a : any\n  b : any\nReturns: any\nCategory: algebra\nKeywords: multiplication, multiply, product, times, field multiplication"
  ((field-times-fn F) a b))

(define (field-negate F a)
  "Return the additive inverse of A in field F.\n\nExamples:\n  (field-negate (rational-field) 3/4)  => -3/4\n\nParameters:\n  F : any\n  a : any\nReturns: any\nCategory: algebra\nKeywords: additive inverse, negation, minus, opposite"
  ((field-negate-fn F) a))

(define (field-reciprocal F a)
  "Return the multiplicative inverse of A in field F.\nA must be nonzero. The result satisfies: A times its\nreciprocal equals the field's multiplicative identity (one).\n\nExamples:\n  (field-reciprocal (rational-field) 4)    => 1/4\n  (field-reciprocal (rational-field) 2/3)  => 3/2\n\nParameters:\n  F : any\n  a : any\nReturns: any\nCategory: algebra\nKeywords: multiplicative inverse, reciprocal, 1/x, division, invert\n\nSee also: `field-divide', `field-times'."
  ((field-reciprocal-fn F) a))

(define (field-divide F a b)
  "Divide A by B in field F.\nComputed as A times the multiplicative inverse of B.\nB must be nonzero.\n\nExamples:\n  (field-divide (rational-field) 3 4)  => 3/4\n  (field-divide (rational-field) 1 3)  => 1/3\n\nParameters:\n  F : any\n  a : any\n  b : any\nReturns: any\nCategory: algebra\nKeywords: division, divide, quotient, ratio, fraction\n\nSee also: `field-reciprocal', `field-times'."
  (field-times F a (field-reciprocal F b)))

(define (field->ring F)
  "Project field F to its underlying ring by forgetting the reciprocal.\nThe resulting ring has the same PLUS, TIMES, ZERO, ONE, and NEGATE.\n\nExamples:\n  (let ((R (field->ring (rational-field))))\n    (ring-plus R 1/3 2/3))  => 1\n\nParameters:\n  F : any\nReturns: any\nCategory: algebra\nKeywords: forgetful functor, projection, forget reciprocal\n\nSee also: `make-field', `make-ring'."
  (make-ring (field-plus-fn F) (field-times-fn F)
             (field-zero F) (field-one F) (field-negate-fn F)))

(define-syntax with-field
  (syntax-rules ()
    ((with-field F (plus times zero one negate reciprocal) body ...)
     (let ((tmp F))
       (let ((plus       (lambda (a b) (field-plus tmp a b)))
             (times      (lambda (a b) (field-times tmp a b)))
             (zero       (field-zero tmp))
             (one        (field-one tmp))
             (negate     (lambda (a) (field-negate tmp a)))
             (reciprocal (lambda (a) (field-reciprocal tmp a))))
         body ...)))))

;; ─── Pre-built field instances ───────────────

(define (rational-field)
  "Construct the field of rational numbers with standard arithmetic.\nPLUS is +, TIMES is *, ZERO is 0, ONE is 1, NEGATE is -,\nand RECIPROCAL is 1/x.\n\nExamples:\n  (field-divide (rational-field) 3 4)      => 3/4\n  (field-reciprocal (rational-field) 5)    => 1/5\n\nReturns: any\nCategory: algebra\nKeywords: rationals, Q, fractions, rational numbers, exact arithmetic\n\nSee also: `make-field', `field->ring'."
  (make-field + * 0 1 - (lambda (x) (/ 1 x))))

;; ─── Field validation ────────────────────────

(define (validate-field F samples)
  "Spot-check that F satisfies the field laws on SAMPLES.\nDelegates ring law checks to `validate-ring', then additionally\ntests that every nonzero element in SAMPLES has a multiplicative\ninverse. SAMPLES should exclude the zero element. Returns #t if\nall laws hold, or a list of (violation-type element ...) entries\ndescribing failures.\n\nExamples:\n  (validate-field (rational-field) '(1 2 1/2 3/4))  => #t\n\nParameters:\n  F : any\n  samples : list\nReturns: any\nCategory: algebra\nKeywords: multiplicative inverse, division, law checking, validation\n\nSee also: `make-field', `field->ring', `validate-ring'."
  (let ((fail! (make-violation-reporter))
        (z (field-zero F))
        (o (field-one F)))
    ;; Ring laws
    (let ((ring-result (validate-ring (field->ring F) samples)))
      (unless (eq? #t ring-result)
        (for-each (lambda (v) (apply fail! v)) ring-result)))
    ;; Multiplicative inverse for nonzero elements
    (for-each
      (lambda (a)
        (unless (equal? a z)
          (unless (equal? (field-times F a (field-reciprocal F a)) o)
            (fail! 'multiplicative-inverse a))))
      samples)
    (fail!)))
