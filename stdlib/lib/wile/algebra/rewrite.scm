;;; (wile algebra rewrite) — Equational term rewriting from algebraic axioms
;;;
;;; Generates rewrite rules from declared axioms (identity, commutativity,
;;; absorbing, idempotence, involution). Rules are compiled via a term
;;; protocol that abstracts over concrete term representations.
;;;
;;; Two design choices that matter:
;;;   1. Element matching uses predicates (value → boolean), not equal?.
;;;   2. make-term receives (term, new-operands) → term. The term carries
;;;      metadata the algebra doesn't touch; the rebuilder needs it.
;;;
;;; The normalizer returns #f for "no match." Internally, rule lambdas use
;;; *no-match* (a unique identity object checked via eq?) to distinguish
;;; "no match" from a legitimate #f rewrite result. The sentinel and its
;;; predicate no-match? are exported for use by (wile algebra symbolic).

;; ─── No-match sentinel ──────────────────────

(define *no-match* (list 'no-match))

(define (no-match? x)
  "Test whether X is the internal no-match sentinel.\nRewrite rules return this sentinel (via eq? identity) to indicate\nthat no rewriting step applied. Callers of make-normalizer see #f\ninstead; this predicate is for internal rule dispatch only.\n\nExamples:\n  (no-match? *no-match*)  => #t\n  (no-match? 42)          => #f\n\nParameters:\n  x : any\nReturns: boolean\nCategory: algebra"
  (eq? x *no-match*))

;; ─── Term protocol ──────────────────────────

(define-record-type <term-protocol>
  (make-term-protocol* compound-term?-fn get-operator-fn get-operands-fn
                       make-term-fn compare-fn)
  term-protocol?
  (compound-term?-fn term-compound?-fn)
  (get-operator-fn   term-get-operator-fn)
  (get-operands-fn   term-get-operands-fn)
  (make-term-fn      term-make-term-fn)
  (compare-fn        term-compare-fn))

(define (make-term-protocol compound-term? get-operator get-operands
                            make-term compare)
  "Construct a term protocol for abstract term representations.\nCOMPOUND-TERM? tests whether a value is a compound term.\nGET-OPERATOR and GET-OPERANDS extract parts of a compound term.\nMAKE-TERM rebuilds a term with new operands while preserving\nmetadata. COMPARE returns #t when its first argument should sort\nbefore the second, used by commutativity rules to normalize\noperand order.\n\nExamples:\n  ;; A list-based term protocol: (op arg ...)\n  (make-term-protocol\n    pair?\n    car\n    cdr\n    (lambda (term new-args) (cons (car term) new-args))\n    (lambda (a b) (< a b)))\n\nParameters:\n  compound-term? : procedure\n  get-operator : procedure\n  get-operands : procedure\n  make-term : procedure\n  compare : procedure\nReturns: any\nCategory: algebra\nKeywords: term rewriting, abstract syntax, term algebra, pattern matching\n\nSee also: `term-compound?', `term-get-operator', `term-get-operands', `term-make-term'."
  (make-term-protocol* compound-term? get-operator get-operands
                       make-term compare))

(define (term-compound? proto x)
  "Test whether X is a compound term under protocol PROTO.\nA compound term has an operator and operands, as opposed to\nan atomic value like a number or variable.\n\nExamples:\n  ;; With a list-based protocol where compound terms are pairs:\n  ;; (term-compound? proto '(+ 1 2))  => #t\n  ;; (term-compound? proto 42)        => #f\n\nParameters:\n  proto : any\n  x : any\nReturns: boolean\nCategory: algebra"
  ((term-compound?-fn proto) x))

(define (term-get-operator proto term)
  "Extract the operator from compound TERM under protocol PROTO.\n\nExamples:\n  ;; With a list-based protocol:\n  ;; (term-get-operator proto '(+ 1 2))  => +\n\nParameters:\n  proto : any\n  term : any\nReturns: any\nCategory: algebra"
  ((term-get-operator-fn proto) term))

(define (term-get-operands proto term)
  "Extract the list of operands from compound TERM under protocol PROTO.\n\nExamples:\n  ;; With a list-based protocol:\n  ;; (term-get-operands proto '(+ 1 2))  => (1 2)\n\nParameters:\n  proto : any\n  term : any\nReturns: list\nCategory: algebra"
  ((term-get-operands-fn proto) term))

(define (term-make-term proto term new-args)
  "Rebuild TERM with NEW-ARGS as operands under protocol PROTO.\nThe original TERM's operator and any metadata are preserved;\nonly the operands change.\n\nExamples:\n  ;; With a list-based protocol:\n  ;; (term-make-term proto '(+ 1 2) '(3 4))  => (+ 3 4)\n\nParameters:\n  proto : any\n  term : any\n  new-args : list\nReturns: any\nCategory: algebra"
  ((term-make-term-fn proto) term new-args))

(define (term-compare proto a b)
  "Test whether A should sort before B under protocol PROTO's term ordering.\nUsed by commutativity rules to pick a canonical operand order.\n\nExamples:\n  ;; With a numeric comparison protocol:\n  ;; (term-compare proto 1 2)  => #t\n  ;; (term-compare proto 3 1)  => #f\n\nParameters:\n  proto : any\n  a : any\n  b : any\nReturns: boolean\nCategory: algebra"
  ((term-compare-fn proto) a b))

;; ─── Axiom types ────────────────────────────

(define-record-type <identity-axiom>
  (make-identity-axiom op element)
  identity-axiom?
  (op      identity-axiom-op)
  (element identity-axiom-element))   ;; predicate: value → boolean

(define-record-type <commutativity-axiom>
  (make-commutativity-axiom op)
  commutativity-axiom?
  (op commutativity-axiom-op))

(define-record-type <absorbing-axiom>
  (make-absorbing-axiom op element)
  absorbing-axiom?
  (op      absorbing-axiom-op)
  (element absorbing-axiom-element))  ;; predicate: value → boolean

(define-record-type <idempotence-axiom>
  (make-idempotence-axiom op)
  idempotence-axiom?
  (op idempotence-axiom-op))

(define-record-type <involution-axiom>
  (make-involution-axiom op)
  involution-axiom?
  (op involution-axiom-op))

(define-record-type <absorption-axiom>
  (make-absorption-axiom op-outer op-inner)
  absorption-axiom?
  (op-outer absorption-axiom-op-outer)
  (op-inner absorption-axiom-op-inner))

(define-record-type <associativity-axiom>
  (make-associativity-axiom op)
  associativity-axiom?
  (op associativity-axiom-op))

(define (directional-axiom? x)
  "Test whether X is a directional axiom — one whose rule rewrites\nin a single direction (not symmetric like commutativity).\nCurrently only associativity axioms are directional.\n\nExamples:\n  (directional-axiom? (make-associativity-axiom '+))         => #t\n  (directional-axiom? (make-identity-axiom '+ zero?))        => #f\n\nParameters:\n  x : any\nReturns: boolean\nCategory: algebra\nKeywords: axiom, directional, associativity, rewrite direction"
  (associativity-axiom? x))

(define (axiom? x)
  "Test whether X is a recognized axiom type.\nReturns #t for identity, commutativity, absorbing, idempotence,\ninvolution, absorption, or associativity axiom records.\n\nExamples:\n  (axiom? (make-identity-axiom '+ zero?))     => #t\n  (axiom? (make-commutativity-axiom '+))       => #t\n  (axiom? 42)                                  => #f\n\nParameters:\n  x : any\nReturns: boolean\nCategory: algebra\nKeywords: axiom, rewrite rule, equational, algebraic law\n\nSee also: `make-identity-axiom', `make-commutativity-axiom', `make-absorbing-axiom'."
  (or (identity-axiom? x)
      (commutativity-axiom? x)
      (absorbing-axiom? x)
      (idempotence-axiom? x)
      (involution-axiom? x)
      (absorption-axiom? x)
      (associativity-axiom? x)))

;; ─── Axiom → rewrite rules ─────────────────

(define (axiom->rules axiom proto)
  "Compile AXIOM into a list of rewrite-rule procedures using term protocol PROTO.\nEach rule is a procedure (term -> value-or-*no-match*) that attempts\none rewriting step. Identity axioms produce two rules (left and right),\ncommutativity produces one rule that normalizes by term ordering,\nand involution produces one rule that collapses f(f(x)) to x.\n\nExamples:\n  (axiom->rules (make-identity-axiom '+ zero?) proto)\n    => list of two rule procedures (left and right identity)\n  (axiom->rules (make-involution-axiom 'neg) proto)\n    => list of one rule: neg(neg(x)) => x\n\nParameters:\n  axiom : any\n  proto : any\nReturns: list\nCategory: algebra\nKeywords: compile, rewrite rules, term rewriting, normalization\n\nSee also: `make-normalizer', `axiom?', `make-term-protocol'."
  (cond
    ((identity-axiom? axiom)
     (let ((target-op (identity-axiom-op axiom))
           (e?        (identity-axiom-element axiom)))
       (list
         ;; op(x, e) → x
         (lambda (term)
           (let ((op (term-get-operator proto term))
                 (args (term-get-operands proto term)))
             (if (and (equal? op target-op)
                      (= (length args) 2)
                      (e? (cadr args)))
                 (car args)
                 *no-match*)))
         ;; op(e, x) → x
         (lambda (term)
           (let ((op (term-get-operator proto term))
                 (args (term-get-operands proto term)))
             (if (and (equal? op target-op)
                      (= (length args) 2)
                      (e? (car args)))
                 (cadr args)
                 *no-match*))))))

    ((commutativity-axiom? axiom)
     (let ((target-op (commutativity-axiom-op axiom)))
       (list
         ;; op(x, y) → op(y, x) when y < x
         (lambda (term)
           (let ((op (term-get-operator proto term))
                 (args (term-get-operands proto term)))
             (if (and (equal? op target-op)
                      (= (length args) 2)
                      (term-compare proto (cadr args) (car args)))
                 (term-make-term proto term (list (cadr args) (car args)))
                 *no-match*))))))

    ((absorbing-axiom? axiom)
     (let ((target-op (absorbing-axiom-op axiom))
           (z?        (absorbing-axiom-element axiom)))
       (list
         ;; op(x, z) → z
         (lambda (term)
           (let ((op (term-get-operator proto term))
                 (args (term-get-operands proto term)))
             (if (and (equal? op target-op)
                      (= (length args) 2)
                      (z? (cadr args)))
                 (cadr args)
                 *no-match*)))
         ;; op(z, x) → z
         (lambda (term)
           (let ((op (term-get-operator proto term))
                 (args (term-get-operands proto term)))
             (if (and (equal? op target-op)
                      (= (length args) 2)
                      (z? (car args)))
                 (car args)
                 *no-match*))))))

    ((idempotence-axiom? axiom)
     (let ((target-op (idempotence-axiom-op axiom)))
       (list
         ;; op(x, x) → x
         (lambda (term)
           (let ((op (term-get-operator proto term))
                 (args (term-get-operands proto term)))
             (if (and (equal? op target-op)
                      (= (length args) 2)
                      (equal? (car args) (cadr args)))
                 (car args)
                 *no-match*))))))

    ((involution-axiom? axiom)
     (let ((target-op (involution-axiom-op axiom)))
       (list
         ;; op(op(x)) → x  (unary: single operand)
         (lambda (term)
           (let ((op (term-get-operator proto term))
                 (args (term-get-operands proto term)))
             (if (and (equal? op target-op)
                      (= (length args) 1))
                 (let ((inner (car args)))
                   (if (and (term-compound? proto inner)
                            (equal? (term-get-operator proto inner) target-op)
                            (= (length (term-get-operands proto inner)) 1))
                       (car (term-get-operands proto inner))
                       *no-match*))
                 *no-match*))))))

    ((absorption-axiom? axiom)
     (let ((outer-op (absorption-axiom-op-outer axiom))
           (inner-op (absorption-axiom-op-inner axiom)))
       ;; Absorption: op1(a, op2(a, b)) → a
       ;; The shared element a can appear in either position of the inner
       ;; term, and the inner term can be either operand of the outer term.
       ;; Four rules cover all combinations.
       (define (absorption-match? op args inner-idx outer-idx inner-pos)
         ;; inner-idx: which outer arg is the inner compound (0 or 1)
         ;; outer-idx: which outer arg is the shared element (0 or 1)
         ;; inner-pos: which inner operand matches the shared element (0 or 1)
         (and (equal? op outer-op)
              (= (length args) 2)
              (let ((inner (list-ref args inner-idx))
                    (shared (list-ref args outer-idx)))
                (and (term-compound? proto inner)
                     (equal? (term-get-operator proto inner) inner-op)
                     (= (length (term-get-operands proto inner)) 2)
                     (equal? shared
                             (list-ref (term-get-operands proto inner)
                                       inner-pos))))))
       (list
         ;; op1(a, op2(a, b)) → a  [shared in first position of inner]
         (lambda (term)
           (let ((op (term-get-operator proto term))
                 (args (term-get-operands proto term)))
             (if (absorption-match? op args 1 0 0)
                 (car args)
                 *no-match*)))
         ;; op1(a, op2(b, a)) → a  [shared in second position of inner]
         (lambda (term)
           (let ((op (term-get-operator proto term))
                 (args (term-get-operands proto term)))
             (if (absorption-match? op args 1 0 1)
                 (car args)
                 *no-match*)))
         ;; op1(op2(a, b), a) → a  [inner first, shared in first position]
         (lambda (term)
           (let ((op (term-get-operator proto term))
                 (args (term-get-operands proto term)))
             (if (absorption-match? op args 0 1 0)
                 (cadr args)
                 *no-match*)))
         ;; op1(op2(b, a), a) → a  [inner first, shared in second position]
         (lambda (term)
           (let ((op (term-get-operator proto term))
                 (args (term-get-operands proto term)))
             (if (absorption-match? op args 0 1 1)
                 (cadr args)
                 *no-match*))))))

    ((associativity-axiom? axiom)
     (let ((target-op (associativity-axiom-op axiom)))
       (list
         ;; op(op(a, b), c) → op(a, op(b, c))  (right-associate)
         (lambda (term)
           (let ((op (term-get-operator proto term))
                 (args (term-get-operands proto term)))
             (if (and (equal? op target-op)
                      (= (length args) 2)
                      (term-compound? proto (car args))
                      (equal? (term-get-operator proto (car args)) target-op)
                      (= (length (term-get-operands proto (car args))) 2))
                 (let ((inner-args (term-get-operands proto (car args))))
                   (term-make-term proto term
                     (list (car inner-args)
                           (term-make-term proto (car args)
                             (list (cadr inner-args) (cadr args))))))
                 *no-match*))))))

    (else
      (error "axiom->rules: unrecognized axiom type" axiom))))

;; ─── Normalizer ─────────────────────────────

(define (make-normalizer theory proto)
  "Compile a list of axioms (THEORY) into a single normalizer function.\nReturns a procedure (term -> value-or-#f) that tries each compiled\nrule in order. The first matching rule's result is returned; #f is\nreturned if no rule applies. The internal *no-match* sentinel is\ntranslated to #f at the API boundary.\n\nNote: because #f means \"no match,\" a rule that rewrites a term to\nliteral #f is indistinguishable from no-match. If your domain includes\n#f as a valid term, use make-recursive-normalizer from (wile algebra\nsymbolic) instead — it returns (values result trace) where (null? trace)\nunambiguously indicates no rewriting occurred.\n\nExamples:\n  ;; With a list-based protocol, identity axiom for +/0:\n  ;; (let ((norm (make-normalizer\n  ;;              (list (make-identity-axiom '+ zero?))\n  ;;              proto)))\n  ;;   (norm '(+ 0 5)))  => 5\n  ;;   (norm '(* 2 3))   => #f   ; no matching rule\n\nParameters:\n  theory : list\n  proto : any\nReturns: procedure\nCategory: algebra\nKeywords: normalizer, simplify, rewrite, reduce, canonical form, simplification\n\nSee also: `axiom->rules', `make-term-protocol', `make-recursive-normalizer'."
  (let ((rules (apply append
                 (map (lambda (ax) (axiom->rules ax proto)) theory))))
    (lambda (term)
      (let try ((rs rules))
        (if (null? rs) #f
          (let ((result ((car rs) term)))
            (if (no-match? result)
                (try (cdr rs))
                result)))))))
