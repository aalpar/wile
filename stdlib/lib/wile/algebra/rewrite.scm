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
;;; "no match" from a legitimate #f rewrite result. This sentinel is not
;;; exported; callers never see it.

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
  "Construct a term protocol for abstract term representations.\nCOMPOUND-TERM? tests whether a value is a compound term.\nGET-OPERATOR and GET-OPERANDS extract parts of a compound term.\nMAKE-TERM rebuilds a term with new operands while preserving\nmetadata. COMPARE returns #t when its first argument should sort\nbefore the second, used by commutativity rules to normalize\noperand order.\n\nExamples:\n  ;; A list-based term protocol: (op arg ...)\n  (make-term-protocol\n    pair?\n    car\n    cdr\n    (lambda (term new-args) (cons (car term) new-args))\n    (lambda (a b) (< a b)))\n\nParameters:\n  compound-term? : procedure\n  get-operator : procedure\n  get-operands : procedure\n  make-term : procedure\n  compare : procedure\nReturns: any\nCategory: algebra\n\nSee also: `term-compound?', `term-get-operator', `term-get-operands', `term-make-term'."
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

(define (axiom? x)
  "Test whether X is a recognized axiom type.\nReturns #t for identity, commutativity, absorbing, idempotence,\nor involution axiom records.\n\nExamples:\n  (axiom? (make-identity-axiom '+ zero?))     => #t\n  (axiom? (make-commutativity-axiom '+))       => #t\n  (axiom? 42)                                  => #f\n\nParameters:\n  x : any\nReturns: boolean\nCategory: algebra\n\nSee also: `make-identity-axiom', `make-commutativity-axiom', `make-absorbing-axiom'."
  (or (identity-axiom? x)
      (commutativity-axiom? x)
      (absorbing-axiom? x)
      (idempotence-axiom? x)
      (involution-axiom? x)))

;; ─── Axiom → rewrite rules ─────────────────

(define (axiom->rules axiom proto)
  "Compile AXIOM into a list of rewrite-rule procedures using term protocol PROTO.\nEach rule is a procedure (term -> value-or-*no-match*) that attempts\none rewriting step. Identity axioms produce two rules (left and right),\ncommutativity produces one rule that normalizes by term ordering,\nand involution produces one rule that collapses f(f(x)) to x.\n\nExamples:\n  (axiom->rules (make-identity-axiom '+ zero?) proto)\n    => list of two rule procedures (left and right identity)\n  (axiom->rules (make-involution-axiom 'neg) proto)\n    => list of one rule: neg(neg(x)) => x\n\nParameters:\n  axiom : any\n  proto : any\nReturns: list\nCategory: algebra\n\nSee also: `make-normalizer', `axiom?', `make-term-protocol'."
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

    (else
      (error "axiom->rules: unrecognized axiom type" axiom))))

;; ─── Normalizer ─────────────────────────────

(define (make-normalizer theory proto)
  "Compile a list of axioms (THEORY) into a single normalizer function.\nReturns a procedure (term -> value-or-#f) that tries each compiled\nrule in order. The first matching rule's result is returned; #f is\nreturned if no rule applies. The internal *no-match* sentinel is\ntranslated to #f so callers never see it.\n\nExamples:\n  ;; With a list-based protocol, identity axiom for +/0:\n  ;; (let ((norm (make-normalizer\n  ;;              (list (make-identity-axiom '+ zero?))\n  ;;              proto)))\n  ;;   (norm '(+ 0 5)))  => 5\n  ;;   (norm '(* 2 3))   => #f   ; no matching rule\n\nParameters:\n  theory : list\n  proto : any\nReturns: procedure\nCategory: algebra\n\nSee also: `axiom->rules', `make-term-protocol'."
  (let ((rules (apply append
                 (map (lambda (ax) (axiom->rules ax proto)) theory))))
    (lambda (term)
      (let try ((rs rules))
        (if (null? rs) #f
          (let ((result ((car rs) term)))
            (if (no-match? result)
                (try (cdr rs))
                result)))))))
