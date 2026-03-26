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
  (make-term-protocol* compound-term? get-operator get-operands
                       make-term compare))

(define (term-compound? proto x)
  ((term-compound?-fn proto) x))

(define (term-get-operator proto term)
  ((term-get-operator-fn proto) term))

(define (term-get-operands proto term)
  ((term-get-operands-fn proto) term))

(define (term-make-term proto term new-args)
  ((term-make-term-fn proto) term new-args))

(define (term-compare proto a b)
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
  (or (identity-axiom? x)
      (commutativity-axiom? x)
      (absorbing-axiom? x)
      (idempotence-axiom? x)
      (involution-axiom? x)))

;; ─── Axiom → rewrite rules ─────────────────

(define (axiom->rules axiom proto)
  ;; Returns a list of rewrite rule functions: (term → value-or-*no-match*)
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
  ;; Compile all axioms into a flat list of rewrite rules.
  ;; Returns (term → value-or-#f): first match wins, #f if none.
  (let ((rules (apply append
                 (map (lambda (ax) (axiom->rules ax proto)) theory))))
    (lambda (term)
      (let try ((rs rules))
        (if (null? rs) #f
          (let ((result ((car rs) term)))
            (if (no-match? result)
                (try (cdr rs))
                result)))))))
