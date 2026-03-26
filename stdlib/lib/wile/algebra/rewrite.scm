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

;; ─── Term protocol ──────────────────────────

(define-record-type <term-protocol>
  (make-term-protocol get-operator get-operands make-term compare)
  term-protocol?
  (get-operator  term-get-operator)   ;; term → symbol
  (get-operands  term-get-operands)   ;; term → (list operand ...)
  (make-term     term-make-term)      ;; (term, new-operands) → term
  (compare       term-compare))       ;; (a, b) → boolean  (less-than)

;; ─── Axiom types ────────────────────────────

(define-record-type <identity-axiom>
  (identity-axiom op element)
  identity-axiom?
  (op      identity-axiom-op)
  (element identity-axiom-element))   ;; predicate: value → boolean

(define-record-type <commutativity-axiom>
  (commutativity-axiom op)
  commutativity-axiom?
  (op commutativity-axiom-op))

(define-record-type <absorbing-axiom>
  (absorbing-axiom op element)
  absorbing-axiom?
  (op      absorbing-axiom-op)
  (element absorbing-axiom-element))  ;; predicate: value → boolean

(define-record-type <idempotence-axiom>
  (idempotence-axiom op)
  idempotence-axiom?
  (op idempotence-axiom-op))

(define-record-type <involution-axiom>
  (involution-axiom op)
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
  ;; Returns a list of rewrite rule functions: (term → value-or-#f)
  (let ((get-op   (term-get-operator proto))
        (get-args (term-get-operands proto))
        (mk-term  (term-make-term proto))
        (lt?      (term-compare proto)))
    (cond
      ((identity-axiom? axiom)
       (let ((target-op (identity-axiom-op axiom))
             (e?        (identity-axiom-element axiom)))
         (list
           ;; op(x, e) → x
           (lambda (term)
             (let ((op (get-op term)) (args (get-args term)))
               (and (equal? op target-op)
                    (= (length args) 2)
                    (e? (cadr args))
                    (car args))))
           ;; op(e, x) → x
           (lambda (term)
             (let ((op (get-op term)) (args (get-args term)))
               (and (equal? op target-op)
                    (= (length args) 2)
                    (e? (car args))
                    (cadr args)))))))

      ((commutativity-axiom? axiom)
       (let ((target-op (commutativity-axiom-op axiom)))
         (list
           ;; op(x, y) → op(y, x) when y < x
           (lambda (term)
             (let ((op (get-op term)) (args (get-args term)))
               (and (equal? op target-op)
                    (= (length args) 2)
                    (lt? (cadr args) (car args))
                    (mk-term term (list (cadr args) (car args)))))))))

      ((absorbing-axiom? axiom)
       (let ((target-op (absorbing-axiom-op axiom))
             (z?        (absorbing-axiom-element axiom)))
         (list
           ;; op(x, z) → z
           (lambda (term)
             (let ((op (get-op term)) (args (get-args term)))
               (and (equal? op target-op)
                    (= (length args) 2)
                    (z? (cadr args))
                    (cadr args))))
           ;; op(z, x) → z
           (lambda (term)
             (let ((op (get-op term)) (args (get-args term)))
               (and (equal? op target-op)
                    (= (length args) 2)
                    (z? (car args))
                    (car args)))))))

      ((idempotence-axiom? axiom)
       (let ((target-op (idempotence-axiom-op axiom)))
         (list
           ;; op(x, x) → x
           (lambda (term)
             (let ((op (get-op term)) (args (get-args term)))
               (and (equal? op target-op)
                    (= (length args) 2)
                    (equal? (car args) (cadr args))
                    (car args)))))))

      ((involution-axiom? axiom)
       (let ((target-op (involution-axiom-op axiom)))
         (list
           ;; op(op(x)) → x  (unary: single operand)
           (lambda (term)
             (let ((op (get-op term)) (args (get-args term)))
               (and (equal? op target-op)
                    (= (length args) 1)
                    (let ((inner (car args)))
                      (and (pair? inner)
                           (equal? (get-op inner) target-op)
                           (= (length (get-args inner)) 1)
                           (car (get-args inner))))))))))

      (else '()))))

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
            (if result result
              (try (cdr rs)))))))))
