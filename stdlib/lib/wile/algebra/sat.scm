;;; (wile algebra sat) — propositional SAT decision.

(define (cnf->flat clauses)
  "Convert a list of CNF clauses to a flat vector with 0 terminators.
   Each clause is a list of nonzero exact integer literals. The result is
   suitable for the sat-cnf-flat? primitive.

   Examples:
     (cnf->flat '((1 -2 3) (-1 4)))
       => #(1 -2 3 0 -1 4 0)

   Parameters:
     clauses : list of lists of exact integers
   Returns: vector"
  (let* ((total (let loop ((cs clauses) (acc 0))
                  (if (null? cs)
                      acc
                      (loop (cdr cs) (+ acc (length (car cs)) 1)))))
         (v (make-vector total 0)))
    (let outer ((cs clauses) (i 0))
      (cond
        ((null? cs) v)
        (else
         (let inner ((lits (car cs)) (j i))
           (cond
             ((null? lits)
              (vector-set! v j 0)
              (outer (cdr cs) (+ j 1)))
             (else
              (vector-set! v j (car lits))
              (inner (cdr lits) (+ j 1))))))))))

(define (sat-cnf? clauses . opts)
  "Decide CNF satisfiability. Returns #t / #f / 'unknown.
   Optional second argument is the conflict budget (default 1000000;
   pass #f for unlimited).

   Parameters:
     clauses : list of clauses (each a list of nonzero exact integers)
     budget : exact integer or #f (optional)
   Returns: boolean or symbol"
  (let ((budget (if (null? opts) 1000000 (car opts))))
    (sat-cnf-flat? (cnf->flat clauses) budget)))

(define (sat-cnf-model)
  "Return the most recent CNF model as a vector indexed 1..N, or #f."
  (sat-cnf-flat-model))

;; ─── Tseitin transform ─────────────────────
;;
;; Converts a boolean S-expression formula into an equisatisfiable CNF.
;; Vocabulary: #t, #f, <symbol> (variables), (and ..), (or ..), (not e),
;; (xor a b), (iff a b), (=> a b). n-ary and/or are handled directly.

(define (tseitin formula)
  "Convert a boolean S-expression formula to CNF.
   Returns three values:
     - var-alist : ((symbol . var-index) ...) for variables in the formula
     - top-var   : the var-index whose truth ≡ the formula's truth
     - clauses   : list of clauses defining the equisatisfiable CNF"
  (let ((next-var 0)
        (var-alist '())
        (clauses '()))
    (define (intern-symbol sym)
      (let ((cell (assq sym var-alist)))
        (cond
          (cell (cdr cell))
          (else
           (set! next-var (+ next-var 1))
           (set! var-alist (cons (cons sym next-var) var-alist))
           next-var))))
    (define (fresh-var)
      (set! next-var (+ next-var 1))
      next-var)
    (define (emit clause)
      (set! clauses (cons clause clauses)))
    (define (lit-of e)
      (cond
        ((eq? e #t)
         (let ((v (fresh-var))) (emit (list v)) v))
        ((eq? e #f)
         (let ((v (fresh-var))) (emit (list (- v))) v))
        ((symbol? e) (intern-symbol e))
        ((not (pair? e))
         (error "tseitin: unrecognized formula" e))
        (else
         (case (car e)
           ((not)  (- (lit-of (cadr e))))
           ((and)  (and-cl (cdr e)))
           ((or)   (or-cl (cdr e)))
           ((xor)  (xor-cl (cadr e) (caddr e)))
           ((iff)  (iff-cl (cadr e) (caddr e)))
           ((=>)   (lit-of `(or (not ,(cadr e)) ,(caddr e))))
           (else (error "tseitin: unknown operator" (car e)))))))
    ;; v ↔ (and a b c ..): (¬v ∨ a), (¬v ∨ b), .., (v ∨ ¬a ∨ ¬b ∨ ..)
    (define (and-cl subs)
      (let* ((ls (map lit-of subs)) (v (fresh-var)))
        (for-each (lambda (s) (emit (list (- v) s))) ls)
        (emit (cons v (map - ls)))
        v))
    ;; v ↔ (or a b c ..): (v ∨ ¬a), .., (¬v ∨ a ∨ b ∨ ..)
    (define (or-cl subs)
      (let* ((ls (map lit-of subs)) (v (fresh-var)))
        (for-each (lambda (s) (emit (list v (- s)))) ls)
        (emit (cons (- v) ls))
        v))
    ;; v ↔ (xor a b): four clauses
    (define (xor-cl a b)
      (let* ((la (lit-of a)) (lb (lit-of b)) (v (fresh-var)))
        (emit (list (- v) la lb))
        (emit (list (- v) (- la) (- lb)))
        (emit (list v la (- lb)))
        (emit (list v (- la) lb))
        v))
    ;; v ↔ (iff a b): four clauses
    (define (iff-cl a b)
      (let* ((la (lit-of a)) (lb (lit-of b)) (v (fresh-var)))
        (emit (list (- v) (- la) lb))
        (emit (list (- v) la (- lb)))
        (emit (list v la lb))
        (emit (list v (- la) (- lb)))
        v))
    (let ((top (lit-of formula)))
      (emit (list top))
      (values var-alist top (reverse clauses)))))

;; ─── Public sat? ───────────────────────────

(define *sat-var-alist* '())

(define (sat? formula . opts)
  "Decide satisfiability of a boolean S-expression formula. Operators:
   and, or, not, xor, iff, =>. Atoms are variables.

   Returns #t / #f / 'unknown.

   Parameters:
     formula : boolean S-expression
     budget : exact integer or #f (optional, default 1000000)
   Returns: boolean or symbol"
  (let ((budget (if (null? opts) 1000000 (car opts))))
    (call-with-values
      (lambda () (tseitin formula))
      (lambda (var-alist top-var clauses)
        (set! *sat-var-alist* var-alist)
        (sat-cnf-flat? (cnf->flat clauses) budget)))))

(define (sat-model)
  "Return an alist ((sym . #t/#f) ..) for variables in the most recent
   sat? call, or #f if no current model."
  (let ((vec (sat-cnf-flat-model)))
    (cond
      ((not vec) #f)
      (else
       (map (lambda (cell)
              (cons (car cell) (vector-ref vec (cdr cell))))
            *sat-var-alist*)))))

;; ─── Boolean-algebra decision predicates ───

(define (boolean-decide-sat? formula)
  "SAT-backed satisfiability check. Equivalent to (sat? formula) with the
   default conflict budget. Returns #t / #f / 'unknown.

   See also: sat?, boolean-decide-equivalent?"
  (sat? formula))

(define (boolean-decide-equivalent? a b)
  "SAT-backed equivalence check for two boolean S-expression formulas.
   Two formulas are equivalent iff ¬(a ↔ b) is unsatisfiable.

   Returns #t / #f / 'unknown.

   This closes the De Morgan / complement-law / bound-identity / distributivity
   gap left by symbolic-boolean-equivalent? in (wile algebra symbolic).

   Examples:
     (boolean-decide-equivalent? '(not (and x y))
                                  '(or (not x) (not y))) => #t
     (boolean-decide-equivalent? '(or x y) '(and x y))   => #f"
  (let ((result (sat? `(not (iff ,a ,b)))))
    (cond
      ((eq? result #f) #t)        ; UNSAT proven → equivalent
      ((eq? result #t) #f)        ; SAT (witness) → not equivalent
      (else 'unknown))))
