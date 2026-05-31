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

;; Tseitin transform and sat? land in Task 19.
;; boolean-decide-* land in Task 20.

(define (sat? formula . opts)
  "Placeholder — full implementation in Task 19."
  (error "sat?: not yet implemented (Task 19)"))

(define (sat-model)
  "Placeholder — full implementation in Task 19."
  (error "sat-model: not yet implemented (Task 19)"))

(define (boolean-decide-sat? formula)
  "Placeholder — full implementation in Task 20."
  (error "boolean-decide-sat?: not yet implemented (Task 20)"))

(define (boolean-decide-equivalent? a b)
  "Placeholder — full implementation in Task 20."
  (error "boolean-decide-equivalent?: not yet implemented (Task 20)"))
