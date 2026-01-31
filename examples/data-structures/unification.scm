;;; unification.scm - Unification and pattern matching
;;;
;;; Demonstrates: symbolic computation, define-record-type, association
;;;               lists as substitution maps, recursive structural matching,
;;;               the core algorithm behind Prolog and type inference
;;;
;;; Usage: ./dist/scheme --file examples/data-structures/unification.scm

;; -----------------------------------------------------------------------
;; Logic variables
;;
;; A logic variable is a symbol starting with '?'.
;; -----------------------------------------------------------------------

(define (variable? x)
  (and (symbol? x)
       (let ((s (symbol->string x)))
         (and (> (string-length s) 1)
              (char=? (string-ref s 0) #\?)))))

;; -----------------------------------------------------------------------
;; Substitutions
;;
;; A substitution is an association list mapping variables to terms.
;; We use #f to represent failure.
;; -----------------------------------------------------------------------

(define empty-subst '())

(define (walk var subst)
  (if (variable? var)
      (let ((binding (assq var subst)))
        (if binding
            (walk (cdr binding) subst)
            var))
      var))

;; -----------------------------------------------------------------------
;; Occurs check — prevents infinite types like ?x = (f ?x)
;; -----------------------------------------------------------------------

(define (occurs? var term subst)
  (let ((term (walk term subst)))
    (cond
      ((equal? var term) #t)
      ((pair? term)
       (or (occurs? var (car term) subst)
           (occurs? var (cdr term) subst)))
      (else #f))))

;; -----------------------------------------------------------------------
;; Unification
;;
;; Attempts to make two terms equal by finding variable bindings.
;; Returns an extended substitution on success, #f on failure.
;; -----------------------------------------------------------------------

(define (unify t1 t2 subst)
  (if (not subst) #f    ; propagate failure
      (let ((t1 (walk t1 subst))
            (t2 (walk t2 subst)))
        (cond
          ((equal? t1 t2) subst)
          ((variable? t1)
           (if (occurs? t1 t2 subst) #f
               (cons (cons t1 t2) subst)))
          ((variable? t2)
           (if (occurs? t2 t1 subst) #f
               (cons (cons t2 t1) subst)))
          ((and (pair? t1) (pair? t2))
           (unify (cdr t1) (cdr t2)
                  (unify (car t1) (car t2) subst)))
          (else #f)))))

;; -----------------------------------------------------------------------
;; Substitution application — replace all variables with their values
;; -----------------------------------------------------------------------

(define (substitute term subst)
  (let ((term (walk term subst)))
    (cond
      ((variable? term) term)  ; unbound variable
      ((pair? term)
       (cons (substitute (car term) subst)
             (substitute (cdr term) subst)))
      (else term))))

;; -----------------------------------------------------------------------
;; Query interface — try to unify a pattern against multiple facts
;; -----------------------------------------------------------------------

(define (query-all pattern facts)
  (let loop ((facts facts) (results '()))
    (if (null? facts)
        (reverse results)
        (let ((subst (unify pattern (car facts) empty-subst)))
          (loop (cdr facts)
                (if subst
                    (cons (list (substitute pattern subst) subst) results)
                    results))))))

;; -----------------------------------------------------------------------
;; Demo helpers
;; -----------------------------------------------------------------------

(define (show-unify label t1 t2)
  (let ((result (unify t1 t2 empty-subst)))
    (display "  ")
    (display t1)
    (display "  ≡  ")
    (display t2)
    (newline)
    (if result
        (begin
          (display "    bindings: ")
          (display result)
          (display "\n    result:   ")
          (display (substitute t1 result))
          (newline))
        (display "    FAIL\n"))
    (newline)))

;; -----------------------------------------------------------------------
;; Demo
;; -----------------------------------------------------------------------

(display "=== Unification Engine ===\n\n")

(display "--- Simple variable binding ---\n")
(show-unify "bind" '?x 42)
(show-unify "pair" '(?x ?y) '(hello world))

(display "--- Structural unification ---\n")
(show-unify "nested" '(cons ?x (cons ?y ())) '(cons 1 (cons 2 ())))
(show-unify "shared" '(f ?x ?x) '(f 3 3))
(show-unify "conflict" '(f ?x ?x) '(f 3 4))

(display "--- Type inference sketch ---\n")
;; Unifying function types: (-> ?a ?b) with (-> int ?b)
(show-unify "fn-type"
  '(-> ?a ?b)
  '(-> int ?b))

;; Infer: if f : (-> int bool) and we apply f to ?x, then ?x : int
(show-unify "apply"
  '(apply (-> ?input ?output) ?input)
  '(apply (-> int bool) ?x))

;; Higher-order: (-> (-> ?a ?b) (list ?a) (list ?b))  =  map's type
(show-unify "map-type"
  '(-> (-> ?a ?b) (list ?a) (list ?b))
  '(-> (-> int string) (list int) ?result))

(display "--- Occurs check (prevents infinite types) ---\n")
(show-unify "occurs" '?x '(f ?x))

(display "--- Query: matching against a database ---\n")
(let ((database '((parent alice bob)
                  (parent alice carol)
                  (parent bob dave)
                  (parent bob eve)
                  (parent carol frank))))
  (display "  Database: who are Alice's children?\n")
  (let ((results (query-all '(parent alice ?child) database)))
    (for-each (lambda (r)
                (display "    ")
                (display (car r))
                (newline))
              results))

  (display "\n  Database: who is a parent of whom?\n")
  (let ((results (query-all '(parent ?p ?c) database)))
    (for-each (lambda (r)
                (display "    ")
                (display (car r))
                (newline))
              results)))

(display "\nThe core of Prolog — unification in ~80 lines.\n")
