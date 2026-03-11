;;; meta-eval.scm - A meta-circular Scheme evaluator
;;;
;;; Demonstrates: closures, symbols-as-data, homoiconicity, recursive descent,
;;;               first-class procedures, quasiquotation
;;;
;;; Usage: ./dist/wile --file examples/basics/meta-eval.scm

;; -----------------------------------------------------------------------
;; Environments
;;
;; An environment is a list of frames. Each frame is a vector containing
;; an association list of bindings. Bindings use a vector cell for the
;; value so that set! can mutate without set-car!/set-cdr!.
;; -----------------------------------------------------------------------

(define (make-binding var val) (cons var (vector val)))
(define (binding-val b) (vector-ref (cdr b) 0))
(define (binding-set! b val) (vector-set! (cdr b) 0 val))

(define (make-frame vars vals)
  (vector (map make-binding vars vals)))

(define (frame-bindings f) (vector-ref f 0))

(define (frame-add! f var val)
  (vector-set! f 0 (cons (make-binding var val) (frame-bindings f))))

(define (extend-env vars vals env)
  (cons (make-frame vars vals) env))

(define (lookup var env)
  (if (null? env)
      (error "Unbound variable" var)
      (let ((b (assq var (frame-bindings (car env)))))
        (if b (binding-val b) (lookup var (cdr env))))))

(define (set-var! var val env)
  (if (null? env)
      (error "Unbound variable for set!" var)
      (let ((b (assq var (frame-bindings (car env)))))
        (if b (binding-set! b val) (set-var! var val (cdr env))))))

(define (define-var! var val env)
  (let ((b (assq var (frame-bindings (car env)))))
    (if b (binding-set! b val) (frame-add! (car env) var val))))

;; -----------------------------------------------------------------------
;; Closures — tagged lists: (closure params body env)
;; -----------------------------------------------------------------------

(define (make-closure params body env) (list 'closure params body env))
(define (closure? x) (and (pair? x) (eq? (car x) 'closure)))
(define (closure-params c) (cadr c))
(define (closure-body c)   (car (cddr c)))
(define (closure-env c)    (cadr (cddr c)))

;; Primitives — tagged lists: (primitive proc)
(define (make-prim proc)  (list 'primitive proc))
(define (prim? x) (and (pair? x) (eq? (car x) 'primitive)))
(define (prim-proc p) (cadr p))

;; -----------------------------------------------------------------------
;; Evaluator
;; -----------------------------------------------------------------------

(define (my-eval exp env)
  (cond
    ((number? exp)  exp)
    ((string? exp)  exp)
    ((boolean? exp) exp)
    ((symbol? exp)  (lookup exp env))
    ((not (pair? exp)) (error "Unknown expression" exp))
    (else
     (case (car exp)
       ((quote)  (cadr exp))
       ((if)     (if (my-eval (cadr exp) env)
                     (my-eval (car (cddr exp)) env)
                     (if (null? (cdr (cddr exp)))
                         #f
                         (my-eval (cadr (cddr exp)) env))))
       ((lambda) (make-closure (cadr exp) (cddr exp) env))
       ((define) (if (pair? (cadr exp))
                     ;; (define (f x ...) body ...)
                     (let ((var (car (cadr exp)))
                           (params (cdr (cadr exp)))
                           (body (cddr exp)))
                       (define-var! var (make-closure params body env) env)
                       'ok)
                     ;; (define x val)
                     (begin
                       (define-var! (cadr exp) (my-eval (car (cddr exp)) env) env)
                       'ok)))
       ((set!)   (set-var! (cadr exp) (my-eval (car (cddr exp)) env) env)
                 'ok)
       ((begin)  (eval-seq (cdr exp) env))
       ((cond)   (eval-cond (cdr exp) env))
       ((let)    (let* ((bindings (cadr exp))
                        (vars (map car bindings))
                        (vals (map (lambda (b) (my-eval (cadr b) env)) bindings)))
                   (eval-seq (cddr exp) (extend-env vars vals env))))
       ((and)    (eval-and (cdr exp) env))
       ((or)     (eval-or (cdr exp) env))
       (else     (my-apply (my-eval (car exp) env)
                           (map (lambda (e) (my-eval e env)) (cdr exp))))))))

(define (eval-seq exps env)
  (if (null? (cdr exps))
      (my-eval (car exps) env)
      (begin (my-eval (car exps) env)
             (eval-seq (cdr exps) env))))

(define (eval-cond clauses env)
  (if (null? clauses) #f
      (let ((clause (car clauses)))
        (if (eq? (car clause) 'else)
            (eval-seq (cdr clause) env)
            (let ((test (my-eval (car clause) env)))
              (if test
                  (if (null? (cdr clause)) test (eval-seq (cdr clause) env))
                  (eval-cond (cdr clauses) env)))))))

(define (eval-and exps env)
  (if (null? exps) #t
      (let ((val (my-eval (car exps) env)))
        (if (null? (cdr exps)) val
            (if val (eval-and (cdr exps) env) #f)))))

(define (eval-or exps env)
  (if (null? exps) #f
      (let ((val (my-eval (car exps) env)))
        (if val val (eval-or (cdr exps) env)))))

(define (my-apply proc args)
  (cond
    ((prim? proc)    (apply (prim-proc proc) args))
    ((closure? proc) (eval-seq (closure-body proc)
                               (extend-env (closure-params proc)
                                           args
                                           (closure-env proc))))
    (else (error "Not a procedure" proc))))

;; -----------------------------------------------------------------------
;; Base environment — host primitives available to interpreted code
;; -----------------------------------------------------------------------

(define base-env
  (extend-env
   '(+ - * / = < > <= >= zero? null? pair? number? not
     cons car cdr list length append reverse map display newline)
   (map make-prim
        (list + - * / = < > <= >= zero? null? pair? number? not
              cons car cdr list length append reverse map display newline))
   '()))

;; -----------------------------------------------------------------------
;; Demo
;; -----------------------------------------------------------------------

(define (run label expr)
  (display "  ")
  (display expr)
  (display "\n  => ")
  (display (my-eval expr base-env))
  (display "\n\n"))

(display "=== Meta-Circular Evaluator ===\n\n")

(display "--- Arithmetic ---\n")
(run "add" '(+ 1 2))
(run "nested" '(* 3 (+ 4 5)))

(display "--- Lambda & closures ---\n")
(run "square" '((lambda (x) (* x x)) 7))
(run "closure"
  '(let ((make-adder (lambda (n) (lambda (x) (+ n x)))))
     ((make-adder 10) 32)))

(display "--- Recursion (Y combinator) ---\n")
(run "factorial"
  '(let ((Y (lambda (f)
              ((lambda (x) (f (lambda (v) ((x x) v))))
               (lambda (x) (f (lambda (v) ((x x) v))))))))
     (let ((fact (Y (lambda (self)
                      (lambda (n)
                        (if (zero? n) 1 (* n (self (- n 1)))))))))
       (fact 10))))

(display "--- Higher-order functions ---\n")
(my-eval '(define (my-map f lst)
            (if (null? lst)
                (quote ())
                (cons (f (car lst)) (my-map f (cdr lst)))))
         base-env)
(run "map" '(my-map (lambda (x) (* x x)) (list 1 2 3 4 5)))

(display "--- Mutation ---\n")
(run "set!"
  '(let ((counter 0))
     (let ((inc (lambda () (set! counter (+ counter 1)) counter)))
       (inc) (inc) (inc))))

(display "--- Conditionals ---\n")
(run "cond"
  '(cond ((> 3 5) (quote nope))
         ((< 3 5) (quote yes))
         (else (quote default))))

(display "Scheme interpreting Scheme — in ~130 lines.\n")
