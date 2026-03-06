;; miniKanren macro layer — syntactic sugar over microKanren
;; Provides fresh, conde, run, run* via syntax-rules

;; Zzz: inverse-eta-delay for recursive goals
(define-syntax zzz
  (syntax-rules ()
    ((zzz g) (lambda (s/c) (lambda () (g s/c))))))

;; conj+: conjunction of one or more goals
(define-syntax conj+
  (syntax-rules ()
    ((conj+ g) (zzz g))
    ((conj+ g0 g ...) (conj (zzz g0) (conj+ g ...)))))

;; disj+: disjunction of one or more goals
(define-syntax disj+
  (syntax-rules ()
    ((disj+ g) (zzz g))
    ((disj+ g0 g ...) (disj (zzz g0) (disj+ g ...)))))

;; conde: disjunctive normal form
;; (conde ((g ...) ...) ...)
(define-syntax conde
  (syntax-rules ()
    ((conde (g0 g ...) ...)
     (disj+ (conj+ g0 g ...) ...))))

;; fresh: introduce logic variables
;; (fresh (x y z) goal ...)
(define-syntax fresh
  (syntax-rules ()
    ((fresh () g0 g ...)
     (conj+ g0 g ...))
    ((fresh (x0 x ...) g0 g ...)
     (call/fresh
       (lambda (x0)
         (fresh (x ...) g0 g ...))))))

;; Pull: force a stream to a list
(define (pull $)
  (if (procedure? $) (pull ($)) $))

(define (take-inf n $)
  (cond
    ((zero? n) '())
    (else
     (let (($ (pull $)))
       (cond
         ((null? $) '())
         (else (cons (car $) (take-inf (- n 1) (cdr $)))))))))

(define (take-all-inf $)
  (let (($ (pull $)))
    (cond
      ((null? $) '())
      (else (cons (car $) (take-all-inf (cdr $)))))))

;; Reification
(define (reify-name n)
  (string->symbol
    (string-append "_." (number->string n))))

(define (walk* v s)
  (let ((v (walk v s)))
    (cond
      ((var? v) v)
      ((pair? v)
       (cons (walk* (car v) s)
             (walk* (cdr v) s)))
      (else v))))

(define (reify-s v s)
  (let ((v (walk v s)))
    (cond
      ((var? v) (ext-s v (reify-name (length s)) s))
      ((pair? v) (reify-s (cdr v) (reify-s (car v) s)))
      (else s))))

(define (reify v)
  (walk* v (reify-s v '())))

(define (reify-1st s/c)
  (let ((v (walk* (var 0) (car s/c))))
    (walk* v (reify-s v '()))))

;; run: bounded query
(define-syntax run
  (syntax-rules ()
    ((run n (x) g0 g ...)
     (let ((results (take-inf n
                      ((fresh (x) g0 g ...) empty-state))))
       (map reify-1st results)))))

;; run*: unbounded query
(define-syntax run*
  (syntax-rules ()
    ((run* (x) g0 g ...)
     (let ((results (take-all-inf
                      ((fresh (x) g0 g ...) empty-state))))
       (map reify-1st results)))))
