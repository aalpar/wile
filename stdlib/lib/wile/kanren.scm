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
  "Force a lazy answer stream $ to a concrete value.\nRepeatedly invokes $ while it is a suspension (procedure)\nuntil a pair or the empty list is obtained."
  (if (procedure? $) (pull ($)) $))

(define (take-inf n $)
  "Take at most N answers from the lazy stream $.\nForces suspensions via pull and collects results into a list.\nReturns fewer than N answers if the stream is exhausted."
  (cond
    ((zero? n) '())
    (else
     (let (($ (pull $)))
       (cond
         ((null? $) '())
         (else (cons (car $) (take-inf (- n 1) (cdr $)))))))))

(define (take-all-inf $)
  "Force the entire lazy stream $ and collect all answers into a list.\nWarning: diverges if the stream is infinite."
  (let (($ (pull $)))
    (cond
      ((null? $) '())
      (else (cons (car $) (take-all-inf (cdr $)))))))

;; Reification
(define (reify-name n)
  "Generate a reification symbol for the Nth unnamed logic variable.\nProduces symbols of the form _.0, _.1, _.2, etc."
  (string->symbol
    (string-append "_." (number->string n))))

(define (walk* v s)
  "Deeply walk value V under substitution S.\nLike walk, but recursively resolves variables inside pairs,\nproducing a fully ground term when possible."
  (let ((v (walk v s)))
    (cond
      ((var? v) v)
      ((pair? v)
       (cons (walk* (car v) s)
             (walk* (cdr v) s)))
      (else v))))

(define (reify-s v s)
  "Build a reification substitution for value V.\nWalks V under S and maps each remaining free variable to a\nhuman-readable symbol (_.0, _.1, ...) using reify-name.\nRecurses into pairs to discover all unbound variables."
  (let ((v (walk v s)))
    (cond
      ((var? v) (ext-s v (reify-name (length s)) s))
      ((pair? v) (reify-s (cdr v) (reify-s (car v) s)))
      (else s))))

(define (reify v)
  "Reify value V by replacing all free logic variables with\nhuman-readable symbols (_.0, _.1, ...). Composes walk* and\nreify-s to produce a fully readable term."
  (walk* v (reify-s v '())))

(define (reify-1st s/c)
  "Reify the first query variable (var 0) from state/counter S/C.\nExtracts the substitution, deeply walks variable 0, then\nreplaces remaining free variables with readable names.\nUsed by run and run* to present answers."
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
