;; microKanren — Hemann & Friedman (2013)
;; Adapted for R7RS (assp → assoc with custom comparator)

;; Logic variables: represented as single-element vectors
(define (var c) (vector c))
(define (var? x) (vector? x))
(define (var=? x1 x2) (= (vector-ref x1 0) (vector-ref x2 0)))

;; Substitution: association list of (var . value) pairs
(define (walk u s)
  (let ((pr (and (var? u) (assoc u s (lambda (a b) (var=? a b))))))
    (if pr (walk (cdr pr) s) u)))

(define (ext-s x v s) (cons (cons x v) s))

;; Unification
(define (unify u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
      ((and (var? u) (var? v) (var=? u v)) s)
      ((var? u) (ext-s u v s))
      ((var? v) (ext-s v u s))
      ((and (pair? u) (pair? v))
       (let ((s (unify (car u) (car v) s)))
         (and s (unify (cdr u) (cdr v) s))))
      (else (and (eqv? u v) s)))))

;; Goals: state/counter → stream
(define (== u v)
  (lambda (s/c)
    (let ((s (unify u v (car s/c))))
      (if s (unit (cons s (cdr s/c))) mzero))))

(define (unit s/c) (cons s/c '()))
(define mzero '())

(define (call/fresh f)
  (lambda (s/c)
    (let ((c (cdr s/c)))
      ((f (var c)) (cons (car s/c) (+ c 1))))))

;; Goal combinators
(define (disj g1 g2) (lambda (s/c) (mplus (g1 s/c) (g2 s/c))))
(define (conj g1 g2) (lambda (s/c) (bind (g1 s/c) g2)))

;; Stream operations (interleaving search)
(define (mplus $1 $2)
  (cond
    ((null? $1) $2)
    ((procedure? $1) (lambda () (mplus $2 ($1))))
    (else (cons (car $1) (mplus (cdr $1) $2)))))

(define (bind $ g)
  (cond
    ((null? $) mzero)
    ((procedure? $) (lambda () (bind ($) g)))
    (else (mplus (g (car $)) (bind (cdr $) g)))))

;; Initial state: empty substitution, counter at 0
(define empty-state (cons '() 0))
