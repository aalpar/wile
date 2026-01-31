;;; symbolic-diff.scm - Symbolic differentiation engine
;;;
;;; Demonstrates: symbolic computation, S-expressions as algebra,
;;;               exact rational arithmetic, recursive case analysis
;;;
;;; Usage: ./dist/scheme --file examples/numeric-tower/symbolic-diff.scm

;; -----------------------------------------------------------------------
;; Differentiation rules
;;
;; Expressions are S-expressions:  (+ x (* 3 (expt x 2)))
;; Variables are symbols, constants are numbers.
;; -----------------------------------------------------------------------

(define (variable? x) (symbol? x))
(define (same-var? x y) (and (variable? x) (eq? x y)))

(define (deriv exp var)
  (cond
    ((number? exp) 0)
    ((variable? exp)
     (if (same-var? exp var) 1 0))
    ((not (pair? exp)) (error "Unknown expression" exp))
    (else
     (case (car exp)
       ((+) (make-sum (deriv (cadr exp) var)
                      (deriv (car (cddr exp)) var)))
       ((-) (make-diff (deriv (cadr exp) var)
                       (deriv (car (cddr exp)) var)))
       ((*) ;; Product rule: d(uv) = u dv + v du
        (let ((u (cadr exp)) (v (car (cddr exp))))
          (make-sum (make-product u (deriv v var))
                    (make-product v (deriv u var)))))
       ((/) ;; Quotient rule: d(u/v) = (v du - u dv) / v^2
        (let ((u (cadr exp)) (v (car (cddr exp))))
          (make-quotient
           (make-diff (make-product v (deriv u var))
                      (make-product u (deriv v var)))
           (make-product v v))))
       ((expt) ;; Power rule: d(u^n) = n * u^(n-1) * du
        (let ((u (cadr exp)) (n (car (cddr exp))))
          (make-product
           (make-product n (make-power u (make-diff n 1)))
           (deriv u var))))
       ((sin) ;; d(sin u) = cos(u) * du
        (make-product (list 'cos (cadr exp))
                      (deriv (cadr exp) var)))
       ((cos) ;; d(cos u) = -sin(u) * du
        (make-product (make-product -1 (list 'sin (cadr exp)))
                      (deriv (cadr exp) var)))
       ((exp) ;; d(e^u) = e^u * du
        (make-product exp (deriv (cadr exp) var)))
       ((log) ;; d(ln u) = du / u
        (make-quotient (deriv (cadr exp) var) (cadr exp)))
       (else (error "Unknown operator" (car exp)))))))

;; -----------------------------------------------------------------------
;; Algebraic simplification constructors
;; -----------------------------------------------------------------------

(define (make-sum a b)
  (cond ((eqv? a 0) b)
        ((eqv? b 0) a)
        ((and (number? a) (number? b)) (+ a b))
        (else (list '+ a b))))

(define (make-diff a b)
  (cond ((eqv? b 0) a)
        ((eqv? a 0) (make-product -1 b))
        ((and (number? a) (number? b)) (- a b))
        ((equal? a b) 0)
        (else (list '- a b))))

(define (make-product a b)
  (cond ((eqv? a 0) 0)
        ((eqv? b 0) 0)
        ((eqv? a 1) b)
        ((eqv? b 1) a)
        ((and (number? a) (number? b)) (* a b))
        (else (list '* a b))))

(define (make-quotient a b)
  (cond ((eqv? a 0) 0)
        ((eqv? b 1) a)
        ((and (number? a) (number? b)) (/ a b))  ; exact rational!
        (else (list '/ a b))))

(define (make-power base exp)
  (cond ((eqv? exp 0) 1)
        ((eqv? exp 1) base)
        (else (list 'expt base exp))))

;; -----------------------------------------------------------------------
;; Infix pretty-printer
;; -----------------------------------------------------------------------

(define (infix exp)
  (cond
    ((not (pair? exp)) exp)
    ((null? (cddr exp))  ; unary: (sin x)
     (list (car exp) (infix (cadr exp))))
    (else
     (let ((op (car exp))
           (left (infix (cadr exp)))
           (right (infix (car (cddr exp)))))
       (list left op right)))))

;; -----------------------------------------------------------------------
;; Demo
;; -----------------------------------------------------------------------

(define (show label expr var)
  (let ((d (deriv expr var)))
    (display "  d/d")
    (display var)
    (display " ")
    (display expr)
    (display "\n    = ")
    (display d)
    (display "\n\n")))

(display "=== Symbolic Differentiation ===\n\n")

(display "--- Polynomials ---\n")
(show "linear"    '(+ (* 3 x) 5) 'x)
(show "quadratic" '(+ (* 2 (expt x 2)) (* 3 x)) 'x)

(display "--- Exact rational coefficients ---\n")
;; d/dx (x^3 / 3) = x^2  — exact arithmetic keeps 1/3 * 3 = 1
(show "rational" '(* 1/3 (expt x 3)) 'x)

(display "--- Trigonometric ---\n")
(show "sin" '(sin (* 2 x)) 'x)
(show "cos" '(* x (cos x)) 'x)

(display "--- Quotient rule ---\n")
(show "quotient" '(/ x (+ x 1)) 'x)

(display "--- Chain rule (exp) ---\n")
(show "exp" '(exp (expt x 2)) 'x)

(display "--- Logarithm ---\n")
(show "log" '(log (expt x 2)) 'x)

(display "The data IS the syntax — symbolic math in ~100 lines.\n")
