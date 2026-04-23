;; ================================================================
;; Chapter 05 -- Symbolic and polynomial differentiation
;;
;; What you will learn:
;;   - How `(wile algebra polynomial)` represents univariate polynomials
;;     over any ring, and how its arithmetic composes with the ring API.
;;   - How `poly-derivative` computes formal derivatives without a
;;     notion of "variable" -- just shifted and scaled coefficients.
;;   - How `(wile algebra differential)` wraps the polynomial ring into
;;     a differential ring whose derivation is the formal derivative.
;;   - How to write a symbolic S-expression differentiator by hand, using
;;     the sum, product, power, and chain rules; and how to check it
;;     against the polynomial derivative on shared test inputs.
;;
;; Prerequisites: chapters 02, 03.
;; Sub-libraries used:
;;   (wile algebra ring), (wile algebra polynomial),
;;   (wile algebra differential), (wile algebra field).
;; ================================================================

(import (scheme base) (scheme write)
        (wile algebra ring)
        (wile algebra polynomial)
        (wile algebra differential))
(include "../lib/check.scm")

;; ----------------------------------------------------------------
;; Part 1: Polynomials over a ring.
;;
;; A polynomial over ring R is a list of coefficients in ascending
;; order of power. `(make-poly R '(3 2 1))` represents 3 + 2x + x^2.
;; The library strips trailing zeros so a polynomial's representation
;; is canonical.
;; ----------------------------------------------------------------

(define Z (integer-ring))

(define p (make-poly Z '(3 2 1)))            ; 3 + 2x + x^2
(define q (make-poly Z '(1 1)))              ; 1 + x

(check-true (polynomial? p)                          "p is a polynomial")
(check= (poly-coeffs p)          '(3 2 1)            "coefficients preserved")
(check= (poly-degree p)          2                   "deg(3+2x+x^2) = 2")
(check= (poly-leading-coeff p)   1                   "leading coefficient")

;; Zero polynomial has degree -1 by PARI/GP convention.
(check= (poly-degree (poly-zero Z))  -1              "deg(0) = -1 (zero polynomial)")
(check= (poly-coeffs (poly-one Z))   '(1)            "one polynomial")
(check= (poly-coeffs (make-poly Z '(5 0 0)))  '(5)   "trailing zeros stripped")

;; ----------------------------------------------------------------
;; Part 2: Polynomial arithmetic.
;;
;; Addition, negation, multiplication all go through the ring API of
;; the coefficients. Change the ring, change what the arithmetic
;; means -- same polynomial code works over Q, Z/nZ, any ring.
;; ----------------------------------------------------------------

(check= (poly-coeffs (poly-plus p q))
        '(4 3 1)                      ; (3+2x+x^2) + (1+x) = 4 + 3x + x^2
        "poly addition")

(check= (poly-coeffs (poly-negate p))
        '(-3 -2 -1)
        "poly negation")

(check= (poly-coeffs (poly-times q q))
        '(1 2 1)                      ; (1+x)^2 = 1 + 2x + x^2
        "(1+x)^2 = 1 + 2x + x^2")

(check= (poly-coeffs (poly-times p (poly-zero Z)))
        '()
        "p * 0 = 0")

(check= (poly-coeffs (poly-minus p q))
        '(2 1 1)                      ; 3+2x+x^2 - (1+x) = 2+x+x^2
        "poly subtraction")

;; ----------------------------------------------------------------
;; Part 3: Evaluation via Horner's rule.
;;
;; `poly-eval` threads through the coefficient ring's plus and times,
;; so evaluation at an integer x uses ring-plus and ring-times from Z.
;; ----------------------------------------------------------------

(check= (poly-eval p 0)  3         "p(0) = 3")
(check= (poly-eval p 1)  6         "p(1) = 3+2+1 = 6")
(check= (poly-eval p 2)  11        "p(2) = 3+4+4 = 11")
(check= (poly-eval q 10) 11        "q(10) = 1+10 = 11")

;; ----------------------------------------------------------------
;; Part 4: Formal differentiation on polynomials.
;;
;; Formal derivative does not "compute" a limit -- it applies the
;; coefficient shift rule directly:
;;   d/dx (a_0 + a_1 x + a_2 x^2 + ... + a_n x^n)
;;   =      a_1 + 2 a_2 x + 3 a_3 x^2 + ... + n a_n x^(n-1)
;; ----------------------------------------------------------------

(check= (poly-coeffs (poly-derivative p))
        '(2 2)                        ; d/dx (3 + 2x + x^2) = 2 + 2x
        "d/dx (3 + 2x + x^2) = 2 + 2x")

(check= (poly-coeffs (poly-derivative (make-poly Z '(1 0 0 1))))
        '(0 0 3)                      ; d/dx (1 + x^3) = 3x^2
        "d/dx (1 + x^3) = 3x^2")

(check= (poly-coeffs (poly-derivative (poly-one Z)))
        '()
        "d/dx 1 = 0")

(check= (poly-coeffs (poly-derivative (poly-zero Z)))
        '()
        "d/dx 0 = 0")

;; ----------------------------------------------------------------
;; Part 5: Polynomial GCD and division (over a field).
;;
;; `poly-divmod` and `poly-gcd` require a field for the coefficient
;; ring -- integer divisions in the algorithm require reciprocals.
;; We use `rational-polynomials` to avoid building Q by hand.
;; ----------------------------------------------------------------

(define Qpolys (rational-polynomials))
(check-true (ring? Qpolys)  "Q[x] is a ring")

;; `poly-gcd` needs a field as its third argument. Rational field
;; provides the reciprocals the Euclidean algorithm relies on.
(define g (poly-gcd (make-poly (field->ring (rational-field)) '(-6 -5 -1))  ; -6 - 5x - x^2
                    (make-poly (field->ring (rational-field)) '(-2 -1))     ; -2 - x
                    (rational-field)))
;; (-2 - x)(3 + x) = -6 - 5x - x^2, so gcd should be monic 2 + x (or scaled).
;; The library normalizes to a monic polynomial over Q.
(check-true (polynomial? g)                       "gcd is a polynomial")
(check-true (>= (poly-degree g) 0)                "gcd degree >= 0")

;; ----------------------------------------------------------------
;; Part 6: Polynomial differential ring.
;;
;; `polynomial-derivation R` wraps the polynomial ring over R into a
;; differential ring whose derivation is `poly-derivative`. This is
;; the classical algebraic setting for differential equations over a
;; commutative ring.
;; ----------------------------------------------------------------

(define Dpoly (polynomial-derivation Z))
(check-true (differential-ring? Dpoly)           "polynomial-derivation builds a differential ring")

;; Differentiation via the differential ring API -- identical result
;; to calling poly-derivative directly.
(check= (poly-coeffs (differential-deriv Dpoly p))
        '(2 2)
        "differential-deriv agrees with poly-derivative")

;; Higher-order derivatives.
(define p5 (make-poly Z '(0 0 0 0 0 1)))         ; x^5
(check= (poly-coeffs (differential-nth-deriv Dpoly 0 p5))  '(0 0 0 0 0 1)
        "0th deriv is identity")
(check= (poly-coeffs (differential-nth-deriv Dpoly 1 p5))  '(0 0 0 0 5)
        "d/dx x^5 = 5 x^4")
(check= (poly-coeffs (differential-nth-deriv Dpoly 2 p5))  '(0 0 0 20)
        "d^2/dx^2 x^5 = 20 x^3")
(check= (poly-coeffs (differential-nth-deriv Dpoly 5 p5))  '(120)
        "d^5/dx^5 x^5 = 120 (constant)")
(check= (poly-coeffs (differential-nth-deriv Dpoly 6 p5))  '()
        "d^6/dx^6 x^5 = 0")

;; A constant polynomial is recognized as a differential constant.
(check-true  (differential-constant? Dpoly (poly-one Z))      "1 is a differential constant")
(check-false (differential-constant? Dpoly p)                 "3+2x+x^2 is not a constant")

;; ----------------------------------------------------------------
;; Part 7: A hand-written S-expression differentiator.
;;
;; The algebra library's polynomial-derivation handles polynomials in
;; their coefficient-list form. For symbolic terms like (* x (+ y 1))
;; you need a recursive walker that applies the sum, product, constant,
;; and chain rules. This section writes one in ~20 lines.
;;
;; The standard rules for d/dx:
;;   d/dx c                = 0            (constant rule)
;;   d/dx x                = 1            (variable)
;;   d/dx (+ u v)          = du + dv      (sum rule)
;;   d/dx (- u v)          = du - dv
;;   d/dx (* u v)          = u*dv + v*du  (product rule)
;;   d/dx (^ u n)          = n * u^(n-1) * du (power rule with chain)
;; ----------------------------------------------------------------

(define (diff expr var)
  (cond
    ((number? expr) 0)
    ((symbol? expr) (if (eq? expr var) 1 0))
    ((pair? expr)
     (let ((op (car expr)) (args (cdr expr)))
       (cond
         ((and (eq? op '+) (= (length args) 2))
          (list '+ (diff (car args) var) (diff (cadr args) var)))
         ((and (eq? op '-) (= (length args) 2))
          (list '- (diff (car args) var) (diff (cadr args) var)))
         ((and (eq? op '*) (= (length args) 2))
          (let ((u (car args)) (v (cadr args)))
            (list '+
                  (list '* u (diff v var))
                  (list '* v (diff u var)))))
         ((and (eq? op '^) (= (length args) 2)
               (number? (cadr args)))
          (let ((u (car args)) (n (cadr args)))
            (list '* n
                  (list '* (list '^ u (- n 1)) (diff u var)))))
         (else (error "diff: unsupported operator" expr)))))
    (else (error "diff: unsupported expression" expr))))

;; Sanity checks.
(check= (diff 42 'x)                          0           "d/dx 42 = 0")
(check= (diff 'x 'x)                          1           "d/dx x = 1")
(check= (diff 'y 'x)                          0           "d/dx y = 0")

;; Sum rule: d/dx (x + y) = 1 + 0 = 1 (unsimplified).
(check= (diff '(+ x y) 'x)                    '(+ 1 0)    "d/dx (x + y) unsimplified")

;; Product rule: d/dx (x * x) = x*1 + x*1 (unsimplified).
(check= (diff '(* x x) 'x)                    '(+ (* x 1) (* x 1))
        "product rule, unsimplified")

;; Power rule with chain: d/dx x^3 = 3 * x^2 * 1 (unsimplified).
(check= (diff '(^ x 3) 'x)                    '(* 3 (* (^ x 2) 1))
        "power rule, unsimplified")

;; ----------------------------------------------------------------
;; Part 8: Cross-check symbolic diff against polynomial diff.
;;
;; The two systems represent polynomials differently: coefficient lists
;; vs S-expressions. A simple evaluator bridges them -- evaluate the
;; symbolic derivative at concrete x values and compare to evaluating
;; the polynomial derivative at the same values.
;;
;; This is the pattern you use to catch bugs in either implementation:
;; if they agree on every test input, neither is wrong in the easily
;; observable ways.
;; ----------------------------------------------------------------

(define (seval expr env)
  ;; Tiny s-expression evaluator.
  (cond
    ((number? expr) expr)
    ((symbol? expr) (cdr (assq expr env)))
    ((pair? expr)
     (let ((op (car expr)) (args (map (lambda (a) (seval a env)) (cdr expr))))
       (case op
         ((+) (+ (car args) (cadr args)))
         ((-) (- (car args) (cadr args)))
         ((*) (* (car args) (cadr args)))
         ((^) (let pow ((b (car args)) (n (cadr args)))
                (if (= n 0) 1 (* b (pow b (- n 1))))))
         (else (error "seval: unknown op" op)))))))

(define p-sexp '(+ 3 (+ (* 2 x) (^ x 2))))    ; same polynomial as p

(check= (seval p-sexp '((x . 0)))   3   "p(0) = 3 via symbolic evaluator")
(check= (seval p-sexp '((x . 1)))   6   "p(1) = 6 via symbolic evaluator")
(check= (seval p-sexp '((x . 2)))  11   "p(2) = 11 via symbolic evaluator")

(define dp-sexp (diff p-sexp 'x))             ; symbolic derivative

;; Evaluate symbolic derivative and compare to poly-eval of poly-derivative.
(define dp-poly (poly-derivative p))

(for-each
  (lambda (x-val)
    (check= (seval dp-sexp `((x . ,x-val)))
            (poly-eval dp-poly x-val)
            (string-append "symbolic vs polynomial derivative at x="
                           (number->string x-val))))
  '(-2 -1 0 1 2 3 5 10))

;; ----------------------------------------------------------------
;; Part 9: with-polynomial destructuring for compact code.
;;
;; When you are doing a lot of polynomial arithmetic, the repeated
;; `poly-*` prefix is noise. `with-polynomial` binds plus, times,
;; zero, one, and negate to the names you choose. Derivative is not
;; part of with-polynomial's field list -- it belongs to the
;; differential-ring, not the polynomial-ring.
;; ----------------------------------------------------------------

(define plus-one-squared-plus-p
  (with-polynomial Z (p+ pt zero one neg)
    (lambda (poly)
      (p+ (pt (p+ one poly) (p+ one poly)) poly))))

;; (1+x)^2 + x = 1 + 3x + x^2
(check= (poly-coeffs (plus-one-squared-plus-p (make-poly Z '(0 1))))
        '(1 3 1)
        "(1+x)^2 + x = 1 + 3x + x^2 via with-polynomial")

(display "chapter 05 complete") (newline)
