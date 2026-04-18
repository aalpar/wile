;;; (wile algebra polynomial) — Univariate polynomials over a ring
;;;
;;; Representation: coeffs is a list in ascending power order.
;;;   '(a0 a1 a2 ...) represents a0 + a1*x + a2*x^2 + ...
;;; Zero polynomial: coeffs = '().
;;; Invariant (enforced by make-poly): if coeffs is non-empty,
;;; its last element is not (ring-zero R).

;; ─── Polynomials ─────────────────────────────

(define-record-type <polynomial>
  (make-poly* ring coeffs)
  polynomial?
  (ring   poly-ring)
  (coeffs poly-coeffs))

;; Strip trailing zeros from the ascending coefficient list.
;; Internal — exposed downstream only through make-poly.
(define (poly-normalize R coeffs)
  (if (null? coeffs)
      '()
      (let loop ((cs (reverse coeffs)))
        (cond
          ((null? cs) '())
          ((equal? (car cs) (ring-zero R)) (loop (cdr cs)))
          (else (reverse cs))))))

(define (make-poly R coeffs)
  "Construct a polynomial over ring R from coefficient list COEFFS.\nCOEFFS is in ascending power order: (a0 a1 a2 ...) represents\na0 + a1*x + a2*x^2 + .... Trailing zero coefficients are stripped\nto enforce the normal-form invariant. The empty list represents\nthe zero polynomial.\n\nExamples:\n  (let ((R (integer-ring)))\n    (poly-coeffs (make-poly R '(1 2 3))))     => (1 2 3)\n  (let ((R (integer-ring)))\n    (poly-coeffs (make-poly R '(1 2 0 0))))   => (1 2)\n  (let ((R (integer-ring)))\n    (poly-coeffs (make-poly R '())))          => ()\n\nParameters:\n  R : any\n  coeffs : list\nReturns: any\nCategory: algebra\nKeywords: polynomial, univariate, constructor, coefficient list, normal form\n\nSee also: `polynomial?', `poly-ring', `poly-coeffs'."
  (make-poly* R (poly-normalize R coeffs)))

;; ─── Constants ───────────────────────────────

(define (poly-zero R)
  "Return the zero polynomial over ring R.\nRepresented internally by an empty coefficient list.\n\nExamples:\n  (poly-coeffs (poly-zero (integer-ring)))  => ()\n\nParameters:\n  R : any\nReturns: any\nCategory: algebra\nKeywords: zero polynomial, additive identity, empty polynomial\n\nSee also: `poly-one', `make-poly'."
  (make-poly* R '()))

(define (poly-one R)
  "Return the unit polynomial 1 over ring R.\n\nExamples:\n  (poly-coeffs (poly-one (integer-ring)))  => (1)\n\nParameters:\n  R : any\nReturns: any\nCategory: algebra\nKeywords: unit polynomial, multiplicative identity, constant one\n\nSee also: `poly-zero', `make-poly'."
  (make-poly* R (list (ring-one R))))

;; ─── Shape accessors ─────────────────────────

(define (poly-degree p)
  "Return the degree of polynomial P.\nBy convention, the zero polynomial has degree -1 (PARI/GP\nconvention); this lets callers test for zero via negative\ndegree without special-casing.\n\nExamples:\n  (poly-degree (make-poly (integer-ring) '()))       => -1\n  (poly-degree (make-poly (integer-ring) '(5)))      => 0\n  (poly-degree (make-poly (integer-ring) '(1 2 3)))  => 2\n\nParameters:\n  p : any\nReturns: integer\nCategory: algebra\nKeywords: degree, polynomial degree, order, rank\n\nSee also: `poly-leading-coeff'."
  (let ((cs (poly-coeffs p)))
    (if (null? cs)
        -1
        (- (length cs) 1))))

(define (poly-leading-coeff p)
  "Return the leading (highest-power) coefficient of P.\nFor the zero polynomial, returns the ring's zero element.\n\nExamples:\n  (poly-leading-coeff (make-poly (integer-ring) '(1 2 3)))  => 3\n  (poly-leading-coeff (make-poly (integer-ring) '()))       => 0\n\nParameters:\n  p : any\nReturns: any\nCategory: algebra\nKeywords: leading coefficient, highest coefficient, top coefficient\n\nSee also: `poly-degree', `poly-coeffs'."
  (let ((cs (poly-coeffs p)))
    (if (null? cs)
        (ring-zero (poly-ring p))
        (let loop ((xs cs))
          (if (null? (cdr xs)) (car xs) (loop (cdr xs)))))))

;; ─── Additive structure ─────────────────────

(define (poly-plus p q)
  "Add polynomials P and Q. Both must share the same coefficient ring.\nResult is normalized (trailing zeros stripped after coefficient-wise add).\n\nExamples:\n  (let ((R (integer-ring)))\n    (poly-coeffs (poly-plus (make-poly R '(1 2)) (make-poly R '(3 4 5)))))\n  => (4 6 5)\n\nParameters:\n  p : any\n  q : any\nReturns: any\nCategory: algebra\nKeywords: polynomial addition, add, sum, plus\n\nSee also: `poly-minus', `poly-negate'."
  (let ((R (poly-ring p)))
    (make-poly R
      (let loop ((xs (poly-coeffs p)) (ys (poly-coeffs q)))
        (cond
          ((null? xs) ys)
          ((null? ys) xs)
          (else
            (cons (ring-plus R (car xs) (car ys))
                  (loop (cdr xs) (cdr ys)))))))))

(define (poly-negate p)
  "Return the additive inverse of polynomial P.\nNegates every coefficient under the coefficient ring's negation.\n\nExamples:\n  (let ((R (integer-ring)))\n    (poly-coeffs (poly-negate (make-poly R '(1 2 3)))))  => (-1 -2 -3)\n\nParameters:\n  p : any\nReturns: any\nCategory: algebra\nKeywords: polynomial negation, additive inverse, unary minus\n\nSee also: `poly-plus', `poly-minus'."
  (let ((R (poly-ring p)))
    (make-poly* R
      (map (lambda (c) (ring-negate R c)) (poly-coeffs p)))))

(define (poly-minus p q)
  "Subtract polynomial Q from P. Computed as P plus negation of Q.\n\nExamples:\n  (let ((R (integer-ring)))\n    (poly-coeffs (poly-minus (make-poly R '(3 4)) (make-poly R '(1 2)))))\n  => (2 2)\n\nParameters:\n  p : any\n  q : any\nReturns: any\nCategory: algebra\nKeywords: polynomial subtraction, subtract, difference, minus\n\nSee also: `poly-plus', `poly-negate'."
  (poly-plus p (poly-negate q)))

;; ─── Multiplication ─────────────────────────
;;
;; Naive O(n·m) schoolbook multiplication. Karatsuba/FFT would lower
;; this to O(n^1.58) / O(n log n) but are not warranted until a real
;; benchmark justifies the added complexity.

(define (poly-times p q)
  "Multiply polynomials P and Q. Both must share the same coefficient ring.\nComputed via schoolbook multiplication in O(n*m) coefficient operations.\n\nExamples:\n  (let ((R (integer-ring)))\n    (poly-coeffs (poly-times (make-poly R '(1 1)) (make-poly R '(1 1)))))\n  => (1 2 1)\n\nParameters:\n  p : any\n  q : any\nReturns: any\nCategory: algebra\nKeywords: polynomial multiplication, multiply, product, times, convolution\n\nSee also: `poly-plus', `poly-eval'."
  (let ((R  (poly-ring p))
        (xs (poly-coeffs p))
        (ys (poly-coeffs q)))
    (cond
      ((null? xs) (poly-zero R))
      ((null? ys) (poly-zero R))
      (else
        (let ((rz (ring-zero R))
              (n  (+ (length xs) (length ys) -1)))
          ;; Accumulate into a vector of length n, then convert.
          (let ((acc (make-vector n rz)))
            (let loop-i ((i 0) (xs xs))
              (if (null? xs)
                  (make-poly R (vector->list acc))
                  (begin
                    (let loop-j ((j 0) (ys ys))
                      (if (null? ys)
                          (if #f #f)  ; unspecified
                          (let ((k (+ i j)))
                            (vector-set! acc k
                              (ring-plus R (vector-ref acc k)
                                           (ring-times R (car xs) (car ys))))
                            (loop-j (+ j 1) (cdr ys)))))
                    (loop-i (+ i 1) (cdr xs)))))))))))

;; ─── Evaluation (Horner's method) ──────────
;;
;; For coeffs in ascending order (a0 a1 ... an), Horner's scheme
;; computes p(x) = a0 + x(a1 + x(a2 + ... + x*an)) using n
;; multiplications and n additions — O(n) rather than the O(n^2)
;; of naive power-accumulation.

(define (poly-eval p x)
  "Evaluate polynomial P at point X via Horner's method.\nUses n multiplications and n additions under the coefficient\nring for a degree-n polynomial — O(n) rather than the O(n^2)\nof naive power accumulation. The point X must belong to the\ncoefficient ring (or a ring extension where ring-plus and\nring-times remain meaningful).\n\nExamples:\n  (let ((R (integer-ring)))\n    (poly-eval (make-poly R '(1 2 3)) 2))  => 17\n  ; because 1 + 2*2 + 3*4 = 17\n\nParameters:\n  p : any\n  x : any\nReturns: any\nCategory: algebra\nKeywords: polynomial evaluation, Horner's method, Horner scheme, synthetic substitution\n\nSee also: `poly-plus', `poly-times'."
  (let ((R (poly-ring p)))
    (let loop ((cs (reverse (poly-coeffs p))) (acc (ring-zero R)))
      (if (null? cs)
          acc
          (loop (cdr cs) (ring-plus R (car cs) (ring-times R x acc)))))))

;; ─── Formal derivative ──────────────────────
;;
;; The formal derivative is purely symbolic — no limits, no analysis.
;; For coeffs (a0 a1 a2 ... an) in ascending order, the derivative is
;; (a1 2*a2 3*a3 ... n*an). In rings without natural integer embedding
;; (e.g., GF(p)), the "k" factor is built by repeated addition of ring-one.

(define (poly-derivative p)
  "Return the formal derivative of polynomial P.\nPurely symbolic: for (a0 a1 a2 ... an), produces\n(a1 2*a2 3*a3 ... n*an). The integer multiplier k is realized\ninside the coefficient ring by summing ring-one k times, so this\nworks correctly over rings of positive characteristic (e.g., GF(p)).\n\nExamples:\n  (let ((R (integer-ring)))\n    (poly-coeffs (poly-derivative (make-poly R '(1 2 3)))))  => (2 6)\n  (let ((R (integer-ring)))\n    (poly-coeffs (poly-derivative (make-poly R '(42)))))     => ()\n\nParameters:\n  p : any\nReturns: any\nCategory: algebra\nKeywords: formal derivative, symbolic derivative, differentiation, polynomial derivative\n\nSee also: `poly-plus', `poly-eval'."
  (let ((R (poly-ring p)))
    (let ((cs (poly-coeffs p)))
      (if (or (null? cs) (null? (cdr cs)))
          (poly-zero R)
          (let ((rz (ring-zero R))
                (ro (ring-one R)))
            ;; Build ring-element representing natural number k.
            (define (ring-nat k)
              (let loop ((i 0) (acc rz))
                (if (>= i k)
                    acc
                    (loop (+ i 1) (ring-plus R acc ro)))))
            (make-poly R
              (let loop ((xs (cdr cs)) (k 1))
                (if (null? xs)
                    '()
                    (cons (ring-times R (ring-nat k) (car xs))
                          (loop (cdr xs) (+ k 1)))))))))))

;; ─── Division with remainder ───────────────
;;
;; poly-divmod requires the coefficient structure to be a FIELD (not
;; merely a ring) because long division must divide each leading term
;; of the remainder by the divisor's leading coefficient, which needs
;; a multiplicative inverse. The divisor must be non-zero. Returns
;; (values quotient remainder) satisfying
;;   p = q*d + r,  (poly-degree r) < (poly-degree d).

(define (poly-divmod p d F)
  "Divide polynomial P by non-zero polynomial D over field F.\nReturns two values: the quotient Q and remainder R satisfying\nP = Q*D + R with (poly-degree R) < (poly-degree D).\nRequires F to be a field (for reciprocal of D's leading coefficient).\nThe polynomial records P and D carry their own coefficient ring,\nbut F provides the field structure needed for division; typically\nF is the field whose underlying ring matches (poly-ring P).\n\nExamples:\n  (let* ((F (rational-field)) (R (field->ring F)))\n    (call-with-values\n      (lambda () (poly-divmod (make-poly R '(-1 0 1)) (make-poly R '(-1 1)) F))\n      (lambda (q r) (list (poly-coeffs q) (poly-coeffs r)))))\n  => ((1 1) ())   ; (x^2 - 1) / (x - 1) = x + 1\n\nParameters:\n  p : any\n  d : any\n  F : any\nReturns: (values any any)\nCategory: algebra\nKeywords: polynomial division, long division, divmod, quotient, remainder, Euclidean division\n\nSee also: `poly-gcd', `field-reciprocal'."
  (let ((R (poly-ring p)))
    (when (null? (poly-coeffs d))
      (error "poly-divmod: division by zero polynomial"))
    (let ((lead-d-inv (field-reciprocal F (poly-leading-coeff d)))
          (deg-d      (poly-degree d))
          (deg-p      (poly-degree p)))
      ;; Quotient capacity: highest shift is (deg-p - deg-d) from the
      ;; first iteration, so vector size is (deg-p - deg-d) + 1.
      ;; Clamp to 0 when deg-p < deg-d (no iterations run).
      (let ((cap (max 0 (+ 1 (- deg-p deg-d)))))
        (let loop ((rem p)
                   (q-coeffs-rev '()))
          (let ((deg-r (poly-degree rem)))
            (if (< deg-r deg-d)
                (values (make-poly R (reverse-pad q-coeffs-rev R cap))
                        rem)
                (let* ((lead-r  (poly-leading-coeff rem))
                       (coeff   (ring-times R lead-r lead-d-inv))
                       (shift   (- deg-r deg-d))
                       ;; term = coeff * x^shift, as a polynomial
                       (term    (make-poly R
                                  (append (make-list shift (ring-zero R))
                                          (list coeff))))
                       (rem*    (poly-minus rem (poly-times term d))))
                  (loop rem* (cons (cons shift coeff) q-coeffs-rev))))))))))

;; Internal helper: given a list of (shift . coeff) entries produced
;; by long division, materialize the quotient's ascending coefficient
;; list of length N, filling unassigned slots with ring-zero. Order
;; of entries does not matter — each entry places its coeff at index
;; shift directly via vector-set!.
(define (reverse-pad entries R n)
  (let ((acc (make-vector n (ring-zero R))))
    (for-each
      (lambda (e) (vector-set! acc (car e) (cdr e)))
      entries)
    (vector->list acc)))
