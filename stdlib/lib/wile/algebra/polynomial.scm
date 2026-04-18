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
