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
