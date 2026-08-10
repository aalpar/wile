;;; algebra-differential-test.scm — Differential ring tests

(import (scheme base)
        (chibi test)
        (wile algebra ring)
        (wile algebra polynomial)
        (wile algebra differential))

(test-begin "differential-rings")

;; ──────────────────────────────────────────────
;; Dual number ring
;; ──────────────────────────────────────────────

(test-group "dual-number-construction"
  (let ((D (dual-number-ring)))
    (test #t (differential-ring? D))
    (test #t (ring? (differential-ring-ring D)))))

(test-group "dual-ring-arithmetic"
  (let* ((D (dual-number-ring))
         (R (differential-ring-ring D)))
    ;; plus: (2,3) + (4,5) = (6,8)
    (test (cons 6 8) (ring-plus R (cons 2 3) (cons 4 5)))
    ;; times: (2,3) * (4,5) = (8, 2*5 + 3*4) = (8, 22)
    (test (cons 8 22) (ring-times R (cons 2 3) (cons 4 5)))
    ;; zero
    (test (cons 0 0) (ring-zero R))
    ;; one
    (test (cons 1 0) (ring-one R))
    ;; negate: -(3,7) = (-3,-7)
    (test (cons -3 -7) (ring-negate R (cons 3 7)))))

(test-group "dual-deriv"
  (let ((D (dual-number-ring)))
    ;; D(a,b) = (0,b) — the derivation on dual numbers
    (test (cons 0 5) (differential-deriv D (cons 3 5)))
    (test (cons 0 0) (differential-deriv D (cons 7 0)))
    (test (cons 0 1) (differential-deriv D (cons 0 1)))))

(test-group "dual-ad-x-squared"
  ;; f(x) = x², f'(x) = 2x, so f'(2) = 4
  ;; Represent x=2 as (2 . 1), compute x*x
  (let* ((D (dual-number-ring))
         (R (differential-ring-ring D))
         (x (cons 2 1))
         (x-squared (ring-times R x x)))
    ;; x² at x=2: (4 . 4)  — real part=4, ε-coeff=f'(2)=4
    (test (cons 4 4) x-squared)
    ;; AD derivative extraction: cdr gives f'(x)
    (test 4 (cdr x-squared))))

(test-group "dual-ad-polynomial"
  ;; f(x) = x³ + 2x, f'(x) = 3x² + 2, f'(3) = 29
  (let* ((D (dual-number-ring))
         (R (differential-ring-ring D))
         (x (cons 3 1))
         (x2 (ring-times R x x))
         (x3 (ring-times R x2 x))
         ;; 2x = x + x
         (two-x (ring-plus R x x))
         (result (ring-plus R x3 two-x)))
    ;; f(3) = 27 + 6 = 33
    (test 33 (car result))
    ;; f'(3) = 27 + 2 = 29
    (test 29 (cdr result))))

(test-group "dual-ad-product-rule"
  ;; f(x) = x*(x+1), f'(x) = 2x+1, f'(3) = 7
  (let* ((D (dual-number-ring))
         (R (differential-ring-ring D))
         (x (cons 3 1))
         (one (ring-one R))
         (x-plus-1 (ring-plus R x one))
         (result (ring-times R x x-plus-1)))
    ;; f(3) = 3*4 = 12
    (test 12 (car result))
    ;; f'(3) = 7
    (test 7 (cdr result))))

;; ──────────────────────────────────────────────
;; Polynomial derivation
;; ──────────────────────────────────────────────

(test-group "polynomial-ring-arithmetic"
  (let* ((IR (integer-ring))
         (D  (polynomial-derivation IR))
         (R  (differential-ring-ring D))
         (mk (lambda (cs) (make-poly IR cs))))
    ;; plus: (1 2) + (3 4) = (4 6)
    (test '(4 6) (poly-coeffs (ring-plus R (mk '(1 2)) (mk '(3 4)))))
    ;; plus with different lengths: (1 2 3) + (4 5) = (5 7 3)
    (test '(5 7 3) (poly-coeffs (ring-plus R (mk '(1 2 3)) (mk '(4 5)))))
    ;; times: (1 1) * (1 1) = 1 + 2x + x² = (1 2 1)
    (test '(1 2 1) (poly-coeffs (ring-times R (mk '(1 1)) (mk '(1 1)))))
    ;; zero
    (test '() (poly-coeffs (ring-zero R)))
    ;; one
    (test '(1) (poly-coeffs (ring-one R)))
    ;; negate: -(1 2 3) = (-1 -2 -3)
    (test '(-1 -2 -3) (poly-coeffs (ring-negate R (mk '(1 2 3)))))))

(test-group "polynomial-deriv"
  (let* ((IR (integer-ring))
         (D  (polynomial-derivation IR))
         (mk (lambda (cs) (make-poly IR cs))))
    ;; D(3 + 2x + x²) = 2 + 2x = (2 2)
    (test '(2 2) (poly-coeffs (differential-deriv D (mk '(3 2 1)))))
    ;; D(5) = 0 = ()
    (test '() (poly-coeffs (differential-deriv D (mk '(5)))))
    ;; D(x) = 1 = (1)
    (test '(1) (poly-coeffs (differential-deriv D (mk '(0 1)))))
    ;; D(x³) = 3x² = (0 0 3)
    (test '(0 0 3) (poly-coeffs (differential-deriv D (mk '(0 0 0 1)))))))

(test-group "polynomial-nth-deriv"
  (let* ((IR (integer-ring))
         (D  (polynomial-derivation IR))
         (mk (lambda (cs) (make-poly IR cs))))
    ;; D²(3 + 2x + x²) = D(2 + 2x) = (2)
    (test '(2) (poly-coeffs (differential-nth-deriv D 2 (mk '(3 2 1)))))
    ;; D³(3 + 2x + x²) = D(2) = ()
    (test '() (poly-coeffs (differential-nth-deriv D 3 (mk '(3 2 1)))))))

(test-group "polynomial-constant?"
  (let* ((IR (integer-ring))
         (D  (polynomial-derivation IR))
         (mk (lambda (cs) (make-poly IR cs))))
    ;; (5) is constant: D(5) = 0
    (test #t (differential-constant? D (mk '(5))))
    ;; () is constant: D(()) = 0
    (test #t (differential-constant? D (mk '())))
    ;; (1 1) is not constant: D(1+x) = (1) ≠ 0
    (test #f (differential-constant? D (mk '(1 1))))))

;; ──────────────────────────────────────────────
;; General operations
;; ──────────────────────────────────────────────

(test-group "dual-nth-deriv"
  (let ((D (dual-number-ring)))
    ;; D⁰ = identity
    (test (cons 3 5) (differential-nth-deriv D 0 (cons 3 5)))
    ;; D¹(3,5) = (0,5)
    (test (cons 0 5) (differential-nth-deriv D 1 (cons 3 5)))
    ;; D²(3,5) = D(0,5) = (0,5) — D is idempotent on dual numbers
    (test (cons 0 5) (differential-nth-deriv D 2 (cons 3 5)))))

(test-group "dual-constant?"
  (let ((D (dual-number-ring)))
    ;; (5 . 0) is constant: D(5,0) = (0,0) = zero
    (test #t (differential-constant? D (cons 5 0)))
    ;; (5 . 1) is not constant
    (test #f (differential-constant? D (cons 5 1)))))

(test-group "differential-ring->ring"
  (let ((D (dual-number-ring)))
    (test #t (ring? (differential-ring->ring D)))
    ;; Same object as differential-ring-ring
    (let ((R1 (differential-ring-ring D))
          (R2 (differential-ring->ring D)))
      (test (ring-zero R1) (ring-zero R2))
      (test (ring-one R1) (ring-one R2)))))

;; ──────────────────────────────────────────────
;; Validation
;; ──────────────────────────────────────────────

(test-group "validate-differential-ring"
  ;; Valid: dual number ring
  (test #t (validate-differential-ring
             (dual-number-ring)
             (list (cons 0 0) (cons 1 0) (cons 0 1) (cons 2 3))))
  ;; Valid: polynomial derivation over integers
  (let* ((IR (integer-ring))
         (mk (lambda (cs) (make-poly IR cs))))
    (test #t (validate-differential-ring
               (polynomial-derivation IR)
               (list (mk '()) (mk '(1)) (mk '(0 1)) (mk '(1 1))))))
  ;; Invalid: D(x)=1 (constant derivation breaks additivity and Leibniz)
  ;; Additivity: D(a+b)=1 but D(a)+D(b)=1+1=2
  ;; Leibniz: D(a*b)=1 but D(a)*b+a*D(b)=b+a
  (let* ((R (integer-ring))
         (bad-D (make-differential-ring R (lambda (x) 1)))
         (result (validate-differential-ring bad-D '(0 1 2 3))))
    (test #f (eq? #t result))
    ;; Should contain violations (additivity fails first)
    (test #t (pair? result))
    (test 'additivity (car (car result)))))

;; ──────────────────────────────────────────────
;; with-differential macro
;; ──────────────────────────────────────────────

(test-group "with-differential"
  (let ((D (dual-number-ring)))
    ;; Use the macro to compute (2,3) + (4,5) and take its derivative
    (test (cons 0 8)
      (with-differential D (plus times zero one negate deriv)
        (deriv (plus (cons 2 3) (cons 4 5)))))))

;; ──────────────────────────────────────────────
;; Negative validation — the third validate-ring inheritor
;; ──────────────────────────────────────────────
;;
;; validate-differential-ring replays validate-ring's findings into its own
;; reporter, so a ring axiom validate-ring did not check was invisible here
;; too. Observed at 003b3353: the structure below validated as a differential
;; ring, because the underlying non-ring validated as a ring.
;;
;; WHAT THIS DOES NOT COVER: the derivation laws themselves (additivity and
;; Leibniz) — the trivial derivation below satisfies both, deliberately, so the
;; only thing that can fail is the inherited ring axiom.

(test-group "validate-differential-ring inherits ring rejection"
  (let* ((broken (make-ring + (lambda (a b) (* a a b)) 0 1 -))
         (D (make-differential-ring broken (lambda (x) 0)))
         (result (validate-differential-ring D '(-2 -1 0 1 2)))
         (types (if (eq? result #t) '() (map car result)))
         (has? (lambda (type)
                 (let loop ((ts types))
                   (cond ((null? ts) #f)
                         ((eq? (car ts) type) #t)
                         (else (loop (cdr ts))))))))
    (test #f (eq? result #t))
    (test #t (has? 'multiplicative-associativity))
    (test #t (has? 'right-distributivity))))

(test-end)
(test-exit)
