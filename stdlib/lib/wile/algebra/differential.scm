;;; (wile algebra differential) — Differential rings
;;;
;;; A differential ring is a ring R equipped with a derivation D : R → R
;;; satisfying additivity D(a+b) = D(a)+D(b) and the Leibniz rule
;;; D(a·b) = D(a)·b + a·D(b).

;; ─── Differential rings ─────────────────────

(define-record-type <differential-ring>
  (make-differential-ring* ring deriv-fn)
  differential-ring?
  (ring     differential-ring-ring)
  (deriv-fn differential-deriv-fn))

(define (make-differential-ring R deriv)
  "Construct a differential ring from ring R and derivation DERIV.\nDERIV must be a unary procedure satisfying additivity\nD(a+b) = D(a)+D(b) and the Leibniz rule\nD(a*b) = D(a)*b + a*D(b).\n\nExamples:\n  (let ((D (make-differential-ring (integer-ring) (lambda (x) 0))))\n    (differential-deriv D 42))  => 0\n\nParameters:\n  R : any\n  deriv : procedure\nReturns: any\nCategory: algebra\nKeywords: differential ring, derivation, Leibniz rule, differential algebra\n\nSee also: `differential-deriv', `validate-differential-ring'."
  (make-differential-ring* R deriv))

(define (differential-deriv D a)
  "Apply the derivation of differential ring D to element A.\n\nExamples:\n  (differential-deriv (dual-number-ring) (cons 3 5))  => (0 . 5)\n\nParameters:\n  D : any\n  a : any\nReturns: any\nCategory: algebra\nKeywords: derivation, derivative, differentiate, D operator"
  ((differential-deriv-fn D) a))

(define (differential-nth-deriv D n a)
  "Apply the derivation of D to A exactly N times.\nD^0 is the identity: (differential-nth-deriv D 0 a) returns a.\n\nExamples:\n  (differential-nth-deriv (dual-number-ring) 0 (cons 3 5))  => (3 . 5)\n  (differential-nth-deriv (dual-number-ring) 1 (cons 3 5))  => (0 . 5)\n\nParameters:\n  D : any\n  n : integer\n  a : any\nReturns: any\nCategory: algebra\nKeywords: iterated derivation, higher derivative, nth derivative, repeated"
  (let loop ((remaining n) (acc a))
    (if (<= remaining 0)
        acc
        (loop (- remaining 1) (differential-deriv D acc)))))

(define (differential-constant? D a)
  "Test whether A is a constant under D's derivation.\nAn element is constant when D(a) equals the zero of the\nunderlying ring.\n\nExamples:\n  (differential-constant? (dual-number-ring) (cons 5 0))  => #t\n  (differential-constant? (dual-number-ring) (cons 5 1))  => #f\n\nParameters:\n  D : any\n  a : any\nReturns: boolean\nCategory: algebra\nKeywords: constant, kernel, zero derivative"
  (equal? (differential-deriv D a)
          (ring-zero (differential-ring-ring D))))

(define (differential-ring->ring D)
  "Extract the underlying ring from differential ring D.\n\nExamples:\n  (ring? (differential-ring->ring (dual-number-ring)))  => #t\n\nParameters:\n  D : any\nReturns: any\nCategory: algebra\nKeywords: forgetful functor, projection, underlying ring\n\nSee also: `differential-ring-ring'."
  (differential-ring-ring D))

(define-syntax with-differential
  (syntax-rules ()
    ((with-differential D (plus times zero one negate deriv) body ...)
     (let ((tmp D))
       (let ((r (differential-ring-ring tmp)))
         (let ((plus   (lambda (a b) (ring-plus r a b)))
               (times  (lambda (a b) (ring-times r a b)))
               (zero   (ring-zero r))
               (one    (ring-one r))
               (negate (lambda (a) (ring-negate r a)))
               (deriv  (lambda (a) (differential-deriv tmp a))))
           body ...))))))

;; ─── Dual numbers: R[ε]/(ε²=0) ─────────────

(define (dual-number-ring)
  "Construct the differential ring of dual numbers over integers.\nElements are pairs (a . b) representing a + b*ε where ε²=0.\nThe derivation D(a,b) = (0,b) satisfies the Leibniz rule.\nFor automatic differentiation, evaluate f at (x . 1) to get\n(f(x) . f'(x)), then extract f'(x) with cdr.\n\nExamples:\n  (let ((D (dual-number-ring)))\n    (differential-deriv D (cons 3 5)))  => (0 . 5)\n  ;; AD for f(x)=x²: evaluate at x=(2 . 1)\n  (let ((D (dual-number-ring)))\n    (let ((R (differential-ring-ring D)))\n      (let ((x (cons 2 1)))\n        (cdr (ring-times R x x)))))  => 4\n\nReturns: any\nCategory: algebra\nKeywords: dual numbers, automatic differentiation, AD, epsilon, infinitesimal\n\nSee also: `polynomial-derivation', `make-differential-ring'."
  (let ((R (make-ring
             ;; plus: (a,b) + (c,d) = (a+c, b+d)
             (lambda (x y)
               (cons (+ (car x) (car y))
                     (+ (cdr x) (cdr y))))
             ;; times: (a,b) * (c,d) = (a*c, a*d + b*c)
             (lambda (x y)
               (cons (* (car x) (car y))
                     (+ (* (car x) (cdr y))
                        (* (cdr x) (car y)))))
             ;; zero
             (cons 0 0)
             ;; one
             (cons 1 0)
             ;; negate
             (lambda (x)
               (cons (- (car x)) (- (cdr x)))))))
    (make-differential-ring R (lambda (x) (cons 0 (cdr x))))))

;; ─── Polynomial derivation ──────────────────

(define (polynomial-derivation R)
  "Construct a differential ring of polynomials over ring R.\nElements are <polynomial> records (see (wile algebra polynomial)).\nThe underlying ring is (polynomial-ring R); the derivation is the\nformal derivative poly-derivative.\n\nExamples:\n  (let* ((R (integer-ring))\n         (D (polynomial-derivation R))\n         (p (make-poly R '(3 2 1))))\n    (poly-coeffs (differential-deriv D p)))  => (2 2)\n\nParameters:\n  R : any\nReturns: any\nCategory: algebra\nKeywords: polynomial, formal derivative, differential ring, polynomial derivation\n\nSee also: `dual-number-ring', `make-differential-ring', `polynomial-ring'."
  (make-differential-ring (polynomial-ring R) poly-derivative))

;; ─── Differential ring validation ───────────

(define (validate-differential-ring D samples)
  "Spot-check that D satisfies the differential ring laws on SAMPLES.\nFirst delegates to validate-ring on the underlying ring, then\nchecks additivity D(a+b) = D(a)+D(b) and the Leibniz rule\nD(a*b) = D(a)*b + a*D(b) for all pairs in SAMPLES.\nReturns #t if all laws hold, or a list of (violation-type ...)\nentries describing failures.\n\nExamples:\n  (validate-differential-ring\n    (dual-number-ring)\n    (list (cons 1 0) (cons 0 1) (cons 2 3)))  => #t\n\nParameters:\n  D : any\n  samples : list\nReturns: any\nCategory: algebra\nKeywords: Leibniz rule, additivity, derivation, law checking, validation\n\nSee also: `make-differential-ring', `validate-ring'."
  (let ((violations '())
        (R (differential-ring-ring D)))
    (define (fail! type . args)
      (set! violations (cons (cons type args) violations)))
    ;; Ring laws
    (let ((ring-result (validate-ring R samples)))
      (when (not (eq? #t ring-result))
        (set! violations (append ring-result violations))))
    ;; Derivation laws
    (for-each
      (lambda (a)
        (for-each
          (lambda (b)
            ;; Additivity: D(a+b) = D(a)+D(b)
            (let ((lhs (differential-deriv D (ring-plus R a b)))
                  (rhs (ring-plus R
                         (differential-deriv D a)
                         (differential-deriv D b))))
              (unless (equal? lhs rhs)
                (fail! 'additivity a b)))
            ;; Leibniz rule: D(a*b) = D(a)*b + a*D(b)
            (let ((lhs (differential-deriv D (ring-times R a b)))
                  (rhs (ring-plus R
                         (ring-times R (differential-deriv D a) b)
                         (ring-times R a (differential-deriv D b)))))
              (unless (equal? lhs rhs)
                (fail! 'leibniz-rule a b))))
          samples))
      samples)
    (if (null? violations) #t (reverse violations))))
