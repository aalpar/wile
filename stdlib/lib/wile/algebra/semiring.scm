;;; (wile algebra semiring) — Semirings
;;;
;;; A semiring (S, +, ×, 0, 1) has:
;;; - (S, +, 0) is a commutative monoid
;;; - (S, ×, 1) is a monoid
;;; - × distributes over +
;;; - 0 annihilates ×: 0 × a = a × 0 = 0

(define-record-type <semiring>
  (make-semiring* plus-fn times-fn zero one)
  semiring?
  (plus-fn  semiring-plus-fn)
  (times-fn semiring-times-fn)
  (zero     semiring-zero)
  (one      semiring-one))

(define (make-semiring plus times zero one)
  "Construct a semiring from PLUS, TIMES, ZERO, and ONE.\nPLUS must be associative and commutative with ZERO as identity.\nTIMES must be associative with ONE as identity and must\ndistribute over PLUS. ZERO must annihilate TIMES from both sides."
  (make-semiring* plus times zero one))

(define (semiring-plus S a b)
  "Add A and B under semiring S's additive operation."
  ((semiring-plus-fn S) a b))

(define (semiring-times S a b)
  "Multiply A and B under semiring S's multiplicative operation."
  ((semiring-times-fn S) a b))

(define (semiring->additive-monoid S)
  "Extract the additive monoid (PLUS, ZERO) from semiring S."
  (make-monoid (semiring-plus-fn S) (semiring-zero S)))

(define (semiring->multiplicative-monoid S)
  "Extract the multiplicative monoid (TIMES, ONE) from semiring S."
  (make-monoid (semiring-times-fn S) (semiring-one S)))

;; ─── Pre-built instances ─────────────────────

(define (boolean-semiring)
  "Construct the Boolean semiring where PLUS is logical or and TIMES is logical and.\nThe additive identity (zero) is #f and the multiplicative\nidentity (one) is #t."
  (make-semiring
    (lambda (a b) (or a b))
    (lambda (a b) (and a b))
    #f #t))

(define (tropical-semiring)
  "Construct the tropical semiring where PLUS is min and TIMES is +.\nThe additive identity (zero) is +inf.0 and the multiplicative\nidentity (one) is 0. Useful for shortest-path problems."
  (make-semiring min + +inf.0 0))

(define (counting-semiring)
  "Construct the standard counting semiring over exact integers.\nPLUS is addition, TIMES is multiplication, zero is 0, one is 1."
  (make-semiring + * 0 1))

;; ─── Macro ───────────────────────────────────

(define-syntax with-semiring
  (syntax-rules ()
    ((with-semiring S (plus times zero one) body ...)
     (let ((tmp S))
       (let ((plus  (lambda (a b) (semiring-plus tmp a b)))
             (times (lambda (a b) (semiring-times tmp a b)))
             (zero  (semiring-zero tmp))
             (one   (semiring-one tmp)))
         body ...)))))

;; ─── Validation ──────────────────────────────

(define (validate-semiring S samples)
  "Spot-check that S satisfies the semiring laws on SAMPLES.\nTests additive and multiplicative identity, zero annihilation,\nadditive commutativity, and left and right distributivity for\nall elements and triples in SAMPLES. Returns #t if all laws\nhold, or a list of (violation-type element ...) entries\ndescribing failures."
  (let ((violations '())
        (z (semiring-zero S))
        (o (semiring-one S)))
    (define (fail! type . args)
      (set! violations (cons (cons type args) violations)))
    (for-each
      (lambda (a)
        ;; Additive identity
        (unless (equal? (semiring-plus S z a) a)
          (fail! 'additive-left-identity a))
        (unless (equal? (semiring-plus S a z) a)
          (fail! 'additive-right-identity a))
        ;; Multiplicative identity
        (unless (equal? (semiring-times S o a) a)
          (fail! 'multiplicative-left-identity a))
        (unless (equal? (semiring-times S a o) a)
          (fail! 'multiplicative-right-identity a))
        ;; Zero annihilation
        (unless (equal? (semiring-times S z a) z)
          (fail! 'left-annihilation a))
        (unless (equal? (semiring-times S a z) z)
          (fail! 'right-annihilation a))
        (for-each
          (lambda (b)
            ;; Additive commutativity
            (unless (equal? (semiring-plus S a b) (semiring-plus S b a))
              (fail! 'additive-commutativity a b))
            ;; Left distributivity: a × (b + c)
            (for-each
              (lambda (c)
                (unless (equal? (semiring-times S a (semiring-plus S b c))
                                (semiring-plus S (semiring-times S a b)
                                                 (semiring-times S a c)))
                  (fail! 'left-distributivity a b c))
                (unless (equal? (semiring-times S (semiring-plus S a b) c)
                                (semiring-plus S (semiring-times S a c)
                                                 (semiring-times S b c)))
                  (fail! 'right-distributivity a b c)))
              samples))
          samples))
      samples)
    (if (null? violations) #t (reverse violations))))
