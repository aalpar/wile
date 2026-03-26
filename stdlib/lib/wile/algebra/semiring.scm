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
  (make-semiring* plus times zero one))

(define (semiring-plus S a b)
  ((semiring-plus-fn S) a b))

(define (semiring-times S a b)
  ((semiring-times-fn S) a b))

(define (semiring->additive-monoid S)
  (make-monoid (semiring-plus-fn S) (semiring-zero S)))

(define (semiring->multiplicative-monoid S)
  (make-monoid (semiring-times-fn S) (semiring-one S)))

;; ─── Pre-built instances ─────────────────────

(define (boolean-semiring)
  (make-semiring
    (lambda (a b) (or a b))
    (lambda (a b) (and a b))
    #f #t))

(define (tropical-semiring)
  (make-semiring min + +inf.0 0))

(define (counting-semiring)
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
