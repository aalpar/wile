;;; (wile algebra ring) — Rings and fields
;;;
;;; A ring (R, +, ×, 0, 1, -) is a semiring where (R, +, 0, -) is an
;;; abelian group. A field adds multiplicative inverses for nonzero elements.

;; ─── Rings ───────────────────────────────────

(define-record-type <ring>
  (make-ring* plus-fn times-fn zero one negate-fn)
  ring?
  (plus-fn   ring-plus-fn)
  (times-fn  ring-times-fn)
  (zero      ring-zero)
  (one       ring-one)
  (negate-fn ring-negate-fn))

(define (make-ring plus times zero one negate)
  (make-ring* plus times zero one negate))

(define (ring-plus R a b)   ((ring-plus-fn R) a b))
(define (ring-times R a b)  ((ring-times-fn R) a b))
(define (ring-negate R a)   ((ring-negate-fn R) a))
(define (ring-minus R a b)  (ring-plus R a (ring-negate R b)))

(define (ring->semiring R)
  (make-semiring (ring-plus-fn R) (ring-times-fn R)
                 (ring-zero R) (ring-one R)))

(define (ring->additive-group R)
  (make-group (ring-plus-fn R) (ring-zero R) (ring-negate-fn R)))

(define-syntax with-ring
  (syntax-rules ()
    ((with-ring R (plus times zero one negate) body ...)
     (let ((tmp R))
       (let ((plus   (lambda (a b) (ring-plus tmp a b)))
             (times  (lambda (a b) (ring-times tmp a b)))
             (zero   (ring-zero tmp))
             (one    (ring-one tmp))
             (negate (lambda (a) (ring-negate tmp a))))
         body ...)))))

;; ─── Pre-built ring instances ────────────────

(define (integer-ring)
  (make-ring + * 0 1 -))

(define (modular-ring n)
  (make-ring
    (lambda (a b) (modulo (+ a b) n))
    (lambda (a b) (modulo (* a b) n))
    0 1
    (lambda (a) (modulo (- a) n))))

;; ─── Ring validation ─────────────────────────

(define (validate-ring R samples)
  (let ((violations '())
        (z (ring-zero R))
        (o (ring-one R)))
    (define (fail! type . args)
      (set! violations (cons (cons type args) violations)))
    (for-each
      (lambda (a)
        ;; Additive identity
        (unless (equal? (ring-plus R z a) a)
          (fail! 'additive-left-identity a))
        ;; Multiplicative identity
        (unless (equal? (ring-times R o a) a)
          (fail! 'multiplicative-left-identity a))
        ;; Additive inverse
        (unless (equal? (ring-plus R a (ring-negate R a)) z)
          (fail! 'additive-inverse a))
        ;; Distributivity
        (for-each
          (lambda (b)
            (for-each
              (lambda (c)
                (unless (equal? (ring-times R a (ring-plus R b c))
                                (ring-plus R (ring-times R a b)
                                             (ring-times R a c)))
                  (fail! 'left-distributivity a b c)))
              samples))
          samples))
      samples)
    (if (null? violations) #t (reverse violations))))

;; ─── Fields ──────────────────────────────────

(define-record-type <field>
  (make-field* plus-fn times-fn zero one negate-fn reciprocal-fn)
  field?
  (plus-fn       field-plus-fn)
  (times-fn      field-times-fn)
  (zero          field-zero)
  (one           field-one)
  (negate-fn     field-negate-fn)
  (reciprocal-fn field-reciprocal-fn))

(define (make-field plus times zero one negate reciprocal)
  (make-field* plus times zero one negate reciprocal))

(define (field-plus F a b)       ((field-plus-fn F) a b))
(define (field-times F a b)      ((field-times-fn F) a b))
(define (field-negate F a)       ((field-negate-fn F) a))
(define (field-reciprocal F a)   ((field-reciprocal-fn F) a))
(define (field-divide F a b)     (field-times F a (field-reciprocal F b)))

(define (field->ring F)
  (make-ring (field-plus-fn F) (field-times-fn F)
             (field-zero F) (field-one F) (field-negate-fn F)))

(define-syntax with-field
  (syntax-rules ()
    ((with-field F (plus times zero one negate reciprocal) body ...)
     (let ((tmp F))
       (let ((plus       (lambda (a b) (field-plus tmp a b)))
             (times      (lambda (a b) (field-times tmp a b)))
             (zero       (field-zero tmp))
             (one        (field-one tmp))
             (negate     (lambda (a) (field-negate tmp a)))
             (reciprocal (lambda (a) (field-reciprocal tmp a))))
         body ...)))))

;; ─── Pre-built field instances ───────────────

(define (rational-field)
  (make-field + * 0 1 - (lambda (x) (/ 1 x))))

;; ─── Field validation ────────────────────────

(define (validate-field F samples)
  ;; Samples should exclude zero for multiplicative inverse checks.
  (let ((violations '())
        (z (field-zero F))
        (o (field-one F)))
    (define (fail! type . args)
      (set! violations (cons (cons type args) violations)))
    ;; Ring laws
    (let ((ring-result (validate-ring (field->ring F) samples)))
      (when (not (eq? #t ring-result))
        (set! violations (append ring-result violations))))
    ;; Multiplicative inverse for nonzero elements
    (for-each
      (lambda (a)
        (unless (equal? a z)
          (unless (equal? (field-times F a (field-reciprocal F a)) o)
            (fail! 'multiplicative-inverse a))))
      samples)
    (if (null? violations) #t (reverse violations))))
