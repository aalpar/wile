;;; (wile algebra group) — Groups
;;;
;;; A group (G, ⊕, e, ⁻¹) is a monoid with inverses:
;;; a ⊕ a⁻¹ = a⁻¹ ⊕ a = e.

(define-record-type <group>
  (make-group* op-fn identity inverse-fn)
  group?
  (op-fn      group-op-fn)
  (identity   group-identity)
  (inverse-fn group-inverse-fn))

(define (make-group op identity inverse)
  (make-group* op identity inverse))

(define (group-op G a b)
  ((group-op-fn G) a b))

(define (group-inverse G a)
  ((group-inverse-fn G) a))

(define (group->monoid G)
  (make-monoid (group-op-fn G) (group-identity G)))

(define-syntax with-group
  (syntax-rules ()
    ((with-group G (op identity inverse) body ...)
     (let ((tmp G))
       (let ((op      (lambda (a b) (group-op tmp a b)))
             (identity (group-identity tmp))
             (inverse  (lambda (a) (group-inverse tmp a))))
         body ...)))))

(define (validate-group G samples)
  (let ((violations '())
        (e (group-identity G)))
    (define (fail! type . args)
      (set! violations (cons (cons type args) violations)))
    ;; Monoid laws + inverse
    (for-each
      (lambda (a)
        (unless (equal? (group-op G e a) a)
          (fail! 'left-identity a))
        (unless (equal? (group-op G a e) a)
          (fail! 'right-identity a))
        ;; Inverse
        (unless (equal? (group-op G a (group-inverse G a)) e)
          (fail! 'right-inverse a))
        (unless (equal? (group-op G (group-inverse G a) a) e)
          (fail! 'left-inverse a))
        ;; Associativity
        (for-each
          (lambda (b)
            (for-each
              (lambda (c)
                (unless (equal? (group-op G (group-op G a b) c)
                                (group-op G a (group-op G b c)))
                  (fail! 'associativity a b c)))
              samples))
          samples))
      samples)
    (if (null? violations) #t (reverse violations))))
