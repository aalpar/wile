;;; (wile algebra monoid) — Monoids
;;;
;;; A monoid is a set with an associative binary operation and an identity
;;; element: (S, ⊕, e) where a ⊕ (b ⊕ c) = (a ⊕ b) ⊕ c and e ⊕ a = a ⊕ e = a.

(define-record-type <monoid>
  (make-monoid op identity)
  monoid?
  (op       monoid-op-fn)
  (identity monoid-identity))

(define (monoid-op M a b)
  ((monoid-op-fn M) a b))

(define (monoid-fold M lst)
  (let loop ((acc (monoid-identity M)) (xs lst))
    (if (null? xs) acc
        (loop (monoid-op M acc (car xs)) (cdr xs)))))

(define (monoid-power M a n)
  ;; Repeated application: a ⊕ a ⊕ ... (n times). O(n).
  (let loop ((acc (monoid-identity M)) (remaining n))
    (if (<= remaining 0) acc
        (loop (monoid-op M acc a) (- remaining 1)))))

(define-syntax with-monoid
  (syntax-rules ()
    ((with-monoid M (op identity) body ...)
     (let ((tmp M))
       (let ((op       (lambda (a b) (monoid-op tmp a b)))
             (identity (monoid-identity tmp)))
         body ...)))))

(define (validate-monoid M samples)
  (let ((violations '())
        (e (monoid-identity M)))
    (define (fail! type . args)
      (set! violations (cons (cons type args) violations)))
    (for-each
      (lambda (a)
        ;; Left identity
        (unless (equal? (monoid-op M e a) a)
          (fail! 'left-identity a))
        ;; Right identity
        (unless (equal? (monoid-op M a e) a)
          (fail! 'right-identity a))
        ;; Associativity (with all pairs)
        (for-each
          (lambda (b)
            (for-each
              (lambda (c)
                (unless (equal? (monoid-op M (monoid-op M a b) c)
                                (monoid-op M a (monoid-op M b c)))
                  (fail! 'associativity a b c)))
              samples))
          samples))
      samples)
    (if (null? violations) #t (reverse violations))))
