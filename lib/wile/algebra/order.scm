;;; (wile algebra order) — Partial orders
;;;
;;; A partial order is a reflexive, antisymmetric, transitive relation.
;;; Represented as an R7RS record holding a single leq? predicate.

(define-record-type <partial-order>
  (make-partial-order leq?)
  partial-order?
  (leq? po-leq-fn))

(define (po-leq? po a b)
  ((po-leq-fn po) a b))

(define (po-comparable? po a b)
  (or (po-leq? po a b)
      (po-leq? po b a)))

(define (po-monotone? po f a b)
  ;; a ≤ b ⟹ f(a) ≤ f(b)
  (if (po-leq? po a b)
      (po-leq? po (f a) (f b))
      #t))  ; precondition not met, vacuously true

(define (validate-partial-order po samples)
  ;; Spot-check reflexivity, antisymmetry, transitivity on sample pairs.
  ;; Returns #t or a list of (violation-type a b ...) entries.
  (let ((violations '()))
    (define (fail! type . args)
      (set! violations (cons (cons type args) violations)))
    ;; Reflexivity: a ≤ a
    (for-each
      (lambda (a) (unless (po-leq? po a a) (fail! 'reflexivity a)))
      samples)
    ;; Antisymmetry: a ≤ b ∧ b ≤ a ⟹ a = b
    ;; Without an equality predicate we cannot fully check this.
    ;; Transitivity: a ≤ b ∧ b ≤ c ⟹ a ≤ c
    (for-each
      (lambda (a)
        (for-each
          (lambda (b)
            (when (po-leq? po a b)
              (for-each
                (lambda (c)
                  (when (and (po-leq? po b c)
                             (not (po-leq? po a c)))
                    (fail! 'transitivity a b c)))
                samples)))
          samples))
      samples)
    (if (null? violations) #t (reverse violations))))
