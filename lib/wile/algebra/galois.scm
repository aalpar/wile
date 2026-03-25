;;; (wile algebra galois) — Galois connections
;;;
;;; A Galois connection (α, γ) between a concrete partial order C and
;;; an abstract lattice A satisfies:
;;;   ∀c ∈ C. c ≤_C γ(α(c))     (soundness / extensive)
;;;   ∀a ∈ A. α(γ(a)) ≤_A a     (reductive)

(define-record-type <galois-connection>
  (make-galois-connection* alpha-fn gamma-fn concrete-po abstract-lattice)
  galois-connection?
  (alpha-fn        gc-alpha-fn)
  (gamma-fn        gc-gamma-fn)
  (concrete-po     gc-concrete-po)
  (abstract-lattice gc-abstract-lattice))

(define (make-galois-connection alpha gamma concrete-po abstract-lattice)
  (make-galois-connection* alpha gamma concrete-po abstract-lattice))

(define (gc-alpha GC concrete-val)
  ((gc-alpha-fn GC) concrete-val))

(define (gc-gamma GC abstract-val)
  ((gc-gamma-fn GC) abstract-val))

(define (gc-sound? GC concrete-samples abstract-samples)
  ;; Spot-check both Galois conditions on sample elements.
  ;; Returns #t or list of (violation-type ...) entries.
  (let ((violations '()))
    (define (fail! type . args)
      (set! violations (cons (cons type args) violations)))
    ;; Extensive: ∀c. c ≤ γ(α(c))
    (for-each
      (lambda (c)
        (let ((round-tripped (gc-gamma GC (gc-alpha GC c))))
          (unless (po-leq? (gc-concrete-po GC) c round-tripped)
            (fail! 'extensive c round-tripped))))
      concrete-samples)
    ;; Reductive: ∀a. α(γ(a)) ≤ a
    (for-each
      (lambda (a)
        (let ((round-tripped (gc-alpha GC (gc-gamma GC a))))
          (unless (lattice-leq? (gc-abstract-lattice GC) round-tripped a)
            (fail! 'reductive a round-tripped))))
      abstract-samples)
    (if (null? violations) #t (reverse violations))))
