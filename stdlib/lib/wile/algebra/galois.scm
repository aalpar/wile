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
  "Construct a Galois connection from ALPHA, GAMMA, CONCRETE-PO, and ABSTRACT-LATTICE.\nALPHA abstracts concrete values into the abstract lattice. GAMMA\nconcretizes abstract values back. The pair must satisfy: every\nconcrete value is below the concretization of its abstraction\n(soundness), and abstracting a concretization never exceeds the\noriginal abstract value (reductiveness).\n\nSee also: `gc-alpha', `gc-gamma', `gc-sound?'."
  (make-galois-connection* alpha gamma concrete-po abstract-lattice))

(define (gc-alpha GC concrete-val)
  "Abstract CONCRETE-VAL through Galois connection GC.\nMaps a concrete value into the abstract lattice.\n\nSee also: `gc-gamma', `gc-abstract-lattice'."
  ((gc-alpha-fn GC) concrete-val))

(define (gc-gamma GC abstract-val)
  "Concretize ABSTRACT-VAL through Galois connection GC.\nMaps an abstract lattice element back to the concrete domain.\n\nSee also: `gc-alpha', `gc-concrete-po'."
  ((gc-gamma-fn GC) abstract-val))

(define (gc-sound? GC concrete-samples abstract-samples)
  "Spot-check that GC satisfies the Galois connection laws on sample elements.\nTests extensiveness (c <= gamma(alpha(c)) for each concrete sample)\nand reductiveness (alpha(gamma(a)) <= a for each abstract sample).\nReturns #t if all conditions hold, or a list of (violation-type ...)\nentries describing failures.\n\nSee also: `make-galois-connection', `gc-alpha', `gc-gamma'."
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
