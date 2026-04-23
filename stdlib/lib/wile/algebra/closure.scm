;;; (wile algebra closure) -- Closure operators
;;;
;;; A closure operator on a lattice L is a function cl : L -> L that is
;;; extensive (a <= cl(a)), monotone (a <= b => cl(a) <= cl(b)), and
;;; idempotent (cl(cl(a)) = cl(a)).  The fixed points form a sublattice
;;; of closed elements.

;; -- Record type -----------------------------------------

(define-record-type <closure-operator>
  (make-closure-operator close lattice)
  closure-operator?
  (close   closure-close-fn)
  (lattice closure-lattice))

;; -- Core operations -------------------------------------

(define (closure-close C a)
  "Apply closure operator C to element A.\nReturns cl(A), which is always >= A in the underlying lattice.\n\nExamples:\n  ;; On powerset-lattice {1,2,3}, closure that adds all elements\n  ;; whenever 1 is present:\n  ;; (closure-close C '(1))  => (1 2 3)\n  ;; (closure-close C '(2))  => (2)\n\nParameters:\n  C : any\n  a : any\nReturns: any\nCategory: algebra\nKeywords: closure, apply, close, extensive, idempotent\n\nSee also: `closure-closed?', `make-closure-operator'."
  ((closure-close-fn C) a))

(define (closure-closed? C a)
  "Test whether A is a fixed point of closure operator C.\nAn element is closed when cl(A) = A in the underlying lattice.\n\nExamples:\n  ;; (closure-closed? C '(1 2 3))  => #t  ; already closed\n  ;; (closure-closed? C '(1))      => #f  ; cl({1}) = {1,2,3}\n\nParameters:\n  C : any\n  a : any\nReturns: boolean\nCategory: algebra\nKeywords: fixed point, closed element, idempotent, stable\n\nSee also: `closure-close', `closed-elements'."
  (let ((L (closure-lattice C)))
    (lattice-equal? L (closure-close C a) a)))

;; -- Closed elements -------------------------------------

(define (closed-elements C samples)
  "Filter SAMPLES to those that are fixed points of closure operator C.\nReturns the sublist of elements A where cl(A) = A.\n\nExamples:\n  ;; (closed-elements C '(() (1) (2) (1 2 3)))\n  ;; => (() (2) (1 2 3))  ; only these are fixed points\n\nParameters:\n  C : any\n  samples : list\nReturns: list\nCategory: algebra\nKeywords: fixed points, closed set, filter, stable elements\n\nSee also: `closure-closed?', `closure->closed-lattice'."
  (let loop ((xs samples) (acc '()))
    (if (null? xs)
        (reverse acc)
        (loop (cdr xs)
              (if (closure-closed? C (car xs))
                  (cons (car xs) acc)
                  acc)))))

;; -- Closed lattice --------------------------------------

(define (closure->closed-lattice C samples)
  "Construct the lattice of closed elements from closure operator C.\nThe closed elements (fixed points of C) form a lattice where:\n  - join is cl(join_L(a, b)) -- lattice join composed with closure\n  - meet is inherited from the underlying lattice\n  - bottom is cl(bottom_L)\n  - top is cl(top_L)\n  - leq is inherited from the underlying lattice\nThe join of two closed elements is not necessarily closed under\nthe underlying lattice join (Moore family property), so closure\nmust be applied.  Meet of closed elements IS always closed.\nSAMPLES is not used in the construction but documents the\nintended domain; the lattice operations work on any closed elements.\n\nExamples:\n  ;; (let ((CL (closure->closed-lattice C '(() (2) (1 2 3)))))\n  ;;   (lattice? CL))  => #t\n\nParameters:\n  C : any\n  samples : list\nReturns: any\nCategory: algebra\nKeywords: closed lattice, fixed point lattice, sublattice, Moore family\n\nSee also: `closed-elements', `closure-close'."
  (let ((L (closure-lattice C)))
    (make-lattice
      ;; join: cl(join_L(a, b)) — join of closed elements may not be closed
      (lambda (a b)
        (closure-close C (lattice-join L a b)))
      ;; meet: inherited from L — meet of closed elements is always closed
      (lambda (a b)
        (lattice-meet L a b))
      ;; bottom: cl(bottom_L)
      (closure-close C (lattice-bottom L))
      ;; top: cl(top_L)
      (closure-close C (lattice-top L))
      ;; leq: inherited from L
      (lambda (a b)
        (lattice-leq? L a b)))))

;; -- Downward closure operator ---------------------------

(define (downward-closure-operator po universe)
  "Construct a downward closure operator on the powerset lattice of UNIVERSE.\nGiven a set S, cl(S) = S union {y in UNIVERSE : exists x in S, y <= x under PO}.\nThis adds all elements below existing elements according to the\npartial order, forming a downward-closed (lower) set.\n\nExamples:\n  ;; With <= on integers:\n  ;; (closure-close C '(3))  => (1 2 3)\n  ;; (closure-close C '(1))  => (1)\n\nParameters:\n  po : any\n  universe : list\nReturns: any\nCategory: algebra\nKeywords: downward closure, lower set, downset, order ideal, principal ideal\n\nSee also: `make-closure-operator', `powerset-lattice'."
  (define (downward-close s)
    ;; cl(S) = S union {y in universe : exists x in S, po-leq? po y x}
    (let loop ((us universe) (result s))
      (if (null? us)
          result
          (let ((y (car us)))
            (if (member y result)
                (loop (cdr us) result)
                ;; Check if any x in s has y <= x
                (let check ((xs s))
                  (cond ((null? xs)
                         (loop (cdr us) result))
                        ((po-leq? po y (car xs))
                         (loop (cdr us) (cons y result)))
                        (else
                         (check (cdr xs))))))))))
  (make-closure-operator
    downward-close
    (powerset-lattice universe)))

;; -- Validation ------------------------------------------

(define (validate-closure-operator C samples)
  "Spot-check that C satisfies the closure operator laws on SAMPLES.\nTests extensiveness (a <= cl(a)), monotonicity (a <= b => cl(a) <= cl(b)),\nand idempotence (cl(cl(a)) = cl(a)) for all elements and pairs\nin SAMPLES.  Returns #t if all laws hold, or a list of\n(violation-type element ...) entries describing failures.\n\nExamples:\n  ;; (validate-closure-operator C '(() (1) (2) (1 2 3)))  => #t\n\nParameters:\n  C : any\n  samples : list\nReturns: any\nCategory: algebra\nKeywords: extensive, monotone, idempotent, law checking, validation\n\nSee also: `make-closure-operator', `closure-close'."
  (let ((violations '())
        (L (closure-lattice C)))
    (define (fail! type . args)
      (set! violations (cons (cons type args) violations)))
    (for-each
      (lambda (a)
        (let ((cl-a (closure-close C a)))
          ;; Extensive: a <= cl(a)
          (unless (lattice-leq? L a cl-a)
            (fail! 'extensive a))
          ;; Idempotent: cl(cl(a)) = cl(a)
          (unless (lattice-equal? L (closure-close C cl-a) cl-a)
            (fail! 'idempotent a))
          ;; Monotonicity: for all b, a <= b => cl(a) <= cl(b)
          (for-each
            (lambda (b)
              (when (lattice-leq? L a b)
                (unless (lattice-leq? L cl-a (closure-close C b))
                  (fail! 'monotone a b))))
            samples)))
      samples)
    (if (null? violations) #t (reverse violations))))

;; -- with-closure macro ----------------------------------

(define-syntax with-closure
  (syntax-rules ()
    ((with-closure C (close lattice) body ...)
     (let ((tmp C))
       (let ((close   (lambda (a) (closure-close tmp a)))
             (lattice (closure-lattice tmp)))
         body ...)))))
