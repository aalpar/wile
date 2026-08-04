;;; (wile algebra incidence) — Incidence algebras and Möbius functions
;;;
;;; Rota (1964). Given a locally-finite poset P and a ring R, the
;;; incidence algebra I(P,R) is the set of functions
;;;     f : {(x,y) : x ≤ y} → R
;;; under the convolution
;;;     (f*g)(x,y) = Σ_{x ≤ z ≤ y} f(x,z) · g(z,y).
;;; The zeta function ζ(x,y) = 1 iff x ≤ y is the identity-element-
;;; analog of the algebra; its multiplicative inverse is the Möbius
;;; function μ, defined recursively by
;;;     μ(x,x) = 1
;;;     μ(x,y) = -Σ_{x ≤ z < y} μ(x,z)    for x < y
;;;     μ(x,y) = 0                          when ¬(x ≤ y).

;; ─── Locally-finite poset ────────────────────────────────────────
;;
;; The <locally-finite-poset> record carries the two mandatory fields
;; (leq?, interval) and one optional metadata field (elements) used by
;; §5.5's Birkhoff reconstruction. Consumers built through
;; finite-set->locally-finite-poset automatically gain the element list;
;; hand-rolled callers opt in by passing (cons 'elements LIST) in the
;; options alist. Absent elements defaults to #f.

(define-record-type <locally-finite-poset>
  (%make-locally-finite-poset leq? interval elements)
  locally-finite-poset?
  (leq?     lf-poset-leq?)
  (interval lf-poset-interval)
  (elements lf-poset-elements))

(define (make-locally-finite-poset leq? interval . opts)
  "Construct a locally-finite poset from LEQ? predicate and INTERVAL procedure.\nINTERVAL takes two elements X, Y and returns the list of Z with X <= Z <= Y,\nthe empty list when ¬(X <= Y), and the singleton (X) when X = Y.\n\nOptional trailing alist entries:\n  (elements . LIST) — full enumeration of the poset (required by\n                      birkhoff-reconstruction; populated automatically\n                      by finite-set->locally-finite-poset).\n\nExamples:\n  (make-locally-finite-poset\n    (lambda (a b) (<= a b))\n    (lambda (x y) (iota (+ 1 (- y x)) x)))\n\nParameters:\n  leq? : procedure\n  interval : procedure\n  opts : alist\nReturns: locally-finite-poset\nCategory: algebra\nKeywords: poset, locally finite, interval, construction\n\nSee also: `finite-set->locally-finite-poset', `lf-poset-elements'."
  (validate-opts-keys "make-locally-finite-poset" opts '(elements))
  (%make-locally-finite-poset leq? interval
                              (assv-or opts 'elements #f)))

(define (finite-set->locally-finite-poset leq? elements)
  "Build a locally-finite poset from a predicate LEQ? and an explicit\nelement list ELEMENTS. The interval-enumeration procedure scans\nthe full element set by the two-sided bound. Use this when the\nunderlying poset is bounded and enumerable; prefer a direct\ninterval procedure when the universe is infinite.\n\nThe resulting poset carries ELEMENTS in its elements field; access\nvia `lf-poset-elements'. This is the canonical way to construct a\nposet consumable by `birkhoff-reconstruction'.\n\nExamples:\n  (define P\n    (finite-set->locally-finite-poset\n      (lambda (a b) (zero? (modulo b a)))\n      '(1 2 3 4 5 6)))\n  ((lf-poset-interval P) 1 6)  => (1 2 3 6)\n  (lf-poset-elements P)        => (1 2 3 4 5 6)\n\nParameters:\n  leq? : procedure\n  elements : list\nReturns: locally-finite-poset\nCategory: algebra\nKeywords: poset, finite, interval, enumeration, construction"
  (make-locally-finite-poset
    leq?
    (lambda (x y)
      (if (not (leq? x y))
          '()
          (let loop ((es elements) (acc '()))
            (cond
              ((null? es) (reverse acc))
              ((and (leq? x (car es)) (leq? (car es) y))
               (loop (cdr es) (cons (car es) acc)))
              (else
               (loop (cdr es) acc))))))
    (cons 'elements elements)))

;; ─── Incidence algebra ───────────────────────────────────────────

(define-record-type <incidence-algebra>
  (make-incidence-algebra* poset ring mu-cache)
  incidence-algebra?
  (poset    incidence-algebra-poset)
  (ring     incidence-algebra-ring)
  ;; mu-cache is an alist: ((x . y) . μ(x,y)) entries.
  ;; Mutable field so compute-mu can cons on a hit.
  ;; Alist chosen over hashtable because Wile hashtables required atomic keys.
  ;; That constraint is RETIRED — make-equal-hashtable now takes a (cons x y)
  ;; key directly and compares it with the same equal? this assoc uses, so it
  ;; would be a drop-in with O(1) lookup instead of O(n). Left as an alist here
  ;; only because nothing has measured the cache at a size where it matters;
  ;; see TODO.md.
  (mu-cache incidence-algebra-mu-cache
            set-incidence-algebra-mu-cache!))

(define make-incidence-algebra
  (case-lambda
    ((poset)
     (make-incidence-algebra* poset (integer-ring) '()))
    ((poset ring)
     (make-incidence-algebra* poset ring '()))))

;; ─── ζ (zeta) ────────────────────────────────────────────────────

(define (zeta-function IA)
  "Return the zeta function of incidence algebra IA as a procedure.\nζ(x,y) = ring-one when x <= y in the underlying poset, else\nring-zero. ζ is the identity-up-to-convolution element of the\nincidence algebra; its convolutional inverse is the Möbius function.\n\nExamples:\n  (define IA (make-incidence-algebra\n               (finite-set->locally-finite-poset <= '(1 2 3))))\n  ((zeta-function IA) 1 2)  => 1\n  ((zeta-function IA) 2 1)  => 0\n\nParameters:\n  ia : incidence-algebra\nReturns: procedure\nCategory: algebra\nKeywords: zeta, incidence, poset, indicator, order"
  (let ((R     (incidence-algebra-ring IA))
        (poset (incidence-algebra-poset IA)))
    (let ((leq? (lf-poset-leq? poset)))
      (lambda (x y)
        (if (leq? x y)
            (ring-one R)
            (ring-zero R))))))

;; ─── μ (Möbius) ──────────────────────────────────────────────────

(define (mobius-function IA)
  "Return the Möbius function of incidence algebra IA as a procedure.\nμ is the convolutional inverse of ζ, defined recursively:\n  μ(x,x) = 1\n  μ(x,y) = -Σ_{x <= z < y} μ(x,z)   for x < y\n  μ(x,y) = 0                          when x is not <= y\nValues are lazily computed and memoized in IA's internal cache;\nrepeated calls with the same (x,y) are O(1) after the first.\n\nExamples:\n  (define IA (make-incidence-algebra\n               (finite-set->locally-finite-poset <= '(1 2 3 4))))\n  ((mobius-function IA) 1 1)  => 1\n  ((mobius-function IA) 1 2)  => -1\n  ((mobius-function IA) 1 3)  => 0\n\nParameters:\n  ia : incidence-algebra\nReturns: procedure\nCategory: algebra\nKeywords: mobius, möbius, incidence, inversion, Rota, poset"
  (lambda (x y) (compute-mu IA x y)))

(define (compute-mu IA x y)
  ;; Alist-based memoization, keyed on (cons x y). `assoc` uses equal? so any
  ;; R7RS-structural element type works — numbers, strings, symbols, lists,
  ;; vectors, nested pairs. An equal?-keyed hashtable would accept exactly the
  ;; same keys now (it did not when this was written) and lookup in O(1).
  ;;
  ;; Recursive calls from compute-mu-uncached must come back through
  ;; *this* wrapper (not compute-mu-uncached directly) or memoization
  ;; is defeated and μ degrades from O(n³) to exponential.
  (let* ((cache (incidence-algebra-mu-cache IA))
         (hit   (assoc (cons x y) cache)))
    (if hit
        (cdr hit)
        (let ((result (compute-mu-uncached IA x y)))
          (set-incidence-algebra-mu-cache!
            IA
            (cons (cons (cons x y) result) cache))
          result))))

(define (compute-mu-uncached IA x y)
  ;; Rota's recursive definition:
  ;;   μ(x,y) = 0            when ¬(x ≤ y)      ← check leq? first
  ;;   μ(x,x) = 1            base case
  ;;   μ(x,y) = -Σ μ(x,z) over z in [x,y), z ≠ y
  ;;
  ;; Guard ordering: ¬leq? first so a non-reflexive predicate (illegal
  ;; input, but we don't validate) doesn't silently short-circuit via
  ;; equal? into the wrong branch. The "proper sub-interval" sum and
  ;; the z ≠ y filter are fused into one loop — no intermediate list.
  (let ((R     (incidence-algebra-ring IA))
        (poset (incidence-algebra-poset IA)))
    (let ((leq? (lf-poset-leq? poset)))
      (cond
        ((not (leq? x y))
         (ring-zero R))
        ((equal? x y)
         (ring-one R))
        (else
         (let ((iv ((lf-poset-interval poset) x y)))
           (let loop ((zs iv) (acc (ring-zero R)))
             (cond
               ((null? zs)
                (ring-negate R acc))
               ((equal? (car zs) y)
                (loop (cdr zs) acc))
               (else
                (loop (cdr zs)
                      (ring-plus R acc (compute-mu IA x (car zs)))))))))))))

;; ─── Convolution ─────────────────────────────────────────────────

(define (incidence-convolve IA f g)
  "Convolve two incidence-algebra elements F and G over IA.\nReturns a new procedure (lambda (x y) -> R) computing\n  (f*g)(x,y) = Σ_{x <= z <= y} f(x,z) · g(z,y).\nWhen x is not <= y the result is ring-zero. F and G must be\nprocedures of two arguments returning ring elements.\n\nExamples:\n  (define IA (make-incidence-algebra\n               (finite-set->locally-finite-poset <= '(1 2 3))))\n  (define z (zeta-function IA))\n  (define m (mobius-function IA))\n  ((incidence-convolve IA z m) 1 3)  => 0   ; ζ * μ = δ (Kronecker)\n  ((incidence-convolve IA z m) 1 1)  => 1\n\nParameters:\n  ia : incidence-algebra\n  f : procedure\n  g : procedure\nReturns: procedure\nCategory: algebra\nKeywords: convolution, incidence, Rota, Dirichlet, algebra"
  (let ((R     (incidence-algebra-ring IA))
        (poset (incidence-algebra-poset IA)))
    (let ((leq?     (lf-poset-leq? poset))
          (interval (lf-poset-interval poset)))
      (lambda (x y)
        (if (not (leq? x y))
            (ring-zero R)
            (let loop ((zs (interval x y)) (acc (ring-zero R)))
              (if (null? zs)
                  acc
                  (loop (cdr zs)
                        (ring-plus R acc
                                   (ring-times R (f x (car zs))
                                                 (g (car zs) y)))))))))))

;; ─── Möbius inversion ────────────────────────────────────────────

(define (mobius-inversion IA g x lower-set)
  "Apply Möbius inversion at X given G and its lower-set.\nIf g(x) = Σ_{y <= x} f(y), recover f(x) = Σ_{y <= x} μ(y,x) · g(y).\nLOWER-SET must enumerate exactly {y ∈ P : y <= x}; the library\ncannot derive it in general (locally-finite posets may have\ninfinite principal ideals). G is a procedure element → R.\n\nExamples:\n  (define IA (make-incidence-algebra\n               (finite-set->locally-finite-poset <= '(1 2 3))))\n  (define f (lambda (y) (* y y)))\n  (define (lower-list upto)\n    (let loop ((ys '(1 2 3)) (acc '()))\n      (cond ((null? ys) (reverse acc))\n            ((<= (car ys) upto)\n             (loop (cdr ys) (cons (car ys) acc)))\n            (else (loop (cdr ys) acc)))))\n  (define (g x)\n    (let loop ((ys (lower-list x)) (s 0))\n      (if (null? ys) s (loop (cdr ys) (+ s (f (car ys)))))))\n  (mobius-inversion IA g 3 (lower-list 3))  => 9\n\nParameters:\n  ia : incidence-algebra\n  g : procedure\n  x : any\n  lower-set : list\nReturns: any\nCategory: algebra\nKeywords: mobius inversion, möbius, incidence, inversion, Rota, poset"
  (let ((R  (incidence-algebra-ring IA))
        (mu (mobius-function IA)))
    (let loop ((ys lower-set) (acc (ring-zero R)))
      (if (null? ys)
          acc
          (loop (cdr ys)
                (ring-plus R acc
                           (ring-times R (mu (car ys) x)
                                         (g (car ys)))))))))
