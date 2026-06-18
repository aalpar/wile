;;; (wile algebra interval) — Interval arithmetic and interval lattice
;;;
;;; Infinity-aware arithmetic over intervals represented as (lo . hi) pairs.
;;; Symbols `neg-inf` and `pos-inf` represent negative and positive infinity.
;;; The interval lattice orders by containment: (a . b) <= (c . d) iff c <= a
;;; and b <= d. Bottom is the empty interval `interval-bot`; top is
;;; (neg-inf . pos-inf).
;;;
;;; Interval operations (add, sub, mul) follow standard interval arithmetic
;;; rules. Multiplication uses the four-corner product.
;;;
;;; References:
;;;   Moore, R.E. (1966) Interval Analysis.
;;;   Cousot & Cousot (1977) Abstract Interpretation.

;; ─── Infinity-aware comparison ──────────────────

(define (inf<= a b)
  "Infinity-aware less-than-or-equal.\nSymbols `neg-inf` and `pos-inf` represent infinities.\nneg-inf <= everything, everything <= pos-inf.\n\nParameters:\n  a : number-or-symbol\n  b : number-or-symbol\nReturns: boolean\nCategory: algebra"
  (cond ((eq? a 'neg-inf) #t)
        ((eq? b 'pos-inf) #t)
        ((eq? b 'neg-inf) #f)
        ((eq? a 'pos-inf) #f)
        (else (<= a b))))

(define (inf-min a b)
  "Infinity-aware minimum of two values.\n\nParameters:\n  a : number-or-symbol\n  b : number-or-symbol\nReturns: number-or-symbol\nCategory: algebra"
  (if (inf<= a b) a b))

(define (inf-max a b)
  "Infinity-aware maximum of two values.\n\nParameters:\n  a : number-or-symbol\n  b : number-or-symbol\nReturns: number-or-symbol\nCategory: algebra"
  (if (inf<= a b) b a))

;; ─── Infinity-aware arithmetic ──────────────────

(define (inf+ a b)
  "Infinity-aware addition.\npos-inf + neg-inf = pos-inf (conservative widening choice).\n\nParameters:\n  a : number-or-symbol\n  b : number-or-symbol\nReturns: number-or-symbol\nCategory: algebra"
  (cond ((or (and (eq? a 'pos-inf) (eq? b 'neg-inf))
             (and (eq? a 'neg-inf) (eq? b 'pos-inf)))
         'pos-inf)
        ((or (eq? a 'pos-inf) (eq? b 'pos-inf)) 'pos-inf)
        ((or (eq? a 'neg-inf) (eq? b 'neg-inf)) 'neg-inf)
        (else (+ a b))))

(define (inf- a b)
  "Infinity-aware subtraction.\npos-inf - pos-inf = pos-inf (conservative widening choice).\n\nParameters:\n  a : number-or-symbol\n  b : number-or-symbol\nReturns: number-or-symbol\nCategory: algebra"
  (cond ((or (and (eq? a 'pos-inf) (eq? b 'pos-inf))
             (and (eq? a 'neg-inf) (eq? b 'neg-inf)))
         'pos-inf)
        ((eq? a 'pos-inf) 'pos-inf)
        ((eq? a 'neg-inf) 'neg-inf)
        ((eq? b 'pos-inf) 'neg-inf)
        ((eq? b 'neg-inf) 'pos-inf)
        (else (- a b))))

(define (inf* a b)
  "Infinity-aware multiplication.\n0 * infinity = 0 (absorbing element convention).\n\nParameters:\n  a : number-or-symbol\n  b : number-or-symbol\nReturns: number-or-symbol\nCategory: algebra"
  (cond ((or (and (eqv? a 0) (or (eq? b 'pos-inf) (eq? b 'neg-inf)))
             (and (eqv? b 0) (or (eq? a 'pos-inf) (eq? a 'neg-inf))))
         0)
        ((and (eq? a 'pos-inf) (eq? b 'pos-inf)) 'pos-inf)
        ((and (eq? a 'neg-inf) (eq? b 'neg-inf)) 'pos-inf)
        ((or (and (eq? a 'pos-inf) (eq? b 'neg-inf))
             (and (eq? a 'neg-inf) (eq? b 'pos-inf)))
         'neg-inf)
        ((eq? a 'pos-inf) (if (< b 0) 'neg-inf 'pos-inf))
        ((eq? a 'neg-inf) (if (< b 0) 'pos-inf 'neg-inf))
        ((eq? b 'pos-inf) (if (< a 0) 'neg-inf 'pos-inf))
        ((eq? b 'neg-inf) (if (< a 0) 'pos-inf 'neg-inf))
        (else (* a b))))

;; ─── Interval lattice ───────────────────────────

(define (interval-lattice)
  "Construct the interval lattice with infinity-aware arithmetic.\nElements are (lo . hi) pairs, interval-bot, or (neg-inf . pos-inf) as top.\nJoin widens to encompass both intervals; meet narrows to intersection.\nLeq tests containment.\n\nReturns: lattice\nCategory: algebra\n\nSee also: `interval-add', `interval-sub', `interval-mul'."
  (make-lattice
    ;; join: widen to encompass both
    (lambda (a b)
      (cond ((eq? a 'interval-bot) b)
            ((eq? b 'interval-bot) a)
            (else (cons (inf-min (car a) (car b))
                        (inf-max (cdr a) (cdr b))))))
    ;; meet: narrow to intersection
    (lambda (a b)
      (cond ((eq? a 'interval-bot) 'interval-bot)
            ((eq? b 'interval-bot) 'interval-bot)
            (else (let ((lo (inf-max (car a) (car b)))
                        (hi (inf-min (cdr a) (cdr b))))
                    (if (inf<= lo hi)
                        (cons lo hi)
                        'interval-bot)))))
    'interval-bot
    (cons 'neg-inf 'pos-inf)
    ;; leq: a contained in b
    (lambda (a b)
      (cond ((eq? a 'interval-bot) #t)
            ((eq? b 'interval-bot) #f)
            (else (and (inf<= (car b) (car a))
                       (inf<= (cdr a) (cdr b))))))))

;; ─── Interval arithmetic ────────────────────────

;; interval-bot is absorbing for arithmetic: an unreachable operand yields an
;; unreachable result, matching sign-binop's bottom handling. This shared
;; predicate keeps the bottom-detection logic in one place while each public
;; operator retains its own docstring (the doc system reads define-form docs).
(define (%interval-bottom-operand? a b)
  (or (eq? a 'interval-bot) (eq? b 'interval-bot)))

(define (interval-add a b)
  "Add two intervals: [a.lo+b.lo, a.hi+b.hi].\nReturns interval-bot if either operand is interval-bot (absorbing).\n\nParameters:\n  a : pair-or-symbol\n  b : pair-or-symbol\nReturns: pair-or-symbol\nCategory: algebra"
  (if (%interval-bottom-operand? a b) 'interval-bot
      (cons (inf+ (car a) (car b)) (inf+ (cdr a) (cdr b)))))

(define (interval-sub a b)
  "Subtract two intervals: [a.lo-b.hi, a.hi-b.lo].\nReturns interval-bot if either operand is interval-bot (absorbing).\n\nParameters:\n  a : pair-or-symbol\n  b : pair-or-symbol\nReturns: pair-or-symbol\nCategory: algebra"
  (if (%interval-bottom-operand? a b) 'interval-bot
      (cons (inf- (car a) (cdr b)) (inf- (cdr a) (car b)))))

(define (interval-mul a b)
  "Multiply two intervals using four-corner product.\nComputes all products of endpoint combinations and takes min/max.\nReturns interval-bot if either operand is interval-bot (absorbing).\n\nParameters:\n  a : pair-or-symbol\n  b : pair-or-symbol\nReturns: pair-or-symbol\nCategory: algebra"
  (if (%interval-bottom-operand? a b) 'interval-bot
      (let* ((corners (list (inf* (car a) (car b))
                            (inf* (car a) (cdr b))
                            (inf* (cdr a) (car b))
                            (inf* (cdr a) (cdr b))))
             (lo (let loop ((cs (cdr corners)) (m (car corners)))
                   (if (null? cs) m (loop (cdr cs) (inf-min m (car cs))))))
             (hi (let loop ((cs (cdr corners)) (m (car corners)))
                   (if (null? cs) m (loop (cdr cs) (inf-max m (car cs)))))))
        (cons lo hi))))

;; ─── Abstraction and widening ───────────────────

(define (abstract-interval n)
  "Abstract an integer N into the interval domain as the point interval [n,n].\nThe interval analog of abstract-sign.\n\nExamples:\n  (abstract-interval 5)   => (5 . 5)\n  (abstract-interval -2)  => (-2 . -2)\n\nParameters:\n  n : integer\nReturns: pair\nCategory: algebra\nKeywords: abstraction, interval, abstract interpretation, point interval\n\nSee also: `interval-widen', `interval-lattice'."
  (cons n n))

(define (interval-widen cur next)
  "Interval widening operator: keep a bound if stable, else jump to infinity.\nWidening forces ascending chains finite so fixpoint iteration over the\ninfinite-height interval lattice terminates (Cousot & Cousot 1977). Returns\nan interval at least as large as the join of CUR and NEXT. interval-bot is\nabsorbed in either position. A lower bound that does not decrease is kept,\nelse it drops to neg-inf; an upper bound that does not increase is kept,\nelse it rises to pos-inf.\n\nExamples:\n  (interval-widen '(0 . 0) '(0 . 1))      => (0 . pos-inf)\n  (interval-widen '(0 . 5) '(0 . 5))      => (0 . 5)\n  (interval-widen 'interval-bot '(0 . 1)) => (0 . 1)\n\nParameters:\n  cur : pair-or-symbol\n  next : pair-or-symbol\nReturns: pair-or-symbol\nCategory: algebra\nKeywords: widening, abstract interpretation, termination, infinite chains, acceleration\n\nSee also: `abstract-interval', `interval-lattice', `fixpoint/widen'."
  (cond ((eq? cur 'interval-bot) next)
        ((eq? next 'interval-bot) cur)
        (else (cons (if (inf<= (car cur) (car next)) (car cur) 'neg-inf)
                    (if (inf<= (cdr next) (cdr cur)) (cdr cur) 'pos-inf)))))

;; ─── Galois connection: P(Z) <-> interval ───────

;; Concrete domain: subsets of the integers. Bounded sets are finite int
;; lists; unbounded extents use *typed* sentinels so gamma is invertible —
;; alpha(gamma(iv)) = iv on the FULL interval lattice, including the one-sided
;; intervals widening produces (e.g. (0 . pos-inf)). Sentinels:
;;   (all-ge . n) = {x : x >= n}     <- gamma of (n . pos-inf)
;;   (all-le . n) = {x : x <= n}     <- gamma of (neg-inf . n)
;;   all-int      = all integers      <- gamma of (neg-inf . pos-inf)
;;   ()           = empty set         <- gamma of interval-bot
;; This mirrors the typed-sentinel design of sign-galois-connection so the
;; soundness certificate covers what an interval dataflow actually emits.

(define (%int-range a b)
  ;; Inclusive integer list [a..b]; assumes finite a <= b.
  (let loop ((i b) (acc '()))
    (if (< i a) acc (loop (- i 1) (cons i acc)))))

(define (%iv-sentinel? x)
  (or (eq? x 'all-int)
      (and (pair? x) (memq (car x) '(all-ge all-le)))))

(define (%iv-every pred xs)
  (let loop ((xs xs))
    (cond ((null? xs) #t) ((pred (car xs)) (loop (cdr xs))) (else #f))))

(define (%interval-subset-leq a b)
  ;; Containment on the concrete domain (finite sets + typed sentinels),
  ;; a genuine partial order: reflexive, with () below everything.
  (cond ((equal? a b) #t)                                  ; reflexivity
        ((null? a) #t)                                      ; empty subset of all
        ((eq? b 'all-int) #t)                               ; all-int is top
        ((%iv-sentinel? a)                                  ; sentinel a vs (non-equal) b
         (cond ((and (pair? a) (pair? b) (eq? (car a) 'all-ge) (eq? (car b) 'all-ge))
                (>= (cdr a) (cdr b)))                       ; {>=m} subset {>=n} iff m>=n
               ((and (pair? a) (pair? b) (eq? (car a) 'all-le) (eq? (car b) 'all-le))
                (<= (cdr a) (cdr b)))
               (else #f)))
        ((and (pair? b) (eq? (car b) 'all-ge))             ; finite a vs {x>=n}
         (%iv-every (lambda (x) (>= x (cdr b))) a))
        ((and (pair? b) (eq? (car b) 'all-le))             ; finite a vs {x<=n}
         (%iv-every (lambda (x) (<= x (cdr b))) a))
        (else                                               ; both finite sets
         (%iv-every (lambda (x) (member x b)) a))))

(define (interval-galois-connection)
  "Construct the Galois connection between integer sets and the interval lattice.\nConcrete domain: subsets of the integers, ordered by containment. Bounded sets\nare finite lists; unbounded extents use typed sentinels ((all-ge . n),\n(all-le . n), all-int) so the connection is a closed loop on the FULL interval\nlattice. alpha(S) = [min S, max S] (interval-bot for the empty set); gamma maps\nbounded [a,b] to {a..b}, (n . pos-inf) to (all-ge . n), (neg-inf . n) to\n(all-le . n), and top to all-int. This is the soundness certificate for an\ninterval dataflow result — including the one-sided intervals widening produces\n(e.g. (0 . pos-inf)). Passes gc-sound? over the full lattice.\n\nReturns: galois-connection\nCategory: algebra\nKeywords: Galois connection, interval, abstract interpretation, soundness, abstraction\n\nSee also: `make-galois-connection', `gc-sound?', `interval-lattice', `interval-widen'."
  (make-galois-connection
    ;; alpha: concrete -> interval. Inverts the gamma sentinels.
    (lambda (c)
      (cond ((eq? c 'all-int) (cons 'neg-inf 'pos-inf))
            ((and (pair? c) (eq? (car c) 'all-ge)) (cons (cdr c) 'pos-inf))
            ((and (pair? c) (eq? (car c) 'all-le)) (cons 'neg-inf (cdr c)))
            ((null? c) 'interval-bot)
            (else
             (let loop ((xs (cdr c)) (lo (car c)) (hi (car c)))
               (if (null? xs) (cons lo hi)
                   (loop (cdr xs) (min lo (car xs)) (max hi (car xs))))))))
    ;; gamma: interval -> concrete. Typed sentinels for unbounded extents.
    (lambda (iv)
      (cond ((eq? iv 'interval-bot) '())
            ((and (eq? (car iv) 'neg-inf) (eq? (cdr iv) 'pos-inf)) 'all-int)
            ((eq? (cdr iv) 'pos-inf) (cons 'all-ge (car iv)))
            ((eq? (car iv) 'neg-inf) (cons 'all-le (cdr iv)))
            (else (%int-range (car iv) (cdr iv)))))
    (make-partial-order %interval-subset-leq)
    (interval-lattice)))
