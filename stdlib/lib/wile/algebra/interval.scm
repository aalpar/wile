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

(define (interval-add a b)
  "Add two intervals: [a.lo+b.lo, a.hi+b.hi].\n\nParameters:\n  a : pair\n  b : pair\nReturns: pair\nCategory: algebra"
  (cons (inf+ (car a) (car b)) (inf+ (cdr a) (cdr b))))

(define (interval-sub a b)
  "Subtract two intervals: [a.lo-b.hi, a.hi-b.lo].\n\nParameters:\n  a : pair\n  b : pair\nReturns: pair\nCategory: algebra"
  (cons (inf- (car a) (cdr b)) (inf- (cdr a) (car b))))

(define (interval-mul a b)
  "Multiply two intervals using four-corner product.\nComputes all products of endpoint combinations and takes min/max.\n\nParameters:\n  a : pair\n  b : pair\nReturns: pair\nCategory: algebra"
  (let* ((corners (list (inf* (car a) (car b))
                        (inf* (car a) (cdr b))
                        (inf* (cdr a) (car b))
                        (inf* (cdr a) (cdr b))))
         (lo (let loop ((cs (cdr corners)) (m (car corners)))
               (if (null? cs) m (loop (cdr cs) (inf-min m (car cs))))))
         (hi (let loop ((cs (cdr corners)) (m (car corners)))
               (if (null? cs) m (loop (cdr cs) (inf-max m (car cs)))))))
    (cons lo hi)))
