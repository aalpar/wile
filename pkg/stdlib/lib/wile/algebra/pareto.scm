;;; (wile algebra pareto) — Pareto Dominance
;;;
;;; Multi-objective dominance testing and Pareto frontier computation.
;;; Given a set of candidates each scored on multiple factors, identifies
;;; the non-dominated frontier: candidates where no other candidate is
;;; better on every factor simultaneously.
;;;
;;; Factors are alists ((key . val) ...) with mixed boolean/numeric values.
;;; Boolean ordering: #f < #t. Numeric ordering: standard <=.
;;;
;;; Each factor also carries a DIRECTION of improvement, 'up (higher is
;;; better, the default) or 'down. Supplying it is what lets a benefit count
;;; and a parameter count sit in one alist without one of them being ranked
;;; backwards. See "Factor direction" below.

;;; ── Local utilities ─────────────────────────────────────

(define (filter-map f lst)
  (let loop ((xs lst) (acc '()))
    (if (null? xs) (reverse acc)
      (let ((v (f (car xs))))
        (loop (cdr xs) (if v (cons v acc) acc))))))

(define (member? x lst) (and (member x lst) #t))

(define (filter pred lst)
  (let loop ((xs lst) (acc '()))
    (if (null? xs) (reverse acc)
      (loop (cdr xs)
            (if (pred (car xs)) (cons (car xs) acc) acc)))))

;;; ── Factor comparison ───────────────────────────────────

;; Compare two factor values with mixed-type ordering.
;; Booleans: #f < #t (false is "worse"). Numbers: standard <=.
(define (factor-leq? a b)
  "Non-strict mixed-type factor comparison.\nBooleans: #f <= #t. Numbers: standard <=.\n\nParameters:\n  a : boolean or number\n  b : boolean or number\nReturns: boolean\nCategory: algebra"
  (cond ((boolean? a) (or (not a) b))
        ((boolean? b) (and a b))
        (else (<= a b))))

;; Strict version: a < b.
(define (factor-less? a b)
  "Strict mixed-type factor comparison.\n\nParameters:\n  a : boolean or number\n  b : boolean or number\nReturns: boolean\nCategory: algebra"
  (and (factor-leq? a b) (not (equal? a b))))

;;; ── Factor direction ────────────────────────────────────
;;;
;;; factor-leq? orders values; it cannot know which end of that order is an
;;; IMPROVEMENT. For a similarity or a benefit count, higher is better; for a
;;; parameter count, a coupling count, or an edge count, lower is. Both are
;;; numbers, so nothing about the value distinguishes them.
;;;
;;; Before directions existed the only way to rank a lower-is-better axis was
;;; for the caller to negate it before handing it over. That convention lives
;;; nowhere the code can check: a producer that reports an honest positive
;;; count gets silently ranked backwards, and the output still looks like a
;;; frontier. Naming the direction moves the fact from the caller's memory to
;;; the call.

;; A direction spec is either a list of factor-name symbols (the historical
;; documentation-only form, meaning every factor is 'up) or an alist of
;; (name . 'up|'down). normalize-directions collapses the first to '().
(define (normalize-directions spec)
  (cond ((not (pair? spec)) '())
        ((pair? (car spec)) spec)
        (else '())))

;; Direction of improvement for KEY. Absent means 'up, which is what every
;; caller got before this existed. A value that is neither 'up nor 'down is an
;; error rather than a default: a misspelled direction silently meaning "up" is
;; exactly the failure this facility removes.
(define (factor-direction directions key)
  (let ((entry (assq key directions)))
    (if (not entry)
      'up
      (let ((d (cdr entry)))
        (if (memq d '(up down))
          d
          (error "pareto: factor direction must be up or down" key d))))))

;;; ── Pareto dominance ────────────────────────────────────

;; X dominates Y iff X is at least as good as Y on every factor and strictly
;; better on at least one, where "better" is the factor's own direction.
;; Factors are alists ((key . val) ...). Keys in X must appear in Y.
;; DIRECTIONS is optional and takes either direction-spec form; omitted, every
;; factor is 'up and this is the original higher-is-better dominance.
(define (dominates? factors-x factors-y . opt)
  "Test Pareto dominance: X dominates Y iff X is at least as good as Y on\nevery factor and strictly better on at least one. Direction per factor comes\nfrom DIRECTIONS ('up, the default, or 'down).\n\nParameters:\n  factors-x : alist of (key . value) pairs\n  factors-y : alist of (key . value) pairs\n  directions : optional alist of (key . up|down), or a list of names\nReturns: boolean\nCategory: algebra"
  (let ((directions (normalize-directions (if (null? opt) '() (car opt)))))
    (let loop ((fx factors-x) (any-strict #f))
      (if (null? fx)
        any-strict
        (let* ((key (car (car fx)))
               (vx (cdr (car fx)))
               (vy (cdr (assoc key factors-y)))
               (down (eq? (factor-direction directions key) 'down))
               ;; "x at least as good as y" is vy <= vx going up, vx <= vy
               ;; going down. The strict test is the same swap.
               (at-least (if down (factor-leq? vx vy) (factor-leq? vy vx)))
               (strictly (if down (factor-less? vx vy) (factor-less? vy vx))))
          (if at-least
            (loop (cdr fx) (or any-strict strictly))
            #f))))))

;; Compute Pareto frontier and dominated groups.
;; candidates: list of (id factors-alist) pairs.
;; factors: either a list of factor-name symbols (documentation only, every
;;   factor higher-is-better) or an alist of (name . 'up|'down), in which case
;;   it is load-bearing and states each axis's direction of improvement.
;; Returns: ((frontier id ...) (dominated (dominator-id dominated-id ...) ...))
(define (pareto-frontier candidates factors)
  "Compute Pareto frontier and dominated groups from scored candidates.\n\nParameters:\n  candidates : list of (id factors-alist) pairs\n  factors : list of names (documentation only) or alist of (name . up|down)\nReturns: ((frontier id ...) (dominated (dominator-id dominated-id ...) ...))\nCategory: algebra"
  (let* ((directions (normalize-directions factors))
         (ids (map car candidates))
         (factors-of (lambda (id)
                       (cadr (let loop ((cs candidates))
                               (cond ((null? cs) #f)
                                     ((equal? (car (car cs)) id) (car cs))
                                     (else (loop (cdr cs))))))))
         (frontier-ids
           (filter-map
             (lambda (c)
               (let ((c-id (car c))
                     (c-factors (cadr c)))
                 (let dominated? ((rest candidates))
                   (cond ((null? rest) c-id)
                         ((equal? (car (car rest)) c-id) (dominated? (cdr rest)))
                         ((dominates? (cadr (car rest)) c-factors directions) #f)
                         (else (dominated? (cdr rest)))))))
             candidates))
         (dominated-ids (filter (lambda (id) (not (member? id frontier-ids))) ids))
         (dom-groups
           (filter-map
             (lambda (fid)
               (let* ((fid-factors (factors-of fid))
                      (doms (filter
                              (lambda (did)
                                (dominates? fid-factors (factors-of did) directions))
                              dominated-ids)))
                 (if (null? doms) #f
                   (cons fid doms))))
             frontier-ids)))
    (list (cons 'frontier frontier-ids)
          (cons 'dominated dom-groups))))
