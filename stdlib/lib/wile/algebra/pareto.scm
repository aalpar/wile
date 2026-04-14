;;; (wile algebra pareto) — Pareto Dominance
;;;
;;; Multi-objective dominance testing and Pareto frontier computation.
;;; Given a set of candidates each scored on multiple factors, identifies
;;; the non-dominated frontier: candidates where no other candidate is
;;; better on every factor simultaneously.
;;;
;;; Factors are alists ((key . val) ...) with mixed boolean/numeric values.
;;; Boolean ordering: #f < #t. Numeric ordering: standard <=.

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

;;; ── Pareto dominance ────────────────────────────────────

;; X dominates Y iff X >= Y on every factor and X > Y on at least one.
;; Factors are alists ((key . val) ...). Keys in X must appear in Y.
(define (dominates? factors-x factors-y)
  "Test Pareto dominance: X dominates Y iff X >= Y on every factor\nand X > Y on at least one.\n\nParameters:\n  factors-x : alist of (key . value) pairs\n  factors-y : alist of (key . value) pairs\nReturns: boolean\nCategory: algebra"
  (let loop ((fx factors-x) (any-strict #f))
    (if (null? fx)
      any-strict
      (let* ((key (car (car fx)))
             (vx (cdr (car fx)))
             (vy (cdr (assoc key factors-y))))
        (if (factor-leq? vy vx)
          (loop (cdr fx) (or any-strict (factor-less? vy vx)))
          #f)))))

;; Compute Pareto frontier and dominated groups.
;; candidates: list of (id factors-alist) pairs.
;; factor-names: list of factor name symbols (documentation only).
;; Returns: ((frontier id ...) (dominated (dominator-id dominated-id ...) ...))
(define (pareto-frontier candidates factor-names)
  "Compute Pareto frontier and dominated groups from scored candidates.\n\nParameters:\n  candidates : list of (id factors-alist) pairs\n  factor-names : list of symbols (documentation only)\nReturns: ((frontier id ...) (dominated (dominator-id dominated-id ...) ...))\nCategory: algebra"
  (let* ((ids (map car candidates))
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
                         ((dominates? (cadr (car rest)) c-factors) #f)
                         (else (dominated? (cdr rest)))))))
             candidates))
         (dominated-ids (filter (lambda (id) (not (member? id frontier-ids))) ids))
         (dom-groups
           (filter-map
             (lambda (fid)
               (let* ((fid-factors (factors-of fid))
                      (doms (filter
                              (lambda (did)
                                (dominates? fid-factors (factors-of did)))
                              dominated-ids)))
                 (if (null? doms) #f
                   (cons fid doms))))
             frontier-ids)))
    (list (cons 'frontier frontier-ids)
          (cons 'dominated dom-groups))))
