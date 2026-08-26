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

(define (every? pred lst)
  (let loop ((xs lst))
    (or (null? xs)
        (and (pred (car xs)) (loop (cdr xs))))))

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
;;
;; The form is decided over the WHOLE list, not over its first element. A spec
;; that mixes them — '(gain (cost . down)), the natural half-migration of an
;; existing '(gain cost) call — is a caller who named a direction and a caller
;; who did not, in one list. Reading it as either form silently ranks an axis
;; the other way, which is the failure this facility exists to remove, so it
;; raises instead. So does anything that is not a list of one kind or the
;; other: a bare 'down reads as "this axis descends", and answering it with
;; "no directions" is the same silence one shape over.
(define (normalize-directions spec)
  "Collapse a direction spec to its alist form.\nA list of factor names is documentation only (every axis 'up) and becomes '();\nan alist of (name . up|down) is returned unchanged. A spec that mixes the two\nforms, or is neither, is an error.\n\nParameters:\n  spec : list of factor names, or alist of (name . up|down)\nReturns: alist of (name . up|down)\nCategory: algebra"
  (cond ((null? spec) '())
        ((and (pair? spec) (every? pair? spec)) spec)
        ((and (pair? spec) (every? symbol? spec)) '())
        (else
          (error "normalize-directions: spec must be all factor names or all (name . 'up|'down)" spec))))

;; Direction of improvement for KEY. Absent means 'up, which is what every
;; caller got before this existed. A value that is neither 'up nor 'down is an
;; error rather than a default: a misspelled direction silently meaning "up" is
;; exactly the failure this facility removes.
;;
;; Normalizes its own argument, so an exported caller may hand it either spec
;; form and get the same answer dominates? would use.
(define (factor-direction directions key)
  "Direction of improvement for KEY: 'up (the default when KEY is absent) or 'down.\nAccepts either direction-spec form. A value that is neither 'up nor 'down is an\nerror, never a silent 'up.\n\nParameters:\n  directions : list of factor names, or alist of (name . up|down)\n  key : symbol\nReturns: 'up or 'down\nCategory: algebra"
  (let ((entry (assq key (normalize-directions directions))))
    (if (not entry)
      'up
      (let ((d (cdr entry)))
        (if (memq d '(up down))
          d
          (error "factor-direction: direction must be 'up or 'down" key d))))))

;; Every direction entry must name a factor that is present and carry a
;; direction that is 'up or 'down.
;;
;; The name half is load-bearing. Directions are consulted per factor key, so a
;; direction whose name matches no factor is never looked up and never checked:
;; without this, a misspelled VALUE raises cleanly while a misspelled NAME is
;; silent, and the frontier still looks like a frontier. Half a guarantee is
;; worse than none, because the half that works advertises the half that does
;; not.
;;
;; DIRECTIONS must already be normalized. FACTORS is one candidate's alist; a
;; directions spec is not permitted to name axes a candidate does not carry.
;; WHO is the public procedure the caller invoked, so the message names that
;; rather than this private helper.
(define (check-directions who directions factors)
  (for-each
    (lambda (entry)
      (let ((key (car entry))
            (d (cdr entry)))
        (if (not (assoc key factors))
          (error (string-append who ": direction names a factor that is not present") key))
        (if (not (memq d '(up down)))
          (error (string-append who ": direction must be 'up or 'down") key d))))
    directions))

;;; ── Pareto dominance ────────────────────────────────────

;; X dominates Y iff X is at least as good as Y on every factor and strictly
;; better on at least one, where "better" is the factor's own direction.
;; Factors are alists ((key . val) ...). Keys in X must appear in Y.
;; DIRECTIONS is optional and takes either direction-spec form; omitted, every
;; factor is 'up and this is the original higher-is-better dominance.
(define (dominates? factors-x factors-y . opt)
  "Test Pareto dominance: X dominates Y iff X is at least as good as Y on\nevery factor and strictly better on at least one. Direction per factor comes\nfrom DIRECTIONS ('up, the default, or 'down).\n\nParameters:\n  factors-x : alist of (key . value) pairs\n  factors-y : alist of (key . value) pairs\n  directions : optional alist of (key . up|down), or a list of names\nReturns: boolean\nCategory: algebra"
  ;; One optional argument, not "one and then whatever". Reading (car opt) and
  ;; dropping the rest would run happily on (dominates? x y 'up 'down), which is
  ;; a caller who thinks directions are positional — the same class of silence
  ;; the directions facility exists to remove.
  (if (and (pair? opt) (pair? (cdr opt)))
    (error "dominates?: expected at most one directions argument" opt))
  (let ((directions (normalize-directions (if (null? opt) '() (car opt)))))
    (check-directions "dominates?" directions factors-x)
    (let loop ((fx factors-x) (any-strict #f))
      (if (null? fx)
        any-strict
        (let* ((key (car (car fx)))
               (vx (cdr (car fx)))
               ;; The "keys in X must appear in Y" precondition, checked rather
               ;; than assumed: (cdr (assoc ...)) on a miss reports a Go type at
               ;; a Scheme caller and names neither the key nor the candidate,
               ;; which inside pareto-frontier gives no way to tell which of N
               ;; candidates is malformed.
               (vy (let ((entry (assoc key factors-y)))
                     (if (not entry)
                       (error "dominates?: factor missing from the second candidate" key)
                       (cdr entry))))
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
  (let ((directions (normalize-directions factors)))
    ;; Checked here as well as inside dominates?, because a one-candidate
    ;; frontier calls dominates? zero times — nothing compares against itself —
    ;; so a typo'd axis would go unexamined in exactly the degenerate case a
    ;; caller is least likely to eyeball.
    (for-each (lambda (c) (check-directions "pareto-frontier" directions (cadr c)))
              candidates)
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
            (cons 'dominated dom-groups)))))
