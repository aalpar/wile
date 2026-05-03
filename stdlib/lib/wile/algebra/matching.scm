;;; matching.scm — Two-sided matching primitives.
;;;
;;; Three layers per directions doc §4.6:
;;;   Local optimization → gale-shapley, tropical-assignment
;;;   Stability constraint → stable?, blocking-pairs
;;;   Global selection → stable-matching-lattice, egalitarian-stable-matching
;;;
;;; Theorems brought into scope:
;;;   Gale-Shapley (1962) — deferred acceptance produces a stable matching
;;;   Conway (1976) — stable matchings form a distributive lattice
;;;   Roth (1985) — hospital/intern reduces to one-to-one with synthetic copies
;;;   Birkhoff (1937) — finite distributive lattices are downset lattices

(define-record-type <preference-profile>
  (make-preference-profile* agents ranks-of setoid)
  preference-profile?
  (agents preference-profile-agents)
  (ranks-of preference-profile-ranks-of)
  (setoid preference-profile-setoid))

(define (make-preference-profile agents ranks-of . opts)
  "Construct a preference profile.\n\nParameters:\n  agents : list — the agents on this side of the market\n  ranks-of : procedure — agent → ordered list of preferred candidates (best first)\n  opts : trailing alist — supports (setoid . S)\nReturns: <preference-profile>\nCategory: algebra\nKeywords: stable matching, preferences, two-sided market"
  (assert-procedure "make-preference-profile" ranks-of)
  (validate-opts-keys "make-preference-profile" opts '(setoid))
  (let ((setoid (assv-or opts 'setoid (default-setoid))))
    (make-preference-profile* agents ranks-of setoid)))

(define (preference-profile-rank-of P agent candidate)
  "Return 1-based rank of CANDIDATE in AGENT's preference list, or #f if absent.\n\nParameters:\n  P : preference-profile\n  agent : any\n  candidate : any\nReturns: positive integer or #f\nCategory: algebra\nKeywords: preferences, ranking"
  (let ((S (preference-profile-setoid P))
        (lst ((preference-profile-ranks-of P) agent)))
    (let loop ((xs lst) (i 1))
      (cond ((null? xs) #f)
            ((setoid-equiv? S candidate (car xs)) i)
            (else (loop (cdr xs) (+ i 1)))))))

(define (preference-profile-prefers-strictly? P agent x y)
  "Return #t iff AGENT strictly prefers X to Y under preference profile P.\nReturns #f if they tie, or if either is absent from AGENT's list.\n\nParameters:\n  P : preference-profile\n  agent : any\n  x : any\n  y : any\nReturns: boolean\nCategory: algebra\nKeywords: preferences, ranking, strict order"
  (let ((rx (preference-profile-rank-of P agent x))
        (ry (preference-profile-rank-of P agent y)))
    (and rx ry (< rx ry))))

(define (validate-preference-profile P candidate-set)
  "Verify that every agent in P ranks only members of CANDIDATE-SET, with no ties.\nReturns #t on success or a reversed list of (violation-type agent ...) entries.\n\nParameters:\n  P : preference-profile\n  candidate-set : list — universe of valid candidates\nReturns: #t or list\nCategory: algebra\nKeywords: validation, preferences"
  (let* ((fail! (make-violation-reporter))
         (S (preference-profile-setoid P)))
    (for-each
      (lambda (agent)
        (let ((lst ((preference-profile-ranks-of P) agent)))
          (for-each
            (lambda (c)
              (unless (setoid-member? S c candidate-set)
                (fail! 'preference-out-of-set agent c)))
            lst)
          (let loop ((xs lst))
            (cond
              ((or (null? xs) (null? (cdr xs))) 'ok)
              (else
                (when (setoid-member? S (car xs) (cdr xs))
                  (fail! 'tied-preference agent (car xs)))
                (loop (cdr xs)))))))
      (preference-profile-agents P))
    (fail!)))

(define-record-type <bipartite-matching>
  (make-bipartite-matching* pairs prop-setoid recv-setoid)
  bipartite-matching?
  (pairs bipartite-matching-pairs)
  (prop-setoid bipartite-matching-prop-setoid)
  (recv-setoid bipartite-matching-recv-setoid))

(define (make-bipartite-matching pairs . opts)
  "Construct a bipartite matching from an alist of (proposer . receiver) pairs.\nOptional trailing alist supports (prop-setoid . S), (recv-setoid . S).\n\nParameters:\n  pairs : alist of (any . any)\nReturns: <bipartite-matching>\nCategory: algebra\nKeywords: bipartite matching, assignment, two-sided"
  (validate-opts-keys "make-bipartite-matching" opts '(prop-setoid recv-setoid))
  (let ((ps (assv-or opts 'prop-setoid (default-setoid)))
        (rs (assv-or opts 'recv-setoid (default-setoid))))
    (make-bipartite-matching* pairs ps rs)))

(define (bipartite-matching-partner M agent)
  "Return the partner of AGENT in matching M, or #f if AGENT is unmatched.\nLookup is symmetric — works whether AGENT is on the proposer or receiver side.\n\nParameters:\n  M : bipartite-matching\n  agent : any\nReturns: any or #f\nCategory: algebra\nKeywords: matching, partner, lookup"
  (let ((PS (bipartite-matching-prop-setoid M))
        (RS (bipartite-matching-recv-setoid M))
        (pairs (bipartite-matching-pairs M)))
    (let loop ((ps pairs))
      (cond ((null? ps) #f)
            ((setoid-equiv? PS agent (car (car ps))) (cdr (car ps)))
            ((setoid-equiv? RS agent (cdr (car ps))) (car (car ps)))
            (else (loop (cdr ps)))))))

(define (bipartite-matching-unmatched M side agents)
  "Return AGENTS not appearing on SIDE ('proposer or 'receiver) of matching M.\n\nParameters:\n  M : bipartite-matching\n  side : symbol — 'proposer or 'receiver\n  agents : list — agents on that side\nReturns: list — agents from AGENTS not appearing in M on the given side\nCategory: algebra\nKeywords: matching, unmatched, partial"
  (let* ((S (case side
              ((proposer) (bipartite-matching-prop-setoid M))
              ((receiver) (bipartite-matching-recv-setoid M))
              (else (error "bipartite-matching-unmatched: side must be 'proposer or 'receiver" side))))
         (key (case side ((proposer) car) ((receiver) cdr)))
         (matched (map key (bipartite-matching-pairs M))))
    (let loop ((xs agents) (acc '()))
      (cond ((null? xs) (reverse acc))
            ((setoid-member? S (car xs) matched) (loop (cdr xs) acc))
            (else (loop (cdr xs) (cons (car xs) acc)))))))

(define (bipartite-matching-equal? M1 M2)
  "Return #t iff M1 and M2 represent the same matching (order-insensitive).\n\nParameters:\n  M1 : bipartite-matching\n  M2 : bipartite-matching\nReturns: boolean\nCategory: algebra\nKeywords: equality, matching"
  (let ((PS (bipartite-matching-prop-setoid M1))
        (RS (bipartite-matching-recv-setoid M1))
        (p1 (bipartite-matching-pairs M1))
        (p2 (bipartite-matching-pairs M2)))
    (define (pair-in pair pairs)
      (let loop ((ps pairs))
        (cond ((null? ps) #f)
              ((and (setoid-equiv? PS (car pair) (car (car ps)))
                    (setoid-equiv? RS (cdr pair) (cdr (car ps))))
               #t)
              (else (loop (cdr ps))))))
    (define (every-in? xs ys)
      (let loop ((xs xs))
        (cond ((null? xs) #t)
              ((pair-in (car xs) ys) (loop (cdr xs)))
              (else #f))))
    (and (= (length p1) (length p2))
         (every-in? p1 p2))))

(define (validate-bipartite-matching M proposers receivers)
  "Verify M is a valid one-to-one matching: every proposer/receiver appears at most once;\nevery agent in M's pairs is drawn from PROPOSERS or RECEIVERS.\nReturns #t on success or reversed violation list.\n\nParameters:\n  M : bipartite-matching\n  proposers : list\n  receivers : list\nReturns: #t or list\nCategory: algebra\nKeywords: validation, matching"
  (let* ((fail! (make-violation-reporter))
         (PS (bipartite-matching-prop-setoid M))
         (RS (bipartite-matching-recv-setoid M))
         (pairs (bipartite-matching-pairs M)))
    (let loop ((seen-p '()) (seen-r '()) (ps pairs))
      (cond
        ((null? ps) 'done)
        (else
          (let ((p (car (car ps))) (r (cdr (car ps))))
            (unless (setoid-member? PS p proposers)
              (fail! 'proposer-not-in-set p))
            (unless (setoid-member? RS r receivers)
              (fail! 'receiver-not-in-set r))
            (when (setoid-member? PS p seen-p)
              (fail! 'proposer-matched-twice p))
            (when (setoid-member? RS r seen-r)
              (fail! 'receiver-matched-twice r))
            (loop (cons p seen-p) (cons r seen-r) (cdr ps))))))
    (fail!)))

(define (blocking-pairs M prop-prefs recv-prefs)
  "Return the list of (proposer . receiver) blocking pairs in matching M.\nA blocking pair (p,r) satisfies: they are not currently matched together,\nyet p prefers r over its current partner (or is unmatched), AND\nr prefers p over its current partner (or is unmatched).\nM is stable iff this list is empty.\n\nParameters:\n  M : bipartite-matching\n  prop-prefs : preference-profile — proposers' preferences over receivers\n  recv-prefs : preference-profile — receivers' preferences over proposers\nReturns: list of (any . any)\nCategory: algebra\nKeywords: stability, blocking pair, Gale-Shapley"
  (let* ((RS (preference-profile-setoid recv-prefs))
         (proposers (preference-profile-agents prop-prefs))
         (receivers (preference-profile-agents recv-prefs)))
    (let outer ((ps proposers) (acc '()))
      (cond
        ((null? ps) (reverse acc))
        (else
          (let* ((p (car ps))
                 (cur-r (bipartite-matching-partner M p)))
            (let inner ((rs receivers) (acc2 acc))
              (cond
                ((null? rs) (outer (cdr ps) acc2))
                (else
                  (let* ((r (car rs))
                         (cur-p (bipartite-matching-partner M r))
                         (already-matched? (and cur-r (setoid-equiv? RS cur-r r)))
                         (p-prefers-r (or (not cur-r)
                                          (preference-profile-prefers-strictly?
                                            prop-prefs p r cur-r)))
                         (r-prefers-p (or (not cur-p)
                                          (preference-profile-prefers-strictly?
                                            recv-prefs r p cur-p))))
                    (if (and (not already-matched?) p-prefers-r r-prefers-p)
                        (inner (cdr rs) (cons (cons p r) acc2))
                        (inner (cdr rs) acc2))))))))))))

(define (gale-shapley prop-prefs recv-prefs)
  "Run the Gale-Shapley deferred-acceptance algorithm, proposer-optimal.\nEach proposer proposes down its preference list; receivers tentatively accept\ntheir best offer so far, releasing previously held matches. Terminates in\nO(n²) steps. Returns the unique proposer-optimal stable matching.\n\nParameters:\n  prop-prefs : preference-profile — proposers and their ordered receiver preferences\n  recv-prefs : preference-profile — receivers and their ordered proposer preferences\nReturns: bipartite-matching\nCategory: algebra\nKeywords: Gale-Shapley, stable matching, deferred acceptance, proposer-optimal"
  (let* ((PS (preference-profile-setoid prop-prefs))
         (RS (preference-profile-setoid recv-prefs))
         (proposers (preference-profile-agents prop-prefs))
         ;; cursors: alist mapping each proposer to the tail of its pref list
         ;; not yet proposed to (car = next candidate to propose to).
         (cursors (map (lambda (p)
                         (cons p ((preference-profile-ranks-of prop-prefs) p)))
                       proposers))
         ;; matches: alist mapping receiver → current proposer (or absent if unmatched)
         (matches '()))
    ;; Return the tail of p's cursor list (remaining proposals).
    (define (cursor-of p)
      (let loop ((cs cursors))
        (cond ((null? cs) '())
              ((setoid-equiv? PS p (car (car cs))) (cdr (car cs)))
              (else (loop (cdr cs))))))
    ;; Advance p's cursor by one position.
    (define (advance-cursor! p)
      (let loop ((cs cursors))
        (cond ((null? cs) (error "gale-shapley: proposer not found" p))
              ((setoid-equiv? PS p (car (car cs)))
               (set-cdr! (car cs) (cdr (cdr (car cs)))))
              (else (loop (cdr cs))))))
    ;; Return current holder of receiver r, or #f if unmatched.
    (define (holder-of r)
      (let loop ((ms matches))
        (cond ((null? ms) #f)
              ((setoid-equiv? RS r (car (car ms))) (cdr (car ms)))
              (else (loop (cdr ms))))))
    ;; Update matches so r is matched to p (replacing any existing entry).
    (define (set-match! r p)
      (let loop ((ms matches))
        (cond ((null? ms)
               (set! matches (cons (cons r p) matches)))
              ((setoid-equiv? RS r (car (car ms)))
               (set-cdr! (car ms) p))
              (else (loop (cdr ms))))))
    ;; Find any free proposer with remaining proposals; returns #f if none.
    (define (next-free-proposer)
      (let loop ((ps proposers))
        (cond ((null? ps) #f)
              ((and (not (null? (cursor-of (car ps))))
                    (not (holder-of* (car ps))))
               (car ps))
              (else (loop (cdr ps))))))
    ;; Return #t if p currently holds some match (is not free).
    (define (holder-of* p)
      (let loop ((ms matches))
        (cond ((null? ms) #f)
              ((setoid-equiv? PS p (cdr (car ms))) #t)
              (else (loop (cdr ms))))))
    ;; Main loop: iterate until no free proposer with remaining proposals.
    (let loop ()
      (let ((p (next-free-proposer)))
        (when p
          (let* ((rem (cursor-of p))
                 (r (car rem)))
            (advance-cursor! p)
            (let ((incumbent (holder-of r)))
              (cond
                ((not incumbent)
                 ;; r is free — tentatively accept.
                 (set-match! r p))
                ((preference-profile-prefers-strictly? recv-prefs r p incumbent)
                 ;; r prefers p over incumbent — swap.
                 (set-match! r p))
                ;; else r prefers incumbent; p is rejected (cursor already advanced).
                ))
            (loop)))))
    ;; Build the result alist as (proposer . receiver) pairs.
    (let ((pairs (map (lambda (cell) (cons (cdr cell) (car cell))) matches)))
      (make-bipartite-matching pairs
                               (cons 'prop-setoid PS)
                               (cons 'recv-setoid RS)))))

(define (stable? M prop-prefs recv-prefs)
  "Return #t iff matching M is stable under the given preferences (no blocking pair).\n\nParameters:\n  M : bipartite-matching\n  prop-prefs : preference-profile\n  recv-prefs : preference-profile\nReturns: boolean\nCategory: algebra\nKeywords: stability, Gale-Shapley, two-sided matching"
  (null? (blocking-pairs M prop-prefs recv-prefs)))

(define-syntax with-preference-profile
  (syntax-rules ()
    ((with-preference-profile p (agents ranks-of) body ...)
     (let ((tmp p))
       (let ((agents (preference-profile-agents tmp))
             (ranks-of (preference-profile-ranks-of tmp)))
         body ...)))))

(define-syntax with-bipartite-matching
  (syntax-rules ()
    ((with-bipartite-matching m (pairs) body ...)
     (let ((tmp m))
       (let ((pairs (bipartite-matching-pairs tmp)))
         body ...)))))
