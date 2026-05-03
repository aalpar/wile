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

(define (gale-shapley/receiver-optimal prop-prefs recv-prefs)
  "Compute the receiver-optimal stable matching by running Gale-Shapley with sides swapped.\nReturns a matching with proposer-shaped pairs (proposers as keys) for consistency with\nthe proposer-side variant.\n\nParameters:\n  prop-prefs : preference-profile\n  recv-prefs : preference-profile\nReturns: <bipartite-matching>\nCategory: algebra\nKeywords: Gale-Shapley, receiver-optimal, stable matching"
  (let* ((swapped (gale-shapley recv-prefs prop-prefs))
         (pairs (bipartite-matching-pairs swapped)))
    (make-bipartite-matching
      (map (lambda (pr) (cons (cdr pr) (car pr))) pairs)
      (cons 'prop-setoid (preference-profile-setoid prop-prefs))
      (cons 'recv-setoid (preference-profile-setoid recv-prefs)))))

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

(define (validate-quotas! quotas hospitals)
  (for-each
    (lambda (h)
      (let ((cell (assoc h quotas)))
        (cond
          ((not cell)
           (error "hospital-intern-match: missing quota for hospital" h))
          ((or (not (integer? (cdr cell)))
               (not (positive? (cdr cell))))
           (error "hospital-intern-match: quota must be a positive integer" h (cdr cell))))))
    hospitals))

(define (hospital-intern-match intern-prefs hospital-prefs hospital-quotas)
  "Compute an intern-optimal stable many-to-one matching via Roth's reduction.\nReturns an alist ((hospital . (intern ...)) ...) of accepted interns per hospital.\nUnmatched interns are absent; caller can derive them via set difference.\n\nParameters:\n  intern-prefs : preference-profile — interns' preferences over hospitals\n  hospital-prefs : preference-profile — hospitals' preferences over interns\n  hospital-quotas : alist of (hospital . positive-integer)\nReturns: alist of (any . list)\nCategory: algebra\nKeywords: hospital-intern, college-admissions, many-to-one, Roth, quota"
  (validate-quotas! hospital-quotas (preference-profile-agents hospital-prefs))
  (let* ((HS (preference-profile-setoid hospital-prefs))
         (hospitals (preference-profile-agents hospital-prefs))
         (interns (preference-profile-agents intern-prefs))
         ;; Build all synthetic copies: list of (h . k) cons cells
         ;; grouped per hospital.  copies-for returns the copies for h.
         (all-copies
           (let loop ((hs hospitals) (acc '()))
             (if (null? hs)
                 (reverse acc)
                 (let* ((h (car hs))
                        (q (cdr (assoc h hospital-quotas)))
                        (copies
                          (let inner ((k 1) (cacc '()))
                            (if (> k q)
                                (reverse cacc)
                                (inner (+ k 1) (cons (cons h k) cacc))))))
                   (loop (cdr hs) (cons (cons h copies) acc))))))
         ;; Look up copies for a given hospital h (using HS for identity).
         (copies-for
           (lambda (h)
             (let loop ((cs all-copies))
               (cond
                 ((null? cs) '())
                 ((setoid-equiv? HS h (car (car cs))) (cdr (car cs)))
                 (else (loop (cdr cs)))))))
         ;; Flat list of all copies, in hospital-list order.
         (flat-copies
           (let loop ((cs all-copies) (acc '()))
             (if (null? cs)
                 (reverse acc)
                 (loop (cdr cs)
                       (let inner ((ks (cdr (car cs))) (a acc))
                         (if (null? ks)
                             a
                             (inner (cdr ks) (cons (car ks) a))))))))
         ;; Inflated intern preference profile.
         ;; Each intern's pref list: for each hospital h in original list,
         ;; append all copies of h in index order.
         (inflated-intern-ranks
           (lambda (intern)
             (let loop ((hs ((preference-profile-ranks-of intern-prefs) intern))
                        (acc '()))
               (if (null? hs)
                   (reverse acc)
                   (loop (cdr hs)
                         (let inner ((ks (copies-for (car hs))) (a acc))
                           (if (null? ks)
                               a
                               (inner (cdr ks) (cons (car ks) a)))))))))
         (inflated-iprefs
           (make-preference-profile interns inflated-intern-ranks))
         ;; Inflated copy preference profile.
         ;; Each copy (h . k) has the same preference list as h.
         (inflated-copy-ranks
           (lambda (copy)
             ((preference-profile-ranks-of hospital-prefs) (car copy))))
         (inflated-hprefs
           (make-preference-profile flat-copies inflated-copy-ranks))
         ;; Run intern-proposing Gale-Shapley on the inflated instance.
         (inflated-match (gale-shapley inflated-iprefs inflated-hprefs))
         ;; Collapse: for each hospital h, collect interns matched to any copy of h.
         (pairs (bipartite-matching-pairs inflated-match)))
    ;; Build result alist: ((h . (matched-interns ...)) ...) for all hospitals.
    (let loop ((hs hospitals) (acc '()))
      (if (null? hs)
          (reverse acc)
          (let* ((h (car hs))
                 ;; Gather interns whose matched copy has (car copy) ≡ h under HS.
                 (matched-interns
                   (let inner ((ps pairs) (ilist '()))
                     (if (null? ps)
                         (reverse ilist)
                         (let* ((pr (car ps))
                                (intern (car pr))
                                (copy (cdr pr)))
                           (if (setoid-equiv? HS (car copy) h)
                               (inner (cdr ps) (cons intern ilist))
                               (inner (cdr ps) ilist)))))))
            (loop (cdr hs) (cons (cons h matched-interns) acc)))))))

;; ─── Phase 5: Rotations and Conway lattice ─────────────

(define-record-type <rotation>
  (make-rotation* cycle)
  rotation?
  (cycle rotation-cycle))

(define (make-rotation cycle)
  "Construct a rotation from a list of (proposer . receiver) pairs in cyclic order.\\nApplying the rotation to a stable matching M produces M' where each proposer pᵢ is\\nreassigned from its current partner rᵢ to r_{i+1 mod k}.\\n\\nParameters:\\n  cycle : list of (any . any), length ≥ 2\\nReturns: <rotation>\\nCategory: algebra\\nKeywords: rotation, Irving, Gusfield, stable matching"
  (when (or (not (list? cycle)) (< (length cycle) 2))
    (error "make-rotation: cycle must be a list of at least 2 (proposer . receiver) pairs" cycle))
  (make-rotation* cycle))

(define (apply-rotation M rho)
  "Apply rotation RHO to matching M, returning a new matching where each rotation\\nproposer is reassigned to the next receiver in the cycle.\\n\\nParameters:\\n  M : bipartite-matching\\n  rho : rotation\\nReturns: <bipartite-matching>\\nCategory: algebra\\nKeywords: rotation, Gusfield-Irving, stable matching, lattice traversal"
  (let* ((cycle (rotation-cycle rho))
         (k (length cycle))
         (PS (bipartite-matching-prop-setoid M))
         ;; Build new-partner alist: pᵢ → r_{(i+1) mod k}
         (rotmap
           (let loop ((i 0) (acc '()))
             (if (>= i k)
                 (reverse acc)
                 (loop (+ i 1)
                       (cons (cons (car (list-ref cycle i))
                                   (cdr (list-ref cycle (modulo (+ i 1) k))))
                             acc)))))
         (new-pairs
           (map (lambda (pr)
                  (let* ((p (car pr))
                         (override (setoid-assoc PS p rotmap)))
                    (if override (cons p (cdr override)) pr)))
                (bipartite-matching-pairs M))))
    (make-bipartite-matching
      new-pairs
      (cons 'prop-setoid (bipartite-matching-prop-setoid M))
      (cons 'recv-setoid (bipartite-matching-recv-setoid M)))))

;; ─── Rotation enumeration — Phase 5 ────────────────────

;; proposer-succ: find the first receiver strictly worse than M(p) for p, who strictly
;; prefers p over her current partner in M (or is unmatched).  Returns #f if none exists.
(define (proposer-succ p M prop-prefs recv-prefs)
  (let* ((cur-r (bipartite-matching-partner M p))
         (lst ((preference-profile-ranks-of prop-prefs) p))
         ;; Drop all receivers p prefers strictly over cur-r, then drop cur-r itself.
         (rest (let drop ((xs lst))
                 (cond
                   ((null? xs) '())
                   ((preference-profile-prefers-strictly? prop-prefs p (car xs) cur-r)
                    (drop (cdr xs)))
                   (else (cdr xs))))))  ; cdr skips cur-r itself
    (let try ((xs rest))
      (cond
        ((null? xs) #f)
        (else
          (let* ((r (car xs))
                 (cur-p (bipartite-matching-partner M r)))
            (if (or (not cur-p)
                    (preference-profile-prefers-strictly? recv-prefs r p cur-p))
                r
                (try (cdr xs)))))))))

;; walk-for-cycle: starting from p, follow the successor graph p → M(succ(p)) → ...
;; accumulating path newest-first.  When a proposer is revisited, extract the cycle
;; as (proposer . M(proposer)) pairs in cycle order (oldest-first) and return a rotation.
;; Returns #f if the walk terminates without a cycle.
(define (walk-for-cycle start M prop-prefs recv-prefs PS)
  (let walk ((p start) (path '()))
    ;; Check whether p already appears in path (path entries = (proposer . succ-receiver)).
    (let ((seen (let search ((xs path))
                  (cond ((null? xs) #f)
                        ((setoid-equiv? PS p (car (car xs))) (car xs))
                        (else (search (cdr xs)))))))
      (cond
        (seen
         ;; p is revisited.  The cycle is all path entries from the FRONT up to and
         ;; including `seen`, then reversed to oldest-first order.
         (let ((cycle-path
                 (let collect ((xs path) (acc '()))
                   (cond
                     ((null? xs) (reverse acc))  ; safety — seen must be in path
                     ((eq? (car xs) seen)
                      (reverse (cons (car xs) acc)))
                     (else
                      (collect (cdr xs) (cons (car xs) acc)))))))
           (make-rotation
             (map (lambda (cell)
                    (cons (car cell)
                          (bipartite-matching-partner M (car cell))))
                  cycle-path))))
        (else
         (let ((s (proposer-succ p M prop-prefs recv-prefs)))
           (and s
                (let ((p-next (bipartite-matching-partner M s)))
                  (and p-next
                       (walk p-next (cons (cons p s) path)))))))))))

;; find-one-rotation: try each proposer as a walk start; return the first rotation
;; found, or #f if M has no exposed rotation (M = M_bot).
(define (find-one-rotation M prop-prefs recv-prefs)
  (let ((proposers (preference-profile-agents prop-prefs))
        (PS (preference-profile-setoid prop-prefs)))
    (let try ((ps proposers))
      (cond
        ((null? ps) #f)
        (else
         (let ((rho (walk-for-cycle (car ps) M prop-prefs recv-prefs PS)))
           (if rho rho (try (cdr ps)))))))))

(define (rotations prop-prefs recv-prefs)
  "Enumerate all rotations of the stable-matching system for the given preferences.\nEach rotation, when applied to M_top, produces another stable matching. The set of\nrotations is in bijection with the join-irreducibles of the Conway distributive lattice\n(Gusfield-Irving 1989, Theorem 3.3.1). Returns an empty list when M_top = M_bot.\n\nParameters:\n  prop-prefs : preference-profile\n  recv-prefs : preference-profile\nReturns: list of <rotation>\nCategory: algebra\nKeywords: rotation, Gusfield-Irving, Conway, stable matching, join-irreducibles"
  (let walk ((M (gale-shapley prop-prefs recv-prefs))
             (acc '()))
    (let ((rho (find-one-rotation M prop-prefs recv-prefs)))
      (cond
        ((not rho) (reverse acc))
        (else (walk (apply-rotation M rho) (cons rho acc)))))))

;; ─── Conway lattice — Phase 5 ──────────────────────────

(define (stable-matching-lattice prop-prefs recv-prefs)
  "Construct the Conway distributive lattice of stable matchings (Conway 1976) by\nbrute-force enumeration of all 2^|rotations| subsets applied to the proposer-optimal\nmatching, plus deduplication. Carrier is the set of stable matchings; partial order\nis proposer-utility (M ≤ M' iff every proposer is at least as well off in M').\n\nFor degenerate inputs (M_top = M_bot, no rotations), returns a one-element lattice.\nExponential in |rotations| — soft cap of ~10 agents per side.\n\nParameters:\n  prop-prefs : preference-profile\n  recv-prefs : preference-profile\nReturns: <lattice>\nCategory: algebra\nKeywords: Conway, Birkhoff, distributive lattice, stable matching, rotation"
  (let* ((M-top (gale-shapley prop-prefs recv-prefs))
         (rhos (rotations prop-prefs recv-prefs))
         (all-matchings (enumerate-stable-matchings M-top rhos prop-prefs recv-prefs)))
    (define (leq? M1 M2)
      (let loop ((ps (preference-profile-agents prop-prefs)))
        (cond
          ((null? ps) #t)
          (else
            (let* ((p (car ps))
                   (pa (bipartite-matching-partner M1 p))
                   (pb (bipartite-matching-partner M2 p)))
              (cond
                ((and (not pa) (not pb)) (loop (cdr ps)))
                ((not pa) (loop (cdr ps)))
                ((not pb) #f)
                ((let ((RS (preference-profile-setoid recv-prefs)))
                   (setoid-equiv? RS pa pb))
                 (loop (cdr ps)))
                ((preference-profile-prefers-strictly? prop-prefs p pb pa)
                 (loop (cdr ps)))
                (else #f)))))))
    (define (find-join a b)
      (let outer ((xs all-matchings) (best #f))
        (cond
          ((null? xs) (or best a))
          ((and (leq? a (car xs)) (leq? b (car xs))
                (let inner ((ys all-matchings))
                  (cond
                    ((null? ys) #t)
                    ((and (leq? a (car ys)) (leq? b (car ys))
                          (not (leq? (car xs) (car ys))))
                     #f)
                    (else (inner (cdr ys))))))
           (car xs))
          (else (outer (cdr xs) best)))))
    (define (find-meet a b)
      (let outer ((xs all-matchings) (best #f))
        (cond
          ((null? xs) (or best a))
          ((and (leq? (car xs) a) (leq? (car xs) b)
                (let inner ((ys all-matchings))
                  (cond
                    ((null? ys) #t)
                    ((and (leq? (car ys) a) (leq? (car ys) b)
                          (not (leq? (car ys) (car xs))))
                     #f)
                    (else (inner (cdr ys))))))
           (car xs))
          (else (outer (cdr xs) best)))))
    (let ((M-bot (gale-shapley/receiver-optimal prop-prefs recv-prefs)))
      (make-lattice find-join find-meet
                    M-bot
                    M-top
                    leq?
                    (cons 'elements all-matchings)
                    (cons 'cardinality (length all-matchings))))))

(define (enumerate-stable-matchings M-top rhos prop-prefs recv-prefs)
  (let* ((subsets (all-subsets rhos))
         (candidates
           (map (lambda (subset)
                  (let apply-loop ((rs subset) (M M-top))
                    (cond
                      ((null? rs) M)
                      (else (apply-loop (cdr rs) (apply-rotation M (car rs)))))))
                subsets)))
    (let outer ((xs candidates) (acc '()))
      (cond
        ((null? xs) (reverse acc))
        (else
          (let* ((M (car xs))
                 (already-in?
                   (let inner ((ys acc))
                     (cond
                       ((null? ys) #f)
                       ((bipartite-matching-equal? M (car ys)) #t)
                       (else (inner (cdr ys)))))))
            (cond
              (already-in? (outer (cdr xs) acc))
              ((stable? M prop-prefs recv-prefs) (outer (cdr xs) (cons M acc)))
              (else (outer (cdr xs) acc)))))))))

(define (all-subsets xs)
  (cond
    ((null? xs) '(()))
    (else
      (let ((rest (all-subsets (cdr xs))))
        (let combine ((rs rest) (acc rest))
          (cond
            ((null? rs) acc)
            (else (combine (cdr rs) (cons (cons (car xs) (car rs)) acc)))))))))

;; ─── Hungarian (Kuhn-Munkres) — Phase 4 ─────────────────

(define (kuhn-munkres-square C n)
  ;; Jonker-Volgenant 1987 form: potential-based shortest augmenting path.
  ;; 1-indexed internally; index 0 is a sentinel row/col.
  ;;   u[i] : row potential for row i (1..n)
  ;;   v[j] : col potential for col j (1..n)
  ;;   p[j] : row currently assigned to col j (1..n), or 0 if unassigned
  ;;   way[j] : predecessor column in the alternating path from j
  ;; Returns a vector mapping row → assigned-col (0-indexed).
  (let ((u (make-vector (+ n 1) 0))
        (v (make-vector (+ n 1) 0))
        (p (make-vector (+ n 1) 0))
        (way (make-vector (+ n 1) 0)))
    (let row-loop ((i 1))
      (when (<= i n)
        (vector-set! p 0 i)
        (let ((j0 0)
              (minv (make-vector (+ n 1) +inf.0))
              (used (make-vector (+ n 1) #f)))
          ;; Reset minv to +inf.0 each row (make-vector did init, but explicit
          ;; reset keeps the loop correct under repeated row additions).
          (let init-loop ((j 0))
            (when (<= j n)
              (vector-set! minv j +inf.0)
              (vector-set! used j #f)
              (init-loop (+ j 1))))
          (let phase-loop ()
            (vector-set! used j0 #t)
            (let ((i0 (vector-ref p j0))
                  (delta +inf.0)
                  (j1 0))
              (let col-loop ((j 1))
                (when (<= j n)
                  (when (not (vector-ref used j))
                    (let ((cur (- (vector-ref (vector-ref C (- i0 1)) (- j 1))
                                  (vector-ref u i0)
                                  (vector-ref v j))))
                      (when (< cur (vector-ref minv j))
                        (vector-set! minv j cur)
                        (vector-set! way j j0))
                      (when (< (vector-ref minv j) delta)
                        (set! delta (vector-ref minv j))
                        (set! j1 j))))
                  (col-loop (+ j 1))))
              (let update-loop ((j 0))
                (when (<= j n)
                  (cond
                    ((vector-ref used j)
                     (vector-set! u (vector-ref p j)
                                  (+ (vector-ref u (vector-ref p j)) delta))
                     (vector-set! v j (- (vector-ref v j) delta)))
                    (else
                     (vector-set! minv j (- (vector-ref minv j) delta))))
                  (update-loop (+ j 1))))
              (set! j0 j1)
              (when (not (= (vector-ref p j0) 0))
                (phase-loop))))
          (let augment-loop ()
            (let ((j1 (vector-ref way j0)))
              (vector-set! p j0 (vector-ref p j1))
              (set! j0 j1)
              (when (not (= j0 0))
                (augment-loop)))))
        (row-loop (+ i 1))))
    (let ((result (make-vector n 0)))
      (let result-loop ((j 1))
        (when (<= j n)
          (when (> (vector-ref p j) 0)
            (vector-set! result (- (vector-ref p j) 1) (- j 1)))
          (result-loop (+ j 1))))
      result)))

(define (tropical-assignment cost-fn proposers receivers)
  "Compute a minimum-cost perfect assignment via the Hungarian algorithm (Kuhn 1955; Munkres 1957).\nReturns (matching . total-cost) where matching is a <bipartite-matching>.\nUse +inf.0 in COST-FN to forbid a (proposer, receiver) pair.\n\nUnequal-size sides are padded internally with synthetic agents at +inf.0 cost; synthetic\npairs are excluded from the returned matching. The Shapley-Shubik core allocation\n(LP dual potentials) is computed internally but not returned in v1.\n\nParameters:\n  cost-fn : procedure — (proposer × receiver) → number ∪ +inf.0\n  proposers : list\n  receivers : list\nReturns: pair (<bipartite-matching> . number)\nCategory: algebra\nKeywords: Hungarian, assignment, Kuhn-Munkres, tropical, bipartite, Shapley-Shubik"
  (let* ((m (length proposers))
         (n (length receivers))
         (size (max m n))
         (INF +inf.0)
         (proposers-vec (list->vector proposers))
         (receivers-vec (list->vector receivers))
         (C (make-vector size #f)))
    (let row-loop ((i 0))
      (when (< i size)
        (let ((row (make-vector size INF)))
          (when (< i m)
            ;; Real proposer: real receivers from cost-fn
            (let col-loop ((j 0))
              (when (< j n)
                (vector-set! row j (cost-fn (vector-ref proposers-vec i)
                                            (vector-ref receivers-vec j)))
                (col-loop (+ j 1))))
            ;; Real proposer to synthetic receiver: cost 0 (decode filters out)
            (let col-loop ((j n))
              (when (< j size)
                (vector-set! row j 0)
                (col-loop (+ j 1)))))
          ;; Synthetic proposer: cost 0 to all receivers (decode filters out)
          (when (>= i m)
            (let col-loop ((j 0))
              (when (< j size)
                (vector-set! row j 0)
                (col-loop (+ j 1)))))
          (vector-set! C i row)
          (row-loop (+ i 1)))))
    (let ((assignment (kuhn-munkres-square C size)))
      (let loop ((i 0) (pairs '()) (total 0))
        (cond
          ((>= i size) (cons (make-bipartite-matching (reverse pairs)) total))
          (else
            (let ((j (vector-ref assignment i)))
              (cond
                ((or (>= i m) (>= j n)) (loop (+ i 1) pairs total))
                ((= (vector-ref (vector-ref C i) j) INF) (loop (+ i 1) pairs total))
                (else
                  (loop (+ i 1)
                        (cons (cons (vector-ref proposers-vec i)
                                    (vector-ref receivers-vec j)) pairs)
                        (+ total (vector-ref (vector-ref C i) j))))))))))))

