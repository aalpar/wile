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
