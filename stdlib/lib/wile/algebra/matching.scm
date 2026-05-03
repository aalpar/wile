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
