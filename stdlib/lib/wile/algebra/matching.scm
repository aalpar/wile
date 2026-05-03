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
  "Construct a preference profile.

Parameters:
  agents : list — the agents on this side of the market
  ranks-of : procedure — agent → ordered list of preferred candidates (best first)
  opts : trailing alist — supports (setoid . S)
Returns: <preference-profile>
Category: algebra
Keywords: stable matching, preferences, two-sided market"
  (assert-procedure "make-preference-profile" ranks-of)
  (validate-opts-keys "make-preference-profile" opts '(setoid))
  (let ((setoid (assv-or opts 'setoid (default-setoid))))
    (make-preference-profile* agents ranks-of setoid)))
