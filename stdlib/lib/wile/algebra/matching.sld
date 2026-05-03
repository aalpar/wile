(define-library (wile algebra matching)
  (description "Two-sided matching: Gale-Shapley, Hungarian assignment, hospital/intern, Conway-lattice selection.")
  (export
    ;; Preference profiles
    make-preference-profile preference-profile?
    preference-profile-agents preference-profile-ranks-of
    preference-profile-setoid
    preference-profile-rank-of
    preference-profile-prefers-strictly?
    validate-preference-profile
    ;; Bipartite matchings
    make-bipartite-matching bipartite-matching?
    bipartite-matching-pairs
    bipartite-matching-prop-setoid
    bipartite-matching-recv-setoid
    bipartite-matching-partner
    bipartite-matching-unmatched
    bipartite-matching-equal?
    validate-bipartite-matching
    ;; Stability
    blocking-pairs stable?
    ;; Algorithms
    gale-shapley gale-shapley/receiver-optimal
    hospital-intern-match
    tropical-assignment
    ;; Rotations
    make-rotation rotation? rotation-cycle
    apply-rotation rotations
    ;; Conway lattice
    stable-matching-lattice
    ;; Stable-matching selectors
    egalitarian-stable-matching sex-equal-stable-matching
    ;; Field binders
    with-preference-profile with-bipartite-matching)
  (import (scheme base)
          (wile algebra setoid)
          (wile algebra lattice))
  (include "matching.scm"))
