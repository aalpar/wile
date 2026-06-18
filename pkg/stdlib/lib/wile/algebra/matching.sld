(define-library (wile algebra matching)
  (description
    "Two-sided matching primitives — Roth-Sotomayor (1990).\nThree layers per algebra-foundations directions §4.6:\n  Local optimization → gale-shapley, gale-shapley/receiver-optimal, tropical-assignment\n  Stability constraint → stable?, blocking-pairs\n  Global selection → stable-matching-lattice, egalitarian-stable-matching, sex-equal-stable-matching\n\nTheorems brought into scope:\n  Gale-Shapley (1962) — deferred acceptance produces a stable matching\n  Conway (1976) — stable matchings form a distributive lattice (proposer-utility order)\n  Birkhoff (1937, via §5.5) — finite distributive lattices are downset lattices\n  Roth (1985) — hospital/intern reduces to one-to-one with synthetic copies\n  Irving-Leather-Gusfield (1987) — egalitarian stable matching is polynomial via min-weight closed subsets of the rotation poset (v1 brute-forces anyway)\n  Kato (1993) — sex-equal stable matching is NP-hard (documents the brute-force wall)\n  Kuhn (1955), Munkres (1957) — Hungarian algorithm O(n^3) for assignment")
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
