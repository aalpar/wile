(define-library (wile algebra matching)
  (description "Two-sided matching: Gale-Shapley, Hungarian assignment, hospital/intern, Conway-lattice selection.")
  (export
    ;; Preference profiles
    make-preference-profile preference-profile?
    preference-profile-agents preference-profile-ranks-of
    preference-profile-setoid
    preference-profile-rank-of
    preference-profile-prefers-strictly?)
  (import (scheme base)
          (wile algebra setoid))
  (include "matching.scm"))
