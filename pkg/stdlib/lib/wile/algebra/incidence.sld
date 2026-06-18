(define-library (wile algebra incidence)
  (description "Incidence algebras and Möbius functions on locally-finite posets (Rota 1964).")
  (export
    ;; Locally-finite poset — richer than <partial-order>, carries interval enumeration
    make-locally-finite-poset locally-finite-poset?
    lf-poset-leq? lf-poset-interval lf-poset-elements
    finite-set->locally-finite-poset
    ;; Incidence algebra over a ring
    make-incidence-algebra incidence-algebra?
    incidence-algebra-poset incidence-algebra-ring
    incidence-algebra-mu-cache
    ;; Canonical elements
    zeta-function mobius-function
    ;; Operations
    incidence-convolve
    ;; Classical Möbius inversion
    mobius-inversion)
  (import (scheme base)
          (wile algebra setoid)
          (wile algebra ring))
  (include "incidence.scm"))
