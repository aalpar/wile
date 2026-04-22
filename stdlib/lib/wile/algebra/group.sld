(define-library (wile algebra group)
  (description "Groups: abstract algebraic structure, actions, orbits, Burnside counting.")
  (export make-group group?
          group-op group-identity group-inverse
          group->monoid
          validate-group assert-group with-group
          ;; §5.4 — extended introspection
          group-element? group-setoid group-equal?
          group-order group-elements group-generators
          finite-group? finitely-generated-group?
          subgroup-generated subgroup? enumerate-finite-group
          ;; §5.4 — actions
          make-group-action group-action? group-action-group
          group-action-act group-action-act-fn group-action-set-element?
          orbit stabilizer fixed-points
          orbit-representative burnside-count
          ;; §5.4 — presets
          trivial-group cyclic-group symmetric-group product-group
          trivial-action permutation-action regular-action
          conjugation-action product-action)
  (import (scheme base)
          (srfi 1)
          (wile algebra monoid)
          (wile algebra setoid))
  (include "group.scm"))
