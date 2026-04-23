(define-library (wile algebra heyting)
  (description "Heyting algebras: bounded distributive lattices with implication.")
  (export make-heyting-algebra heyting-algebra?
          heyting-join heyting-meet heyting-bottom heyting-top
          heyting-leq? heyting-implies heyting-negate
          heyting->lattice
          powerset-heyting map-heyting
          validate-heyting-algebra
          with-heyting)
  (import (scheme base)
          (wile algebra setoid)
          (wile algebra lattice))
  (include "heyting.scm"))
