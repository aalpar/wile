(define-library (wile algebra boolean)
  (description "Boolean algebras: complemented distributive lattices.")
  (export make-boolean-algebra boolean-algebra?
          boolean-join boolean-meet boolean-bottom boolean-top
          boolean-leq? boolean-complement
          boolean->heyting boolean->lattice boolean->ring
          powerset-boolean
          validate-boolean-algebra
          with-boolean)
  (import (scheme base)
          (wile algebra lattice)
          (wile algebra heyting)
          (wile algebra ring))
  (include "boolean.scm"))
