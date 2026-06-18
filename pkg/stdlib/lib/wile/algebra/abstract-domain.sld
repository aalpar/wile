(define-library (wile algebra abstract-domain)
  (description "Pre-built abstract interpretation domains built on (wile algebra lattice). Sibling of (wile algebra interval). Currently exports the sign domain: 5-element flat lattice {flat-bottom, neg, zero, pos, flat-top}, abstraction function from integers, and sign arithmetic table for add/sub/mul.")
  (export sign-lattice
          sign?
          abstract-sign
          sign-binop
          sign-galois-connection)
  (import (scheme base)
          (wile algebra lattice)
          (wile algebra order)
          (wile algebra galois))
  (include "abstract-domain.scm"))
