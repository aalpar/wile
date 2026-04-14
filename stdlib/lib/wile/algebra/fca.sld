(define-library (wile algebra fca)
  (description "Formal Concept Analysis: contexts, Galois connections, concept lattices (NextClosure, Ganter 1984).")
  (export
    make-context context-from-alist fca-context?
    context-objects context-attributes
    intent extent
    concept-lattice concept-extent concept-intent
    concept-lattice->algebra-lattice concept-relationship
    set-add set-intersect set-union set-subset? set-member? set-before
    sort-strings)
  (import (scheme base)
          (wile algebra lattice)
          (wile algebra closure))
  (include "fca.scm"))
