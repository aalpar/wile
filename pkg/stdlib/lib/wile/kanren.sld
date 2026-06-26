(define-library (wile kanren)
  (description "miniKanren relational programming: run, fresh, conde, ==.")
  (export
    ;; Re-export microKanren core
    var var? var=?
    walk ext-s unify
    == call/fresh disj conj
    mplus bind unit mzero
    empty-state
    ;; miniKanren syntactic sugar
    fresh conde run run*
    ;; Stream forcing
    pull take-inf take-all-inf
    ;; Reification
    reify reify-1st reify-name walk* reify-s)
  (import (scheme base)
          (wile microkanren))
  (include "kanren.scm"))
