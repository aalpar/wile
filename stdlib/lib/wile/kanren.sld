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
    ;; Reification
    reify reify-1st)
  (import (scheme base)
          (wile microkanren))
  (include "kanren.scm"))
