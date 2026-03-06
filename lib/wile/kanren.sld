(define-library (wile kanren)
  (export
    ;; Re-export microKanren core
    var var? var=?
    walk ext-s unify
    == call/fresh disj conj
    mplus bind unit mzero
    empty-state
    ;; miniKanren syntactic sugar
    fresh conde run run*
    ;; Internal macros (exported for hygiene — used by fresh/conde/run)
    zzz conj+ disj+
    ;; Reification
    reify reify-1st)
  (import (scheme base)
          (wile microkanren))
  (include "kanren.scm"))
