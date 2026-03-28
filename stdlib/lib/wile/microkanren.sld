(define-library (wile microkanren)
  (description "microKanren core: minimal relational programming kernel.")
  (export var var? var=?
          walk ext-s unify
          == call/fresh disj conj
          mplus bind unit mzero
          empty-state)
  (import (scheme base))
  (include "microkanren.scm"))
