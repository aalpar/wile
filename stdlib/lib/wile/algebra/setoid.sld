(define-library (wile algebra setoid)
  (description "Setoids: sets with explicit equivalence relations.")
  (export make-setoid setoid?
          setoid-equiv?
          default-setoid numeric-setoid string-setoid eqv-setoid
          setoid-equivalence-class
          validate-setoid
          with-setoid)
  (import (scheme base))
  (include "setoid.scm"))
