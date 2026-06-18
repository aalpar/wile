(define-library (wile algebra setoid)
  (description "Setoids: sets with explicit equivalence relations. Also hosts equivalence-parameterized collection helpers (setoid-member?, setoid-assoc, setoid-dedup) and options-alist plumbing (assv-or, validate-opts-keys) shared by algebraic-structure constructors.")
  (export make-setoid setoid?
          setoid-equiv?
          default-setoid numeric-setoid string-setoid eqv-setoid
          setoid-equivalence-class
          setoid-member? setoid-assoc setoid-dedup
          assv-or validate-opts-keys
          make-violation-reporter
          validate-setoid
          with-setoid
          assert-validation
          assert-procedure)
  (import (scheme base))
  (include "setoid.scm"))
