(define-library (wile algebra unification)
  (description "AC-matching and AC-unification on terms modulo associative-commutative theories.")
  (export
    ;; Pattern variables
    make-pattern-var pattern-var? pattern-var-name
    ;; Pattern parsing (?-convention sugar)
    parse-pattern
    ;; Substitutions
    make-substitution substitution? substitution-bindings
    empty-substitution
    substitution-lookup
    substitution-compose
    substitution-apply)
  (import (scheme base)
          (wile algebra rewrite))
  (include "unification.scm"))
