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
    substitution-apply
    ;; Diophantine basis (standalone primitive)
    diophantine-basis
    ;; AC matching
    ac-match
    ;; AC unification
    ac-unify
    ;; Internal helpers (exposed for testing)
    flatten-ac)
  (import (scheme base)
          (srfi 1)
          (wile algebra rewrite)
          (wile algebra symbolic))
  (include "unification.scm"))
