(define-library (wile algebra rewrite)
  (description "Term rewriting with algebraic simplification rules.")
  (export
    ;; Term protocol
    make-term-protocol term-protocol?
    term-compound? term-get-operator term-get-operands term-make-term term-compare
    ;; Axioms
    make-identity-axiom identity-axiom?
    make-commutativity-axiom commutativity-axiom?
    make-absorbing-axiom absorbing-axiom?
    make-idempotence-axiom idempotence-axiom?
    make-involution-axiom involution-axiom?
    make-absorption-axiom absorption-axiom?
    make-associativity-axiom associativity-axiom?
    directional-axiom?
    axiom?
    ;; Rule compilation
    axiom->rules
    ;; No-match sentinel
    *no-match* no-match?
    ;; Normalizer
    make-normalizer)
  (import (scheme base))
  (include "rewrite.scm"))
