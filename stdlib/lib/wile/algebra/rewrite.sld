(define-library (wile algebra rewrite)
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
    axiom?
    ;; Normalizer
    make-normalizer)
  (import (scheme base))
  (include "rewrite.scm"))
