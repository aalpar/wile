(define-library (wile algebra rewrite)
  (export
    ;; Term protocol
    make-term-protocol term-protocol?
    term-get-operator term-get-operands term-make-term term-compare
    ;; Axioms
    identity-axiom identity-axiom?
    commutativity-axiom commutativity-axiom?
    absorbing-axiom absorbing-axiom?
    idempotence-axiom idempotence-axiom?
    involution-axiom involution-axiom?
    axiom?
    ;; Normalizer
    make-normalizer)
  (include "rewrite.scm"))
