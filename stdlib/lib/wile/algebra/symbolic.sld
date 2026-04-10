(define-library (wile algebra symbolic)
  (description "Symbolic algebra: theory projections, recursive normalization, and transformation tracing.")
  (export
    ;; Named axioms
    make-named-axiom named-axiom?
    named-axiom-name named-axiom-general-form named-axiom-axiom
    ;; Theory
    make-theory theory?
    theory-axioms theory-associative-ops
    ;; Theory combinators
    theory-filter theory-exclude theory-prioritize theory-merge
    ;; Rewrite steps
    make-rewrite-step rewrite-step?
    step-rule-name step-general-form step-before step-after
    fuel-exhausted-step?
    ;; Term protocol
    sexp-term-protocol
    ;; Recursive normalizer
    make-recursive-normalizer
    ;; Theory projections
    monoid->theory lattice->theory boolean->theory
    ;; Reporter
    format-trace)
  (import (scheme base)
          (wile algebra rewrite)
          (wile algebra monoid)
          (wile algebra lattice)
          (wile algebra boolean))
  (include "symbolic.scm"))
