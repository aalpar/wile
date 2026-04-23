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
    monoid->theory group->theory semiring->theory ring->theory
    field->theory lattice->theory heyting->theory boolean->theory
    ;; Equivalence discovery
    discover-equivalences
    ;; Reporter
    format-trace
    ;; Boolean normalization facade (§2.2 free Boolean algebra on atoms)
    symbolic-boolean-normalize
    symbolic-boolean-equivalent?)
  (import (scheme base)
          (wile algebra rewrite)
          (wile algebra monoid)
          (wile algebra group)
          (wile algebra semiring)
          (wile algebra ring)
          (wile algebra lattice)
          (wile algebra heyting)
          (wile algebra boolean))
  (include "symbolic.scm"))
