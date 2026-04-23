(define-library (wile algebra dataflow)
  (description "Monotone framework (MFP) worklist dataflow solver with CFG-protocol abstraction. Lattice-parameterized forward/backward fixpoint analysis. Pairs with (wile algebra lattice) for the algebra of states and (wile algebra abstract-domain) for pre-built domains.")
  (export make-cfg-protocol
          cfg-protocol?
          cfg-protocol-blocks-of
          cfg-protocol-index-of
          cfg-protocol-preds-of
          cfg-protocol-succs-of
          reverse-postorder
          run-analysis
          analysis-in
          analysis-out
          analysis-states)
  (import (scheme base)
          (srfi 1)
          (wile algebra lattice))
  (include "dataflow.scm"))
