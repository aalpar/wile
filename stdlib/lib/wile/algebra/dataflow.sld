(define-library (wile algebra dataflow)
  (description "Monotone framework (MFP) worklist dataflow solver with CFG-protocol abstraction. Lattice-parameterized forward/backward fixpoint analysis. Pairs with (wile algebra lattice) for the algebra of states and (wile algebra abstract-domain) for pre-built domains.")
  (export make-cfg-protocol
          cfg-protocol?
          cfg-protocol-blocks-of-fn
          cfg-protocol-index-of-fn
          cfg-protocol-preds-of-fn
          cfg-protocol-succs-of-fn
          cfg-blocks-of
          cfg-index-of
          cfg-preds-of
          cfg-succs-of
          init-state init-state? init-state-value
          reverse-postorder
          run-analysis
          analysis-in
          analysis-out
          analysis-states)
  (import (scheme base)
          (srfi 1)
          (wile algebra lattice))
  (include "dataflow.scm"))
