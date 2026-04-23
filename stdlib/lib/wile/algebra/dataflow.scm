;; (wile algebra dataflow) — MFP worklist dataflow solver
;;
;; Lattice-parameterized forward/backward fixpoint analysis over a
;; CFG-protocol-abstracted control flow graph. The protocol separates
;; the algorithm (here) from the CFG representation (supplied by the
;; caller via make-cfg-protocol).
;;
;; References:
;;   Kildall (1973) — original worklist MFP algorithm
;;   Kam & Ullman (1976) — MFP vs MOP on distributive frameworks
;;   Cousot & Cousot (1977) — abstract interpretation framework
;;
;; Extracted from wile-goast's goast/dataflow.scm to give the generic
;; solver a home outside the SSA/Go-AST analysis pipeline. wile-goast
;; supplies an ssa-cfg-protocol adapter for its SSA function shape.

;; Body populated in Phase 6.
