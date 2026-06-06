(define-library (wile algebra cfl)
  (description "Context-free-language reachability over a labeled directed graph. A path counts iff its edge-label string lies in the language of a context-free grammar. Generalizes semiring path-algebra (Boolean reachability, tropical shortest-path) to grammar-constrained composition — the basis of context-sensitive (interprocedural, field-sensitive) program analysis. Reps-Horwitz-Sagiv (1995).")
  (export
    ;; Productions (typed kernels)
    cfl-epsilon cfl-terminal cfl-unary cfl-binary
    cfl-production? cfl-production-kind cfl-production-lhs
    cfl-production-rhs1 cfl-production-rhs2
    ;; Grammar
    make-cfl-grammar cfl-grammar?
    cfl-grammar-start cfl-grammar-productions
    cfl-grammar-nonterminals cfl-grammar-terminals
    ;; Graph
    make-cfl-graph cfl-graph?
    cfl-graph-nodes cfl-graph-edges
    ;; Solver + query
    cfl-solve cfl-solution?
    cfl-reachable? cfl-reachable-from cfl-reachable-pairs cfl-derives?
    ;; Preset
    dyck-grammar)
  (import (scheme base)
          (wile algebra setoid))   ; validation-helper idiom (validate-* siblings)
  (include "cfl.scm"))
