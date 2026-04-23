(define-library (wile algebra combinatorial-graph)
  (description "Graphs as combinatorial objects: isomorphism via 1-WL + individualization-refinement backtracking, chromatic/Tutte polynomials, spanning-tree count, bipartite matching. Distinct from (wile algebra graph), which handles semiring-parameterized path analytics on the same adjacency-alist shape.")
  (export
    ;; Core
    make-graph graph?
    graph-vertices graph-edges graph-neighbors graph-degree
    graph-edge? graph-vertex-equiv? graph-setoid
    graph-order graph-size graph-directed? graph-multi? graph-self-loops?
    ;; Tier predicates + promotion (per §5.4)
    finite-graph? finitely-generated-graph?
    enumerate-finite-graph
    ;; Validation
    validate-graph assert-graph with-graph
    ;; Traversal + connectivity
    graph-bfs graph-dfs graph-connected-components
    graph-bipartite? graph-bipartition
    ;; Isomorphism
    graph-isomorphic? graph-canonical-form
    ;; Invariants
    graph-spanning-tree-count
    graph-chromatic-polynomial
    ;; Presets
    complete-graph cycle-graph path-graph
    complete-bipartite-graph empty-graph petersen-graph)
  (import (scheme base)
          (srfi 1)
          (wile algebra setoid))
  (include "combinatorial-graph.scm"))
