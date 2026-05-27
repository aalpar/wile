(define-library (wile algebra graph)
  (description "Semiring-parameterized graph algorithms: shortest path, reachability, path counting via Bellman-Ford.")
  (export make-graph-analysis graph-analysis?
          graph-query graph-query-all
          graph-analysis-fast-path? graph-analysis-fast-path-kind)
  (import (scheme base)
          (wile algebra semiring)
          (wile algebragraph))
  (include "graph.scm"))
