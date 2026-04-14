(define-library (wile algebra graph)
  (description "Semiring-parameterized graph algorithms: shortest path, reachability, path counting via Bellman-Ford.")
  (export make-graph-analysis graph-analysis?
          graph-query graph-query-all)
  (import (scheme base)
          (wile algebra semiring))
  (include "graph.scm"))
