(define-library (wile algebra graph)
  (description "Semiring-parameterized graph algorithms: shortest path, reachability, path counting via Bellman-Ford.")
  (export make-graph-analysis graph-analysis?
          graph-query graph-query-all
          graph-analysis-fast-path? graph-analysis-fast-path-kind
          graph-analysis-sccs graph-node-in-cycle? graph-cyclic-nodes
          graph-scc?
          gscc-scc-vec gscc-non-trivial-vec
          gscc-name->idx gscc-idx->name gscc-num-nodes)
  (import (scheme base)
          (wile algebra semiring))
  ;; (wile algebragraph) is the Go FFI extension exposing `count-paths-in-dag`,
  ;; the in-place-arithmetic kernel that the bigint-counting fast path
  ;; dispatches to. It is only present under the `kitchen-sink` profile (see
  ;; `internal/bootstrap/bootstrap.go`'s `ProfileExtensions`). Under smaller
  ;; profiles the import is skipped and the fast path is suppressed — graph
  ;; queries fall back transparently to the pure-Scheme inner loop.
  (cond-expand
    ((library (wile algebragraph))
     (import (wile algebragraph)))
    (else))
  (include "graph.scm"))
