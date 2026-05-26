;;; (wile algebra graph) — Semiring-parameterized graph algorithms
;;;
;;; Lazy single-source Bellman-Ford parameterized by semiring.
;;; Boolean semiring = reachability, tropical = shortest path,
;;; counting = path count.
;;;
;;; Adjacency is an alist: ((node . ((neighbor . edge-data) ...)) ...)
;;; Weight function maps edge-data to semiring values; #f for unit weights.

;; --- Local utilities ---

(define (ga-filter pred lst)
  (let loop ((xs lst) (acc '()))
    (if (null? xs) (reverse acc)
      (loop (cdr xs)
            (if (pred (car xs)) (cons (car xs) acc) acc)))))

;; --- Record type ---

(define-record-type <graph-analysis>
  (make-graph-analysis* semiring adjacency weight-fn cache)
  graph-analysis?
  (semiring   ga-semiring)
  (adjacency  ga-adjacency)
  (weight-fn  ga-weight-fn)
  (cache      ga-cache set-ga-cache!))

;; --- Constructor ---

(define (make-graph-analysis semiring adjacency weight-fn)
  "Construct a graph analysis from a semiring, adjacency alist, and weight function.\nADJACENCY is an alist: ((node . ((neighbor . edge-data) ...)) ...).\nWEIGHT-FN receives edge-data and returns a semiring value.\nPass #f for unit weights (each edge = semiring-one).\n\nExamples:\n  (make-graph-analysis (boolean-semiring)\n    '((\"A\" . ((\"B\" . 1))) (\"B\" . ()))\n    #f)\n\nParameters:\n  semiring : any\n  adjacency : list\n  weight-fn : procedure-or-false\nReturns: graph-analysis\nCategory: algebra\n\nSee also: `graph-query', `graph-query-all'."
  (let ((wfn (or weight-fn (lambda (_) (semiring-one semiring)))))
    (make-graph-analysis* semiring adjacency wfn '())))

;; --- Single-source computation ---

;; Compute distances from source. Strategy:
;;   - Attempt topological sort of the subgraph reachable from `source`.
;;   - DAG (no cycle in reachable subgraph): process nodes in topological
;;     order with a single forward pass. Each reachable edge is relaxed
;;     exactly once. Correct for *all* semirings.
;;   - Cyclic: fall back to the worklist algorithm. Correct for idempotent
;;     semirings (boolean reachability, tropical shortest path). The
;;     counting semiring on cycles diverges algebraically; this
;;     implementation will then not terminate.
;;
;; The previous worklist-only algorithm over-counted on non-trivial DAGs
;; for the counting semiring: re-popping a node after its count was updated
;; re-propagated the full new count, adding to what was already sent
;; forward. Topological-order processing visits each node exactly once
;; after its count has settled.
(define (compute-single-source ga source)
  (call-with-values
    (lambda () (topological-order-from ga source))
    (lambda (order cyclic?)
      (if cyclic?
          (compute-via-worklist ga source)
          (compute-via-topological-order ga source order)))))

;; Compute a topological order of the subgraph reachable from `source`.
;; Returns two values (via `values`):
;;   - order: list of reachable nodes in topological order
;;            (root first, leaves last); #f if cyclic
;;   - cyclic?: #t iff the reachable subgraph contains a cycle
;;
;; Iterative DFS with white/gray/black coloring. Back-edge (gray-on-gray)
;; signals a cycle. Each node is prepended to `topo-order` when finalized
;; (marked black), which yields reverse-postorder = topological order
;; directly (no final reverse needed).
(define (topological-order-from ga source)
  (let ((adj      (ga-adjacency ga))
        (colors   (list (cons source 'gray))) ; node → 'gray | 'black; absent = 'white
        (topo     '())
        (cyclic?  #f))
    (define (color-of n)
      (cond ((assoc n colors) => cdr)
            (else 'white)))
    (define (out-edges n)
      (let ((e (assoc n adj)))
        (if e (cdr e) '())))
    ;; Stack frames: (node . pending-edges) where pending-edges are the
    ;; outgoing edges of `node` not yet visited.
    (let loop ((stack (list (cons source (out-edges source)))))
      (cond
        ((or cyclic? (null? stack))
         (if cyclic?
             (values #f #t)
             (values topo #f)))
        (else
         (let* ((top     (car stack))
                (node    (car top))
                (pending (cdr top)))
           (cond
             ((null? pending)
              ;; All neighbors visited; finalize node.
              (set! colors (cons (cons node 'black) colors))
              (set! topo (cons node topo))
              (loop (cdr stack)))
             (else
              (let* ((edge        (car pending))
                     (neighbor    (car edge))
                     (rest-edges  (cdr pending))
                     (c           (color-of neighbor)))
                (cond
                  ((eq? c 'white)
                   (set! colors (cons (cons neighbor 'gray) colors))
                   (loop (cons (cons neighbor (out-edges neighbor))
                               (cons (cons node rest-edges) (cdr stack)))))
                  ((eq? c 'gray)
                   ;; Back-edge: cycle in reachable subgraph.
                   (set! cyclic? #t)
                   (loop '()))
                  (else
                   ;; 'black — forward or cross-edge; skip this neighbor.
                   (loop (cons (cons node rest-edges) (cdr stack))))))))))))))

;; Process nodes in topological order with a single forward pass.
;; For each node u (in topo order), propagate its current dist value to each
;; successor v via `dist[v] := dist[v] ⊕ (dist[u] ⊗ w)`. Because the topo
;; order visits u only after all of u's predecessors have settled, dist[u]
;; is final at u's turn — no re-propagation occurs, so non-idempotent
;; semirings (counting) get the right answer.
(define (compute-via-topological-order ga source order)
  (let ((S   (ga-semiring ga))
        (adj (ga-adjacency ga))
        (wfn (ga-weight-fn ga)))
    (let outer ((nodes order)
                (dist (list (cons source (semiring-one S)))))
      (if (null? nodes)
          dist
          (let* ((node      (car nodes))
                 (node-dist (cond ((assoc node dist) => cdr)
                                  (else (semiring-zero S))))
                 (entry     (assoc node adj)))
            (if (not entry)
                (outer (cdr nodes) dist)
                (let inner ((edges (cdr entry))
                            (d     dist))
                  (if (null? edges)
                      (outer (cdr nodes) d)
                      (let* ((neighbor-name (caar edges))
                             (edge-data     (cdar edges))
                             (w             (wfn edge-data))
                             (candidate     (semiring-times S node-dist w))
                             (old-entry     (assoc neighbor-name d))
                             (old-val       (if old-entry
                                                (cdr old-entry)
                                                (semiring-zero S)))
                             (merged        (semiring-plus S old-val candidate))
                             (new-d         (cons (cons neighbor-name merged)
                                                  (if old-entry
                                                      (ga-filter
                                                        (lambda (p)
                                                          (not (equal? (car p) neighbor-name)))
                                                        d)
                                                      d))))
                        (inner (cdr edges) new-d))))))))))

;; Worklist Bellman-Ford. Retained for cyclic graphs. Correct for idempotent
;; semirings (boolean OR, tropical min): repeated propagation is harmless
;; because `(x ⊕ x) = x`. For non-idempotent semirings on cycles, this
;; algorithm does not terminate (the counting semiring on cycles has no
;; finite answer — see the bignum-allocation-reduction plan's five-layer
;; failure analysis).
(define (compute-via-worklist ga source)
  (let ((S   (ga-semiring ga))
        (adj (ga-adjacency ga))
        (wfn (ga-weight-fn ga)))
    (let loop ((worklist (list source))
               (dist (list (cons source (semiring-one S)))))
      (if (null? worklist) dist
          (let* ((node (car worklist))
                 (rest (cdr worklist))
                 (node-dist (cdr (assoc node dist))))
            (let ((entry (assoc node adj)))
              (if (not entry)
                  (loop rest dist)
                  (let edge-loop ((edges (cdr entry))
                                  (wl rest)
                                  (d dist))
                    (if (null? edges)
                        (loop wl d)
                        (let* ((neighbor-name (caar edges))
                               (edge-data (cdar edges))
                               (w (wfn edge-data))
                               (candidate (semiring-times S node-dist w))
                               (old-entry (assoc neighbor-name d))
                               (old-val (if old-entry (cdr old-entry) (semiring-zero S)))
                               (merged (semiring-plus S old-val candidate)))
                          (if (equal? merged old-val)
                              (edge-loop (cdr edges) wl d)
                              (let ((new-d (cons (cons neighbor-name merged)
                                                 (if old-entry
                                                     (ga-filter (lambda (p) (not (equal? (car p) neighbor-name))) d)
                                                     d))))
                                (edge-loop (cdr edges)
                                           (if (member neighbor-name wl) wl (cons neighbor-name wl))
                                           new-d)))))))))))))

;; --- Cache layer ---

(define (get-or-compute ga source)
  (let ((cached (assoc source (ga-cache ga))))
    (if cached (cdr cached)
        (let ((result (compute-single-source ga source)))
          (set-ga-cache! ga (cons (cons source result) (ga-cache ga)))
          result))))

;; --- Public API ---

(define (graph-query ga source target)
  "Query the semiring value between source and target nodes.\nReturns semiring-zero if target is unreachable. Lazily computes\nand caches single-source distances on first query per source.\n\nExamples:\n  (let ((ga (make-graph-analysis (boolean-semiring)\n              '((\"A\" . ((\"B\" . 1))) (\"B\" . ()))\n              #f)))\n    (graph-query ga \"A\" \"B\"))  => #t\n\nParameters:\n  ga : graph-analysis\n  source : any\n  target : any\nReturns: any\nCategory: algebra\n\nSee also: `graph-query-all', `make-graph-analysis'."
  (let* ((dist (get-or-compute ga source))
         (entry (assoc target dist)))
    (if entry (cdr entry) (semiring-zero (ga-semiring ga)))))

(define (graph-query-all ga source)
  "Return distance alist for all reachable nodes from source.\nEach entry is (name . semiring-value). Lazily computed and cached.\n\nExamples:\n  (let ((ga (make-graph-analysis (tropical-semiring)\n              '((\"A\" . ((\"B\" . 1))) (\"B\" . ()))\n              (lambda (e) e))))\n    (graph-query-all ga \"A\"))  => ((\"A\" . 0) (\"B\" . 1))\n\nParameters:\n  ga : graph-analysis\n  source : any\nReturns: list\nCategory: algebra\n\nSee also: `graph-query', `make-graph-analysis'."
  (get-or-compute ga source))
