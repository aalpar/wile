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
  (make-graph-analysis* semiring adjacency weight-fn cache fast-path-kind)
  graph-analysis?
  (semiring         ga-semiring)
  (adjacency        ga-adjacency)
  (weight-fn        ga-weight-fn)
  (cache            ga-cache set-ga-cache!)
  (fast-path-kind   ga-fast-path-kind))

;; --- Constructor ---

(define (make-graph-analysis semiring adjacency weight-fn)
  "Construct a graph analysis from a semiring, adjacency alist, and weight function.\nADJACENCY is an alist: ((node . ((neighbor . edge-data) ...)) ...).\nWEIGHT-FN receives edge-data and returns a semiring value.\nPass #f for unit weights (each edge = semiring-one).\n\nWhen the semiring declares carrier `'big-int' (via `(make-semiring ... '(carrier . big-int))' or the built-in `bigint-counting-semiring') AND `weight-fn' is `#f', this constructor attaches the unit-weight counting fast path (sub-path 4A of the bignum-allocation-reduction plan). Queries route through `count-paths-in-dag', which uses in-place `*big.Int' arithmetic instead of the per-relaxation allocating loop. A non-`#f' `weight-fn' falls through to the generic Scheme inner loop — weighted-bigint acceleration (sub-path 4B) is not yet implemented.\n\nUse `(graph-analysis-fast-path? ga)' to verify whether the fast path attached.\n\nExamples:\n  (make-graph-analysis (boolean-semiring)\n    '((\"A\" . ((\"B\" . 1))) (\"B\" . ()))\n    #f)\n\nParameters:\n  semiring : any\n  adjacency : list\n  weight-fn : procedure-or-false\nReturns: graph-analysis\nCategory: algebra\n\nSee also: `graph-query', `graph-query-all', `graph-analysis-fast-path?'."
  (let* ((carrier  (semiring-carrier semiring))
         (fast-kind (cond
                      ((and (eq? carrier 'big-int) (not weight-fn))
                       'unit-weight-counting)
                      (else #f)))
         (wfn (or weight-fn (lambda (_) (semiring-one semiring)))))
    (make-graph-analysis* semiring adjacency wfn '() fast-kind)))

;; --- Fast-path introspection ---

(define (graph-analysis-fast-path? ga)
  "Return #t iff GA has a fast-path strategy attached.\n\nThe fast path is non-#f when the semiring declares a carrier with a registered Go-side kernel and the constructor's other arguments are compatible. Currently the only attached strategy is `'unit-weight-counting' (bigint carrier + #f weight-fn).\n\nExamples:\n  (graph-analysis-fast-path? (make-graph-analysis (counting-semiring) '() #f))\n  => #f\n  (graph-analysis-fast-path? (make-graph-analysis (bigint-counting-semiring) '() #f))\n  => #t\n\nParameters:\n  ga : graph-analysis\nReturns: boolean\nCategory: algebra\n\nSee also: `graph-analysis-fast-path-kind', `make-graph-analysis'."
  (if (ga-fast-path-kind ga) #t #f))

(define (graph-analysis-fast-path-kind ga)
  "Return the symbol naming GA's fast-path strategy, or #f if none.\n\nKnown strategies:\n  'unit-weight-counting — bigint-carrier semiring + #f weight-fn, dispatches\n                         to `count-paths-in-dag'.\n\nExamples:\n  (graph-analysis-fast-path-kind (make-graph-analysis (bigint-counting-semiring) '() #f))\n  => unit-weight-counting\n\nParameters:\n  ga : graph-analysis\nReturns: symbol-or-false\nCategory: algebra\n\nSee also: `graph-analysis-fast-path?', `make-graph-analysis'."
  (ga-fast-path-kind ga))

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
  (case (ga-fast-path-kind ga)
    ((unit-weight-counting)
     (compute-via-count-paths-in-dag ga source))
    (else
     (call-with-values
       (lambda () (topological-order-from ga source))
       (lambda (order cyclic?)
         (if cyclic?
             (compute-via-worklist ga source)
             (compute-via-topological-order ga source order)))))))

;; --- Sub-path 4A: unit-weight bigint counting via count-paths-in-dag ---
;;
;; The kernel `count-paths-in-dag' (in (wile algebragraph)) takes integer
;; node indices and a unit-weight edge list, runs reverse-postorder
;; propagation with in-place `*big.Int' Add, and returns either a vector
;; of counts or #f for cyclic input. The wrapper here translates between
;; the name-keyed adjacency surface of `(wile algebra graph)' and that
;; integer-indexed kernel.
;;
;; Why an explicit wrapper rather than direct exposure of the kernel:
;; `(wile algebra graph)' is the user-facing entry point for graph
;; queries (with caching, semiring-parameterization, name-keyed nodes).
;; The kernel speaks a narrower, performance-tuned protocol. The carrier
;; opt on the semiring is the bridge: when the user opts in via
;; `bigint-counting-semiring' (or any `(carrier . big-int)' annotation),
;; the wrapper picks up the dispatch and the kernel does the arithmetic.
;;
;; Name→index translation uses a hashtable for O(1) lookups; the naive
;; alist version was O(V·E) for setup and dominated the kernel's actual
;; arithmetic cost on graphs >100 nodes.
(define (compute-via-count-paths-in-dag ga source)
  (let* ((adj       (ga-adjacency ga))
         (name->idx (make-hashtable))
         (idx->name (make-vector 16))
         (next-idx  0))

    ;; Intern: return existing idx for name, or assign next available.
    ;; `vec` is mutated in place; we reallocate via `vector-resize!`-style
    ;; doubling when capacity runs out. `idx->name` shadowing is the
    ;; idiomatic way to capture the new vector reference.
    (define (intern! name)
      (let ((existing (hashtable-ref name->idx name -1)))
        (cond
          ((>= existing 0) existing)
          (else
           (let ((i next-idx))
             (when (>= i (vector-length idx->name))
               (let* ((old-len (vector-length idx->name))
                      (new-vec (make-vector (* 2 old-len) #f)))
                 (let copy ((j 0))
                   (cond
                     ((= j old-len)
                      (set! idx->name new-vec))
                     (else
                      (vector-set! new-vec j (vector-ref idx->name j))
                      (copy (+ j 1)))))))
             (vector-set! idx->name i name)
             (hashtable-set! name->idx name i)
             (set! next-idx (+ i 1))
             i)))))

    ;; Pass 1: intern every vertex name (adj keys + edge targets) and
    ;; collect the integer edges.
    (let* ((edges
             (let outer ((entries adj) (acc '()))
               (cond
                 ((null? entries) acc)
                 (else
                  (let* ((entry (car entries))
                         (u     (intern! (car entry))))
                    (let inner ((es (cdr entry)) (acc2 acc))
                      (cond
                        ((null? es) (outer (cdr entries) acc2))
                        (else
                         (let ((v (intern! (caar es))))
                           (inner (cdr es) (cons (cons u v) acc2)))))))))))
           (src-idx (hashtable-ref name->idx source -1)))
      (when (< src-idx 0)
        (error "graph-query: source not present in graph adjacency" source))
      (let ((counts (count-paths-in-dag next-idx edges src-idx)))
        (cond
          ((not counts)
           ;; Reachable subgraph contains a cycle. The counting semiring
           ;; on cycles has no finite answer; the fast path declines to
           ;; spin.
           (error
            (string-append
             "graph-query: bigint-counting-semiring on a cyclic graph "
             "(non-idempotent semiring; the counting algebra diverges on "
             "cycles). Remedies: "
             "(a) (import (wile algebragraph)) and use "
             "(count-paths-cyclic ...) for exact counts via SCC condensation; "
             "(b) use a bounded-carrier semiring (saturating, modular, log) "
             "for approximate counts.")))
          (else
           ;; Walk indices once, build alist. Reachable nodes have count
           ;; > 0; unreachable get omitted to match the slow path's
           ;; result shape.
           (let l ((i 0) (acc '()))
             (cond
               ((= i next-idx) acc)
               (else
                (let ((c (vector-ref counts i)))
                  (cond
                    ((zero? c) (l (+ i 1) acc))
                    (else
                     (l (+ i 1)
                        (cons (cons (vector-ref idx->name i) c) acc))))))))))))))

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
;; algorithm does not terminate algebraically — the counting semiring on
;; cycles has no finite answer (see the bignum-allocation-reduction plan's
;; five-layer failure analysis).
;;
;; To prevent the documented hang from being silent, a safety cap of
;; 2·V·E outer-loop iterations is enforced. Textbook Bellman-Ford
;; converges in V·E node-pops on any well-posed semiring; the 2× margin
;; is for worklist variants that may re-pop slightly more under
;; particular insertion orders. When the cap fires the function raises
;; an error pointing the caller at (wile algebragraph) count-paths-cyclic
;; for the exact-cyclic-counting case, or at the approximate-counting
;; semirings (saturating/modular/log) for bounded-carrier alternatives.
(define (compute-via-worklist ga source)
  (let* ((S   (ga-semiring ga))
         (adj (ga-adjacency ga))
         (wfn (ga-weight-fn ga))
         (V   (length adj))
         (E   (apply + (map (lambda (entry) (length (cdr entry))) adj)))
         ;; max 1 below guards trivial graphs (E=0): without it, max-iter
         ;; would be 0 and the cap would fire on the very first pop.
         (max-iter (* 2 V (max 1 E))))
    (let loop ((worklist (list source))
               (dist (list (cons source (semiring-one S))))
               (iter 0))
      (cond
        ((null? worklist) dist)
        ((>= iter max-iter)
         (error
          (string-append
           "compute-via-worklist: exceeded "
           (number->string max-iter)
           " iterations without convergence. Likely a non-idempotent semiring "
           "(e.g. (counting-semiring)) on a cyclic graph. Remedies: "
           "(a) (import (wile algebragraph)) and use (count-paths-cyclic ...) "
           "for exact counts via SCC condensation; "
           "(b) use a bounded-carrier semiring (saturating, modular, log) "
           "from (wile algebra semiring) if approximate counts suffice.")))
        (else
          (let* ((node (car worklist))
                 (rest (cdr worklist))
                 (node-dist (cdr (assoc node dist))))
            (let ((entry (assoc node adj)))
              (if (not entry)
                  (loop rest dist (+ iter 1))
                  (let edge-loop ((edges (cdr entry))
                                  (wl rest)
                                  (d dist))
                    (if (null? edges)
                        (loop wl d (+ iter 1))
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
                                           new-d))))))))))))))

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
