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

;; --- Record types ---

(define-record-type <graph-analysis>
  (make-graph-analysis* semiring adjacency weight-fn cache fast-path-kind scc)
  graph-analysis?
  (semiring         ga-semiring)
  (adjacency        ga-adjacency)
  (weight-fn        ga-weight-fn)
  (cache            ga-cache set-ga-cache!)
  (fast-path-kind   ga-fast-path-kind)
  ;; scc: lazily populated <graph-scc> bundling SCC structure + name interning.
  ;; Source-independent and immutable once set; mutator is purely a memoization
  ;; hook. #f until first call to graph-analysis-sccs or a cyclic-counting query.
  (scc              ga-scc          set-ga-scc!))

;; <graph-scc> bundles the SCC decomposition of a <graph-analysis>'s
;; adjacency together with the name<->index interning state the Go-side
;; kernels need. Source-independent: every source on the same analysis
;; sees the same SCC structure and the same interning. The per-source
;; counts-by-scc returned by count-paths-cyclic are NOT cached here
;; (they go in ga-cache like every other per-source query result).
(define-record-type <graph-scc>
  (make-graph-scc* scc-vec non-trivial-vec name->idx idx->name num-nodes edges)
  graph-scc?
  (scc-vec          graph-scc-scc-vec)         ;; node-idx -> scc-id (vector of ints)
  (non-trivial-vec  graph-scc-non-trivial-vec) ;; scc-id   -> bool   (vector)
  (name->idx        graph-scc-name->idx)       ;; hashtable name -> int
  (idx->name        graph-scc-idx->name)       ;; vector idx -> name
  (num-nodes        graph-scc-num-nodes)       ;; how many entries in idx->name
  (edges            graph-scc-edges))          ;; list of (u . v) integer-pair edges

(define (make-graph-scc scc-vec non-trivial-vec name->idx idx->name num-nodes edges)
  "Construct a <graph-scc> bundling an SCC decomposition with name-interning state.\n\nIntended for internal use by `%ensure-graph-scc!'. Validates: scc-vec\nis a vector of length num-nodes; non-trivial-vec is a vector; num-nodes\nis a non-negative integer; idx->name is a vector with at least num-nodes\nentries; edges is a list. Does NOT validate cross-vector index consistency\n(every scc-id in scc-vec must index non-trivial-vec) — that contract is\nenforced by the kernel and trusted on internal calls.\n\nParameters:\n  scc-vec : vector of non-negative integers\n  non-trivial-vec : vector of booleans\n  name->idx : hashtable\n  idx->name : vector\n  num-nodes : non-negative integer\n  edges : list of (integer . integer) pairs\nReturns: graph-scc\nCategory: algebra"
  (unless (and (integer? num-nodes) (exact? num-nodes) (>= num-nodes 0))
    (error "make-graph-scc: num-nodes must be a non-negative exact integer" num-nodes))
  (unless (and (vector? scc-vec) (= (vector-length scc-vec) num-nodes))
    (error "make-graph-scc: scc-vec must be a vector of length num-nodes"
           num-nodes (and (vector? scc-vec) (vector-length scc-vec))))
  (unless (vector? non-trivial-vec)
    (error "make-graph-scc: non-trivial-vec must be a vector" non-trivial-vec))
  (unless (vector? idx->name)
    (error "make-graph-scc: idx->name must be a vector" idx->name))
  (unless (>= (vector-length idx->name) num-nodes)
    (error "make-graph-scc: idx->name must have at least num-nodes entries"
           num-nodes (vector-length idx->name)))
  (unless (list? edges)
    (error "make-graph-scc: edges must be a list" edges))
  (make-graph-scc* scc-vec non-trivial-vec name->idx idx->name num-nodes edges))

;; The fast-path kernel (count-paths-in-dag) lives in (wile algebragraph),
;; which is an opt-in Go extension only present under the `kitchen-sink`
;; profile. Under smaller profiles the import in graph.sld is suppressed via
;; `cond-expand'; %fast-path-available? mirrors that decision so the eligibility
;; check in `make-graph-analysis' can short-circuit before referencing the
;; (then-unbound) kernel name. With the kernel absent, every `<graph-analysis>'
;; falls back to the pure-Scheme inner loop transparently.
(cond-expand
  ((library (wile algebragraph))
   (define %fast-path-available? #t))
  (else
   (define %fast-path-available? #f)))

;; The fast path interns node identifiers into a hashtable, which Wile's
;; `make-hashtable' restricts to atomic Hashable values (symbol, string,
;; number, char, boolean). The slow path accepts any `equal?'-comparable
;; value, including pairs, vectors, and lists. To keep the carrier opt
;; advisory (per `stdlib/lib/wile/algebra/CLAUDE.md' "Consumer libraries...
;; never error on unrecognised carrier"), we walk the adjacency at
;; construction time and suppress fast-path attachment if any node
;; identifier is non-atomic — the analysis falls back to the slow path
;; transparently.
(define (%atomic-node-id? v)
  (or (symbol? v) (string? v) (number? v) (char? v) (boolean? v)))

(define (%adjacency-keys-all-atomic? adj)
  (let outer ((entries adj))
    (cond
      ((null? entries) #t)
      ((not (%atomic-node-id? (caar entries))) #f)
      (else
       (let inner ((es (cdar entries)))
         (cond
           ((null? es) (outer (cdr entries)))
           ((not (%atomic-node-id? (caar es))) #f)
           (else (inner (cdr es)))))))))

;; --- Constructor ---

(define (make-graph-analysis semiring adjacency weight-fn)
  "Construct a graph analysis from a semiring, adjacency alist, and weight function.\nADJACENCY is an alist: ((node . ((neighbor . edge-data) ...)) ...).\nWEIGHT-FN receives edge-data and returns a semiring value.\nPass #f for unit weights (each edge = semiring-one).\n\nWhen the semiring declares carrier `'big-int' (via `(make-semiring ... '(carrier . big-int))' or the built-in `bigint-counting-semiring') AND `weight-fn' is `#f', this constructor attaches the unit-weight counting fast path (sub-path 4A of the bignum-allocation-reduction plan). Queries route through `count-paths-in-dag', which uses in-place `*big.Int' arithmetic instead of the per-relaxation allocating loop. A non-`#f' `weight-fn' falls through to the generic Scheme inner loop — weighted-bigint acceleration (sub-path 4B) is not yet implemented.\n\nUse `(graph-analysis-fast-path? ga)' to verify whether the fast path attached.\n\nExamples:\n  (make-graph-analysis (boolean-semiring)\n    '((\"A\" . ((\"B\" . 1))) (\"B\" . ()))\n    #f)\n\nParameters:\n  semiring : any\n  adjacency : list\n  weight-fn : procedure-or-false\nReturns: graph-analysis\nCategory: algebra\n\nSee also: `graph-query', `graph-query-all', `graph-analysis-fast-path?'."
  (let* ((carrier  (semiring-carrier semiring))
         (fast-kind (cond
                      ((and %fast-path-available?
                            (eq? carrier 'big-int)
                            (not weight-fn)
                            (%adjacency-keys-all-atomic? adjacency))
                       'bigint-counting)
                      (else #f)))
         (wfn (or weight-fn (lambda (_) (semiring-one semiring)))))
    (make-graph-analysis* semiring adjacency wfn '() fast-kind #f)))

;; --- Fast-path introspection ---

(define (graph-analysis-fast-path? ga)
  "Return #t iff GA has a fast-path strategy attached.\n\nThe fast path is non-#f when the semiring declares a carrier with a registered Go-side kernel and the constructor's other arguments are compatible. Currently the only attached strategy is `'bigint-counting' (bigint carrier + #f weight-fn).\n\nExamples:\n  (graph-analysis-fast-path? (make-graph-analysis (counting-semiring) '() #f))\n  => #f\n  (graph-analysis-fast-path? (make-graph-analysis (bigint-counting-semiring) '() #f))\n  => #t\n\nParameters:\n  ga : graph-analysis\nReturns: boolean\nCategory: algebra\n\nSee also: `graph-analysis-fast-path-kind', `make-graph-analysis'."
  (if (ga-fast-path-kind ga) #t #f))

(define (graph-analysis-fast-path-kind ga)
  "Return the symbol naming GA's fast-path strategy, or #f if none.\n\nKnown strategies:\n  'bigint-counting — bigint-carrier semiring + #f weight-fn,\n                         dispatches to `count-paths-in-dag' on acyclic\n                         input or `count-paths-cyclic' on cyclic input\n                         (the dispatcher pre-detects).\n\nExamples:\n  (graph-analysis-fast-path-kind (make-graph-analysis (bigint-counting-semiring) '() #f))\n  => bigint-counting\n\nParameters:\n  ga : graph-analysis\nReturns: symbol-or-false\nCategory: algebra\n\nSee also: `graph-analysis-fast-path?', `make-graph-analysis'."
  (ga-fast-path-kind ga))

;; --- SCC side-query API (Open Q-2 of plans/2026-05-26-scc-condensation.md) ---
;;
;; `graph-query' / `graph-query-all' remain shape-stable: the alist they
;; return is (name . count) on every node, whether the node lives in a
;; trivial or non-trivial SCC. The semantic shift on non-trivial SCCs
;; (the count is the SCC's entry-count, not a true per-node path count)
;; is surfaced through this side-query API: callers who need to
;; distinguish call `graph-node-in-cycle?' or walk `graph-cyclic-nodes'.
;; Callers who don't care see a plain integer alist.
;;
;; All three procedures force `%ensure-graph-scc!' on first use; the
;; result is cached on `ga-scc' and shared with the cyclic-counting
;; dispatch path.

(define (graph-analysis-sccs ga)
  "Force computation of the SCC decomposition for GA and return the\n<graph-scc> record. Idempotent: subsequent calls return the same\nobject (eq?). Requires the (wile algebragraph) extension — only loaded\nunder the kitchen-sink profile; raises otherwise.\n\nWorks on any carrier — SCC is a structural property of the adjacency,\nindependent of the analysis's semiring. A boolean-semiring analysis\nand a bigint-counting-semiring analysis built on the same adjacency\nshare the same SCC structure.\n\nThe <graph-scc> bundles six pieces of source-independent state\n(five exported as accessors, one internal):\n  graph-scc-scc-vec         — node-index -> SCC ID (vector of int)\n  graph-scc-non-trivial-vec — SCC ID -> #t/#f (true iff that SCC contains a cycle)\n  graph-scc-name->idx       — hashtable from node names to integer indices\n  graph-scc-idx->name       — vector from integer indices back to node names\n  graph-scc-num-nodes       — number of distinct nodes interned\n\nMost callers want `graph-node-in-cycle?' or `graph-cyclic-nodes'\ninstead; this is the lower-level introspection hook.\n\nExamples:\n  (let* ((ga (make-graph-analysis (bigint-counting-semiring)\n                                  '((a . ((b))) (b . ((a))))\n                                  #f))\n         (s (graph-analysis-sccs ga)))\n    (graph-scc? s))                       => #t\n\nParameters:\n  ga : graph-analysis\nReturns: graph-scc\nCategory: algebra\n\nSee also: `graph-node-in-cycle?', `graph-cyclic-nodes', `graph-scc?'."
  (%ensure-graph-scc! ga))

(define (graph-node-in-cycle? ga node)
  "Return #t iff NODE lies in a non-trivial SCC of GA's adjacency.\nA non-trivial SCC contains a cycle (either multiple mutually-reachable\nnodes, or a single node with a self-loop).\n\nRaises if NODE is not in GA's adjacency. This is the conservative\nchoice: a typo in a node name otherwise silently returns the same\n#f as a known-acyclic node, which masks consumer bugs. Use\n(member NODE (graph-cyclic-nodes ga)) if you need a non-raising\nmembership check.\n\nForces SCC computation on first call per GA; subsequent calls are\nO(1) hashtable + vector lookups.\n\nExamples:\n  (let ((ga (make-graph-analysis (bigint-counting-semiring)\n               '((a . ((b))) (b . ((a))) (c . ()))\n               #f)))\n    (graph-node-in-cycle? ga 'a))   => #t   ; a<->b is a 2-cycle\n  (let ((ga (make-graph-analysis (bigint-counting-semiring)\n               '((a . ((b))) (b . ((a))) (c . ()))\n               #f)))\n    (graph-node-in-cycle? ga 'c))   => #f   ; c is a trivial SCC\n\nParameters:\n  ga : graph-analysis\n  node : any (must be a node of GA's adjacency)\nReturns: boolean\nCategory: algebra\n\nSee also: `graph-cyclic-nodes', `graph-analysis-sccs'."
  (let* ((scc-rec (%ensure-graph-scc! ga))
         (idx     (hashtable-ref (graph-scc-name->idx scc-rec) node -1)))
    (cond
      ((< idx 0)
       (error "graph-node-in-cycle?: node is not in GA's adjacency"
              (list 'fix "use (member node (graph-cyclic-nodes ga)) for a non-raising check")
              node))
      (else
       (vector-ref (graph-scc-non-trivial-vec scc-rec)
                   (vector-ref (graph-scc-scc-vec scc-rec) idx))))))

(define (graph-cyclic-nodes ga)
  "Return the list of node names in GA that lie in non-trivial SCCs.\nA non-trivial SCC contains a cycle (multiple mutually-reachable nodes\nor a self-loop). Order matches the kernel's interning order, which for\ntypical adjacencies (where every edge target also appears as an\nadjacency key) matches adjacency-insertion order; for adjacencies\nwith targets that don't appear as keys, the targets sort after the\nlast key in interning order.\n\nForces SCC computation on first call per GA. Returns '() on a\ncompletely acyclic graph and on the empty graph.\n\nExamples:\n  (let ((ga (make-graph-analysis (bigint-counting-semiring)\n               '((a . ((b))) (b . ((a))) (c . ((b))))\n               #f)))\n    (graph-cyclic-nodes ga))    => (a b)\n\nParameters:\n  ga : graph-analysis\nReturns: list\nCategory: algebra\n\nSee also: `graph-node-in-cycle?', `graph-analysis-sccs'."
  (let* ((scc-rec   (%ensure-graph-scc! ga))
         (n         (graph-scc-num-nodes scc-rec))
         (scc-vec   (graph-scc-scc-vec scc-rec))
         (nt-vec    (graph-scc-non-trivial-vec scc-rec))
         (idx->name (graph-scc-idx->name scc-rec)))
    (let loop ((i 0) (acc '()))
      (cond
        ((= i n) (reverse acc))
        ((vector-ref nt-vec (vector-ref scc-vec i))
         (loop (+ i 1) (cons (vector-ref idx->name i) acc)))
        (else (loop (+ i 1) acc))))))

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
    ((bigint-counting)
     ;; Kernel-first dispatch. count-paths-in-dag does its own O(V+E)
     ;; cycle detection internally (Go side, hashtable-backed) and returns
     ;; #f on cyclic input. Running topological-order-from in Scheme first
     ;; would duplicate that work using O(V) assoc lookups per node — slow
     ;; enough to erase the fast-path speedup on large DAGs (Copilot
     ;; finding on PR #759).
     ;;
     ;; LOAD-BEARING CONTRACT: compute-via-count-paths-in-dag must return
     ;;   - an alist (truthy)         on acyclic input with reachable nodes
     ;;   - '() (truthy in Scheme)    on source-not-in-graph
     ;;   - #f (the ONLY falsy value) on cyclic input reachable from source
     ;; The `or' below relies on this trichotomy: '() short-circuits to an
     ;; empty alist (correct), #f falls through to the cyclic adapter. A
     ;; future refactor that returns a different falsy/truthy shape (e.g.
     ;; 'cyclic symbol, or wrapping the alist in a record) silently
     ;; misroutes — see `test-group "dispatch contract: ..."' in
     ;; algebra-graph-test.scm for the regression canary.
     ;;
     ;; If ga-scc is already populated, the graph is known cyclic
     ;; (population only happens via the cyclic kernel or the side-query
     ;; API), so skip the wasted DAG call and route directly. Acyclic
     ;; graphs that haven't had graph-analysis-sccs called on them never
     ;; see the cyclic path.
     (cond
       ((ga-scc ga)
        (compute-via-count-paths-cyclic ga source))
       (else
        (or (compute-via-count-paths-in-dag ga source)
            (compute-via-count-paths-cyclic ga source)))))
    (else
     (call-with-values
       (lambda () (topological-order-from ga source))
       (lambda (order cyclic?)
         (if cyclic?
             (compute-via-worklist ga source)
             (compute-via-topological-order ga source order)))))))

;; --- Sub-path 4A and 4C: name interning shared by all bigint dispatch paths ---
;;
;; The kernels `count-paths-in-dag' and `count-paths-cyclic' (in
;; (wile algebragraph)) speak integer node indices. The Scheme-side
;; adapters need to translate between Wile's name-keyed adjacency and
;; that integer-indexed kernel protocol. The interning step is identical
;; for both kernels, so it lives here as a shared helper rather than
;; duplicated inside each adapter.
;;
;; Returns 4 values: name->idx hashtable, idx->name vector,
;; num-nodes integer (distinct names interned), and edges list of (u . v)
;; integer-index pairs. The hashtable + vector are exactly what the
;; <graph-scc> cache stores; the cyclic dispatch path keeps both, while
;; the DAG dispatch path discards them after one kernel call.
;;
;; Name->index translation uses a hashtable for O(1) lookups; the naive
;; alist version was O(V*E) for setup and dominated the kernel's actual
;; arithmetic cost on graphs >100 nodes.
(define (%intern-adjacency-for-kernel adj)
  (let ((name->idx (make-hashtable))
        (idx->name (make-vector 16))
        (next-idx  0))
    ;; Intern: return existing idx for name, or assign next available.
    ;; `idx->name' is mutated via `set!' when capacity doubles, because
    ;; the inner `intern!' closure captures the original binding and
    ;; needs to observe the reassignment.
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
    (let ((edges
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
                          (inner (cdr es) (cons (cons u v) acc2))))))))))))
      (values name->idx idx->name next-idx edges))))

;; Shared projection helper: walk node indices [0, num-nodes), look each
;; one up in `counts-vec' (length num-nodes), and emit
;; (idx->name[i] . counts-vec[i]) for non-zero entries. Used by the DAG
;; adapter directly. The cyclic adapter uses a one-level-indirect variant
;; (counts via scc-vec) and inlines its own loop.
(define (%project-counts-to-alist idx->name counts-vec num-nodes)
  (let l ((i 0) (acc '()))
    (cond
      ((= i num-nodes) acc)
      (else
       (let ((c (vector-ref counts-vec i)))
         (cond
           ((zero? c) (l (+ i 1) acc))
           (else
            (l (+ i 1)
               (cons (cons (vector-ref idx->name i) c) acc)))))))))

;; --- Sub-path 4A: unit-weight bigint counting via count-paths-in-dag ---
;;
;; LOAD-BEARING CONTRACT (consumed by compute-single-source's `or'-dispatch):
;;   - alist of (name . count) on acyclic input with reachable nodes (TRUTHY)
;;   - '() if SOURCE is not in the adjacency                          (TRUTHY)
;;   - #f if the reachable subgraph contains a cycle                  (FALSY)
;;
;; The dispatcher's `(or (compute-via-count-paths-in-dag ga source)
;;                       (compute-via-count-paths-cyclic ga source))'
;; distinguishes the FALSY case (#f -> fall through to cyclic adapter)
;; from the TRUTHY cases ('() and alist -> short-circuit, return as-is).
;; Do NOT change the falsy value to e.g. 'cyclic-symbol or wrap returns
;; in a record without also updating the dispatcher — the asymmetry
;; between `'()' (truthy) and `#f' (falsy) is the entire signal.
;;
;; Regression canary: test-group "dispatch contract: ..." in
;; algebra-graph-test.scm pins this trichotomy explicitly.
;;
;; Implementation: the kernel (count-paths-in-dag, in Go) does its own
;; O(V+E) cycle detection internally; relying on its #f return saves
;; the dispatcher a redundant Scheme-side topological pass.
;;
;; `cond-expand' lives inside the function body so the top-level binding
;; `compute-via-count-paths-in-dag' is visible to the dispatcher
;; regardless of profile. Under profiles without (wile algebragraph) the
;; body is a stub `error' — but `%fast-path-available?' ensures
;; `make-graph-analysis' never assigns `'bigint-counting' there, so
;; the stub is unreachable from the public surface.
(define (compute-via-count-paths-in-dag ga source)
  (cond-expand
    ((library (wile algebragraph))
     (call-with-values
       (lambda () (%intern-adjacency-for-kernel (ga-adjacency ga)))
       (lambda (name->idx idx->name num-nodes edges)
         (let ((src-idx (hashtable-ref name->idx source -1)))
           (cond
             ;; Source not in graph — match the slow path's permissive
             ;; behaviour: return an empty distance alist so `graph-query'
             ;; surfaces `semiring-zero' for every target. The fast path
             ;; must not narrow the contract that the carrier opt is
             ;; documented to leave unchanged.
             ((< src-idx 0) '())
             (else
              (let ((counts (count-paths-in-dag num-nodes edges src-idx)))
                (cond
                  ;; #f from the kernel = reachable subgraph is cyclic.
                  ;; Propagate to the dispatcher; do not raise — the cyclic
                  ;; adapter handles this case.
                  ((not counts) #f)
                  (else
                   (%project-counts-to-alist idx->name counts num-nodes))))))))))
    (else
     (error
      (string-append
       "compute-via-count-paths-in-dag: (wile algebragraph) extension not "
       "loaded; %fast-path-available? was #f so this dispatch path is "
       "unreachable from `make-graph-analysis' — this is an internal "
       "invariant violation, please file a bug.")))))

;; --- Sub-path 4C: cyclic-graph counting via count-paths-cyclic (SCC) ---
;;
;; Bridges between (wile algebra graph)'s name-keyed adjacency surface
;; and the SCC-condensation kernel from (wile algebragraph). The kernel
;; Tarjan-condenses the graph and runs in-place big.Int arithmetic on
;; the resulting DAG; this wrapper does the name<->index translation
;; (cached on `ga-scc' so multi-source workloads don't re-walk the
;; adjacency) and projects the (counts-by-scc, scc-vec) pair back into
;; the per-node alist that callers of `graph-query' / `graph-query-all'
;; expect.
;;
;; Per-node semantics on non-trivial SCCs: every node in a non-trivial
;; SCC reports the SCC's *entry count* (paths from source's SCC into
;; this SCC in the condensed DAG), not a true within-SCC path count
;; (which is infinite). Callers who need to distinguish use
;; `graph-node-in-cycle?' or `graph-cyclic-nodes' — the side-query API.
;; The alist shape is unchanged.
;;
;; The kernel re-runs SCC every call (the SCC step is internal to
;; `count-paths-cyclic'), so multi-source workloads pay O(V+E) for SCC
;; per source. A future kernel split (separate `compute-sccs' primitive)
;; could eliminate that; tracked in the parent plan's "out of scope"
;; section. The cache here still wins by skipping the interning re-walk
;; and by sharing `scc-vec' with the side-query API.
(define (compute-via-count-paths-cyclic ga source)
  (cond-expand
    ((library (wile algebragraph))
     (let* ((scc-record (%ensure-graph-scc! ga))
            (name->idx  (graph-scc-name->idx scc-record))
            (idx->name  (graph-scc-idx->name scc-record))
            (num-nodes  (graph-scc-num-nodes scc-record))
            (scc-vec    (graph-scc-scc-vec   scc-record))
            (edges      (graph-scc-edges     scc-record))
            (src-idx    (hashtable-ref name->idx source -1)))
       (cond
         ((< src-idx 0) '())
         (else
          (call-with-values
            (lambda () (count-paths-cyclic num-nodes edges src-idx))
            (lambda (_kernel-scc-vec counts-by-scc _kernel-nt-vec)
              ;; Walk node indices; project (idx->name[i] . counts-by-scc[scc-vec[i]])
              ;; for non-zero entries. The returned scc-vec / non-trivial-vec
              ;; from this kernel call match the cached versions on
              ;; `scc-record' (SCC is source-independent) and are discarded.
              (let l ((i 0) (acc '()))
                (cond
                  ((= i num-nodes) acc)
                  (else
                   (let* ((s (vector-ref scc-vec i))
                          (c (vector-ref counts-by-scc s)))
                     (cond
                       ((zero? c) (l (+ i 1) acc))
                       (else
                        (l (+ i 1)
                           (cons (cons (vector-ref idx->name i) c)
                                 acc))))))))))))))
    (else
     (error
      (string-append
       "compute-via-count-paths-cyclic: (wile algebragraph) extension not "
       "loaded; %fast-path-available? was #f so this dispatch path is "
       "unreachable from `make-graph-analysis' — this is an internal "
       "invariant violation, please file a bug.")))))

;; %ensure-graph-scc! — populate ga-scc if not yet computed. Picks an
;; arbitrary kernel source (node-idx 0) for the call; the returned
;; counts-by-scc is discarded because that's source-dependent and the
;; user's query will re-invoke the kernel for its actual source. We only
;; harvest the source-independent scc-vec + non-trivial-vec.
;;
;; Returns the `<graph-scc>' (newly created or pre-existing). Empty
;; adjacency yields a 0-vector pair; the kernel is not called because
;; `count-paths-cyclic' requires source < num-nodes.
(define (%ensure-graph-scc! ga)
  (or (ga-scc ga)
      (cond-expand
        ((library (wile algebragraph))
         (cond
           ;; The kernel interns node identifiers into Wile's make-hashtable,
           ;; which restricts keys to atomic Hashable values (symbol, string,
           ;; number, char, boolean). Public APIs (graph-analysis-sccs,
           ;; graph-node-in-cycle?, graph-cyclic-nodes) can be called on
           ;; analyses whose adjacency has non-atomic node IDs (pairs, vectors,
           ;; lists) — for those, the fast-path-eligibility check at
           ;; make-graph-analysis already suppresses the dispatch fast path,
           ;; but the side-query API still routes here. Surface the
           ;; restriction explicitly rather than letting hashtable-set! fail
           ;; mid-walk with an opaque error (Copilot finding on PR #759).
           ((not (%adjacency-keys-all-atomic? (ga-adjacency ga)))
            (error
             (string-append
              "%ensure-graph-scc!: cannot compute SCC on adjacency with "
              "non-atomic node identifiers. The Go kernel's name-interning "
              "hashtable requires atomic keys (symbol, string, number, "
              "char, boolean); pairs, vectors, and lists are rejected. "
              "Reachable from graph-analysis-sccs, graph-node-in-cycle?, "
              "graph-cyclic-nodes, and the cyclic-counting dispatch.")
             (list 'fix
                   "use atomic node identifiers, or stay within the slow-path "
                   "API (graph-query / graph-query-all) which accepts any "
                   "equal?-comparable values.")))
           (else
            (call-with-values
              (lambda () (%intern-adjacency-for-kernel (ga-adjacency ga)))
              (lambda (name->idx idx->name num-nodes edges)
                (cond
                  ((zero? num-nodes)
                   (let ((rec (make-graph-scc* (make-vector 0) (make-vector 0)
                                               name->idx idx->name 0 edges)))
                     (set-ga-scc! ga rec)
                     rec))
                  (else
                   (call-with-values
                     (lambda () (count-paths-cyclic num-nodes edges 0))
                     (lambda (scc-vec _counts non-trivial-vec)
                       (let ((rec (make-graph-scc*
                                    scc-vec non-trivial-vec
                                    name->idx idx->name num-nodes edges)))
                         (set-ga-scc! ga rec)
                         rec))))))))))
        (else
         (error
          (string-append
           "%ensure-graph-scc!: SCC computation requires the (wile "
           "algebragraph) extension, which is only loaded under the "
           "kitchen-sink profile. Reachable from graph-analysis-sccs, "
           "graph-node-in-cycle?, graph-cyclic-nodes, and the cyclic-"
           "counting dispatch."))))))

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
;;
;; No `semiring-eq?' call here, by design: the topo-order loop does not iterate
;; to a fixpoint. Each node is visited exactly once and each edge relaxed
;; exactly once. There is no convergence to detect, so no equality predicate
;; is consulted on semiring values. The `assoc' / `equal?' calls below compare
;; *node identifiers* (alist keys), not semiring values, and intentionally use
;; the host equality contract regardless of the semiring's declared equality.
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
                          ;; Convergence detection uses the semiring's declared
                          ;; equality (defaulting to `equal?'). This lets carriers
                          ;; with non-canonical representations — log-space floats
                          ;; with tolerance, modular Z/PZ values requiring
                          ;; normalization — terminate the worklist without
                          ;; spinning on representational-but-not-value differences.
                          (if (semiring-eq? S merged old-val)
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
