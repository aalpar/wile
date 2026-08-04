;;; (wile algebra combinatorial-graph) — Graphs as combinatorial objects.
;;;
;;; Distinct from (wile algebra graph) (which does semiring-parameterized
;;; path analytics). This library treats graphs as objects of combinatorial
;;; interest: isomorphism, chromatic/Tutte polynomials, spanning-tree count,
;;; bipartite matching.
;;;
;;; Adjacency shape (matching graph.sld for ergonomic interop):
;;;
;;;   simple (multi? = #f):
;;;     ((vertex . ((neighbor . edge-data) ...)) ...)
;;;     each neighbor appears at most once per vertex
;;;
;;;   multi   (multi? = #t):
;;;     ((vertex . ((neighbor . edge-data) ...)) ...)
;;;     neighbor may repeat; each inner pair is one edge instance
;;;
;;; Terminology (McKay & Piperno 2014, §2.2): "cell" and "color class" are
;;; used interchangeably; "stable partition" = 1-WL fixed point; "discrete"
;;; = every cell is a singleton; "non-trivial cell" = cardinality ≥ 2.
;;;
;;; Graph-theory terms (Diestel §1.1): "order" = |V|, "size" = |E|, "loop"
;;; = self-edge; "bridge" = edge whose removal increases component count.

;;; -- Record type --

(define-record-type <graph>
  (%make-graph adjacency directed? multi? self-loops? setoid
               order size seed neighbor-fn reverse-cached)
  graph?
  (adjacency   graph-adjacency)
  (directed?   graph-directed?)
  (multi?      graph-multi?)
  (self-loops? graph-self-loops?)
  (setoid      graph-setoid)
  (order       graph-order)
  (size        graph-size)
  (seed        graph-seed)
  (neighbor-fn graph-neighbor-fn)
  ;; reverse-cached: lazily populated <graph> with edges reversed, or #f
  ;; if not yet computed. Source-independent and immutable once set, so the
  ;; mutator is purely a memoization hook (compare ga-cache in graph.scm).
  ;; Undirected graphs leave this #f because graph-reverse short-circuits.
  (reverse-cached graph-reverse-cached set-graph-reverse-cached!))

;;; -- Symmetrization for undirected graphs --

(define (%symmetrize-adjacency adj S)
  ;; For each directed edge u→v in ADJ, ensure v→u is also present.
  ;; Preserves vertex order and edge payloads; adds reverse-edge entries
  ;; using the original payload so data survives the round trip.
  (let* ((out-edges
           ;; (u v edge-data) triples for every directed edge
           (append-map
             (lambda (entry)
               (let ((u (car entry)))
                 (map (lambda (p) (list u (car p) (cdr p))) (cdr entry))))
             adj)))
    (map
      (lambda (entry)
        (let* ((u (car entry))
               (existing (cdr entry))
               (incoming
                 (filter-map
                   (lambda (triple)
                     (and (setoid-equiv? S (cadr triple) u)
                          (not (setoid-equiv? S (car triple) u))
                          ;; reverse edge v→u with the original payload
                          (cons (car triple) (caddr triple))))
                   out-edges))
               ;; Keep only reverse edges not already present.
               (to-add
                 (filter
                   (lambda (p)
                     (not (setoid-assoc S (car p) existing)))
                   incoming))
               (merged (append existing (setoid-dedup S to-add))))
          (cons u merged)))
      adj)))

;;; -- Adjacency derivations --

(define (%adj-vertices adj)
  (map car adj))

(define (%adj-edges adj directed? S)
  ;; Enumerate edges. For undirected, a pair (u,v) is yielded once: the
  ;; canonical representative is the lex-first in vertex order (where
  ;; "vertex order" = position in the adjacency alist; u comes before v
  ;; iff u appears earlier in ADJ). Self-loops are always yielded once.
  ;; Returns a list of (u v edge-data) triples.
  (let* ((vs (%adj-vertices adj))
         (pos-of
           (lambda (v)
             (let loop ((xs vs) (i 0))
               (cond
                 ((null? xs) #f)
                 ((setoid-equiv? S v (car xs)) i)
                 (else (loop (cdr xs) (+ i 1))))))))
    (append-map
      (lambda (entry)
        (let* ((u (car entry))
               (u-pos (pos-of u)))
          (filter-map
             (lambda (p)
               (let* ((v (car p))
                      (data (cdr p))
                      (v-pos (pos-of v)))
                 (cond
                   (directed?
                    (list u v data))
                   ;; undirected: emit once (u ≤ v in position)
                   ((and u-pos v-pos (<= u-pos v-pos))
                    (list u v data))
                   (else #f))))
             (cdr entry))))
      adj)))

;;; -- Constructor --

(define (make-graph adjacency . opts)
  "Construct a graph from an ADJACENCY alist. Shape:\n  ((vertex . ((neighbor . edge-data) ...)) ...)\n\nOptional trailing alist entries:\n  (directed? . BOOL)    — default #f (undirected)\n  (multi? . BOOL)       — default #f (simple graph; no parallel edges)\n  (self-loops? . BOOL)  — default #t (loops permitted)\n  (setoid . S)          — vertex equivalence (defaults to default-setoid)\n  (symmetrize? . BOOL)  — undirected-only: auto-add reverse edges so the\n                          adjacency is symmetric. Default #f.\n  (seed . VERTEX)       — tier-2: starting vertex for BFS enumeration\n  (neighbor-fn . PROC)  — tier-2: v → ((neighbor . edge-data) ...)\n  (max-size . N)        — tier-2: bound for enumerate-finite-graph closure\n\nExamples:\n  (define K3\n    (make-graph '((a . ((b) (c))) (b . ((a) (c))) (c . ((a) (b))))))\n  (graph-order K3)  => 3\n  (graph-size K3)   => 3\n\nParameters:\n  adjacency : list\n  opts : alist\nReturns: graph\nCategory: algebra\nKeywords: graph, adjacency, combinatorial graph, graph theory\n\nSee also: `validate-graph', `complete-graph', `graph-isomorphic?'."
  (validate-opts-keys "make-graph" opts
    '(directed? multi? self-loops? setoid symmetrize?
      seed neighbor-fn max-size))
  (let* ((directed?   (assv-or opts 'directed?   #f))
         (multi?      (assv-or opts 'multi?      #f))
         (self-loops? (assv-or opts 'self-loops? #t))
         (setoid      (assv-or opts 'setoid      (default-setoid)))
         (symmetrize? (assv-or opts 'symmetrize? #f))
         (seed        (assv-or opts 'seed        #f))
         (nfn         (assv-or opts 'neighbor-fn #f))
         (adj
           (cond
             ((and (not directed?) symmetrize?)
              (%symmetrize-adjacency adjacency setoid))
             (else adjacency))))
    (unless (list? adj)
      (error "make-graph: adjacency must be a list" adj))
    (when nfn (assert-procedure "make-graph" nfn)) ; neighbor-fn is optional; check only when present
    ;; Tier-1 and tier-2 are mutually exclusive (Q-d).
    (when (and (pair? adj) (or seed nfn))
      (error "make-graph: non-empty adjacency conflicts with seed/neighbor-fn"
             (list 'fix
               "choose ONE: pass adjacency (tier-1) OR pass (seed . v) + (neighbor-fn . proc) (tier-2)")))
    ;; symmetrize? collapses multi-edge multiplicity; reject the
    ;; combination (reviewer Copilot / consistency). A multigraph that
    ;; needs symmetrization should symmetrize its edges at the adjacency
    ;; layer before calling make-graph.
    (when (and symmetrize? multi?)
      (error "make-graph: (symmetrize? . #t) is incompatible with (multi? . #t)"
             (list 'fix
               "symmetrize the adjacency manually before calling make-graph, or drop multi?")))
    ;; Deeper adjacency shape validation: each entry must be
    ;; (vertex . ((neighbor . edge-data) ...)). Catches typos at
    ;; construction time.
    (for-each
      (lambda (entry)
        (unless (pair? entry)
          (error "make-graph: adjacency entry must be a pair" entry))
        (unless (list? (cdr entry))
          (error "make-graph: adjacency entry's cdr must be a list of (neighbor . data) pairs"
                 entry))
        (for-each
          (lambda (p)
            (unless (pair? p)
              (error "make-graph: neighbor entry must be a (neighbor . edge-data) pair"
                     (car entry) p)))
          (cdr entry)))
      adj)
    (let* ((vs    (%adj-vertices adj))
           (order (if (pair? adj) (length vs) 0))
           (edges (%adj-edges adj directed? setoid))
           (size  (length edges)))
      (%make-graph adj directed? multi? self-loops? setoid
                   order size seed nfn #f))))

;;; -- Accessors on derived data --

(define (graph-vertices G)
  "Return the list of vertices of G in adjacency-order.\n\nExamples:\n  (graph-vertices (complete-graph 3))  => (0 1 2)\n\nParameters:\n  G : graph\nReturns: list\nCategory: algebra\nKeywords: vertices, nodes\n\nSee also: `graph-edges', `graph-order'."
  (%adj-vertices (graph-adjacency G)))

(define (graph-edges G)
  "Return the list of edges of G as (u v edge-data) triples.\nFor undirected graphs each edge appears once; for directed, the\nnatural directed triples.\n\nExamples:\n  (length (graph-edges (complete-graph 3)))  => 3\n\nParameters:\n  G : graph\nReturns: list\nCategory: algebra\nKeywords: edges, edge list\n\nSee also: `graph-vertices', `graph-size'."
  (%adj-edges (graph-adjacency G) (graph-directed? G) (graph-setoid G)))

(define (graph-has-vertex? G v)
  "Return #t if V is a vertex of G (under G's setoid).\n\nParameters:\n  G : graph\n  v : vertex\nReturns: boolean\nCategory: algebra\nKeywords: vertex, membership, graph\n\nSee also: `graph-neighbors', `graph-vertex-equiv?'."
  (and (setoid-assoc (graph-setoid G) v (graph-adjacency G)) #t))

(define (graph-neighbors G v)
  "Return the neighbor alist for vertex V in G as ((neighbor . edge-data) ...).\nRaises if V is not a vertex of G (use `graph-has-vertex?' to test).\n\nParameters:\n  G : graph\n  v : vertex\nReturns: list\nCategory: algebra\nKeywords: neighbors, adjacency\n\nSee also: `graph-degree', `graph-edge?', `graph-has-vertex?'."
  (let ((entry (setoid-assoc (graph-setoid G) v (graph-adjacency G))))
    (unless entry
      (error "graph-neighbors: vertex is not a member of G"
             (list 'fix "use graph-has-vertex? to test membership first")
             v))
    (cdr entry)))

(define (graph-degree G v)
  "Return the degree of V in G.\nFor undirected graphs, a loop at V contributes 2 to the degree (both\nendpoints incident); a non-loop contributes 1.\nFor directed graphs, this returns the out-degree. In-degree and\ntotal-degree are not exported in v1.\n\nRaises if V is not a vertex of G.\n\nExamples:\n  (graph-degree (cycle-graph 4) 0)  => 2\n\nParameters:\n  G : graph\n  v : vertex\nReturns: non-negative integer\nCategory: algebra\nKeywords: degree, valence, loop"
  (unless (graph-has-vertex? G v)
    (error "graph-degree: vertex is not a member of G"
           (list 'fix "use graph-has-vertex? to test membership first") v))
  (let* ((S    (graph-setoid G))
         (nbrs (graph-neighbors G v)))
    (if (graph-directed? G)
        (length nbrs)
        (fold
          (lambda (p acc)
            (if (setoid-equiv? S (car p) v)
                (+ acc 2)    ;; loop: both endpoints at v
                (+ acc 1)))
          0
          nbrs))))

(define (graph-reverse G)
  "Return G with every directed edge reversed.\n\nFor undirected graphs the reverse is the same graph (an undirected\nedge has no direction to flip); G itself is returned, no copy.\n\nFor directed graphs a fresh <graph> is built with the inverted\nadjacency: an edge u→v in G becomes v→u in the result. The result\nis cached on G so repeated calls return the same object (eq?).\nThe directed?, multi?, self-loops?, and setoid options are preserved.\nTier-2 generator metadata (seed, neighbor-fn) is dropped — the\nreverse adjacency is fully enumerated.\n\nExamples:\n  (graph-reverse (cycle-graph 3))                          ;; undirected → eq?\n  (graph-edges (graph-reverse\n                 (make-graph '((a . ((b))) (b . ()))\n                             '(directed? . #t))))\n  => ((b a #f))\n\nParameters:\n  G : graph\nReturns: graph\nCategory: algebra\nKeywords: reverse, transpose, inverted adjacency, in-degree, predecessors\n\nSee also: `graph-in-degree', `graph-predecessors'."
  (cond
    ((not (graph-directed? G)) G)
    (else
     (let ((cached (graph-reverse-cached G)))
       (or cached
           (let ((rev (%compute-reverse G)))
             (set-graph-reverse-cached! G rev)
             rev))))))

;; Vertices on which `equal?' bucketing is known to agree with `default-setoid'.
;;
;; This predicate USED to mean "hashable", mirroring %atomic-node-id? /
;; %adjacency-keys-all-atomic? in (wile algebra graph), because Wile's
;; hashtables admitted only atomic keys. The R6RS hashtable work moved the hash
;; from the KEY to the TABLE, so every vertex is hashable now and those two
;; symbols are gone — but this gate MUST NOT go with them. It is what still
;; selects between the two reverse-adjacency algorithms, and container vertices
;; are the only case that reaches the setoid path at all: dropping the gate would
;; make %compute-reverse-setoid dead code and silently stop honouring a
;; non-default setoid entirely.
;;
;; So the name changed and the meaning narrowed to what it was always really
;; deciding: is `equal?' an acceptable stand-in for this graph's setoid?
;; Whether atomicity is the right proxy for THAT question is a separate one,
;; filed in TODO.md.
(define (%equal-safe-vertex? v)
  (or (symbol? v) (string? v) (number? v) (char? v) (boolean? v)))

(define (%adj-vertices-all-equal-safe? adj)
  (let outer ((entries adj))
    (cond
      ((null? entries) #t)
      ((not (%equal-safe-vertex? (caar entries))) #f)
      (else
       (let inner ((es (cdar entries)))
         (cond
           ((null? es) (outer (cdr entries)))
           ((not (%equal-safe-vertex? (caar es))) #f)
           (else (inner (cdr es)))))))))

(define (%compute-reverse G)
  (let* ((adj (graph-adjacency G))
         (vs  (%adj-vertices adj))
         (rev-adj
           (cond
             ((%adj-vertices-all-equal-safe? adj)
              (%compute-reverse-hashed adj vs))
             (else
              (%compute-reverse-setoid (graph-setoid G) adj vs)))))
    ;; Call make-graph rather than %make-graph: we want the same shape
    ;; validation, order/size derivation, and reverse-cached=#f init that
    ;; user-constructed graphs get. The cost is one extra adjacency walk
    ;; in make-graph's validator; trivial relative to graph-reverse itself.
    (make-graph rev-adj
                (cons 'directed?   #t)
                (cons 'multi?      (graph-multi?      G))
                (cons 'self-loops? (graph-self-loops? G))
                (cons 'setoid      (graph-setoid G)))))

;; Fast path: O(V+E) via single-pass hashtable accumulation.
;; The table is equal?-keyed, which agrees with default-setoid on atomic keys.
;; A non-default setoid that identifies two distinct atomic values (e.g.
;; numeric-setoid with 1 and 1.0) gets equal? bucketing here, which can produce
;; a different — but equally well-defined — reverse adjacency than the setoid
;; path below. %equal-safe-vertex? is what bounds that trade-off.
;;
;; (wile algebra graph) used to gate its bigint fast path the same way and no
;; longer does — its gate was purely about key admission, which no longer
;; restricts anything. This one is NOT the same gate despite the shared
;; ancestry, so the two libraries are deliberately no longer in lockstep.
(define (%compute-reverse-hashed adj vs)
  (let ((preds (make-equal-hashtable)))
    ;; Walk forward adjacency once, prepending (u . d) onto v's
    ;; predecessor list. O(1) per edge. After the walk, each list is in
    ;; reverse-discovery order; one final per-vertex `reverse' restores
    ;; the source-order graph-predecessors documents
    ;; ("Order matches the order predecessors appear in the underlying
    ;; reverse adjacency"). Total work: O(V + E) — strictly better than
    ;; the setoid path's O(V * E) and the prior naive append's O(E^2).
    (for-each
      (lambda (entry)
        (let ((u (car entry)))
          (for-each
            (lambda (p)
              (let ((v (car p)))
                (hashtable-set! preds v
                  (cons (cons u (cdr p))
                        (hashtable-ref preds v '())))))
            (cdr entry))))
      adj)
    (map
      (lambda (v) (cons v (reverse (hashtable-ref preds v '()))))
      vs)))

;; Slow path: O(V*E) — kept verbatim for graphs whose vertex keys aren't
;; make-hashtable-compatible (pairs, vectors, records) or whose setoid
;; mismatches equal? in a way the caller relies on. Self-loops survive
;; verbatim: u->u in G is still u->u in the reverse.
(define (%compute-reverse-setoid S adj vs)
  (let ((rev-edges
          (append-map
            (lambda (entry)
              (let ((u (car entry)))
                (map (lambda (p) (cons (car p) (cons u (cdr p))))
                     (cdr entry))))
            adj)))
    (map
      (lambda (v)
        (cons v
              (filter-map
                (lambda (re)
                  (and (setoid-equiv? S (car re) v) (cdr re)))
                rev-edges)))
      vs)))

(define (graph-in-degree G v)
  "Return the in-degree of V in G.\nFor undirected graphs (where in- and out-degree coincide) this is the\nsame as `graph-degree' — a loop contributes 2. For directed graphs it\nreturns the count of edges arriving at V; a loop contributes 1 (one\nin-edge and one out-edge per directed-graph convention).\n\nRaises if V is not a vertex of G.\n\nExamples:\n  (graph-in-degree (cycle-graph 4) 0)  => 2\n  (graph-in-degree (make-graph '((a . ((b))) (b . ()))\n                               '(directed? . #t))\n                   'b)                 => 1\n\nParameters:\n  G : graph\n  v : vertex\nReturns: non-negative integer\nCategory: algebra\nKeywords: in-degree, predecessors, directed, valence\n\nSee also: `graph-degree', `graph-predecessors', `graph-reverse'."
  (unless (graph-has-vertex? G v)
    (error "graph-in-degree: vertex is not a member of G"
           (list 'fix "use graph-has-vertex? to test membership first") v))
  (cond
    ((not (graph-directed? G)) (graph-degree G v))
    (else (length (graph-neighbors (graph-reverse G) v)))))

(define (graph-predecessors G v)
  "Return the predecessor alist of V in G as ((u . edge-data) ...).\nFor each edge u→v in G (where u may equal v for a self-loop) the\nresult contains the (u . edge-data) pair. Order matches the order\npredecessors appear in the underlying reverse adjacency.\n\nFor undirected graphs `graph-predecessors' returns the same alist as\n`graph-neighbors' because every neighbor is both a successor and a\npredecessor.\n\nRaises if V is not a vertex of G.\n\nExamples:\n  (graph-predecessors (make-graph '((a . ((c))) (b . ((c))) (c . ()))\n                                  '(directed? . #t))\n                      'c)\n  => ((a . #f) (b . #f))\n\nParameters:\n  G : graph\n  v : vertex\nReturns: list of (vertex . edge-data) pairs\nCategory: algebra\nKeywords: predecessors, in-neighbors, directed, reverse adjacency\n\nSee also: `graph-neighbors', `graph-in-degree', `graph-reverse'."
  (unless (graph-has-vertex? G v)
    (error "graph-predecessors: vertex is not a member of G"
           (list 'fix "use graph-has-vertex? to test membership first") v))
  (graph-neighbors (graph-reverse G) v))

(define (graph-edge? G u v)
  "Return #t if there is an edge from U to V in G.\nFor undirected graphs, symmetric: (graph-edge? G u v) ⟺ (graph-edge? G v u).\n\nExamples:\n  (graph-edge? (complete-graph 3) 0 1)  => #t\n  (graph-edge? (empty-graph 3) 0 1)     => #f\n\nParameters:\n  G : graph\n  u : vertex\n  v : vertex\nReturns: boolean\nCategory: algebra\nKeywords: edge, incidence, adjacency"
  (let ((S (graph-setoid G)))
    (and (setoid-assoc S v (graph-neighbors G u)) #t)))

(define (graph-vertex-equiv? G u v)
  "Return #t if U and V are equivalent under G's vertex setoid.\n\nParameters:\n  G : graph\n  u : vertex\n  v : vertex\nReturns: boolean\nCategory: algebra\nKeywords: setoid, vertex equality, equivalence"
  (setoid-equiv? (graph-setoid G) u v))

;;; -- Tier predicates --

(define (finite-graph? G)
  "Return #t if G has an enumerated adjacency (tier-1).\n\nTier-1 means the adjacency alist is populated. At user-level\nconstruction, tier-1 and tier-2 are mutually exclusive (Q-d). Internally\n(`enumerate-finite-graph') may produce a graph that is both tier-1 and\ntier-2 — both predicates return #t in that case, reflecting that the\nadjacency and the generator are jointly available.\n\nAn empty adjacency '() counts as tier-1 only when no seed/neighbor-fn\nare present (otherwise it is a tier-2 stub awaiting enumeration).\n\nParameters:\n  G : graph\nReturns: boolean\nCategory: algebra\nKeywords: finite, tier-1, enumerated\n\nSee also: `finitely-generated-graph?', `enumerate-finite-graph'."
  (or (pair? (graph-adjacency G))
      (not (finitely-generated-graph? G))))

(define (finitely-generated-graph? G)
  "Return #t if G has a seed vertex and neighbor function (tier-2).\n\nParameters:\n  G : graph\nReturns: boolean\nCategory: algebra\nKeywords: finitely generated, tier-2, BFS\n\nSee also: `finite-graph?', `enumerate-finite-graph'."
  (and (graph-seed G) (graph-neighbor-fn G) #t))

;;; -- BFS closure (tier-2 → tier-1) --

;; Default backstop for the tier-2 BFS closure when the caller supplies no
;; (max-size . N). A tier-2 neighbor-fn can describe an infinite graph (e.g.
;; a successor over the integers), in which case the frontier never empties
;; and the closure hangs silently. This cap turns that into a remedy-pointing
;; error (mirrors dataflow.scm/graph.scm). Membership is O(n) per step
;; (setoid-member? over `seen`), so on a truly infinite generator the cap
;; surfaces only after ~cap^2 work; a caller who wants a fast, precise failure
;; should pass an explicit small (max-size . N). 100000 is beyond any
;; realistic interactive finite graph.
(define %default-graph-closure-cap 100000)

(define (enumerate-finite-graph G . opts)
  "Promote a finitely-generated graph to a finite graph by enumerating its\nvertices via BFS closure from the seed under the neighbor-fn.\nIdempotent: if G is already tier-1 (has an enumerated adjacency), G is\nreturned unchanged.\n\nThe enumerated result preserves G's directed?, multi?, self-loops?,\nsetoid AND the original seed/neighbor-fn (so the result stays a tier-2\ngraph with a precomputed adjacency — the adjacency and the generator\nare internally coherent, bypassing the Q-d rejection that applies to\nuser-constructed tier-1/tier-2 coexistence).\n\nOptional trailing alist entries:\n  (max-size . N) — abort with an error if closure exceeds N vertices\n\nParameters:\n  G : graph\n  opts : alist\nReturns: graph\nCategory: algebra\nKeywords: enumerate, BFS closure, tier promotion\n\nSee also: `finite-graph?', `finitely-generated-graph?'."
  (cond
    ((finite-graph? G) G)
    ((finitely-generated-graph? G)
     (validate-opts-keys "enumerate-finite-graph" opts '(max-size))
     (let* ((S        (graph-setoid G))
            (seed     (graph-seed G))
            (nfn      (graph-neighbor-fn G))
            (max-size (assv-or opts 'max-size #f))
            ;; No max-size: fall back to the default cap so an infinite
            ;; neighbor-fn raises instead of hanging.
            (cap      (or max-size %default-graph-closure-cap)))
       (let loop ((frontier (list seed))
                  (seen     (list seed))
                  (size     1)
                  (adj      '()))
         (cond
           ((null? frontier)
            ;; Construct directly via %make-graph so the result can
            ;; carry both the enumerated adjacency AND the source
            ;; seed/neighbor-fn. The user-facing make-graph rejects
            ;; this combination (Q-d); internally we know the adj
            ;; and generator are coherent because the adj was just
            ;; enumerated from the generator.
            (let* ((final-adj (reverse adj))
                   (vs        (%adj-vertices final-adj))
                   (n         (length vs))
                   (edges     (%adj-edges final-adj (graph-directed? G) S))
                   (e-count   (length edges)))
              (%make-graph final-adj
                           (graph-directed?   G)
                           (graph-multi?      G)
                           (graph-self-loops? G)
                           S
                           n
                           e-count
                           seed
                           nfn
                           #f)))
           (else
            (let* ((v     (car frontier))
                   (rest  (cdr frontier))
                   (nbrs  (nfn v))
                   (new-vs
                     (filter
                       (lambda (n) (not (setoid-member? S n seen)))
                       (map car nbrs)))
                   (new-vs* (setoid-dedup S new-vs))
                   (new-size (+ size (length new-vs*))))
              (when (> new-size cap)
                (if max-size
                    (error "enumerate-finite-graph: closure exceeded max-size"
                           (list 'max-size max-size 'seed seed))
                    (error (string-append
                            "enumerate-finite-graph: closure exceeded the default cap of "
                            (number->string %default-graph-closure-cap)
                            " vertices without terminating — the neighbor-fn likely"
                            " generates an infinite graph (e.g. a successor over the"
                            " integers). Remedies: pass an explicit (max-size . N) to"
                            " bound the closure, or supply a neighbor-fn with a finite"
                            " reachable set")
                           (list 'seed seed 'cap %default-graph-closure-cap))))
              (loop (append rest new-vs*)
                    (append seen new-vs*)
                    new-size
                    (cons (cons v nbrs) adj))))))))
    (else
     (error "enumerate-finite-graph: graph has neither adjacency nor seed+neighbor-fn"
            (list 'fix "pass (seed . v) and (neighbor-fn . proc) to make-graph, or supply an adjacency")
            G))))

;;; -- Validation --

(define (validate-graph G samples)
  "Check structural invariants on G. Returns #t if all invariants hold,\nor a list of (violation-type arg ...) entries (group.scm convention).\n\nInvariants checked:\n  — vertex set equals keys of adjacency alist\n  — undirected adjacency is symmetric (unless symmetrize? was used)\n  — self-loops absent when (graph-self-loops? G) is #f\n  — parallel edges absent when (graph-multi? G) is #f\n  — vertices are distinguishable under the setoid\n\nSAMPLES is accepted for fixed-arity parity with validate-group /\nvalidate-lattice; ignored in v1. Pass '() for the default case.\n\nParameters:\n  G : graph\n  samples : list\nReturns: #t or list\nCategory: algebra\nKeywords: validate, invariant check, structural\n\nSee also: `assert-graph', `make-graph'."
  (let ((fail! (make-violation-reporter))
        (S          (graph-setoid G))
        (adj        (graph-adjacency G))
        (directed?  (graph-directed? G))
        (multi?     (graph-multi?    G))
        (loops?     (graph-self-loops? G)))
    ;; 1. Vertex distinguishability.
    (let loop ((xs (%adj-vertices adj)))
      (cond
        ((null? xs) #f)
        ((setoid-member? S (car xs) (cdr xs))
         (fail! 'duplicate-vertex (car xs)))
        (else (loop (cdr xs)))))
    ;; 2. Every neighbor must be a known vertex.
    (let ((vs (%adj-vertices adj)))
      (for-each
        (lambda (entry)
          (for-each
            (lambda (p)
              (unless (setoid-member? S (car p) vs)
                (fail! 'unknown-neighbor (car entry) (car p))))
            (cdr entry)))
        adj))
    ;; 3. Self-loops when not permitted.
    (unless loops?
      (for-each
        (lambda (entry)
          (when (setoid-assoc S (car entry) (cdr entry))
            (fail! 'unexpected-self-loop (car entry))))
        adj))
    ;; 4. Parallel edges when not permitted.
    (unless multi?
      (for-each
        (lambda (entry)
          (let scan ((nbrs (cdr entry)) (seen '()))
            (cond
              ((null? nbrs) #f)
              ((setoid-member? S (caar nbrs) seen)
               (fail! 'parallel-edge (car entry) (caar nbrs))
               (scan (cdr nbrs) seen))
              (else (scan (cdr nbrs) (cons (caar nbrs) seen))))))
        adj))
    ;; 5. Undirected symmetry.
    (unless directed?
      (for-each
        (lambda (entry)
          (let ((u (car entry)))
            (for-each
              (lambda (p)
                (let* ((v       (car p))
                       (v-entry (setoid-assoc S v adj)))
                  ;; A self-loop doesn't need a reverse.
                  (unless (or (setoid-equiv? S u v)
                              (and v-entry
                                   (setoid-assoc S u (cdr v-entry))))
                    (fail! 'asymmetric-undirected u v))))
              (cdr entry))))
        adj))
    (fail!)))

(define (assert-graph G samples)
  "Raise an error if G fails any structural invariant; return unspecified on\nsuccess. Thin raising variant of `validate-graph'.\n\nExamples:\n  (assert-graph (complete-graph 3) '())  ; no error\n\nParameters:\n  G : graph\n  samples : list\nReturns: unspecified\nCategory: algebra\nKeywords: assert, raise, validate\n\nSee also: `validate-graph'."
  (let ((result (validate-graph G samples)))
    (unless (eq? result #t)
      (error "assert-graph: graph invariant violations" result))))

;;; -- with-graph binder (parallel to with-group / with-lattice) --

(define-syntax with-graph
  (syntax-rules ()
    ((with-graph G (vertices neighbors degree edge?) body ...)
     (let ((tmp G))
       (let ((vertices  (graph-vertices tmp))
             (neighbors (lambda (v) (graph-neighbors tmp v)))
             (degree    (lambda (v) (graph-degree tmp v)))
             (edge?     (lambda (u v) (graph-edge? tmp u v))))
         body ...)))))

;;; -- Traversal --

(define (graph-bfs G source)
  "Breadth-first traversal of G starting from SOURCE. Returns the list of\nvisited vertices in BFS order. Vertices unreachable from SOURCE are\nomitted.\n\nRaises if SOURCE is not a vertex of G.\n\nExamples:\n  (graph-bfs (cycle-graph 4) 0)  => (0 1 3 2)\n\nParameters:\n  G : graph\n  source : vertex\nReturns: list\nCategory: algebra\nKeywords: BFS, breadth-first, traversal\n\nSee also: `graph-dfs', `graph-connected-components'."
  (unless (graph-has-vertex? G source)
    (error "graph-bfs: source is not a vertex of G"
           (list 'fix "use graph-has-vertex? to test membership first") source))
  (let ((S (graph-setoid G)))
    (let loop ((frontier (list source))
               (visited  (list source))
               (order    (list source)))
      (cond
        ((null? frontier) (reverse order))
        (else
         (let* ((v    (car frontier))
                (rest (cdr frontier))
                (new-nbrs
                  (filter
                    (lambda (n) (not (setoid-member? S n visited)))
                    (map car (graph-neighbors G v))))
                (new-nbrs* (setoid-dedup S new-nbrs)))
           (loop (append rest new-nbrs*)
                 (append visited new-nbrs*)
                 (append (reverse new-nbrs*) order))))))))

(define (graph-dfs G source)
  "Depth-first traversal of G starting from SOURCE. Returns the list of\nvisited vertices in DFS preorder. Vertices unreachable from SOURCE are\nomitted.\n\nRaises if SOURCE is not a vertex of G.\n\nParameters:\n  G : graph\n  source : vertex\nReturns: list\nCategory: algebra\nKeywords: DFS, depth-first, traversal\n\nSee also: `graph-bfs', `graph-connected-components'."
  (unless (graph-has-vertex? G source)
    (error "graph-dfs: source is not a vertex of G"
           (list 'fix "use graph-has-vertex? to test membership first") source))
  (let ((S (graph-setoid G))
        (order '())
        (visited '()))
    (define (visit v)
      (unless (setoid-member? S v visited)
        (set! visited (cons v visited))
        (set! order   (cons v order))
        (for-each
          (lambda (p) (visit (car p)))
          (graph-neighbors G v))))
    (visit source)
    (reverse order)))

(define (graph-connected-components G)
  "Return the list of connected components of G. Each component is a list\nof vertices. Vertex order within each component follows BFS-from-seed\norder where seed = first unvisited vertex in adjacency order.\n\nFor directed graphs, components are *weakly* connected (the underlying\nundirected graph's components); v2 can add separate strongly-connected\nif a consumer surfaces.\n\nExamples:\n  (length\n    (graph-connected-components\n      (make-graph '((a . ((b))) (b . ((a)))\n                    (c . ((d))) (d . ((c)))))))  => 2\n\nParameters:\n  G : graph\nReturns: list of lists\nCategory: algebra\nKeywords: connected components, connectivity, weak components"
  (let ((S (graph-setoid G)))
    (let loop ((remaining (graph-vertices G))
               (acc '()))
      (cond
        ((null? remaining) (reverse acc))
        (else
         (let* ((seed (car remaining))
                ;; For directed graphs, walk the underlying undirected
                ;; graph by also following reverse edges. For undirected
                ;; graphs, graph-neighbors already gives the full local
                ;; neighborhood.
                (component
                  (if (graph-directed? G)
                      (%weakly-connected-component G seed)
                      (graph-bfs G seed)))
                (rest
                  (filter
                    (lambda (v) (not (setoid-member? S v component)))
                    remaining)))
           (loop rest (cons component acc))))))))

(define (%weakly-connected-component G source)
  ;; BFS over the underlying undirected graph (forward edges ∪ reverse
  ;; edges). Used for connected-components on directed graphs.
  (let* ((S    (graph-setoid G))
         (adj  (graph-adjacency G))
         (in-neighbors
           (lambda (v)
             (filter-map
               (lambda (entry)
                 (and (setoid-assoc S v (cdr entry))
                      (not (setoid-equiv? S (car entry) v))
                      (car entry)))
               adj))))
    (let loop ((frontier (list source))
               (visited  (list source))
               (order    (list source)))
      (cond
        ((null? frontier) (reverse order))
        (else
         (let* ((v     (car frontier))
                (rest  (cdr frontier))
                (outs  (map car (graph-neighbors G v)))
                (ins   (in-neighbors v))
                (new
                  (filter
                    (lambda (n) (not (setoid-member? S n visited)))
                    (append outs ins)))
                (new* (setoid-dedup S new)))
           (loop (append rest new*)
                 (append visited new*)
                 (append (reverse new*) order))))))))

;;; -- Bipartiteness --

(define (graph-bipartite? G)
  "Return #t if G is bipartite (admits a 2-coloring with no monochromatic\nedge); #f otherwise. A graph with no edges is trivially bipartite.\n\nSelf-loops make a graph non-bipartite by definition (a loop forces both\nendpoints into the same color class).\n\nExamples:\n  (graph-bipartite? (cycle-graph 4))  => #t\n  (graph-bipartite? (cycle-graph 5))  => #f\n\nParameters:\n  G : graph\nReturns: boolean\nCategory: algebra\nKeywords: bipartite, 2-coloring, odd cycle"
  (not (eq? (%try-bipartition G) 'not-bipartite)))

(define (graph-bipartition G)
  "Return a two-element list (part-A part-B) witnessing bipartiteness of G.\nPart-A is the vertex list colored 0 (starting with the first vertex of\neach component); part-B is colored 1. Raises if G is not bipartite.\n\nFor disconnected graphs, each component is 2-colored independently and\nthe parts are unioned.\n\nExamples:\n  (graph-bipartition (cycle-graph 4))  => ((0 2) (1 3))\n\nParameters:\n  G : graph\nReturns: list of two lists\nCategory: algebra\nKeywords: bipartite, 2-coloring, parts, partition"
  (let ((result (%try-bipartition G)))
    (if (eq? result 'not-bipartite)
        (error "graph-bipartition: graph is not bipartite"
               '(not-bipartite odd-cycle-or-self-loop))
        result)))

(define (%try-bipartition G)
  ;; Two-color G via BFS. Returns (A B) lists or the symbol 'not-bipartite.
  ;; Handles disconnected graphs by restarting BFS from each uncolored
  ;; vertex in adjacency order.
  (let ((S (graph-setoid G))
        (colors '())          ;; alist vertex → 0 or 1
        (failed? #f))
    (define (color-of v)
      (let ((p (setoid-assoc S v colors)))
        (and p (cdr p))))
    (define (set-color! v c)
      (set! colors (cons (cons v c) colors)))
    (define (bfs root)
      (set-color! root 0)
      (let loop ((frontier (list root)))
        (cond
          (failed? #f)
          ((null? frontier) #t)
          (else
           (let* ((v (car frontier))
                  (c (color-of v))
                  (other (if (zero? c) 1 0))
                  (rest  (cdr frontier))
                  (new-frontier
                    (fold
                      (lambda (p acc)
                        (let ((n (car p)))
                          (cond
                            (failed? acc)
                            ;; self-loop kills bipartiteness immediately
                            ((setoid-equiv? S n v)
                             (set! failed? #t) acc)
                            (else
                             (let ((nc (color-of n)))
                               (cond
                                 ((not nc)
                                  (set-color! n other)
                                  (cons n acc))
                                 ((= nc c)
                                  (set! failed? #t) acc)
                                 (else acc)))))))
                      '()
                      (graph-neighbors G v))))
             (loop (append rest (reverse new-frontier))))))))
    (for-each
      (lambda (v)
        (unless (or failed? (color-of v))
          (bfs v)))
      (graph-vertices G))
    (if failed?
        'not-bipartite
        (let ((part-a '()) (part-b '()))
          (for-each
            (lambda (v)
              (let ((c (color-of v)))
                (cond
                  ((eqv? c 0) (set! part-a (cons v part-a)))
                  ((eqv? c 1) (set! part-b (cons v part-b)))
                  (else
                   (error "%try-bipartition: vertex received no color"
                          (list 'fix "report as a bug — BFS should color every vertex per-component")
                          v)))))
            (graph-vertices G))
          (list (reverse part-a) (reverse part-b))))))

;;; ====================================================================
;;;
;;; Isomorphism: 1-WL color refinement + individualization-refinement
;;; backtracking (McKay & Piperno 2014, simplified nauty-lite).
;;;
;;; Terminology per McKay-Piperno §2.2 (pinned in plan Vocabulary):
;;;   — a *partition* is a list of cells (color classes)
;;;   — a partition is *stable* when 1-WL refinement is a fixed point
;;;   — a partition is *discrete* when every cell has cardinality 1
;;;   — a *non-trivial cell* has cardinality ≥ 2
;;;
;;; Algorithm pipeline:
;;;   graph-canonical-form G
;;;     └─▶ initial coloring by vertex invariant (degree, self-loops, ...)
;;;         └─▶ refine-partition (Layer 1; until stable)
;;;             └─▶ if discrete: emit canonical adjacency
;;;             └─▶ else: individualize each vertex in smallest non-trivial
;;;                       cell, refine, recurse; return lex-smallest leaf
;;;                       (McKay-Piperno §3.3)
;;;
;;; graph-isomorphic? G H = (equal? canonical-G canonical-H) with fast-
;;; path short-circuits (different order/size/degree sequences).
;;;
;;; ====================================================================

;; Coloring representation: alist ((vertex . color-integer) ...).
;; Colors are integers starting at 0. The first refinement step buckets
;; vertices by their initial invariant; subsequent steps bucket by
;; (current-color, sorted-multiset-of-neighbor-colors).

(define (%initial-color G v)
  ;; Initial invariant of v: (degree-out, self-loop-count, in-degree-if-directed).
  (let* ((S     (graph-setoid G))
         (nbrs  (graph-neighbors G v))
         (loops (length (filter (lambda (p) (setoid-equiv? S (car p) v)) nbrs)))
         (deg   (length nbrs)))
    (if (graph-directed? G)
        ;; In-degree for directed graphs.
        (let ((in 0))
          (for-each
            (lambda (entry)
              (when (setoid-assoc S v (cdr entry))
                (set! in (+ in 1))))
            (graph-adjacency G))
          (list deg in loops))
        (list deg loops))))

(define (%colors-initial G)
  ;; Compute the initial color of each vertex; bucket and assign integer
  ;; colors 0.. in invariant-sorted order for determinism. Result is
  ;; returned in graph-vertices order for stable between-iteration
  ;; comparison.
  (let* ((vs (graph-vertices G))
         (invariants (map (lambda (v) (cons v (%initial-color G v))) vs))
         (groups (%group-by-signature invariants cdr))
         (unordered (%labels-from-groups groups)))
    (%reorder-alist-by-keys unordered vs equal?)))

(define (%reorder-alist-by-keys al keys key=?)
  ;; Reorder an alist so its keys appear in KEYS order. Linear scan per
  ;; key; acceptable for small V. For large V, swap for a hashtable.
  (map
    (lambda (k)
      (let loop ((xs al))
        (cond
          ((null? xs) (error "reorder: key not found" k))
          ((key=? (caar xs) k) (car xs))
          (else (loop (cdr xs))))))
    keys))

(define (%group-by-signature xs sig-of)
  ;; Partition xs into groups by signature. Returns a list of groups,
  ;; each group = list of xs with equal signature. Groups are emitted in
  ;; canonical signature-sorted order for deterministic color assignment.
  ;; Within each group, xs appear in their original input order.
  (let loop ((src xs) (buckets '()))
    (cond
      ((null? src)
       ;; buckets = ((sig xs-in-reverse) ...). Sort by sig; un-reverse.
       (map (lambda (b) (reverse (cdr b)))
            (list-sort (lambda (a b) (%sig< (car a) (car b)))
                       buckets)))
      (else
       (let* ((x   (car src))
              (s   (sig-of x))
              (hit (assoc s buckets)))
         (if hit
             (begin
               (set-cdr! hit (cons x (cdr hit)))
               (loop (cdr src) buckets))
             (loop (cdr src) (cons (list s x) buckets))))))))

(define (%labels-from-groups groups)
  ;; Given groups = list of lists, each an equivalence class, assign
  ;; integer color 0..N-1 to the i-th group. Return alist vertex→color.
  (let loop ((groups groups) (color 0) (acc '()))
    (cond
      ((null? groups) acc)
      (else
       (loop (cdr groups)
             (+ color 1)
             (append (map (lambda (p) (cons (car p) color)) (car groups))
                     acc))))))

(define (%sig< a b)
  ;; Compare two signatures (arbitrary nested lists of numbers/pairs/symbols)
  ;; by a total order. Used only for deterministic bucket ordering.
  (let ((sa (%sig->key a))
        (sb (%sig->key b)))
    (%key< sa sb)))

(define (%sig->key s)
  ;; Flatten a signature into a list of integers for lexicographic
  ;; comparison. Symbols are hashed to their string; pairs recurse.
  (cond
    ((null? s)    '())
    ((number? s)  (list s))
    ((symbol? s)  (list (%string->intkey (symbol->string s))))
    ((string? s)  (list (%string->intkey s)))
    ((boolean? s) (list (if s 1 0)))
    ((pair? s)    (append (%sig->key (car s)) (%sig->key (cdr s))))
    (else (list 0))))

(define (%string->intkey str)
  ;; Simple deterministic hash of a string to an integer for sort order.
  ;; Sum of char codes weighted by position; order is stable for same
  ;; inputs, which is all we need.
  (let loop ((i 0) (acc 0))
    (cond
      ((= i (string-length str)) acc)
      (else
       (loop (+ i 1)
             (+ (* acc 257) (char->integer (string-ref str i))))))))

(define (%key< a b)
  (cond
    ((and (null? a) (null? b)) #f)
    ((null? a) #t)
    ((null? b) #f)
    ((< (car a) (car b)) #t)
    ((> (car a) (car b)) #f)
    (else (%key< (cdr a) (cdr b)))))

(define (%refine-step G coloring)
  ;; One 1-WL refinement step. Each vertex's new signature is
  ;; (current-color, sorted-multiset-of-neighbor-colors). Bucket, relabel.
  ;; Returns a new coloring alist in graph-vertex order (stable across
  ;; iterations so %coloring-equal? by position is well-defined).
  (let* ((S (graph-setoid G))
         (vs (graph-vertices G))
         (color-of
           (lambda (v)
             (let ((p (setoid-assoc S v coloring)))
               (if p (cdr p) 0))))
         (sigs
           (map
             (lambda (entry)
               (let* ((v (car entry))
                      (nbrs (graph-neighbors G v))
                      (nbr-colors
                        (list-sort < (map (lambda (p) (color-of (car p))) nbrs))))
                 (cons v (list (color-of v) nbr-colors))))
             (graph-adjacency G)))
         (groups (%group-by-signature sigs cdr))
         (unordered (%labels-from-groups groups)))
    (%reorder-alist-by-keys unordered vs equal?)))

(define (%coloring-equal? c1 c2)
  ;; Two colorings are equal (up to per-cell relabeling) if they induce
  ;; the same partition on the vertex set. Simpler sufficient check:
  ;; same vertex→color mapping under alist-ordered comparison.
  ;; Since both colorings come from the same refinement pipeline on the
  ;; same vertex list, a direct alist-equal? check is sufficient.
  (let loop ((c1 c1) (c2 c2))
    (cond
      ((and (null? c1) (null? c2)) #t)
      ((or  (null? c1) (null? c2)) #f)
      ((and (equal? (caar c1) (caar c2))
            (= (cdar c1) (cdar c2)))
       (loop (cdr c1) (cdr c2)))
      (else #f))))

(define (%partition-by-color coloring)
  ;; Turn a coloring alist into a list of cells in color-order.
  ;; Each cell = list of vertices sharing that color.
  (let* ((max-color (fold max -1 (map cdr coloring)))
         (cells (make-vector (+ max-color 1) '())))
    (for-each
      (lambda (p)
        (let ((c (cdr p)))
          (vector-set! cells c (cons (car p) (vector-ref cells c)))))
      coloring)
    (let loop ((i 0) (acc '()))
      (cond
        ((> i max-color) (reverse acc))
        (else (loop (+ i 1) (cons (reverse (vector-ref cells i)) acc)))))))

(define (%discrete? partition)
  (every (lambda (cell) (= (length cell) 1)) partition))

(define (refine-partition G coloring)
  ;; Iterate %refine-step until stable. Returns the stable coloring.
  (let loop ((c coloring))
    (let ((c* (%refine-step G c)))
      (if (%coloring-equal? c c*)
          c
          (loop c*)))))

(define (%find-non-trivial-cell partition)
  ;; Return the smallest non-trivial cell, or #f if none.
  ;; "Smallest" = fewest elements; tie-broken by lowest cell index.
  (let loop ((parts partition) (best #f))
    (cond
      ((null? parts) best)
      ((= (length (car parts)) 1) (loop (cdr parts) best))
      ((or (not best) (< (length (car parts)) (length best)))
       (loop (cdr parts) (car parts)))
      (else (loop (cdr parts) best)))))

(define (%individualize G coloring target)
  ;; Give TARGET a unique color (one greater than the current max); the
  ;; subsequent %refine-step re-buckets from the new coloring. Setoid-
  ;; aware: uses the graph's vertex setoid to match TARGET in the
  ;; coloring alist, so graphs with non-equal? setoids (e.g. numeric-
  ;; setoid, string-ci-setoid) individualize the intended vertex.
  (let* ((S         (graph-setoid G))
         (max-color (fold max 0 (map cdr coloring)))
         (new-color (+ max-color 1)))
    (map
      (lambda (p)
        (if (setoid-equiv? S (car p) target)
            (cons (car p) new-color)
            p))
      coloring)))

(define (%canonical-adjacency G coloring)
  ;; Given a discrete coloring, emit a canonical adjacency form:
  ;; a sorted list of edges, where each vertex is replaced by its
  ;; color integer. For undirected graphs, each edge is (min max);
  ;; for directed, preserves (src dst).
  ;;
  ;; Unlabeled iso (Q-a resolved): edge-data does NOT participate in
  ;; the canonical form. Two graphs with identical topology but
  ;; different edge-data payloads produce equal canonical forms.
  ;;
  ;; The structured representation:
  ;;   (directed? multi? N (edges-as-sorted-list))
  ;; where N = number of vertices; each edge is a (u-color v-color)
  ;; pair lex-sorted.
  (let* ((S (graph-setoid G))
         (color-of
           (lambda (v)
             (let ((p (setoid-assoc S v coloring)))
               (unless p
                 (error "%canonical-adjacency: vertex missing from coloring" v))
               (cdr p))))
         (raw-edges (graph-edges G))
         (relabeled
           (map
             (lambda (e)
               (let ((u (color-of (car  e)))
                     (v (color-of (cadr e))))
                 (cond
                   ((graph-directed? G) (list u v))
                   ((<= u v)            (list u v))
                   (else                (list v u)))))
             raw-edges))
         (sorted (list-sort %edge< relabeled)))
    (list (if (graph-directed? G) 'directed 'undirected)
          (if (graph-multi? G)    'multi    'simple)
          (length coloring)
          sorted)))

(define (%edge< e1 e2)
  ;; Lex-compare two edge triples (u v data).
  (cond
    ((< (car e1) (car e2)) #t)
    ((> (car e1) (car e2)) #f)
    ((< (cadr e1) (cadr e2)) #t)
    ((> (cadr e1) (cadr e2)) #f)
    (else #f)))

(define (graph-canonical-form G)
  "Compute a canonical form for G such that two graphs G and H are\nisomorphic iff their canonical forms are equal under equal?.\n\nAlgorithm: 1-WL color refinement (Weisfeiler-Leman 1968), followed by\nindividualization-refinement backtracking (McKay & Piperno 2014) when\nrefinement terminates on a non-discrete partition. Over all leaves of\nthe search tree, the lex-smallest leaf canonical is returned\n(McKay-Piperno §3.3).\n\nComplexity: O((V+E) log V) on almost-all graphs (refinement discretizes\non first pass). Exponential worst case on highly-symmetric graphs\n(K_n, Kneser, Paley) due to automorphism-group-size branching.\n\nExamples:\n  (equal? (graph-canonical-form (cycle-graph 4))\n          (graph-canonical-form (cycle-graph 4)))  => #t\n\nParameters:\n  G : graph\nReturns: list (canonical adjacency)\nCategory: algebra\nKeywords: canonical form, canonical labeling, graph isomorphism, nauty"
  (let* ((initial (%colors-initial G))
         (stable  (refine-partition G initial))
         (part    (%partition-by-color stable)))
    (cond
      ((%discrete? part)
       (%canonical-adjacency G stable))
      (else
       (%backtrack-canonical G stable)))))

(define (%backtrack-canonical G coloring)
  ;; Individualize each vertex in the smallest non-trivial cell in turn;
  ;; recurse; track the lex-smallest leaf canonical seen.
  (let ((best #f))
    (define (try-from c)
      (let* ((stable (refine-partition G c))
             (part   (%partition-by-color stable)))
        (cond
          ((%discrete? part)
           (let ((leaf (%canonical-adjacency G stable)))
             (when (or (not best) (%canonical< leaf best))
               (set! best leaf))))
          (else
           (let ((cell (%find-non-trivial-cell part)))
             (for-each
               (lambda (v)
                 ;; Pruning (branch-and-bound): skip this branch if the
                 ;; partial canonical already exceeds the best-seen leaf.
                 ;; v1 uses the leaf-only comparison — a full prefix
                 ;; compare would be a v2 optimization.
                 (try-from (%individualize G stable v)))
               cell))))))
    (try-from coloring)
    (unless best
      (error "graph-canonical-form: individualization-refinement failed to discretize"
             (list 'fix "report as a bug — all finite tier-1 graphs should discretize under individualization-refinement")))
    best))

(define (%canonical< c1 c2)
  ;; Lex-compare two canonical forms. Each is (directed? multi? N edges).
  (cond
    ((not (= (list-ref c1 2) (list-ref c2 2)))
     (< (list-ref c1 2) (list-ref c2 2)))
    (else (%edges< (list-ref c1 3) (list-ref c2 3)))))

(define (%edges< es1 es2)
  (cond
    ((and (null? es1) (null? es2)) #f)
    ((null? es1) #t)
    ((null? es2) #f)
    ((%edge< (car es1) (car es2)) #t)
    ((%edge< (car es2) (car es1)) #f)
    (else (%edges< (cdr es1) (cdr es2)))))

(define (graph-isomorphic? G H)
  "Return #t if G and H are isomorphic, #f otherwise. Complete — always\nreturns a definite answer — via 1-WL refinement plus individualization-\nrefinement backtracking (McKay & Piperno 2014, simplified).\n\nShort-circuits: returns #f immediately if\n  — (graph-order G) ≠ (graph-order H)\n  — (graph-size  G) ≠ (graph-size  H)\n  — degree sequences differ\n\nOtherwise falls through to canonical-form comparison.\n\nIsomorphism is UNLABELED: edge-data payloads do not participate. Two\ngraphs with identical topology but different edge-data are considered\nisomorphic (see Q-a, plan `2026-04-22-combinatorial-graph-impl.md`).\n\nExamples:\n  (graph-isomorphic? (cycle-graph 4) (cycle-graph 4))  => #t\n  (graph-isomorphic? (cycle-graph 4) (cycle-graph 5))  => #f\n\nParameters:\n  G : graph\n  H : graph\nReturns: boolean\nCategory: algebra\nKeywords: isomorphism, graph iso, canonical form, nauty\n\nSee also: `graph-canonical-form'."
  (cond
    ((not (= (graph-order G) (graph-order H))) #f)
    ((not (= (graph-size  G) (graph-size  H))) #f)
    ((not (equal? (list-sort < (map (lambda (v) (graph-degree G v))
                                    (graph-vertices G)))
                  (list-sort < (map (lambda (v) (graph-degree H v))
                                    (graph-vertices H)))))
     #f)
    (else
     (equal? (graph-canonical-form G)
             (graph-canonical-form H)))))

;;; ====================================================================
;;;
;;; Invariants: spanning-tree count, chromatic polynomial, Tutte polynomial.
;;;
;;; Deletion-contraction recursion (Tutte 1954):
;;;   — delete edge:    G − e  (remove e; vertex set unchanged)
;;;   — contract edge:  G / e  (identify e's endpoints into one vertex;
;;;                             other incident edges follow the surviving
;;;                             endpoint; self-loops created are preserved
;;;                             as loops, parallel edges merged for chromatic
;;;                             but preserved for Tutte/spanning)
;;;
;;; Size cap: v1 caps general-case deletion-contraction at |E| ≤ 20 for
;;; spanning-tree count and |V|+|E| ≤ 20 for chromatic / Tutte. Fast paths
;;; (K_n, C_n, P_n, trees, empty) bypass the cap.
;;;
;;; ====================================================================

(define %dc-edge-cap 20)          ;; for graph-spanning-tree-count
(define %dc-order-size-cap 20)    ;; for chromatic / Tutte

(define (%relabel-to-naturals G)
  ;; Produce a vertex→index table and an adjacency where vertices are
  ;; integers 0..n-1 (preserving graph-vertices order). Returns
  ;; (index-of-vertex fn, naturalized adjacency vector).
  (let* ((vs (graph-vertices G))
         (n  (length vs))
         (tbl (make-vector n '()))
         (S   (graph-setoid G))
         (idx-of
           (lambda (v)
             (let loop ((xs vs) (i 0))
               (cond
                 ((null? xs) #f)
                 ((setoid-equiv? S v (car xs)) i)
                 (else (loop (cdr xs) (+ i 1))))))))
    (for-each
      (lambda (v)
        (let ((i (idx-of v)))
          (vector-set! tbl i
            (map (lambda (p) (idx-of (car p)))
                 (graph-neighbors G v)))))
      vs)
    (values n tbl)))

(define (%nat-edges-undirected-unique n adj-vec)
  ;; From a natural-labeled adjacency vector, emit a deduplicated list of
  ;; undirected edges (u v) with u < v. Self-loops emitted as (u u).
  ;; Multi-edges preserved.
  (let loop ((i 0) (acc '()))
    (cond
      ((= i n) (reverse acc))
      (else
       (loop (+ i 1)
             (fold (lambda (j a)
                     (cond
                       ((< j i) a)                ;; already seen from j→i
                       (else (cons (list i j) a))))
                   acc
                   (vector-ref adj-vec i)))))))

(define (%nat-adj-copy adj-vec)
  (let* ((n (vector-length adj-vec))
         (copy (make-vector n '())))
    (let loop ((i 0))
      (cond
        ((= i n) copy)
        (else
         (vector-set! copy i (vector-ref adj-vec i))
         (loop (+ i 1)))))))

(define (%nat-remove-one lst target)
  ;; Remove the first occurrence of TARGET from LST. Preserves other
  ;; occurrences (used for multi-edge preservation in deletion).
  (let loop ((src lst) (acc '()))
    (cond
      ((null? src) (reverse acc))
      ((= (car src) target) (append (reverse acc) (cdr src)))
      (else (loop (cdr src) (cons (car src) acc))))))

(define (%nat-delete-edge adj-vec u v)
  ;; G − e: remove ONE occurrence of the edge {u,v} (both directions for
  ;; undirected representation). Multi-edges beyond that are preserved.
  (let ((copy (%nat-adj-copy adj-vec)))
    (vector-set! copy u (%nat-remove-one (vector-ref copy u) v))
    (unless (= u v)
      (vector-set! copy v (%nat-remove-one (vector-ref copy v) u)))
    copy))

(define (%nat-contract-edge adj-vec u v)
  ;; G / e: merge v into u on the undirected adjacency representation.
  ;; Semantics: delete edge {u,v} once, then identify u and v. Parallel
  ;; edges between u and v (if any) become self-loops on the merged
  ;; vertex. Edges from v to w ≠ u redirect to the merged vertex.
  ;;
  ;; Double-counting hazard: an undirected edge u-v is stored as TWO
  ;; half-edge entries ("v in u's list", "u in v's list"). When we merge
  ;; v into u, both halves would collapse to self-loops if naively
  ;; appended. We filter v's u-pointing entries before appending so only
  ;; u's forward-halves (rewritten v → u below) contribute the self-loops.
  ;;
  ;; Vertex v is retained in the adjacency vector as a dead slot
  ;; (empty neighbor list) to preserve indexing during recursion.
  (cond
    ((= u v) adj-vec)               ;; self-loop: contraction is identity
    (else
     (let* ((copy (%nat-adj-copy adj-vec))
            (_    (vector-set! copy u (%nat-remove-one (vector-ref copy u) v)))
            (_    (vector-set! copy v (%nat-remove-one (vector-ref copy v) u)))
            (v-nbrs    (vector-ref copy v))
            ;; Filter v's u-pointing entries: each is the back-half of a
            ;; parallel u-v edge whose forward-half is still in u's list
            ;; and will become a self-loop under the v → u rewrite below.
            (v-to-move (filter (lambda (x) (not (= x u))) v-nbrs)))
       (vector-set! copy u (append (vector-ref copy u) v-to-move))
       ;; Rewrite v → u in v-to-move's neighbors' adjacency lists (so the
       ;; back-references point to the merged vertex).
       (for-each
         (lambda (n)
           (unless (= n v)
             (vector-set! copy n
               (map (lambda (x) (if (= x v) u x)) (vector-ref copy n)))))
         v-to-move)
       ;; Rewrite v → u in u's own list: u's former v-pointing entries
       ;; become self-loops (one per surviving u-v edge after the initial
       ;; delete above).
       (vector-set! copy u
         (map (lambda (x) (if (= x v) u x)) (vector-ref copy u)))
       (vector-set! copy v '())
       copy))))

(define (%nat-size adj-vec)
  ;; Count undirected edges. A loop contributes 1; a non-loop edge is
  ;; double-counted in the adjacency vector so we halve after subtracting
  ;; loops.
  (let loop ((i 0) (loops 0) (total 0))
    (cond
      ((= i (vector-length adj-vec))
       (+ loops (/ (- total loops) 2)))
      (else
       (let* ((nbrs (vector-ref adj-vec i))
              (self (count (lambda (x) (= x i)) nbrs)))
         (loop (+ i 1)
               (+ loops self)
               (+ total (length nbrs))))))))

(define (%nat-active-vertices adj-vec)
  ;; A vertex is "active" if it has at least one neighbor. Contracted
  ;; vertices have an empty neighbor list (see %nat-contract-edge).
  (let loop ((i 0) (acc 0))
    (cond
      ((= i (vector-length adj-vec)) acc)
      ((null? (vector-ref adj-vec i)) (loop (+ i 1) acc))
      (else (loop (+ i 1) (+ acc 1))))))

(define (%nat-first-nonloop-edge n adj-vec)
  ;; Return a (u v) edge with u < v and u ≠ v, or #f if none.
  ;; Scans adjacency slots in order; for each, finds the first neighbor
  ;; strictly greater than the slot index (so the edge is reported once
  ;; per pair, from its smaller-indexed endpoint, and self-loops are
  ;; skipped).
  (let loop ((i 0))
    (cond
      ((= i n) #f)
      (else
       (let scan ((nbrs (vector-ref adj-vec i)))
         (cond
           ((null? nbrs) (loop (+ i 1)))
           ((> (car nbrs) i) (list i (car nbrs)))
           (else (scan (cdr nbrs)))))))))

(define (%nat-connected? n adj-vec)
  ;; Check connectivity over active (non-empty-neighbor-list) vertices
  ;; via BFS. Isolated vertices are counted as separate components.
  (let* ((active (filter (lambda (i) (not (null? (vector-ref adj-vec i))))
                          (iota n))))
    (cond
      ((null? active) #f)     ;; no edges: 0 spanning trees unless n=1
      (else
       (let* ((seed (car active))
              (visited (make-vector n #f)))
         (vector-set! visited seed #t)
         (let bfs ((frontier (list seed)))
           (cond
             ((null? frontier) #t)
             (else
              (let* ((v (car frontier))
                     (rest (cdr frontier))
                     (new-nodes
                       (filter
                         (lambda (u) (not (vector-ref visited u)))
                         (vector-ref adj-vec v))))
                (for-each (lambda (u) (vector-set! visited u #t)) new-nodes)
                (bfs (append rest new-nodes))))))
         (every (lambda (i) (vector-ref visited i)) active))))))

(define (%nat-spanning-tree-count eff-n n adj-vec)
  ;; Deletion-contraction: τ(G) = τ(G − e) + τ(G / e) for non-loop e.
  ;; Loops contribute nothing to τ (they're never in a spanning tree).
  ;;
  ;; EFF-N = current effective vertex count of the (possibly-contracted)
  ;; graph. Initial call: eff-n = original graph-order. Each contraction
  ;; decrements eff-n; deletion leaves eff-n unchanged.
  ;; N = slot count of adj-vec (stays constant across recursion).
  ;;
  ;; Base cases:
  ;;   — eff-n ≤ 1: τ = 1 (a single vertex is its own spanning tree,
  ;;     regardless of any self-loops).
  ;;   — eff-n > 1 and disconnected over active vertices: τ = 0.
  ;;   — eff-n > 1 but no non-loop edges available: τ = 0
  ;;     (disconnected — loops alone cannot span multiple vertices).
  ;;
  ;; Precondition: the caller has already handled the "no edges and
  ;; multiple vertices" disconnected case (for empty-graph n>1 → 0).
  (cond
    ((<= eff-n 1) 1)
    ((not (%nat-connected? n adj-vec)) 0)
    (else
     (let ((e (%nat-first-nonloop-edge n adj-vec)))
       (cond
         ((not e) 0)
         (else
          (let ((u (car e)) (v (cadr e)))
            (+ (%nat-spanning-tree-count eff-n     n (%nat-delete-edge   adj-vec u v))
               (%nat-spanning-tree-count (- eff-n 1) n (%nat-contract-edge adj-vec u v))))))))))

(define (%tree? G)
  ;; G is a tree iff it is connected AND |E| = |V| - 1.
  (and (> (graph-order G) 0)
       (= (graph-size G) (- (graph-order G) 1))
       (= (length (graph-connected-components G)) 1)))

(define (%complete? G n)
  ;; G is K_n iff every pair of distinct vertices is adjacent and there
  ;; are no self-loops or multi-edges and no extra edges.
  (and (= (graph-size G) (/ (* n (- n 1)) 2))
       (not (graph-multi? G))
       (every
         (lambda (v)
           (= (graph-degree G v) (- n 1)))
         (graph-vertices G))))

(define (%cycle? G n)
  ;; G is C_n iff every vertex has degree 2 and G is connected with
  ;; exactly n edges. Requires n ≥ 3 for a true cycle; for n = 2 we
  ;; treat it as not-a-cycle (P_2 fast path applies).
  (and (>= n 3)
       (= (graph-size G) n)
       (every (lambda (v) (= (graph-degree G v) 2)) (graph-vertices G))
       (= (length (graph-connected-components G)) 1)))

;;; -- Inline polynomial arithmetic helpers --
;;;
;;; A polynomial is represented as a list of coefficients, indexed by
;;; degree: (a_0 a_1 ... a_d) meaning a_0 + a_1 x + ... + a_d x^d.
;;; The leading zeroes are trimmed on return from %poly-trim so
;;; equal? comparison between polynomials is canonical.

(define (%poly-trim p)
  ;; Remove trailing zeros (highest-degree) so equal? is canonical.
  (reverse (let drop ((xs (reverse p)))
             (cond
               ((null? xs) '())
               ((and (= (car xs) 0) (pair? (cdr xs))) (drop (cdr xs)))
               (else xs)))))

(define (%poly-add p q)
  (let loop ((p p) (q q) (acc '()))
    (cond
      ((and (null? p) (null? q)) (%poly-trim (reverse acc)))
      ((null? p) (loop '() (cdr q) (cons (car q) acc)))
      ((null? q) (loop (cdr p) '() (cons (car p) acc)))
      (else      (loop (cdr p) (cdr q)
                       (cons (+ (car p) (car q)) acc))))))

(define (%poly-sub p q)
  (let loop ((p p) (q q) (acc '()))
    (cond
      ((and (null? p) (null? q)) (%poly-trim (reverse acc)))
      ((null? p) (loop '() (cdr q) (cons (- (car q)) acc)))
      ((null? q) (loop (cdr p) '() (cons (car p) acc)))
      (else      (loop (cdr p) (cdr q)
                       (cons (- (car p) (car q)) acc))))))

(define (%poly-mul p q)
  (cond
    ((or (null? p) (null? q)) '())
    (else
     (let ((result (make-vector (+ (length p) (length q) -1) 0)))
       (let i-loop ((p p) (i 0))
         (cond
           ((null? p) (%poly-trim (vector->list result)))
           (else
            (let j-loop ((q q) (j 0))
              (cond
                ((null? q) (i-loop (cdr p) (+ i 1)))
                (else
                 (vector-set! result (+ i j)
                   (+ (vector-ref result (+ i j))
                      (* (car p) (car q))))
                 (j-loop (cdr q) (+ j 1))))))))))))

(define (%poly-scale p c)
  (%poly-trim (map (lambda (a) (* a c)) p)))

(define (%poly-shift p k)
  ;; Multiply p by x^k: prepend k zeros.
  (if (or (null? p) (= k 0))
      (%poly-trim p)
      (%poly-trim (append (make-list k 0) p))))

(define (%poly-x-minus-k k)
  ;; (x - k) as a polynomial.
  (list (- k) 1))

(define (%poly-x)
  '(0 1))

(define (%poly-one)
  '(1))

(define (%poly-const c)
  (list c))

;;; -- Chromatic-polynomial helpers --

(define (%falling-factorial n)
  ;; x(x-1)(x-2)...(x-n+1) as a coefficient list.
  (let loop ((k 0) (acc (%poly-one)))
    (cond
      ((= k n) acc)
      (else (loop (+ k 1) (%poly-mul acc (%poly-x-minus-k k)))))))

(define (%tree-chromatic n)
  ;; x(x-1)^(n-1)
  (cond
    ((= n 0) '())
    ((= n 1) (%poly-x))
    (else (%poly-mul (%poly-x) (%poly-pow (%poly-x-minus-k 1) (- n 1))))))

(define (%poly-pow p n)
  (cond
    ((= n 0) (%poly-one))
    ((= n 1) p)
    (else (%poly-mul p (%poly-pow p (- n 1))))))

(define (%cycle-chromatic n)
  ;; χ(C_n, x) = (x-1)^n + (-1)^n (x-1)
  (let* ((x-1 (%poly-x-minus-k 1))
         (p1 (%poly-pow x-1 n))
         (p2 (if (even? n) x-1 (%poly-scale x-1 -1))))
    (%poly-add p1 p2)))

(define (%empty-chromatic n)
  ;; x^n
  (cond
    ((= n 0) (%poly-one))
    (else (%poly-shift (%poly-one) n))))

;;; -- Chromatic polynomial via deletion-contraction on the nat-adj vector --

(define (%nat-chromatic eff-n n adj-vec)
  ;; χ(G − e, x) − χ(G / e, x) for non-loop e; 0 if a loop exists.
  ;; Base cases:
  ;;   — A loop in the graph forces χ = 0 (can't color endpoints distinctly).
  ;;   — No edges (size = 0): χ(empty-graph on eff-n vertices, x) = x^eff-n.
  (cond
    ((%nat-has-loop? n adj-vec) '())              ;; zero polynomial
    ((= eff-n 0) (%poly-one))
    ((= (%nat-size adj-vec) 0)
     (%poly-shift (%poly-one) eff-n))
    (else
     (let ((e (%nat-first-nonloop-edge n adj-vec)))
       (cond
         ((not e) (%poly-shift (%poly-one) eff-n))
         (else
          (let ((u (car e)) (v (cadr e)))
            (%poly-sub
              (%nat-chromatic eff-n     n (%nat-delete-edge   adj-vec u v))
              (%nat-chromatic (- eff-n 1) n (%nat-contract-edge adj-vec u v))))))))))

(define (%nat-has-loop? n adj-vec)
  (let loop ((i 0))
    (cond
      ((= i n) #f)
      ((memv i (vector-ref adj-vec i)) #t)
      (else (loop (+ i 1))))))

(define (graph-chromatic-polynomial G)
  "Return the chromatic polynomial χ(G, x) of G as a coefficient list\n(a_0 a_1 ... a_V) meaning χ(G, x) = a_0 + a_1·x + ... + a_V·x^V\n(Read 1968).\n\nAlgorithm: closed-form fast paths for K_n (x(x-1)...(x-n+1)), C_n\n((x-1)^n + (-1)^n (x-1)), trees (x(x-1)^(n-1)), empty graph (x^n).\nOtherwise deletion-contraction χ(G) = χ(G−e) − χ(G/e) per Read 1968 —\nsize-capped at |V|+|E| ≤ 20 for the general fallback.\n\nDirected graphs are not supported in v1; raises.\n\nExamples:\n  (graph-chromatic-polynomial (complete-graph 3))  => (0 2 -3 1)\n  (graph-chromatic-polynomial (cycle-graph 4))     => (0 -3 6 -4 1)\n  (graph-chromatic-polynomial (empty-graph 3))     => (0 0 0 1)\n\nParameters:\n  G : graph\nReturns: list of integers (polynomial coefficients, ascending degree)\nCategory: algebra\nKeywords: chromatic polynomial, deletion-contraction, Read, coloring"
  (when (graph-directed? G)
    (error "graph-chromatic-polynomial: directed graphs not supported in v1"
           (list 'fix "chromatic polynomial is defined on undirected graphs; model as undirected")))
  (let ((n (graph-order G))
        (m (graph-size G)))
    (cond
      ;; K_n fast path.
      ((%complete? G n) (%falling-factorial n))
      ;; Empty-graph fast path.
      ((= m 0) (%empty-chromatic n))
      ;; Tree fast path (connected + |E| = |V|-1).
      ((%tree? G) (%tree-chromatic n))
      ;; Cycle fast path (connected, n ≥ 3, degrees all 2, |E| = n).
      ((%cycle? G n) (%cycle-chromatic n))
      ;; General case: deletion-contraction.
      (else
       (when (> (+ n m) %dc-order-size-cap)
         (error "graph-chromatic-polynomial: general-case |V|+|E| exceeds cap"
                'order+size (+ n m) 'cap %dc-order-size-cap
                (list 'fix "deletion-contraction is O(1.618^(V+E)); reduce input or pre-decompose")))
       (call-with-values
         (lambda () (%relabel-to-naturals G))
         (lambda (N adj)
           (%nat-chromatic N N adj)))))))

;;; -- Tutte polynomial --
;;;
;;; Tutte polynomial T(G; x, y) per Tutte 1954. Bivariate; represented
;;; here as a list of rows, where row i is a list of y-coefficients for
;;; the x^i term. Example: x^2 + x + y  =  ((0 1) (1) (1))
;;;                                         ↑     ↑   ↑
;;;                                         |     |   x^2-row = [1]
;;;                                         |     x^1-row = [1]
;;;                                         x^0-row = [y-coeff 0, y-coeff 1]
;;;
;;; Recursion (Tutte 1954):
;;;   — loop e:       T(G) = y · T(G − e)
;;;   — bridge e:     T(G) = x · T(G / e)
;;;   — ordinary e:   T(G) = T(G − e) + T(G / e)
;;;
;;; Base: T(edgeless graph on any n) = 1.

(define (%tutte-zero) '())

(define (%tutte-one) '((1)))

(define (%tutte-y) '((0 1)))     ;; x^0 y^1 coefficient = 1

(define (%tutte-x) '((0) (1)))   ;; x^1 y^0 coefficient = 1

(define (%tutte-row-add r1 r2)
  ;; Add two row lists.
  (let loop ((r1 r1) (r2 r2) (acc '()))
    (cond
      ((and (null? r1) (null? r2)) (reverse acc))
      ((null? r1) (loop '() (cdr r2) (cons (car r2) acc)))
      ((null? r2) (loop (cdr r1) '() (cons (car r1) acc)))
      (else (loop (cdr r1) (cdr r2) (cons (+ (car r1) (car r2)) acc))))))

(define (%tutte-add t1 t2)
  (let loop ((t1 t1) (t2 t2) (acc '()))
    (cond
      ((and (null? t1) (null? t2)) (reverse acc))
      ((null? t1) (loop '() (cdr t2) (cons (car t2) acc)))
      ((null? t2) (loop (cdr t1) '() (cons (car t1) acc)))
      (else (loop (cdr t1) (cdr t2)
                  (cons (%tutte-row-add (car t1) (car t2)) acc))))))

(define (%tutte-scale-x t)
  ;; Multiply by x: prepend an empty row at degree 0.
  (cond
    ((null? t) t)
    (else (cons '() t))))

(define (%tutte-scale-y t)
  ;; Multiply by y: in each row, prepend a 0 (shift y-coefficients up).
  (map
    (lambda (row)
      (cond
        ((null? row) '())
        (else (cons 0 row))))
    t))

(define (%nat-is-bridge? n adj-vec u v)
  ;; Edge {u,v} is a bridge iff removing it disconnects u from v.
  ;; (Equivalent to: removal increases component count by 1.)
  (let* ((after (%nat-delete-edge adj-vec u v)))
    (not (%nat-reachable? n after u v))))

(define (%nat-reachable? n adj-vec src dst)
  ;; BFS from src; return #t if dst visited.
  (let ((visited (make-vector n #f)))
    (vector-set! visited src #t)
    (let bfs ((frontier (list src)))
      (cond
        ((null? frontier) (vector-ref visited dst))
        (else
         (let* ((v (car frontier))
                (rest (cdr frontier))
                (new-nodes
                  (filter
                    (lambda (u) (not (vector-ref visited u)))
                    (vector-ref adj-vec v))))
           (for-each (lambda (u) (vector-set! visited u #t)) new-nodes)
           (cond
             ((vector-ref visited dst) #t)
             (else (bfs (append rest new-nodes))))))))))

(define (%nat-first-edge n adj-vec)
  ;; Return the first edge (u v) found (may be a loop). Used by Tutte.
  (let loop ((i 0))
    (cond
      ((= i n) #f)
      (else
       (let ((nbrs (vector-ref adj-vec i)))
         (cond
           ((null? nbrs) (loop (+ i 1)))
           ((= (car nbrs) i) (list i i))       ;; self-loop (first occurrence)
           ((>= (car nbrs) i) (list i (car nbrs)))
           (else
            ;; Edge (i, car) where car < i is the reverse-direction
            ;; entry of an earlier edge we've already counted; keep scanning.
            (let scan ((nbrs (cdr nbrs)))
              (cond
                ((null? nbrs) (loop (+ i 1)))
                ((= (car nbrs) i) (list i i))
                ((>= (car nbrs) i) (list i (car nbrs)))
                (else (scan (cdr nbrs))))))))))))

(define (%nat-tutte n adj-vec)
  ;; Deletion-contraction Tutte recursion.
  (cond
    ((= 0 (%nat-size adj-vec))
     (%tutte-one))                     ;; edgeless → T = 1
    (else
     (let ((e (%nat-first-edge n adj-vec)))
       (cond
         ((not e) (%tutte-one))
         (else
          (let ((u (car e)) (v (cadr e)))
            (cond
              ;; Loop: T(G) = y · T(G − e)
              ((= u v)
               (%tutte-scale-y
                 (%nat-tutte n (%nat-delete-edge adj-vec u v))))
              ;; Bridge: T(G) = x · T(G / e)
              ((%nat-is-bridge? n adj-vec u v)
               (%tutte-scale-x
                 (%nat-tutte n (%nat-contract-edge adj-vec u v))))
              ;; Ordinary: T(G) = T(G − e) + T(G / e)
              (else
               (%tutte-add
                 (%nat-tutte n (%nat-delete-edge   adj-vec u v))
                 (%nat-tutte n (%nat-contract-edge adj-vec u v))))))))))))

(define (graph-tutte-polynomial G)
  "Return the Tutte polynomial T(G; x, y) of G as a list of rows, where\nrow i is a list of y-coefficients for the x^i term (Tutte 1954).\n\nExample: T(K_3) = x^2 + x + y, represented as ((0 1) (1) (1)).\n\nAlgorithm: deletion-contraction with bridge/loop detection.\n  — loop e:       T(G) = y · T(G − e)\n  — bridge e:     T(G) = x · T(G / e)\n  — ordinary e:   T(G) = T(G − e) + T(G / e)\n\nSize-capped at |V|+|E| ≤ 20 for the general case. Directed graphs are\nnot supported in v1; raises.\n\nConsistency with chromatic polynomial:\n  χ(G, x) = (-1)^(V − c(G)) · x^c(G) · T(G; 1-x, 0)\n\nExamples:\n  (graph-tutte-polynomial (cycle-graph 3))  => ((0 1) (1) (1))\n\nParameters:\n  G : graph\nReturns: list of lists\nCategory: algebra\nKeywords: Tutte polynomial, bridge, loop, deletion-contraction"
  (when (graph-directed? G)
    (error "graph-tutte-polynomial: directed graphs not supported in v1"
           (list 'fix "Tutte polynomial is defined on undirected graphs; model as undirected")))
  (let ((n (graph-order G))
        (m (graph-size G)))
    (cond
      ((= m 0) (%tutte-one))
      (else
       (when (> (+ n m) %dc-order-size-cap)
         (error "graph-tutte-polynomial: general-case |V|+|E| exceeds cap"
                'order+size (+ n m) 'cap %dc-order-size-cap
                (list 'fix "deletion-contraction is O(1.618^(V+E)); reduce input or pre-decompose")))
       (call-with-values
         (lambda () (%relabel-to-naturals G))
         (lambda (N adj)
           (%nat-tutte N adj)))))))

(define (graph-spanning-tree-count G)
  "Return the number of spanning trees of G (Kirchhoff 1847) as a\nnon-negative integer. Zero if G is disconnected (including the empty\ngraph on n ≥ 2 vertices).\n\nAlgorithm: closed-form fast paths for K_n (Cayley: n^(n-2)), C_n (n),\ntrees (1), empty (0 for n ≥ 2; 1 for n = 1). Otherwise deletion-\ncontraction recursion per Tutte 1954 — size-capped at |E| ≤ 20 for\nthe general fallback. The Kirchhoff-matrix-tree theorem (via Laplacian\nminor determinant) is a v2 opt-in that would lift the cap to\npolynomial in |V|.\n\nDirected graphs are not supported in v1; raises with a diagnostic\npointing at v2 for directed spanning trees (arborescences).\n\nExamples:\n  (graph-spanning-tree-count (complete-graph 4))  => 16\n  (graph-spanning-tree-count (cycle-graph 5))     => 5\n  (graph-spanning-tree-count (petersen-graph))    => 2000\n\nParameters:\n  G : graph\nReturns: non-negative integer\nCategory: algebra\nKeywords: spanning tree, Cayley, Kirchhoff, matrix tree, deletion contraction"
  (when (graph-directed? G)
    (error "graph-spanning-tree-count: directed graphs not supported in v1"
           (list 'fix "v2 will add arborescence counting (directed spanning trees)")))
  (let ((n (graph-order G))
        (m (graph-size G)))
    (cond
      ((= n 0) 0)
      ((= n 1) 1)
      ;; Any multi-vertex graph with no edges is disconnected: τ = 0.
      ((= m 0) 0)
      ;; Multi-vertex disconnected graph: τ = 0.
      ((> (length (graph-connected-components G)) 1) 0)
      ;; K_n fast path (Cayley): τ(K_n) = n^(n-2) for n ≥ 2.
      ((%complete? G n) (expt n (- n 2)))
      ;; Tree fast path.
      ((%tree? G) 1)
      ;; Cycle fast path.
      ((%cycle? G n) n)
      ;; General case: deletion-contraction.
      (else
       (when (> m %dc-edge-cap)
         (error "graph-spanning-tree-count: general-case edge count exceeds cap"
                'edges m 'cap %dc-edge-cap
                (list 'fix "v2 Kirchhoff-via-matrix would lift the cap; for now, pre-decompose or use smaller inputs")))
       (call-with-values
         (lambda () (%relabel-to-naturals G))
         (lambda (N adj)
           (%nat-spanning-tree-count N N adj)))))))

;;; ====================================================================
;;;
;;; Maximum bipartite matching: Hopcroft-Karp algorithm (O(E·√V)).
;;;
;;; For a bipartite graph G = (A ∪ B, E), find a matching M ⊆ E of
;;; maximum size such that no two edges in M share a vertex.
;;;
;;; Algorithm (Hopcroft & Karp 1973):
;;;   repeat:
;;;     BFS from unmatched A-vertices, building layers in the
;;;     alternating-path sense (match/unmatched edges alternate).
;;;     If no unmatched B-vertex is reached: done.
;;;     DFS along layered edges to find vertex-disjoint augmenting paths;
;;;     flip each path's match-edges.
;;;
;;; ====================================================================

(define (graph-maximum-bipartite-matching G)
  "Return a maximum matching of the bipartite graph G as an alist of\nmatched pairs ((u . v) ...). Each u appears at most once; each v\nappears at most once. Raises if G is not bipartite.\n\nAlgorithm: Hopcroft-Karp (1973) shape (BFS phases + DFS augmenting).\nThe canonical complexity bound is O(E·√V), but the v1 implementation\nuses alist-backed match/coloring maps for setoid compatibility; this\nintroduces an O(V) factor at every membership test, so the realized\ncomplexity is closer to O(V·E·√V). Swapping in hashtable- or\nvector-backed maps (requires a relabel-to-naturals pass) is a v2\noptimization.\n\nDirected graphs are not supported in v1; raises.\n\nExamples:\n  (length (graph-maximum-bipartite-matching (complete-bipartite-graph 3 3)))\n    => 3\n  (length (graph-maximum-bipartite-matching (complete-bipartite-graph 2 4)))\n    => 2\n\nParameters:\n  G : graph\nReturns: list of pairs\nCategory: algebra\nKeywords: matching, bipartite, Hopcroft-Karp, assignment"
  (when (graph-directed? G)
    (error "graph-maximum-bipartite-matching: directed graphs not supported in v1"
           (list 'fix "bipartite matching is defined on undirected graphs; model as undirected")))
  (let ((parts (graph-bipartition G)))      ;; raises if non-bipartite
    (let* ((A (car  parts))
           (B (cadr parts))
           (S (graph-setoid G)))
      ;; State: two mutable alists holding match pointers.
      (define matchA '())
      (define matchB '())
      (define (in-A? v)
        (setoid-member? S v A))
      (define (match-of side k)
        (let* ((al (if (eq? side 'A) matchA matchB))
               (p (setoid-assoc S k al)))
          (if p (cdr p) #f)))
      (define (set-match! side k v)
        (cond
          ((eq? side 'A)
           (set! matchA
             (cons (cons k v)
                   (filter
                     (lambda (p) (not (setoid-equiv? S (car p) k)))
                     matchA))))
          (else
           (set! matchB
             (cons (cons k v)
                   (filter
                     (lambda (p) (not (setoid-equiv? S (car p) k)))
                     matchB))))))
      (define (neighbors-in-B u)
        ;; u is in A; return u's neighbors that are in B.
        (filter-map
          (lambda (p)
            (and (not (in-A? (car p)))
                 (car p)))
          (graph-neighbors G u)))
      ;; BFS: compute dist[u] = layer for u ∈ A. Returns (found? . dist-alist).
      ;; `found?` = an unmatched B-vertex is reachable via tight alternating path.
      (define (bfs!)
        (let ((dist '())
              (found? #f))
          (define (dist-of u)
            (let ((p (setoid-assoc S u dist))) (if p (cdr p) #f)))
          (define (set-dist! u d)
            (set! dist
              (cons (cons u d)
                    (filter (lambda (p) (not (setoid-equiv? S (car p) u)))
                            dist))))
          (let* ((frontier0 (filter (lambda (u) (not (match-of 'A u))) A)))
            (for-each (lambda (u) (set-dist! u 0)) frontier0)
            (let layer-loop ((frontier frontier0))
              (cond
                ((null? frontier) #t)
                (else
                 (let ((next
                         (fold
                           (lambda (u acc)
                             (let ((d (dist-of u)))
                               (fold
                                 (lambda (v acc2)
                                   (let ((u* (match-of 'B v)))
                                     (cond
                                       ((not u*)
                                        (set! found? #t)
                                        acc2)
                                       ((not (dist-of u*))
                                        (set-dist! u* (+ d 1))
                                        (cons u* acc2))
                                       (else acc2))))
                                 acc
                                 (neighbors-in-B u))))
                           '()
                           frontier)))
                   (layer-loop (reverse next))))))
            (values found? dist-of))))
      ;; DFS from u (in A) along tight alternating edges; return #t if
      ;; an augmenting path was found (and match updated).
      (define (dfs! u dist-of)
        (let try-next ((nbrs (neighbors-in-B u)))
          (cond
            ((null? nbrs) #f)
            (else
             (let* ((v (car nbrs))
                    (u* (match-of 'B v)))
               (cond
                 ((not u*)
                  (set-match! 'A u v)
                  (set-match! 'B v u)
                  #t)
                 ((let ((d-u (dist-of u))
                        (d-u* (dist-of u*)))
                    (and d-u d-u* (= d-u* (+ d-u 1))))
                  (cond
                    ((dfs! u* dist-of)
                     (set-match! 'A u v)
                     (set-match! 'B v u)
                     #t)
                    (else (try-next (cdr nbrs)))))
                 (else (try-next (cdr nbrs)))))))))
      ;; Main loop.
      (let outer ()
        (call-with-values bfs!
          (lambda (found? dist-of)
            (cond
              ((not found?)
               (filter-map
                 (lambda (u)
                   (let ((v (match-of 'A u)))
                     (and v (cons u v))))
                 A))
              (else
               (for-each
                 (lambda (u)
                   (when (not (match-of 'A u))
                     (dfs! u dist-of)))
                 A)
               (outer)))))))))

;;; ====================================================================
;;; Maximum common connected induced subgraph (MCCIS) via McGregor (1982)
;;; branch-and-bound with a bipartite-matching (assignment) relaxation
;;; bound (foundations doc §4.2, plan 2026-06-09-mcs-combinatorial-graph-impl).
;;;
;;; A correspondence M : V(G) ⊇ D → V(H) is an injective vertex mapping
;;; that is INDUCED-preserving — for every (a→b), (a'→b') in M:
;;;   edge?(G,a,a') = edge?(H,b,b')   (both directions when directed),
;;;   plus self-loop agreement edge?(G,a,a) = edge?(H,b,b).
;;; Default search keeps D connected (clones are connected fragments);
;;; (disconnected? . #t) relaxes that. Objective: maximize |M|.
;;; ====================================================================

;; Setoid-aware list removal — the search threads each graph's own setoid,
;; so vertices must be removed by equivalence, not by equal?.
(define (%mcs-remove S x lst)
  (filter
    (lambda (y) (not (setoid-equiv? S x y)))
    lst))

;; Adjacency in the underlying undirected sense (either direction). Used for
;; connectivity of the mapped domain in a digraph: an induced connected
;; subgraph is one whose underlying undirected graph is connected.
(define (%mcs-adjacent? G u v)
  (or (graph-edge? G u v)
      (graph-edge? G v u)))

;; Can mapping M be extended by (a→b) while staying induced-consistent?
;; M is an alist ((g . h) ...). a is unmapped in G, b unmapped in H.
(define (%mcs-consistent? G H M a b)
  (and
    ;; self-loop agreement: a loop is part of induced structure.
    (eq? (graph-edge? G a a) (graph-edge? H b b))
    (every
      (lambda (pr)
        (let ((a* (car pr))
              (b* (cdr pr)))
          (and (eq? (graph-edge? G a a*) (graph-edge? H b b*))
               (eq? (graph-edge? G a* a) (graph-edge? H b* b)))))
      M)))

;; Frontier: unmapped G-vertices adjacent to the mapped domain. Restricting
;; growth to the frontier is exactly what enforces connectivity.
(define (%mcs-frontier G dom rem-g)
  (filter
    (lambda (g)
      (any
        (lambda (d) (%mcs-adjacent? G g d))
        dom))
    rem-g))

;; Build the bipartite compatibility graph used by the bound. Relabel the
;; remaining G-vertices to integers 0..n-1 and the remaining H-vertices to
;; n..n+m-1 (two disjoint integer ranges) so the matcher never has to compare
;; a G-vertex against an H-vertex, and so two graphs that happen to share a
;; vertex name don't collide. An edge (i, n+j) means "g_i could still pair
;; with h_j" under compatible?.
;;
;; NOTE: full-graph degree is deliberately NOT used to filter candidate pairs.
;; In an INDUCED common subgraph a matched vertex's degree is measured WITHIN
;; the shared subgraph, not in the full graph, so a degree mismatch in the
;; full graphs does not rule out a pair. Filtering on it would break the
;; bound's admissibility. compatible? (labels/types) is the only filter.
(define (%mcs-compat-graph rem-g rem-h compatible?)
  (let* ((gv (list->vector rem-g))
         (hv (list->vector rem-h))
         (n  (vector-length gv))
         (m  (vector-length hv))
         (gi (iota n))
         (hj (iota m)))
    (let ((left
           (map
             (lambda (i)
               (cons i
                     (filter-map
                       (lambda (j)
                         (and (compatible? (vector-ref gv i) (vector-ref hv j))
                              (cons (+ n j) #f)))
                       hj)))
             gi))
          (right
           (map
             (lambda (j)
               (cons (+ n j)
                     (filter-map
                       (lambda (i)
                         (and (compatible? (vector-ref gv i) (vector-ref hv j))
                              (cons i #f)))
                       gi)))
             hj)))
      ;; symmetric adjacency (both sides listed) — undirected, no symmetrize?
      (make-graph (append left right)))))

;; %mcs-upper-bound — the assignment-relaxation bound (foundations §4.2).
;;
;; Returns an UPPER bound on the maximum number of ADDITIONAL pairs that any
;; extension of the current mapping could achieve, given the still-unmapped
;; vertices rem-g and rem-h.
;;
;; Admissibility (the load-bearing property): any set of future extensions is,
;; in particular, an injective assignment rem-g → rem-h respecting compatible?.
;; A maximum bipartite matching is the LARGEST such injective assignment once
;; the induced constraints are RELAXED (dropped entirely). Dropping constraints
;; can only admit more assignments, never fewer — so the matching size is ≥ the
;; true achievable count. It can over-count, never under-count. That one-sided
;; error is what makes the branch-and-bound prune sound: we never discard a
;; branch that could have beaten the incumbent.
;;
;; With the default compatible? (always true) this reduces to min(|rem-g|,
;; |rem-h|) — the max matching of a complete bipartite graph. A real
;; compatible? (matching node labels/types) tightens it.
(define (%mcs-upper-bound rem-g rem-h compatible?)
  (if (or (null? rem-g) (null? rem-h))
      0
      (length
        (graph-maximum-bipartite-matching
          (%mcs-compat-graph rem-g rem-h compatible?)))))

;; Branch-and-bound driver. Closes over the best mapping seen (the incumbent).
;; v1 accepts permutation redundancy in the growth order (frontier vertices may
;; be added in different orders along different paths); for the exact-on-small-
;; graphs regime this is fine. A fixed-G-vertex-order branching that eliminates
;; the redundancy is a v2 optimization.
(define (%mcs-search G H compatible? connected?)
  (let ((Sg   (graph-setoid G))
        (Sh   (graph-setoid H))
        (best '()))
    (define (extend M rem-g rem-h)
      (when (> (length M) (length best))
        (set! best M))
      ;; Prune: if even the relaxed bound can't beat the incumbent, stop.
      (when (> (+ (length M) (%mcs-upper-bound rem-g rem-h compatible?))
               (length best))
        (let ((cand-g
                (if (and connected? (pair? M))
                    (%mcs-frontier G (map car M) rem-g)
                    rem-g)))
          (for-each
            (lambda (g)
              (for-each
                (lambda (h)
                  (when (and (compatible? g h)
                             (%mcs-consistent? G H M g h))
                    (extend (cons (cons g h) M)
                            (%mcs-remove Sg g rem-g)
                            (%mcs-remove Sh h rem-h))))
                rem-h))
            cand-g))))
    (extend '() (graph-vertices G) (graph-vertices H))
    (reverse best)))

(define (graph-maximum-common-subgraph G H . opts)
  "Return a maximum common connected induced subgraph (MCCIS) of G and H as a\nvertex correspondence ((g-vertex . h-vertex) ...). The correspondence is the\nlargest injective mapping whose mapped G-vertices and their images induce\nisomorphic subgraphs — for every pair of mapped vertices, an edge exists in G\niff the corresponding edge exists in H (induced subgraph isomorphism), and the\nmapped G-vertices form a connected subgraph.\n\nOptions (trailing alist):\n  (disconnected? . BOOL)  — #t drops the connectivity requirement, returning a\n                            maximum common INDUCED subgraph (pieces may be\n                            disconnected). Default #f.\n  (compatible? . PROC)    — (lambda (g-vertex h-vertex) -> boolean) gating which\n                            vertex pairs may be matched (e.g. node-type/label\n                            equality for clone detection). Default: all pairs\n                            compatible (pure topology). A real predicate also\n                            tightens the search bound.\n\nExact (branch-and-bound with a bipartite-matching relaxation bound). MCS is\nNP-hard; intended for small graphs (per-function CFGs/ASTs). The empty\ncorrespondence is trivially common; any single compatible pair beats it.\nDirected graphs are supported (the induced check is direction-aware).\nEdge-data and multi-edge multiplicity are ignored in v1 (topology only).\n\nExamples:\n  (length (graph-maximum-common-subgraph (complete-graph 3) (complete-graph 3)))\n    => 3\n  (length (graph-maximum-common-subgraph (cycle-graph 4) (path-graph 4)))\n    => 3\n  (length (graph-maximum-common-subgraph (complete-graph 3) (path-graph 3)))\n    => 2\n\nParameters:\n  G : graph\n  H : graph\n  opts : alist\nReturns: list of pairs ((g-vertex . h-vertex) ...)\nCategory: algebra\nKeywords: maximum common subgraph, MCS, MCCIS, induced subgraph isomorphism, clone detection, branch and bound\n\nSee also: `graph-isomorphic?', `graph-maximum-bipartite-matching'."
  (validate-opts-keys "graph-maximum-common-subgraph" opts
    '(disconnected? compatible?))
  (let ((disconnected? (assv-or opts 'disconnected? #f))
        (compatible?   (assv-or opts 'compatible?   #f)))
    (when compatible?
      (assert-procedure "graph-maximum-common-subgraph" compatible?))
    (%mcs-search G H
                 (or compatible? (lambda (g h) #t))
                 (not disconnected?))))

(define (complete-graph n)
  "Return K_n, the complete graph on n vertices 0..n-1 (every pair adjacent).\nChromatic = x(x-1)...(x-n+1); spanning-tree count = n^(n-2) (Cayley).\n\nExamples:\n  (graph-order (complete-graph 4))  => 4\n  (graph-size  (complete-graph 4))  => 6\n\nParameters:\n  n : non-negative integer\nReturns: graph\nCategory: algebra\nKeywords: K_n, complete graph, clique"
  (unless (and (integer? n) (>= n 0))
    (error "complete-graph: n must be a non-negative integer" n))
  (let ((vs (iota n)))
    (make-graph
      (map
        (lambda (v)
          (cons v
                (filter-map
                  (lambda (u) (and (not (= u v)) (cons u #f)))
                  vs)))
        vs))))

(define (cycle-graph n)
  "Return C_n, the cycle graph on n vertices 0..n-1. For n ≥ 3 this is a\nsimple n-cycle; for n = 2 it is two vertices joined by one edge;\nfor n ≤ 1 the call raises.\n\nExamples:\n  (graph-order (cycle-graph 5))  => 5\n  (graph-size  (cycle-graph 5))  => 5\n\nParameters:\n  n : integer ≥ 2\nReturns: graph\nCategory: algebra\nKeywords: C_n, cycle, circular graph"
  (unless (and (integer? n) (>= n 2))
    (error "cycle-graph: n must be an integer ≥ 2" n))
  (make-graph
    (map
      (lambda (i)
        (cons i
              (if (= n 2)
                  (list (cons (modulo (+ i 1) n) #f))
                  (list (cons (modulo (- i 1) n) #f)
                        (cons (modulo (+ i 1) n) #f)))))
      (iota n))))

(define (path-graph n)
  "Return P_n, the path graph on n vertices 0..n-1. For n ≥ 2 this has\nedges {i, i+1} for i = 0..n-2; for n = 1 a single isolated vertex; for\nn = 0 an empty graph.\n\nExamples:\n  (graph-order (path-graph 5))  => 5\n  (graph-size  (path-graph 5))  => 4\n\nParameters:\n  n : non-negative integer\nReturns: graph\nCategory: algebra\nKeywords: P_n, path, line graph"
  (unless (and (integer? n) (>= n 0))
    (error "path-graph: n must be a non-negative integer" n))
  (make-graph
    (map
      (lambda (i)
        (cond
          ((= n 1)       (cons 0 '()))
          ((= i 0)       (cons 0 (list (cons 1 #f))))
          ((= i (- n 1)) (cons i (list (cons (- i 1) #f))))
          (else          (cons i (list (cons (- i 1) #f)
                                       (cons (+ i 1) #f))))))
      (iota n))))

(define (complete-bipartite-graph m n)
  "Return K_{m,n}, the complete bipartite graph with parts of size m and n.\nVertices are 0..m-1 (part A) and m..m+n-1 (part B); every A-B pair is\nadjacent. Chromatic = 2 for m,n ≥ 1 (bipartite); matching = min(m, n).\n\nExamples:\n  (graph-order (complete-bipartite-graph 3 3))  => 6\n  (graph-size  (complete-bipartite-graph 3 3))  => 9\n\nParameters:\n  m : non-negative integer\n  n : non-negative integer\nReturns: graph\nCategory: algebra\nKeywords: K_{m,n}, complete bipartite, bipartite graph"
  (unless (and (integer? m) (integer? n) (>= m 0) (>= n 0))
    (error "complete-bipartite-graph: m,n must be non-negative integers" m n))
  (let ((a-vs (iota m))
        (b-vs (iota n m)))
    (make-graph
      (append
        (map (lambda (u) (cons u (map (lambda (v) (cons v #f)) b-vs))) a-vs)
        (map (lambda (v) (cons v (map (lambda (u) (cons u #f)) a-vs))) b-vs)))))

(define (empty-graph n)
  "Return the edgeless graph on n vertices 0..n-1. Chromatic = x^n.\n\nParameters:\n  n : non-negative integer\nReturns: graph\nCategory: algebra\nKeywords: empty graph, edgeless, independent set"
  (unless (and (integer? n) (>= n 0))
    (error "empty-graph: n must be a non-negative integer" n))
  (make-graph (map (lambda (v) (cons v '())) (iota n))))

(define %petersen-edges
  ;; Kneser-graph construction on 2-subsets of {0,1,2,3,4}; edges = disjoint
  ;; pairs. Vertices labeled 0..9 in lex-order of the 2-subset they encode.
  ;;   0:{0,1} 1:{0,2} 2:{0,3} 3:{0,4}
  ;;   4:{1,2} 5:{1,3} 6:{1,4}
  ;;   7:{2,3} 8:{2,4}
  ;;   9:{3,4}
  '((0 7) (0 8) (0 9)
    (1 5) (1 6) (1 9)
    (2 4) (2 6) (2 8)
    (3 4) (3 5) (3 7)
    (4 9)
    (5 8)
    (6 7)))

(define (petersen-graph)
  "Return the Petersen graph — the vertex-transitive, 3-regular graph on\n10 vertices and 15 edges constructed as the Kneser graph on 2-subsets\nof {0,1,2,3,4}. Vertex-transitive and 3-regular; 1-WL refinement does\nnot discretize; exercises the individualization-refinement backtracking\nlayer of graph-isomorphic? / graph-canonical-form.\n\nSpanning-tree count τ(Petersen) = 2000 (Sedláček 1970).\n\nExamples:\n  (graph-order (petersen-graph))  => 10\n  (graph-size  (petersen-graph))  => 15\n\nReturns: graph\nCategory: algebra\nKeywords: Petersen graph, Kneser graph, vertex-transitive, 3-regular"
  (let* ((adj-table (make-vector 10 '()))
         (_ (for-each
              (lambda (e)
                (let ((u (car e)) (v (cadr e)))
                  (vector-set! adj-table u
                    (cons (cons v #f) (vector-ref adj-table u)))
                  (vector-set! adj-table v
                    (cons (cons u #f) (vector-ref adj-table v)))))
              %petersen-edges))
         (adj (map (lambda (i)
                     (cons i (reverse (vector-ref adj-table i))))
                   (iota 10))))
    (make-graph adj)))

;;; ── Balanced graph partition (Kernighan-Lin) ───────────────────────────────
;;;
;;; graph-partition cleaves a weighted graph into two groups minimizing cut
;;; weight at a fixed balance. This is a BALANCED cut, not a global minimum cut:
;;; a global min-cut degenerates to isolating a single vertex, so the partition
;;; sizes are held by the seed and the cut is optimized via Kernighan-Lin
;;; pair-swaps (one vertex from each side per step, preserving |A| and |B|).
;;; Design: plans/2026-06-08-balanced-graph-partition-design.md.

;; Edge-weight accessor over edge-data; weight-fn is always a procedure.
(define (%weight-of weight-fn edge-data)
  (weight-fn edge-data))

;; Side lookup. `side` is an alist vertex -> 'a | 'b keyed under G's setoid S.
;; Vertices may be arbitrary atoms, so match with setoid-assoc — the same
;; equality the rest of this file uses (cf. graph-neighbors).
(define (%side-of S side v)
  (let ((p (setoid-assoc S v side)))
    (and p (cdr p))))

;; Total weight of edges crossing the partition. graph-edges yields each
;; undirected edge once as (u v edge-data), so each crossing edge counts once.
(define (%cut-weight G side weight-fn)
  (let ((S (graph-setoid G)))
    (fold
      (lambda (e acc)
        (let ((u (car e)) (v (cadr e)) (d (caddr e)))
          (if (eq? (%side-of S side u) (%side-of S side v))
              acc
              (+ acc (%weight-of weight-fn d)))))
      0
      (graph-edges G))))

;; Total weight over all edges — denominator for the normalized-cut metric.
(define (%total-weight G weight-fn)
  (fold (lambda (e acc) (+ acc (%weight-of weight-fn (caddr e))))
        0
        (graph-edges G)))

;; Allowed integer size-difference under the balance tolerance:
;;   clamp(floor(tol*n), n mod 2, n-2)
;; Lower floor (n mod 2) keeps the most-balanced split admissible for odd n;
;; upper cap (n-2) keeps both groups non-empty. Used to validate the seed ratio
;; (KL preserves it), not to gate moves.
(define (%allowed-diff tol n)
  (max (modulo n 2)
       (min (exact (floor (* tol n))) (- n 2))))

;; FM/KL gain D(v) = (weight of v's edges to the OPPOSITE side)
;;                 - (weight of v's edges to the SAME side).
;; A self-loop never crosses the cut, so it contributes 0 (skipped).
(define (%partition-gain G side weight-fn v)
  (let* ((S  (graph-setoid G))
         (my (%side-of S side v)))
    (fold
      (lambda (nbr acc)
        (let ((u (car nbr)) (d (cdr nbr)))
          (cond
            ((setoid-equiv? S u v) acc)
            ((eq? (%side-of S side u) my)
             (- acc (%weight-of weight-fn d)))
            (else
             (+ acc (%weight-of weight-fn d))))))
      0
      (graph-neighbors G v))))

;; Total weight of edges directly between v and u (sums parallel edges).
(define (%edge-weight G weight-fn S v u)
  (fold (lambda (nbr acc)
          (if (setoid-equiv? S (car nbr) u)
              (+ acc (%weight-of weight-fn (cdr nbr)))
              acc))
        0
        (graph-neighbors G v)))

;; Deterministic balanced seed: first ceil(n/2) vertices (adjacency order) -> 'a.
(define (%default-seed G)
  (let* ((vs   (graph-vertices G))
         (n    (length vs))
         (half (quotient (+ n 1) 2)))
    (let loop ((vs vs) (i 0) (acc '()))
      (if (null? vs)
          (reverse acc)
          (loop (cdr vs) (+ i 1)
                (cons (cons (car vs) (if (< i half) 'a 'b)) acc))))))

;; Validate a caller seed: covers every vertex with a value in {a,b}, both sides
;; non-empty, and imbalance within the balance tolerance (KL holds this ratio).
(define (%validate-seed G seed tol)
  (let* ((S  (graph-setoid G))
         (vs (graph-vertices G))
         (n  (length vs)))
    (for-each
      (lambda (v)
        (let ((p (setoid-assoc S v seed)))
          (unless (and p (memq (cdr p) '(a b)))
            (error "graph-partition: seed must assign every vertex a side in {a,b}"
                   (list 'fix "provide (vertex . a) or (vertex . b) for every vertex")
                   v))))
      vs)
    (let* ((na (count (lambda (v) (eq? (%side-of S seed v) 'a)) vs))
           (nb (- n na)))
      (when (or (zero? na) (zero? nb))
        (error "graph-partition: seed must place at least one vertex on each side"
               (list 'fix "both 'a and 'b sides must be non-empty")))
      (when (> (abs (- na nb)) (%allowed-diff tol n))
        (error "graph-partition: seed imbalance exceeds the balance tolerance"
               (list 'fix "raise 'balance or supply a more balanced seed")
               (list 'sizes na nb 'allowed (%allowed-diff tol n)))))
    seed))

;; Kernighan-Lin refinement: passes of pair-swaps keeping the best prefix; repeat
;; while a pass strictly reduces the cut. side0 covers every vertex; swaps hold
;; |A|,|B| invariant, so the seed's ratio is preserved.
(define (%kl-refine G side0 weight-fn)
  (let* ((S  (graph-setoid G))
         (vs (graph-vertices G))
         (n  (length vs)))

    (define (a-side side) (filter (lambda (v) (eq? (%side-of S side v) 'a)) vs))
    (define (b-side side) (filter (lambda (v) (eq? (%side-of S side v) 'b)) vs))

    (define (swap side v u)            ; v: A->B, u: B->A
      (map (lambda (e)
             (cond ((setoid-equiv? S (car e) v) (cons (car e) 'b))
                   ((setoid-equiv? S (car e) u) (cons (car e) 'a))
                   (else e)))
           side))

    (define (remove-v x lst)           ; drop first setoid-equiv to x
      (let loop ((lst lst) (acc '()))
        (cond ((null? lst) (reverse acc))
              ((setoid-equiv? S (car lst) x) (append (reverse acc) (cdr lst)))
              (else (loop (cdr lst) (cons (car lst) acc))))))

    ;; best unlocked (v in A, u in B) by swap gain D(v)+D(u)-2w(v,u); D computed
    ;; once per vertex. Deterministic: earliest v, then earliest u, strict >.
    (define (best-pair side a-un b-un)
      (let ((da (map (lambda (v) (cons v (%partition-gain G side weight-fn v))) a-un))
            (db (map (lambda (u) (cons u (%partition-gain G side weight-fn u))) b-un)))
        (let outer ((as da) (bv #f) (bu #f) (bg #f))
          (if (null? as)
              (values bv bu bg)
              (let* ((vp (car as)) (v (car vp)) (dv (cdr vp)))
                (let inner ((bs db) (bv bv) (bu bu) (bg bg))
                  (if (null? bs)
                      (outer (cdr as) bv bu bg)
                      (let* ((up (car bs)) (u (car up)) (du (cdr up))
                             (g  (- (+ dv du)
                                    (* 2 (%edge-weight G weight-fn S v u)))))
                        (if (or (not bg) (> g bg))
                            (inner (cdr bs) v u g)
                            (inner (cdr bs) bv bu bg))))))))))

    ;; one pass -> (values best-prefix-side best-cumulative-gain)
    (define (one-pass side)
      (let loop ((side side) (a-un (a-side side)) (b-un (b-side side))
                 (cum 0) (best-side side) (best-gain 0))
        (if (or (null? a-un) (null? b-un))
            (values best-side best-gain)
            (let-values (((v u g) (best-pair side a-un b-un)))
              (let* ((side* (swap side v u))
                     (cum*  (+ cum g)))
                (if (> cum* best-gain)
                    (loop side* (remove-v v a-un) (remove-v u b-un) cum* side* cum*)
                    (loop side* (remove-v v a-un) (remove-v u b-un) cum* best-side best-gain)))))))

    (let pass ((side side0) (guard n))
      (if (<= guard 0)
          side
          (let-values (((side* gain) (one-pass side)))
            (if (> gain 0)
                (pass side* (- guard 1))
                side))))))

(define (graph-partition G . opts)
  "Partition weighted graph G into two groups minimizing cut weight at a fixed
balance, via Kernighan-Lin pair-swaps.

This is a BALANCED cut, not a minimum cut: a global min-cut degenerates to
isolating a single vertex, so KL holds the partition sizes (set by the seed)
and optimizes only the cut. See
plans/2026-06-08-balanced-graph-partition-design.md.

Opts (trailing alist):
  (method . 'kernighan-lin)  default; only value in Phase 1.
  (balance . 0.25)           imbalance tolerance in (0,1); bounds the seed ratio
                             that KL preserves. Allowed |A|-|B| =
                             clamp(floor(balance*|V|), |V| mod 2, |V|-2).
  (weight . PROC)            edge-data -> non-negative number; default unit weight.
  (seed . ALIST)             vertex -> 'a|'b initial bipartition; default balanced.

Examples:
  (graph-partition (complete-bipartite-graph 3 3))
  ;; Each opt is its own trailing arg (the `make-X . opts` convention), not one
  ;; combined alist:
  (graph-partition g '(balance . 0.3) (cons 'weight (lambda (d) (car d))))

Parameters:
  G : graph
  opts : alist
Returns: alist with keys group-a group-b cut-weight sizes normalized-cut
  (normalized-cut = cut-weight / total-edge-weight; a cost, lower is better; 0.0 if no edges)
Category: algebra
Keywords: partition, balanced cut, kernighan-lin, min cut, package split, graph clustering

See also: `graph-bipartition', `graph-connected-components'."
  (validate-opts-keys "graph-partition" opts '(method balance weight seed))
  (let ((method  (assv-or opts 'method  'kernighan-lin))
        (balance (assv-or opts 'balance 0.25))
        (weight  (assv-or opts 'weight  (lambda (ignored) 1)))
        (seed    (assv-or opts 'seed    #f)))
    (assert-procedure "graph-partition" weight)
    (unless (and (real? balance) (< 0 balance) (< balance 1))
      (error "graph-partition: balance must be in (0,1)"
             (list 'fix "pass an imbalance tolerance such as 0.25 (1 is excluded: it would empty a side)")
             balance))
    (unless (eq? method 'kernighan-lin)
      (error "graph-partition: only 'kernighan-lin is available in Phase 1"
             (list 'fix "'normalized-cut is Phase 3, gated on the gonum eigensolver")
             method))
    (let* ((S  (graph-setoid G))
           (vs (graph-vertices G))
           (n  (length vs)))
      (if (< n 2)
          (list (cons 'group-a vs)
                (cons 'group-b '())
                (cons 'cut-weight 0)
                (cons 'sizes (cons n 0))
                (cons 'normalized-cut 0.0))
          (let* ((seed* (if seed (%validate-seed G seed balance) (%default-seed G)))
                 (side  (%kl-refine G seed* weight))
                 (ga    (filter (lambda (v) (eq? (%side-of S side v) 'a)) vs))
                 (gb    (filter (lambda (v) (eq? (%side-of S side v) 'b)) vs))
                 (cut   (%cut-weight G side weight))
                 (total (%total-weight G weight))
                 (ncut  (if (zero? total) 0.0 (exact->inexact (/ cut total)))))
            (list (cons 'group-a ga)
                  (cons 'group-b gb)
                  (cons 'cut-weight cut)
                  (cons 'sizes (cons (length ga) (length gb)))
                  (cons 'normalized-cut ncut)))))))
