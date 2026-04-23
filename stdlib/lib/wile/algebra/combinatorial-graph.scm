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
               order size seed neighbor-fn)
  graph?
  (adjacency   graph-adjacency)
  (directed?   graph-directed?)
  (multi?      graph-multi?)
  (self-loops? graph-self-loops?)
  (setoid      graph-setoid)
  (order       graph-order)
  (size        graph-size)
  (seed        graph-seed)
  (neighbor-fn graph-neighbor-fn))

;;; -- Options-alist helpers (mirror group.scm convention) --

(define (%assv-or opts key fallback)
  (let ((p (assv key opts)))
    (if p (cdr p) fallback)))

(define (%validate-opts-keys site opts known-keys)
  (for-each
    (lambda (pair)
      (unless (and (pair? pair) (memv (car pair) known-keys))
        (error (string-append site ": unknown option key") pair known-keys)))
    opts))

;;; -- Setoid-aware helpers --

(define (%setoid-member? S x xs)
  (let loop ((xs xs))
    (cond
      ((null? xs) #f)
      ((setoid-equiv? S x (car xs)) #t)
      (else (loop (cdr xs))))))

(define (%setoid-assoc S key alist)
  ;; Setoid-driven assoc. Returns the first (key . v) pair whose car is
  ;; setoid-equivalent to KEY, or #f if none.
  (let loop ((xs alist))
    (cond
      ((null? xs) #f)
      ((and (pair? (car xs))
            (setoid-equiv? S key (caar xs)))
       (car xs))
      (else (loop (cdr xs))))))

(define (%setoid-dedup S xs)
  ;; Return xs with later setoid-equivalent duplicates removed.
  ;; Preserves first-seen order.
  (let loop ((src xs) (seen '()))
    (cond
      ((null? src) (reverse seen))
      ((%setoid-member? S (car src) seen) (loop (cdr src) seen))
      (else (loop (cdr src) (cons (car src) seen))))))

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
                     (not (%setoid-assoc S (car p) existing)))
                   incoming))
               (merged (append existing (%setoid-dedup S to-add))))
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
  (%validate-opts-keys "make-graph" opts
    '(directed? multi? self-loops? setoid symmetrize?
      seed neighbor-fn max-size))
  (let* ((directed?   (%assv-or opts 'directed?   #f))
         (multi?      (%assv-or opts 'multi?      #f))
         (self-loops? (%assv-or opts 'self-loops? #t))
         (setoid      (%assv-or opts 'setoid      (default-setoid)))
         (symmetrize? (%assv-or opts 'symmetrize? #f))
         (seed        (%assv-or opts 'seed        #f))
         (nfn         (%assv-or opts 'neighbor-fn #f))
         (adj
           (cond
             ((and (not directed?) symmetrize?)
              (%symmetrize-adjacency adjacency setoid))
             (else adjacency))))
    (unless (list? adj)
      (error "make-graph: adjacency must be a list" adj))
    (when (and nfn (not (procedure? nfn)))
      (error "make-graph: neighbor-fn must be a procedure" nfn))
    (let* ((vs    (%adj-vertices adj))
           (order (if (pair? adj) (length vs) 0))
           (edges (%adj-edges adj directed? setoid))
           (size  (length edges)))
      (%make-graph adj directed? multi? self-loops? setoid
                   order size seed nfn))))

;;; -- Accessors on derived data --

(define (graph-vertices G)
  "Return the list of vertices of G in adjacency-order.\n\nExamples:\n  (graph-vertices (complete-graph 3))  => (0 1 2)\n\nParameters:\n  G : graph\nReturns: list\nCategory: algebra\nKeywords: vertices, nodes\n\nSee also: `graph-edges', `graph-order'."
  (%adj-vertices (graph-adjacency G)))

(define (graph-edges G)
  "Return the list of edges of G as (u v edge-data) triples.\nFor undirected graphs each edge appears once; for directed, the\nnatural directed triples.\n\nExamples:\n  (length (graph-edges (complete-graph 3)))  => 3\n\nParameters:\n  G : graph\nReturns: list\nCategory: algebra\nKeywords: edges, edge list\n\nSee also: `graph-vertices', `graph-size'."
  (%adj-edges (graph-adjacency G) (graph-directed? G) (graph-setoid G)))

(define (graph-neighbors G v)
  "Return the neighbor alist for vertex V in G as ((neighbor . edge-data) ...).\nIf V is not a vertex of G, returns the empty list.\n\nParameters:\n  G : graph\n  v : vertex\nReturns: list\nCategory: algebra\nKeywords: neighbors, adjacency\n\nSee also: `graph-degree', `graph-edge?'."
  (let ((entry (%setoid-assoc (graph-setoid G) v (graph-adjacency G))))
    (if entry (cdr entry) '())))

(define (graph-degree G v)
  "Return the degree of V in G.\nFor undirected graphs, a loop at V contributes 2 to the degree (both\nendpoints incident); a non-loop contributes 1.\nFor directed graphs, this returns the out-degree. In-degree and\ntotal-degree are not exported in v1.\n\nExamples:\n  (graph-degree (cycle-graph 4) 0)  => 2\n\nParameters:\n  G : graph\n  v : vertex\nReturns: non-negative integer\nCategory: algebra\nKeywords: degree, valence, loop"
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

(define (graph-edge? G u v)
  "Return #t if there is an edge from U to V in G.\nFor undirected graphs, symmetric: (graph-edge? G u v) ⟺ (graph-edge? G v u).\n\nExamples:\n  (graph-edge? (complete-graph 3) 0 1)  => #t\n  (graph-edge? (empty-graph 3) 0 1)     => #f\n\nParameters:\n  G : graph\n  u : vertex\n  v : vertex\nReturns: boolean\nCategory: algebra\nKeywords: edge, incidence, adjacency"
  (let ((S (graph-setoid G)))
    (and (%setoid-assoc S v (graph-neighbors G u)) #t)))

(define (graph-vertex-equiv? G u v)
  "Return #t if U and V are equivalent under G's vertex setoid.\n\nParameters:\n  G : graph\n  u : vertex\n  v : vertex\nReturns: boolean\nCategory: algebra\nKeywords: setoid, vertex equality, equivalence"
  (setoid-equiv? (graph-setoid G) u v))

;;; -- Tier predicates --

(define (finite-graph? G)
  "Return #t if G has enumerated vertices AND enumerated edges (tier-1).\nA graph constructed from an explicit adjacency alist is tier-1 by default.\n\nParameters:\n  G : graph\nReturns: boolean\nCategory: algebra\nKeywords: finite, tier-1, enumerated\n\nSee also: `finitely-generated-graph?', `enumerate-finite-graph'."
  (and (pair? (graph-adjacency G))
       (integer? (graph-order G))
       (integer? (graph-size G))
       #t))

(define (finitely-generated-graph? G)
  "Return #t if G has a seed vertex and neighbor function (tier-2).\n\nParameters:\n  G : graph\nReturns: boolean\nCategory: algebra\nKeywords: finitely generated, tier-2, BFS\n\nSee also: `finite-graph?', `enumerate-finite-graph'."
  (and (graph-seed G) (graph-neighbor-fn G) #t))

;;; -- BFS closure (tier-2 → tier-1) --

(define (enumerate-finite-graph G . opts)
  "Promote a finitely-generated graph to a finite graph by enumerating its\nvertices via BFS closure from the seed under the neighbor-fn.\nIdempotent: if G is already tier-1 (has an enumerated adjacency), G is\nreturned unchanged.\n\nOptional trailing alist entries:\n  (max-size . N) — abort with an error if closure exceeds N vertices\n\nParameters:\n  G : graph\n  opts : alist\nReturns: graph\nCategory: algebra\nKeywords: enumerate, BFS closure, tier promotion\n\nSee also: `finite-graph?', `finitely-generated-graph?'."
  (cond
    ((finite-graph? G) G)
    ((finitely-generated-graph? G)
     (%validate-opts-keys "enumerate-finite-graph" opts '(max-size))
     (let* ((S        (graph-setoid G))
            (seed     (graph-seed G))
            (nfn      (graph-neighbor-fn G))
            (max-size (%assv-or opts 'max-size #f)))
       (let loop ((frontier (list seed))
                  (seen     (list seed))
                  (size     1)
                  (adj      '()))
         (cond
           ((null? frontier)
            (make-graph (reverse adj)
                        (cons 'directed?   (graph-directed?   G))
                        (cons 'multi?      (graph-multi?      G))
                        (cons 'self-loops? (graph-self-loops? G))
                        (cons 'setoid      S)))
           (else
            (let* ((v     (car frontier))
                   (rest  (cdr frontier))
                   (nbrs  (nfn v))
                   (new-vs
                     (filter
                       (lambda (n) (not (%setoid-member? S n seen)))
                       (map car nbrs)))
                   (new-vs* (%setoid-dedup S new-vs))
                   (new-size (+ size (length new-vs*))))
              (when (and max-size (> new-size max-size))
                (error "enumerate-finite-graph: closure exceeded max-size"
                       max-size))
              (loop (append rest new-vs*)
                    (append seen new-vs*)
                    new-size
                    (cons (cons v nbrs) adj))))))))
    (else
     (error "enumerate-finite-graph: graph has neither adjacency nor seed+neighbor-fn"
            G))))

;;; -- Validation --

(define (validate-graph G . maybe-samples)
  "Check structural invariants on G. Returns #t if all invariants hold,\nor a list of (violation-type arg ...) entries (group.scm convention).\n\nInvariants checked:\n  — vertex set equals keys of adjacency alist\n  — undirected adjacency is symmetric (unless symmetrize? was used)\n  — self-loops absent when (graph-self-loops? G) is #f\n  — parallel edges absent when (graph-multi? G) is #f\n  — vertices are distinguishable under the setoid\n\nOptional SAMPLES argument is accepted for parity with validate-group /\nvalidate-lattice; ignored in v1.\n\nParameters:\n  G : graph\n  maybe-samples : list (optional)\nReturns: #t or list\nCategory: algebra\nKeywords: validate, invariant check, structural\n\nSee also: `assert-graph', `make-graph'."
  (let ((violations '())
        (S          (graph-setoid G))
        (adj        (graph-adjacency G))
        (directed?  (graph-directed? G))
        (multi?     (graph-multi?    G))
        (loops?     (graph-self-loops? G)))
    (define (fail! type . args)
      (set! violations (cons (cons type args) violations)))
    ;; 1. Vertex distinguishability.
    (let loop ((xs (%adj-vertices adj)))
      (cond
        ((null? xs) #f)
        ((%setoid-member? S (car xs) (cdr xs))
         (fail! 'duplicate-vertex (car xs)))
        (else (loop (cdr xs)))))
    ;; 2. Every neighbor must be a known vertex.
    (let ((vs (%adj-vertices adj)))
      (for-each
        (lambda (entry)
          (for-each
            (lambda (p)
              (unless (%setoid-member? S (car p) vs)
                (fail! 'unknown-neighbor (car entry) (car p))))
            (cdr entry)))
        adj))
    ;; 3. Self-loops when not permitted.
    (unless loops?
      (for-each
        (lambda (entry)
          (when (%setoid-assoc S (car entry) (cdr entry))
            (fail! 'unexpected-self-loop (car entry))))
        adj))
    ;; 4. Parallel edges when not permitted.
    (unless multi?
      (for-each
        (lambda (entry)
          (let scan ((nbrs (cdr entry)) (seen '()))
            (cond
              ((null? nbrs) #f)
              ((%setoid-member? S (caar nbrs) seen)
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
                       (v-entry (%setoid-assoc S v adj)))
                  ;; A self-loop doesn't need a reverse.
                  (unless (or (setoid-equiv? S u v)
                              (and v-entry
                                   (%setoid-assoc S u (cdr v-entry))))
                    (fail! 'asymmetric-undirected u v))))
              (cdr entry))))
        adj))
    (if (null? violations)
        #t
        (reverse violations))))

(define (assert-graph G . maybe-samples)
  "Raise an error if G fails any structural invariant; return unspecified on\nsuccess. Thin raising variant of `validate-graph'.\n\nExamples:\n  (assert-graph (complete-graph 3))  ; no error\n\nParameters:\n  G : graph\n  maybe-samples : list (optional)\nReturns: unspecified\nCategory: algebra\nKeywords: assert, raise, validate\n\nSee also: `validate-graph'."
  (let ((result (apply validate-graph G maybe-samples)))
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
  "Breadth-first traversal of G starting from SOURCE. Returns the list of\nvisited vertices in BFS order. Vertices unreachable from SOURCE are\nomitted.\n\nExamples:\n  (graph-bfs (cycle-graph 4) 0)  => (0 1 3 2)\n\nParameters:\n  G : graph\n  source : vertex\nReturns: list\nCategory: algebra\nKeywords: BFS, breadth-first, traversal\n\nSee also: `graph-dfs', `graph-connected-components'."
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
                    (lambda (n) (not (%setoid-member? S n visited)))
                    (map car (graph-neighbors G v))))
                (new-nbrs* (%setoid-dedup S new-nbrs)))
           (loop (append rest new-nbrs*)
                 (append visited new-nbrs*)
                 (append (reverse new-nbrs*) order))))))))

(define (graph-dfs G source)
  "Depth-first traversal of G starting from SOURCE. Returns the list of\nvisited vertices in DFS preorder. Vertices unreachable from SOURCE are\nomitted.\n\nParameters:\n  G : graph\n  source : vertex\nReturns: list\nCategory: algebra\nKeywords: DFS, depth-first, traversal\n\nSee also: `graph-bfs', `graph-connected-components'."
  (let ((S (graph-setoid G))
        (order '())
        (visited '()))
    (define (visit v)
      (unless (%setoid-member? S v visited)
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
                    (lambda (v) (not (%setoid-member? S v component)))
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
                 (and (%setoid-assoc S v (cdr entry))
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
                    (lambda (n) (not (%setoid-member? S n visited)))
                    (append outs ins)))
                (new* (%setoid-dedup S new)))
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
      (let ((p (%setoid-assoc S v colors)))
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
                  ((eqv? c 1) (set! part-b (cons v part-b))))))
            (graph-vertices G))
          (list (reverse part-a) (reverse part-b))))))
