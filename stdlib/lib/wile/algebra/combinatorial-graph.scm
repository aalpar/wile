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
              (when (%setoid-assoc S v (cdr entry))
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

(define (list-sort cmp lst)
  ;; Stable insertion sort: small n, simple, deterministic.
  (let loop ((src lst) (acc '()))
    (cond
      ((null? src) acc)
      (else
       (loop (cdr src)
             (%insert cmp (car src) acc))))))

(define (%insert cmp x sorted)
  (cond
    ((null? sorted) (list x))
    ((cmp x (car sorted)) (cons x sorted))
    (else (cons (car sorted) (%insert cmp x (cdr sorted))))))

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
             (let ((p (%setoid-assoc S v coloring)))
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

(define (%individualize coloring target)
  ;; Give TARGET a unique color (one greater than the current max);
  ;; shift other vertices in its cell down appropriately is not needed —
  ;; subsequent %refine-step will re-bucket from the new coloring.
  (let* ((old (%setoid-assoc (make-setoid equal?) target coloring))
         (old-color (if old (cdr old) 0))
         (max-color (fold max 0 (map cdr coloring)))
         (new-color (+ max-color 1)))
    ;; Replace target's color; leave everything else alone.
    (map
      (lambda (p)
        (if (equal? (car p) target)
            (cons target new-color)
            p))
      coloring)))

(define (%canonical-adjacency G coloring)
  ;; Given a discrete coloring, emit a canonical adjacency form:
  ;; a sorted list of edges, where each vertex is replaced by its
  ;; color integer. For undirected graphs, each edge is (min max);
  ;; for directed, preserves (src dst).
  ;;
  ;; The structured representation:
  ;;   (directed? multi? self-loops? N (edges-as-sorted-list))
  ;; where N = number of vertices, edges-as-sorted-list is a list of
  ;; (u-color v-color edge-data) triples lex-sorted.
  (let* ((S (graph-setoid G))
         (color-of
           (lambda (v)
             (cdr (%setoid-assoc S v coloring))))
         (raw-edges (graph-edges G))
         (relabeled
           (map
             (lambda (e)
               (let ((u (color-of (car  e)))
                     (v (color-of (cadr e)))
                     (d (caddr e)))
                 (cond
                   ((graph-directed? G) (list u v d))
                   ((<= u v)            (list u v d))
                   (else                (list v u d)))))
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
                 (try-from (%individualize stable v)))
               cell))))))
    (try-from coloring)
    (or best
        ;; Fallback (should not happen for finite tier-1 graphs): return
        ;; the current coloring's canonical form even if non-discrete.
        (%canonical-adjacency G coloring))))

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
  "Return #t if G and H are isomorphic, #f otherwise. Complete — always\nreturns a definite answer — via 1-WL refinement plus individualization-\nrefinement backtracking (McKay & Piperno 2014, simplified).\n\nShort-circuits: returns #f immediately if\n  — (graph-order G) ≠ (graph-order H)\n  — (graph-size  G) ≠ (graph-size  H)\n  — degree sequences differ\n  — stable-partition cell cardinalities differ\n\nOtherwise falls through to canonical-form comparison.\n\nExamples:\n  (graph-isomorphic? (cycle-graph 4) (cycle-graph 4))  => #t\n  (graph-isomorphic? (cycle-graph 4) (cycle-graph 5))  => #f\n\nParameters:\n  G : graph\n  H : graph\nReturns: boolean\nCategory: algebra\nKeywords: isomorphism, graph iso, canonical form, nauty\n\nSee also: `graph-canonical-form'."
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
  ;; G / e: merge v into u; redirect all neighbors-of-v (except v itself
  ;; and u) to u; remove self-loops created by the edge being contracted
  ;; (one occurrence of u↔v), but preserve other self-loops and
  ;; parallel edges. Vertex v is retained in the adjacency vector as a
  ;; singleton-with-no-edges to preserve indexing (subsequent recursions
  ;; filter it; n decreases by one for graph-theoretic purposes).
  (cond
    ((= u v) adj-vec)             ;; self-loop: contraction is identity
    (else
     (let* ((copy       (%nat-adj-copy adj-vec))
            ;; Start by deleting the contracted edge (one occurrence).
            (copy       (let ((a (%nat-adj-copy copy)))
                          (vector-set! a u (%nat-remove-one (vector-ref a u) v))
                          (vector-set! a v (%nat-remove-one (vector-ref a v) u))
                          a))
            (v-nbrs    (vector-ref copy v)))
       ;; Move all of v's remaining neighbors onto u; rewrite their
       ;; back-references from v → u.
       (vector-set! copy u (append (vector-ref copy u) v-nbrs))
       (for-each
         (lambda (n)
           (when (not (= n v))   ;; skip v→v self-loops (can't happen here)
             (let ((nbrs (vector-ref copy n)))
               (vector-set! copy n
                 (map (lambda (x) (if (= x v) u x)) nbrs)))))
         v-nbrs)
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

(define (%nat-isolated-count n adj-vec)
  ;; Count vertices with empty neighbor list AND that were never merged.
  ;; In v1 we cannot distinguish "originally isolated" from "contracted
  ;; away" — this is a conservative approximation used only as a
  ;; short-circuit (spanning-tree count = 0 when the non-isolated
  ;; vertices form more than one component).
  (let loop ((i 0) (acc 0))
    (cond
      ((= i n) acc)
      ((null? (vector-ref adj-vec i)) (loop (+ i 1) (+ acc 1)))
      (else (loop (+ i 1) acc)))))

(define (%nat-first-nonloop-edge n adj-vec)
  ;; Return a (u v) edge with u < v and u ≠ v, or #f if none.
  (let loop ((i 0))
    (cond
      ((= i n) #f)
      (else
       (let scan ((nbrs (vector-ref adj-vec i)))
         (cond
           ((null? nbrs) (loop (+ i 1)))
           ((and (> (car nbrs) i)) (list i (car nbrs)))
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

(define (graph-spanning-tree-count G)
  "Return the number of spanning trees of G (Kirchhoff 1847) as a\nnon-negative integer. Zero if G is disconnected (including the empty\ngraph on n ≥ 2 vertices).\n\nAlgorithm: closed-form fast paths for K_n (Cayley: n^(n-2)), C_n (n),\ntrees (1), empty (0 for n ≥ 2; 1 for n = 1). Otherwise deletion-\ncontraction recursion per Tutte 1954 — size-capped at |E| ≤ 20 for\nthe general fallback. The Kirchhoff-matrix-tree theorem (via Laplacian\nminor determinant) is a v2 opt-in that would lift the cap to\npolynomial in |V|.\n\nExamples:\n  (graph-spanning-tree-count (complete-graph 4))  => 16\n  (graph-spanning-tree-count (cycle-graph 5))     => 5\n  (graph-spanning-tree-count (petersen-graph))    => 2000\n\nParameters:\n  G : graph\nReturns: non-negative integer\nCategory: algebra\nKeywords: spanning tree, Cayley, Kirchhoff, matrix tree, deletion contraction"
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
                (list 'graph-spanning-tree-count-too-large m %dc-edge-cap)))
       (call-with-values
         (lambda () (%relabel-to-naturals G))
         (lambda (N adj)
           (%nat-spanning-tree-count N N adj)))))))

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
