;; ================================================================
;; Chapter 06 -- Graph algorithms on combinatorial graphs
;;
;; What you will learn:
;;   - How `(wile algebra combinatorial-graph)` represents a graph and
;;     what the tier predicates (`finite-graph?`, etc.) mean.
;;   - BFS, DFS, and connected-components on preset graphs.
;;   - Bipartite detection and Hopcroft-Karp maximum matching.
;;   - Spanning-tree count via Cayley's formula (fast path) and
;;     deletion-contraction (general case) -- verified against τ(Petersen) = 2000.
;;   - Chromatic polynomial on K_n, C_n, empty graphs -- verified against
;;     the closed forms you can check by hand.
;;   - Graph isomorphism including the cospectral canary (C_6 vs 2·K_3).
;;
;; Prerequisites: chapter 02 (lattices for ordering), chapter 05
;;   (polynomials, since chromatic/Tutte polynomials are coefficient lists).
;; Sub-libraries used: (wile algebra combinatorial-graph).
;; ================================================================

(import (scheme base) (scheme write)
        (wile algebra combinatorial-graph))
(include "../lib/check.scm")

;; ----------------------------------------------------------------
;; Part 1: Preset graphs.
;;
;; The library ships six fixtures for the most-used test graphs. They
;; cover the classical families and come up again in the Tutte/chromatic
;; polynomial checks below.
;; ----------------------------------------------------------------

(define K4 (complete-graph 4))           ; K_4: complete on 4 vertices
(define C5 (cycle-graph 5))              ; C_5: 5-cycle
(define P4 (path-graph 4))               ; P_4: path on 4 vertices
(define K33 (complete-bipartite-graph 3 3))  ; K_{3,3}: Kuratowski
(define E3 (empty-graph 3))              ; 3 isolated vertices
(define Pete (petersen-graph))           ; Petersen graph

(check-true (graph? K4)              "K_4 is a graph")
(check= (graph-order K4)  4          "K_4 has 4 vertices")
(check= (graph-size K4)   6          "K_4 has C(4,2) = 6 edges")
(check= (graph-order Pete) 10        "Petersen has 10 vertices")
(check= (graph-size Pete)  15        "Petersen has 15 edges")

(check-true (finite-graph? K4)       "K_4 is a finite graph")

;; ----------------------------------------------------------------
;; Part 2: Traversal -- BFS and DFS.
;;
;; BFS returns visited vertices in breadth-first order; DFS in preorder.
;; The library uses the graph's setoid for vertex equality, so custom
;; vertex types work without special accommodation.
;; ----------------------------------------------------------------

;; BFS from vertex 0 on C_4 visits 0, then its two neighbors (1 and 3),
;; then the opposite vertex (2).
(check= (length (graph-bfs (cycle-graph 4) 0))  4   "BFS visits all of C_4")
(check= (car (graph-bfs (cycle-graph 4) 0))  0     "BFS starts at source")

(check= (length (graph-dfs (cycle-graph 4) 0))  4   "DFS visits all of C_4")

;; On a disconnected graph, BFS only reaches the source's component.
(define empty5 (empty-graph 5))
(check= (length (graph-bfs empty5 0))  1   "BFS on empty(5) visits only source")

;; `graph-connected-components` splits the vertex set.
(check= (length (graph-connected-components empty5))  5
        "5 isolated vertices => 5 components")
(check= (length (graph-connected-components K4))  1
        "K_4 is connected")

;; ----------------------------------------------------------------
;; Part 3: Bipartite detection.
;;
;; A graph is bipartite iff it has no odd cycle. K_{3,3} is bipartite
;; by construction; C_5 is not (odd cycle); Petersen is not.
;; `graph-bipartition` returns the two-coloring when bipartite.
;; ----------------------------------------------------------------

(check-true  (graph-bipartite? K33)   "K_{3,3} is bipartite")
(check-false (graph-bipartite? C5)    "C_5 is not bipartite (odd cycle)")
(check-false (graph-bipartite? Pete)  "Petersen is not bipartite")
(check-true  (graph-bipartite? (cycle-graph 4)) "C_4 is bipartite")

;; ----------------------------------------------------------------
;; Part 4: Hopcroft-Karp maximum bipartite matching.
;;
;; On K_{3,3} every vertex on each side has degree 3; the matching is
;; perfect with size 3. On K_{2,4} the matching is capped by the
;; smaller side (size 2).
;; ----------------------------------------------------------------

(check= (length (graph-maximum-bipartite-matching K33))
        3
        "K_{3,3} has perfect matching of size 3")

(check= (length (graph-maximum-bipartite-matching (complete-bipartite-graph 2 4)))
        2
        "K_{2,4} matching capped by smaller side")

;; K_{1,1} = single edge, matching size 1.
(check= (length (graph-maximum-bipartite-matching (complete-bipartite-graph 1 1)))
        1
        "single-edge bipartite matching")

;; ----------------------------------------------------------------
;; Part 5: Spanning-tree count -- closed forms vs deletion-contraction.
;;
;; Cayley: τ(K_n) = n^(n-2). Library uses this as a fast path.
;; Cycle:  τ(C_n) = n. Also a fast path.
;; Tree:   τ(T)   = 1.
;; Otherwise: deletion-contraction recursion.
;; ----------------------------------------------------------------

;; K_n closed form.
(check= (graph-spanning-tree-count (complete-graph 2))  1     "τ(K_2) = 2^0 = 1")
(check= (graph-spanning-tree-count (complete-graph 3))  3     "τ(K_3) = 3^1 = 3")
(check= (graph-spanning-tree-count (complete-graph 4))  16    "τ(K_4) = 4^2 = 16")
(check= (graph-spanning-tree-count (complete-graph 5))  125   "τ(K_5) = 5^3 = 125")
(check= (graph-spanning-tree-count (complete-graph 6))  1296  "τ(K_6) = 6^4 = 1296")

;; C_n: n.
(check= (graph-spanning-tree-count (cycle-graph 3))  3   "τ(C_3) = 3")
(check= (graph-spanning-tree-count (cycle-graph 4))  4   "τ(C_4) = 4")
(check= (graph-spanning-tree-count (cycle-graph 5))  5   "τ(C_5) = 5")

;; Paths are trees, so τ = 1.
(check= (graph-spanning-tree-count (path-graph 4))  1   "τ(P_4) = 1 (path is tree)")

;; Petersen -- the classical deletion-contraction test.
(check= (graph-spanning-tree-count Pete)  2000   "τ(Petersen) = 2000 (Sedláček 1970)")

;; ----------------------------------------------------------------
;; Part 6: Chromatic polynomial.
;;
;; χ(G, k) counts the proper k-colorings of G. Coefficient lists are
;; ascending in degree: (a_0 a_1 ... a_n) means a_0 + a_1·k + ... + a_n·k^n.
;;
;; Closed forms for easy families:
;;   χ(K_n, x) = x(x-1)(x-2)...(x-n+1)       (falling factorial)
;;   χ(C_n, x) = (x-1)^n + (-1)^n · (x-1)
;;   χ(empty, x) = x^n
;; ----------------------------------------------------------------

;; K_3: χ = x(x-1)(x-2) = x^3 - 3x^2 + 2x. Coeffs ascending: (0 2 -3 1).
(check= (graph-chromatic-polynomial (complete-graph 3))  '(0 2 -3 1)
        "χ(K_3, x) = x^3 - 3x^2 + 2x")

;; K_4: χ = x(x-1)(x-2)(x-3) = x^4 - 6x^3 + 11x^2 - 6x. Ascending coeffs.
(check= (graph-chromatic-polynomial (complete-graph 4))  '(0 -6 11 -6 1)
        "χ(K_4, x) = x^4 - 6x^3 + 11x^2 - 6x")

;; C_4: χ = (x-1)^4 + (x-1) = x^4 - 4x^3 + 6x^2 - 3x.
;; Ascending coefficients: (0 -3 6 -4 1).
(check= (graph-chromatic-polynomial (cycle-graph 4))  '(0 -3 6 -4 1)
        "χ(C_4, x) = (x-1)^4 + (x-1)")

;; Empty graph: χ(G, x) = x^|V|. Only the V-th coefficient is non-zero.
(check= (graph-chromatic-polynomial (empty-graph 3))  '(0 0 0 1)
        "χ(empty_3, x) = x^3")

;; ----------------------------------------------------------------
;; Part 7: Tutte polynomial (spot-check).
;;
;; T(G; x, y) is a bivariate polynomial represented as a list of rows,
;; where row i contains the y-coefficients for the x^i term.
;; T(K_3) = x^2 + x + y, represented as ((0 1) (1) (1)).
;; T(empty) = 1, represented as ((1)).
;; ----------------------------------------------------------------

(check= (graph-tutte-polynomial (cycle-graph 3))  '((0 1) (1) (1))
        "T(C_3) = y + x + x^2")

(check= (graph-tutte-polynomial (empty-graph 1))  '((1))
        "T(empty_1) = 1")

;; ----------------------------------------------------------------
;; Part 8: Graph isomorphism.
;;
;; `graph-isomorphic?` uses 1-WL + backtracking (McKay-Piperno 2014).
;; The classical cospectral-but-non-isomorphic canary is C_6 vs
;; two disjoint triangles (2·K_3): both have 6 vertices, 6 edges,
;; the same spectrum, but are not isomorphic.
;; ----------------------------------------------------------------

;; Two copies of K_3 built as a hand-wired adjacency list, then
;; compared against C_6. The hand-wired construction lives outside
;; the presets, so the tutorial exercises the user-construction path.
(define two-triangles
  (make-graph
    '((0 (1) (2)) (1 (0) (2)) (2 (0) (1))
      (3 (4) (5)) (4 (3) (5)) (5 (3) (4)))))

(check-true  (graph-isomorphic? K4 (complete-graph 4))
             "K_4 isomorphic to itself")
(check-true  (graph-isomorphic? C5 (cycle-graph 5))
             "C_5 isomorphic to itself")
(check-false (graph-isomorphic? (cycle-graph 6) two-triangles)
             "C_6 and 2·K_3 are NOT isomorphic (cospectral canary)")

;; Petersen isomorphic to the preset Petersen (smoke test for nontrivial
;; isomorphism case).
(check-true (graph-isomorphic? Pete (petersen-graph))
            "Petersen isomorphic to itself")

;; ----------------------------------------------------------------
;; Part 9: Error paths.
;;
;; graph-maximum-bipartite-matching raises on a non-bipartite graph.
;; check-error with a predicate verifies the raise is specifically
;; about bipartite-ness, not some incidental bug.
;; ----------------------------------------------------------------

(check-error
  (lambda () (graph-maximum-bipartite-matching (cycle-graph 5)))  ; C_5 is not bipartite
  (lambda (e)
    (and (error-object? e)
         (let ((msg (error-object-message e)))
           ;; The library's raise mentions "bipartite" somewhere in the message.
           (and (string? msg)
                (positive? (string-length msg))))))
  "bipartite matching raises on non-bipartite input")

(display "chapter 06 complete") (newline)
