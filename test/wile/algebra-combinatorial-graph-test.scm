;;; algebra-combinatorial-graph-test.scm — (wile algebra combinatorial-graph)

(import (scheme base)
        (srfi 1)
        (chibi test)
        (wile algebra setoid)
        (wile algebra combinatorial-graph))

;;; Inline fixtures. Presets (complete-graph, cycle-graph, ...) arrive in
;;; Phase 3, so Phase 1 / Phase 2 tests build adjacency alists directly.

(define k3-adj
  '((a . ((b) (c)))
    (b . ((a) (c)))
    (c . ((a) (b)))))

(define c4-adj
  '((0 . ((1) (3)))
    (1 . ((0) (2)))
    (2 . ((1) (3)))
    (3 . ((0) (2)))))

(define p3-directed-adj
  '((a . ((b . 1)))
    (b . ((c . 1)))
    (c . ())))

(test-begin "combinatorial-graph-phase-1")

(test-group "graph? predicate"
  (let ((G (make-graph k3-adj)))
    (test #t (graph? G))
    (test #f (graph? 42))
    (test #f (graph? '()))))

(test-group "core accessors on K_3"
  (let ((G (make-graph k3-adj)))
    (test '(a b c) (graph-vertices G))
    (test 3 (graph-order G))
    (test 3 (graph-size G))
    (test 2 (graph-degree G 'a))
    (test 2 (graph-degree G 'b))
    (test 2 (graph-degree G 'c))
    (test #t (graph-edge? G 'a 'b))
    (test #t (graph-edge? G 'b 'a))
    (test #f (graph-edge? G 'a 'a))))

(test-group "flag defaults"
  (let ((G (make-graph k3-adj)))
    (test #f (graph-directed?   G))
    (test #f (graph-multi?      G))
    (test #t (graph-self-loops? G))))

(test-group "directed flag"
  (let ((G (make-graph p3-directed-adj '(directed? . #t))))
    (test #t (graph-directed? G))
    (test 3 (graph-order G))
    (test 2 (graph-size G))             ;; directed edges: a→b, b→c
    (test 1 (graph-degree G 'a))        ;; out-degree
    (test 0 (graph-degree G 'c))
    (test #t (graph-edge? G 'a 'b))
    (test #f (graph-edge? G 'b 'a))))

(test-group "self-loop degree (undirected: loop contributes 2)"
  (let ((G (make-graph '((v . ((v)))))))
    (test 2 (graph-degree G 'v))))

(test-group "graph-neighbors returns neighbor alist"
  (let ((G (make-graph k3-adj)))
    (test '((b) (c)) (graph-neighbors G 'a))
    (test '()       (graph-neighbors G 'missing))))

(test-group "graph-vertex-equiv? delegates to setoid"
  (let ((G (make-graph k3-adj)))
    (test #t (graph-vertex-equiv? G 'a 'a))
    (test #f (graph-vertex-equiv? G 'a 'b))))

(test-group "symmetrize? option"
  ;; Supplied with only the forward edges; symmetrize? adds reverses.
  (let ((G (make-graph
             '((a . ((b . 1) (c . 2)))
               (b . ())
               (c . ()))
             '(symmetrize? . #t))))
    (test #t (graph-edge? G 'a 'b))
    (test #t (graph-edge? G 'b 'a))
    (test #t (graph-edge? G 'a 'c))
    (test #t (graph-edge? G 'c 'a))
    (test 2 (graph-degree G 'a))
    (test 1 (graph-degree G 'b))))

(test-group "validate-graph on valid graphs returns #t"
  (test #t (validate-graph (make-graph k3-adj)))
  (test #t (validate-graph (make-graph c4-adj)))
  (test #t (validate-graph (make-graph p3-directed-adj '(directed? . #t)))))

(test-group "validate-graph catches asymmetric undirected"
  (let* ((bad '((a . ((b))) (b . ())))
         (G   (make-graph bad)))
    (let ((result (validate-graph G)))
      (test #t (and (list? result)
                    (any (lambda (v) (eq? (car v) 'asymmetric-undirected))
                         result))))))

(test-group "validate-graph catches unknown neighbor"
  (let* ((bad '((a . ((z)))))
         (G   (make-graph bad)))
    (let ((result (validate-graph G)))
      (test #t (and (list? result)
                    (any (lambda (v) (eq? (car v) 'unknown-neighbor))
                         result))))))

(test-group "validate-graph catches parallel edges when multi? = #f"
  (let* ((bad '((a . ((b) (b))) (b . ((a) (a)))))
         (G   (make-graph bad)))    ;; multi? default = #f
    (let ((result (validate-graph G)))
      (test #t (and (list? result)
                    (any (lambda (v) (eq? (car v) 'parallel-edge))
                         result))))))

(test-group "parallel edges allowed when multi? = #t"
  (let* ((adj '((a . ((b) (b))) (b . ((a) (a)))))
         (G   (make-graph adj '(multi? . #t))))
    (test #t (validate-graph G))
    (test #t (graph-multi? G))))

(test-group "validate-graph catches self-loop when self-loops? = #f"
  (let* ((adj '((v . ((v)))))
         (G   (make-graph adj '(self-loops? . #f))))
    (let ((result (validate-graph G)))
      (test #t (and (list? result)
                    (any (lambda (v) (eq? (car v) 'unexpected-self-loop))
                         result))))))

(test-group "assert-graph raises on invalid"
  (let ((G (make-graph '((a . ((z)))))))
    (test-error (assert-graph G))))

(test-group "assert-graph silent on valid"
  (let ((G (make-graph k3-adj)))
    (assert-graph G)  ;; no error
    (test #t #t)))

(test-group "make-graph rejects unknown option keys"
  (test-error (make-graph k3-adj '(directd? . #t)))        ;; typo
  (test-error (make-graph k3-adj '(not-a-known-key . 42))))

(test-group "finite-graph? predicate"
  (test #t (finite-graph? (make-graph k3-adj)))
  ;; tier-2 (no explicit adjacency, just seed + nfn) is NOT finite
  (let ((G (make-graph '()
                       (cons 'seed 'root)
                       (cons 'neighbor-fn (lambda (v) '())))))
    (test #f (finite-graph? G))
    (test #t (finitely-generated-graph? G))))

(test-group "enumerate-finite-graph is idempotent on tier-1"
  (let ((G (make-graph k3-adj)))
    (let ((G* (enumerate-finite-graph G)))
      (test 3 (graph-order G*))
      (test 3 (graph-size  G*)))))

(test-group "enumerate-finite-graph promotes tier-2 to tier-1"
  ;; Small, finite graph defined purely by seed + neighbor-fn.
  ;;
  ;;   0 → 1 → 2  (directed path)
  ;;
  (let* ((nfn (lambda (v)
                (cond
                  ((eqv? v 0) (list (cons 1 #f)))
                  ((eqv? v 1) (list (cons 2 #f)))
                  (else '()))))
         (G (make-graph '()
                        '(directed? . #t)
                        (cons 'seed 0)
                        (cons 'neighbor-fn nfn)))
         (G* (enumerate-finite-graph G)))
    (test #t (finite-graph? G*))
    (test 3 (graph-order G*))
    (test 2 (graph-size  G*))))

(test-group "enumerate-finite-graph respects max-size"
  (let* ((nfn (lambda (v) (list (cons (+ v 1) #f))))   ;; infinite chain
         (G   (make-graph '()
                          '(directed? . #t)
                          (cons 'seed 0)
                          (cons 'neighbor-fn nfn))))
    (test-error (enumerate-finite-graph G '(max-size . 5)))))

(test-group "with-graph binder"
  (with-graph (make-graph k3-adj) (vs neighbors deg edge)
    (test '(a b c) vs)
    (test 2 (deg 'a))
    (test #t (edge 'a 'b))
    (test 2 (length (neighbors 'a)))))

(test-group "setoid-carried vertex equality"
  ;; String vertices compared via string-setoid. Distinct strings that
  ;; compare equal under string=? are treated as one vertex.
  (let* ((adj `(("a" . (("b")))
                ("b" . (("a")))))
         (G   (make-graph adj (cons 'setoid (string-setoid)))))
    (test #t (graph-edge? G "a" "b"))
    (test #t (graph-vertex-equiv? G "a" "a"))))

(test-end)

(test-begin "combinatorial-graph-phase-2")

;;; Helper: build C_n (cycle on n vertices 0..n-1) and P_n (path on n).

(define (inline-cycle-adj n)
  (map
    (lambda (i)
      (cons i
            (list (cons (modulo (- i 1) n) #f)
                  (cons (modulo (+ i 1) n) #f))))
    (iota n)))

(define (inline-path-adj n)
  (map
    (lambda (i)
      (cond
        ((= i 0)       (cons 0 (list (cons 1 #f))))
        ((= i (- n 1)) (cons (- n 1) (list (cons (- n 2) #f))))
        (else          (cons i (list (cons (- i 1) #f) (cons (+ i 1) #f))))))
    (iota n)))

(test-group "graph-bfs on cycle"
  (let ((C4 (make-graph (inline-cycle-adj 4))))
    ;; From 0, BFS expands to {1, 3}, then {2}. Order is 0 then the
    ;; neighbors in adjacency-list order.
    (test 4 (length (graph-bfs C4 0)))
    (test 0 (car (graph-bfs C4 0)))))

(test-group "graph-dfs on cycle"
  (let ((C4 (make-graph (inline-cycle-adj 4))))
    (test 4 (length (graph-dfs C4 0)))
    (test 0 (car (graph-dfs C4 0)))))

(test-group "graph-bfs on path"
  (let ((P5 (make-graph (inline-path-adj 5))))
    (test '(0 1 2 3 4) (graph-bfs P5 0))
    (test '(4 3 2 1 0) (graph-bfs P5 4))))

(test-group "connected components: single component"
  (let ((C4 (make-graph (inline-cycle-adj 4))))
    (test 1 (length (graph-connected-components C4)))
    (test 4 (length (car (graph-connected-components C4))))))

(test-group "connected components: disjoint union"
  ;; K_2 ⊔ P_2 (both are trivially connected each)
  (let ((G (make-graph
             '((a . ((b))) (b . ((a)))
               (c . ((d))) (d . ((c)))))))
    (test 2 (length (graph-connected-components G)))))

(test-group "connected components on directed graph use weak components"
  (let ((G (make-graph
             '((a . ((b . 1)))
               (b . ())
               (c . ((d . 1)))
               (d . ()))
             '(directed? . #t))))
    (test 2 (length (graph-connected-components G)))))

(test-group "bipartiteness: even cycle yes, odd cycle no"
  (test #t (graph-bipartite? (make-graph (inline-cycle-adj 4))))
  (test #t (graph-bipartite? (make-graph (inline-cycle-adj 6))))
  (test #f (graph-bipartite? (make-graph (inline-cycle-adj 3))))
  (test #f (graph-bipartite? (make-graph (inline-cycle-adj 5)))))

(test-group "bipartiteness: path yes, K_3 no"
  (test #t (graph-bipartite? (make-graph (inline-path-adj 5))))
  (test #f (graph-bipartite? (make-graph k3-adj))))

(test-group "bipartiteness: empty / singleton trivially bipartite"
  (test #t (graph-bipartite? (make-graph '())))
  (test #t (graph-bipartite? (make-graph '((v . ()))))))

(test-group "bipartiteness: self-loop kills bipartiteness"
  (test #f (graph-bipartite? (make-graph '((v . ((v)))))))
  (test-error (graph-bipartition (make-graph '((v . ((v))))))))

(test-group "graph-bipartition returns two parts"
  (let* ((C4   (make-graph (inline-cycle-adj 4)))
         (parts (graph-bipartition C4)))
    (test 2 (length parts))
    ;; The two parts should partition the vertices.
    (test 4 (+ (length (car parts)) (length (cadr parts))))
    ;; Every edge crosses parts.
    (test #t
      (every
        (lambda (edge)
          (let ((u (car edge))
                (v (cadr edge)))
            (not (equal?
                   (if (member u (car parts)) 'A 'B)
                   (if (member v (car parts)) 'A 'B)))))
        (graph-edges C4)))))

(test-group "graph-bipartition raises on odd cycle"
  (test-error (graph-bipartition (make-graph (inline-cycle-adj 5)))))

(test-group "bipartiteness: K_{m,n} is bipartite"
  ;; K_{2,3}: parts {a,b} and {x,y,z}, all cross edges.
  (let ((K23 (make-graph
               '((a . ((x) (y) (z)))
                 (b . ((x) (y) (z)))
                 (x . ((a) (b)))
                 (y . ((a) (b)))
                 (z . ((a) (b)))))))
    (test #t (graph-bipartite? K23))
    (let ((parts (graph-bipartition K23)))
      (test 2 (length parts)))))

(test-end)

(test-begin "combinatorial-graph-phase-3")

(test-group "preset shapes"
  (test 3 (graph-order (complete-graph 3)))
  (test 3 (graph-size  (complete-graph 3)))
  (test 4 (graph-order (complete-graph 4)))
  (test 6 (graph-size  (complete-graph 4)))
  (test 5 (graph-order (cycle-graph 5)))
  (test 5 (graph-size  (cycle-graph 5)))
  (test 5 (graph-order (path-graph 5)))
  (test 4 (graph-size  (path-graph 5)))
  (test 6 (graph-order (complete-bipartite-graph 3 3)))
  (test 9 (graph-size  (complete-bipartite-graph 3 3)))
  (test 10 (graph-order (petersen-graph)))
  (test 15 (graph-size  (petersen-graph)))
  (test 0 (graph-size  (empty-graph 5)))
  (test 5 (graph-order (empty-graph 5))))

(test-group "preset input validation"
  (test-error (complete-graph -1))
  (test-error (cycle-graph 1))
  (test-error (path-graph -1))
  (test-error (complete-bipartite-graph 2 -1))
  (test-error (empty-graph -3)))

(test-group "Petersen is 3-regular"
  (let ((P (petersen-graph)))
    (for-each
      (lambda (v) (test 3 (graph-degree P v)))
      (graph-vertices P))))

;; --- Positive isomorphism tests (non-regular → fast path) ---

(test-group "path iso path (non-regular, fast-path)"
  (test #t (graph-isomorphic? (path-graph 4) (path-graph 4)))
  (test #t (graph-isomorphic? (path-graph 5) (path-graph 5))))

(test-group "complete-bipartite iso complete-bipartite"
  (test #t (graph-isomorphic? (complete-bipartite-graph 2 3)
                              (complete-bipartite-graph 2 3))))

;; --- Positive isomorphism tests (regular → backtracking required) ---

(test-group "complete graph self-iso (regular, backtracking)"
  (test #t (graph-isomorphic? (complete-graph 3) (complete-graph 3)))
  (test #t (graph-isomorphic? (complete-graph 4) (complete-graph 4)))
  (test #t (graph-isomorphic? (complete-graph 5) (complete-graph 5))))

(test-group "cycle self-iso (vertex-transitive, backtracking)"
  (test #t (graph-isomorphic? (cycle-graph 4) (cycle-graph 4)))
  (test #t (graph-isomorphic? (cycle-graph 5) (cycle-graph 5)))
  (test #t (graph-isomorphic? (cycle-graph 6) (cycle-graph 6))))

(test-group "BACKTRACKING CORRECTNESS CANARY — Petersen self-iso → #t"
  ;; 1-WL refinement alone cannot discretize the Petersen graph (it is
  ;; vertex-transitive and 3-regular, so every vertex gets the same
  ;; color signature). This test passing #t proves the individualization-
  ;; refinement backtracking layer (Layer 2, McKay-Piperno §3.1) is
  ;; correctly wired.
  (test #t (graph-isomorphic? (petersen-graph) (petersen-graph))))

(test-group "Petersen iso under non-trivial relabeling"
  ;; Apply a permutation to the Petersen vertex labels and verify the
  ;; relabeled graph is still iso to the original.
  (let* ((orig (petersen-graph))
         (perm (list 5 2 8 1 9 0 7 4 3 6))
         (at   (lambda (v) (list-ref perm v)))
         (permuted-adj
           (map
             (lambda (v)
               (cons (at v)
                     (map (lambda (p) (cons (at (car p)) (cdr p)))
                          (graph-neighbors orig v))))
             (graph-vertices orig)))
         (permuted (make-graph permuted-adj)))
    (test #t (graph-isomorphic? orig permuted))))

;; --- Negative isomorphism tests ---

(test-group "different vertex count → #f (short-circuit)"
  (test #f (graph-isomorphic? (complete-graph 3) (complete-graph 4)))
  (test #f (graph-isomorphic? (path-graph 5)     (path-graph 6))))

(test-group "different edge count → #f (short-circuit)"
  (test #f (graph-isomorphic? (complete-graph 4) (cycle-graph 4)))
  (test #f (graph-isomorphic? (cycle-graph 5)    (path-graph 5))))

(test-group "different degree sequence → #f"
  ;; K_{1,3} (star, degrees 3,1,1,1) vs P_4 (degrees 1,2,2,1)
  (let ((star (make-graph '((c . ((a) (b) (d)))
                             (a . ((c)))
                             (b . ((c)))
                             (d . ((c))))))
        (p4   (path-graph 4)))
    (test #f (graph-isomorphic? star p4))))

(test-group "COSPECTRAL NON-ISO CANARY — C_6 vs 2K_3"
  ;; Both 6 vertices, 6 edges, all degree 2 — degree sequence matches.
  ;; 1-WL refinement cannot separate them (all signatures collapse to
  ;; the same color). But C_6 is connected, 2K_3 is two triangles;
  ;; the backtracking layer discovers this via canonical-form
  ;; comparison after individualization.
  (let ((C6 (cycle-graph 6))
        (two-triangles
          (make-graph
            '((0 . ((1) (2))) (1 . ((0) (2))) (2 . ((0) (1)))
              (3 . ((4) (5))) (4 . ((3) (5))) (5 . ((3) (4)))))))
    (test 6 (graph-order C6))
    (test 6 (graph-order two-triangles))
    (test 6 (graph-size  C6))
    (test 6 (graph-size  two-triangles))
    (test #f (graph-isomorphic? C6 two-triangles))))

(test-group "more negative pairs"
  (test #f (graph-isomorphic? (complete-graph 4) (path-graph 4)))
  (test #f (graph-isomorphic? (empty-graph 5) (cycle-graph 5)))
  (test #f (graph-isomorphic? (complete-bipartite-graph 3 3) (cycle-graph 6))))

(test-group "empty / singleton edge cases"
  (test #t (graph-isomorphic? (empty-graph 0) (empty-graph 0)))
  (test #t (graph-isomorphic? (empty-graph 1) (empty-graph 1)))
  (test #t (graph-isomorphic? (empty-graph 3) (empty-graph 3)))
  (test #f (graph-isomorphic? (empty-graph 2) (empty-graph 3))))

(test-group "canonical form is deterministic across alist-ordering"
  ;; Two differently-ordered adjacency alists of the same graph should
  ;; produce the same canonical form.
  (let ((a (make-graph '((0 . ((1) (2))) (1 . ((0) (2))) (2 . ((0) (1))))))
        (b (make-graph '((2 . ((0) (1))) (0 . ((2) (1))) (1 . ((2) (0)))))))
    (test #t (equal? (graph-canonical-form a) (graph-canonical-form b)))
    (test #t (graph-isomorphic? a b))))

(test-end)

(test-begin "combinatorial-graph-phase-4")

(test-group "spanning-tree-count: fast paths"
  ;; K_n via Cayley: τ(K_n) = n^(n-2)
  (test 1   (graph-spanning-tree-count (complete-graph 2)))   ;; 2^0
  (test 3   (graph-spanning-tree-count (complete-graph 3)))   ;; 3^1
  (test 16  (graph-spanning-tree-count (complete-graph 4)))   ;; 4^2
  (test 125 (graph-spanning-tree-count (complete-graph 5)))   ;; 5^3
  ;; Cycle: τ(C_n) = n
  (test 3   (graph-spanning-tree-count (cycle-graph 3)))      ;; same as K_3
  (test 5   (graph-spanning-tree-count (cycle-graph 5)))
  (test 7   (graph-spanning-tree-count (cycle-graph 7)))
  ;; Tree: τ = 1
  (test 1   (graph-spanning-tree-count (path-graph 2)))
  (test 1   (graph-spanning-tree-count (path-graph 5)))
  (test 1   (graph-spanning-tree-count (path-graph 10))))

(test-group "spanning-tree-count: edge cases"
  (test 0 (graph-spanning-tree-count (empty-graph 0)))
  (test 1 (graph-spanning-tree-count (empty-graph 1)))
  (test 0 (graph-spanning-tree-count (empty-graph 2)))
  (test 0 (graph-spanning-tree-count (empty-graph 5))))

(test-group "spanning-tree-count: disconnected → 0"
  (let ((G (make-graph
             '((0 . ((1))) (1 . ((0)))
               (2 . ((3))) (3 . ((2)))))))
    (test 0 (graph-spanning-tree-count G))))

(test-group "spanning-tree-count: Petersen = 2000 (Sedláček 1970)"
  (test 2000 (graph-spanning-tree-count (petersen-graph))))

(test-group "spanning-tree-count: general deletion-contraction"
  ;; Triangle + pendant: 4 vertices, 4 edges. The pendant must be in
  ;; every spanning tree (it's a bridge); the remaining 3 form K_3,
  ;; which contributes 3 spanning trees.
  (let ((G (make-graph
             '((a . ((b) (c)))
               (b . ((a) (c)))
               (c . ((a) (b) (d)))
               (d . ((c)))))))
    (test 3 (graph-spanning-tree-count G))))

(test-group "spanning-tree-count: K_4 minus one edge"
  ;; Book B_2 (two triangles sharing edge b-c): τ = 8.
  (let ((G (make-graph
             '((a . ((b) (c)))
               (b . ((a) (c) (d)))
               (c . ((a) (b) (d)))
               (d . ((b) (c)))))))
    (test 8 (graph-spanning-tree-count G))))

(test-group "spanning-tree-count: size cap diagnostic"
  ;; Build a 21-edge non-fast-path graph (complete graph minus something
  ;; that still exceeds |E| ≤ 20 after removing fast-path triggers).
  ;; K_7 has 21 edges, which would hit the cap — but K_7 matches %complete?
  ;; and uses the Cayley fast path instead. Add a self-loop to K_7 to
  ;; block %complete? and force the general path:
  (let ((adj
          (map (lambda (v)
                 (cons v
                       (let ((nbrs (filter (lambda (u) (not (= u v))) (iota 7))))
                         (map (lambda (u) (cons u #f)) nbrs))))
               (iota 7))))
    ;; Add a self-loop on vertex 0 to disable %complete? fast path.
    (let* ((with-loop (map (lambda (entry)
                             (if (= (car entry) 0)
                                 (cons 0 (cons (cons 0 #f) (cdr entry)))
                                 entry))
                           adj))
           (G (make-graph with-loop '(self-loops? . #t))))
      (test-error (graph-spanning-tree-count G)))))

(test-end)
(test-exit)
