;;; algebra-combinatorial-graph-test.scm — (wile algebra combinatorial-graph)

(import (scheme base)
        (srfi 1)
        (chibi test)
        (wile algebra setoid)
        (wile algebra combinatorial-graph))

;;; --- Inline fixtures ---------------------------------------------------

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

;;; --- Test helpers ------------------------------------------------------

(define (valid-matching? G M)
  ;; Returns #t if M is a valid matching of G: no vertex repeated on either
  ;; side, and every pair is an edge.
  (and (= (length (delete-duplicates (map car M))) (length M))
       (= (length (delete-duplicates (map cdr M))) (length M))
       (every (lambda (pr) (graph-edge? G (car pr) (cdr pr))) M)))

(define (permute-graph G perm)
  ;; Relabel G's vertices: vertex at position i in (graph-vertices G) →
  ;; perm[i]. Returns a new graph iso to G when perm is a permutation.
  (let* ((vs (graph-vertices G))
         (pos-of
           (lambda (v)
             (let loop ((xs vs) (i 0))
               (cond
                 ((null? xs) #f)
                 ((equal? v (car xs)) i)
                 (else (loop (cdr xs) (+ i 1)))))))
         (at (lambda (v) (list-ref perm (pos-of v)))))
    (make-graph
      (map
        (lambda (v)
          (cons (at v)
                (map (lambda (p) (cons (at (car p)) (cdr p)))
                     (graph-neighbors G v))))
        vs))))

;;; --- Test suite --------------------------------------------------------

(test-begin "combinatorial-graph")

;;; ===== Phase 1 — scaffold =============================================

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

(test-group "graph-has-vertex? membership predicate"
  (let ((G (make-graph k3-adj)))
    (test #t (graph-has-vertex? G 'a))
    (test #t (graph-has-vertex? G 'b))
    (test #f (graph-has-vertex? G 'z))
    (test #f (graph-has-vertex? G 42))))

(test-group "graph-neighbors raises on unknown vertex"
  (let ((G (make-graph k3-adj)))
    (test '((b) (c)) (graph-neighbors G 'a))
    (test-error (graph-neighbors G 'missing))))

(test-group "graph-degree raises on unknown vertex"
  (let ((G (make-graph k3-adj)))
    (test 2 (graph-degree G 'a))
    (test-error (graph-degree G 'missing))))

(test-group "graph-bfs raises on unknown source"
  (let ((G (make-graph k3-adj)))
    (test-error (graph-bfs G 'missing))))

(test-group "graph-dfs raises on unknown source"
  (let ((G (make-graph k3-adj)))
    (test-error (graph-dfs G 'missing))))

(test-group "graph-vertex-equiv? delegates to setoid"
  (let ((G (make-graph k3-adj)))
    (test #t (graph-vertex-equiv? G 'a 'a))
    (test #f (graph-vertex-equiv? G 'a 'b))))

(test-group "symmetrize? option"
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

(test-group "symmetrize? rejected when multi? = #t"
  (test-error
    (make-graph
      '((a . ((b))) (b . ((a))))
      '(multi? . #t)
      '(symmetrize? . #t))))

(test-group "validate-graph on valid graphs returns #t"
  (test #t (validate-graph (make-graph k3-adj) '()))
  (test #t (validate-graph (make-graph c4-adj) '()))
  (test #t (validate-graph (make-graph p3-directed-adj '(directed? . #t)) '())))

(test-group "validate-graph catches asymmetric undirected"
  (let* ((bad '((a . ((b))) (b . ())))
         (G   (make-graph bad)))
    (let ((result (validate-graph G '())))
      (test #t (and (list? result)
                    (any (lambda (v) (eq? (car v) 'asymmetric-undirected))
                         result))))))

(test-group "validate-graph catches unknown neighbor"
  (let* ((bad '((a . ((z)))))
         (G   (make-graph bad)))
    (let ((result (validate-graph G '())))
      (test #t (and (list? result)
                    (any (lambda (v) (eq? (car v) 'unknown-neighbor))
                         result))))))

(test-group "validate-graph catches parallel edges when multi? = #f"
  (let* ((bad '((a . ((b) (b))) (b . ((a) (a)))))
         (G   (make-graph bad)))
    (let ((result (validate-graph G '())))
      (test #t (and (list? result)
                    (any (lambda (v) (eq? (car v) 'parallel-edge))
                         result))))))

(test-group "validate-graph catches duplicate-vertex"
  ;; Two adjacency entries for the same vertex identity.
  (let* ((bad '((a . ((b))) (a . ((c))) (b . ((a))) (c . ((a)))))
         (G   (make-graph bad)))
    (let ((result (validate-graph G '())))
      (test #t (and (list? result)
                    (any (lambda (v) (eq? (car v) 'duplicate-vertex))
                         result))))))

(test-group "parallel edges allowed when multi? = #t"
  (let* ((adj '((a . ((b) (b))) (b . ((a) (a)))))
         (G   (make-graph adj '(multi? . #t))))
    (test #t (validate-graph G '()))
    (test #t (graph-multi? G))))

(test-group "validate-graph catches self-loop when self-loops? = #f"
  (let* ((adj '((v . ((v)))))
         (G   (make-graph adj '(self-loops? . #f))))
    (let ((result (validate-graph G '())))
      (test #t (and (list? result)
                    (any (lambda (v) (eq? (car v) 'unexpected-self-loop))
                         result))))))

(test-group "assert-graph raises on invalid"
  (let ((G (make-graph '((a . ((z)))))))
    (test-error (assert-graph G '()))))

(test-group "assert-graph silent on valid"
  (let ((G (make-graph k3-adj)))
    (assert-graph G '())  ;; no error
    (test #t #t)))

(test-group "make-graph rejects unknown option keys"
  (test-error (make-graph k3-adj '(directd? . #t)))
  (test-error (make-graph k3-adj '(not-a-known-key . 42))))

(test-group "make-graph rejects malformed adjacency shape"
  (test-error (make-graph '(1 2 3)))                   ;; entries not pairs
  (test-error (make-graph '((a . b))))                  ;; cdr not a list
  (test-error (make-graph '((a . (b c))))))             ;; neighbor not a pair

(test-group "make-graph rejects tier-1 + tier-2 coexistence"
  (test-error
    (make-graph k3-adj
      (cons 'seed 'a)
      (cons 'neighbor-fn (lambda (v) '())))))

(test-group "finite-graph? predicate"
  (test #t (finite-graph? (make-graph k3-adj)))
  (test #t (finite-graph? (make-graph '())))        ;; empty adj is tier-1
  (let ((G (make-graph '()
                       (cons 'seed 'root)
                       (cons 'neighbor-fn (lambda (v) '())))))
    (test #f (finite-graph? G))
    (test #t (finitely-generated-graph? G))))

(test-group "enumerate-finite-graph is idempotent on tier-1"
  (let ((G (make-graph k3-adj)))
    (let ((G* (enumerate-finite-graph G)))
      (test 3 (graph-order G*))
      (test 3 (graph-size  G*))
      (test #t (eq? G G*)))))

(test-group "enumerate-finite-graph promotes tier-2 to tier-1"
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

(test-group "enumerate-finite-graph preserves seed/neighbor-fn forward"
  ;; Q-e: the enumerated result still carries its generator for reuse.
  (let* ((nfn (lambda (v)
                (cond
                  ((eqv? v 0) (list (cons 1 #f)))
                  ((eqv? v 1) (list (cons 2 #f)))
                  (else '()))))
         (G  (make-graph '()
                         '(directed? . #t)
                         (cons 'seed 0)
                         (cons 'neighbor-fn nfn)))
         (G* (enumerate-finite-graph G)))
    (test #t (finitely-generated-graph? G*))))

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

(test-group "setoid-carried vertex equality: construction"
  (let* ((adj `(("a" . (("b")))
                ("b" . (("a")))))
         (G   (make-graph adj (cons 'setoid (string-setoid)))))
    (test #t (graph-edge? G "a" "b"))
    (test #t (graph-vertex-equiv? G "a" "a"))))

;;; ===== Phase 2 — traversal and bipartite ==============================

(test-group "graph-bfs on cycle"
  (let ((C4 (make-graph (inline-cycle-adj 4))))
    (test 4 (length (graph-bfs C4 0)))
    (test 0 (car (graph-bfs C4 0)))))

(test-group "graph-dfs on cycle is a permutation of vertices"
  (let ((C4 (make-graph (inline-cycle-adj 4))))
    (test 4 (length (graph-dfs C4 0)))
    (test 0 (car (graph-dfs C4 0)))
    ;; Vertices must be distinct (DFS doesn't revisit)
    (test 4 (length (delete-duplicates (graph-dfs C4 0))))))

(test-group "graph-bfs on path"
  (let ((P5 (make-graph (inline-path-adj 5))))
    (test '(0 1 2 3 4) (graph-bfs P5 0))
    (test '(4 3 2 1 0) (graph-bfs P5 4))))

(test-group "connected components: single component"
  (let ((C4 (make-graph (inline-cycle-adj 4))))
    (test 1 (length (graph-connected-components C4)))
    (test 4 (length (car (graph-connected-components C4))))))

(test-group "connected components: disjoint union covers all vertices"
  (let* ((G (make-graph
              '((a . ((b))) (b . ((a)))
                (c . ((d))) (d . ((c))))))
         (cs (graph-connected-components G)))
    (test 2 (length cs))
    (test (graph-order G) (apply + (map length cs)))))

(test-group "connected components on directed graph: weak components"
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

(test-group "graph-bipartition returns two parts that partition V"
  (let* ((C4   (make-graph (inline-cycle-adj 4)))
         (parts (graph-bipartition C4)))
    (test 2 (length parts))
    (test (graph-order C4) (+ (length (car parts)) (length (cadr parts))))
    ;; Every edge crosses parts.
    (test #t
      (every
        (lambda (edge)
          (let ((u (car edge)) (v (cadr edge)))
            (not (equal?
                   (if (member u (car parts)) 'A 'B)
                   (if (member v (car parts)) 'A 'B)))))
        (graph-edges C4)))))

(test-group "graph-bipartition raises on odd cycle"
  (test-error (graph-bipartition (make-graph (inline-cycle-adj 5)))))

(test-group "bipartiteness: K_{m,n} is bipartite"
  (let ((K23 (make-graph
               '((a . ((x) (y) (z)))
                 (b . ((x) (y) (z)))
                 (x . ((a) (b)))
                 (y . ((a) (b)))
                 (z . ((a) (b)))))))
    (test #t (graph-bipartite? K23))
    (let ((parts (graph-bipartition K23)))
      (test 2 (length parts))
      (test 5 (+ (length (car parts)) (length (cadr parts)))))))

;;; ===== Phase 3 — isomorphism + presets ================================

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

(test-group "path iso path (non-regular, fast-path)"
  (test #t (graph-isomorphic? (path-graph 4) (path-graph 4)))
  (test #t (graph-isomorphic? (path-graph 5) (path-graph 5))))

(test-group "complete-bipartite iso complete-bipartite"
  (test #t (graph-isomorphic? (complete-bipartite-graph 2 3)
                              (complete-bipartite-graph 2 3))))

(test-group "complete graph self-iso (regular, backtracking)"
  (test #t (graph-isomorphic? (complete-graph 3) (complete-graph 3)))
  (test #t (graph-isomorphic? (complete-graph 4) (complete-graph 4)))
  (test #t (graph-isomorphic? (complete-graph 5) (complete-graph 5))))

(test-group "cycle self-iso (vertex-transitive, backtracking)"
  (test #t (graph-isomorphic? (cycle-graph 4) (cycle-graph 4)))
  (test #t (graph-isomorphic? (cycle-graph 5) (cycle-graph 5)))
  (test #t (graph-isomorphic? (cycle-graph 6) (cycle-graph 6))))

(test-group "BACKTRACKING CORRECTNESS CANARY — Petersen self-iso → #t"
  (test #t (graph-isomorphic? (petersen-graph) (petersen-graph))))

(test-group "iso under multiple relabelings"
  ;; Each relabeling exercises a distinct path through the backtracking
  ;; target-cell selector.
  (let ((perms
          (list
            ;; A hand-picked non-identity permutation.
            (list 5 2 8 1 9 0 7 4 3 6)
            ;; Reverse.
            (list 9 8 7 6 5 4 3 2 1 0)
            ;; Shift by 1.
            (list 1 2 3 4 5 6 7 8 9 0))))
    (for-each
      (lambda (perm)
        (test #t (graph-isomorphic? (petersen-graph)
                                    (permute-graph (petersen-graph) perm))))
      perms))
  ;; K_5 under a non-identity permutation.
  (test #t (graph-isomorphic? (complete-graph 5)
                              (permute-graph (complete-graph 5)
                                             '(4 3 2 1 0))))
  ;; K_{2,3} under a non-trivial permutation.
  (test #t (graph-isomorphic? (complete-bipartite-graph 2 3)
                              (permute-graph (complete-bipartite-graph 2 3)
                                             '(3 4 0 2 1))))
  ;; C_7 under rotation.
  (test #t (graph-isomorphic? (cycle-graph 7)
                              (permute-graph (cycle-graph 7)
                                             '(3 4 5 6 0 1 2)))))

(test-group "different vertex count → #f (short-circuit)"
  (test #f (graph-isomorphic? (complete-graph 3) (complete-graph 4)))
  (test #f (graph-isomorphic? (path-graph 5)     (path-graph 6))))

(test-group "different edge count → #f (short-circuit)"
  (test #f (graph-isomorphic? (complete-graph 4) (cycle-graph 4)))
  (test #f (graph-isomorphic? (cycle-graph 5)    (path-graph 5))))

(test-group "different degree sequence → #f"
  (let ((star (make-graph '((c . ((a) (b) (d)))
                             (a . ((c)))
                             (b . ((c)))
                             (d . ((c))))))
        (p4   (path-graph 4)))
    (test #f (graph-isomorphic? star p4))))

(test-group "COSPECTRAL NON-ISO CANARY — C_6 vs 2K_3"
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

(test-group "unlabeled iso: edge-data does not participate"
  ;; Q-a: two graphs with identical topology but different edge-data
  ;; payloads are iso.
  (let ((G1 (make-graph '((a . ((b . "red"))) (b . ((a . "red"))))))
        (G2 (make-graph '((x . ((y . 42)))   (y . ((x . 42)))))))
    (test #t (graph-isomorphic? G1 G2))))

(test-group "empty / singleton edge cases"
  (test #t (graph-isomorphic? (empty-graph 0) (empty-graph 0)))
  (test #t (graph-isomorphic? (empty-graph 1) (empty-graph 1)))
  (test #t (graph-isomorphic? (empty-graph 3) (empty-graph 3)))
  (test #f (graph-isomorphic? (empty-graph 2) (empty-graph 3))))

(test-group "canonical form is deterministic across alist-ordering"
  (let ((a (make-graph '((0 . ((1) (2))) (1 . ((0) (2))) (2 . ((0) (1))))))
        (b (make-graph '((2 . ((0) (1))) (0 . ((2) (1))) (1 . ((2) (0)))))))
    (test #t (equal? (graph-canonical-form a) (graph-canonical-form b)))
    (test #t (graph-isomorphic? a b))))

(test-group "canonical form determinism on Petersen"
  ;; Reordering the adjacency alist must produce the same canonical form.
  (let* ((P  (petersen-graph))
         (P2 (permute-graph P (list 9 8 7 6 5 4 3 2 1 0))))
    (test #t (equal? (graph-canonical-form P) (graph-canonical-form P2)))))

;;; ===== Phase 4 — spanning tree =========================================

(test-group "spanning-tree-count: fast paths"
  (test 1   (graph-spanning-tree-count (complete-graph 2)))
  (test 3   (graph-spanning-tree-count (complete-graph 3)))
  (test 16  (graph-spanning-tree-count (complete-graph 4)))
  (test 125 (graph-spanning-tree-count (complete-graph 5)))
  (test 3   (graph-spanning-tree-count (cycle-graph 3)))
  (test 5   (graph-spanning-tree-count (cycle-graph 5)))
  (test 7   (graph-spanning-tree-count (cycle-graph 7)))
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
  ;; Triangle + pendant: τ = 3.
  (let ((G (make-graph
             '((a . ((b) (c)))
               (b . ((a) (c)))
               (c . ((a) (b) (d)))
               (d . ((c)))))))
    (test 3 (graph-spanning-tree-count G))))

(test-group "spanning-tree-count: K_4 minus one edge"
  ;; Book B_2: τ = 8.
  (let ((G (make-graph
             '((a . ((b) (c)))
               (b . ((a) (c) (d)))
               (c . ((a) (b) (d)))
               (d . ((b) (c)))))))
    (test 8 (graph-spanning-tree-count G))))

(test-group "spanning-tree-count: multigraph (parallel edges)"
  ;; Two parallel edges between u, v: τ = 2.
  (let ((G (make-graph
             '((u . ((v) (v)))
               (v . ((u) (u))))
             '(multi? . #t))))
    (test 2 (graph-spanning-tree-count G))))

(test-group "spanning-tree-count: directed graphs raise"
  (let ((G (make-graph p3-directed-adj '(directed? . #t))))
    (test-error (graph-spanning-tree-count G))))

(test-group "spanning-tree-count: size cap diagnostic"
  (let ((adj
          (map (lambda (v)
                 (cons v
                       (let ((nbrs (filter (lambda (u) (not (= u v))) (iota 7))))
                         (map (lambda (u) (cons u #f)) nbrs))))
               (iota 7))))
    (let* ((with-loop (map (lambda (entry)
                             (if (= (car entry) 0)
                                 (cons 0 (cons (cons 0 #f) (cdr entry)))
                                 entry))
                           adj))
           (G (make-graph with-loop '(self-loops? . #t))))
      (test-error (graph-spanning-tree-count G)))))

;;; ===== Phase 5 — chromatic polynomial =================================

(test-group "chromatic: K_n via falling-factorial fast path"
  (test '(0 1)                  (graph-chromatic-polynomial (complete-graph 1)))
  (test '(0 -1 1)               (graph-chromatic-polynomial (complete-graph 2)))
  (test '(0 2 -3 1)             (graph-chromatic-polynomial (complete-graph 3)))
  (test '(0 -6 11 -6 1)         (graph-chromatic-polynomial (complete-graph 4)))
  (test '(0 24 -50 35 -10 1)    (graph-chromatic-polynomial (complete-graph 5))))

(test-group "chromatic: empty-graph fast path"
  (test '(1)          (graph-chromatic-polynomial (empty-graph 0)))
  (test '(0 1)        (graph-chromatic-polynomial (empty-graph 1)))
  (test '(0 0 1)      (graph-chromatic-polynomial (empty-graph 2)))
  (test '(0 0 0 0 1)  (graph-chromatic-polynomial (empty-graph 4))))

(test-group "chromatic: tree fast path (x(x-1)^(n-1))"
  (test '(0 1)          (graph-chromatic-polynomial (path-graph 1)))
  (test '(0 -1 1)       (graph-chromatic-polynomial (path-graph 2)))
  (test '(0 1 -2 1)     (graph-chromatic-polynomial (path-graph 3)))
  (test '(0 -1 3 -3 1)  (graph-chromatic-polynomial (path-graph 4))))

(test-group "chromatic: cycle fast path"
  (test '(0 2 -3 1)             (graph-chromatic-polynomial (cycle-graph 3)))
  (test '(0 -3 6 -4 1)          (graph-chromatic-polynomial (cycle-graph 4)))
  (test '(0 4 -10 10 -5 1)      (graph-chromatic-polynomial (cycle-graph 5)))
  (test '(0 -5 15 -20 15 -6 1)  (graph-chromatic-polynomial (cycle-graph 6))))

(test-group "chromatic: general deletion-contraction"
  ;; Triangle + pendant: χ = x(x-1)²(x-2) = x^4 - 4x^3 + 5x^2 - 2x.
  (let ((G (make-graph
             '((a . ((b) (c)))
               (b . ((a) (c)))
               (c . ((a) (b) (d)))
               (d . ((c)))))))
    (test '(0 -2 5 -4 1) (graph-chromatic-polynomial G))))

(test-group "chromatic: directed graphs raise"
  (let ((G (make-graph p3-directed-adj '(directed? . #t))))
    (test-error (graph-chromatic-polynomial G))))

(test-group "chromatic: size cap diagnostic"
  (let* ((n 8)
         (edges (append
                  (map (lambda (i) (list i (modulo (+ i 1) n))) (iota n))
                  (list '(0 2) '(0 3) '(0 4) '(0 5) '(0 6))))
         (adj (map
                (lambda (v)
                  (cons v
                        (filter-map
                          (lambda (e)
                            (cond
                              ((= (car e) v)  (cons (cadr e) #f))
                              ((= (cadr e) v) (cons (car e) #f))
                              (else           #f)))
                          edges)))
                (iota n)))
         (G (make-graph adj)))
    (test-error (graph-chromatic-polynomial G))))

;;; ===== Phase 6 — Tutte polynomial =====================================

(test-group "Tutte: base cases"
  (test '((1)) (graph-tutte-polynomial (empty-graph 0)))
  (test '((1)) (graph-tutte-polynomial (empty-graph 1)))
  (test '((1)) (graph-tutte-polynomial (empty-graph 5))))

(test-group "Tutte: basic cases"
  (test '(() (1))     (graph-tutte-polynomial (path-graph 2)))
  (test '(() () (1))  (graph-tutte-polynomial (path-graph 3))))

(test-group "Tutte: triangle"
  (test '((0 1) (1) (1)) (graph-tutte-polynomial (complete-graph 3))))

(test-group "Tutte: cycles"
  (test '((0 1) (1) (1))         (graph-tutte-polynomial (cycle-graph 3)))
  (test '((0 1) (1) (1) (1))     (graph-tutte-polynomial (cycle-graph 4)))
  (test '((0 1) (1) (1) (1) (1)) (graph-tutte-polynomial (cycle-graph 5))))

(test-group "Tutte: K_4 (Tutte 1954 reference)"
  ;; T(K_4) = x^3 + 3x^2 + 2x + 4xy + 2y + 3y^2 + y^3
  (test '((0 2 3 1) (2 4) (3) (1)) (graph-tutte-polynomial (complete-graph 4))))

(test-group "Tutte: directed graphs raise"
  (let ((G (make-graph p3-directed-adj '(directed? . #t))))
    (test-error (graph-tutte-polynomial G))))

;;; Chromatic-from-Tutte identity (Tutte 1954 §9)

(define (tutte-at-1-minus-x-0 T)
  (let ((p-in-x (map (lambda (row) (if (null? row) 0 (car row))) T)))
    (let loop ((coefs p-in-x) (i 0) (acc '(0)))
      (cond
        ((null? coefs) acc)
        (else
         (loop (cdr coefs) (+ i 1)
               (poly-add acc
                         (poly-scale (%expand-1-minus-x^i i) (car coefs)))))))))

(define (%expand-1-minus-x^i i)
  (let loop ((k 0) (acc '(1)))
    (cond
      ((= k i) acc)
      (else (loop (+ k 1) (poly-sub acc (cons 0 acc)))))))

(define (poly-add p q)
  (let loop ((p p) (q q) (acc '()))
    (cond
      ((and (null? p) (null? q)) (reverse acc))
      ((null? p) (loop '() (cdr q) (cons (car q) acc)))
      ((null? q) (loop (cdr p) '() (cons (car p) acc)))
      (else (loop (cdr p) (cdr q) (cons (+ (car p) (car q)) acc))))))

(define (poly-sub p q)
  (let loop ((p p) (q q) (acc '()))
    (cond
      ((and (null? p) (null? q)) (reverse acc))
      ((null? p) (loop '() (cdr q) (cons (- (car q)) acc)))
      ((null? q) (loop (cdr p) '() (cons (car p) acc)))
      (else (loop (cdr p) (cdr q) (cons (- (car p) (car q)) acc))))))

(define (poly-scale p c)
  (map (lambda (a) (* a c)) p))

(define (poly-trim p)
  (reverse (let drop ((xs (reverse p)))
             (cond
               ((null? xs) '())
               ((and (= (car xs) 0) (pair? (cdr xs))) (drop (cdr xs)))
               (else xs)))))

(define (poly-shift p k)
  (append (make-list k 0) p))

(define (chromatic-from-tutte G)
  (let* ((T (graph-tutte-polynomial G))
         (at (tutte-at-1-minus-x-0 T))
         (c  (length (graph-connected-components G)))
         (v  (graph-order G))
         (sign (if (odd? (- v c)) -1 1)))
    (poly-trim (poly-scale (poly-shift at c) sign))))

(test-group "chromatic-from-Tutte consistency: fast-path graphs"
  (test (graph-chromatic-polynomial (complete-graph 3))
        (chromatic-from-tutte (complete-graph 3)))
  (test (graph-chromatic-polynomial (complete-graph 4))
        (chromatic-from-tutte (complete-graph 4)))
  (test (graph-chromatic-polynomial (cycle-graph 4))
        (chromatic-from-tutte (cycle-graph 4)))
  (test (graph-chromatic-polynomial (cycle-graph 5))
        (chromatic-from-tutte (cycle-graph 5)))
  (test (graph-chromatic-polynomial (path-graph 4))
        (chromatic-from-tutte (path-graph 4))))

(test-group "chromatic-from-Tutte consistency: general deletion-contraction"
  ;; Triangle + pendant and K_4-minus-edge: neither hits a chromatic OR
  ;; Tutte fast path; exercises the %nat-chromatic / %nat-tutte recursion.
  (let ((triangle+pendant
          (make-graph
            '((a . ((b) (c)))
              (b . ((a) (c)))
              (c . ((a) (b) (d)))
              (d . ((c))))))
        (k4-minus-edge
          (make-graph
            '((a . ((b) (c)))
              (b . ((a) (c) (d)))
              (c . ((a) (b) (d)))
              (d . ((b) (c)))))))
    (test (graph-chromatic-polynomial triangle+pendant)
          (chromatic-from-tutte triangle+pendant))
    (test (graph-chromatic-polynomial k4-minus-edge)
          (chromatic-from-tutte k4-minus-edge))))

;;; ===== Phase 7 — bipartite matching ===================================

(test-group "matching size on complete bipartite graphs"
  (test 0 (length (graph-maximum-bipartite-matching (complete-bipartite-graph 0 0))))
  (test 0 (length (graph-maximum-bipartite-matching (complete-bipartite-graph 0 3))))
  (test 0 (length (graph-maximum-bipartite-matching (complete-bipartite-graph 3 0))))
  (test 1 (length (graph-maximum-bipartite-matching (complete-bipartite-graph 1 1))))
  (test 2 (length (graph-maximum-bipartite-matching (complete-bipartite-graph 2 3))))
  (test 2 (length (graph-maximum-bipartite-matching (complete-bipartite-graph 2 4))))
  (test 3 (length (graph-maximum-bipartite-matching (complete-bipartite-graph 3 3))))
  (test 3 (length (graph-maximum-bipartite-matching (complete-bipartite-graph 3 5))))
  (test 4 (length (graph-maximum-bipartite-matching (complete-bipartite-graph 4 4)))))

(test-group "matching returns valid matching (not just correct size)"
  (for-each
    (lambda (G)
      (let ((M (graph-maximum-bipartite-matching G)))
        (test #t (valid-matching? G M))))
    (list (complete-bipartite-graph 3 3)
          (complete-bipartite-graph 2 4)
          (complete-bipartite-graph 4 4)
          (path-graph 4)
          (path-graph 6)
          (cycle-graph 4)
          (cycle-graph 6))))

(test-group "matching on paths"
  (test 0 (length (graph-maximum-bipartite-matching (path-graph 1))))
  (test 1 (length (graph-maximum-bipartite-matching (path-graph 2))))
  (test 1 (length (graph-maximum-bipartite-matching (path-graph 3))))
  (test 2 (length (graph-maximum-bipartite-matching (path-graph 4))))
  (test 2 (length (graph-maximum-bipartite-matching (path-graph 5))))
  (test 3 (length (graph-maximum-bipartite-matching (path-graph 6)))))

(test-group "matching on even cycles"
  (test 2 (length (graph-maximum-bipartite-matching (cycle-graph 4))))
  (test 3 (length (graph-maximum-bipartite-matching (cycle-graph 6)))))

(test-group "matching raises on non-bipartite"
  (test-error (graph-maximum-bipartite-matching (cycle-graph 5)))
  (test-error (graph-maximum-bipartite-matching (cycle-graph 3)))
  (test-error (graph-maximum-bipartite-matching (complete-graph 3))))

(test-group "matching on an irregular bipartite graph"
  (let ((G (make-graph
             '((a1 . ((b1) (b2)))
               (a2 . ((b2)))
               (a3 . ((b3)))
               (b1 . ((a1)))
               (b2 . ((a1) (a2)))
               (b3 . ((a3)))))))
    (let ((M (graph-maximum-bipartite-matching G)))
      (test 3 (length M))
      (test #t (valid-matching? G M)))))

(test-group "matching König-style: greedy would miss, Hopcroft-Karp doesn't"
  (let ((G (make-graph
             '((a1 . ((b1) (b2)))
               (a2 . ((b1)))
               (b1 . ((a1) (a2)))
               (b2 . ((a1)))))))
    (let ((M (graph-maximum-bipartite-matching G)))
      (test 2 (length M))
      (test #t (valid-matching? G M)))))

(test-group "matching: directed graphs raise"
  (let ((G (make-graph p3-directed-adj '(directed? . #t))))
    (test-error (graph-maximum-bipartite-matching G))))

;;; ===== Setoid end-to-end ==============================================

(test-group "setoid-carried vertex equality: end-to-end"
  ;; Run the whole stack through string-setoid to exercise that every
  ;; primitive routes vertex comparison through the graph's setoid
  ;; (not through Scheme equal?).
  (let* ((k3-strings '(("a" . (("b") ("c")))
                       ("b" . (("a") ("c")))
                       ("c" . (("a") ("b")))))
         (G (make-graph k3-strings (cons 'setoid (string-setoid)))))
    ;; Core queries
    (test 3 (graph-order G))
    (test 3 (graph-size G))
    (test 2 (graph-degree G "a"))
    (test #t (graph-has-vertex? G "a"))
    (test #f (graph-has-vertex? G "z"))
    ;; Traversal
    (test 3 (length (graph-bfs G "a")))
    (test 3 (length (graph-dfs G "a")))
    (test 1 (length (graph-connected-components G)))
    ;; Bipartite (K_3 is not bipartite)
    (test #f (graph-bipartite? G))
    ;; Isomorphism (K_3 iso to integer K_3; edge-data ignored)
    (test #t (graph-isomorphic? G (complete-graph 3)))
    ;; Chromatic and spanning-tree
    (test 3 (graph-spanning-tree-count G))
    (test '(0 2 -3 1) (graph-chromatic-polynomial G))))

(test-end)
(test-exit)
