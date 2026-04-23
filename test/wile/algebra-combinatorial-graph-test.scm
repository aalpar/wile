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
(test-exit)
