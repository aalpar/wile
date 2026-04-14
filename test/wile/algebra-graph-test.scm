;;; algebra-graph-test.scm — Semiring graph analysis tests

(import (scheme base)
        (chibi test)
        (wile algebra semiring)
        (wile algebra graph))

;; Test graph:
;;   A → B (weight 1)
;;   A → C (weight 1)
;;   B → D (weight 1)
;;   C → D (weight 1)
(define test-adj
  '(("A" . (("B" . 1) ("C" . 1)))
    ("B" . (("D" . 1)))
    ("C" . (("D" . 1)))
    ("D" . ())))

(test-begin "graph-analysis")

(test-group "graph-analysis? predicate"
  (let ((ga (make-graph-analysis (boolean-semiring) test-adj #f)))
    (test #t (graph-analysis? ga))
    (test #f (graph-analysis? 42))
    (test #f (graph-analysis? '()))))

(test-group "boolean semiring (reachability)"
  (let ((ga (make-graph-analysis (boolean-semiring) test-adj #f)))
    ;; A can reach all nodes
    (test #t (graph-query ga "A" "B"))
    (test #t (graph-query ga "A" "C"))
    (test #t (graph-query ga "A" "D"))
    ;; D cannot reach A (directed graph)
    (test #f (graph-query ga "D" "A"))
    ;; D cannot reach B
    (test #f (graph-query ga "D" "B"))
    ;; B can reach D but not C
    (test #t (graph-query ga "B" "D"))
    (test #f (graph-query ga "B" "C"))))

(test-group "tropical semiring (shortest path)"
  (let ((ga (make-graph-analysis (tropical-semiring) test-adj
              (lambda (e) e))))
    ;; A→B = 1 hop
    (test 1 (graph-query ga "A" "B"))
    ;; A→D = 2 hops (A→B→D or A→C→D)
    (test 2 (graph-query ga "A" "D"))
    ;; A→A = 0 (source to itself = semiring-one = 0 in tropical)
    (test 0 (graph-query ga "A" "A"))
    ;; Unreachable = tropical-inf
    (test tropical-inf (graph-query ga "D" "A"))))

(test-group "custom edge weights"
  ;; Multiply each weight by 10
  (let ((ga (make-graph-analysis (tropical-semiring) test-adj
              (lambda (e) (* e 10)))))
    (test 10 (graph-query ga "A" "B"))
    (test 20 (graph-query ga "A" "D"))))

(test-group "caching"
  (let ((ga (make-graph-analysis (boolean-semiring) test-adj #f)))
    ;; First query computes and caches
    (test #t (graph-query ga "A" "D"))
    ;; Second query uses cache — same result
    (test #t (graph-query ga "A" "D"))
    ;; Different source
    (test #f (graph-query ga "D" "A"))))

(test-group "graph-query-all"
  (let ((ga (make-graph-analysis (boolean-semiring) test-adj #f)))
    (let ((result (graph-query-all ga "A")))
      ;; Should contain entries for A, B, C, D
      (test #t (and (assoc "A" result) #t))
      (test #t (and (assoc "B" result) #t))
      (test #t (and (assoc "C" result) #t))
      (test #t (and (assoc "D" result) #t))
      ;; All reachable
      (test #t (cdr (assoc "A" result)))
      (test #t (cdr (assoc "B" result)))))
  ;; From D: only D itself
  (let ((ga (make-graph-analysis (boolean-semiring) test-adj #f)))
    (let ((result (graph-query-all ga "D")))
      (test #t (and (assoc "D" result) #t))
      (test #f (assoc "A" result)))))

(test-group "counting semiring (path count)"
  (let ((ga (make-graph-analysis (counting-semiring) test-adj
              (lambda (e) e))))
    ;; A→D: two paths (A→B→D and A→C→D), each weight 1*1=1, sum = 2
    (test 2 (graph-query ga "A" "D"))
    ;; A→B: one path, weight 1
    (test 1 (graph-query ga "A" "B"))))

(test-end)
(test-exit)
