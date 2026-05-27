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

;; Regression: counting semiring on a non-trivially-ordered DAG.
;; The previous worklist-with-propagate-on-pop algorithm over-counted because
;; once a node's count was popped and propagated, a later update to that
;; node's count would re-pop it and re-propagate the full new value — adding
;; to what was already sent forward. This test pins the correct count.
(define diamond-with-sink-adj
  '(("A" . (("B") ("C")))
    ("B" . (("D")))
    ("C" . (("D")))
    ("D" . (("E")))   ; D has a successor — exposes the over-count
    ("E" . ())))

(test-group "counting semiring on diamond-with-sink DAG"
  (let ((ga (make-graph-analysis (counting-semiring) diamond-with-sink-adj #f)))
    ;; Two paths to D (A→B→D and A→C→D)
    (test 2 (graph-query ga "A" "D"))
    ;; Same two paths extended through D, so two paths to E
    ;; (A→B→D→E and A→C→D→E), NOT three.
    (test 2 (graph-query ga "A" "E"))))

;; Regression: counting semiring on a cyclic graph used to hang
;; indefinitely (memory/feedback-counting-semiring-on-cycles.md
;; records a 3-hour incident). compute-via-worklist now caps at
;; 2·V·E outer-loop iterations and raises an error pointing the
;; caller at (wile algebragraph) count-paths-cyclic. This test pins
;; the cap rather than the message text — text is brittle, the
;; raise-vs-hang behavior is the contract.
(define cyclic-adj
  '(("A" . (("B")))
    ("B" . (("C")))
    ("C" . (("A")))))    ; back-edge → cyclic; counting-semiring diverges

(test-group "counting semiring on cyclic graph raises"
  (let ((ga (make-graph-analysis (counting-semiring) cyclic-adj #f)))
    (test-error (graph-query ga "A" "C"))))

;; --- Sub-path 4A: bigint-counting-semiring fast path ---
;;
;; The fast path attaches when the semiring declares carrier 'big-int AND
;; weight-fn is #f. Queries route through count-paths-in-dag, which does
;; in-place *big.Int arithmetic instead of allocating per relaxation.

(test-group "fast path — eligibility"
  ;; bigint carrier + #f wfn → attaches
  (let ((ga (make-graph-analysis (bigint-counting-semiring) test-adj #f)))
    (test #t (graph-analysis-fast-path? ga))
    (test 'unit-weight-counting (graph-analysis-fast-path-kind ga)))
  ;; bigint carrier + non-#f wfn → does NOT attach (defers to slow path
  ;; pending sub-path 4B)
  (let ((ga (make-graph-analysis (bigint-counting-semiring) test-adj
              (lambda (_) 1))))
    (test #f (graph-analysis-fast-path? ga))
    (test #f (graph-analysis-fast-path-kind ga)))
  ;; Non-bigint carrier (default counting-semiring) → does NOT attach
  (let ((ga (make-graph-analysis (counting-semiring) test-adj #f)))
    (test #f (graph-analysis-fast-path? ga))
    (test #f (graph-analysis-fast-path-kind ga))))

(test-group "fast path — diamond DAG counts match slow path"
  ;; Diamond: A→{B,C}→D. Two paths from A to D.
  (let ((fast (make-graph-analysis (bigint-counting-semiring) test-adj #f))
        (slow (make-graph-analysis (counting-semiring) test-adj #f)))
    (test (graph-query slow "A" "D") (graph-query fast "A" "D"))
    (test (graph-query slow "A" "B") (graph-query fast "A" "B"))
    (test (graph-query slow "A" "A") (graph-query fast "A" "A"))))

(test-group "fast path — diamond-with-sink: pins the over-count regression"
  ;; The over-count regression that motivated topological-order processing
  ;; would yield 3 for A→E. The fast path's kernel uses reverse-postorder
  ;; propagation; this confirms it doesn't reintroduce the bug.
  (let ((ga (make-graph-analysis (bigint-counting-semiring)
                                 diamond-with-sink-adj #f)))
    (test 2 (graph-query ga "A" "D"))
    (test 2 (graph-query ga "A" "E"))))

(test-group "fast path — unreachable nodes absent from alist"
  ;; Querying from D in test-adj reaches only D itself.
  (let ((ga (make-graph-analysis (bigint-counting-semiring) test-adj #f)))
    (let ((dist (graph-query-all ga "D")))
      (test #t (and (assoc "D" dist) #t))
      (test #f (assoc "A" dist))
      (test #f (assoc "B" dist))
      (test #f (assoc "C" dist))
      ;; semiring-zero for unreachable
      (test 0 (graph-query ga "D" "A")))))

(test-group "fast path — cyclic input raises"
  ;; The kernel returns #f on cyclic-from-source input; the wrapper
  ;; surfaces this as an error rather than spinning. Pins the
  ;; raise-vs-hang behaviour rather than the message text.
  (let ((ga (make-graph-analysis (bigint-counting-semiring) cyclic-adj #f)))
    (test-error (graph-query ga "A" "C"))))

(test-group "fast path — vertex appearing only as edge target"
  ;; A graph where some node is referenced from another's out-edges but
  ;; has no entry of its own. The wrapper must include it in the
  ;; vertex set; otherwise the kernel sees a missing index.
  (let* ((adj '(("X" . (("Y" . 1) ("Z" . 1)))))   ; Y, Z have no own entries
         (ga  (make-graph-analysis (bigint-counting-semiring) adj #f)))
    (test 1 (graph-query ga "X" "Y"))
    (test 1 (graph-query ga "X" "Z"))))

(test-group "fast path — caching shares the slow-path machinery"
  ;; The cache layer sits above compute-single-source; the fast path
  ;; benefits from it transparently. Repeat the same query and confirm
  ;; the result is stable (and same as a freshly-constructed analysis).
  (let ((ga (make-graph-analysis (bigint-counting-semiring) test-adj #f)))
    (test 2 (graph-query ga "A" "D"))
    (test 2 (graph-query ga "A" "D"))     ; cached
    (test 1 (graph-query ga "A" "B"))))

(test-end)
(test-exit)
