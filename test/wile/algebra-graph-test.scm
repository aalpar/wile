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
    ;; Absolute pins — anchors the comparison so a symmetric bug
    ;; producing the same wrong value on both paths still trips a test.
    (test 2 (graph-query fast "A" "D"))
    (test 1 (graph-query fast "A" "B"))
    (test 1 (graph-query fast "A" "A"))
    ;; Cross-path agreement.
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

(test-group "fast path — cyclic input dispatches to count-paths-cyclic"
  ;; The bigint carrier now handles cyclic input via SCC condensation
  ;; (sub-path 4C). The 3-cycle A→B→C→A is one non-trivial SCC; the
  ;; entry-count from any source in the SCC is 1 (only one entry: the
  ;; source itself, no other SCC reaches it). Every node reports 1.
  (let* ((ga (make-graph-analysis (bigint-counting-semiring) cyclic-adj #f))
         (r  (graph-query ga "A" "C")))
    (test 1 r)))

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

(test-group "fast path — non-atomic adjacency keys suppress attachment"
  ;; The fast path's name interning uses a hashtable, which Wile restricts
  ;; to atomic keys. Adjacency with a pair-keyed node falls back to the
  ;; slow path transparently — the carrier opt stays advisory.
  (let ((ga (make-graph-analysis (bigint-counting-semiring)
                                 '(((1 2) . (((3 4) . 1))) ((3 4) . ()))
                                 #f)))
    (test #f (graph-analysis-fast-path? ga))
    ;; Slow path still works on the same input — value verifies
    ;; semantic equivalence under fall-back.
    (test 1 (graph-query ga '(1 2) '(3 4))))
  ;; Adjacency mixing atomic and non-atomic keys: presence of either
  ;; non-atomic key disables the fast path. (Edge target #(0) is a
  ;; vector — also non-Hashable.)
  (let ((ga (make-graph-analysis (bigint-counting-semiring)
                                 (list (cons "A" (list (cons #(0) 1)))
                                       (cons #(0) '()))
                                 #f)))
    (test #f (graph-analysis-fast-path? ga))))

(test-group "fast path — source not in adjacency returns semiring-zero"
  ;; The fast path used to error on missing source; slow path returns
  ;; semiring-zero. Aligning them keeps the carrier opt's "advisory"
  ;; contract intact: same query, same result, regardless of carrier.
  (let ((fast (make-graph-analysis (bigint-counting-semiring) test-adj #f))
        (slow (make-graph-analysis (counting-semiring) test-adj #f)))
    (test (graph-query slow "MISSING" "A")
          (graph-query fast "MISSING" "A"))
    (test 0 (graph-query fast "MISSING" "A"))
    ;; graph-query-all returns an empty alist for a missing source.
    (test '() (graph-query-all fast "MISSING"))))

(test-group "worklist consults semiring-eq? not host equal?"
  ;; A cyclic adjacency forces compute-via-worklist (not topological-order).
  ;; A counter-incrementing custom :eq? proves the worklist dispatches
  ;; through the semiring's declared equality predicate.
  (let* ((adj '(("a" . (("b" . 1)))
                ("b" . (("a" . 1)))))                 ; 2-cycle
         (calls 0)
         (counting-eq? (lambda (a b)
                         (set! calls (+ calls 1))
                         ;; Boolean idempotent: #t = #t terminates the worklist
                         ;; well before any safety cap.
                         (eq? a b)))
         (S (make-semiring (lambda (a b) (or a b))
                           (lambda (a b) (and a b))
                           #f #t
                           (cons 'eq? counting-eq?)))
         (ga (make-graph-analysis S adj #f)))
    ;; Query terminates (boolean idempotent) and we expect counting-eq?
    ;; to have fired at least once.
    (test #t (graph-query ga "a" "b"))
    (test #t (> calls 0))))

(test-group "approximate counting variants — cycle tractability"
  ;; The headline payoff: modular and saturating terminate on cycles where
  ;; the exact counting-semiring would diverge (or, in our impl, hit the
  ;; 2·V·E safety cap with a 3-hour-style hang in real scale). Small cycle
  ;; here is enough to prove the algorithmic termination.
  (let ((adj '(("a" . (("b" . 1)))
               ("b" . (("c" . 1)))
               ("c" . (("a" . 1))))))
    ;; Modular: carrier is finite (Z/7Z), worklist converges within the
    ;; finite carrier's cycle length.
    (let ((ga (make-graph-analysis (modular-counting-semiring 7) adj #f)))
      ;; Walk-count from a to c is well-defined modulo 7 once the worklist
      ;; settles. Exact value depends on iteration order; verify it's
      ;; in-range (0..6) rather than asserting a specific value.
      (let ((r (graph-query ga "a" "c")))
        (test #t (and (integer? r) (>= r 0) (< r 7)))))
    ;; Saturating: carrier is bounded by cap, worklist converges once all
    ;; reachable nodes hit the cap.
    (let ((ga (make-graph-analysis (saturating-counting-semiring 100) adj #f)))
      (let ((r (graph-query ga "a" "c")))
        (test #t (and (integer? r) (>= r 0) (<= r 100)))))))

(test-group "log-counting-semiring — DAG ranking"
  ;; Log is bounded-precision but unbounded-magnitude. The docstring
  ;; explicitly notes it is NOT cycle-tractable. It IS useful for DAG
  ;; ranking workloads, which we verify here.
  ;; Diamond DAG: a -> {b, c} -> d. Two paths from a to d, each with
  ;; product 0 in log-space, sum via log-sum-exp = log(2).
  (let* ((adj '(("a" . (("b" . 1) ("c" . 1)))
                ("b" . (("d" . 1)))
                ("c" . (("d" . 1)))
                ("d" . ())))
         (ga (make-graph-analysis (log-counting-semiring) adj #f))
         (r  (graph-query ga "a" "d")))
    ;; Result should be ~log(2) to float precision.
    (test #t (< (abs (- r (log 2))) 1e-12))))

;;; ===== Sub-path 4C: cyclic-counting via SCC condensation =============

(test-group "cyclic counting — cycle + tail"
  ;; A→B→C→A is a 3-cycle; A→D is a tail edge. SCC0 = {A,B,C} (non-trivial);
  ;; SCC1 = {D} (trivial). From A, the cycle SCC's entry count is 1 (the
  ;; source) and D's count is 1 (one path from A's SCC to D).
  (let* ((adj '(("A" . (("B") ("D")))
                ("B" . (("C")))
                ("C" . (("A")))
                ("D" . ())))
         (ga  (make-graph-analysis (bigint-counting-semiring) adj #f)))
    (test 1 (graph-query ga "A" "A"))
    (test 1 (graph-query ga "A" "B"))
    (test 1 (graph-query ga "A" "C"))
    (test 1 (graph-query ga "A" "D"))))

(test-group "cyclic counting — bowtie (two cycles sharing a vertex)"
  ;; A→B→A and A→C→A. SCC = {A,B,C} (all reachable from each other).
  ;; Single non-trivial SCC; all nodes report the SCC entry-count = 1.
  (let* ((adj '(("A" . (("B") ("C")))
                ("B" . (("A")))
                ("C" . (("A")))))
         (ga  (make-graph-analysis (bigint-counting-semiring) adj #f)))
    (test 1 (graph-query ga "A" "A"))
    (test 1 (graph-query ga "A" "B"))
    (test 1 (graph-query ga "A" "C"))))

(test-group "cyclic counting — self-loop is a non-trivial SCC"
  ;; A single node with a self-loop. SCC0 = {A} but non-trivial (has the loop).
  (let* ((adj '(("A" . (("A")))))
         (ga  (make-graph-analysis (bigint-counting-semiring) adj #f)))
    (test 1 (graph-query ga "A" "A"))
    (test #t (graph-node-in-cycle? ga "A"))))

(test-group "cyclic counting — mutual recursion + parallel tails (mini call graph)"
  ;; f<->g forms a cycle. h is called only by f (one tail). i is called only by g.
  ;; SCC0 = {f, g} (non-trivial); SCC1 = {h}; SCC2 = {i}.
  ;; From f: cycle entry-count 1; h entry-count 1 (one edge f→h); i entry-count
  ;; 1 (one edge g→i, but g is in source's SCC so it's a single inter-SCC edge).
  (let* ((adj '(("f" . (("g") ("h")))
                ("g" . (("f") ("i")))
                ("h" . ())
                ("i" . ())))
         (ga  (make-graph-analysis (bigint-counting-semiring) adj #f)))
    (test 1 (graph-query ga "f" "f"))
    (test 1 (graph-query ga "f" "g"))
    (test 1 (graph-query ga "f" "h"))
    (test 1 (graph-query ga "f" "i"))))

(test-group "graph-node-in-cycle? on cycle + tail"
  (let* ((adj '(("A" . (("B") ("D")))
                ("B" . (("C")))
                ("C" . (("A")))
                ("D" . ())))
         (ga  (make-graph-analysis (bigint-counting-semiring) adj #f)))
    (test #t (graph-node-in-cycle? ga "A"))
    (test #t (graph-node-in-cycle? ga "B"))
    (test #t (graph-node-in-cycle? ga "C"))
    (test #f (graph-node-in-cycle? ga "D"))
    ;; Unknown node raises (not silently #f) — a typo would otherwise be
    ;; indistinguishable from a known-acyclic node, masking consumer bugs.
    (test-error (graph-node-in-cycle? ga "missing"))))

(test-group "graph-cyclic-nodes on cycle + tail"
  (let* ((adj '(("A" . (("B") ("D")))
                ("B" . (("C")))
                ("C" . (("A")))
                ("D" . ())))
         (ga  (make-graph-analysis (bigint-counting-semiring) adj #f))
         (cy  (graph-cyclic-nodes ga)))
    (test 3 (length cy))
    ;; order matches adjacency-insertion order: A, B, C (D is acyclic, filtered out)
    (test '("A" "B" "C") cy)))

(test-group "graph-cyclic-nodes on fully acyclic graph"
  (let* ((adj '(("A" . (("B"))) ("B" . (("C"))) ("C" . ())))
         (ga  (make-graph-analysis (bigint-counting-semiring) adj #f)))
    (test '() (graph-cyclic-nodes ga))))

(test-group "graph-analysis-sccs is idempotent (eq? same record)"
  (let* ((adj '(("A" . (("B"))) ("B" . (("A")))))
         (ga  (make-graph-analysis (bigint-counting-semiring) adj #f))
         (s1  (graph-analysis-sccs ga))
         (s2  (graph-analysis-sccs ga)))
    (test #t (eq? s1 s2))
    (test #t (graph-scc? s1))))

(test-group "SCC cache shared across cyclic queries with different sources"
  ;; Two queries against different sources must reuse the same <graph-scc>.
  (let* ((adj '(("A" . (("B") ("D")))
                ("B" . (("C")))
                ("C" . (("A")))
                ("D" . ())))
         (ga  (make-graph-analysis (bigint-counting-semiring) adj #f)))
    ;; Force initial cache population via a query.
    (graph-query ga "A" "A")
    (let ((s1 (graph-analysis-sccs ga)))
      ;; A second query for a different source must not invalidate or
      ;; rebuild the cache.
      (graph-query ga "B" "C")
      (let ((s2 (graph-analysis-sccs ga)))
        (test #t (eq? s1 s2))))))

(test-group "graph-query-all on cyclic graph returns plain alist (Q-2 side query)"
  ;; The alist shape is unchanged: bare (name . count) pairs. Callers
  ;; that want to distinguish ask graph-node-in-cycle? separately.
  (let* ((adj '(("A" . (("B") ("D")))
                ("B" . (("C")))
                ("C" . (("A")))
                ("D" . ())))
         (ga    (make-graph-analysis (bigint-counting-semiring) adj #f))
         (dists (graph-query-all ga "A")))
    (test 4 (length dists))
    (for-each
      (lambda (entry)
        (test #t (pair? entry))
        (test #t (string? (car entry)))
        (test #t (integer? (cdr entry))))
      dists)))

;;; ===== Crosscheck follow-up coverage ==================================

(test-group "cyclic counting — entry-count > 1 (multi-edge condensed DAG)"
  ;; Fixture stress-tests the SCC-condensation arithmetic by constructing
  ;; multiple parallel inter-SCC edges into a non-trivial SCC.
  ;;
  ;;   A → B, A → C, B → D, B → E, C → D, D ⇄ E
  ;;
  ;; SCCs: {A}, {B}, {C}, {D, E}. Condensed edges from A's SCC:
  ;;   {A} → {B} (1 edge), {A} → {C} (1 edge)
  ;; Condensed edges into {D, E}:
  ;;   {B} → {D, E} preserves B→D + B→E as 2 multi-edges
  ;;   {C} → {D, E} (1 edge: C→D)
  ;; Path count from A's SCC to {D, E} via the condensed DAG:
  ;;   {A}→{B}→{D,E}: 1 × 2 = 2
  ;;   {A}→{C}→{D,E}: 1 × 1 = 1
  ;;   total = 3
  ;; Both D and E (same non-trivial SCC) report the SCC entry count = 3.
  (let* ((adj '(("A" . (("B") ("C")))
                ("B" . (("D") ("E")))
                ("C" . (("D")))
                ("D" . (("E")))
                ("E" . (("D")))))
         (ga  (make-graph-analysis (bigint-counting-semiring) adj #f)))
    (test 1 (graph-query ga "A" "A"))
    (test 1 (graph-query ga "A" "B"))
    (test 1 (graph-query ga "A" "C"))
    (test 3 (graph-query ga "A" "D"))
    (test 3 (graph-query ga "A" "E"))))

(test-group "source not in cyclic adjacency returns semiring-zero"
  ;; Mirrors the DAG-path coverage at "fast path — source not in adjacency"
  ;; above; pins identical behavior on the cyclic dispatch path.
  (let* ((adj '(("A" . (("B")))
                ("B" . (("A")))))
         (ga  (make-graph-analysis (bigint-counting-semiring) adj #f)))
    ;; "ZZ" is not in the adjacency; graph-query-all returns '()
    ;; (no reachable targets), so graph-query surfaces semiring-zero.
    (test '() (graph-query-all ga "ZZ"))
    (test 0 (graph-query ga "ZZ" "A"))))

(test-group "empty adjacency — graph-cyclic-nodes and graph-node-in-cycle?"
  ;; Empty graph: %ensure-graph-scc! skips the kernel (count-paths-cyclic
  ;; requires source < num-nodes) and builds zero-length vectors.
  (let ((ga (make-graph-analysis (bigint-counting-semiring) '() #f)))
    (test '() (graph-cyclic-nodes ga))
    (test #t (graph-scc? (graph-analysis-sccs ga)))
    (test 0 (graph-scc-num-nodes (graph-analysis-sccs ga)))
    ;; Any node query raises (no nodes to look up)
    (test-error (graph-node-in-cycle? ga "anything"))))

(test-group "graph-analysis-sccs on non-bigint carrier works"
  ;; SCC is a structural property of the adjacency; the carrier of the
  ;; analysis doesn't affect it. Three different analyses on the same
  ;; cyclic adjacency report the same SCC structure.
  (let* ((adj '((a . ((b))) (b . ((a))) (c . ()))))
    (let* ((ga-bool (make-graph-analysis (boolean-semiring) adj #f))
           (s       (graph-analysis-sccs ga-bool)))
      (test #t (graph-scc? s))
      (test 3 (graph-scc-num-nodes s))
      ;; a and b are in a non-trivial SCC; c is not.
      (test #t (graph-node-in-cycle? ga-bool 'a))
      (test #t (graph-node-in-cycle? ga-bool 'b))
      (test #f (graph-node-in-cycle? ga-bool 'c)))
    (let ((ga-trop (make-graph-analysis (tropical-semiring) adj
                     (lambda (_) 1))))
      (test '(a b) (graph-cyclic-nodes ga-trop)))))

(test-group "SCC structure stable across kernel calls with different sources"
  ;; The cyclic-counting adapter discards the per-call SCC vector returned
  ;; by count-paths-cyclic and uses the cached one. This pins the
  ;; load-bearing determinism invariant: kernel SCC IDs must be the same
  ;; across calls regardless of source.
  (let* ((adj '(("A" . (("B") ("D")))
                ("B" . (("C")))
                ("C" . (("A")))
                ("D" . ())))
         (ga  (make-graph-analysis (bigint-counting-semiring) adj #f)))
    ;; Force population
    (let ((s (graph-analysis-sccs ga)))
      ;; Each query routes through compute-via-count-paths-cyclic which
      ;; re-invokes count-paths-cyclic for that source. The cached vector
      ;; would mis-index if kernel SCC numbering varied per call.
      (graph-query ga "A" "D")
      (graph-query ga "B" "C")
      (graph-query ga "C" "A")
      ;; After three queries with three different sources, the cached
      ;; scc-vec must still be the same object — eq? confirms no cache
      ;; invalidation, and the queries above would have crashed on
      ;; out-of-range counts-by-scc[scc[i]] if the invariant failed.
      (test #t (eq? s (graph-analysis-sccs ga))))))

(test-end)
(test-exit)
