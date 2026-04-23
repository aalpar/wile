;; quick-tour: (wile algebra graph)
;;
;; This is the *abstract* graph library -- distinct from (wile algebra
;; combinatorial-graph) which handles isomorphism, chromatic polynomial,
;; etc. (wile algebra graph) offers semiring-parameterized
;; single-source Bellman-Ford. Change the semiring to change the
;; analysis: boolean = reachability, tropical = shortest path, counting
;; = path count.

(import (scheme base) (wile algebra graph) (wile algebra semiring))
(include "../lib/check.scm")

;; -- A weighted graph: A -> B -> C, A -> C direct ------------------

(define adj
  '(("A" . (("B" . 1) ("C" . 5)))
    ("B" . (("C" . 2)))
    ("C" . ())))

;; -- Boolean semiring: reachability -------------------------------

(define ga-bool
  (make-graph-analysis (boolean-semiring) adj #f))  ; #f = unit weights

(check-true (graph-analysis? ga-bool)               "graph analysis built")
(check= (graph-query ga-bool "A" "C")   #t          "A reaches C (boolean)")
(check= (graph-query ga-bool "A" "X")   #f          "A does not reach X (unknown node)")

;; -- Tropical semiring: shortest path ------------------------------

(define ga-trop
  (make-graph-analysis (tropical-semiring) adj
                       (lambda (e) e)))             ; edge-data IS the weight

(check= (graph-query ga-trop "A" "A")   0           "distance A->A = 0")
(check= (graph-query ga-trop "A" "B")   1           "distance A->B = 1 (direct)")
(check= (graph-query ga-trop "A" "C")   3           "distance A->C = 3 (via B, not 5)")
(check= (graph-query ga-trop "B" "C")   2           "distance B->C = 2")

;; Distance to unreachable: tropical-inf.
(check= (graph-query ga-trop "C" "A")  tropical-inf
        "C does not reach A: tropical-inf")

;; -- All distances from a source ----------------------------------

(define all-from-A (graph-query-all ga-trop "A"))
;; Pin distances rather than only presence: A->A=0, A->B=1 (direct),
;; A->C=3 (via B, not the 5 direct).
(check= (cdr (assoc "A" all-from-A))  0   "distance to A from A is 0")
(check= (cdr (assoc "B" all-from-A))  1   "distance to B is 1")
(check= (cdr (assoc "C" all-from-A))  3   "distance to C is 3 (via B)")

(display "graph (abstract) tour complete") (newline)
