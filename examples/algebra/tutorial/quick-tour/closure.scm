;; quick-tour: (wile algebra closure)
;;
;; A closure operator on a lattice is a map cl : L -> L that is
;; extensive (a <= cl(a)), monotone, and idempotent (cl(cl(a)) = cl(a)).
;; The fixed points are called "closed elements." Reach for this when
;; you need the smallest X containing Y under some rule -- topological
;; closure, Galois closure, transitive closure, etc.

(import (scheme base)
        (srfi 132)                ; sort (not in scheme base)
        (wile algebra closure) (wile algebra lattice)
        (wile algebra order))
(include "../lib/check.scm")

;; -- Downward closure on {1..5} under <= ----------------------------

(define le-po (make-partial-order <=))
(define down (downward-closure-operator le-po '(1 2 3 4 5)))

(check-true (closure-operator? down)                   "downward-closure is a closure operator")

;; Adding everything <= 3: cl({3}) = {1, 2, 3}.
(check= (sort < (closure-close down '(3)))  '(1 2 3)   "cl({3}) = {1,2,3}")

;; A fixed point: cl({1, 2, 3}) is already {1, 2, 3}.
(check-true (closure-closed? down '(1 2 3))            "{1,2,3} is closed")
(check-false (closure-closed? down '(3 5))             "{3,5} is not closed (missing 1,2,4)")

;; -- Extracting closed elements -------------------------------------

(define samples '(() (1) (2) (3) (1 2) (1 2 3) (1 2 3 4 5)))
(define closed (closed-elements down samples))
(check-true (member '(1 2 3) closed)                   "{1,2,3} is in closed-elements")
(check-false (member '(3) closed)                      "{3} is NOT closed (would need 1,2)")

;; -- The lattice of closed elements ---------------------------------

(define closed-L (closure->closed-lattice down samples))
(check-true (lattice? closed-L)                        "closed elements form a lattice")

;; -- Validation -----------------------------------------------------

(check= (validate-closure-operator down samples)  #t
        "downward closure satisfies closure laws")

(display "closure tour complete") (newline)
