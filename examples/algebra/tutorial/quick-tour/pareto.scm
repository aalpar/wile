;; quick-tour: (wile algebra pareto)
;;
;; Pareto dominance on multi-objective scores. X dominates Y if X is
;; at least as good as Y on every factor and strictly better on at
;; least one. The frontier is the set of non-dominated candidates.
;; Pick it up when you have candidates scored along several axes
;; and need "the set of points no other point beats on all axes."

(import (scheme base) (wile algebra pareto))
(include "../lib/check.scm")

;; -- Factor comparison: bool < bool, number <= number -------------

(check-true  (factor-leq? #f #t)           "boolean: #f <= #t")
(check-false (factor-leq? #t #f)           "boolean: not (#t <= #f)")
(check-true  (factor-leq? 5 10)            "numeric: 5 <= 10")
(check-true  (factor-less? 3 5)            "numeric: 3 < 5")
(check-false (factor-less? 3 3)            "numeric: not 3 < 3 (strict)")

;; -- Pareto dominance on a single factor -------------------------

(check-true  (dominates? '((speed . 10)) '((speed . 5)))
             "10 mph dominates 5 mph")
(check-false (dominates? '((speed . 10)) '((speed . 10)))
             "10 mph does NOT dominate 10 mph (no strict improvement)")

;; -- Two-objective dominance -------------------------------------
;;
;; A car is better than another if it is at least as fast and at least
;; as fuel-efficient, and strictly better on at least one.

(define car-A '((speed . 120) (mpg . 30)))
(define car-B '((speed . 100) (mpg . 25)))   ; dominated by A
(define car-C '((speed .  80) (mpg . 50)))   ; trades off with A
(define car-D '((speed . 120) (mpg . 30)))   ; tied with A

(check-true  (dominates? car-A car-B)      "A dominates B")
(check-false (dominates? car-A car-C)      "A does not dominate C (C has more mpg)")
(check-false (dominates? car-C car-A)      "C does not dominate A (A has more speed)")
(check-false (dominates? car-A car-D)      "A does not dominate D (tie)")

;; -- Pareto frontier -----------------------------------------------

(define cars
  `((A ,car-A)
    (B ,car-B)
    (C ,car-C)
    (D ,car-D)))

(define result (pareto-frontier cars '(speed mpg)))

;; Result shape: ((frontier . (ids ...)) (dominated . (groups ...))).
(define frontier-ids (cdr (assq 'frontier result)))

(check-true (member 'A frontier-ids)       "A is on the frontier")
(check-true (member 'C frontier-ids)       "C is on the frontier (Pareto-optimal tradeoff)")
(check-true (member 'D frontier-ids)       "D is on the frontier (tied with A)")
(check-false (member 'B frontier-ids)      "B is NOT on the frontier (A dominates)")

(display "pareto tour complete") (newline)
