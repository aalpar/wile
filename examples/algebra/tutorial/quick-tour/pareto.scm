;; quick-tour: (wile algebra pareto)
;;
;; Pareto dominance on multi-objective scores. X dominates Y if X is
;; at least as good as Y on every factor and strictly better on at
;; least one. The frontier is the set of non-dominated candidates.
;; Pick it up when you have candidates scored along several axes
;; and need "the set of points no other point beats on all axes."
;;
;; "Better" is per axis: a speed improves upward, a price downward.
;; Say which, or every axis is read as higher-is-better. See the last
;; section.

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

;; -- Factor direction: naming a lower-is-better axis ----------------
;;
;; Every axis above improves upward, so '(speed mpg) -- a plain list of
;; names -- is documentation and nothing more. Price does not improve
;; upward, and nothing about the number says so. Name the direction and
;; the alist becomes load-bearing; leave it unnamed and a cheaper car is
;; read as the worse one, with the frontier still looking like a frontier.

(define car-E '((speed . 120) (price . 20000)))
(define car-F '((speed . 100) (price . 35000)))

;; Read all-up, E and F are incomparable: F "wins" on price by costing more.
(check-false (dominates? car-E car-F)
             "without directions, E does not dominate F (price read upward)")

;; Name price as 'down and E dominates outright: faster AND cheaper.
(check-true  (dominates? car-E car-F '((speed . up) (price . down)))
             "with (price . down), E dominates F (faster and cheaper)")

;; Per-axis lookup. An axis the spec does not name defaults to 'up.
(check= 'down (factor-direction '((speed . up) (price . down)) 'price)
        "price improves downward")
(check= 'up   (factor-direction '((price . down)) 'speed)
        "an unnamed axis defaults to 'up")

;; Directions reach the frontier, not just dominates?.
(define priced-result
  (pareto-frontier `((E ,car-E) (F ,car-F)) '((speed . up) (price . down))))

(check= '(E) (cdr (assq 'frontier priced-result))
        "E alone is on the frontier once price is read downward")

;; Every way of getting a direction wrong raises, rather than quietly
;; meaning "higher is better" -- which is the failure the facility removes.

(check-error (lambda () (dominates? car-E car-F '((price . dwon))))
             "a misspelled direction is an error, not a silent 'up")
(check-error (lambda () (dominates? car-E car-F '((prise . down))))
             "a direction naming no factor is an error, not an unread entry")
(check-error (lambda () (dominates? car-E car-F '(speed (price . down))))
             "a spec half-migrated from names to pairs is an error")

(display "pareto tour complete") (newline)
