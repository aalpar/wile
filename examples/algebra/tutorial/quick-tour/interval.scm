;; quick-tour: (wile algebra interval)
;;
;; Infinity-aware interval arithmetic. Intervals are (lo . hi) pairs where
;; endpoints may be numbers or the sentinels 'neg-inf / 'pos-inf. Reach for
;; this when you want a conservative envelope for uncertain quantities, or
;; a concrete abstract domain for static analysis.

(import (scheme base) (wile algebra interval) (wile algebra lattice))
(include "../lib/check.scm")

;; -- Infinity-aware comparisons ------------------------------------

(check-true  (inf<= 'neg-inf 100)            "neg-inf <= anything")
(check-true  (inf<= 100 'pos-inf)            "anything <= pos-inf")
(check-false (inf<= 'pos-inf 'neg-inf)       "pos-inf not <= neg-inf")
(check= (inf-min 3 'pos-inf)   3             "min(3, pos-inf) = 3")
(check= (inf-max 3 'neg-inf)   3             "max(3, neg-inf) = 3")

;; -- Infinity-aware arithmetic -------------------------------------

(check= (inf+ 'pos-inf 5)  'pos-inf          "pos-inf + 5 = pos-inf")
(check= (inf- 3 'neg-inf)  'pos-inf          "3 - (-inf) = pos-inf")
(check= (inf* 0 'pos-inf)  0                 "0 * pos-inf = 0 (absorbing)")

;; -- Interval arithmetic on finite intervals -----------------------

(define a '(1 . 3))       ; [1, 3]
(define b '(2 . 5))       ; [2, 5]

(check= (interval-add a b)  '(3 . 8)         "[1,3] + [2,5] = [3,8]")
(check= (interval-sub a b)  '(-4 . 1)        "[1,3] - [2,5] = [-4,1]")
(check= (interval-mul a b)  '(2 . 15)        "[1,3] * [2,5] = [2,15]")

;; Four-corner multiplication handles signs correctly.
(define neg '(-3 . -1))   ; [-3, -1]
(check= (interval-mul neg neg)  '(1 . 9)     "[-3,-1] * [-3,-1] = [1,9]")

;; -- The interval lattice ------------------------------------------

(define L (interval-lattice))
(check-true (lattice? L)                     "interval-lattice is a lattice")

;; Join widens; meet narrows.
(check= (lattice-join L '(1 . 3) '(2 . 5))  '(1 . 5)
        "join widens to [1,5]")
(check= (lattice-meet L '(1 . 3) '(2 . 5))  '(2 . 3)
        "meet narrows to [2,3]")

;; Meet of disjoint intervals is the bottom sentinel.
(check= (lattice-meet L '(1 . 3) '(5 . 7))  'interval-bot
        "disjoint meet = interval-bot")

;; Top is (neg-inf . pos-inf); bottom is 'interval-bot.
(check= (lattice-top L)    '(neg-inf . pos-inf)  "top is the universe")
(check= (lattice-bottom L) 'interval-bot         "bottom is the empty interval")

;; Containment is the lattice's leq.
(check-true  (lattice-leq? L '(2 . 3) '(1 . 5))
             "[2,3] contained in [1,5]")
(check-false (lattice-leq? L '(1 . 5) '(2 . 3))
             "[1,5] not contained in [2,3]")

(display "interval tour complete") (newline)
