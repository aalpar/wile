;; quick-tour: (wile algebra matching)
;;
;; Two-sided matching -- Gale-Shapley stable marriage and its
;; relatives (Roth-Sotomayor 1990). Given two sides that rank each
;; other, find a pairing with no blocking pair: no two agents who both
;; prefer each other to their assigned partners. You reach for it for
;; assignment problems with preferences -- residency match, course
;; allocation, escrow-free swaps.

(import (scheme base)
        (wile algebra matching))
(include "../lib/check.scm")

;; -- Part 1: Preference profiles ----------------------------------
;;
;; A profile pairs an agent set with a ranks-of function mapping each
;; agent to its preference list (most-preferred first). Here three
;; proposers {a,b,c} and three receivers {x,y,z}, arranged so the two
;; sides' first choices conflict.

(define m-prefs '((a . (x y z)) (b . (y z x)) (c . (z x y))))
(define w-prefs '((x . (b c a)) (y . (c a b)) (z . (a b c))))

(define mp (make-preference-profile '(a b c) (lambda (m) (cdr (assq m m-prefs)))))
(define wp (make-preference-profile '(x y z) (lambda (w) (cdr (assq w w-prefs)))))

(check-true (preference-profile? mp)                       "mp is a preference profile")
(check= (preference-profile-rank-of mp 'a 'x) 1           "a ranks x first")
(check= (preference-profile-rank-of mp 'a 'z) 3           "a ranks z last")

;; -- Part 2: Gale-Shapley, both orientations ----------------------
;;
;; The deferred-acceptance algorithm is optimal for the *proposing*
;; side and pessimal for the other. Swapping who proposes yields a
;; different stable matching whenever preferences conflict.

(define M-prop (gale-shapley mp wp))                  ; proposers propose
(define M-recv (gale-shapley/receiver-optimal mp wp)) ; receivers propose

;; Proposer-optimal: every proposer gets their first choice.
(check= (bipartite-matching-partner M-prop 'a) 'x     "proposer-optimal: a-x")
(check= (bipartite-matching-partner M-prop 'b) 'y     "proposer-optimal: b-y")
(check= (bipartite-matching-partner M-prop 'c) 'z     "proposer-optimal: c-z")

;; Receiver-optimal: every receiver gets their first choice instead.
(check= (bipartite-matching-partner M-recv 'x) 'b     "receiver-optimal: x-b")
(check= (bipartite-matching-partner M-recv 'y) 'c     "receiver-optimal: y-c")
(check= (bipartite-matching-partner M-recv 'z) 'a     "receiver-optimal: z-a")

;; Both are stable; they differ because the two sides disagree.
(check-true  (stable? M-prop mp wp)                   "proposer-optimal is stable")
(check-true  (stable? M-recv mp wp)                   "receiver-optimal is stable")
(check-false (bipartite-matching-equal? M-prop M-recv)
             "the two optima are distinct matchings")

;; -- Part 3: Stability is the whole point -------------------------
;;
;; A matching is unstable iff some pair blocks it: both members prefer
;; each other to their current partners. blocking-pairs lists them
;; (empty iff stable).

(define unstable (make-bipartite-matching '((a . y) (b . x) (c . z))))
(check-false (stable? unstable mp wp)                 "the hand-built matching is unstable")
(check= (blocking-pairs unstable mp wp) '((b . z))    "b and z block it (both prefer each other)")

;; -- Part 4: Minimum-cost assignment (Hungarian) ------------------
;;
;; When you want to *minimize total cost* rather than satisfy
;; preferences, tropical-assignment runs Kuhn-Munkres. Here cost =
;; combined rank, so the optimum minimizes summed unhappiness.

(define result
  (tropical-assignment
    (lambda (p w) (+ (preference-profile-rank-of mp p w)
                     (preference-profile-rank-of wp w p)))
    '(a b c) '(x y z)))

(check= (bipartite-matching-partner (car result) 'a) 'x "min-cost assigns a-x")
(check= (cdr result) 12                                 "total combined rank is 12")

(display "matching tour complete") (newline)
