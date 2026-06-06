;; quick-tour: (wile algebra sat)
;;
;; A CDCL SAT solver -- decides whether a Boolean formula can be made
;; true. It answers #t / #f / 'unknown (the last only when a conflict
;; budget is exhausted). You reach for it when a question reduces to
;; "is this constraint set satisfiable?": Boolean-formula equivalence,
;; theory closure, or any NP-shaped search you can encode in clauses.

(import (scheme base)
        (wile algebra sat))
(include "../lib/check.scm")

;; -- Part 1: CNF satisfiability ------------------------------------
;;
;; A CNF is a list of clauses; each clause is a list of nonzero
;; integers. A positive literal n means variable n, a negative -n its
;; negation. The clause is a disjunction, the formula a conjunction.
;;
;;   (x1 v x2) /\ (~x1 v x3) /\ (~x2 v ~x3)

(define cnf '((1 2) (-1 3) (-2 -3)))

(check-true (sat-cnf? cnf)              "the 3-clause CNF is satisfiable")

;; After a satisfiable check, the witness is available as a vector
;; indexed 1..N (index 0 is unused). Here: x1=#t, x2=#f, x3=#t.

(check= (sat-cnf-model) #(#f #t #f #t) "model assigns x1=#t x2=#f x3=#t")

;; A direct contradiction is unsatisfiable.

(check-false (sat-cnf? '((1) (-1)))     "(x1) /\\ (~x1) is unsatisfiable")

;; -- Part 2: S-expression formulas --------------------------------
;;
;; sat? takes a Boolean S-expression over and/or/not/xor/iff/=> with
;; symbols as variables -- no manual CNF encoding required.

(check-true  (sat? '(and x (or y (not x)))) "formula is satisfiable")
(check-false (sat? '(and x (not x)))        "x /\\ ~x is unsatisfiable")

;; The most recent S-expression model is an alist of var -> boolean.

(sat? '(and x (or y (not x))))
(check= (sat-model) '((y . #t) (x . #t))    "witness sets x and y true")

;; -- Part 3: SAT-backed Boolean reasoning -------------------------
;;
;; boolean-decide-equivalent? decides A == B by asking whether
;; ~(A <-> B) is unsatisfiable. This closes the De Morgan / complement
;; / distributivity gaps that the purely-axiomatic
;; symbolic-boolean-equivalent? (in (wile algebra symbolic)) cannot.

(check-true  (boolean-decide-equivalent? '(not (and x y))
                                         '(or (not x) (not y)))
             "De Morgan: ~(x/\\y) == ~x \\/ ~y")

(check-false (boolean-decide-equivalent? '(or x y) '(and x y))
             "x \\/ y is not equivalent to x /\\ y")

(check-false (boolean-decide-sat? '(and x (not x)))
             "boolean-decide-sat? agrees: x /\\ ~x has no model")

(display "sat tour complete") (newline)
