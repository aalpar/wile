;; quick-tour: (wile algebra cfl)
;;
;; Context-free-language reachability: a path from s to t "counts" only when
;; its edge-label string is balanced under a context-free grammar. This is
;; how interprocedural analysis stays precise -- a call must return to its
;; own call site, not someone else's. You reach for it when plain (Boolean)
;; reachability over-approximates because it ignores call/return or field
;; open/close matching.

(import (scheme base) (wile algebra cfl))
(include "../lib/check.scm")

;; -- Two call sites into one procedure --
(define g
  (make-cfl-graph '(a1 a2 b1 b2 p)
    '((a1 call1 p) (p return1 a2)
      (b1 call2 p) (p return2 b2))))
(define sol (cfl-solve (dyck-grammar '((call1 . return1) (call2 . return2))) g))

;; -- Matched call/return is reachable --
(check-true  (cfl-reachable? sol 'a1 'a2) "a1 -> a2: call1 matched by return1")
(check-true  (cfl-reachable? sol 'b1 'b2) "b1 -> b2: call2 matched by return2")

;; -- Mismatched is NOT (this is the precision plain reachability lacks) --
(check-false (cfl-reachable? sol 'a1 'b2) "a1 -> b2: call1 / return2 do not balance")

;; -- General grammars too: balanced brackets on a line graph --
(define gn (make-cfl-graph '(x0 x1 x2 x3 x4)
              '((x0 open x1) (x1 open x2) (x2 close x3) (x3 close x4))))
(define soln (cfl-solve (dyck-grammar '((open . close))) gn))
(check-true  (cfl-reachable? soln 'x0 'x4) "[[ ]] balances")
(check-false (cfl-reachable? soln 'x0 'x3) "[[ ] does not")

(display "cfl tour complete") (newline)
