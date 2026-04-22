;;; algebra-unification-test.scm — AC-matching and AC-unification tests

(import (scheme base)
        (chibi test)
        (wile algebra unification))

(test-begin "unification")

(test-group "pattern-var construction and identity"
  (let ((vx (make-pattern-var 'x)))
    (test #t (pattern-var? vx))
    (test 'x (pattern-var-name vx))
    (test #f (pattern-var? 'x))               ; symbol, not pattern-var
    (test #f (pattern-var? '(+ 1 2)))))       ; pair, not pattern-var

(test-end "unification")
