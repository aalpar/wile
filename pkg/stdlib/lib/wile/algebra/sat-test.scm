;;; sat-test.scm — Scheme-level smoke tests for (wile algebra sat).
;;; Run via: wile --file stdlib/lib/wile/algebra/sat-test.scm

(import (scheme base)
        (wile algebra sat))

(define test-count 0)
(define fail-count 0)

(define (check label expected actual)
  (set! test-count (+ test-count 1))
  (unless (equal? expected actual)
    (set! fail-count (+ fail-count 1))
    (display "FAIL: ") (display label) (newline)
    (display "  expected: ") (write expected) (newline)
    (display "  actual:   ") (write actual) (newline)))

(check "cnf->flat trivial"
       #(1 -2 3 0 -1 4 0)
       (cnf->flat '((1 -2 3) (-1 4))))

(check "sat-cnf? SAT"
       #t
       (sat-cnf? '((1 2) (-1 2))))

(check "sat-cnf? UNSAT"
       #f
       (sat-cnf? '((1) (-1))))

(check "sat? trivial true"
       #t
       (sat? '#t))

(check "sat? trivial false"
       #f
       (sat? '#f))

(check "sat? single var"
       #t
       (sat? 'x))

(check "sat? conjunction (SAT)"
       #t
       (sat? '(and x y)))

(check "sat? contradiction (UNSAT)"
       #f
       (sat? '(and x (not x))))

(check "sat? De Morgan biconditional (tautology — SAT)"
       #t
       (sat? '(iff (not (and x y))
                   (or (not x) (not y)))))

(check "boolean-decide-equivalent? closes De Morgan"
       #t
       (boolean-decide-equivalent? '(not (and x y))
                                    '(or (not x) (not y))))

(check "boolean-decide-equivalent? closes complement law"
       #t
       (boolean-decide-equivalent? '(and x (not x)) '#f))

(check "boolean-decide-equivalent? closes bound identity"
       #t
       (boolean-decide-equivalent? '(or x #t) '#t))

(check "boolean-decide-equivalent? closes distributivity"
       #t
       (boolean-decide-equivalent? '(and x (or y z))
                                    '(or (and x y) (and x z))))

(check "boolean-decide-equivalent? rejects non-equivalent"
       #f
       (boolean-decide-equivalent? '(or x y) '(and x y)))

(check "boolean-decide-equivalent? commutativity (sanity)"
       #t
       (boolean-decide-equivalent? '(and a b) '(and b a)))

(check "boolean-decide-equivalent? absorption (sanity)"
       #t
       (boolean-decide-equivalent? '(or x (and x y)) 'x))

(check "boolean-decide-sat? wrapper"
       #t
       (boolean-decide-sat? '(or x y)))

(if (zero? fail-count)
    (begin (display "OK: ") (display test-count) (display " tests passed") (newline))
    (begin (display "FAIL: ") (display fail-count) (display "/") (display test-count) (newline)
           ;; Raise so the process exits non-zero — cover-scm.sh gates make
           ;; covercheck on the exit code, so a silent display alone would let
           ;; failures pass CI (the same gap (test-exit) closes for chibi-test
           ;; files; this file uses a custom `check` harness instead).
           (error "sat-test: assertions failed" fail-count test-count)))
