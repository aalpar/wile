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

(if (zero? fail-count)
    (begin (display "OK: ") (display test-count) (display " tests passed") (newline))
    (begin (display "FAIL: ") (display fail-count) (display "/") (display test-count) (newline)))
