;;; benchmark.scm - Schelog integration benchmark for Wile
;;;
;;; Runs fast schelog operations: basic predicates, map coloring, and the
;;; games puzzle. All tests complete in seconds. For the zebra puzzle
;;; stress test, see stress-test.scm.
;;;
;;; Usage (from project root):
;;;   ./dist/wile -q -i -f examples/logic/schelog/schelog.scm \
;;;                    -i -f examples/logic/schelog/toys.scm \
;;;                    -i -f examples/logic/schelog/puzzle.scm \
;;;                    -i -f examples/logic/schelog/mapcol.scm \
;;;                    -i -f examples/logic/schelog/games.scm \
;;;                    -f examples/logic/schelog/benchmark.scm

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
        (set! tests-passed (+ tests-passed 1))
        (display "  PASS: ")
        (display name)
        (newline))
      (begin
        (set! tests-failed (+ tests-failed 1))
        (display "  FAIL: ")
        (display name)
        (display " - expected ")
        (display expected)
        (display ", got ")
        (display actual)
        (newline))))

(define (test-not-false name result)
  (if result
      (begin
        (set! tests-passed (+ tests-passed 1))
        (display "  PASS: ")
        (display name)
        (newline))
      (begin
        (set! tests-failed (+ tests-failed 1))
        (display "  FAIL: ")
        (display name)
        (display " - expected truthy result, got #f")
        (newline))))

(display "=== Schelog Integration Benchmark ===\n\n")

;; ---------------------------------------------------------------------------
;; toys.scm - Basic schelog operations
;; ---------------------------------------------------------------------------
(display "--- Basic Operations (toys.scm) ---\n")

(test "%length '(a b c)"
      '((n 3))
      (%which (n) (%length '(a b c) n)))

(test "%append '(1 2) '(3 4)"
      '((z (1 2 3 4)))
      (%which (z) (%append '(1 2) '(3 4) z)))

(test "%reverse '(a b c d)"
      '((y (d c b a)))
      (%which (y) (%reverse '(a b c d) y)))

(test "%fact 5"
      '((n 120))
      (%which (n) (%fact 5 n)))

(test "%fact 10"
      '((n 3628800))
      (%which (n) (%fact 10 n)))

;; ---------------------------------------------------------------------------
;; mapcol.scm - Map coloring
;; ---------------------------------------------------------------------------
(display "\n--- Map Coloring (mapcol.scm) ---\n")

(test-not-false "test map coloring" (%which (M) (%test-color 'test M)))
(test-not-false "western-europe map coloring" (%which (M) (%test-color 'western-europe M)))

;; ---------------------------------------------------------------------------
;; games.scm - Logic puzzle
;; ---------------------------------------------------------------------------
(display "\n--- Logic Puzzle (games.scm) ---\n")

(let ((result (solve-puzzle %games)))
  (test-not-false "games puzzle has solution" result)
  (when result
    (let ((solution (cadr (car result))))
      (test "games puzzle: michael is australian"
            '(michael is the australian)
            (car solution))
      (test "games puzzle: richard plays tennis"
            '(richard plays tennis)
            (cadr solution)))))

;; ---------------------------------------------------------------------------
;; Summary
;; ---------------------------------------------------------------------------
(newline)
(display "=== Benchmark Summary ===\n")
(display "Passed: ")
(display tests-passed)
(newline)
(display "Failed: ")
(display tests-failed)
(newline)

(if (= tests-failed 0)
    (display "\nAll tests passed!\n")
    (begin
      (display "\nSome tests failed.\n")
      (exit 1)))
