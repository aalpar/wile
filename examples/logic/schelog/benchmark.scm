;;; benchmark.scm - Schelog integration benchmark for Wile
;;;
;;; Runs the Zebra puzzle (a classic logic programming benchmark) and
;;; basic schelog operations. Designed for single-process execution to
;;; enable accurate time and memory measurement.
;;;
;;; Usage (from project root):
;;;   ./dist/scheme -q -i -f examples/logic/schelog/schelog.scm \
;;;                    -i -f examples/logic/schelog/toys.scm \
;;;                    -i -f examples/logic/schelog/puzzle.scm \
;;;                    -i -f examples/logic/schelog/houses.scm \
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
;; houses.scm - Zebra puzzle (main benchmark)
;; ---------------------------------------------------------------------------
(display "\n--- Zebra Puzzle (houses.scm) ---\n")
(display "Solving the Einstein/Zebra puzzle...\n")

;; Enable occurs check for this puzzle (required for correct unification)
(set! *schelog-use-occurs-check?* #t)

(let ((result (solve-puzzle %houses)))
  (test-not-false "zebra puzzle has solution" result)
  (when result
    ;; Solution structure: ((solution= ((japan owns the zebra) (norway drinks water))))
    (let ((solution (car (cdr (car result)))))
      (test "zebra puzzle: japan owns zebra"
            '(japan owns the zebra)
            (car solution))
      (test "zebra puzzle: norway drinks water"
            '(norway drinks water)
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
