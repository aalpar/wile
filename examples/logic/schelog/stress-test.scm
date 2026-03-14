;;; stress-test.scm - Zebra puzzle stress test
;;;
;;; This is a stress test, not a unit test. The zebra puzzle is a brute-force
;;; constraint satisfaction problem that exercises heavy backtracking with
;;; occurs-check enabled. It takes significant time in an interpreted Scheme.
;;;
;;; Dedicated Prolog implementations use constraint propagation and indexing
;;; to prune the search space; Schelog's pure backtracking approach does not.
;;;
;;; Usage (from project root):
;;;   ./dist/wile -f examples/logic/schelog/stress-test.scm

(include "examples/logic/schelog/schelog.scm")
(include "examples/logic/schelog/puzzle.scm")
(include "examples/logic/schelog/houses.scm")

(display "=== Zebra Puzzle Stress Test ===\n\n")
(display "Solving the Einstein/Zebra puzzle (this takes significant time)...\n")

;; Enable occurs check (required for correct unification in this puzzle)
(set! *schelog-use-occurs-check?* #t)

(let ((result (solve-puzzle %houses)))
  (if (not result)
      (begin
        (display "FAIL: zebra puzzle returned no solution\n")
        (exit 1))
      (let ((solution (schelog:deref* (cadr (car result)))))
        (let ((answer1 (schelog:deref* (car solution)))
              (answer2 (schelog:deref* (cadr solution))))
          (display "Solution: ")
          (display solution)
          (newline)
          (newline)

          (if (and (equal? answer1 '(japan owns the zebra))
                   (equal? answer2 '(norway drinks water)))
              (display "PASS: Zebra puzzle solution is correct.\n")
              (begin
                (display "FAIL: unexpected solution\n")
                (display "  Expected: ((japan owns the zebra) (norway drinks water))\n")
                (display "  Got: ")
                (display (list answer1 answer2))
                (newline)
                (exit 1)))))))
