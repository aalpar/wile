;;; run-all-tests.scm - Comprehensive Schelog validation suite for Wile
;;;
;;; This file tests all schelog examples to validate Wile's compatibility
;;; with the Schelog logic programming library.
;;;
;;; Usage:
;;;   cd <wile-root>
;;;   ./dist/wile -f examples/logic/schelog/run-all-tests.scm
;;;
;;; Expected output: All tests should pass with no errors.

;; Load schelog and all example files
(include "examples/logic/schelog/schelog.scm")
(include "examples/logic/schelog/toys.scm")
(include "examples/logic/schelog/puzzle.scm")
(include "examples/logic/schelog/mapcol.scm")
(include "examples/logic/schelog/england.scm")
(include "examples/logic/schelog/bible.scm")
(include "examples/logic/schelog/games.scm")
(include "examples/logic/schelog/holland.scm")

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

(display "=== Schelog Validation Suite for Wile ===\n\n")

;; ---------------------------------------------------------------------------
;; toys.scm tests
;; ---------------------------------------------------------------------------
(display "--- toys.scm ---\n")

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

(test "%fact 0"
      '((n 1))
      (%which (n) (%fact 0 n)))

;; ---------------------------------------------------------------------------
;; holland.scm tests
;; ---------------------------------------------------------------------------
(display "\n--- holland.scm ---\n")

(test-not-false "%city amsterdam" (%which () (%city 'amsterdam)))
(test-not-false "%city brussels" (%which () (%city 'brussels)))
(test "%country amsterdam" #f (%which () (%country 'amsterdam)))
(test-not-false "%country holland" (%which () (%country 'holland)))

;; ---------------------------------------------------------------------------
;; england.scm tests
;; ---------------------------------------------------------------------------
(display "\n--- england.scm ---\n")

(test-not-false "%male philip" (%which () (%male 'philip)))
(test-not-false "%female elizabeth" (%which () (%female 'elizabeth)))
(test-not-false "%father-of philip charles" (%which () (%father-of 'philip 'charles)))
(test-not-false "%mother-of elizabeth charles" (%which () (%mother-of 'elizabeth 'charles)))

;; Count Philip's children
(let ((count 0))
  (let loop ((result (%which (c) (%father-of 'philip c))))
    (when result
      (set! count (+ count 1))
      (loop (%more))))
  (test "Philip has 4 children" 4 count))

;; ---------------------------------------------------------------------------
;; bible.scm tests
;; ---------------------------------------------------------------------------
(display "\n--- bible.scm ---\n")

(let ((result (terachs-kids-test)))
  (test-not-false "terachs-kids-test returns result" result)
  (when result
    (let ((kids (cdr (car result))))
      (test "Terach has 3 kids" 3 (length kids)))))

(let ((result (terachs-kids-test-2)))
  (test-not-false "terachs-kids-test-2 (with %set-of)" result))

;; ---------------------------------------------------------------------------
;; mapcol.scm tests
;; ---------------------------------------------------------------------------
(display "\n--- mapcol.scm ---\n")

(test-not-false "test map coloring" (%which (M) (%test-color 'test M)))
(test-not-false "western-europe map coloring" (%which (M) (%test-color 'western-europe M)))

;; ---------------------------------------------------------------------------
;; games.scm tests
;; ---------------------------------------------------------------------------
(display "\n--- games.scm ---\n")

(let ((result (solve-puzzle %games)))
  (test-not-false "games puzzle has solution" result)
  (when result
    (let ((solution (schelog:deref* (cdr (car result)))))
      (test "games puzzle answer 1"
            '(michael is the australian)
            (schelog:deref* (car solution)))
      (test "games puzzle answer 2"
            '(richard plays tennis)
            (schelog:deref* (cadr solution))))))

;; ---------------------------------------------------------------------------
;; Summary
;; ---------------------------------------------------------------------------
(newline)
(display "=== Test Summary ===\n")
(display "Passed: ")
(display tests-passed)
(newline)
(display "Failed: ")
(display tests-failed)
(newline)

(if (= tests-failed 0)
    (display "\nAll tests passed! Wile is fully compatible with Schelog.\n")
    (begin
      (display "\nSome tests failed. See above for details.\n")
      (exit 1)))
