;;; run-all.scm - Discover and run all Scheme-level unit tests
;;;
;;; Usage:
;;;   ./test/run-all.scm
;;;   scheme -f test/run-all.scm
;;;
;;; Discovers all *-test.scm files and executes them.

(import (scheme base)
        (scheme write)
        (scheme process-context)
        (chibi test))

;; Test discovery: hardcoded list for now
;; Future enhancement: Use directory traversal when available
(define test-files
  '(;; Core language tests
    ;; "test/scheme/numeric-tower-test.scm"
    ;; "test/scheme/hygiene-test.scm"

    ;; Regression tests
    ;; "test/regression/issue-123-example.scm"

    ;; Library tests (example paths)
    ;; "lib/srfi/1/test/fold-test.scm"
    ))

(define (run-test-file path)
  (display "▶ Running ")
  (display path)
  (newline)
  (load path))

(define (main)
  (test-begin "Wile Scheme Test Suite")

  (cond
   ((null? test-files)
    (display "No test files registered yet.\n")
    (display "Add test files to the test-files list in test/run-all.scm\n"))
   (else
    (for-each run-test-file test-files)))

  (test-end)
  (test-exit))

(main)
