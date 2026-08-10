;;; chibi-test-selftest.scm - does (chibi test) actually fail a failing test?
;;;
;;; The review's §8: "(chibi test) has no self-test, so a harness that silently
;;; passes everything looks identical to a passing suite." That is not a
;;; hypothetical -- %approx-equal? compared an EXACT expected value against an
;;; INEXACT actual through the relative-epsilon path, so (test 7 7.0),
;;; (test 1 1.0), (test 1/2 0.5) and (test 100 100.00000001) all recorded as
;;; PASSES with zero failures and exit 0.
;;;
;;; ASSERTED ON EXIT STATUS, NOT PRINTED OUTPUT. This is the whole point. A
;;; suite that prints "FAIL:" and exits 0 is indistinguishable from a passing
;;; suite to anything that greps output, which is exactly how a harness gap
;;; hides. Each fixture below runs in a SUBPROCESS and the assertion is on the
;;; integer (system) returns -- the same status test-exit computes from
;;; *test-fail-count*.
;;;
;;; WHAT THIS SUITE DOES NOT COVER:
;;;   - the non-numeric comparator paths: the equal? fallback, and the pair and
;;;     vector recursions. Only the numeric branch of %approx-equal? is exercised.
;;;   - explicitly-supplied comparators (test-equal with a compare argument),
;;;     which bypass %approx-equal? entirely and are honoured verbatim.
;;;   - test-error, test-assert, test-not and test-values accounting.

;; `system' is a global primitive from the process extension, not a library
;; export, so it needs no import set.
(import (scheme base)
        (scheme process-context)
        (chibi test))

(test-begin "chibi-test-selftest")

;; The binary under test. run-all.sh cd's to the repository root, so the
;; relative path resolves there; SCHEME overrides it for other harnesses. A
;; wrong path makes the control group below fail loudly (127) rather than
;; letting every assertion pass vacuously.
(define wile-binary
  (or (get-environment-variable "SCHEME") "./dist/wile"))

;; Run a one-line Scheme program in a subprocess and return its exit status.
;; Output is discarded deliberately: the assertion is the status.
;; The program text must not contain a single quote, since it is passed inside
;; a single-quoted shell word.
(define (fixture-status program)
  (system (string-append wile-binary " -q -e '" program "' >/dev/null 2>&1")))

(define (suite body)
  (string-append "(import (chibi test)) (test-begin \"fixture\") "
                 body
                 " (test-end) (test-exit)"))

;; ── Control: the harness can report success at all ───────────────
;;
;; Without this, "every fixture exits 1" would satisfy every assertion below,
;; including when wile-binary is wrong and the shell returns 127.

(test-group "a passing fixture exits 0"
  (test 0 (fixture-status (suite "(test 7 7)")))
  (test 0 (fixture-status (suite "(test 1.0 1.0)")))
  ;; Upstream allows an inexact EXPECTED value to accept an equivalent exact
  ;; actual -- the direction that stays approximate.
  (test 0 (fixture-status (suite "(test 1.0 1)")))
  ;; Within current-test-epsilon (1e-5), relative.
  (test 0 (fixture-status (suite "(test 1.0 1.000000001)"))))

;; ── The defect: an exact expectation must not accept an inexact actual ──
;;
;; Observed at 003b3353: every one of these exited 0 with "Passed: 4, Failed: 0".

(test-group "an exact expectation rejects an inexact actual"
  (test 1 (fixture-status (suite "(test 7 7.0)")))
  (test 1 (fixture-status (suite "(test 1 1.0)")))
  (test 1 (fixture-status (suite "(test 1/2 0.5)")))
  (test 1 (fixture-status (suite "(test 100 100.00000001)"))))

;; ── A genuinely wrong answer must still fail ─────────────────────
;;
;; Guards against "fix" by making the comparator reject everything.

(test-group "an unrelated value fails"
  (test 1 (fixture-status (suite "(test 1 2)")))
  (test 1 (fixture-status (suite "(test 1.0 2.0)"))))

;; ── The counter drives the status, so several failures still exit 1 ──

(test-group "mixed suites exit 1 when any test fails"
  (test 1 (fixture-status (suite "(test 1 1) (test 7 7.0)")))
  (test 0 (fixture-status (suite "(test 1 1) (test 2 2)"))))

(test-end)
(test-exit)
