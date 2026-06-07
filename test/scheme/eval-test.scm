;;; eval-test.scm - R7RS 6.12 Environments and evaluation: edge cases
;;;
;;; Edge cases and detailed coverage extracted from Go test suite
;;; (internal/extensions/eval/prim_eval_test.go).
;;; Complements the canonical R7RS tests in integration/testdata/r7rs-tests.scm.

(import (scheme base) (scheme eval) (chibi test))

(test-begin "eval")

;; ── eval ─────────────────────────────────────────────────────────

(test-group "eval simple expression"
  (test 3 (eval '(+ 1 2) (interaction-environment)))
  (test 6 (eval '(* 2 3) (interaction-environment)))
  (test '(1 2 3) (eval '(list 1 2 3) (interaction-environment))))

(test-group "eval variable reference"
  (define eval-test-var 42)
  (test 42 (eval 'eval-test-var (interaction-environment))))

(test-group "eval in null environment"
  ;; null-environment has no bindings, so + is unbound
  (test-error (eval '(+ 1 2) (null-environment 5))))

(test-group "eval errors"
  (test-error (eval '(+ 1 2) 42))        ; wrong environment type
  (test-error (eval '(let) (interaction-environment)))) ; expansion error

;; ── interaction-environment ──────────────────────────────────────

(test-group "interaction-environment"
  (test #t (not (eq? (interaction-environment) #f)))
  (test 3 (eval '(+ 1 2) (interaction-environment))))

;; ── scheme-report-environment ────────────────────────────────────

(test-group "scheme-report-environment"
  ;; R5RS and R7RS versions should return something
  (test #t (not (eq? (scheme-report-environment 5) #f)))
  (test #t (not (eq? (scheme-report-environment 7) #f))))

(test-group "scheme-report-environment errors"
  (test-error (scheme-report-environment 4))   ; unsupported version
  (test-error (scheme-report-environment "5"))) ; wrong type

;; ── null-environment ─────────────────────────────────────────────

(test-group "null-environment"
  (test #t (not (eq? (null-environment 7) #f)))
  ;; null environment has no bindings
  (test-error (eval '(+ 1 2) (null-environment 7))))

(test-group "null-environment errors"
  (test-error (null-environment 4))   ; unsupported version
  (test-error (null-environment "7"))) ; wrong type

;; ── environment ──────────────────────────────────────────────────

(test-group "environment"
  ;; empty environment has no bindings
  (test-error (eval '(+ 1 2) (environment)))
  ;; environment with library imports
  (test 3 (eval '(+ 1 2) (environment '(scheme base)))))

(test-group "environment errors"
  (test-error (environment "foo"))) ; wrong argument type

;; ── eval with library environments ───────────────────────────────

(test-group "eval with scheme base"
  (test 1024 (eval '(expt 2 10) (environment '(scheme base)))))

(test-group "eval with scheme cxr"
  (test 1 (eval '(caar '((1 2) 3)) (environment '(scheme cxr)))))

(test-group "eval with multiple libraries"
  (test #t (eval '(> (+ 1 (inexact (sin 0))) 0)
           (environment '(scheme base) '(scheme inexact)))))

(test-group "eval preserves multiple return values"
  (test '(1 2 3)
    (call-with-values
      (lambda () (eval '(values 1 2 3) (interaction-environment)))
      list))
  (test '(42)
    (call-with-values
      (lambda () (eval '(values 42) (interaction-environment)))
      list)))

(test-end)
(test-exit)
