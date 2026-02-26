;;; macros-test.scm - R7RS 4.3 Macros
;;;
;;; Edge cases and detailed coverage extracted from Go test suites:
;;;   machine/compile_syntax_rules_test.go
;;;   machine/let_shadow_macro_test.go
;;;   machine/hygiene_test.go
;;; Complements the canonical R7RS tests in integration/testdata/r7rs-tests.scm.

(import (scheme base)
        (chibi test))

(test-begin "macros")

;; ── syntax-rules round-trip ──────────────────────────────────────
;; These tests define a macro, then immediately use it and check the result.

(test-group "syntax-rules round-trip"
  ;; identity macro
  (test 42
    (let ()
      (define-syntax m-id (syntax-rules () ((m-id x) x)))
      (m-id 42)))

  ;; rewrite with arithmetic
  (test 11
    (let ()
      (define-syntax m-inc (syntax-rules () ((m-inc x) (+ x 1))))
      (m-inc 10)))

  ;; ellipsis capture
  (test '(1 2 3)
    (let ()
      (define-syntax m-list (syntax-rules () ((m-list x ...) (list x ...))))
      (m-list 1 2 3)))

  ;; literals match
  (test 42
    (let ()
      (define-syntax m-lit
        (syntax-rules (lit)
          ((m-lit lit x) x)
          ((m-lit other x) (+ x 1))))
      (m-lit lit 42)))

  ;; literals non-match (second clause selected)
  (test 11
    (let ()
      (define-syntax m-lit2
        (syntax-rules (lit)
          ((m-lit2 lit x) x)
          ((m-lit2 other x) (+ x 1))))
      (m-lit2 something 10)))

  ;; multi-clause dispatch: zero args
  (test 0
    (let ()
      (define-syntax m-multi
        (syntax-rules ()
          ((m-multi) 0)
          ((m-multi x) x)
          ((m-multi x y) (+ x y))))
      (m-multi)))

  ;; multi-clause dispatch: one arg
  (test 7
    (let ()
      (define-syntax m-multi2
        (syntax-rules ()
          ((m-multi2) 0)
          ((m-multi2 x) x)
          ((m-multi2 x y) (+ x y))))
      (m-multi2 7)))

  ;; multi-clause dispatch: two args
  (test 30
    (let ()
      (define-syntax m-multi3
        (syntax-rules ()
          ((m-multi3) 0)
          ((m-multi3 x) x)
          ((m-multi3 x y) (+ x y))))
      (m-multi3 10 20)))

  ;; custom ellipsis
  (test '(1 2)
    (let ()
      (define-syntax m-cust (syntax-rules ::: () ((m-cust x :::) (list x :::))))
      (m-cust 1 2)))

  ;; nested pattern
  (test 7
    (let ()
      (define-syntax m-nest (syntax-rules () ((m-nest (a b)) (+ a b))))
      (m-nest (3 4))))

  ;; ellipsis in literals disables ellipsis
  (test 42
    (let ()
      (define-syntax m-elli-lit (syntax-rules ... () ((m-elli-lit x) x)))
      (m-elli-lit 42))))

;; ── let shadows macro ────────────────────────────────────────────
;; R7RS 4.2.2: let bindings shadow outer bindings including macros.

(test-group "let shadows macro"
  ;; let shadows 'and' macro - use as value
  (test 100 (let ((and 100)) and))

  ;; let shadows 'or' macro - use as value
  (test 200 (let ((or 200)) or))

  ;; nested let shadows macro
  (test 2 (let ((and 2)) (let ((x and)) x)))

  ;; lambda parameter shadows 'and' macro
  (test 123 ((lambda (and) and) 123))

  ;; lambda parameter shadows 'or' macro
  (test 51 ((lambda (or) (+ or 1)) 50))

  ;; shadowed macro name can be used in arithmetic
  (test 8 (let ((and 5)) (+ and 3)))

  ;; macro still works when not shadowed - and
  (test #t (let ((x 1)) (and #t #t)))

  ;; macro still works when not shadowed - or
  (test #t (let ((x 1)) (or #f #t)))

  ;; shadow in inner let only
  (test 11 (let ((x (and #t 10)))
             (let ((and 1))
               (+ x and)))))

;; ── hygiene preserved with shadowing ─────────────────────────────
;; Verify that the let-shadows-macro feature does not break hygiene.

(test-group "hygiene preserved with shadowing"
  ;; and macro still works correctly
  (test 42 (and #t #t 42))

  ;; or macro still works correctly
  (test 99 (or #f #f 99))

  ;; shadowed and doesn't affect outer scope
  (test 10
    (let ((result (and #t 10)))
      (let ((and 1))
        result)))

  ;; let macro expands correctly with shadowed variable
  (test 5
    (let ((x 5))
      (let ((and x))
        and))))

(test-end)
