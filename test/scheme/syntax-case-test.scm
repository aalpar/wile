;;; syntax-case-test.scm - R6RS §12.4 syntax-case and syntax, §12.8 with-syntax
;;; and quasisyntax
;;;
;;; Runs under both syntax layers (WILE_SYNTAX_FORMS=go|scheme) and both drivers.
;;; A guard: every row here passes on the Go layer at HEAD, so a failure is a
;;; Scheme-layer regression. The shapes the Go layer fails — §5.3 and §5.4 of the
;;; design, and the two matcher gaps (`x ... ...`, and a with-syntax body
;;; template reading an outer pattern variable) — and the diagnostics, for the
;;; reason stated above the test-end below, live in
;;; pkg/wile/syntax_forms_switch_test.go until P3.

(import (scheme base)
        (chibi test))

(test-begin "syntax-case")

(test-group "clauses and patterns"
  (define-syntax sc-swap
    (lambda (stx)
      (syntax-case stx ()
        ((_ a b) #'(list b a)))))
  (test '(2 1) (sc-swap 1 2))

  (define-syntax sc-arity
    (lambda (stx)
      (syntax-case stx ()
        ((_) #''zero)
        ((_ x) #''one)
        ((_ x y) #''two)
        ((_ x . rest) #''many))))
  (test '(zero one two many) (list (sc-arity) (sc-arity 1) (sc-arity 1 2) (sc-arity 1 2 3)))

  (define-syntax sc-lit
    (lambda (stx)
      (syntax-case stx (=> else)
        ((_ => x) #'(list 'arrow x))
        ((_ else x) #'(list 'else x))
        ((_ y x) #'(list 'other x)))))
  (test '((arrow 1) (else 2) (other 3)) (list (sc-lit => 1) (sc-lit else 2) (sc-lit foo 3)))

  ;; a use-site local shadows a literal (R6RS §12.4, design §3.1)
  (test '(else other) (list (car (sc-lit else 1)) (let ((else 1)) (car (sc-lit else 2)))))

  (define-syntax sc-fender
    (lambda (stx)
      (syntax-case stx ()
        ((_ x) (number? (syntax->datum #'x)) #''number)
        ((_ x) #''not-number))))
  (test '(number not-number) (list (sc-fender 1) (sc-fender a)))

  (define-syntax sc-datum
    (lambda (stx)
      (syntax-case stx ()
        ((_ 1) #''one)
        ((_ "s") #''string)
        ((_ #t) #''true)
        ((_ x) #''other))))
  (test '(one string true other) (list (sc-datum 1) (sc-datum "s") (sc-datum #t) (sc-datum 2)))

  (define-syntax sc-vec
    (lambda (stx)
      (syntax-case stx ()
        ((_ #(a b c)) #'(list a b c))
        ((_ #(a ...)) #'(list a ...))
        ((_ x) #''no))))
  (test '((1 2 3) (1 2) no) (list (sc-vec #(1 2 3)) (sc-vec #(1 2)) (sc-vec 1)))

  (define-syntax sc-wild
    (lambda (stx)
      (syntax-case stx ()
        ((_ _ x _) #'x))))
  (test 2 (sc-wild 1 2 3))

  (define-syntax sc-dotted
    (lambda (stx)
      (syntax-case stx ()
        ((_ (a . b)) #'(list a 'b)))))
  ;; b is a pattern variable, so it substitutes inside the quote too
  (test '(1 2) (sc-dotted (1 . 2))))

(test-group "ellipsis"
  (define-syntax sc-rev
    (lambda (stx)
      (syntax-case stx ()
        ((_ x ...) #'(list x ...)))))
  (test '(1 2 3) (sc-rev 1 2 3))
  (test '() (sc-rev))

  (define-syntax sc-tail
    (lambda (stx)
      (syntax-case stx ()
        ((_ x ... y) #'(list (list x ...) y)))))
  (test '((1 2) 3) (sc-tail 1 2 3))
  (test '(() 3) (sc-tail 3))

  (define-syntax sc-pairs
    (lambda (stx)
      (syntax-case stx ()
        ((_ (k v) ...) #'(list (cons 'k v) ...)))))
  (test '((a . 1) (b . 2)) (sc-pairs (a 1) (b 2)))

  (define-syntax sc-nested
    (lambda (stx)
      (syntax-case stx ()
        ((_ (x ...) ...) #'(list (list x ...) ...)))))
  (test '((1 2) (3) ()) (sc-nested (1 2) (3) ()))

  ;; depth-0 variable broadcast into an ellipsis subtemplate (R7RS §4.3.2)
  (define-syntax sc-broadcast
    (lambda (stx)
      (syntax-case stx ()
        ((_ k x ...) #'(list (cons k x) ...)))))
  (test '((0 . 1) (0 . 2)) (sc-broadcast 0 1 2))

  ;; (... ...) escape
  (define-syntax sc-escape
    (lambda (stx)
      (syntax-case stx ()
        ((_ x) #'(quote (x (... ...)))))))
  (test '(1 ...) (sc-escape 1))

  ;; ellipsis followed by a dotted tail
  (define-syntax sc-dotted-tail
    (lambda (stx)
      (syntax-case stx ()
        ((_ x ... . r) #'(list (list x ...) 'r)))))
  ;; r is a pattern variable bound to the (empty) tail, quote included
  (test '((1 2) ()) (sc-dotted-tail 1 2)))

(test-group "hygiene through the Scheme forms"
  (define-syntax sc-my-or
    (lambda (stx)
      (syntax-case stx ()
        ((_ a b) #'(let ((tmp a)) (if tmp tmp b))))))
  (test 5 (let ((tmp 5)) (sc-my-or #f tmp)))

  (define-syntax sc-inner
    (lambda (stx)
      (syntax-case stx ()
        ((_ e) #'(let ((if list)) e)))))
  (test 1 (sc-inner (if #t 1 2)))

  ;; nested syntax-case: an inner clause's pattern variable does not leak out
  ;; (a lone-identifier inner pattern is §5.3, Go-layer-only-failing: see the
  ;; Go pins)
  (define-syntax sc-nest
    (lambda (stx)
      (syntax-case stx ()
        ((_ x)
         (syntax-case #'x ()
           ((a b) #'(list b a))
           ((y) #'(list y)))))))
  (test '((2 1) (3)) (list (sc-nest (1 2)) (sc-nest (3)))))

(test-group "with-syntax and quasisyntax"
  (define-syntax ws-pair
    (lambda (stx)
      (syntax-case stx ()
        ((_ a b)
         (with-syntax ((x #'a) (y #'b))
           #'(cons x y))))))
  (test '(1 . 2) (ws-pair 1 2))

  (define-syntax qs-plain
    (lambda (stx)
      (syntax-case stx ()
        ((_ a) #`(list a #,(+ 1 2))))))
  (test '(9 3) (qs-plain 9))

  (define-syntax qs-splice
    (lambda (stx)
      (syntax-case stx ()
        ((_ a ...) #`(list 0 #,@(syntax->list #'(a ...)) 9)))))
  (test '(0 1 2 9) (qs-splice 1 2))

  ;; nesting: an inner quasisyntax raises the depth, an unsyntax lowers it
  (define-syntax qs-nested
    (lambda (stx)
      (syntax-case stx ()
        ((_) #`(quote (quasisyntax (unsyntax #,(+ 1 1))))))))
  (test '(quasisyntax (unsyntax 2)) (qs-nested)))

;; The diagnostics live in pkg/wile/syntax_forms_switch_test.go, not here, and
;; the reason is a real asymmetry between the two layers rather than a
;; convenience. Under the Go layer a syntax-case diagnostic is a Go error raised
;; by the expander and never enters the VM, so (guard … (eval …)) catches it.
;; Under the Scheme layer it is %syntax-violation returning from a primitive
;; INSIDE a macro transformer; the VM turns a returned error into a raise, the
;; transformer's macro sub-context carries no handler chain, and the exception
;; escapes past any guard around the eval that triggered the expansion. That is
;; not new: (error "boom") inside a transformer is equally uncatchable on the Go
;; layer — measured 2026-09-06. A suite that must pass under BOTH layers cannot
;; assert on it, so the Go pins read the error directly instead.

(test-end)
(test-exit)
