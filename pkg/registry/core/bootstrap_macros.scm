;; Bootstrap Macros
;;
;; Essential derived expression forms (define-syntax) required for Scheme
;; to function. This file is embedded at compile-time via go:embed.
;;
;; Loaded after all primitives are registered but before bootstrap
;; procedures or any user code runs. Provides standard R7RS
;; conditionals, lazy evaluation, parameters, exceptions, records, and
;; iteration. Binding forms (let, let*, letrec, letrec*) are core
;; compiled — see plans/CORE-LET.md.
;;
;; Lambda: The Ultimate Imperative (Steele & Sussman, AIM-353, 1976).
;; Derived forms are defined as macros over core forms per R7RS §7.3.
;;
;;   (let ((x e) ...) body ...) ≡ ((lambda (x ...) body ...) e ...)
;;
;;   let IS lambda application. This is not syntactic sugar — it is
;;   the formal identity from the Lambda Papers, made executable.
;;
;;   Invariant: every derived form must reduce to core forms only
;;     (if, lambda, begin, define, set!, quote, dynamic-wind, case-lambda,
;;      let, let*, letrec, letrec*).
;;   Constrained by: with-binding-scope (Wile-specific form for Flatt 2016
;;     scope creation), syntax-rules hygiene (intro scopes on expansion).
;;
;; See BIBLIOGRAPHY.md "The Lambda Papers".

;; Boolean operators
(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and test) test)
    ((and test1 test2 ...)
     (if test1 (and test2 ...) #f))))

(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or test) test)
    ((or test1 test2 ...)
     (let ((x test1))
       (if x x (or test2 ...))))))

;; Binding forms (let, let*, letrec, letrec*) are now core compiled forms,
;; handled directly by the expander/validator/compiler pipeline.
;; See plans/CORE-LET.md for the design.
;; with-binding-scope is retained for user-defined binding macros.

;; Conditional forms
(define-syntax cond
  (syntax-rules (else =>)
    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))
    ((cond (test => result))
     (let ((temp test))
       (if temp (result temp))))
    ((cond (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (cond clause1 clause2 ...))))
    ((cond (test))
     test)
    ((cond (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp temp (cond clause1 clause2 ...))))
    ((cond (test result1 result2 ...))
     (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...) clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (cond clause1 clause2 ...)))))

(define-syntax case
  (syntax-rules (else =>)
    ((case (key ...) clauses ...)
     (let ((atom-key (key ...)))
       (case atom-key clauses ...)))
    ((case key (else => result))
     (result key))
    ((case key (else result1 result2 ...))
     (begin result1 result2 ...))
    ((case key ((atoms ...) => result))
     (if (memv key '(atoms ...))
         (result key)))
    ((case key ((atoms ...) => result) clause clauses ...)
     (if (memv key '(atoms ...))
         (result key)
         (case key clause clauses ...)))
    ((case key ((atoms ...) result1 result2 ...))
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)))
    ((case key ((atoms ...) result1 result2 ...) clause clauses ...)
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)
         (case key clause clauses ...)))))

(define-syntax when
  (syntax-rules ()
    ((when test result1 result2 ...)
     (if test (begin result1 result2 ...)))))

;; Lazy evaluation (promises)
(define-syntax delay
  (syntax-rules ()
    ((delay expression)
     (%make-lazy-promise (lambda () expression)))))

(define-syntax delay-force
  (syntax-rules ()
    ((delay-force expression)
     (%make-lazy-promise (lambda () expression)))))

;; Parameters (dynamic binding via continuation marks)
;;
;; parameterize uses with-continuation-mark to store parameter bindings on the
;; continuation frame. Parameter lookup (0-arg call) walks the mark chain,
;; falling back to the base value. This is correct under composable
;; continuations: the marks ride on the continuation frames, so composing a
;; captured continuation automatically carries its parameter bindings without
;; firing before/after thunks that could clobber unrelated parameterize extents.
;;
;; %parameter-convert applies the converter (if any) before storing the mark,
;; so the converter runs exactly once per parameterize entry.
;;
;; R7RS §4.2.6 requires every value expression (and converter) to be evaluated
;; in the dynamic environment in effect *before* the parameterize, then bound.
;; The macro therefore runs in two phases:
;;   %bind — evaluate each param object + converted value into temporaries (pt vt)
;;           in the OUTER dynamic extent (no marks installed yet), accumulating
;;           them left-to-right; a later clause's value cannot see an earlier
;;           clause's not-yet-installed binding.
;;   %mark — nest with-continuation-mark over the already-evaluated temporaries.
;; The accumulator is threaded through parameterize's own phase-marked recursion.
;; A two-helper-macro split (one macro accumulating the temporaries, another
;; emitting the marks) expands correctly too; this single self-recursive form
;; just keeps the threading in one definition.
(define-syntax parameterize
  (syntax-rules (%bind %mark)
    ((parameterize ((p v) ...) body ...)
     (parameterize %bind ((p v) ...) () (body ...)))
    ((parameterize %bind ((p v) rest ...) (acc ...) bw)
     (let* ((pt p) (vt (%parameter-convert pt v)))   ; let* — vt's init reads pt
       (parameterize %bind (rest ...) (acc ... (pt vt)) bw)))
    ((parameterize %bind () (acc ...) bw)
     (parameterize %mark (acc ...) bw))
    ((parameterize %mark () (b ...)) (begin b ...))
    ((parameterize %mark ((pt vt) rest ...) bw)
     (with-continuation-mark pt vt (parameterize %mark (rest ...) bw)))))


;; Records (SRFI-9 / R7RS define-record-type)
;;
;; Both define-record-type and define-opaque-record-type delegate to
;; define-record-type-impl, passing the maker procedure as the first
;; template variable. Opaque records are hidden from record? and
;; record-type but their type-specific predicates and accessors work
;; normally.
;; define-record-type-impl is defined BEFORE its two delegators (D0, 2026-07-22)
;; so that their templates, which freely reference define-record-type-impl, pin
;; that reference to its definition-site binding at macro-definition time — a
;; use-site (define-syntax define-record-type-impl …) then cannot capture it (D2,
;; R7RS 4.3.2). It references only define/begin + record primitives (registered
;; before bootstrap macros), so it compiles standalone here.
(define-syntax define-record-type-impl
  (syntax-rules ()
    ((define-record-type-impl maker type (constructor constructor-tag ...) predicate (field-name ...) (defn ...))
     (begin
       (define type (maker 'type '(field-name ...)))
       (define constructor (record-constructor type '(constructor-tag ...)))
       (define predicate (record-predicate type))
       defn ...))
    ((define-record-type-impl maker type (constructor constructor-tag ...) predicate (field-name ...) (defn ...) (field-tag accessor) rest ...)
     (define-record-type-impl maker type (constructor constructor-tag ...) predicate
       (field-name ... field-tag)
       (defn ... (define accessor (record-accessor type 'field-tag)))
       rest ...))
    ((define-record-type-impl maker type (constructor constructor-tag ...) predicate (field-name ...) (defn ...) (field-tag accessor modifier) rest ...)
     (define-record-type-impl maker type (constructor constructor-tag ...) predicate
       (field-name ... field-tag)
       (defn ... (begin (define accessor (record-accessor type 'field-tag)) (define modifier (record-modifier type 'field-tag))))
       rest ...))))

(define-syntax define-record-type
  (syntax-rules ()
    ((define-record-type type (constructor constructor-tag ...) predicate field-spec ...)
     (define-record-type-impl make-record-type type (constructor constructor-tag ...) predicate () () field-spec ...))))

(define-syntax define-opaque-record-type
  (syntax-rules ()
    ((define-opaque-record-type type (constructor constructor-tag ...) predicate field-spec ...)
     (define-record-type-impl make-opaque-record-type type (constructor constructor-tag ...) predicate () () field-spec ...))))

;; Multiple values binding forms
;;
;; Note: with-binding-scope adds scope to the entire body including the expr.
;; This is harmless - adding scopes to non-binding references doesn't break them
;; because the scope check is "binding scopes ⊆ use scopes".
(define-syntax let-values
  (syntax-rules ()
    ((let-values () body ...)
     (let () body ...))
    ((let-values ((formals expr)) body ...)
     (with-binding-scope ()
       (call-with-values
         (lambda () expr)
         (lambda formals body ...))))
    ((let-values ((formals expr) more ...) body ...)
     (with-binding-scope ()
       (call-with-values
         (lambda () expr)
         (lambda formals
           (let-values (more ...) body ...)))))))

(define-syntax let*-values
  (syntax-rules ()
    ((let*-values () body ...)
     (let () body ...))
    ((let*-values ((formals expr) more ...) body ...)
     (call-with-values
       (lambda () expr)
       (lambda formals
         (let*-values (more ...) body ...))))))

;; define-values - R7RS 5.3.3
;; Binds multiple variables to values from a multiple-value expression.
;; Collects the values into a template-introduced temporary `tmp`, then binds
;; each variable from it. `tmp` is a macro-introduced TOP-LEVEL binder: the
;; expander's top-level binder-hygiene pass (compilation/toplevel_binder_hygiene.go)
;; gives it a fresh unique name per expansion, so this is set!-free and works
;; under the immutable top level, across compilation units, and under the
;; NoMutation dialect (which removes set!). define-values is a DEFINITION
;; (R7RS §5.3.3), not a mutation, so NoMutation must keep it.
;; Also supports rest patterns: (define-values (x . rest) expr) and
;; (define-values var expr) collects all values as a list.
(define-syntax define-values
  (syntax-rules ()
    ((define-values () expr)
     (call-with-values (lambda () expr) (lambda () (if #f #f))))
    ((define-values (var) expr)
     (define var (call-with-values (lambda () expr) (lambda (x) x))))
    ;; Dotted pattern base case: (var . rest) where rest is a single symbol
    ;; This must come AFTER the multi-var dotted case to avoid matching (x y . z)
    ;; Proper list pattern: (var0 var1 ...)
    ((define-values (var0 var1 ...) expr)
     (begin
       (define tmp (call-with-values (lambda () expr) list))
       (define var0 (car tmp))
       (define-values (var1 ...) (apply values (cdr tmp)))))
    ;; Dotted pattern with two+ vars: (var0 var1 . rest) reduces to (var1 . rest)
    ((define-values (var0 var1 . rest) expr)
     (begin
       (define tmp (call-with-values (lambda () expr) list))
       (define var0 (car tmp))
       (define-values (var1 . rest) (apply values (cdr tmp)))))
    ;; Dotted pattern base case: (var . rest) binds var to first, rest to remaining list
    ((define-values (var . rest) expr)
     (begin
       (define tmp (call-with-values (lambda () expr) list))
       (define var (car tmp))
       (define rest (cdr tmp))))
    ;; All values as list: var (no parens)
    ((define-values var expr)
     (define var (call-with-values (lambda () expr) list)))))

;; Iteration
(define-syntax do
  (syntax-rules ()
    ((do ((var init step ...) ...)
         (test result ...)
         command ...)
     (letrec ((loop (lambda (var ...)
                      (if test
                          (begin result ...)
                          (begin
                            command ...
                            (loop (do "step" var step ...) ...))))))
       (loop init ...)))
    ((do "step" var)
     var)
    ((do "step" var step)
     step)))

;; Continuation barriers (S7-compatible)
(define-syntax with-continuation-barrier
  (syntax-rules ()
    ((_ body ...)
     (call-with-continuation-barrier (lambda () body ...)))))

(define-syntax with-baffle
  (syntax-rules ()
    ((_ body ...)
     (call-with-continuation-barrier (lambda () body ...)))))
