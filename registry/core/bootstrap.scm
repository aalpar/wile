;; Bootstrap Macros
;;
;; Essential derived expression forms required for Scheme to function.
;; This file is embedded at compile-time into the Wile binary via go:embed.
;;
;; These macros are loaded after all primitives are registered but before
;; any user code runs. They provide the standard R7RS binding forms, conditionals,
;; lazy evaluation, parameters, exceptions, records, and iteration.

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

;; Binding forms
;;
;; Each binding form uses with-binding-scope to create a fresh scope for its
;; bindings. This is essential for hygienic macro expansion - it ensures that
;; nested bindings of the same name can be distinguished by their scopes.
;; See Flatt 2016 "Binding as Sets of Scopes" for the theoretical foundation.
(define-syntax let
  (syntax-rules ()
    ((let ((name val) ...) body ...)
     (with-binding-scope (name ...)
       ((lambda (name ...) (begin body ...)) val ...)))
    ((let tag ((name val) ...) body ...)
     (with-binding-scope (tag name ...)
       (letrec ((tag (lambda (name ...) body ...)))
         (tag val ...))))))

(define-syntax let*
  (syntax-rules ()
    ((let* () body ...)
     (let () body ...))
    ((let* ((name1 val1) (name2 val2) ...) body ...)
     (let ((name1 val1))
       (let* ((name2 val2) ...) body ...)))))

(define-syntax letrec
  (syntax-rules ()
    ((letrec ((var init) ...) body ...)
     (let ((var #f) ...)
       (set! var init) ...
       body ...))))

;; letrec* - like letrec but initializers are evaluated left-to-right
;; R7RS §4.2.2: each variable is assigned in left-to-right order.
;;
;; This implementation delegates to letrec because Wile's letrec expansion
;; produces sequential (set! var init) statements, which are evaluated
;; left-to-right per R7RS §4.2.3. This differs from the canonical R7RS §7.3
;; recursive macro but is semantically equivalent for this implementation.
;; See plans/IMPLEMENTATION_NOTES.md for details.
(define-syntax letrec*
  (syntax-rules ()
    ((letrec* ((var init) ...) body ...)
     (letrec ((var init) ...) body ...))))

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

(define-syntax unless
  (syntax-rules ()
    ((unless test result1 result2 ...)
     (if (not test) (begin result1 result2 ...)))))

;; Lazy evaluation (promises)
(define-syntax delay
  (syntax-rules ()
    ((delay expression)
     (%make-lazy-promise (lambda () expression)))))

(define-syntax delay-force
  (syntax-rules ()
    ((delay-force expression)
     (%make-lazy-promise (lambda () expression)))))

;; Parameters (dynamic binding)
(define-syntax parameterize
  (syntax-rules ()
    ((parameterize () body ...)
     (begin body ...))
    ((parameterize ((param val) rest ...) body ...)
     (let ((p param)
           (new val)
           (old (param)))
       (dynamic-wind
         (lambda () (p new))
         (lambda () (parameterize (rest ...) body ...))
         (lambda () (p old)))))))

;; Exception handling (R7RS guard macro)
(define-syntax guard
  (syntax-rules ()
    ((guard (var clause ...) body ...)
     (call/cc
       (lambda (guard-continuation)
         (with-exception-handler
           (lambda (condition)
             (guard-continuation
               (let ((var condition))
                 (guard-aux var clause ...))))
           (lambda () body ...)))))))

(define-syntax guard-aux
  (syntax-rules (else =>)
    ((guard-aux var (else result ...))
     (begin result ...))
    ((guard-aux var (test => proc) clause ...)
     (let ((t test))
       (if t
           (proc t)
           (guard-aux var clause ...))))
    ((guard-aux var (test result ...) clause ...)
     (if test
         (begin result ...)
         (guard-aux var clause ...)))
    ((guard-aux var)
     (raise var))))

;; Records (SRFI-9 / R7RS define-record-type)
(define-syntax define-record-type
  (syntax-rules ()
    ((define-record-type type (constructor constructor-tag ...) predicate field-spec ...)
     (define-record-type-impl type (constructor constructor-tag ...) predicate () () field-spec ...))))

(define-syntax define-record-type-impl
  (syntax-rules ()
    ((define-record-type-impl type (constructor constructor-tag ...) predicate (field-name ...) (defn ...))
     (begin
       (define type (make-record-type 'type '(field-name ...)))
       (define constructor (record-constructor type '(constructor-tag ...)))
       (define predicate (record-predicate type))
       defn ...))
    ((define-record-type-impl type (constructor constructor-tag ...) predicate (field-name ...) (defn ...) (field-tag accessor) rest ...)
     (define-record-type-impl type (constructor constructor-tag ...) predicate
       (field-name ... field-tag)
       (defn ... (define accessor (record-accessor type 'field-tag)))
       rest ...))
    ((define-record-type-impl type (constructor constructor-tag ...) predicate (field-name ...) (defn ...) (field-tag accessor modifier) rest ...)
     (define-record-type-impl type (constructor constructor-tag ...) predicate
       (field-name ... field-tag)
       (defn ... (begin (define accessor (record-accessor type 'field-tag)) (define modifier (record-modifier type 'field-tag))))
       rest ...))))

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
;; Uses a recursive expansion that collects values into a list, then
;; extracts them one by one with set!.
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
       (define var0 (call-with-values (lambda () expr) list))
       (define-values (var1 ...) (apply values (cdr var0)))
       (set! var0 (car var0))))
    ;; Dotted pattern with two+ vars: (var0 var1 . rest) reduces to (var1 . rest)
    ((define-values (var0 var1 . rest) expr)
     (begin
       (define var0 (call-with-values (lambda () expr) list))
       (define-values (var1 . rest) (apply values (cdr var0)))
       (set! var0 (car var0))))
    ;; Dotted pattern base case: (var . rest) binds var to first, rest to remaining list
    ((define-values (var . rest) expr)
     (begin
       (define var (call-with-values (lambda () expr) list))
       (define rest (cdr var))
       (set! var (car var))))
    ;; All values as list: var (no parens)
    ((define-values var expr)
     (define var (call-with-values (lambda () expr) list)))))

;; Higher-order list operations
;; Implemented in Scheme so that iteration produces capturable Scheme
;; continuation frames (enabling call/cc inside map/for-each callbacks).
(define map
  (case-lambda
    ((f lst)
     (let loop ((lst lst))
       (if (null? lst) '()
           (cons (f (car lst)) (loop (cdr lst))))))
    ((f lst . lsts)
     (let loop ((all (cons lst lsts)))
       (if (let any-null? ((ls all))
             (if (null? ls) #f
                 (if (null? (car ls)) #t
                     (any-null? (cdr ls)))))
           '()
           (cons (apply f (map car all))
                 (loop (map cdr all))))))))

(define for-each
  (case-lambda
    ((f lst)
     (let loop ((lst lst))
       (if (null? lst) (if #f #f)
           (begin (f (car lst)) (loop (cdr lst))))))
    ((f lst . lsts)
     (let loop ((all (cons lst lsts)))
       (if (let any-null? ((ls all))
             (if (null? ls) #f
                 (if (null? (car ls)) #t
                     (any-null? (cdr ls)))))
           (if #f #f)
           (begin (apply f (map car all))
                  (loop (map cdr all))))))))

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
