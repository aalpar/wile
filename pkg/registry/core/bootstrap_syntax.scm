;;; bootstrap_syntax.scm — the Scheme-specified syntax layer, part 2
;;; (plans/2026-09-04-scheme-specified-syntax-forms-design §3.0 step 2).
;;;
;;; One-line shells over the generators in bootstrap_syntax_procedures.scm. Each
;;; form carries a docstring derived from its specialforms.go row (prose extended
;;; to the shapes the Scheme layer adds; the two %…/ellipsis forms have no row
;;; and get a new docstring in the same shape);
;;; ,doc reads it from the binding (BindingMeta.Doc), so the rows go in P4. Loaded as the
;;; second macro source, before bootstrap_macros.scm.

(define-syntax syntax-case
  "Pattern-matching macro transformer (R6RS §12.4). Each clause is
(PATTERN BODY) or (PATTERN FENDER BODY); the first pattern that matches
EXPRESSION, whose fender (if any) is true, selects its body. Pattern
variables are bound in BODY and substituted by (syntax TEMPLATE). Identifiers
in the LITERAL list match by free-identifier=?; _ matches anything; a lone
identifier pattern binds the whole form.
Syntax: (syntax-case EXPRESSION (LITERAL ...) CLAUSE ...)
Category: macros

Examples:
  (define-syntax swap!
    (lambda (stx)
      (syntax-case stx ()
        ((_ a b) #'(let ((t a)) (set! a b) (set! b t))))))"
  (lambda (x)
    (%syntax-case-transform x (quote-syntax ...))))

(define-syntax %syntax-case/ellipsis
  "syntax-case with an explicit ellipsis identifier; what syntax-rules threads
its custom ellipsis through (design Q7).
Syntax: (%syntax-case/ellipsis ELLIPSIS EXPRESSION (LITERAL ...) CLAUSE ...)
Category: macros"
  (lambda (x)
    (let ((parts (syntax->list x)))
      (if (if parts (null? (cdr parts)) #t)
          (%syntax-violation '%syntax-case/ellipsis "expected an ellipsis identifier" x)
          #f)
      (%syntax-case-transform (datum->syntax x (cons (car parts) (cdr (cdr parts))))
                              (car (cdr parts))))))

(define-syntax syntax
  "Constructs a syntax object from TEMPLATE, substituting the pattern
variables of the enclosing syntax-case or with-syntax at their ellipsis
depth; every other identifier keeps the lexical context it was written in
(quote-syntax). #'TEMPLATE reads as (syntax TEMPLATE). R6RS §12.4.
Syntax: (syntax TEMPLATE)
Category: macros

Examples:
  (syntax-case #'(1 2) () ((a b) #'(b a)))  ; => syntax for (2 1)"
  (lambda (x)
    (%syntax-transform x (quote-syntax ...))))

(define-syntax %syntax/ellipsis
  "syntax with an explicit ellipsis identifier; what syntax-rules threads its
custom ellipsis through (design Q7).
Syntax: (%syntax/ellipsis ELLIPSIS TEMPLATE)
Category: macros"
  (lambda (x)
    (let ((parts (syntax->list x)))
      (if (if parts (if (null? (cdr parts)) #t (null? (cdr (cdr parts)))) #t)
          (%syntax-violation '%syntax/ellipsis "expected (%syntax/ellipsis ellipsis template)" x)
          #f)
      (%syntax-transform (datum->syntax x (cons (car parts) (cdr (cdr parts))))
                         (car (cdr parts))))))

(define-syntax with-syntax
  "Binds syntax-case pattern variables from EXPRESSIONs: each PATTERN is
matched against the value of its EXPRESSION (a syntax object, or a datum,
which is wrapped) and the variables are bound in BODY. R6RS §12.8.
Syntax: (with-syntax ((PATTERN EXPRESSION) ...) BODY ...)
Category: macros

Examples:
  (with-syntax (((t ...) (generate-temporaries #'(a b))))
    #'(let ((t 0) ...) (list t ...)))"
  (lambda (x)
    (%with-syntax-transform x)))

(define-syntax quasisyntax
  "Like quasiquote for syntax objects: TEMPLATE is a syntax template whose
(unsyntax EXPRESSION) and (unsyntax-splicing EXPRESSION) escapes are
evaluated and inserted; pattern variables substitute as in syntax.
#`TEMPLATE, #,EXPRESSION and #,@EXPRESSION are the reader forms. R6RS §12.8.
Syntax: (quasisyntax TEMPLATE) or #`TEMPLATE
Category: macros

Examples:
  (syntax-case #'(1) () ((a) #`(list a #,(+ 1 2))))  ; => syntax for (list 1 3)"
  (lambda (x)
    (%quasisyntax-transform x)))

(define-syntax unsyntax
  "Inside quasisyntax, evaluates EXPRESSION and inserts its value (a syntax
object or a datum). A syntax violation outside quasisyntax. R6RS §12.8.
Syntax: (unsyntax EXPRESSION) or #,EXPRESSION
Category: macros"
  (lambda (x)
    (%syntax-violation 'unsyntax "not in quasisyntax context" x)))

(define-syntax unsyntax-splicing
  "Inside quasisyntax, evaluates EXPRESSION, which must return a list, and
splices its elements. A syntax violation outside quasisyntax. R6RS §12.8.
Syntax: (unsyntax-splicing EXPRESSION) or #,@EXPRESSION
Category: macros"
  (lambda (x)
    (%syntax-violation 'unsyntax-splicing "not in quasisyntax context" x)))
