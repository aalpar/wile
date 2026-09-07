;;; bootstrap_syntax.scm — the Scheme-specified syntax layer, part 2
;;; (plans/2026-09-04-scheme-specified-syntax-forms-design §3.0 step 2).
;;;
;;; One-line shells over the generators in bootstrap_syntax_procedures.scm. Each
;;; form carries a docstring derived from its specialforms.go row (prose extended
;;; to the shapes the Scheme layer adds; the two %…/ellipsis forms have no row
;;; and get a new docstring in the same shape). Loaded as the second macro
;;; source, before bootstrap_macros.scm.
;;;
;;; ,doc does NOT yet read these: measured 2026-09-07 under the layer,
;;; `,doc syntax-rules` prints the specialforms.go text, because cmdDoc reaches
;;; the registry row for a name that has one and the row wins. The docstrings
;;; here become visible when P4 deletes the eight rows — which is also when a
;;; row's ApplyDocs pass would otherwise overwrite the macro's own docstring.
;;; Six of the eight are still accurate for the DEFAULT layer, so nothing
;;; user-visible is wrong today; er-macro-transformer's row describes the Go
;;; form contract, which is the one the default serves.

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

(define-syntax syntax-rules
  "Defines a pattern-based macro transformer: each clause is (PATTERN TEMPLATE)
and the first pattern matching the macro use selects its template. The
pattern's first element is ignored. Identifiers in the LITERAL list match by
free-identifier=?; _ matches anything; ... (or ELLIPSIS when given) marks
repetition; (... ...) escapes an ellipsis in a template. R7RS §4.3.2.
Syntax: (syntax-rules (LITERAL ...) CLAUSE ...) or (syntax-rules ELLIPSIS (LITERAL ...) CLAUSE ...)
Category: macros

Examples:
  (define-syntax my-if
    (syntax-rules (then else)
      ((my-if test then c else a) (if test c a))))"
  (lambda (x)
    (%syntax-rules-transform x)))

(define-syntax er-macro-transformer
  "Explicit-renaming macro transformer (Clinger 1991). PROC receives the macro
use as a form whose pairs and vectors are plain and whose identifiers stay
syntax objects (identifier? is the test), a RENAME procedure giving a symbol
the macro definition's context, and a COMPARE procedure (free-identifier=?)
testing two identifiers for the same binding; both COMPARE arguments must be
identifiers, a bare symbol is an error. A symbol PROC introduces without
renaming takes the use site's context. Wile extension.
Syntax: (er-macro-transformer PROC)
Category: macros

Examples:
  (define-syntax swap!
    (er-macro-transformer
      (lambda (form rename compare)
        (let ((a (cadr form)) (b (caddr form)) (tmp (rename 'tmp)))
          `(,(rename 'let) ((,tmp ,a)) (,(rename 'set!) ,a ,b) (,(rename 'set!) ,b ,tmp))))))"
  (lambda (x)
    (syntax-case x ()
      ((k proc)
       #'(let ((p (%er-proc proc #'k)))
           (lambda (stx)
             (datum->syntax (syntax-car stx)
               (p (%syntax-spine stx)
                  (lambda (sym) (datum->syntax #'k sym))
                  free-identifier=?))))))))
