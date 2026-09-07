;;; bootstrap_syntax_procedures.scm — the Scheme-specified syntax layer, part 1
;;; (plans/2026-09-04-scheme-specified-syntax-forms-design §3.0 step 1).
;;;
;;; Loaded into the sealed base as the FIRST macro source, before
;;; bootstrap_macros.scm. Only Go core forms (lambda if begin set! define quote
;;; let let* letrec letrec*) and Go primitives are bound here; and/or/cond/when,
;;; not, map, cadr are bootstrap macros and procedures that load later. Generated
;;; code (the %gen-* procedures) uses only these helpers and Go primitives for the
;;; same reason: a bootstrap macro's expansion runs before bootstrap_procedures.scm
;;; has loaded.
;;;
;;; Hygiene is the kernel's (design §2.2): nothing here mints or copies a scope.
;;; Generated identifiers are quote-syntax constants pinned to this file's
;;; definitions; the kernel's per-invocation intro scope keeps them apart from
;;; user code, and generate-temporaries keeps nested binders apart from each
;;; other.
;;;
;;; A pattern variable is bound twice under one identifier: as a let variable
;;; holding the matched syntax, and as a let-syntax %pattern-variable record.
;;; The two reach different bindings, and the generated code depends on that. A
;;; plain reference in a clause body reaches the let variable, because
;;; ExpandSyntaxExpression tests for a local variable binding before it looks for
;;; a keyword; (syntax x) reaches the record, because %pv-record goes through
;;; syntax-local-value, which resolves without that test.
;;;
;;; That is not a blanket win for the record. lookupMacroBinding's first arm takes
;;; the maximal-scope binding of the name (localBinding, environment_frame.go) and
;;; only then filters for BindingTypeSyntax, so a clause-body (let ((x 1)) ...)
;;; binder is maximal, blocks the record, and leaves (syntax x) emitting the
;;; identifier unsubstituted, which is Chez's shadowing rule. Task 8 Step 2 pins it.
;;;
;;; The whole body is ONE (begin …) unit, and that is load-bearing rather than
;;; cosmetic: LoadBootstrapSources compiles a bootstrap source form by form, so a
;;; reference from one top-level define to a later one is an unbound binding at
;;; compile time. The generators here are mutually recursive by construction —
;;; %gen-match with %gen-match-ellipsis, %gen-template with %gen-pair,
;;; %gen-template-ellipsis and %gen-ellipsis-map — so no ordering fixes it. A
;;; begin body predeclares every definition before compiling any of them
;;; (expander_body.go), which is the same reason file execution begin-wraps.
;;; begin makes no region, so the definitions still land at top level.

(begin

;;; ---- the %pattern-variable record (Q4: a tagged list, stored bare) ----

(define (%make-pattern-variable depth id)
  (list '%pattern-variable depth id))

(define (%pattern-variable? v)
  (if (pair? v) (eq? (car v) '%pattern-variable) #f))

(define (%pattern-variable-depth pv)
  (car (cdr pv)))

(define (%pattern-variable-id pv)
  (car (cdr (cdr pv))))

;;; ---- list helpers (map is not loaded yet) ----

(define (%map1 f l)
  (if (null? l) '() (cons (f (car l)) (%map1 f (cdr l)))))

(define (%all-null? ls)
  (if (null? ls) #t (if (null? (car ls)) (%all-null? (cdr ls)) #f)))

(define (%any-null? ls)
  (if (null? ls) #f (if (null? (car ls)) #t (%any-null? (cdr ls)))))

;;; ---- runtime helpers called by generated code ----

;; %syntax-map: n-ary map over equal-length lists, the ellipsis iterator of a
;; template. A length mismatch between the pattern variables of one ellipsis
;; subtemplate is unspecified by R6RS §12.4, which constrains ellipsis depth and
;; not lengths; Wile raises it as a syntax violation, as Chez does
;; ("incompatible ellipsis match counts").
(define (%syntax-map f . lists)
  (letrec ((loop (lambda (lists acc)
                   (if (%any-null? lists)
                       (if (%all-null? lists)
                           (reverse acc)
                           (%syntax-violation 'syntax "ellipsis subtemplate lists have different lengths" '()))
                       (loop (%map1 cdr lists)
                             (cons (apply f (%map1 car lists)) acc))))))
    (loop lists '())))

;; %syntax-append*: flatten one level, for a subtemplate followed by more than
;; one ellipsis.
(define (%syntax-append* lists)
  (apply append lists))

;; %syntax-split-ellipsis: for a pattern (p <ellipsis> . tail) whose tail has AFTER
;; fixed elements, split syntax E into (items . rest): items, a Scheme list of the
;; syntax elements p matches (all but the last AFTER of the pair spine), and rest,
;; the syntax at the position of those AFTER elements, so the tail pattern
;; matches it. #f when the spine has fewer than AFTER elements.
(define (%syntax-split-ellipsis e after)
  (letrec ((count (lambda (s n)
                    (if (syntax-pair? s) (count (syntax-cdr s) (+ n 1)) n)))
           (take (lambda (s k acc)
                   (if (eqv? k 0)
                       (cons (reverse acc) s)
                       (take (syntax-cdr s) (- k 1) (cons (syntax-car s) acc))))))
    (let ((n (count e 0)))
      (if (< n after) #f (take e (- n after) '())))))

;; %syntax-match-each: apply the sub-matcher to every item; each answer is a row
;; (a list of the sub-pattern's variable values) or #f. The rows, or #f at the
;; first failed item.
(define (%syntax-match-each items match)
  (letrec ((loop (lambda (items acc)
                   (if (null? items)
                       (reverse acc)
                       (let ((row (match (car items))))
                         (if row (loop (cdr items) (cons row acc)) #f))))))
    (loop items '())))

;; %syntax-column: the i-th value of every row — one ellipsis variable's list.
(define (%syntax-column rows i)
  (%map1 (lambda (row) (list-ref row i)) rows))

;; %syntax-literal?: an input identifier matches a pattern literal when both
;; denote the same binding (R6RS §12.4; the use-site shadow in design §3.1).
(define (%syntax-literal? e lit)
  (if (identifier? e) (free-identifier=? e lit) #f))

(define (%syntax-case-fail stx)
  (%syntax-violation 'syntax-case "no clause matches the form" stx))

;;; ---- generated-code vocabulary: quote-syntax constants pinned here ----

(define %k-let            (quote-syntax let))
(define %k-let-syntax     (quote-syntax let-syntax))
(define %k-lambda         (quote-syntax lambda))
(define %k-if             (quote-syntax if))
(define %k-quote          (quote-syntax quote))
(define %k-quote-syntax   (quote-syntax quote-syntax))
(define %k-list           (quote-syntax list))
(define %k-cons           (quote-syntax cons))
(define %k-car            (quote-syntax car))
(define %k-cdr            (quote-syntax cdr))
(define %k-append         (quote-syntax append))
(define %k-list->vector   (quote-syntax list->vector))
(define %k-equal?         (quote-syntax equal?))
(define %k-datum->syntax  (quote-syntax datum->syntax))
(define %k-syntax->datum  (quote-syntax syntax->datum))
(define %k-syntax-pair?   (quote-syntax syntax-pair?))
(define %k-syntax-null?   (quote-syntax syntax-null?))
(define %k-syntax-car     (quote-syntax syntax-car))
(define %k-syntax-cdr     (quote-syntax syntax-cdr))
(define %k-syntax-vector? (quote-syntax syntax-vector?))
(define %k-syntax-vector->list (quote-syntax syntax-vector->list))
(define %k-literal?       (quote-syntax %syntax-literal?))
(define %k-split          (quote-syntax %syntax-split-ellipsis))
(define %k-match-each     (quote-syntax %syntax-match-each))
(define %k-column         (quote-syntax %syntax-column))
(define %k-case-fail      (quote-syntax %syntax-case-fail))
(define %k-make-pv        (quote-syntax %make-pattern-variable))
(define %k-syntax-map     (quote-syntax %syntax-map))
(define %k-append*        (quote-syntax %syntax-append*))
;; Bound by bootstrap_syntax.scm, after this file: nil pins, resolved by scopes.
(define %k-syntax-case    (quote-syntax syntax-case))
(define %k-syntax-case/e  (quote-syntax %syntax-case/ellipsis))
(define %k-syntax/e       (quote-syntax %syntax/ellipsis))
(define %k-unsyntax       (quote-syntax unsyntax))
(define %k-unsyntax-splicing (quote-syntax unsyntax-splicing))
(define %k-quasisyntax    (quote-syntax quasisyntax))
(define %k-underscore     (quote-syntax _))

;; %fresh: a temporary for generated binders. generate-temporaries mints a
;; process-unique name, so nested generated lets never shadow each other by
;; accident; the kernel's intro scope keeps them off user identifiers.
(define (%fresh)
  (car (generate-temporaries '(t))))

(define (%const stx)
  (list %k-quote-syntax stx))

(define (%or-const code stx)
  (if code code (%const stx)))

;;; ---- identifier classification ----

;; Literal membership is bound-identifier=?: the literals list and the pattern
;; come from the same form (psyntax's bound-id-member?).
(define (%id-member? id ids)
  (if (null? ids)
      #f
      (if (bound-identifier=? id (car ids)) #t (%id-member? id (cdr ids)))))

;; ELL is the ellipsis identifier, or #f inside a (... template) escape. A literal
;; spelled like the ellipsis is a literal (R7RS: `...` in the literals list).
(define (%ellipsis? x ell lits)
  (if ell
      (if (identifier? x)
          (if (%id-member? x lits) #f (free-identifier=? x ell))
          #f)
      #f))

(define (%underscore? x lits)
  (if (identifier? x)
      (if (%id-member? x lits) #f (free-identifier=? x %k-underscore))
      #f))

;; (p <ellipsis> . rest)?
(define (%ellipsis-next? pat ell lits)
  (let ((d (syntax-cdr pat)))
    (if (syntax-pair? d) (%ellipsis? (syntax-car d) ell lits) #f)))

;; (... template)?
(define (%escape? t ell)
  (if ell
      (if (syntax-pair? t)
          (if (%ellipsis? (syntax-car t) ell '()) (syntax-pair? (syntax-cdr t)) #f)
          #f)
      #f))

;; the template inside a (... template) escape
(define (%escaped t)
  (let ((d (syntax-cdr t)))
    (if (syntax-null? (syntax-cdr d))
        (syntax-car d)
        (%syntax-violation 'syntax "(... template) takes exactly one template" t))))

(define (%count-fixed t)
  (if (syntax-pair? t) (+ 1 (%count-fixed (syntax-cdr t))) 0))

(define (%check-literals lits form)
  (if (null? lits)
      #t
      (if (identifier? (car lits))
          (%check-literals (cdr lits) form)
          (%syntax-violation 'syntax-case "literal is not an identifier" (car lits)))))

;;; ---- pattern variables ----

;; %pattern-vars/depth: ((id . depth) ...) in pattern order; depth counts the
;; ellipses enclosing the variable.
(define (%pattern-vars/depth pat lits ell depth)
  (letrec ((walk (lambda (p depth acc)
                   (if (identifier? p)
                       (if (%underscore? p lits)
                           acc
                           (if (%ellipsis? p ell lits)
                               acc
                               (if (%id-member? p lits) acc (cons (cons p depth) acc))))
                       (if (syntax-pair? p)
                           (if (%ellipsis-next? p ell lits)
                               (walk (syntax-cdr (syntax-cdr p)) depth
                                     (walk (syntax-car p) (+ depth 1) acc))
                               (walk (syntax-cdr p) depth (walk (syntax-car p) depth acc)))
                           (if (syntax-vector? p)
                               (walk (syntax-vector->list p) depth acc)
                               acc))))))
    (reverse (walk pat depth '()))))

(define (%pattern-vars pat lits ell)
  (%map1 car (%pattern-vars/depth pat lits ell 0)))

(define (%check-linear vars)
  (if (null? vars)
      #t
      (if (%id-member? (car (car vars)) (%map1 car (cdr vars)))
          (%syntax-violation 'syntax-case "duplicate pattern variable" (car (car vars)))
          (%check-linear (cdr vars)))))

;;; ---- syntax-case: patterns to nested conditionals (design §3.1) ----

;; %gen-match: code matching PAT against the syntax bound to temporary E,
;; continuing with K on success and FAIL on failure. Pure: variable depths come
;; from %pattern-vars/depth, so K can already carry the let-syntax records.
(define (%gen-match pat e lits ell k fail)
  (if (identifier? pat)
      (if (%id-member? pat lits)
          (list %k-if (list %k-literal? e (%const pat)) k fail)
          (if (%underscore? pat lits)
              k
              (if (%ellipsis? pat ell lits)
                  (%syntax-violation 'syntax-case "misplaced ellipsis in pattern" pat)
                  (list %k-let (list (list pat e)) k))))
      (if (syntax-pair? pat)
          (if (%ellipsis-next? pat ell lits)
              (%gen-match-ellipsis pat e lits ell k fail)
              (let ((a (%fresh)) (d (%fresh)))
                (list %k-if (list %k-syntax-pair? e)
                      (list %k-let (list (list a (list %k-syntax-car e))
                                         (list d (list %k-syntax-cdr e)))
                            (%gen-match (syntax-car pat) a lits ell
                                        (%gen-match (syntax-cdr pat) d lits ell k fail)
                                        fail))
                      fail)))
          (if (syntax-null? pat)
              (list %k-if (list %k-syntax-null? e) k fail)
              (if (syntax-vector? pat)
                  (let ((l (%fresh)))
                    (list %k-if (list %k-syntax-vector? e)
                          (list %k-let (list (list l (list %k-syntax-vector->list e)))
                                (%gen-match (syntax-vector->list pat) l lits ell k fail))
                          fail))
                  (list %k-if
                        (list %k-equal? (list %k-syntax->datum e) (list %k-quote (syntax->datum pat)))
                        k fail))))))

;; (p1 <ellipsis> . tail): split E, match p1 against every item with a local
;; failure (#f), transpose the rows into one list per variable of p1, then match
;; tail against the rest. Vars of p1 are bound one level deeper.
(define (%gen-match-ellipsis pat e lits ell k fail)
  (let* ((p1 (syntax-car pat))
         (tail (syntax-cdr (syntax-cdr pat)))
         (after (%count-fixed tail))
         (sub-vars (%pattern-vars p1 lits ell))
         (s (%fresh)) (rows (%fresh)) (item (%fresh)) (rest (%fresh)))
    (list %k-let (list (list s (list %k-split e after)))
          (list %k-if s
                (list %k-let
                      (list (list rows (list %k-match-each (list %k-car s)
                                             (list %k-lambda (list item)
                                                   (%gen-match p1 item lits ell (cons %k-list sub-vars) #f)))))
                      (list %k-if rows
                            (list %k-let (%column-bindings sub-vars rows 0)
                                  (list %k-let (list (list rest (list %k-cdr s)))
                                        (%gen-match tail rest lits ell k fail)))
                            fail))
                fail))))

(define (%column-bindings vars rows i)
  (if (null? vars)
      '()
      (cons (list (car vars) (list %k-column rows i))
            (%column-bindings (cdr vars) rows (+ i 1)))))

;; The let-syntax records (design §2.3): each pattern variable bound a second
;; time, as a compile-time %pattern-variable holding its depth and identifier.
(define (%record-bindings vars)
  (%map1 (lambda (v) (list (car v) (list %k-make-pv (cdr v) (%const (car v))))) vars))

(define (%bad-clause? parts)
  (if parts
      (let ((n (length parts)))
        (if (eqv? n 2) #f (if (eqv? n 3) #f #t)))
      #t))

;; One clause: (let ((fail (lambda () NEXT))) <match, then fender/body>).
(define (%gen-clause clause in lits ell next)
  (let ((parts (syntax->list clause)))
    (if (%bad-clause? parts)
        (%syntax-violation 'syntax-case "clause must be (pattern body) or (pattern fender body)" clause)
        #f)
    (let* ((pat (car parts))
           (has-fender (if (null? (cdr (cdr parts))) #f #t))
           (fender (if has-fender (car (cdr parts)) #f))
           (body (if has-fender (car (cdr (cdr parts))) (car (cdr parts))))
           (vars (%pattern-vars/depth pat lits ell 0))
           (fail (%fresh))
           (guarded (if has-fender (list %k-if fender body (list fail)) body))
           (k (list %k-let-syntax (%record-bindings vars) guarded)))
      (%check-linear vars)
      (list %k-let (list (list fail (list %k-lambda '() next)))
            (%gen-match pat in lits ell k (list fail))))))

(define (%gen-clauses clauses in lits ell)
  (if (null? clauses)
      (list %k-case-fail in)
      (%gen-clause (car clauses) in lits ell (%gen-clauses (cdr clauses) in lits ell))))

;; %syntax-case-transform: (syntax-case expr (lit ...) clause ...) with ellipsis
;; identifier ELL. The output is wrapped with the use-site form for provenance;
;; every generated identifier is already syntax, so the wrap adds no scopes.
(define (%syntax-case-transform form ell)
  (let ((parts (syntax->list form)))
    (if (if parts (if (null? (cdr parts)) #t (null? (cdr (cdr parts)))) #t)
        (%syntax-violation 'syntax-case "expected (syntax-case expression (literal ...) clause ...)" form)
        #f)
    (let* ((expr (car (cdr parts)))
           (lits (syntax->list (car (cdr (cdr parts)))))
           (clauses (cdr (cdr (cdr parts))))
           (in (%fresh)))
      (if lits #f (%syntax-violation 'syntax-case "literals must be a list" (car (cdr (cdr parts)))))
      (%check-literals lits form)
      (datum->syntax form
        (list %k-let (list (list in expr))
              (%gen-clauses clauses in lits ell))))))

;;; ---- syntax: templates to builder code (design §3.2), with quasisyntax ----

;; %pv-record: the %pattern-variable record an identifier denotes here, or #f.
;; Resolution decides substitution (Design C): syntax-local-value resolves the
;; occurrence under its own scopes through the let-syntax frames.
(define (%pv-record id)
  (let ((v (syntax-local-value id (lambda () #f))))
    (if (%pattern-variable? v) v #f)))

(define (%template-head t)
  (if (syntax-pair? t)
      (let ((a (syntax-car t)))
        (if (syntax-pair? a) (%template-head a) (if (syntax-vector? a) t a)))
      t))

;; (unsyntax e) / (unsyntax-splicing e) / (quasisyntax t) in a quasisyntax
;; template at depth QD (#f in a plain syntax template).
(define (%qs-form? x qd kw)
  (if qd
      (if (syntax-pair? x)
          (if (identifier? (syntax-car x)) (free-identifier=? (syntax-car x) kw) #f)
          #f)
      #f))

(define (%qs-operand x)
  (let ((d (syntax-cdr x)))
    (if (syntax-pair? d)
        (if (syntax-null? (syntax-cdr d))
            (syntax-car d)
            (%syntax-violation 'quasisyntax "expected exactly one operand" x))
        (%syntax-violation 'quasisyntax "expected exactly one operand" x))))

;; %iteration-vars: the pattern variables in T of depth > LEVEL, by record
;; identifier, deduplicated — the variables an ellipsis at LEVEL iterates.
(define (%iteration-vars t level)
  (letrec ((walk (lambda (t acc)
                   (if (identifier? t)
                       (let ((pv (%pv-record t)))
                         (if pv
                             (if (> (%pattern-variable-depth pv) level)
                                 (let ((id (%pattern-variable-id pv)))
                                   (if (%id-member? id acc) acc (cons id acc)))
                                 acc)
                             acc))
                       (if (syntax-pair? t)
                           (walk (syntax-cdr t) (walk (syntax-car t) acc))
                           (if (syntax-vector? t)
                               (walk (syntax-vector->list t) acc)
                               acc))))))
    (reverse (walk t '()))))

;; %count-ellipses: (k . rest) for the ellipses following a subtemplate.
(define (%count-ellipses d ell k)
  (if (syntax-pair? d)
      (if (%ellipsis? (syntax-car d) ell '())
          (%count-ellipses (syntax-cdr d) ell (+ k 1))
          (cons k d))
      (cons k d)))

;; %gen-template: builder code for template T, or #f when T holds no pattern
;; variable and no escape (the caller emits it as one quote-syntax constant, so
;; the kernel's pins and definition-site scopes apply). LEVEL counts the
;; ellipses enclosing this position; a variable of record depth d needs at least
;; d of them and is broadcast under more. QD is the quasisyntax depth or #f.
(define (%gen-template t ell level qd)
  (if (identifier? t)
      (let ((pv (%pv-record t)))
        (if pv
            (if (> (%pattern-variable-depth pv) level)
                (%syntax-violation 'syntax "pattern variable used with too few ellipses" t)
                (%pattern-variable-id pv))
            (if (%ellipsis? t ell '())
                (%syntax-violation 'syntax "misplaced ellipsis in template" t)
                #f)))
      (if (syntax-pair? t)
          (if (%escape? t ell)
              ;; An escape always yields code: the caller must never quote the
              ;; (... template) wrapper in place of its body.
              (%or-const (%gen-template (%escaped t) #f level qd) (%escaped t))
              (if (%qs-form? t qd %k-unsyntax)
                  (if (eqv? qd 1)
                      (%qs-operand t)
                      (list %k-list (%const (syntax-car t))
                            (%or-const (%gen-template (%qs-operand t) ell level (- qd 1)) (%qs-operand t))))
                  (if (%qs-form? t qd %k-quasisyntax)
                      (list %k-list (%const (syntax-car t))
                            (%or-const (%gen-template (%qs-operand t) ell level (+ qd 1)) (%qs-operand t)))
                      (if (%ellipsis-next? t ell '())
                          (%gen-template-ellipsis t ell level qd)
                          (%gen-pair t ell level qd)))))
          (if (syntax-vector? t)
              (let ((l (%gen-template (syntax-vector->list t) ell level qd)))
                (if l (list %k-list->vector l) #f))
              #f))))

;; A pair whose car may be an unsyntax-splicing form.
(define (%gen-pair t ell level qd)
  (let ((a (syntax-car t)) (d (syntax-cdr t)))
    (if (%qs-form? a qd %k-unsyntax-splicing)
        (if (eqv? qd 1)
            (list %k-append (%qs-operand a) (%or-const (%gen-template d ell level qd) d))
            (list %k-cons
                  (list %k-list (%const (syntax-car a))
                        (%or-const (%gen-template (%qs-operand a) ell level (- qd 1)) (%qs-operand a)))
                  (%or-const (%gen-template d ell level qd) d)))
        (let ((ac (%gen-template a ell level qd))
              (dc (%gen-template d ell level qd)))
          (if ac
              (list %k-cons ac (%or-const dc d))
              (if dc (list %k-cons (%const a) dc) #f))))))

;; (sub <ellipsis>^k . rest): k nested %syntax-map calls over sub's iteration
;; variables, flattened k-1 times, appended to rest (Q7: one walker, the
;; ellipsis is a parameter, so a literal ... under a custom ellipsis needs no
;; escape).
(define (%gen-template-ellipsis t ell level qd)
  (let* ((sub (syntax-car t))
         (k+rest (%count-ellipses (syntax-cdr t) ell 0))
         (k (car k+rest))
         (rest (cdr k+rest))
         (mapped (%gen-ellipsis-map sub ell level k qd)))
    (if (syntax-null? rest)
        mapped
        (list %k-append mapped (%or-const (%gen-template rest ell level qd) rest)))))

(define (%gen-ellipsis-map sub ell level k qd)
  (let ((vars (%iteration-vars sub level)))
    (if (null? vars)
        (%syntax-violation 'syntax "ellipsis subtemplate has no pattern variable of sufficient depth" sub)
        #f)
    (let* ((body (if (eqv? k 1)
                     (%or-const (%gen-template sub ell (+ level 1) qd) sub)
                     (%gen-ellipsis-map sub ell (+ level 1) (- k 1) qd)))
           (mapped (cons %k-syntax-map (cons (list %k-lambda vars body) vars))))
      (if (eqv? k 1) mapped (list %k-append* mapped)))))

;; %syntax-transform: (syntax template) with ellipsis ELL. The builder is
;; wrapped (datum->syntax (quote-syntax <head>) …) so the spine carries the
;; template's source location (coverage attribution); existing syntax leaves pass
;; through the wrap and raw pairs become syntax.
(define (%syntax-transform form ell)
  (let ((parts (syntax->list form)))
    (if (if parts (if (null? (cdr parts)) #t (if (null? (cdr (cdr parts))) #f #t)) #t)
        (%syntax-violation 'syntax "expected (syntax template)" form)
        #f)
    (let* ((tmpl (car (cdr parts)))
           (code (%gen-template tmpl ell 0 #f)))
      (datum->syntax form
        (if code
            (list %k-datum->syntax (%const (%template-head tmpl)) code)
            (%const tmpl))))))

;; %quasisyntax-transform: (quasisyntax template) — the same walker at
;; quasisyntax depth 1 (design §3.3: keeps the datum->syntax wrap shape).
(define (%quasisyntax-transform form)
  (let ((parts (syntax->list form)))
    (if (if parts (if (null? (cdr parts)) #t (if (null? (cdr (cdr parts))) #f #t)) #t)
        (%syntax-violation 'quasisyntax "expected (quasisyntax template)" form)
        #f)
    (let* ((tmpl (car (cdr parts)))
           (code (%gen-template tmpl (quote-syntax ...) 0 1)))
      (datum->syntax form
        (list %k-datum->syntax (%const (%template-head tmpl)) (%or-const code tmpl))))))

;;; ---- with-syntax (R6RS §12.8 derivation) ----

(define (%binding-part b i form)
  (let ((l (syntax->list b)))
    (if (if l (eqv? (length l) 2) #f)
        (list-ref l i)
        (%syntax-violation 'with-syntax "binding must be (pattern expression)" b))))

;; (with-syntax ((p e) ...) body ...) =>
;;   (syntax-case (datum->syntax #f (list e ...)) () ((p ...) (let () body ...)))
;; datum->syntax #f wraps a non-syntax value of e (Racket's convention) and passes
;; syntax through.
(define (%with-syntax-transform form)
  (let ((parts (syntax->list form)))
    (if (if parts (null? (cdr parts)) #t)
        (%syntax-violation 'with-syntax "expected (with-syntax ((pattern expression) ...) body ...)" form)
        #f)
    (let ((bindings (syntax->list (car (cdr parts))))
          (body (cdr (cdr parts))))
      (if bindings #f (%syntax-violation 'with-syntax "bindings must be a list" (car (cdr parts))))
      (let ((pats (%map1 (lambda (b) (%binding-part b 0 form)) bindings))
            (exprs (%map1 (lambda (b) (%binding-part b 1 form)) bindings)))
        (datum->syntax form
          (list %k-syntax-case
                (list %k-datum->syntax #f (cons %k-list exprs))
                '()
                (list pats (cons %k-let (cons '() body)))))))))
)
