;; SRFI 26: Notation for Specializing Parameters without Currying.
;;
;; cut  — specialize a procedure with positional slots; non-slot operands
;;        are re-evaluated on every call to the specialized procedure.
;; cute — "cut with evaluated non-slots"; non-slot operands are evaluated
;;        exactly once, when the specialized procedure is constructed.
;;
;; Slots:  <>     a positional argument hole (becomes a fresh formal)
;;         <...>  a rest-slot; must appear last; collects trailing args.
;;
;; These are the portable syntax-rules reference implementations. They rely
;; on hygienic renaming of the template-introduced formal `x` across each
;; recursive expansion step: two <> slots inject `x` twice, and the expander
;; keeps the two bindings distinct (sets-of-scopes model).

;; ----------------------------------------------------------------------
;; cut
;; ----------------------------------------------------------------------
;; Recursive accumulator over the operand list, growing PARAMS (one fresh
;; formal per <>) and ARGS (the call being rebuilt). Non-slot operands are
;; spliced verbatim into ARGS, so they re-evaluate on each invocation.

(define-syntax cut
  (syntax-rules ()
    ((cut . slots-or-exprs)
     (%cut-walk () () . slots-or-exprs))))

(define-syntax %cut-walk
  (syntax-rules (<> <...>)
    ;; terminal: fixed arity
    ((%cut-walk (params ...) (args ...))
     (lambda (params ...) (args ...)))
    ;; terminal: rest-slot (must be final)
    ((%cut-walk (params ...) (args ...) <...>)
     (lambda (params ... . rest) (apply args ... rest)))
    ;; positional slot: introduce a fresh formal, thread it into the call
    ((%cut-walk (params ...) (args ...) <> . more)
     (%cut-walk (params ... x) (args ... x) . more))
    ;; non-slot expression: splice verbatim (re-evaluated per call)
    ((%cut-walk (params ...) (args ...) expr . more)
     (%cut-walk (params ...) (args ... expr) . more))))

;; ----------------------------------------------------------------------
;; cute
;; ----------------------------------------------------------------------
;; Same walk as cut, but carries a THIRD accumulator, BINDS, of let
;; bindings. Each non-slot operand is lifted into a fresh binding so it is
;; evaluated once; the terminal clauses wrap the lambda in (let BINDS ...).

(define-syntax cute
  (syntax-rules ()
    ((cute . slots-or-exprs)
     (%cute-walk () () () . slots-or-exprs))))

(define-syntax %cute-walk
  (syntax-rules (<> <...>)
    ;; terminal: fixed arity — the wrapping let gives once-evaluation
    ((%cute-walk (params ...) (binds ...) (args ...))
     (let (binds ...) (lambda (params ...) (args ...))))
    ;; terminal: rest-slot
    ((%cute-walk (params ...) (binds ...) (args ...) <...>)
     (let (binds ...) (lambda (params ... . rest) (apply args ... rest))))
    ;; positional slot: fresh formal, threaded into the call (no binding)
    ((%cute-walk (params ...) (binds ...) (args ...) <> . more)
     (%cute-walk (params ... x) (binds ...) (args ... x) . more))
    ;; non-slot expression: lift into a once-evaluated binding, thread the
    ;; bound name (not the expression) into the call. APPEND the binding (not
    ;; prepend) so the let binds operands in textual order — let evaluates its
    ;; inits top-down, so this preserves left-to-right evaluation of non-slot
    ;; operands, matching cut and the SRFI-26 reference implementation.
    ((%cute-walk (params ...) (binds ...) (args ...) expr . more)
     (%cute-walk (params ...) (binds ... (a expr)) (args ... a) . more))))
