;;; unification.scm — AC-matching and AC-unification.
;;;
;;; Term protocol contract: term-compare must be a total order consistent
;;; with equal? modulo the AC-equivalence induced by the caller's theory.

(define-record-type <pattern-var>
  (make-pattern-var name)
  pattern-var?
  (name pattern-var-name))

(define-record-type <substitution>
  (make-substitution bindings)
  substitution?
  (bindings substitution-bindings))

(define empty-substitution (make-substitution '()))

(define (parse-pattern expr)
  "Convert EXPR from sexpr with ?-prefix convention to a pattern using
<pattern-var> records. Symbols starting with #\\? become pattern variables;
repeated ?-names intern to one record (name-based identity).

Parameters:
  expr : any
Returns: pattern (sexpr possibly containing <pattern-var> records)
Category: algebra
Keywords: pattern, parse, match, unification"
  (let ((interned '()))
    (define (var-name-of sym)
      (let* ((s (symbol->string sym))
             (n (string-length s)))
        (and (> n 1)
             (char=? #\? (string-ref s 0))
             (string->symbol (substring s 1 n)))))
    (define (walk x)
      (cond
        ((pair? x)
         (cons (walk (car x)) (walk (cdr x))))
        ((null? x) '())
        ((symbol? x)
         (let ((nm (var-name-of x)))
           (cond
             ((not nm) x)
             ((assq nm interned) => cdr)
             (else
              (let ((v (make-pattern-var nm)))
                (set! interned (cons (cons nm v) interned))
                v)))))
        (else x)))
    (walk expr)))
