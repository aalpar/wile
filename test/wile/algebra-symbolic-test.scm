;;; algebra-symbolic-test.scm — Symbolic algebra tests

(import (scheme base)
        (chibi test)
        (wile algebra rewrite)
        (wile algebra symbolic)
        (wile algebra monoid)
        (wile algebra lattice)
        (wile algebra boolean))

(test-begin "symbolic-algebra")

(test-group "named-axiom-construction"
  (let ((na (make-named-axiom "identity" "a + 0 = a"
              (make-identity-axiom '+ (lambda (x) (eq? x 'zero))))))
    (test #t (named-axiom? na))
    (test "identity" (named-axiom-name na))
    (test "a + 0 = a" (named-axiom-general-form na))
    (test #t (identity-axiom? (named-axiom-axiom na)))))

(test-group "theory-construction"
  (let* ((ax1 (make-named-axiom "identity" "a + 0 = a"
                (make-identity-axiom '+ (lambda (x) (eq? x 'zero)))))
         (ax2 (make-named-axiom "commutativity" "a + b = b + a"
                (make-commutativity-axiom '+)))
         (th (make-theory (list ax1 ax2) '(+))))
    (test #t (theory? th))
    (test 2 (length (theory-axioms th)))
    (test '(+) (theory-associative-ops th))))

(test-group "theory-filter"
  (let* ((ax1 (make-named-axiom "identity" "a + 0 = a"
                (make-identity-axiom '+ (lambda (x) (eq? x 'zero)))))
         (ax2 (make-named-axiom "commutativity" "a + b = b + a"
                (make-commutativity-axiom '+)))
         (th (make-theory (list ax1 ax2) '(+)))
         (filtered (theory-filter th '("identity"))))
    (test 1 (length (theory-axioms filtered)))))

(test-group "theory-exclude"
  (let* ((ax1 (make-named-axiom "identity" "a + 0 = a"
                (make-identity-axiom '+ (lambda (x) (eq? x 'zero)))))
         (ax2 (make-named-axiom "commutativity" "a + b = b + a"
                (make-commutativity-axiom '+)))
         (th (make-theory (list ax1 ax2) '(+)))
         (excluded (theory-exclude th '("identity"))))
    (test 1 (length (theory-axioms excluded)))
    (test "commutativity" (named-axiom-name (car (theory-axioms excluded))))))

(test-group "theory-prioritize"
  (let* ((ax1 (make-named-axiom "identity" "a + 0 = a"
                (make-identity-axiom '+ (lambda (x) (eq? x 'zero)))))
         (ax2 (make-named-axiom "commutativity" "a + b = b + a"
                (make-commutativity-axiom '+)))
         (th (make-theory (list ax1 ax2) '(+)))
         (prioritized (theory-prioritize th '("commutativity"))))
    (test "commutativity"
          (named-axiom-name (car (theory-axioms prioritized))))))

(test-group "theory-merge"
  (let* ((ax1 (make-named-axiom "id-plus" "a + 0 = a"
                (make-identity-axiom '+ (lambda (x) (eq? x 'zero)))))
         (ax2 (make-named-axiom "id-times" "a * 1 = a"
                (make-identity-axiom '* (lambda (x) (eq? x 'one)))))
         (th1 (make-theory (list ax1) '(+)))
         (th2 (make-theory (list ax2) '(*)))
         (merged (theory-merge th1 th2)))
    (test 2 (length (theory-axioms merged)))
    (test 2 (length (theory-associative-ops merged)))))

(test-group "rewrite-step-construction"
  (let ((s (make-rewrite-step "identity" "a + 0 = a" '(+ x zero) 'x)))
    (test #t (rewrite-step? s))
    (test "identity" (step-rule-name s))
    (test "a + 0 = a" (step-general-form s))
    (test '(+ x zero) (step-before s))
    (test 'x (step-after s))))

(test-group "sexp-term-protocol"
  (let ((proto (sexp-term-protocol
                 (lambda (a b)
                   (string<? (symbol->string a) (symbol->string b))))))
    (test #t (term-compound? proto '(+ a b)))
    (test #f (term-compound? proto 'x))
    (test '+ (term-get-operator proto '(+ a b)))
    (test '(a b) (term-get-operands proto '(+ a b)))
    (test '(+ c d) (term-make-term proto '(+ a b) '(c d)))
    (test #t (term-compare proto 'a 'b))
    (test #f (term-compare proto 'b 'a))))

(test-group "format-trace"
  (let* ((s1 (make-rewrite-step "absorption" "a & (a | b) = a"
               '(and x (or x y)) 'x))
         (trace (list s1))
         (output (format-trace trace)))
    (test 1 (length output))
    (test #t (string? (car output)))))

;; ─── Recursive normalizer ───────────────────

;; Compare for commutativity normalization: only compare symbols,
;; compound terms are always "greater" so they sort after atoms.
(define sym-proto
  (sexp-term-protocol
    (lambda (a b)
      (cond
        ((and (symbol? a) (symbol? b))
         (string<? (symbol->string a) (symbol->string b)))
        ((symbol? a) #t)   ; atom before compound
        ((symbol? b) #f)   ; compound after atom
        (else #f)))))

(test-group "recursive-normalizer-identity"
  (let* ((theory (make-theory
                   (list (make-named-axiom "identity" "a + 0 = a"
                           (make-identity-axiom '+ (lambda (x) (eq? x 'zero)))))
                   '(+)))
         (norm (make-recursive-normalizer theory sym-proto)))
    (let-values (((result trace) (norm '(+ x zero))))
      (test 'x result)
      (test 1 (length trace))
      (test "identity" (step-rule-name (car trace))))))

(test-group "recursive-normalizer-nested"
  (let* ((theory (make-theory
                   (list (make-named-axiom "identity" "a + 0 = a"
                           (make-identity-axiom '+ (lambda (x) (eq? x 'zero)))))
                   '(+)))
         (norm (make-recursive-normalizer theory sym-proto)))
    (let-values (((result trace) (norm '(+ (+ x zero) zero))))
      (test 'x result)
      (test 2 (length trace)))))

(test-group "recursive-normalizer-multi-rule"
  (let* ((theory (make-theory
                   (list (make-named-axiom "identity-plus" "a + 0 = a"
                           (make-identity-axiom '+ (lambda (x) (eq? x 'zero))))
                         (make-named-axiom "absorbing-times" "0 * a = 0"
                           (make-absorbing-axiom '* (lambda (x) (eq? x 'zero)))))
                   '(+ *)))
         (norm (make-recursive-normalizer theory sym-proto)))
    (let-values (((result trace) (norm '(+ (* zero y) (+ x zero)))))
      (test 'x result)
      (test #t (> (length trace) 1)))))

(test-group "recursive-normalizer-no-change"
  (let* ((theory (make-theory
                   (list (make-named-axiom "identity" "a + 0 = a"
                           (make-identity-axiom '+ (lambda (x) (eq? x 'zero)))))
                   '(+)))
         (norm (make-recursive-normalizer theory sym-proto)))
    (let-values (((result trace) (norm 'x)))
      (test 'x result)
      (test 0 (length trace)))))

(test-group "recursive-normalizer-boolean-absorption"
  (let* ((theory (make-theory
                   (list (make-named-axiom "absorption" "a ∧ (a ∨ b) = a"
                           (make-absorption-axiom 'and 'or)))
                   '()))
         (norm (make-recursive-normalizer theory sym-proto)))
    (let-values (((result trace) (norm '(and x (or x y)))))
      (test 'x result)
      (test 1 (length trace))
      (test "absorption" (step-rule-name (car trace))))))

;; ─── monoid->theory ─────────────────────────

(test-group "monoid->theory-construction"
  (let* ((M (make-monoid + 0))
         (th (monoid->theory M '+)))
    (test 2 (length (theory-axioms th)))
    (test "identity" (named-axiom-name (car (theory-axioms th))))
    (test "associativity" (named-axiom-name (cadr (theory-axioms th))))
    (test '(+) (theory-associative-ops th))))

(test-group "monoid->theory-normalization"
  (let* ((M (make-monoid + 0))
         (th (monoid->theory M '+))
         (norm (make-recursive-normalizer th sym-proto)))
    (let-values (((result trace) (norm '(+ x 0))))
      (test 'x result))))

;; ─── lattice->theory ────────────────────────

(test-group "lattice->theory-construction"
  (let* ((L (make-lattice max min 0 100 <=))
         (th (lattice->theory L 'join 'meet)))
    (test 10 (length (theory-axioms th)))
    (test '(join meet) (theory-associative-ops th))))

(test-group "lattice->theory-absorption"
  (let* ((L (make-lattice max min 0 100 <=))
         (th (lattice->theory L 'join 'meet))
         (norm (make-recursive-normalizer th sym-proto)))
    (let-values (((result trace) (norm '(join x (meet x y)))))
      (test 'x result))))

;; ─── boolean->theory ────────────────────────

(test-group "boolean->theory-construction"
  (let* ((B (powerset-boolean '(x y z)))
         (th (boolean->theory B 'or 'and 'not)))
    (test 11 (length (theory-axioms th)))))

(test-group "boolean->theory-absorption"
  (let* ((B (powerset-boolean '(x y z)))
         (th (boolean->theory B 'or 'and 'not))
         (norm (make-recursive-normalizer th sym-proto)))
    (let-values (((result trace) (norm '(and x (or x y)))))
      (test 'x result))))

(test-group "boolean->theory-involution"
  (let* ((B (powerset-boolean '(x y z)))
         (th (boolean->theory B 'or 'and 'not))
         (norm (make-recursive-normalizer th sym-proto)))
    (let-values (((result trace) (norm '(not (not x)))))
      (test 'x result))))

(test-group "boolean->theory-nested"
  (let* ((B (powerset-boolean '(x y z)))
         (th (boolean->theory B 'or 'and 'not))
         (norm (make-recursive-normalizer th sym-proto)))
    (let-values (((result trace) (norm '(or (and x (or x y)) (not (not z))))))
      (test '(or x z) result))))

(define (string-includes? s sub)
  (let ((slen (string-length s))
        (sublen (string-length sub)))
    (let loop ((i 0))
      (cond
        ((> (+ i sublen) slen) #f)
        ((string=? (substring s i (+ i sublen)) sub) #t)
        (else (loop (+ i 1)))))))

(test-group "boolean->theory-format-trace"
  (let* ((B (powerset-boolean '(x y z)))
         (th (boolean->theory B 'or 'and 'not))
         (norm (make-recursive-normalizer th sym-proto)))
    (let-values (((result trace) (norm '(and x (or x y)))))
      (let ((formatted (format-trace trace)))
        (test #t (> (length formatted) 0))
        (test #t (string-includes?
                   (apply string-append formatted)
                   "absorption"))))))

;; ─── Fuel exhaustion ────────────────────────

(test-group "recursive-normalizer-fuel-exhaustion"
  ;; Deeply nested identity: requires many steps, fuel=1 forces early stop
  (let* ((theory (make-theory
                   (list (make-named-axiom "identity" "a + 0 = a"
                           (make-identity-axiom '+ (lambda (x) (eq? x 'zero)))))
                   '(+)))
         (norm (make-recursive-normalizer theory sym-proto 1)))
    (let-values (((result trace) (norm '(+ (+ (+ x zero) zero) zero))))
      ;; Should not fully normalize — fuel too low
      (test #t (> (length trace) 0))
      ;; Last trace entry should be fuel-exhausted
      (let ((last-step (list-ref trace (- (length trace) 1))))
        (test #t (fuel-exhausted-step? last-step))))))

;; ─── format-trace with fuel exhaustion ────

(test-group "format-trace-fuel-exhaustion"
  ;; format-trace must handle fuel-exhausted steps (list sentinel, not string)
  (let* ((theory (make-theory
                   (list (make-named-axiom "identity" "a + 0 = a"
                           (make-identity-axiom '+ (lambda (x) (eq? x 'zero)))))
                   '(+)))
         (norm (make-recursive-normalizer theory sym-proto 1)))
    (let-values (((result trace) (norm '(+ (+ (+ x zero) zero) zero))))
      (let ((formatted (format-trace trace)))
        (test #t (> (length formatted) 0))
        ;; Every entry must be a string (was crashing before fix)
        (for-each (lambda (s) (test #t (string? s))) formatted)
        ;; Last entry should mention fuel exhaustion
        (test #t (string-includes?
                   (list-ref formatted (- (length formatted) 1))
                   "fuel exhausted"))))))

;; ─── Validation errors ───────────────────

(test-group "validation-make-recursive-normalizer"
  ;; Passing non-theory should error
  (test-error (make-recursive-normalizer "not a theory" sym-proto))
  ;; Passing non-protocol should error
  (let ((th (make-theory '() '())))
    (test-error (make-recursive-normalizer th "not a proto")))
  ;; Passing non-positive fuel should error
  (let ((th (make-theory '() '())))
    (test-error (make-recursive-normalizer th sym-proto 0))
    (test-error (make-recursive-normalizer th sym-proto -1))))

(test-group "validation-theory-combinators"
  ;; theory-filter with non-theory
  (test-error (theory-filter "not a theory" '("identity")))
  ;; theory-filter with symbol names instead of strings
  (let ((th (make-theory '() '())))
    (test-error (theory-filter th '(identity))))
  ;; theory-exclude with non-theory
  (test-error (theory-exclude "not a theory" '("identity")))
  ;; theory-prioritize with non-theory
  (test-error (theory-prioritize "not a theory" '("identity")))
  ;; theory-merge with non-theories
  (test-error (theory-merge "not a theory" (make-theory '() '())))
  (test-error (theory-merge (make-theory '() '()) "not a theory")))

(test-end)
(test-exit)
