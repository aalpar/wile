;;; algebra-symbolic-test.scm — Symbolic algebra tests

(import (scheme base)
        (chibi test)
        (wile algebra rewrite)
        (wile algebra symbolic)
        (wile algebra monoid)
        (wile algebra group)
        (wile algebra semiring)
        (wile algebra ring)
        (wile algebra lattice)
        (wile algebra heyting)
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
    (test 4 (length (theory-axioms th)))
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
    (test 7 (length (theory-axioms th)))))

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

;; ─── group->theory ─────────────────────────

(test-group "group->theory-construction"
  (let* ((G (make-group + 0 -))
         (th (group->theory G '+ 'neg)))
    (test 3 (length (theory-axioms th)))
    (test "identity" (named-axiom-name (car (theory-axioms th))))
    (test "associativity" (named-axiom-name (cadr (theory-axioms th))))
    (test "inverse-involution" (named-axiom-name (caddr (theory-axioms th))))))

(test-group "group->theory-involution"
  (let* ((G (make-group + 0 -))
         (th (group->theory G '+ 'neg))
         (norm (make-recursive-normalizer th sym-proto)))
    (let-values (((result trace) (norm '(neg (neg x)))))
      (test 'x result)
      (test 1 (length trace)))))

;; ─── semiring->theory ──────────────────────

(test-group "semiring->theory-construction"
  (let* ((S (make-semiring + * 0 1))
         (th (semiring->theory S '+ '*)))
    (test 6 (length (theory-axioms th)))
    (test '(+ *) (theory-associative-ops th))))

(test-group "semiring->theory-absorbing"
  (let* ((S (make-semiring + * 0 1))
         (th (semiring->theory S '+ '*))
         (norm (make-recursive-normalizer th sym-proto)))
    ;; (* 0 y) → 0
    (let-values (((result trace) (norm '(* 0 y))))
      (test 0 result))))

(test-group "semiring->theory-identity"
  (let* ((S (make-semiring + * 0 1))
         (th (semiring->theory S '+ '*))
         (norm (make-recursive-normalizer th sym-proto)))
    ;; (+ x 0) → x
    (let-values (((result trace) (norm '(+ x 0))))
      (test 'x result))))

;; ─── ring->theory ──────────────────────────

(test-group "ring->theory-construction"
  (let* ((R (integer-ring))
         (th (ring->theory R '+ '* 'neg)))
    (test 7 (length (theory-axioms th)))))

(test-group "ring->theory-cross-rule-normalization"
  ;; (+ (* 0 y) (+ x 0)) → x  using absorbing(×) + identity(+)
  (let* ((R (integer-ring))
         (th (ring->theory R '+ '* 'neg))
         (norm (make-recursive-normalizer th sym-proto)))
    (let-values (((result trace) (norm '(+ (* 0 y) (+ x 0)))))
      (test 'x result)
      (test #t (> (length trace) 1)))))

;; ─── field->theory ─────────────────────────

(test-group "field->theory-construction"
  (let* ((F (rational-field))
         (th (field->theory F '+ '* 'neg 'recip)))
    (test 8 (length (theory-axioms th)))))

(test-group "field->theory-reciprocal-involution"
  (let* ((F (rational-field))
         (th (field->theory F '+ '* 'neg 'recip))
         (norm (make-recursive-normalizer th sym-proto)))
    (let-values (((result trace) (norm '(recip (recip x)))))
      (test 'x result)
      (test 1 (length trace)))))

;; ─── heyting->theory ───────────────────────

(test-group "heyting->theory-construction"
  (let* ((H (powerset-heyting '(a b c)))
         (th (heyting->theory H 'join 'meet)))
    (test 4 (length (theory-axioms th)))
    (test '(join meet) (theory-associative-ops th))))

(test-group "heyting->theory-absorption"
  (let* ((H (powerset-heyting '(a b c)))
         (th (heyting->theory H 'join 'meet))
         (norm (make-recursive-normalizer th sym-proto)))
    (let-values (((result trace) (norm '(join x (meet x y)))))
      (test 'x result))))

;; ─── discover-equivalences ──────────────────

(test-group "discover-equivalences-basic"
  ;; Boolean absorption: only one normal form expected
  (let* ((B (powerset-boolean '(x y z)))
         (th (boolean->theory B 'or 'and 'not))
         (equivs (discover-equivalences th sym-proto '(and x (or x y)))))
    ;; At least the full-theory result
    (test #t (> (length equivs) 0))
    ;; First result should be the fully-normalized form
    (test 'x (caar equivs))))

(test-group "discover-equivalences-dedup"
  ;; Same normal form should not appear twice
  (let* ((B (powerset-boolean '(x y z)))
         (th (boolean->theory B 'or 'and 'not))
         (equivs (discover-equivalences th sym-proto '(and x (or x y))))
         (forms (map car equivs)))
    (let check ((remaining forms))
      (unless (null? remaining)
        (test #f (member (car remaining) (cdr remaining)))
        (check (cdr remaining))))))

(test-group "discover-equivalences-directional-skip"
  ;; Associativity is directional — should not be explored as a
  ;; standalone sub-theory (would generate extra bracketings without
  ;; simplification). Verify by checking that all discovered forms
  ;; are simpler or equal, not just re-bracketed.
  (let* ((R (integer-ring))
         (th (ring->theory R '+ '* 'neg))
         (equivs (discover-equivalences th sym-proto '(+ (+ a b) c)))
         (forms (map car equivs)))
    ;; The full theory normalizes via associativity.
    ;; But associativity alone should NOT appear as a separate result
    ;; since it's directional. So we should not see multiple bracketings.
    (test #t (<= (length forms) 2))))

(test-group "discover-equivalences-already-normal"
  ;; An already-normal term should produce exactly one entry
  (let* ((B (powerset-boolean '(x y z)))
         (th (boolean->theory B 'or 'and 'not))
         (equivs (discover-equivalences th sym-proto 'x)))
    (test 1 (length equivs))
    (test 'x (caar equivs))))

(test-group "discover-equivalences-multiple-forms"
  ;; With identity only (no commutativity), (+ 0 x) normalizes to x.
  ;; With commutativity only, (+ 0 x) normalizes to (+ x 0) — different.
  ;; So we should get at least 2 distinct normal forms.
  (let* ((th (make-theory
               (list
                 (make-named-axiom "identity" "a + 0 = a"
                   (make-identity-axiom '+ (lambda (x) (eq? x 'zero))))
                 (make-named-axiom "commutativity" "a + b = b + a"
                   (make-commutativity-axiom '+)))
               '(+)))
         (equivs (discover-equivalences th sym-proto '(+ zero x))))
    (test #t (>= (length equivs) 2))))

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

(test-group "validation-projections"
  ;; group->theory with non-group
  (test-error (group->theory "not a group" '+ 'neg))
  ;; group->theory with non-symbol
  (test-error (group->theory (make-group + 0 -) "plus" 'neg))
  ;; semiring->theory with non-semiring
  (test-error (semiring->theory "not a semiring" '+ '*))
  ;; ring->theory with non-ring
  (test-error (ring->theory "not a ring" '+ '* 'neg))
  ;; field->theory with non-field
  (test-error (field->theory "not a field" '+ '* 'neg 'recip))
  ;; heyting->theory with non-heyting
  (test-error (heyting->theory "not a heyting" 'join 'meet)))

(test-group "validation-discover-equivalences"
  ;; non-theory
  (test-error (discover-equivalences "not a theory" sym-proto 'x))
  ;; non-protocol
  (let ((th (make-theory '() '())))
    (test-error (discover-equivalences th "not a proto" 'x))))

(test-group "discover-equivalences-custom-fuel"
  ;; Verify fuel parameter is forwarded
  (let* ((th (make-theory
               (list (make-named-axiom "identity" "a + 0 = a"
                       (make-identity-axiom '+ (lambda (x) (eq? x 'zero)))))
               '(+)))
         (equivs (discover-equivalences th sym-proto '(+ x zero) 50)))
    (test #t (>= (length equivs) 1))
    (test 'x (caar equivs))))

(test-group "symbolic-boolean-normalize — absorption"
  (let-values (((nf _trace) (symbolic-boolean-normalize '(and x (or x y)))))
    (test 'x nf))
  (let-values (((nf _trace) (symbolic-boolean-normalize '(or x (and x y)))))
    (test 'x nf)))

(test-group "symbolic-boolean-normalize — idempotence"
  (let-values (((nf _trace) (symbolic-boolean-normalize '(and x x))))
    (test 'x nf))
  (let-values (((nf _trace) (symbolic-boolean-normalize '(or x x))))
    (test 'x nf)))

(test-group "symbolic-boolean-normalize — involution"
  (let-values (((nf _trace) (symbolic-boolean-normalize '(not (not x)))))
    (test 'x nf)))

(test-group "symbolic-boolean-normalize — atom"
  (let-values (((nf _trace) (symbolic-boolean-normalize 'x)))
    (test 'x nf))
  (let-values (((nf _trace) (symbolic-boolean-normalize '(foo x y))))
    ;; Opaque non-Boolean compound passes through unchanged.
    (test '(foo x y) nf)))

(test-group "symbolic-boolean-normalize — trace is well-formed"
  (let-values (((_nf trace) (symbolic-boolean-normalize '(and x (or x y)))))
    (test #t (list? trace))
    (test #t (and (not (null? trace))
                  (rewrite-step? (car trace))))))

(test-group "symbolic-boolean-equivalent? — commutativity"
  (test #t (symbolic-boolean-equivalent? '(and a b) '(and b a)))
  (test #t (symbolic-boolean-equivalent? '(or x y) '(or y x))))

(test-group "symbolic-boolean-equivalent? — absorption-congruent"
  (test #t (symbolic-boolean-equivalent? '(and x (or x y)) 'x))
  (test #t (symbolic-boolean-equivalent? '(or x (and x y)) 'x)))

(test-group "symbolic-boolean-equivalent? — distinct terms"
  (test #f (symbolic-boolean-equivalent? '(and a b) '(or a b)))
  (test #f (symbolic-boolean-equivalent? '(and x y) '(and x z))))

(test-group "symbolic-boolean-equivalent? — double negation"
  (test #t (symbolic-boolean-equivalent? '(not (not x)) 'x)))

(test-group "symbolic-boolean-equivalent? — opaque atoms"
  ;; Non-(and/or/not) compounds are opaque — (calls "Lock") ≠ (calls "Unlock").
  (test #t (symbolic-boolean-equivalent?
             '(and (calls "Lock") (calls "Lock"))
             '(calls "Lock")))
  (test #f (symbolic-boolean-equivalent?
             '(calls "Lock") '(calls "Unlock"))))

;; ─── Free-Boolean: AC normalization + De Morgan + complement ──

(define (bn-normal t)
  (let-values (((nf _trace) (symbolic-boolean-normalize t)))
    nf))

(define (bn-terminated? t)
  ;; #t when normalization reached a fixpoint (no fuel-exhausted marker)
  (let-values (((_nf trace) (symbolic-boolean-normalize t)))
    (not (and (pair? trace)
              (fuel-exhausted-step? (list-ref trace (- (length trace) 1)))))))

(test-group "symbolic-boolean-normalize — AC termination regression"
  ;; Pairwise commutativity + associativity used to ping-pong forever on any
  ;; >=3-leaf AC tree (fuel-exhausted at 100). AC normalization sorts once and
  ;; reaches a fixpoint. De Morgan amplified the old bug by manufacturing flat
  ;; multi-leaf and/or trees from negated inputs.
  (test #t (bn-terminated? '(and (and a b) c)))
  (test #t (bn-terminated? '(and a (and b c))))
  (test #t (bn-terminated? '(or (or a b) (or c d))))
  (test #t (bn-terminated? '(not (or (or a b) (or c d))))))

(test-group "symbolic-boolean-normalize — complement and bounds"
  (test #f (bn-normal '(and x (not x))))          ; contradiction => bottom (#f)
  (test #t (bn-normal '(or x (not x))))           ; tautology    => top (#t)
  (test #f (bn-normal '(and a (and b (not a)))))  ; n-way contradiction => bottom
  (test #t (bn-normal '(not (and x (not x))))))   ; bound folds under not => top

(test-group "symbolic-boolean-normalize — De Morgan"
  (test '(or (not x) (not y)) (bn-normal '(not (and x y))))
  (test '(and (not x) (not y)) (bn-normal '(not (or x y)))))

(test-group "symbolic-boolean-equivalent? — De Morgan and complement"
  (test #t (symbolic-boolean-equivalent? '(not (and a b)) '(or (not a) (not b))))
  (test #t (symbolic-boolean-equivalent? '(not (or a b)) '(and (not a) (not b))))
  ;; TODO(you): write the assertion that validates the core design property —
  ;; that the AC complement-fold collapses EVERY shared contradiction to the same
  ;; bottom regardless of nesting or operand order, so the confluence gap that
  ;; the pairwise binary matcher left open is closed. Pick two structurally
  ;; different n-way contradictions and assert they are equivalent, e.g.
  ;;   (test #t (symbolic-boolean-equivalent?
  ;;              '(and a (and b (not a)))
  ;;              '(and (not p) (and p q))))
  ;; (Both normalize to #f.) This is the test that proves flatten-then-fold —
  ;; not a better pairwise comparator — is what made the normalizer confluent.
  )

(test-end)
(test-exit)
