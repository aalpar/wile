# Symbolic Algebra Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Connect the algebra library to the rewrite library via `(wile algebra symbolic)`, enabling recursive normalization of symbolic terms with traced explanations.

**Architecture:** Three layers — existing operational structures, existing single-step rewriter (extended with absorption + associativity axioms), and a new `(wile algebra symbolic)` library that bridges them with `->theory` projections, a recursive normalizer, and transformation tracing.

**Tech Stack:** R7RS Scheme (`.sld` / `.scm` library files), Go integration tests in `engine_stdlib_test.go`.

**Design doc:** `plans/2026-04-10-symbolic-algebra-design.md`

---

## Phase 1 — Foundation

### Task 1: Absorption axiom type in rewrite library

**Files:**
- Modify: `stdlib/lib/wile/algebra/rewrite.scm`
- Modify: `stdlib/lib/wile/algebra/rewrite.sld`
- Modify: `engine_stdlib_test.go`

**Step 1: Write the failing Go test**

Add to `engine_stdlib_test.go`, following the pattern of `TestEngine_EmbeddedStdlib_RewriteIdempotence`:

```go
func TestEngine_EmbeddedStdlib_RewriteAbsorptionRight(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx,
		wile.WithAllExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithSourceOS(),
		wile.WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	result, err := eng.EvalMultiple(ctx, `
		(import (wile algebra rewrite))
		(define proto
		  (make-term-protocol pair? car cdr
		    (lambda (term new-args) (cons (car term) new-args))
		    (lambda (a b) (string<? (symbol->string a) (symbol->string b)))))
		(define normalize
		  (make-normalizer (list (make-absorption-axiom 'and 'or)) proto))
		;; (and x (or x y)) → x
		(normalize '(and x (or x y)))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "x")
}
```

Add a second test for the symmetric case `(and (or x y) x) → x`.

Add a third test for no-match: `(and x y)` → `#f`.

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestEngine_EmbeddedStdlib_RewriteAbsorption ./...`
Expected: FAIL — `make-absorption-axiom` is not exported.

**Step 3: Implement absorption axiom**

In `rewrite.scm`, after the `<involution-axiom>` record definition (around line 90), add:

```scheme
(define-record-type <absorption-axiom>
  (make-absorption-axiom op-outer op-inner)
  absorption-axiom?
  (op-outer absorption-axiom-op-outer)
  (op-inner absorption-axiom-op-inner))
```

Update `axiom?` to include `absorption-axiom?`.

In `axiom->rules`, add a new `cond` clause for `absorption-axiom?`:

```scheme
((absorption-axiom? axiom)
 (let ((outer-op (absorption-axiom-op-outer axiom))
       (inner-op (absorption-axiom-op-inner axiom)))
   (list
     ;; op1(a, op2(a, b)) → a
     (lambda (term)
       (let ((op (term-get-operator proto term))
             (args (term-get-operands proto term)))
         (if (and (equal? op outer-op)
                  (= (length args) 2)
                  (term-compound? proto (cadr args))
                  (equal? (term-get-operator proto (cadr args)) inner-op)
                  (= (length (term-get-operands proto (cadr args))) 2)
                  (equal? (car args) (car (term-get-operands proto (cadr args)))))
             (car args)
             *no-match*)))
     ;; op1(op2(a, b), a) → a
     (lambda (term)
       (let ((op (term-get-operator proto term))
             (args (term-get-operands proto term)))
         (if (and (equal? op outer-op)
                  (= (length args) 2)
                  (term-compound? proto (car args))
                  (equal? (term-get-operator proto (car args)) inner-op)
                  (= (length (term-get-operands proto (car args))) 2)
                  (equal? (cadr args) (car (term-get-operands proto (car args)))))
             (cadr args)
             *no-match*))))))
```

In `rewrite.sld`, add `make-absorption-axiom` and `absorption-axiom?` to exports.

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestEngine_EmbeddedStdlib_RewriteAbsorption ./...`
Expected: PASS

**Step 5: Run full test suite**

Run: `make test`
Expected: All existing tests still pass.

---

### Task 2: Associativity axiom type + directional flag

**Files:**
- Modify: `stdlib/lib/wile/algebra/rewrite.scm`
- Modify: `stdlib/lib/wile/algebra/rewrite.sld`
- Modify: `engine_stdlib_test.go`

**Step 1: Write the failing Go test**

```go
func TestEngine_EmbeddedStdlib_RewriteAssociativity(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx,
		wile.WithAllExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithSourceOS(),
		wile.WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	result, err := eng.EvalMultiple(ctx, `
		(import (wile algebra rewrite))
		(define proto
		  (make-term-protocol pair? car cdr
		    (lambda (term new-args) (cons (car term) new-args))
		    (lambda (a b) (string<? (symbol->string a) (symbol->string b)))))
		(define normalize
		  (make-normalizer (list (make-associativity-axiom '+)) proto))
		;; (+ (+ a b) c) → (+ a (+ b c))
		(normalize '(+ (+ a b) c))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "(+ a (+ b c))")
}
```

Add a second test: already right-associated `(+ a (+ b c))` → `#f` (no match).

Add a third test for `directional-axiom?`:

```go
func TestEngine_EmbeddedStdlib_DirectionalAxiom(t *testing.T) {
	// ...
	result, err := eng.EvalMultiple(ctx, `
		(import (wile algebra rewrite))
		(list (directional-axiom? (make-associativity-axiom '+))
		      (directional-axiom? (make-identity-axiom '+ zero?)))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "(#t #f)")
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestEngine_EmbeddedStdlib_RewriteAssociativity ./...`
Expected: FAIL

**Step 3: Implement associativity axiom and directional predicate**

In `rewrite.scm`, after the `<absorption-axiom>` record (from Task 1), add:

```scheme
(define-record-type <associativity-axiom>
  (make-associativity-axiom op)
  associativity-axiom?
  (op associativity-axiom-op))

(define (directional-axiom? x)
  (associativity-axiom? x))
```

Update `axiom?` to include `associativity-axiom?`.

In `axiom->rules`, add a clause for `associativity-axiom?`:

```scheme
((associativity-axiom? axiom)
 (let ((target-op (associativity-axiom-op axiom)))
   (list
     ;; op(op(a, b), c) → op(a, op(b, c))
     (lambda (term)
       (let ((op (term-get-operator proto term))
             (args (term-get-operands proto term)))
         (if (and (equal? op target-op)
                  (= (length args) 2)
                  (term-compound? proto (car args))
                  (equal? (term-get-operator proto (car args)) target-op)
                  (= (length (term-get-operands proto (car args))) 2))
             (let ((inner-args (term-get-operands proto (car args))))
               (term-make-term proto term
                 (list (car inner-args)
                       (term-make-term proto (car args)
                         (list (cadr inner-args) (cadr args))))))
             *no-match*))))))
```

In `rewrite.sld`, add `make-associativity-axiom`, `associativity-axiom?`, and `directional-axiom?` to exports.

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestEngine_EmbeddedStdlib_RewriteAssociativity ./...`
Run: `go test -v -run TestEngine_EmbeddedStdlib_DirectionalAxiom ./...`
Expected: PASS

**Step 5: Run full test suite**

Run: `make test`
Expected: All tests pass.

---

### Task 3: Create `(wile algebra symbolic)` — record types and theory combinators

**Files:**
- Create: `stdlib/lib/wile/algebra/symbolic.sld`
- Create: `stdlib/lib/wile/algebra/symbolic.scm`
- Create: `test/wile/algebra-symbolic-test.scm`

**Step 1: Write the failing Scheme test**

Create `test/wile/algebra-symbolic-test.scm`:

```scheme
;;; algebra-symbolic-test.scm — Symbolic algebra tests

(import (scheme base)
        (chibi test)
        (wile algebra rewrite)
        (wile algebra symbolic))

(test-begin "symbolic-algebra")

;; ─── Named axioms ────────────────────────────

(test-group "named-axiom-construction"
  (let ((na (make-named-axiom "identity" "a + 0 = a"
              (make-identity-axiom '+ (lambda (x) (eq? x 'zero))))))
    (test #t (named-axiom? na))
    (test "identity" (named-axiom-name na))
    (test "a + 0 = a" (named-axiom-general-form na))
    (test #t (identity-axiom? (named-axiom-axiom na)))))

;; ─── Theory ──────────────────────────────────

(test-group "theory-construction"
  (let* ((ax1 (make-named-axiom "identity" "a + 0 = a"
                (make-identity-axiom '+ (lambda (x) (eq? x 'zero)))))
         (ax2 (make-named-axiom "commutativity" "a + b = b + a"
                (make-commutativity-axiom '+)))
         (th (make-theory (list ax1 ax2) '(+))))
    (test #t (theory? th))
    (test 2 (length (theory-axioms th)))
    (test '(+) (theory-associative-ops th))))

;; ─── Theory combinators ─────────────────────

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

(test-end "symbolic-algebra")
```

**Step 2: Run test to verify it fails**

Run: `make build && dist/$(uname -s | tr A-Z a-z)/$(uname -m)/wile --file test/wile/algebra-symbolic-test.scm`
Expected: FAIL — library `(wile algebra symbolic)` not found.

**Step 3: Create the library definition**

Create `stdlib/lib/wile/algebra/symbolic.sld`:

```scheme
(define-library (wile algebra symbolic)
  (description "Symbolic algebra: theory projections, recursive normalization, and transformation tracing.")
  (export
    ;; Named axioms
    make-named-axiom named-axiom?
    named-axiom-name named-axiom-general-form named-axiom-axiom
    ;; Theory
    make-theory theory?
    theory-axioms theory-associative-ops
    ;; Theory combinators
    theory-filter theory-exclude theory-prioritize theory-merge)
  (import (scheme base)
          (wile algebra rewrite))
  (include "symbolic.scm"))
```

Create `stdlib/lib/wile/algebra/symbolic.scm`:

```scheme
;;; (wile algebra symbolic) — Symbolic algebra over algebraic structures
;;;
;;; Bridges the operational algebra library (structures with closures)
;;; and the equational rewrite library (axiom-based term transformation).
;;; See plans/2026-04-10-symbolic-algebra-design.md for architecture.
;;;
;;; Three roles per algebraic structure:
;;;   1. Operational (closures)  — compute values, ground truth oracle
;;;   2. Equational  (axioms)    — drive the rewriter
;;;   3. Explanatory (metadata)  — name the law, explain transformations

;; ─── Named axioms ──────────────────────────────

(define-record-type <named-axiom>
  (make-named-axiom name general-form axiom)
  named-axiom?
  (name         named-axiom-name)
  (general-form named-axiom-general-form)
  (axiom        named-axiom-axiom))

;; ─── Theory ────────────────────────────────────

(define-record-type <theory>
  (make-theory axioms associative-ops)
  theory?
  (axioms          theory-axioms)
  (associative-ops theory-associative-ops))

;; ─── Theory combinators ────────────────────────

(define (theory-filter theory rule-names)
  "Keep only the named axioms whose names appear in RULE-NAMES.

Parameters:
  theory : any
  rule-names : list
Returns: any
Category: algebra"
  (make-theory
    (filter (lambda (na)
              (member (named-axiom-name na) rule-names))
            (theory-axioms theory))
    (theory-associative-ops theory)))

(define (theory-exclude theory rule-names)
  "Drop named axioms whose names appear in RULE-NAMES.

Parameters:
  theory : any
  rule-names : list
Returns: any
Category: algebra"
  (make-theory
    (filter (lambda (na)
              (not (member (named-axiom-name na) rule-names)))
            (theory-axioms theory))
    (theory-associative-ops theory)))

(define (theory-prioritize theory rule-names)
  "Move named axioms matching RULE-NAMES to the front, preserving
relative order of the rest.

Parameters:
  theory : any
  rule-names : list
Returns: any
Category: algebra"
  (let* ((axioms (theory-axioms theory))
         (front (filter (lambda (na)
                          (member (named-axiom-name na) rule-names))
                        axioms))
         (rest (filter (lambda (na)
                         (not (member (named-axiom-name na) rule-names)))
                       axioms)))
    (make-theory (append front rest)
                 (theory-associative-ops theory))))

(define (theory-merge theory1 theory2)
  "Combine two theories, concatenating axioms and associative-ops.

Parameters:
  theory1 : any
  theory2 : any
Returns: any
Category: algebra"
  (make-theory
    (append (theory-axioms theory1) (theory-axioms theory2))
    (append (theory-associative-ops theory1)
            (theory-associative-ops theory2))))
```

**Step 4: Run test to verify it passes**

Run: `make build && dist/$(uname -s | tr A-Z a-z)/$(uname -m)/wile --file test/wile/algebra-symbolic-test.scm`
Expected: PASS

**Step 5: Run full test suite**

Run: `make test`
Expected: All tests pass.

---

### Task 4: Rewrite step trace and sexp-term-protocol

**Files:**
- Modify: `stdlib/lib/wile/algebra/symbolic.scm`
- Modify: `stdlib/lib/wile/algebra/symbolic.sld`
- Modify: `test/wile/algebra-symbolic-test.scm`

**Step 1: Write the failing Scheme test**

Append to `test/wile/algebra-symbolic-test.scm` (before `test-end`):

```scheme
;; ─── Rewrite steps ──────────────────────────

(test-group "rewrite-step-construction"
  (let ((s (make-rewrite-step "identity" "a + 0 = a" '(+ x zero) 'x)))
    (test #t (rewrite-step? s))
    (test "identity" (step-rule-name s))
    (test "a + 0 = a" (step-general-form s))
    (test '(+ x zero) (step-before s))
    (test 'x (step-after s))))

;; ─── sexp-term-protocol ─────────────────────

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

;; ─── format-trace ───────────────────────────

(test-group "format-trace"
  (let* ((s1 (make-rewrite-step "absorption" "a & (a | b) = a"
               '(and x (or x y)) 'x))
         (trace (list s1))
         (output (format-trace trace)))
    (test 1 (length output))
    (test #t (string? (car output)))))
```

**Step 2: Run test to verify it fails**

Run: `make build && dist/$(uname -s | tr A-Z a-z)/$(uname -m)/wile --file test/wile/algebra-symbolic-test.scm`
Expected: FAIL — `make-rewrite-step` not exported.

**Step 3: Implement**

Add to `symbolic.scm`:

```scheme
;; ─── Rewrite steps ─────────────────────────────

(define-record-type <rewrite-step>
  (make-rewrite-step rule-name general-form before after)
  rewrite-step?
  (rule-name    step-rule-name)
  (general-form step-general-form)
  (before       step-before)
  (after        step-after))

;; ─── Standard S-expression term protocol ───────

(define (sexp-term-protocol compare)
  "Construct a term protocol for S-expression terms.
Compound terms are pairs (op arg ...). Atoms are leaves.
COMPARE orders atoms for commutativity normalization.

Parameters:
  compare : procedure
Returns: any
Category: algebra"
  (make-term-protocol
    pair?
    car
    cdr
    (lambda (term new-args)
      (cons (car term) new-args))
    compare))

;; ─── Trace formatting ─────────────────────────

(define (display-to-string val)
  (let ((port (open-output-string)))
    (display val port)
    (get-output-string port)))

(define (format-trace trace)
  "Format a list of rewrite steps as human-readable strings.

Parameters:
  trace : list
Returns: list
Category: algebra"
  (map (lambda (step)
         (string-append
           (step-rule-name step)
           " (" (step-general-form step) "): "
           (display-to-string (step-before step))
           " → "
           (display-to-string (step-after step))))
       trace))
```

Add to `symbolic.sld` exports:

```scheme
    ;; Rewrite steps
    make-rewrite-step rewrite-step?
    step-rule-name step-general-form step-before step-after
    ;; Term protocol
    sexp-term-protocol
    ;; Reporter
    format-trace
```

**Step 4: Run test to verify it passes**

Expected: PASS

**Step 5: Run full test suite**

Run: `make test`
Expected: All tests pass.

---

### Task 5: Recursive normalizer

**Files:**
- Modify: `stdlib/lib/wile/algebra/symbolic.scm`
- Modify: `stdlib/lib/wile/algebra/symbolic.sld`
- Modify: `test/wile/algebra-symbolic-test.scm`

**Step 1: Write the failing Scheme tests**

Append to `test/wile/algebra-symbolic-test.scm` (before `test-end`):

```scheme
;; ─── Recursive normalizer ───────────────────

(define sym-proto
  (sexp-term-protocol
    (lambda (a b) (string<? (symbol->string a) (symbol->string b)))))

(test-group "recursive-normalizer-identity"
  ;; Single step: (+ x zero) → x
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
  ;; Nested: (+ (+ x zero) zero) → (+ x zero) → x
  (let* ((theory (make-theory
                   (list (make-named-axiom "identity" "a + 0 = a"
                           (make-identity-axiom '+ (lambda (x) (eq? x 'zero)))))
                   '(+)))
         (norm (make-recursive-normalizer theory sym-proto)))
    (let-values (((result trace) (norm '(+ (+ x zero) zero))))
      (test 'x result)
      (test 2 (length trace)))))

(test-group "recursive-normalizer-multi-rule"
  ;; Absorbing + identity: (+ (* zero y) (+ x zero)) → (+ zero x) → x
  (let* ((theory (make-theory
                   (list (make-named-axiom "identity-plus" "a + 0 = a"
                           (make-identity-axiom '+ (lambda (x) (eq? x 'zero))))
                         (make-named-axiom "absorbing-times" "0 * a = 0"
                           (make-absorbing-axiom '* (lambda (x) (eq? x 'zero)))))
                   '(+ *)))
         (norm (make-recursive-normalizer theory sym-proto)))
    (let-values (((result trace) (norm '(+ (* zero y) (+ x zero)))))
      (test 'x result)
      ;; Multiple steps across different rules
      (test #t (> (length trace) 1)))))

(test-group "recursive-normalizer-no-change"
  ;; Already normal: x → x, empty trace
  (let* ((theory (make-theory
                   (list (make-named-axiom "identity" "a + 0 = a"
                           (make-identity-axiom '+ (lambda (x) (eq? x 'zero)))))
                   '(+)))
         (norm (make-recursive-normalizer theory sym-proto)))
    (let-values (((result trace) (norm 'x)))
      (test 'x result)
      (test 0 (length trace)))))

(test-group "recursive-normalizer-boolean-absorption"
  ;; Boolean: (and x (or x y)) → x
  (let* ((theory (make-theory
                   (list (make-named-axiom "absorption" "a ∧ (a ∨ b) = a"
                           (make-absorption-axiom 'and 'or)))
                   '()))
         (norm (make-recursive-normalizer theory sym-proto)))
    (let-values (((result trace) (norm '(and x (or x y)))))
      (test 'x result)
      (test 1 (length trace))
      (test "absorption" (step-rule-name (car trace))))))
```

**Step 2: Run test to verify it fails**

Expected: FAIL — `make-recursive-normalizer` not exported.

**Step 3: Implement the recursive normalizer**

Add to `symbolic.scm`:

```scheme
;; ─── Recursive normalizer ──────────────────────

(define make-recursive-normalizer
  (case-lambda
    ((theory proto)
     (make-recursive-normalizer theory proto 100))
    ((theory proto fuel)
     (let ((step-normalize
             (make-normalizer
               (map named-axiom-axiom (theory-axioms theory))
               proto))
           (named-axioms (theory-axioms theory)))

       ;; Try each named axiom individually, return the first match
       ;; along with its metadata.
       (define (try-named-rules term)
         (let loop ((nas named-axioms))
           (if (null? nas) #f
             (let* ((na (car nas))
                    (rules (axiom->rules (named-axiom-axiom na) proto))
                    (result (let try ((rs rules))
                              (if (null? rs) #f
                                (let ((r ((car rs) term)))
                                  (if (eq? r *no-match*)
                                      (try (cdr rs))
                                      r))))))
               (if result
                   (cons na result)  ;; (named-axiom . rewritten-term)
                   (loop (cdr nas)))))))

       ;; Recursively normalize subterms bottom-up, then the root.
       (define (normalize-once term)
         (if (not (term-compound? proto term))
             (values term '())
             ;; 1. Normalize children first
             (let* ((op (term-get-operator proto term))
                    (args (term-get-operands proto term)))
               (let child-loop ((remaining args)
                                (done '())
                                (child-trace '()))
                 (if (null? remaining)
                     ;; 2. Rebuild with normalized children
                     (let* ((rebuilt (term-make-term proto term (reverse done)))
                            ;; 3. Try rules on rebuilt term
                            (hit (try-named-rules rebuilt)))
                       (if hit
                           (let ((na (car hit))
                                 (result (cdr hit)))
                             (values result
                                     (append child-trace
                                             (list (make-rewrite-step
                                                     (named-axiom-name na)
                                                     (named-axiom-general-form na)
                                                     rebuilt
                                                     result)))))
                           (values rebuilt child-trace)))
                     ;; Normalize next child
                     (let-values (((child-result child-steps)
                                   (normalize-once (car remaining))))
                       (child-loop (cdr remaining)
                                   (cons child-result done)
                                   (append child-trace child-steps))))))))

       ;; Fixed-point: keep normalizing until stable or fuel exhausted.
       (lambda (term)
         (let loop ((current term) (all-trace '()) (remaining fuel))
           (if (<= remaining 0)
               (values current (append all-trace
                                       (list (make-rewrite-step
                                               "fuel-exhausted" "" current current))))
               (let-values (((result trace) (normalize-once current)))
                 (if (null? trace)
                     (values result all-trace)  ;; stable
                     (loop result (append all-trace trace)
                           (- remaining (length trace))))))))))))
```

Note: this uses `*no-match*` from `(wile algebra rewrite)`. That sentinel is currently not exported. Add it to `rewrite.sld` exports, or use a different approach — call `make-normalizer` to build a single-step checker, and use `try-named-rules` with individual `axiom->rules` calls. The implementation above uses `axiom->rules` directly, which requires `*no-match*` to be accessible.

**Option: export `*no-match*` and `no-match?` from `rewrite.sld`.** This is the simplest path — add them to the export list. They are currently internal but stable.

Add `*no-match*` and `no-match?` to `rewrite.sld` exports.

Add `make-recursive-normalizer` to `symbolic.sld` exports. Also add `axiom->rules` to the `(wile algebra rewrite)` import in `symbolic.sld` if not already accessible (it is exported in `rewrite.sld`—check; if not, add it).

Check: `axiom->rules` is NOT currently exported from `rewrite.sld`. Add it to exports.

**Step 4: Run test to verify it passes**

Expected: PASS

**Step 5: Run full test suite**

Run: `make test`
Expected: All tests pass.

---

### Task 6: `monoid->theory` projection

**Files:**
- Modify: `stdlib/lib/wile/algebra/symbolic.scm`
- Modify: `stdlib/lib/wile/algebra/symbolic.sld`
- Modify: `test/wile/algebra-symbolic-test.scm`

**Step 1: Write the failing Scheme test**

Append to `test/wile/algebra-symbolic-test.scm` (before `test-end`):

```scheme
;; ─── Projections ─────────────────────────────

(test-group "monoid->theory"
  (let* ((M (make-monoid + 0))
         (th (monoid->theory M '+)))
    (test #t (theory? th))
    ;; identity + associativity = 2 axioms
    (test 2 (length (theory-axioms th)))
    (test '(+) (theory-associative-ops th))
    ;; Axiom names
    (test "identity"
          (named-axiom-name (car (theory-axioms th))))
    (test "associativity"
          (named-axiom-name (cadr (theory-axioms th))))))

(test-group "monoid->theory-normalizes"
  ;; Use monoid->theory to normalize (+ x 0) → x
  (let* ((M (make-monoid + 0))
         (th (monoid->theory M '+))
         (norm (make-recursive-normalizer th sym-proto)))
    (let-values (((result trace) (norm '(+ x 0))))
      (test 'x result)
      (test 1 (length trace)))))
```

Add `(wile algebra monoid)` to the imports in the test file.

**Step 2: Run test to verify it fails**

Expected: FAIL — `monoid->theory` not exported.

**Step 3: Implement**

Add to `symbolic.scm`:

```scheme
;; ─── Structure → theory projections ────────────
;;
;; Each projection takes a structure and the operator symbol(s) used
;; in the consumer's term language. The symbol bridges the structure's
;; anonymous closures to named operators in symbolic terms.

(define (monoid->theory M op-symbol)
  "Extract the equational theory of monoid M.
OP-SYMBOL is the operator name in the consumer's term language.
Produces: identity, associativity.

Parameters:
  M : any
  op-symbol : symbol
Returns: any
Category: algebra"
  (make-theory
    (list
      (make-named-axiom
        "identity"
        (string-append (symbol->string op-symbol) "(a, e) = a")
        (make-identity-axiom op-symbol
          (lambda (x) (equal? x (monoid-identity M)))))
      (make-named-axiom
        "associativity"
        (string-append (symbol->string op-symbol)
                       "(" (symbol->string op-symbol) "(a,b), c) = "
                       (symbol->string op-symbol) "(a, " (symbol->string op-symbol) "(b,c))")
        (make-associativity-axiom op-symbol)))
    (list op-symbol)))
```

Add `(wile algebra monoid)` to imports in `symbolic.sld`.
Add `monoid->theory` to exports in `symbolic.sld`.

**Step 4: Run test to verify it passes**

Expected: PASS

**Step 5: Run full test suite**

Run: `make test`
Expected: All tests pass.

---

### Task 7: `lattice->theory` projection

**Files:**
- Modify: `stdlib/lib/wile/algebra/symbolic.scm`
- Modify: `stdlib/lib/wile/algebra/symbolic.sld`
- Modify: `test/wile/algebra-symbolic-test.scm`

**Step 1: Write the failing Scheme test**

```scheme
(test-group "lattice->theory"
  (let* ((L (powerset-lattice '(x y z)))
         (th (lattice->theory L 'join 'meet)))
    (test #t (theory? th))
    ;; identity(join), identity(meet), commutativity(join), commutativity(meet),
    ;; idempotence(join), idempotence(meet), absorption(join/meet),
    ;; absorption(meet/join), associativity(join), associativity(meet) = 10
    (test 10 (length (theory-axioms th)))
    (test #t (member 'join (theory-associative-ops th)))
    (test #t (member 'meet (theory-associative-ops th)))))

(test-group "lattice->theory-absorption"
  ;; (join x (meet x y)) → x via absorption
  (let* ((L (powerset-lattice '(x y z)))
         (th (lattice->theory L 'join 'meet))
         (norm (make-recursive-normalizer th sym-proto)))
    (let-values (((result trace) (norm '(join x (meet x y)))))
      (test 'x result))))
```

Add `(wile algebra lattice)` to imports in the test file.

**Step 2: Run test to verify it fails**

Expected: FAIL

**Step 3: Implement**

Add to `symbolic.scm`:

```scheme
(define (lattice->theory L join-sym meet-sym)
  "Extract the equational theory of lattice L.
JOIN-SYM and MEET-SYM are operator names in the consumer's term language.
Produces: identity, commutativity, idempotence, absorption, associativity
for both join and meet.

Parameters:
  L : any
  join-sym : symbol
  meet-sym : symbol
Returns: any
Category: algebra"
  (let ((bot (lattice-bottom L))
        (top (lattice-top L))
        (js (symbol->string join-sym))
        (ms (symbol->string meet-sym)))
    (make-theory
      (list
        ;; Identity
        (make-named-axiom
          "join-identity"
          (string-append js "(a, ⊥) = a")
          (make-identity-axiom join-sym (lambda (x) (equal? x bot))))
        (make-named-axiom
          "meet-identity"
          (string-append ms "(a, ⊤) = a")
          (make-identity-axiom meet-sym (lambda (x) (equal? x top))))
        ;; Commutativity
        (make-named-axiom
          "join-commutativity"
          (string-append js "(a, b) = " js "(b, a)")
          (make-commutativity-axiom join-sym))
        (make-named-axiom
          "meet-commutativity"
          (string-append ms "(a, b) = " ms "(b, a)")
          (make-commutativity-axiom meet-sym))
        ;; Idempotence
        (make-named-axiom
          "join-idempotence"
          (string-append js "(a, a) = a")
          (make-idempotence-axiom join-sym))
        (make-named-axiom
          "meet-idempotence"
          (string-append ms "(a, a) = a")
          (make-idempotence-axiom meet-sym))
        ;; Absorption
        (make-named-axiom
          "absorption-join-meet"
          (string-append js "(a, " ms "(a, b)) = a")
          (make-absorption-axiom join-sym meet-sym))
        (make-named-axiom
          "absorption-meet-join"
          (string-append ms "(a, " js "(a, b)) = a")
          (make-absorption-axiom meet-sym join-sym))
        ;; Associativity
        (make-named-axiom
          "join-associativity"
          (string-append js "(" js "(a,b), c) = " js "(a, " js "(b,c))")
          (make-associativity-axiom join-sym))
        (make-named-axiom
          "meet-associativity"
          (string-append ms "(" ms "(a,b), c) = " ms "(a, " ms "(b,c))")
          (make-associativity-axiom meet-sym)))
      (list join-sym meet-sym))))
```

Add `(wile algebra lattice)` to imports in `symbolic.sld`.
Add `lattice->theory` to exports.

**Step 4: Run test to verify it passes**

Expected: PASS

**Step 5: Run full test suite**

Run: `make test`
Expected: All tests pass.

---

### Task 8: `boolean->theory` projection

**Files:**
- Modify: `stdlib/lib/wile/algebra/symbolic.scm`
- Modify: `stdlib/lib/wile/algebra/symbolic.sld`
- Modify: `test/wile/algebra-symbolic-test.scm`

**Step 1: Write the failing Scheme test**

```scheme
(test-group "boolean->theory"
  (let* ((B (powerset-boolean '(x y z)))
         (th (boolean->theory B 'or 'and 'not)))
    (test #t (theory? th))
    ;; lattice axioms (10) + involution(not) = 11
    (test 11 (length (theory-axioms th)))))

(test-group "boolean->theory-full-normalization"
  ;; (and x (or x y)) → x via absorption
  (let* ((B (powerset-boolean '(x y z)))
         (th (boolean->theory B 'or 'and 'not))
         (norm (make-recursive-normalizer th sym-proto)))
    (let-values (((result trace) (norm '(and x (or x y)))))
      (test 'x result))))

(test-group "boolean->theory-involution"
  ;; (not (not x)) → x via involution
  (let* ((B (powerset-boolean '(x y z)))
         (th (boolean->theory B 'or 'and 'not))
         (norm (make-recursive-normalizer th sym-proto)))
    (let-values (((result trace) (norm '(not (not x)))))
      (test 'x result)
      (test 1 (length trace)))))

(test-group "boolean->theory-nested"
  ;; (or (and x (or x y)) (not (not z))) → (or x z)
  (let* ((B (powerset-boolean '(x y z)))
         (th (boolean->theory B 'or 'and 'not))
         (norm (make-recursive-normalizer th sym-proto)))
    (let-values (((result trace) (norm '(or (and x (or x y)) (not (not z))))))
      (test '(or x z) result))))

(test-group "format-trace-end-to-end"
  ;; Full pipeline: normalize + trace + format
  (let* ((B (powerset-boolean '(x y z)))
         (th (boolean->theory B 'or 'and 'not))
         (norm (make-recursive-normalizer th sym-proto)))
    (let-values (((result trace) (norm '(and x (or x y)))))
      (let ((formatted (format-trace trace)))
        (test 1 (length formatted))
        (test #t (string? (car formatted)))
        ;; Check that "absorption" appears in the explanation
        (test #t (string-contains (car formatted) "absorption"))))))
```

Add `(wile algebra boolean)` to imports in the test file. Also add a helper `string-contains` if not already available (check the test file — the belief DSL has one, but Scheme tests may not).

**Step 2: Run test to verify it fails**

Expected: FAIL

**Step 3: Implement**

Add to `symbolic.scm`:

```scheme
(define (boolean->theory B join-sym meet-sym comp-sym)
  "Extract the equational theory of Boolean algebra B.
JOIN-SYM, MEET-SYM, and COMP-SYM are operator names in the
consumer's term language. Produces: lattice axioms (via
lattice->theory on the underlying lattice) + complement involution.

Parameters:
  B : any
  join-sym : symbol
  meet-sym : symbol
  comp-sym : symbol
Returns: any
Category: algebra"
  (let ((lat-theory (lattice->theory (boolean->lattice B) join-sym meet-sym)))
    (theory-merge
      lat-theory
      (make-theory
        (list
          (make-named-axiom
            "complement-involution"
            (string-append (symbol->string comp-sym)
                           "(" (symbol->string comp-sym) "(a)) = a")
            (make-involution-axiom comp-sym)))
        '()))))
```

Add `(wile algebra boolean)` to imports in `symbolic.sld`.
Add `boolean->theory` to exports.

**Step 4: Run test to verify it passes**

Expected: PASS

**Step 5: Run full test suite and lint**

Run: `make lint && make test`
Expected: All pass.

---

### Task 9: Go integration test — end-to-end symbolic normalization

**Files:**
- Modify: `engine_stdlib_test.go`

**Step 1: Write the Go integration test**

This tests the full pipeline from Go: import symbolic, build boolean theory, normalize, check result.

```go
func TestEngine_EmbeddedStdlib_SymbolicBooleanNormalization(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx,
		wile.WithAllExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithSourceOS(),
		wile.WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	result, err := eng.EvalMultiple(ctx, `
		(import (wile algebra boolean)
		        (wile algebra symbolic)
		        (wile algebra rewrite))

		(let* ((B (powerset-boolean '(x y z)))
		       (th (boolean->theory B 'or 'and 'not))
		       (proto (sexp-term-protocol
		                (lambda (a b)
		                  (string<? (symbol->string a) (symbol->string b)))))
		       (norm (make-recursive-normalizer th proto)))
		  (let-values (((result trace) (norm '(and x (or x y)))))
		    (list result (length trace) (step-rule-name (car trace)))))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, `(x 1 "absorption")`)
}
```

**Step 2: Run test**

Run: `go test -v -run TestEngine_EmbeddedStdlib_SymbolicBooleanNormalization ./...`
Expected: PASS (if all previous tasks are complete).

**Step 3: Run full suite including lint and covercheck**

Run: `make lint && make covercheck`
Expected: All pass. Phase 1 is complete.

---

## Phase 2 — Complete Projections + Equivalence Discovery

### Task 10: Remaining `->theory` projections

**Files:**
- Modify: `stdlib/lib/wile/algebra/symbolic.scm`
- Modify: `stdlib/lib/wile/algebra/symbolic.sld`
- Modify: `test/wile/algebra-symbolic-test.scm`

Implement in order, one test group + projection per commit:

1. `group->theory G op-symbol inv-symbol` — monoid axioms + involution(inverse)
2. `semiring->theory S plus-sym times-sym` — identity(+), identity(×), commutativity(+), absorbing(×,0), associativity(+), associativity(×)
3. `ring->theory R plus-sym times-sym neg-sym` — semiring axioms + involution(negate)
4. `field->theory F plus-sym times-sym neg-sym recip-sym` — ring axioms + involution(reciprocal)
5. `heyting->theory H join-sym meet-sym imp-sym` — lattice axioms (via lattice->theory)

Each projection follows the same pattern as `monoid->theory` and `boolean->theory`. Tests verify axiom count, axiom names, and at least one normalization.

Add `(wile algebra group)`, `(wile algebra semiring)`, `(wile algebra ring)`, `(wile algebra heyting)` to imports in `symbolic.sld` as needed.

---

### Task 11: `discover-equivalences`

**Files:**
- Modify: `stdlib/lib/wile/algebra/symbolic.scm`
- Modify: `stdlib/lib/wile/algebra/symbolic.sld`
- Modify: `test/wile/algebra-symbolic-test.scm`

**Implementation sketch:**

```scheme
(define (discover-equivalences theory proto term)
  "Find distinct normal forms by running TERM through single-rule
and combination theories. Returns a list of (normal-form . trace)
pairs, deduplicated by equal? on normal-form.

Parameters:
  theory : any
  proto : any
  term : any
Returns: list
Category: algebra"
  (let ((seen '())
        (results '()))
    (define (try-theory th)
      (let ((norm (make-recursive-normalizer th proto)))
        (let-values (((result trace) (norm term)))
          (unless (member result seen)
            (set! seen (cons result seen))
            (set! results (cons (cons result trace) results))))))
    ;; Full theory
    (try-theory theory)
    ;; Each non-directional single-rule theory
    (for-each
      (lambda (na)
        (unless (directional-axiom? (named-axiom-axiom na))
          (try-theory (make-theory (list na)
                                   (theory-associative-ops theory)))))
      (theory-axioms theory))
    (reverse results)))
```

Tests should verify:
- Multiple distinct normal forms are discovered
- Directional axioms don't produce extra bracketings
- Deduplication works (same normal form not reported twice)

---

### Task 12: `theory-merge` integration test

**Files:**
- Modify: `test/wile/algebra-symbolic-test.scm`

Test that `ring->theory` correctly merges additive and multiplicative sub-theories, and that normalization across both works:

```scheme
;; (+ (* 0 y) (+ x 0)) normalizes to x using both absorbing(×) and identity(+)
```

---

## Phase 3 — wile-goast Integration

Phase 3 tasks are in the wile-goast repository, not this one. They consume `(wile algebra symbolic)` as a dependency. Outline:

### Task 13: Boolean expression term protocol for Go AST

Create a term protocol in wile-goast that maps Go boolean operators (`&&`, `||`, `!`) to symbolic operators (`and`, `or`, `not`). SSA analysis extracts conditions; the term protocol projects them as S-expression terms for the normalizer.

### Task 14: Belief predicate symbolic representation

wile-goast scripts construct symbolic terms alongside operational belief predicates. Two beliefs normalizing to the same form are flagged as equivalent.

### Task 15: FCA algebraic annotation

Annotate `boundary-report` output with lattice relationships between concepts using `lattice->theory` projections.

---

## Verification

After each task:

```bash
make lint && make test
```

After Phase 1 complete:

```bash
make lint && make covercheck
```

The build is not clean until both pass.
