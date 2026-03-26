# (wile algebra rewrite) Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Build a general-purpose equational term rewriting library in wile's
`(wile algebra)`, then migrate wile-goast's `ssa-normalize` to use it.

**Architecture:** Term protocol (user-provided accessors) + first-class axiom
values (identity, commutativity, absorbing, idempotence, involution) + normalizer
that compiles axioms into rewrite rules via the protocol. Theories are plain lists,
`append` composes them.

**Design refinements over design doc:**
- **`make-term: (term, new-operands) → term`** — receives the original term so
  the rebuilder can preserve metadata (names, types, positions) the algebra
  doesn't touch. The protocol destructures via `get-operator`/`get-operands` and
  reconstructs via `make-term`; reconstruction needs the concrete term because
  the algebraic projection is lossy.
- **Element predicates** — `identity-axiom` and `absorbing-axiom` element fields
  are predicates `(value → boolean)`, not literal values. Consumers use
  domain-specific matching (e.g., `constant-zero?` for SSA constants
  `"0"`, `"0:int"`, `"0:uint64"`) instead of `equal?`.
- **`make-normalizer` returns `#f`** on no-match (not the original term).
  Composable as a rule; consumers wrap with `(or (normalizer term) term)`.
- **`validate-theory` deferred** — the design doc's sketch was a no-op.

**Tech Stack:** Scheme (R7RS), `(wile algebra rewrite)` in wile stdlib,
consumed by `(wile goast ssa-normalize)` in wile-goast.

**Two phases:** Phase 1 builds the library in `wile/`. Phase 2 migrates wile-goast.

---

## Phase 1: Build (wile algebra rewrite) in wile

### Task 1: Term protocol + identity axiom + make-normalizer skeleton

**Files:**
- Create: `wile/stdlib/lib/wile/algebra/rewrite.sld`
- Create: `wile/stdlib/lib/wile/algebra/rewrite.scm`
- Test: `wile/engine_stdlib_test.go`

**Step 1: Write the test**

Add to `wile/engine_stdlib_test.go`:

```go
func TestAlgebraRewrite_IdentityAxiom(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx,
		wile.WithSafeExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	// Term protocol for simple (op left right) lists
	result, err := eng.EvalMultiple(ctx, `
		(import (wile algebra rewrite))

		(define proto
		  (make-term-protocol
		    car                             ;; get-operator
		    cdr                             ;; get-operands
		    (lambda (term new-args)         ;; make-term: term × new-operands → term
		      (cons (car term) new-args))
		    (lambda (a b) (string<? (symbol->string a) (symbol->string b))))) ;; compare

		(define theory (list (identity-axiom '+ (lambda (x) (eq? x 'zero)))))
		(define normalize (make-normalizer theory proto))

		;; (+ x zero) → x
		(normalize '(+ x zero))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "x")
}

func TestAlgebraRewrite_IdentityAxiomLeftZero(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx,
		wile.WithSafeExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	result, err := eng.EvalMultiple(ctx, `
		(import (wile algebra rewrite))

		(define proto
		  (make-term-protocol car cdr
		    (lambda (term new-args) (cons (car term) new-args))
		    (lambda (a b) (string<? (symbol->string a) (symbol->string b)))))

		(define normalize
		  (make-normalizer (list (identity-axiom '+ (lambda (x) (eq? x 'zero)))) proto))

		;; (+ zero x) → x
		(normalize '(+ zero x))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "x")
}

func TestAlgebraRewrite_NoMatchReturnsFalse(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx,
		wile.WithSafeExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	result, err := eng.EvalMultiple(ctx, `
		(import (wile algebra rewrite))

		(define proto
		  (make-term-protocol car cdr
		    (lambda (term new-args) (cons (car term) new-args))
		    (lambda (a b) (string<? (symbol->string a) (symbol->string b)))))

		(define normalize
		  (make-normalizer (list (identity-axiom '+ (lambda (x) (eq? x 'zero)))) proto))

		;; (+ a b) — no zero operand, returns #f
		(normalize '(+ a b))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "#f")
}
```

**Step 2: Run tests to verify they fail**

Run (in wile/): `go test . -run TestAlgebraRewrite -v`
Expected: FAIL — `(wile algebra rewrite)` not found.

**Step 3: Create the library**

`wile/stdlib/lib/wile/algebra/rewrite.sld`:

```scheme
(define-library (wile algebra rewrite)
  (export
    ;; Term protocol
    make-term-protocol term-protocol?
    term-get-operator term-get-operands term-make-term term-compare
    ;; Axioms
    identity-axiom identity-axiom?
    commutativity-axiom commutativity-axiom?
    absorbing-axiom absorbing-axiom?
    idempotence-axiom idempotence-axiom?
    involution-axiom involution-axiom?
    axiom?
    ;; Normalizer
    make-normalizer)
  (include "rewrite.scm"))
```

`wile/stdlib/lib/wile/algebra/rewrite.scm`:

```scheme
;;; (wile algebra rewrite) — Equational term rewriting from algebraic axioms
;;;
;;; Generates rewrite rules from declared axioms (identity, commutativity,
;;; absorbing, idempotence, involution). Rules are compiled via a term
;;; protocol that abstracts over concrete term representations.
;;;
;;; Two design choices that matter:
;;;   1. Element matching uses predicates (value → boolean), not equal?.
;;;   2. make-term receives (term, new-operands) → term. The term carries
;;;      metadata the algebra doesn't touch; the rebuilder needs it.

;; ─── Term protocol ──────────────────────────

(define-record-type <term-protocol>
  (make-term-protocol get-operator get-operands make-term compare)
  term-protocol?
  (get-operator  term-get-operator)   ;; term → symbol
  (get-operands  term-get-operands)   ;; term → (list operand ...)
  (make-term     term-make-term)      ;; (term, new-operands) → term
  (compare       term-compare))       ;; (a, b) → boolean  (less-than)

;; ─── Axiom types ────────────────────────────

(define-record-type <identity-axiom>
  (identity-axiom op element)
  identity-axiom?
  (op      identity-axiom-op)
  (element identity-axiom-element))   ;; predicate: value → boolean

(define-record-type <commutativity-axiom>
  (commutativity-axiom op)
  commutativity-axiom?
  (op commutativity-axiom-op))

(define-record-type <absorbing-axiom>
  (absorbing-axiom op element)
  absorbing-axiom?
  (op      absorbing-axiom-op)
  (element absorbing-axiom-element))  ;; predicate: value → boolean

(define-record-type <idempotence-axiom>
  (idempotence-axiom op)
  idempotence-axiom?
  (op idempotence-axiom-op))

(define-record-type <involution-axiom>
  (involution-axiom op)
  involution-axiom?
  (op involution-axiom-op))

(define (axiom? x)
  (or (identity-axiom? x)
      (commutativity-axiom? x)
      (absorbing-axiom? x)
      (idempotence-axiom? x)
      (involution-axiom? x)))

;; ─── Axiom → rewrite rules ─────────────────

(define (axiom->rules axiom proto)
  ;; Returns a list of rewrite rule functions: (term → value-or-#f)
  (let ((get-op   (term-get-operator proto))
        (get-args (term-get-operands proto))
        (mk-term  (term-make-term proto))
        (lt?      (term-compare proto)))
    (cond
      ((identity-axiom? axiom)
       (let ((target-op (identity-axiom-op axiom))
             (e?        (identity-axiom-element axiom)))
         (list
           ;; op(x, e) → x
           (lambda (term)
             (let ((op (get-op term)) (args (get-args term)))
               (and (equal? op target-op)
                    (= (length args) 2)
                    (e? (cadr args))
                    (car args))))
           ;; op(e, x) → x
           (lambda (term)
             (let ((op (get-op term)) (args (get-args term)))
               (and (equal? op target-op)
                    (= (length args) 2)
                    (e? (car args))
                    (cadr args)))))))

      ((commutativity-axiom? axiom)
       (let ((target-op (commutativity-axiom-op axiom)))
         (list
           ;; op(x, y) → op(y, x) when y < x
           (lambda (term)
             (let ((op (get-op term)) (args (get-args term)))
               (and (equal? op target-op)
                    (= (length args) 2)
                    (lt? (cadr args) (car args))
                    (mk-term term (list (cadr args) (car args)))))))))

      ((absorbing-axiom? axiom)
       (let ((target-op (absorbing-axiom-op axiom))
             (z?        (absorbing-axiom-element axiom)))
         (list
           ;; op(x, z) → z
           (lambda (term)
             (let ((op (get-op term)) (args (get-args term)))
               (and (equal? op target-op)
                    (= (length args) 2)
                    (z? (cadr args))
                    (cadr args))))
           ;; op(z, x) → z
           (lambda (term)
             (let ((op (get-op term)) (args (get-args term)))
               (and (equal? op target-op)
                    (= (length args) 2)
                    (z? (car args))
                    (car args)))))))

      ((idempotence-axiom? axiom)
       (let ((target-op (idempotence-axiom-op axiom)))
         (list
           ;; op(x, x) → x
           (lambda (term)
             (let ((op (get-op term)) (args (get-args term)))
               (and (equal? op target-op)
                    (= (length args) 2)
                    (equal? (car args) (cadr args))
                    (car args)))))))

      ((involution-axiom? axiom)
       (let ((target-op (involution-axiom-op axiom)))
         (list
           ;; op(op(x)) → x  (unary: single operand)
           (lambda (term)
             (let ((op (get-op term)) (args (get-args term)))
               (and (equal? op target-op)
                    (= (length args) 1)
                    (let ((inner (car args)))
                      (and (pair? inner)
                           (equal? (get-op inner) target-op)
                           (= (length (get-args inner)) 1)
                           (car (get-args inner))))))))))

      (else '()))))

;; ─── Normalizer ─────────────────────────────

(define (make-normalizer theory proto)
  ;; Compile all axioms into a flat list of rewrite rules.
  ;; Returns (term → value-or-#f): first match wins, #f if none.
  (let ((rules (apply append
                 (map (lambda (ax) (axiom->rules ax proto)) theory))))
    (lambda (term)
      (let try ((rs rules))
        (if (null? rs) #f
          (let ((result ((car rs) term)))
            (if result result
              (try (cdr rs)))))))))
```

**Step 4: Run tests to verify they pass**

Run (in wile/): `go test . -run TestAlgebraRewrite -v`
Expected: PASS

**Step 5: Commit (in wile)**

```
git add stdlib/lib/wile/algebra/rewrite.sld stdlib/lib/wile/algebra/rewrite.scm engine_stdlib_test.go
git commit -m "feat(algebra): add (wile algebra rewrite) term rewriting library"
```

---

### Task 2: Commutativity, absorbing, idempotence, involution tests

**Files:**
- Modify: `wile/engine_stdlib_test.go`

**Step 1: Add tests for remaining axioms**

```go
func TestAlgebraRewrite_Commutativity(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx,
		wile.WithSafeExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	result, err := eng.EvalMultiple(ctx, `
		(import (wile algebra rewrite))
		(define proto
		  (make-term-protocol car cdr
		    (lambda (term new-args) (cons (car term) new-args))
		    (lambda (a b) (string<? (symbol->string a) (symbol->string b)))))
		(define normalize (make-normalizer (list (commutativity-axiom '+)) proto))
		;; (+ y a) → (+ a y) because a < y
		(normalize '(+ y a))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "(+ a y)")
}

func TestAlgebraRewrite_CommutativityAlreadyOrdered(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx,
		wile.WithSafeExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	result, err := eng.EvalMultiple(ctx, `
		(import (wile algebra rewrite))
		(define proto
		  (make-term-protocol car cdr
		    (lambda (term new-args) (cons (car term) new-args))
		    (lambda (a b) (string<? (symbol->string a) (symbol->string b)))))
		(define normalize (make-normalizer (list (commutativity-axiom '+)) proto))
		;; (+ a y) — already ordered, returns #f
		(normalize '(+ a y))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "#f")
}

func TestAlgebraRewrite_Absorbing(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx,
		wile.WithSafeExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	result, err := eng.EvalMultiple(ctx, `
		(import (wile algebra rewrite))
		(define proto
		  (make-term-protocol car cdr
		    (lambda (term new-args) (cons (car term) new-args))
		    (lambda (a b) (string<? (symbol->string a) (symbol->string b)))))
		(define normalize
		  (make-normalizer (list (absorbing-axiom '* (lambda (x) (eq? x 'zero)))) proto))
		;; (* x zero) → zero
		(normalize '(* x zero))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "zero")
}

func TestAlgebraRewrite_Idempotence(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx,
		wile.WithSafeExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	result, err := eng.EvalMultiple(ctx, `
		(import (wile algebra rewrite))
		(define proto
		  (make-term-protocol car cdr
		    (lambda (term new-args) (cons (car term) new-args))
		    (lambda (a b) (string<? (symbol->string a) (symbol->string b)))))
		(define normalize (make-normalizer (list (idempotence-axiom '&)) proto))
		;; (& x x) → x
		(normalize '(& x x))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "x")
}

func TestAlgebraRewrite_Involution(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx,
		wile.WithSafeExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	result, err := eng.EvalMultiple(ctx, `
		(import (wile algebra rewrite))
		(define proto
		  (make-term-protocol car cdr
		    (lambda (term new-args) (cons (car term) new-args))
		    (lambda (a b) (string<? (symbol->string a) (symbol->string b)))))
		(define normalize (make-normalizer (list (involution-axiom '!)) proto))
		;; (! (! x)) → x
		(normalize '(! (! x)))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "x")
}

func TestAlgebraRewrite_ComposedTheory(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx,
		wile.WithSafeExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	result, err := eng.EvalMultiple(ctx, `
		(import (wile algebra rewrite))
		(define proto
		  (make-term-protocol car cdr
		    (lambda (term new-args) (cons (car term) new-args))
		    (lambda (a b) (string<? (symbol->string a) (symbol->string b)))))
		(define zero? (lambda (x) (eq? x 'zero)))
		(define theory
		  (list (identity-axiom '+ zero?)
		        (commutativity-axiom '+)
		        (absorbing-axiom '* zero?)))
		(define normalize (make-normalizer theory proto))
		;; Identity: (+ x zero) → x
		;; Absorbing: (* y zero) → zero
		;; Commutativity: (+ y a) → (+ a y)
		(list (normalize '(+ x zero))
		      (normalize '(* y zero))
		      (normalize '(+ y a)))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "(x zero (+ a y))")
}
```

**Step 2: Run tests**

Run (in wile/): `go test . -run TestAlgebraRewrite -v`
Expected: PASS (implementation from Task 1 already covers all axiom types)

**Step 3: Commit (in wile)**

```
git add engine_stdlib_test.go
git commit -m "test(algebra): add rewrite tests for all 5 axiom types"
```

---

### Task 3: Re-export from (wile algebra) umbrella + update stdlib manifest

**Files:**
- Modify: `wile/stdlib/lib/wile/algebra.sld`
- Modify: `wile/stdlib/stdlib_test.go`

**Step 1: Add re-export**

In `wile/stdlib/lib/wile/algebra.sld`, add to the imports and exports:

Add to imports:
```scheme
(wile algebra rewrite)
```

Add to exports:
```scheme
    ;; Rewriting
    make-term-protocol term-protocol?
    term-get-operator term-get-operands term-make-term term-compare
    identity-axiom identity-axiom?
    commutativity-axiom commutativity-axiom?
    absorbing-axiom absorbing-axiom?
    idempotence-axiom idempotence-axiom?
    involution-axiom involution-axiom?
    axiom?
    make-normalizer
```

**Step 2: Add to stdlib manifest test**

In `wile/stdlib/stdlib_test.go`, add to the `expected` list:

```go
"wile/algebra/rewrite.sld",
```

**Step 3: Run tests**

Run (in wile/): `go test ./stdlib/ -v && go test . -run TestAlgebraRewrite -v`
Expected: PASS

**Step 4: Version bump, commit, tag, push**

```bash
echo 'v1.9.10' > VERSION
git add stdlib/lib/wile/algebra.sld stdlib/lib/wile/algebra/rewrite.sld stdlib/lib/wile/algebra/rewrite.scm stdlib/stdlib_test.go engine_stdlib_test.go VERSION
git commit -m "feat(algebra): add (wile algebra rewrite) — equational term rewriting from algebraic axioms"
git tag v1.9.10
git push origin master v1.9.10
```

---

## Phase 2: Migrate wile-goast's ssa-normalize

### Task 4: Update wile dependency and rewrite ssa-normalize

**Files:**
- Modify: `wile-goast/go.mod` (via `go get`)
- Modify: `wile-goast/cmd/wile-goast/lib/wile/goast/ssa-normalize.sld`
- Modify: `wile-goast/cmd/wile-goast/lib/wile/goast/ssa-normalize.scm`

**Step 1: Update wile dependency**

Run: `GONOSUMDB=github.com/aalpar/wile GONOSUMCHECK=github.com/aalpar/wile go get github.com/aalpar/wile@v1.9.10 && go mod tidy`

**Step 2: Rewrite ssa-normalize.sld**

```scheme
(define-library (wile goast ssa-normalize)
  (export
    ssa-normalize
    ssa-rule-set
    ssa-rule-identity
    ssa-rule-commutative
    ssa-rule-annihilation)
  (import (wile goast utils)
          (wile algebra rewrite))
  (include "ssa-normalize.scm"))
```

**Step 3: Rewrite ssa-normalize.scm**

```scheme
;;; ssa-normalize.scm — algebraic normalization of SSA binop nodes
;;;
;;; Declares algebraic properties for SSA binary operators and delegates
;;; rule generation to (wile algebra rewrite). Type-scoped to integer
;;; types for identity/absorbing (IEEE 754 safety). Commutativity is
;;; type-agnostic.

;; ─── Domain predicates ──────────────────────

(define integer-types
  '("int" "int8" "int16" "int32" "int64"
    "uint" "uint8" "uint16" "uint32" "uint64" "uintptr"))

(define (integer-type? s)
  (and (string? s) (member s integer-types) #t))

;; SSA constant strings: "0", "0:int", "0:uint64", etc.
(define (constant-zero? s)
  (and (string? s)
       (or (string=? s "0")
           (and (> (string-length s) 1)
                (char=? (string-ref s 0) #\0)
                (char=? (string-ref s 1) #\:)))))

(define (constant-one? s)
  (and (string? s)
       (or (string=? s "1")
           (and (> (string-length s) 1)
                (char=? (string-ref s 0) #\1)
                (char=? (string-ref s 1) #\:)))))

;; ─── Term protocol for SSA binop nodes ──────

(define ssa-binop-protocol
  (make-term-protocol
    ;; get-operator
    (lambda (node) (nf node 'op))
    ;; get-operands
    (lambda (node) (list (nf node 'x) (nf node 'y)))
    ;; make-term: preserve all fields, replace operands
    (lambda (node new-args)
      (cons 'ssa-binop
            (map (lambda (pair)
                   (cond
                     ((eq? (car pair) 'x) (cons 'x (car new-args)))
                     ((eq? (car pair) 'y) (cons 'y (cadr new-args)))
                     ((eq? (car pair) 'operands)
                      (cons 'operands new-args))
                     (else pair)))
                 (cdr node))))
    ;; compare: lexicographic on strings, #f for non-strings
    (lambda (a b) (and (string? a) (string? b) (string<? a b)))))

;; ─── Theory declarations ────────────────────

(define int-identity-theory
  (list (identity-axiom '+ constant-zero?)
        (identity-axiom '* constant-one?)
        (identity-axiom '|\|| constant-zero?)
        (identity-axiom '^ constant-zero?)))

(define int-absorbing-theory
  (list (absorbing-axiom '* constant-zero?)
        (absorbing-axiom '& constant-zero?)))

(define comm-theory
  (list (commutativity-axiom '+)
        (commutativity-axiom '*)
        (commutativity-axiom '&)
        (commutativity-axiom '|\||)
        (commutativity-axiom '^)
        (commutativity-axiom '==)
        (commutativity-axiom '!=)))

;; ─── Normalizers from theories ──────────────

(define int-identity-rewrite
  (make-normalizer int-identity-theory ssa-binop-protocol))

(define int-absorbing-rewrite
  (make-normalizer int-absorbing-theory ssa-binop-protocol))

(define comm-rewrite
  (make-normalizer comm-theory ssa-binop-protocol))

;; ─── Public API (backward-compatible) ───────

;; Each legacy rule constructor returns (node → value-or-#f).
;; Type scoping is the consumer's responsibility — identity and
;; absorbing guard on integer type; commutativity is type-agnostic.

(define (ssa-rule-identity)
  (lambda (node)
    (and (tag? node 'ssa-binop)
         (integer-type? (nf node 'type))
         (int-identity-rewrite node))))

(define (ssa-rule-annihilation)
  (lambda (node)
    (and (tag? node 'ssa-binop)
         (integer-type? (nf node 'type))
         (int-absorbing-rewrite node))))

(define (ssa-rule-commutative)
  (lambda (node)
    (and (tag? node 'ssa-binop)
         (comm-rewrite node))))

;; Compose rules: first non-#f result wins
(define (ssa-rule-set . rules)
  (lambda (node)
    (let loop ((rs rules))
      (if (null? rs) #f
        (let ((result ((car rs) node)))
          (if result result
            (loop (cdr rs))))))))

;; Default rule set: identity → absorbing → commutativity
(define default-rules
  (ssa-rule-set
    (ssa-rule-identity)
    (ssa-rule-annihilation)
    (ssa-rule-commutative)))

;; Main entry point
(define ssa-normalize
  (case-lambda
    ((node) (or (default-rules node) node))
    ((node rules) (or (rules node) node))))
```

**Step 4: Run existing ssa-normalize tests**

Run: `go test ./goast/ -run TestSSANormalize -v`
Expected: PASS — all existing tests must produce identical results.

**Step 5: Run full CI**

Run: `make ci`
Expected: PASS

**Step 6: Commit**

```
git add go.mod go.sum cmd/wile-goast/lib/wile/goast/ssa-normalize.sld cmd/wile-goast/lib/wile/goast/ssa-normalize.scm
git commit -m "refactor(ssa-normalize): derive rules from (wile algebra rewrite) axiom declarations

Declare operators with algebraic properties (identity element, absorbing
element, commutativity) and generate rules via make-normalizer. SSA-specific
concerns (predicate-based constant matching, type scoping, metadata-preserving
term reconstruction) handled through the protocol and consumer wrappers.
Backward-compatible: ssa-rule-set, ssa-rule-identity, ssa-rule-commutative,
ssa-rule-annihilation all preserved."
```

---

### Task 5: Update documentation

**Files:**
- Modify: `wile-goast/TODO.md`
- Modify: `wile-goast/CLAUDE.md`

**Step 1: Update TODO.md**

Change the ssa-normalize item in C1 from "leave as-is" to done with the
algebra rewrite library reference.

**Step 2: Update CLAUDE.md**

Update the SSA Normalization section to note that `(wile algebra rewrite)`
provides the axiom-to-rule compilation, and `ssa-normalize` declares theories
via the term protocol.

**Step 3: Commit and push**

```
git add TODO.md CLAUDE.md
git commit -m "docs: update ssa-normalize migration status"
git push origin master
```

---

### Summary

| Task | Repo | Key change |
|------|------|------------|
| 1 | wile | Term protocol + predicate elements + make-normalizer (returns #f) |
| 2 | wile | Tests for all 5 axiom types + composed theory |
| 3 | wile | Re-export from umbrella, version bump v1.9.10, tag |
| 4 | wile-goast | ssa-normalize uses make-normalizer with SSA protocol + theories |
| 5 | wile-goast | Documentation updates |
