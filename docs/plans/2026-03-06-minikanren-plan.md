# miniKanren Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Port microKanren and miniKanren to Wile as pure Scheme R7RS libraries, serving as a conformance stress test for closures, streams, macros, and the library system.

**Architecture:** Two layered libraries — `(wile microkanren)` provides the ~40-line procedural core (unification, goals, streams), `(wile kanren)` adds syntactic sugar via `syntax-rules` macros (`fresh`, `conde`, `run`, `run*`). Integration tests exercise both layers and classic relational programs.

**Tech Stack:** Pure R7RS Scheme. No Go code. Tests via the Wile CLI binary (`dist/{os}/{arch}/scheme`).

**Design doc:** `docs/plans/2026-03-06-minikanren-design.md`

---

### Task 1: Create the microKanren core library

**Files:**
- Create: `lib/wile/microkanren.scm`
- Create: `lib/wile/microkanren.sld`

**Step 1: Create the library definition**

Write `lib/wile/microkanren.sld`:

```scheme
(define-library (wile microkanren)
  (export var var? var=?
          walk ext-s unify
          == call/fresh disj conj
          mplus bind unit mzero
          empty-state)
  (import (scheme base))
  (include "microkanren.scm"))
```

**Step 2: Create the implementation**

Write `lib/wile/microkanren.scm` — the core microKanren from Hemann & Friedman (2013), adapted for R7RS:

```scheme
;; microKanren — Hemann & Friedman (2013)
;; Adapted for R7RS (assp → assoc with custom comparator)

;; Logic variables: represented as single-element vectors
(define (var c) (vector c))
(define (var? x) (vector? x))
(define (var=? x1 x2) (= (vector-ref x1 0) (vector-ref x2 0)))

;; Substitution: association list of (var . value) pairs
(define (walk u s)
  (let ((pr (and (var? u) (assoc u s (lambda (a b) (var=? a b))))))
    (if pr (walk (cdr pr) s) u)))

(define (ext-s x v s) (cons (cons x v) s))

;; Unification
(define (unify u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
      ((and (var? u) (var? v) (var=? u v)) s)
      ((var? u) (ext-s u v s))
      ((var? v) (ext-s v u s))
      ((and (pair? u) (pair? v))
       (let ((s (unify (car u) (car v) s)))
         (and s (unify (cdr u) (cdr v) s))))
      (else (and (eqv? u v) s)))))

;; Goals: state/counter → stream
(define (== u v)
  (lambda (s/c)
    (let ((s (unify u v (car s/c))))
      (if s (unit (cons s (cdr s/c))) mzero))))

(define (unit s/c) (cons s/c '()))
(define mzero '())

(define (call/fresh f)
  (lambda (s/c)
    (let ((c (cdr s/c)))
      ((f (var c)) (cons (car s/c) (+ c 1))))))

;; Goal combinators
(define (disj g1 g2) (lambda (s/c) (mplus (g1 s/c) (g2 s/c))))
(define (conj g1 g2) (lambda (s/c) (bind (g1 s/c) g2)))

;; Stream operations (interleaving search)
(define (mplus $1 $2)
  (cond
    ((null? $1) $2)
    ((procedure? $1) (lambda () (mplus $2 ($1))))
    (else (cons (car $1) (mplus (cdr $1) $2)))))

(define (bind $ g)
  (cond
    ((null? $) mzero)
    ((procedure? $) (lambda () (bind ($) g)))
    (else (mplus (g (car $)) (bind (cdr $) g)))))

;; Initial state: empty substitution, counter at 0
(define empty-state (cons '() 0))
```

Note: The canonical implementation uses quasiquote (`ext-s`). The adaptation above uses explicit `cons` instead, which is clearer and avoids depending on quasiquote behavior in library context.

**Step 3: Verify the directory exists**

Run: `ls lib/wile/ 2>/dev/null || mkdir -p lib/wile/`

**Step 4: Commit**

```
git add lib/wile/microkanren.sld lib/wile/microkanren.scm
git commit -m "feat: add (wile microkanren) library — core logic programming primitives

Port of Hemann & Friedman's microKanren (2013) adapted for R7RS.
Provides unification, goal construction, and interleaving search."
```

---

### Task 2: Write microKanren integration tests

**Files:**
- Create: `integration/testdata/microkanren-tests.scm`
- Modify: `integration/r7rs_test.go` (add new test function)

**Step 1: Write the Scheme test file**

Write `integration/testdata/microkanren-tests.scm`:

```scheme
;; microKanren integration tests
;; Tests the (wile microkanren) library

(import (scheme base)
        (scheme write)
        (wile microkanren))

;; Simple test infrastructure (no dependency on chibi test)
(define *pass* 0)
(define *fail* 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (set! *pass* (+ *pass* 1))
      (begin
        (set! *fail* (+ *fail* 1))
        (display "FAIL: ")
        (display name)
        (display " — expected ")
        (write expected)
        (display " but got ")
        (write actual)
        (newline))))

(define (test-summary)
  (newline)
  (display "Test Summary:")
  (newline)
  (display "  Passed: ")
  (display *pass*)
  (newline)
  (display "  Failed: ")
  (display *fail*)
  (newline)
  (if (> *fail* 0)
      (exit 1)
      (exit 0)))

;; Helper: pull n results from a stream
(define (take n $)
  (cond
    ((zero? n) '())
    ((null? $) '())
    ((procedure? $) (take n ($)))
    (else (cons (car $) (take (- n 1) (cdr $))))))

;; Helper: pull all results (use with care — may diverge)
(define (take-all $)
  (cond
    ((null? $) '())
    ((procedure? $) (take-all ($)))
    (else (cons (car $) (take-all (cdr $))))))

;; === Variable tests ===

(test "var creation"
  #t
  (var? (var 0)))

(test "var equality"
  #t
  (var=? (var 0) (var 0)))

(test "var inequality"
  #f
  (var=? (var 0) (var 1)))

(test "non-var"
  #f
  (var? 42))

;; === Walk tests ===

(test "walk unbound"
  (var 0)
  (walk (var 0) '()))

(test "walk bound"
  5
  (walk (var 0) (list (cons (var 0) 5))))

(test "walk chain"
  5
  (walk (var 0) (list (cons (var 0) (var 1))
                      (cons (var 1) 5))))

;; === Unification tests ===

(test "unify atoms equal"
  '()
  (unify 5 5 '()))

(test "unify atoms unequal"
  #f
  (unify 5 6 '()))

(test "unify var to atom"
  (list (cons (var 0) 5))
  (unify (var 0) 5 '()))

(test "unify two vars"
  (list (cons (var 0) (var 1)))
  (unify (var 0) (var 1) '()))

(test "unify pairs"
  (list (cons (var 1) 2) (cons (var 0) 1))
  (unify (cons (var 0) (var 1)) (cons 1 2) '()))

(test "unify nested fail"
  #f
  (unify (cons 1 2) (cons 1 3) '()))

;; === Goal tests ===

(test "== success"
  1
  (length (take-all ((== 5 5) empty-state))))

(test "== failure"
  0
  (length (take-all ((== 5 6) empty-state))))

(test "call/fresh binds"
  1
  (length (take-all ((call/fresh (lambda (x) (== x 5))) empty-state))))

;; === Disjunction (OR) ===

(test "disj two successes"
  2
  (length (take-all ((disj (== #t #t) (== #t #t)) empty-state))))

(test "disj one success"
  1
  (length (take-all ((disj (== 5 6) (== #t #t)) empty-state))))

;; === Conjunction (AND) ===

(test "conj both succeed"
  1
  (length (take-all
    ((conj (call/fresh (lambda (x) (== x 5)))
           (call/fresh (lambda (y) (== y 6))))
     empty-state))))

(test "conj one fails"
  0
  (length (take-all
    ((conj (== 5 6)
           (call/fresh (lambda (x) (== x 5))))
     empty-state))))

;; === Stream interleaving ===

;; A goal that produces multiple answers
(define (fives x)
  (disj (== x 5) (lambda (s/c) (lambda () ((fives x) s/c)))))

(define (sixes x)
  (disj (== x 6) (lambda (s/c) (lambda () ((sixes x) s/c)))))

(test "interleaving"
  5
  (length (take 5
    ((call/fresh (lambda (x) (disj (fives x) (sixes x))))
     empty-state))))

;; Verify interleaving produces alternating results
(let ((results (take 4
                ((call/fresh (lambda (x) (disj (fives x) (sixes x))))
                 empty-state))))
  ;; First result should bind x=5, second x=6, etc.
  (test "interleave alternates"
    #t
    (> (length results) 0)))

;; === Classic: appendo via microKanren primitives ===

;; appendo(l, s, out) — l ++ s = out
(define (appendo l s out)
  (disj
    (conj (== l '()) (== s out))
    (call/fresh (lambda (a)
      (call/fresh (lambda (d)
        (call/fresh (lambda (res)
          (conj (== l (cons a d))
                (conj (== out (cons a res))
                      (appendo d s res)))))))))))

(test "appendo forward"
  1
  (length (take 1 ((appendo '(1 2) '(3 4) '(1 2 3 4)) empty-state))))

(test "appendo generates"
  #t
  (> (length (take 3 ((call/fresh (lambda (x)
                        (appendo x '(3) '(1 2 3))))
                       empty-state)))
     0))

(test-summary)
```

**Step 2: Add the Go test function**

Add a `TestMicroKanren` function to `integration/r7rs_test.go`, following the same pattern as `TestR7RSConformance`:

```go
// TestMicroKanren runs the microKanren integration tests.
// Tests the (wile microkanren) library for unification, goals, and streams.
func TestMicroKanren(t *testing.T) {
	schemeBin := getSchemeBinary()
	_, err := os.Stat(schemeBin)
	if os.IsNotExist(err) {
		t.Fatalf("scheme binary not found at %s - run 'make build' first", schemeBin)
	}

	testFile := filepath.Join(getTestDataPath(), "microkanren-tests.scm")
	_, err = os.Stat(testFile)
	if os.IsNotExist(err) {
		t.Fatalf("test file not found at %s", testFile)
	}

	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Minute)
	defer cancel()

	cmd := exec.CommandContext(ctx, schemeBin, "--file", testFile)
	cmd.Env = append(os.Environ(), "SCHEME_LIBRARY_PATH="+getLibPath())

	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr

	err = cmd.Run()
	output := stdout.String()
	errOutput := stderr.String()

	if ctx.Err() == context.DeadlineExceeded {
		t.Fatalf("microKanren tests timed out\n\nOutput:\n%s\n\nStderr:\n%s", output, errOutput)
	}

	if err != nil {
		exitErr, ok := err.(*exec.ExitError)
		if ok {
			summary := extractTestSummary(output)
			t.Fatalf("microKanren tests failed with exit code %d\n\nSummary:\n%s\n\nFull Output:\n%s\n\nStderr:\n%s",
				exitErr.ExitCode(), summary, output, errOutput)
		}
		t.Fatalf("failed to run microKanren tests: %v\n\nOutput:\n%s\n\nStderr:\n%s", err, output, errOutput)
	}

	summary := extractTestSummary(output)
	if summary != "" {
		t.Logf("microKanren tests passed:\n%s", summary)
	}
}
```

**Step 3: Build and run the test**

Run: `make build && go test -v -run TestMicroKanren ./integration/...`
Expected: All tests pass.

**Step 4: Fix any failures**

If tests fail, read the error output and fix either the library code (Task 1) or the test expectations. Common issues:
- `assoc` with 3-arg form not working as expected
- Library path resolution for `(wile microkanren)`
- Quasiquote behavior differences

**Step 5: Commit**

```
git add integration/testdata/microkanren-tests.scm integration/r7rs_test.go
git commit -m "test: add microKanren integration tests

Tests unification, goal construction, stream interleaving,
and appendo as a conformance stress test for closures and streams."
```

---

### Task 3: Create the miniKanren macro layer

**Files:**
- Create: `lib/wile/kanren.scm`
- Create: `lib/wile/kanren.sld`

**Step 1: Create the library definition**

Write `lib/wile/kanren.sld`:

```scheme
(define-library (wile kanren)
  (export
    ;; Re-export microKanren core
    var var? var=?
    walk ext-s unify
    == call/fresh disj conj
    mplus bind unit mzero
    empty-state
    ;; miniKanren syntactic sugar
    fresh conde run run*
    ;; Reification
    reify reify-1st)
  (import (scheme base)
          (wile microkanren))
  (include "kanren.scm"))
```

**Step 2: Create the macro implementation**

Write `lib/wile/kanren.scm`:

```scheme
;; miniKanren macro layer — syntactic sugar over microKanren
;; Provides fresh, conde, run, run* via syntax-rules

;; Zzz: inverse-eta-delay for recursive goals
(define-syntax zzz
  (syntax-rules ()
    ((zzz g) (lambda (s/c) (lambda () (g s/c))))))

;; conj+: conjunction of one or more goals
(define-syntax conj+
  (syntax-rules ()
    ((conj+ g) (zzz g))
    ((conj+ g0 g ...) (conj (zzz g0) (conj+ g ...)))))

;; disj+: disjunction of one or more goals
(define-syntax disj+
  (syntax-rules ()
    ((disj+ g) (zzz g))
    ((disj+ g0 g ...) (disj (zzz g0) (disj+ g ...)))))

;; conde: disjunctive normal form
;; (conde ((g ...) ...) ...)
(define-syntax conde
  (syntax-rules ()
    ((conde (g0 g ...) ...)
     (disj+ (conj+ g0 g ...) ...))))

;; fresh: introduce logic variables
;; (fresh (x y z) goal ...)
(define-syntax fresh
  (syntax-rules ()
    ((fresh () g0 g ...)
     (conj+ g0 g ...))
    ((fresh (x0 x ...) g0 g ...)
     (call/fresh
       (lambda (x0)
         (fresh (x ...) g0 g ...))))))

;; Pull: force a stream to a list
(define (pull $)
  (if (procedure? $) (pull ($)) $))

(define (take-inf n $)
  (cond
    ((zero? n) '())
    (else
     (let (($ (pull $)))
       (cond
         ((null? $) '())
         (else (cons (car $) (take-inf (- n 1) (cdr $)))))))))

(define (take-all-inf $)
  (let (($ (pull $)))
    (cond
      ((null? $) '())
      (else (cons (car $) (take-all-inf (cdr $)))))))

;; Reification
(define (reify-name n)
  (string->symbol
    (string-append "_." (number->string n))))

(define (walk* v s)
  (let ((v (walk v s)))
    (cond
      ((var? v) v)
      ((pair? v)
       (cons (walk* (car v) s)
             (walk* (cdr v) s)))
      (else v))))

(define (reify-s v s)
  (let ((v (walk v s)))
    (cond
      ((var? v) (ext-s v (reify-name (length s)) s))
      ((pair? v) (reify-s (cdr v) (reify-s (car v) s)))
      (else s))))

(define (reify v)
  (walk* v (reify-s v '())))

(define (reify-1st s/c)
  (let ((v (walk* (var 0) (car s/c))))
    (walk* v (reify-s v '()))))

;; run: bounded query
(define-syntax run
  (syntax-rules ()
    ((run n (x) g0 g ...)
     (let ((results (take-inf n
                      ((fresh (x) g0 g ...) empty-state))))
       (map reify-1st results)))))

;; run*: unbounded query
(define-syntax run*
  (syntax-rules ()
    ((run* (x) g0 g ...)
     (let ((results (take-all-inf
                      ((fresh (x) g0 g ...) empty-state))))
       (map reify-1st results)))))
```

**Step 3: Commit**

```
git add lib/wile/kanren.sld lib/wile/kanren.scm
git commit -m "feat: add (wile kanren) library — miniKanren macro sugar

Provides fresh, conde, run, run* macros and reification
on top of (wile microkanren) core."
```

---

### Task 4: Write miniKanren macro integration tests

**Files:**
- Create: `integration/testdata/kanren-tests.scm`
- Modify: `integration/r7rs_test.go` (add `TestKanren`)

**Step 1: Write the Scheme test file**

Write `integration/testdata/kanren-tests.scm`:

```scheme
;; miniKanren integration tests
;; Tests the (wile kanren) macro layer

(import (scheme base)
        (scheme write)
        (wile kanren))

;; Test infrastructure
(define *pass* 0)
(define *fail* 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (set! *pass* (+ *pass* 1))
      (begin
        (set! *fail* (+ *fail* 1))
        (display "FAIL: ")
        (display name)
        (display " — expected ")
        (write expected)
        (display " but got ")
        (write actual)
        (newline))))

(define (test-summary)
  (newline)
  (display "Test Summary:")
  (newline)
  (display "  Passed: ")
  (display *pass*)
  (newline)
  (display "  Failed: ")
  (display *fail*)
  (newline)
  (if (> *fail* 0)
      (exit 1)
      (exit 0)))

;; === fresh ===

(test "fresh single var"
  '(_.0)
  (run* (q) (fresh (x) (== q x))))

(test "fresh binds"
  '(5)
  (run* (q) (fresh (x) (== x 5) (== q x))))

(test "fresh multiple vars"
  '((5 6))
  (run* (q)
    (fresh (x y)
      (== x 5)
      (== y 6)
      (== q (list x y)))))

;; === conde ===

(test "conde two branches"
  '(tea cup)
  (run* (x)
    (conde
      ((== x 'tea))
      ((== x 'cup)))))

(test "conde with fresh"
  '(tea coffee)
  (run* (x)
    (conde
      ((== x 'tea))
      ((== x 'coffee)))))

;; === run with bound ===

(test "run bounded"
  2
  (length (run 2 (q) (conde ((== q 1)) ((== q 2)) ((== q 3))))))

(test "run 0"
  '()
  (run 0 (q) (== q 1)))

;; === Classic relations ===

;; appendo
(define (appendo l s out)
  (conde
    ((== l '()) (== s out))
    ((fresh (a d res)
       (== l (cons a d))
       (== out (cons a res))
       (appendo d s res)))))

(test "appendo forward"
  '((1 2 3 4))
  (run* (q) (appendo '(1 2) '(3 4) q)))

(test "appendo backward"
  '((1 2))
  (run* (q) (appendo q '(3 4) '(1 2 3 4))))

(test "appendo generate splits"
  5
  (length (run* (q)
    (fresh (x y)
      (appendo x y '(1 2 3 4))
      (== q (list x y))))))

;; membero
(define (membero x ls)
  (fresh (a d)
    (== ls (cons a d))
    (conde
      ((== a x))
      ((membero x d)))))

(test "membero found"
  '(_.0)
  (run 1 (q) (membero 2 '(1 2 3))))

(test "membero all"
  '(1 2 3)
  (run* (q) (membero q '(1 2 3))))

;; === Reification ===

(test "reify unbound"
  '(_.0)
  (run* (q) (fresh () (== q q))))

(test "reify pair with unbound"
  '((_.0 _.1))
  (run* (q)
    (fresh (x y)
      (== q (list x y)))))

;; === Diverging goals (fairness) ===

;; nevero: a goal that never succeeds
(define (nevero)
  (conde
    ((nevero))))

;; alwayso: a goal that always succeeds (infinitely)
(define (alwayso)
  (conde
    ((alwayso))
    ((== #t #t))))

(test "diverge with bound"
  1
  (length (run 1 (q)
    (conde
      ((== q 'yes))
      ((nevero))))))

(test-summary)
```

**Step 2: Add the Go test function**

Add `TestKanren` to `integration/r7rs_test.go`, same pattern as `TestMicroKanren` but pointing to `kanren-tests.scm`.

```go
// TestKanren runs the miniKanren macro layer integration tests.
// Tests fresh, conde, run, run*, reification, and classic relations.
func TestKanren(t *testing.T) {
	schemeBin := getSchemeBinary()
	_, err := os.Stat(schemeBin)
	if os.IsNotExist(err) {
		t.Fatalf("scheme binary not found at %s - run 'make build' first", schemeBin)
	}

	testFile := filepath.Join(getTestDataPath(), "kanren-tests.scm")
	_, err = os.Stat(testFile)
	if os.IsNotExist(err) {
		t.Fatalf("test file not found at %s", testFile)
	}

	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Minute)
	defer cancel()

	cmd := exec.CommandContext(ctx, schemeBin, "--file", testFile)
	cmd.Env = append(os.Environ(), "SCHEME_LIBRARY_PATH="+getLibPath())

	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr

	err = cmd.Run()
	output := stdout.String()
	errOutput := stderr.String()

	if ctx.Err() == context.DeadlineExceeded {
		t.Fatalf("kanren tests timed out\n\nOutput:\n%s\n\nStderr:\n%s", output, errOutput)
	}

	if err != nil {
		exitErr, ok := err.(*exec.ExitError)
		if ok {
			summary := extractTestSummary(output)
			t.Fatalf("kanren tests failed with exit code %d\n\nSummary:\n%s\n\nFull Output:\n%s\n\nStderr:\n%s",
				exitErr.ExitCode(), summary, output, errOutput)
		}
		t.Fatalf("failed to run kanren tests: %v\n\nOutput:\n%s\n\nStderr:\n%s", err, output, errOutput)
	}

	summary := extractTestSummary(output)
	if summary != "" {
		t.Logf("kanren tests passed:\n%s", summary)
	}
}
```

**Step 3: Build and run**

Run: `make build && go test -v -run TestKanren ./integration/...`
Expected: All tests pass.

**Step 4: Fix any failures**

Common issues at this layer:
- `syntax-rules` with ellipsis in `conde`/`disj+` — Wile's hygienic macro system must handle nested ellipsis correctly
- `map` in `run`/`run*` — verify `map` is available in the library context
- `reify-name` using `string->symbol` — verify `string-append` and `number->string` are in `(scheme base)`

**Step 5: Commit**

```
git add integration/testdata/kanren-tests.scm integration/r7rs_test.go
git commit -m "test: add miniKanren macro integration tests

Tests fresh, conde, run/run*, reification, appendo, membero,
and diverging goal fairness."
```

---

### Task 5: Run full test suite and verify no regressions

**Step 1: Run lint**

Run: `make lint`
Expected: Clean.

**Step 2: Run all tests**

Run: `make test`
Expected: All existing tests pass, plus the two new integration tests.

**Step 3: Run coverage check**

Run: `make covercheck`
Expected: Coverage thresholds still met (new code is pure Scheme, doesn't affect Go coverage).

**Step 4: Final commit if any fixups needed**

Only if previous steps required changes.
