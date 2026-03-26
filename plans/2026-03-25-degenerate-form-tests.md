# Degenerate Form Full-Pipeline Tests

**Status:** Complete (PR #571)

**Goal:** Add full-pipeline tests (string → tokenize → parse → expand → compile → run) for degenerate forms of all core special forms and macro-based derived forms, ensuring errors are caught and error messages are verified for macros.

**Architecture:** One test file (`internal/validate/validate_degenerate_test.go`) with table-driven tests using `testhelpers.RunSchemeCode` and `RunSchemeCodeExpectError`. A local `degenerateErrorCase` struct adds `WantErr string` for error message substring checking (macros). Tests are organized by form, with success cases for "valid degenerate" forms (e.g., `(and)` → `#t`) and error cases for truly invalid forms.

**Tech Stack:** Go test, `registry/testhelpers`, `quicktest` (`qt`), `valuestest.SchemeEquals`

**Key reference files:**
- `internal/validate/validate_lambda_test.go` — existing full-pipeline pattern (error tests)
- `internal/validate/validate_let_test.go` — existing full-pipeline pattern (error + success)
- `registry/testhelpers/helpers.go` — `RunSchemeCode`, `RunSchemeCodeExpectError`, test case structs
- `registry/core/bootstrap_macros.scm` — all macro definitions (pattern shapes)
- `machine/operation_syntax_rules_transform.go:228` — `"syntax-rules: no matching clause for input"` error message

---

## What's Already Covered (skip these)

| Form | File | Degenerate cases |
|------|------|-----------------|
| `lambda` | `validate_lambda_test.go` | `(lambda)`, `(lambda (x))`, `(lambda (x x) x)` |
| `case-lambda` | `validate_lambda_test.go` | `(case-lambda)`, `(case-lambda ((x)))` |
| `define` | `validate_define_test.go` | `(define x)`, `(define (f x))`, `(define (f x x) x)`, `(define 42 1)` |
| `define-syntax` | `validate_macro_test.go` | `(define-syntax my-macro)`, `(define-syntax 42 ...)` |
| `syntax-rules` | `validate_macro_test.go` | `(define-syntax bad (syntax-rules))` |
| `import` | `validate_macro_test.go` | `(import)`, `(import (scheme nonexistent-library-xyz))` |
| `let` | `validate_let_test.go` | `(let)`, `(let ((x 1)))`, 8 more cases |
| `let*` | `validate_let_test.go` | `(let* ((x 1)))`, 5 more cases |
| `letrec`/`letrec*` | `validate_let_test.go` | `(letrec ((x 1)))`, duplicates, improper |
| `named let` | `validate_let_test.go` | `(let loop)`, `(let loop ((x 1)))`, non-list bindings |
| `apply` (runtime) | `prim_control_test.go` | `(apply + '(1 . 2))`, `(apply + 42)` |

## What's Missing (this plan covers these)

### Core Special Forms (errors caught at validation/compilation)

| Form | Degenerate inputs | Expected error stage |
|------|-------------------|---------------------|
| `if` | `(if)`, `(if #t)`, `(if #t 1 2 3)` | Validation |
| `set!` | `(set!)`, `(set! x)`, `(set! 42 1)`, `(set! x 1 2)` | Validation |
| `quote` | `(quote)`, `(quote a b)` | Validation |
| `quasiquote` | `` (quasiquote) ``, `` (quasiquote a b) `` | Validation |
| `dynamic-wind` | `(dynamic-wind)` through `(dynamic-wind a b c d)` | Validation |
| `with-continuation-mark` | `(with-continuation-mark)` through too-many-args | Validation |
| `apply` (syntax) | `(apply)`, `(apply f)` | Validation |
| `begin` | `(begin)` — **valid**, should return void | N/A (success case) |

### Macro-Based Derived Forms (errors caught at expansion)

All macros are `syntax-rules` in `bootstrap_macros.scm`. When no clause matches, the error is:
`"syntax-rules: no matching clause for input"`

| Form | Degenerate inputs | Notes |
|------|-------------------|-------|
| `and` | `(and)` → `#t` | **Valid** — has zero-arg clause |
| `or` | `(or)` → `#f` | **Valid** — has zero-arg clause |
| `cond` | `(cond)` | No clause matches |
| `case` | `(case)`, `(case 1)` | No clause matches |
| `when` | `(when)`, `(when #t)` | `when` requires `test result1 result2 ...` |
| `unless` | `(unless)`, `(unless #f)` | Same pattern as `when` |
| `do` | `(do)`, `(do ())` | Missing test clause |
| `guard` | `(guard)`, `(guard (e))` | Missing clause list or body |
| `parameterize` | `(parameterize)` | Missing binding list |
| `delay` | `(delay)` | Missing expression |
| `delay-force` | `(delay-force)` | Missing expression |
| `define-record-type` | `(define-record-type)` | Missing all args |
| `let-values` | `(let-values)` | Missing bindings and body |
| `let*-values` | `(let*-values)` | Missing bindings and body |
| `define-values` | `(define-values)` | Missing formals and expr |

---

## Task 1: Create test file with local test case struct

**Files:**
- Create: `internal/validate/validate_degenerate_test.go`

**Step 1: Write the file skeleton with the local test case type**

```go
// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// ...

package validate_test

import (
	"testing"

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// degenerateErrorCase tests that a degenerate form produces an error
// through the full pipeline (string → tokenize → parse → expand → compile → run).
// WantErr, if non-empty, checks that the error message contains the substring.
type degenerateErrorCase struct {
	Name    string
	Code    string
	WantErr string // error message substring to check (empty = just verify error occurs)
}

func runDegenerateErrorTests(t *testing.T, tcs []degenerateErrorCase) {
	t.Helper()
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			err := testhelpers.RunSchemeCodeExpectError(t, tc.Code)
			if tc.WantErr != "" && err != nil {
				qt.Assert(t, err.Error(), qt.Contains, tc.WantErr)
			}
		})
	}
}
```

**Step 2: Run `go build ./internal/validate/` to verify it compiles**

Run: `go build ./internal/validate/`
Expected: clean (no output)

**Step 3: Commit**

```
test: add degenerate form test skeleton
```

---

## Task 2: Core special form degenerate tests — `if`

**Files:**
- Modify: `internal/validate/validate_degenerate_test.go`

**Step 1: Write the test**

```go
func TestIf_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: `(if)`, WantErr: "if"},
		{Name: "test only", Code: `(if #t)`, WantErr: "if"},
		{Name: "too many args", Code: `(if #t 1 2 3)`, WantErr: "if"},
	}
	runDegenerateErrorTests(t, tcs)
}
```

**Step 2: Run the test**

Run: `go test -v -run TestIf_Degenerate ./internal/validate/`
Expected: PASS — all three cases produce errors containing "if"

**Step 3: Commit**

```
test: add full-pipeline degenerate tests for if
```

---

## Task 3: Core special form degenerate tests — `set!`, `quote`, `quasiquote`

**Files:**
- Modify: `internal/validate/validate_degenerate_test.go`

**Step 1: Write the tests**

```go
func TestSetBang_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: `(set!)`, WantErr: "set!"},
		{Name: "name only", Code: `(begin (define x 0) (set! x))`, WantErr: "set!"},
		{Name: "non-symbol target", Code: `(set! 42 1)`, WantErr: "set!"},
		{Name: "too many args", Code: `(begin (define x 0) (set! x 1 2))`, WantErr: "set!"},
	}
	runDegenerateErrorTests(t, tcs)
}

func TestQuote_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: `(quote)`, WantErr: "quote"},
		{Name: "too many args", Code: `(quote a b)`, WantErr: "quote"},
	}
	runDegenerateErrorTests(t, tcs)
}

func TestQuasiquote_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: "(quasiquote)", WantErr: "quasiquote"},
		{Name: "too many args", Code: "(quasiquote a b)", WantErr: "quasiquote"},
	}
	runDegenerateErrorTests(t, tcs)
}
```

**Step 2: Run the tests**

Run: `go test -v -run 'TestSetBang_Degenerate|TestQuote_Degenerate|TestQuasiquote_Degenerate' ./internal/validate/`
Expected: PASS

**Step 3: Commit**

```
test: add full-pipeline degenerate tests for set!, quote, quasiquote
```

---

## Task 4: Core special form degenerate tests — `dynamic-wind`, `with-continuation-mark`, `apply`

**Files:**
- Modify: `internal/validate/validate_degenerate_test.go`

**Step 1: Write the tests**

```go
func TestDynamicWind_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: `(dynamic-wind)`, WantErr: "dynamic-wind"},
		{Name: "one arg", Code: `(dynamic-wind (lambda () #f))`, WantErr: "dynamic-wind"},
		{Name: "two args", Code: `(dynamic-wind (lambda () #f) (lambda () #f))`, WantErr: "dynamic-wind"},
		{Name: "four args", Code: `(dynamic-wind (lambda () #f) (lambda () #f) (lambda () #f) (lambda () #f))`, WantErr: "dynamic-wind"},
	}
	runDegenerateErrorTests(t, tcs)
}

func TestWithContinuationMark_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: `(with-continuation-mark)`, WantErr: "with-continuation-mark"},
		{Name: "one arg", Code: `(with-continuation-mark 'k)`, WantErr: "with-continuation-mark"},
		{Name: "two args", Code: `(with-continuation-mark 'k 1)`, WantErr: "with-continuation-mark"},
		{Name: "four args", Code: `(with-continuation-mark 'k 1 2 3)`, WantErr: "with-continuation-mark"},
	}
	runDegenerateErrorTests(t, tcs)
}

func TestApply_Degenerate_Syntax(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: `(apply)`, WantErr: "apply"},
		{Name: "proc only", Code: `(apply +)`, WantErr: "apply"},
	}
	runDegenerateErrorTests(t, tcs)
}
```

**Step 2: Run the tests**

Run: `go test -v -run 'TestDynamicWind_Degenerate|TestWithContinuationMark_Degenerate|TestApply_Degenerate_Syntax' ./internal/validate/`
Expected: PASS

**Step 3: Commit**

```
test: add full-pipeline degenerate tests for dynamic-wind, with-continuation-mark, apply
```

---

## Task 5: Valid degenerate forms — `begin`, `and`, `or`

These forms have valid degenerate uses that should succeed. Testing them confirms the full pipeline handles edge cases correctly.

**Files:**
- Modify: `internal/validate/validate_degenerate_test.go`

**Step 1: Write the tests**

```go
func TestBegin_Degenerate(t *testing.T) {
	// (begin) is valid per R7RS — returns void
	result, err := testhelpers.RunSchemeCode(t, `(begin)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.Void)
}

func TestAnd_Degenerate(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "no args returns true", Code: `(and)`, Expected: values.TrueValue},
		{Name: "single arg returns it", Code: `(and 42)`, Expected: values.NewInteger(42)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestOr_Degenerate(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "no args returns false", Code: `(or)`, Expected: values.FalseValue},
		{Name: "single arg returns it", Code: `(or 42)`, Expected: values.NewInteger(42)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}
```

**Step 2: Run the tests**

Run: `go test -v -run 'TestBegin_Degenerate|TestAnd_Degenerate|TestOr_Degenerate' ./internal/validate/`
Expected: PASS

**Step 3: Commit**

```
test: add full-pipeline tests for valid degenerate begin, and, or
```

---

## Task 6: Macro degenerate tests — conditionals (`cond`, `case`, `when`, `unless`)

All use `syntax-rules`. Degenerate forms fail at expansion with `"syntax-rules: no matching clause for input"`.

**Files:**
- Modify: `internal/validate/validate_degenerate_test.go`

**Step 1: Write the tests**

```go
func TestCond_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no clauses", Code: `(cond)`, WantErr: "no matching clause"},
	}
	runDegenerateErrorTests(t, tcs)
}

func TestCase_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: `(case)`, WantErr: "no matching clause"},
		{Name: "key only", Code: `(case 1)`, WantErr: "no matching clause"},
	}
	runDegenerateErrorTests(t, tcs)
}

func TestWhen_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: `(when)`, WantErr: "no matching clause"},
		{Name: "test only no body", Code: `(when #t)`, WantErr: "no matching clause"},
	}
	runDegenerateErrorTests(t, tcs)
}

func TestUnless_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: `(unless)`, WantErr: "no matching clause"},
		{Name: "test only no body", Code: `(unless #f)`, WantErr: "no matching clause"},
	}
	runDegenerateErrorTests(t, tcs)
}
```

**Step 2: Run the tests**

Run: `go test -v -run 'TestCond_Degenerate|TestCase_Degenerate|TestWhen_Degenerate|TestUnless_Degenerate' ./internal/validate/`
Expected: PASS — all produce "no matching clause" at expansion time

**Step 3: Commit**

```
test: add full-pipeline degenerate tests for cond, case, when, unless
```

---

## Task 7: Macro degenerate tests — iteration and exceptions (`do`, `guard`)

**Files:**
- Modify: `internal/validate/validate_degenerate_test.go`

**Step 1: Write the tests**

```go
func TestDo_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: `(do)`, WantErr: "no matching clause"},
		{Name: "bindings only", Code: `(do ())`, WantErr: "no matching clause"},
	}
	runDegenerateErrorTests(t, tcs)
}

func TestGuard_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: `(guard)`, WantErr: "no matching clause"},
		{Name: "var only", Code: `(guard (e))`, WantErr: "no matching clause"},
	}
	runDegenerateErrorTests(t, tcs)
}
```

**Step 2: Run the tests**

Run: `go test -v -run 'TestDo_Degenerate|TestGuard_Degenerate' ./internal/validate/`
Expected: PASS

**Step 3: Commit**

```
test: add full-pipeline degenerate tests for do, guard
```

---

## Task 8: Macro degenerate tests — parameters and promises (`parameterize`, `delay`, `delay-force`)

**Files:**
- Modify: `internal/validate/validate_degenerate_test.go`

**Step 1: Write the tests**

```go
func TestParameterize_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: `(parameterize)`, WantErr: "no matching clause"},
	}
	runDegenerateErrorTests(t, tcs)
}

func TestDelay_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: `(delay)`, WantErr: "no matching clause"},
	}
	runDegenerateErrorTests(t, tcs)
}

func TestDelayForce_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: `(delay-force)`, WantErr: "no matching clause"},
	}
	runDegenerateErrorTests(t, tcs)
}
```

**Step 2: Run the tests**

Run: `go test -v -run 'TestParameterize_Degenerate|TestDelay_Degenerate|TestDelayForce_Degenerate' ./internal/validate/`
Expected: PASS

**Step 3: Commit**

```
test: add full-pipeline degenerate tests for parameterize, delay, delay-force
```

---

## Task 9: Macro degenerate tests — records and multiple values

**Files:**
- Modify: `internal/validate/validate_degenerate_test.go`

**Step 1: Write the tests**

```go
func TestDefineRecordType_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: `(define-record-type)`, WantErr: "no matching clause"},
	}
	runDegenerateErrorTests(t, tcs)
}

func TestLetValues_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: `(let-values)`, WantErr: "no matching clause"},
	}
	runDegenerateErrorTests(t, tcs)
}

func TestLetStarValues_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: `(let*-values)`, WantErr: "no matching clause"},
	}
	runDegenerateErrorTests(t, tcs)
}

func TestDefineValues_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: `(define-values)`, WantErr: "no matching clause"},
	}
	runDegenerateErrorTests(t, tcs)
}
```

**Step 2: Run the tests**

Run: `go test -v -run 'TestDefineRecordType_Degenerate|TestLetValues_Degenerate|TestLetStarValues_Degenerate|TestDefineValues_Degenerate' ./internal/validate/`
Expected: PASS

**Step 3: Commit**

```
test: add full-pipeline degenerate tests for define-record-type, let-values, let*-values, define-values
```

---

## Task 10: Run full test suite and lint

**Step 1: Run all degenerate tests together**

Run: `go test -v -run 'Degenerate' ./internal/validate/`
Expected: All tests PASS

**Step 2: Run full project tests**

Run: `make test`
Expected: All tests PASS

**Step 3: Run lint**

Run: `make lint`
Expected: Clean

**Step 4: Commit (if any lint fixes needed)**

---

## Task 11: Verify edge cases discovered during testing

During implementation, verify these assumptions and add tests if behavior differs from expected:

1. **`(begin)` returns void** — If it doesn't, adjust the test to match actual behavior
2. **`(when #t)` and `(unless #f)` fail** — The macro pattern requires `result1` (not optional). If the `...` matches zero elements differently, the test expectation may need adjustment
3. **`(guard (e))` fails** — Guard requires at least a body (`e1 e2 ...`). Confirm this doesn't match with empty body
4. **Panic vs error** — Some degenerate forms may panic (caught by `RunSchemeCodeExpectError`'s `recover`). The error message check still works because panics are converted to errors

If any assumption is wrong, update the test case (change `WantErr` or move from error table to success table).

---

## Design Decisions

### Why one file instead of per-form files?
"Degenerate form handling" is a cohesive testing concern that cuts across all forms. One file makes it easy to see what's covered and what's missing. The file is ~200 lines — manageable.

### Why `WantErr` substring check instead of `errors.Is`?
Macro expansion errors come from the VM (`mc.Error(...)`) and wrap multiple layers of context. Substring matching is robust against wrapping changes. The form name (e.g., "if", "set!") or the expansion error ("no matching clause") are stable identifiers.

### Why not extend `SchemeCodeErrorTestCase`?
That's a shared type in `testhelpers`. Adding `WantErr` would change the contract for ~114 test files. A local `degenerateErrorCase` type is scoped to this concern.

### Why `internal/validate/` and not `integration/`?
The existing full-pipeline form tests already live here (`validate_lambda_test.go`, `validate_let_test.go`, etc.). This follows the established pattern.
