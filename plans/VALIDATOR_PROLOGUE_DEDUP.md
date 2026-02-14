# Validator Prologue Deduplication Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Extract a `formPrologue` helper that consolidates the repeated collectList + improper-check + arity-guard pattern across 16 validator functions.

**Architecture:** Add a single `formPrologue` function to `internal/validate/validate.go` that handles source extraction, proper-list validation, and arity checking. Each validator replaces its 4-8 line prologue with a one-line call. Two non-standard validators (`validateCall`, `validateCaseLambdaClause`) keep their inline prologues because their elements[0] is not a form keyword.

**Tech Stack:** Go, existing `internal/validate` package

---

## Context

### The Duplicated Pattern

Every standard validator repeats this prologue (~4-8 lines):

```go
source := pair.SourceContext()
elements, improper := collectList(pair)
if improper {
    result.addError(source, "FORM", "FORM form must be a proper list")
    return nil
}
// arity check varies per form
```

### Arity Shapes

| Shape | Forms | Helper args |
|-------|-------|-------------|
| Exact N | quote(1), set!(2), define-syntax(2), dynamic-wind(3) | min=N, max=N |
| At least N | lambda(2), define(2), case-lambda(1), syntax-rules(1), import(1), include(1), define-library(1) | min=N, max=-1 |
| Range [min,max] | if(2-3) | min=2, max=3 |
| No check | begin, export, cond-expand | min=0, max=-1 |

### Non-Standard Sites (NOT converted)

- `validateCall` — elements[0] is the procedure expression, not a keyword
- `validateCaseLambdaClause` — elements[0] is params, not a keyword

### Test Safety

Existing tests check `result.Ok()` true/false but do NOT assert on error message text. Standardizing error messages is safe.

---

### Task 1: Add `formPrologue` helper

**Files:**
- Modify: `internal/validate/validate.go` (add helper near existing `collectList` at line ~145)

**Step 1: Write the failing test**

Add to `internal/validate/validate_test.go`:

```go
// TestFormPrologue tests the formPrologue helper directly
func TestFormPrologue(t *testing.T) {
	tests := []struct {
		name    string
		input   values.Value
		form    string
		minArgs int
		maxArgs int
		wantOk  bool
		wantLen int // expected len(elements) when ok
	}{
		{
			name:    "proper list exact arity match",
			input:   values.List(values.NewSymbol("quote"), values.NewSymbol("x")),
			form:    "quote",
			minArgs: 1, maxArgs: 1,
			wantOk: true, wantLen: 2,
		},
		{
			name:    "proper list exact arity mismatch",
			input:   values.List(values.NewSymbol("quote"), values.NewSymbol("x"), values.NewSymbol("y")),
			form:    "quote",
			minArgs: 1, maxArgs: 1,
			wantOk: false,
		},
		{
			name:    "proper list min arity satisfied",
			input:   values.List(values.NewSymbol("begin"), values.NewInteger(1), values.NewInteger(2)),
			form:    "begin",
			minArgs: 1, maxArgs: -1,
			wantOk: true, wantLen: 3,
		},
		{
			name:    "proper list min arity not met",
			input:   values.List(values.NewSymbol("lambda"), values.EmptyList),
			form:    "lambda",
			minArgs: 2, maxArgs: -1,
			wantOk: false,
		},
		{
			name:    "range arity within bounds",
			input:   values.List(values.NewSymbol("if"), values.TrueValue, values.NewInteger(1)),
			form:    "if",
			minArgs: 2, maxArgs: 3,
			wantOk: true, wantLen: 3,
		},
		{
			name:    "range arity above max",
			input:   values.List(values.NewSymbol("if"), values.TrueValue, values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
			form:    "if",
			minArgs: 2, maxArgs: 3,
			wantOk: false,
		},
		{
			name:    "no arity check",
			input:   values.List(values.NewSymbol("begin")),
			form:    "begin",
			minArgs: 0, maxArgs: -1,
			wantOk: true, wantLen: 1,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := qt.New(t)
			syntaxVal := makeSyntax(tt.input)
			pair := syntaxVal.(*syntax.SyntaxPair)
			result := &ValidationResult{}

			source, elements, ok := formPrologue(pair, tt.form, tt.minArgs, tt.maxArgs, result)
			c.Assert(ok, qt.Equals, tt.wantOk)
			if tt.wantOk {
				c.Assert(source, qt.IsNotNil)
				c.Assert(len(elements), qt.Equals, tt.wantLen)
			} else {
				c.Assert(len(result.Errors) > 0, qt.IsTrue)
			}
		})
	}
}

// TestFormPrologueImproperList tests formPrologue with an improper list
func TestFormPrologueImproperList(t *testing.T) {
	c := qt.New(t)
	syntaxVal := makeSyntax(values.NewCons(values.NewSymbol("if"), values.NewInteger(42)))
	pair := syntaxVal.(*syntax.SyntaxPair)
	result := &ValidationResult{}

	_, _, ok := formPrologue(pair, "if", 2, 3, result)
	c.Assert(ok, qt.IsFalse)
	c.Assert(len(result.Errors), qt.Equals, 1)
	c.Assert(result.Errors[0].Message, qt.Contains, "proper list")
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run 'TestFormPrologue' ./internal/validate/...`
Expected: FAIL — `formPrologue` undefined

**Step 3: Write the helper**

Add to `internal/validate/validate.go` after the existing `collectList` function:

```go
// formPrologue collects list elements, validates the list is proper,
// and checks argument count (excluding the form keyword at elements[0]).
// minArgs and maxArgs define acceptable argument counts.
// Use maxArgs < 0 for unlimited.
func formPrologue(
	pair *syntax.SyntaxPair,
	formName string,
	minArgs, maxArgs int,
	result *ValidationResult,
) (*syntax.SourceContext, []syntax.SyntaxValue, bool) {
	source := pair.SourceContext()

	elements, improper := collectList(pair)
	if improper {
		result.addError(source, formName, formName+" form must be a proper list")
		return nil, nil, false
	}

	argCount := len(elements) - 1

	if minArgs == maxArgs && maxArgs >= 0 {
		if argCount != minArgs {
			result.addErrorf(source, formName,
				"%s requires exactly %d argument(s), got %d",
				formName, minArgs, argCount)
			return nil, nil, false
		}
	} else {
		if argCount < minArgs {
			result.addErrorf(source, formName,
				"%s requires at least %d argument(s), got %d",
				formName, minArgs, argCount)
			return nil, nil, false
		}
		if maxArgs >= 0 && argCount > maxArgs {
			result.addErrorf(source, formName,
				"%s requires at most %d argument(s), got %d",
				formName, maxArgs, argCount)
			return nil, nil, false
		}
	}

	return source, elements, true
}
```

**Step 4: Run tests to verify they pass**

Run: `go test -v -run 'TestFormPrologue' ./internal/validate/...`
Expected: PASS

**Step 5: Commit**

```bash
git add internal/validate/validate.go internal/validate/validate_test.go
git commit -m "refactor: add formPrologue helper for validator deduplication"
```

---

### Task 2: Convert simple validators (if, set!, quote, quasiquote)

**Files:**
- Modify: `internal/validate/validate_if.go`
- Modify: `internal/validate/validate_set.go`
- Modify: `internal/validate/validate_quote.go`

**Step 1: Convert `validateIf`**

Replace the prologue in `validateIf` (lines 26-46) with:

```go
func validateIf(ctx context.Context, env *environment.EnvironmentFrame, pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source, elements, ok := formPrologue(pair, "if", 2, 3, result)
	if !ok {
		return nil
	}

	// Validate sub-expressions (continue even if some fail to collect all errors)
	// ... rest unchanged from line 49 onwards ...
```

Remove `argCount` variable — use `len(elements)-1` inline where needed (line 53: `if len(elements)-1 == 3`; line 58: `if ... (len(elements)-1 == 3 && alt == nil)`).

**Step 2: Convert `validateSetBang`**

Replace prologue with:

```go
func validateSetBang(ctx context.Context, env *environment.EnvironmentFrame, pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source, elements, ok := formPrologue(pair, "set!", 2, 2, result)
	if !ok {
		return nil
	}

	// Second element must be a symbol
	// ... rest unchanged from line 42 onwards ...
```

**Step 3: Convert `validateQuote` and `validateQuasiquote`**

Replace prologues with:

```go
// validateQuote
source, elements, ok := formPrologue(pair, "quote", 1, 1, result)

// validateQuasiquote
source, elements, ok := formPrologue(pair, "quasiquote", 1, 1, result)
```

**Step 4: Run tests**

Run: `go test -v -run 'TestValidateIf|TestValidateSetBang|TestValidateQuote|TestValidateQuasiquote' ./internal/validate/...`
Expected: PASS

**Step 5: Commit**

```bash
git add internal/validate/validate_if.go internal/validate/validate_set.go internal/validate/validate_quote.go
git commit -m "refactor: convert if/set!/quote/quasiquote to use formPrologue"
```

---

### Task 3: Convert begin and dynamic-wind validators

**Files:**
- Modify: `internal/validate/validate_begin.go`
- Modify: `internal/validate/validate_dynamic_wind.go`

**Step 1: Convert `validateBegin`**

Replace prologue with:

```go
func validateBegin(ctx context.Context, env *environment.EnvironmentFrame, pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source, elements, ok := formPrologue(pair, "begin", 0, -1, result)
	if !ok {
		return nil
	}

	// elements[0] is 'begin', can have zero or more expressions
	// ... rest unchanged from line 37 onwards ...
```

**Step 2: Convert `validateDynamicWind`**

Replace prologue with:

```go
func validateDynamicWind(ctx context.Context, env *environment.EnvironmentFrame, pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source, elements, ok := formPrologue(pair, "dynamic-wind", 3, 3, result)
	if !ok {
		return nil
	}

	// Validate sub-expressions
	// ... rest unchanged from line 48 onwards ...
```

Remove the `argCount` variable.

**Step 3: Run tests**

Run: `go test -v -run 'TestValidateBegin|TestValidateDynamic' ./internal/validate/...`
Expected: PASS (note: dynamic-wind may not have a dedicated test; run full suite to be sure)

**Step 4: Commit**

```bash
git add internal/validate/validate_begin.go internal/validate/validate_dynamic_wind.go
git commit -m "refactor: convert begin/dynamic-wind to use formPrologue"
```

---

### Task 4: Convert lambda, define, case-lambda validators

**Files:**
- Modify: `internal/validate/validate_lambda.go`
- Modify: `internal/validate/validate_define.go`
- Modify: `internal/validate/validate_case_lambda.go`

**Step 1: Convert `validateLambda`**

Replace prologue with:

```go
func validateLambda(ctx context.Context, env *environment.EnvironmentFrame, pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source, elements, ok := formPrologue(pair, "lambda", 2, -1, result)
	if !ok {
		return nil
	}

	// Validate parameters
	// ... rest unchanged from line 42 onwards ...
```

**Step 2: Convert `validateDefine`**

Replace prologue with:

```go
func validateDefine(ctx context.Context, env *environment.EnvironmentFrame, pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source, elements, ok := formPrologue(pair, "define", 2, -1, result)
	if !ok {
		return nil
	}

	// Check the second element to determine which form
	// ... rest unchanged from line 44 onwards ...
```

**Step 3: Convert `validateCaseLambda`**

Replace prologue with:

```go
func validateCaseLambda(ctx context.Context, env *environment.EnvironmentFrame, pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source, elements, ok := formPrologue(pair, "case-lambda", 1, -1, result)
	if !ok {
		return nil
	}

	// ... rest unchanged from line 42 onwards ...
```

**Note:** `validateCaseLambdaClause` is NOT converted — it has non-standard semantics (elements[0] is params, not a keyword).

**Step 4: Run tests**

Run: `go test -v -run 'TestValidateLambda|TestValidateDefine|TestValidateCaseLambda|TestValidateParamsEdgeCases' ./internal/validate/...`
Expected: PASS

**Step 5: Commit**

```bash
git add internal/validate/validate_lambda.go internal/validate/validate_define.go internal/validate/validate_case_lambda.go
git commit -m "refactor: convert lambda/define/case-lambda to use formPrologue"
```

---

### Task 5: Convert macro-related validators

**Files:**
- Modify: `internal/validate/validate_macro.go`

This file contains 7 validators to convert:

| Validator | minArgs | maxArgs |
|-----------|---------|---------|
| `validateDefineSyntax` | 2 | 2 |
| `validateSyntaxRules` | 1 | -1 |
| `validateImport` | 1 | -1 |
| `validateExport` | 0 | -1 |
| `validateDefineLibrary` | 1 | -1 |
| `validateInclude` | 1 | -1 |
| `validateCondExpand` | 0 | -1 |

**Step 1: Convert all 7 validators**

Replace each prologue with a `formPrologue` call using the min/max values from the table above. For each:

```go
source, elements, ok := formPrologue(pair, "FORM_NAME", MIN, MAX, result)
if !ok {
    return nil
}
```

**Step 2: Remove now-unused `_ = env` or unused import**

Some validators accept `env` but don't use it. The compiler will catch any issues.

**Step 3: Run tests**

Run: `go test -v -run 'TestValidateDefineSyntax|TestValidateSyntaxRules|TestValidateImport|TestValidateExport|TestValidateDefineLibrary|TestValidateInclude|TestValidateCondExpand|TestValidateSyntaxRulesEdgeCases' ./internal/validate/...`
Expected: PASS

**Step 4: Commit**

```bash
git add internal/validate/validate_macro.go
git commit -m "refactor: convert macro validators to use formPrologue"
```

---

### Task 6: Final verification and cleanup

**Step 1: Run full test suite**

Run: `go test -v ./internal/validate/...`
Expected: ALL PASS

**Step 2: Run linter**

Run: `make lint`
Expected: PASS (or only pre-existing warnings)

**Step 3: Run `goimports` on changed files**

Run: `goimports -w internal/validate/validate.go internal/validate/validate_test.go internal/validate/validate_if.go internal/validate/validate_set.go internal/validate/validate_quote.go internal/validate/validate_begin.go internal/validate/validate_dynamic_wind.go internal/validate/validate_lambda.go internal/validate/validate_define.go internal/validate/validate_case_lambda.go internal/validate/validate_macro.go`

**Step 4: Verify no other test suites are affected**

Run: `go test ./...`
Expected: ALL PASS (error messages changed but no callers check message text)

**Step 5: Verify deduplication**

Grep to confirm no remaining inline `collectList` + improper patterns in standard validators:

Run: `grep -n 'collectList' internal/validate/validate_*.go`
Expected: Only `validate_call.go` and `validate_case_lambda.go` (the 2 non-standard sites)

**Step 6: Commit final cleanup if needed**

---

## Summary

| Metric | Before | After |
|--------|--------|-------|
| Prologue sites | 18 (16 standard + 2 non-standard) | 2 inline + 16 via helper |
| Lines per prologue | 4-8 | 3 (call + check + return) |
| Net lines saved | ~60-80 lines | |
| Error message consistency | 16 different wordings | 3 standardized templates |

## Files Changed

- `internal/validate/validate.go` — add `formPrologue` helper
- `internal/validate/validate_test.go` — add `TestFormPrologue` tests
- `internal/validate/validate_if.go` — convert prologue
- `internal/validate/validate_set.go` — convert prologue
- `internal/validate/validate_quote.go` — convert prologue (2 validators)
- `internal/validate/validate_begin.go` — convert prologue
- `internal/validate/validate_dynamic_wind.go` — convert prologue
- `internal/validate/validate_lambda.go` — convert prologue
- `internal/validate/validate_define.go` — convert prologue
- `internal/validate/validate_case_lambda.go` — convert prologue (caseLambda only, NOT clause)
- `internal/validate/validate_macro.go` — convert prologue (7 validators)

## Not Changed

- `internal/validate/validate_call.go` — elements[0] is proc, not keyword
- `validateCaseLambdaClause` in `validate_case_lambda.go` — elements[0] is params, not keyword
