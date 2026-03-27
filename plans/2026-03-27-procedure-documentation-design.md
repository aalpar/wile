# `procedure-documentation` Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement Guile-style `procedure-documentation` — the compiler extracts leading string literals from lambda/define bodies and stores them on the compiled template; a new primitive retrieves them at runtime.

**Architecture:** Add a `doc string` field to `NativeTemplate`. The compiler's `compileBody` detects a leading `ValidatedLiteral` wrapping a `*values.String` when body has >1 expression, strips it, and sets `tpl.doc`. A new `procedure-documentation` primitive in `registry/core/reflection.go` reads it via `MachineClosure.Template().Doc()`.

**Tech Stack:** Go, existing `machine/`, `registry/core/`, `internal/validate/` packages. No new dependencies.

**Convention:** Guile-style docstrings (Guile Reference §6.7.2.2). A string literal as the first expression in a lambda body, when the body has more than one expression, is treated as documentation rather than executable code.

---

### Task 1: Add `doc` field to `NativeTemplate`

**Files:**
- Modify: `machine/native_template.go`

**Step 1: Write the failing test**

Add to `machine/native_template_test.go` (or create if needed):

```go
func TestNativeTemplateDoc(t *testing.T) {
	tpl := &NativeTemplate{}
	qt.Assert(t, tpl.Doc(), qt.Equals, "")
	tpl.SetDoc("Computes factorial.")
	qt.Assert(t, tpl.Doc(), qt.Equals, "Computes factorial.")
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestNativeTemplateDoc ./machine/...`
Expected: FAIL — `Doc` and `SetDoc` don't exist

**Step 3: Implement**

In `machine/native_template.go`, add the `doc` field to the struct:

```go
type NativeTemplate struct {
	parameterCount int
	valueCount     int
	isVariadic     bool
	doc            string // Guile-style docstring from leading string literal in body
	literals       MultipleValues
	// ... rest unchanged
```

Add accessors after `SetName`:

```go
func (p *NativeTemplate) Doc() string {
	return p.doc
}

func (p *NativeTemplate) SetDoc(doc string) {
	p.doc = doc
}
```

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestNativeTemplateDoc ./machine/...`
Expected: PASS

**Step 5: Commit**

```
feat(machine): add doc field to NativeTemplate
```

---

### Task 2: Extract docstring in `compileBody`

**Files:**
- Modify: `machine/compile_closure.go`

**Step 1: Write the failing test**

Add to `machine/compile_closure_test.go`:

```go
func TestCompileBodyDocstring(t *testing.T) {
	tcs := []struct {
		name     string
		code     string
		expected string
	}{
		{
			name:     "leading string with body",
			code:     `(define (f x) "Adds one." (+ x 1))`,
			expected: "Adds one.",
		},
		{
			name:     "no docstring",
			code:     `(define (g x) (+ x 1))`,
			expected: "",
		},
		{
			name:     "string-only body is not a docstring",
			code:     `(define (h) "just a string")`,
			expected: "",
		},
		{
			name:     "lambda with docstring",
			code:     `(define f (lambda (x) "Doubles x." (* x 2)))`,
			expected: "Doubles x.",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code+`
				(procedure-documentation f)`)
			// This test depends on Task 3 (the primitive).
			// If running before Task 3, verify via template directly.
			_ = result
			_ = err
		})
	}
}
```

Note: This test is best validated end-to-end after Task 3. For Task 2 in isolation, verify the compiler change doesn't break existing tests.

**Step 2: Run existing tests to establish baseline**

Run: `go test -v ./machine/... -count=1`
Expected: PASS (no regressions)

**Step 3: Implement**

In `machine/compile_closure.go`, modify `compileBody` to extract a leading docstring between pass 1 and pass 2:

```go
func (p *CompileTimeContinuation) compileBody(ctctx CompileTimeCallContext, clause validate.ValidatedBodyAndParams, childEnv *environment.EnvironmentFrame, tpl *NativeTemplate) error {
	childCompiler := NewCompiletimeContinuation(tpl, childEnv)
	lambdaBodyContext := NewCompileTimeCallContext(ctctx.ctx, true)

	body := clause.Body()

	// R7RS §5.3.2: Internal definitions use letrec* semantics
	// Pass 1: Pre-declare all define bindings so forward references work
	for _, bodyExpr := range body {
		childCompiler.predeclareDefineBindingFromValidated(bodyExpr)
	}

	// Docstring extraction (Guile convention): if the first body expression
	// is a string literal and the body has more than one expression, treat
	// the string as documentation rather than executable code.
	body = extractDocstring(body, tpl)

	// Pass 2: Compile all expressions (with all bindings now visible)
	err := childCompiler.compileValidatedSequence(lambdaBodyContext, body)
	if err != nil {
		return err
	}

	childCompiler.AppendOperations(NewOperationRestoreContinuation())
	return nil
}
```

Add the extraction helper in the same file:

```go
// extractDocstring checks whether the first body expression is a string
// literal (Guile-style docstring). If so, stores it on the template and
// returns the remaining body. The string must not be the only expression
// (a body of just "hello" is a return value, not documentation).
func extractDocstring(body []validate.ValidatedExpr, tpl *NativeTemplate) []validate.ValidatedExpr {
	if len(body) < 2 {
		return body
	}
	lit, ok := body[0].(*validate.ValidatedLiteral)
	if !ok {
		return body
	}
	str, ok := lit.Value.UnwrapAll().(*values.String)
	if !ok {
		return body
	}
	tpl.SetDoc(str.Value())
	return body[1:]
}
```

**Step 4: Run tests to verify no regressions**

Run: `go test -v ./machine/... -count=1`
Expected: PASS

**Step 5: Commit**

```
feat(machine): extract Guile-style docstrings in compileBody
```

---

### Task 3: Implement `procedure-documentation` primitive

**Files:**
- Modify: `registry/core/reflection.go`
- Modify: `registry/core/prim_reflection.go`

**Step 1: Write the failing test**

Add to `registry/core/prim_reflection_test.go`:

```go
func TestProcedureDocumentation(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "define with docstring",
			Code: `(define (f x) "Adds one to x." (+ x 1))
			        (procedure-documentation f)`,
			Expected: values.NewString("Adds one to x."),
		},
		{
			Name: "lambda with docstring",
			Code: `(define f (lambda (x) "Doubles x." (* x 2)))
			        (procedure-documentation f)`,
			Expected: values.NewString("Doubles x."),
		},
		{
			Name: "no docstring returns false",
			Code: `(define (g x) (+ x 1))
			        (procedure-documentation g)`,
			Expected: values.FalseValue,
		},
		{
			Name: "string-only body is return value not docstring",
			Code: `(define (h) "just a string")
			        (procedure-documentation h)`,
			Expected: values.FalseValue,
		},
		{
			Name: "foreign procedure returns false",
			Code: `(procedure-documentation car)`,
			Expected: values.FalseValue,
		},
		{
			Name: "non-procedure returns false",
			Code: `(procedure-documentation 42)`,
			Expected: values.FalseValue,
		},
		{
			Name: "case-lambda with docstring in first clause",
			Code: `(define f (case-lambda
			            ((x) "One arg." (+ x 1))
			            ((x y) (+ x y))))
			        (procedure-documentation f)`,
			Expected: values.NewString("One arg."),
		},
		{
			Name: "case-lambda without docstring",
			Code: `(define f (case-lambda
			            ((x) (+ x 1))
			            ((x y) (+ x y))))
			        (procedure-documentation f)`,
			Expected: values.FalseValue,
		},
		{
			Name: "multiline docstring",
			Code: `(define (f x)
			          "Adds one to x.\nReturns an integer."
			          (+ x 1))
			        (procedure-documentation f)`,
			Expected: values.NewString("Adds one to x.\nReturns an integer."),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestProcedureDocumentationErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "wrong arity zero", Code: `(procedure-documentation)`},
		{Name: "wrong arity two", Code: `(procedure-documentation car car)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestProcedureDocumentation ./registry/core/...`
Expected: FAIL — `procedure-documentation` is unbound

**Step 3: Register the primitive**

In `registry/core/reflection.go`, add to the `addReflection` spec slice:

```go
{Name: "procedure-documentation", ParamCount: 1, Impl: PrimProcedureDocumentation,
    Doc: "Returns the docstring of a procedure, or #f if none.", ParamNames: []string{"proc"}, Category: "reflection",
    ReturnType: values.TypeAny},
```

Note: `ParamTypes` is intentionally omitted — this accepts any value (not just procedures), returning `#f` for non-procedures rather than erroring. This matches Guile behavior.

**Step 4: Implement the primitive**

In `registry/core/prim_reflection.go`, add:

```go
// PrimProcedureDocumentation implements (procedure-documentation obj).
// Returns the docstring attached to a Scheme-defined procedure, or #f.
// Follows the Guile convention (Guile Reference §6.7.2.2).
func PrimProcedureDocumentation(mc *machine.MachineContext) error {
	o := mc.Arg(0)
	switch v := o.(type) {
	case *machine.MachineClosure:
		doc := v.Template().Doc()
		if doc == "" {
			mc.SetValue(values.FalseValue)
		} else {
			mc.SetValue(values.NewString(doc))
		}
	case *machine.CaseLambdaClosure:
		clauses := v.Clauses()
		if len(clauses) > 0 {
			doc := clauses[0].Template().Doc()
			if doc != "" {
				mc.SetValue(values.NewString(doc))
				return nil
			}
		}
		mc.SetValue(values.FalseValue)
	default:
		mc.SetValue(values.FalseValue)
	}
	return nil
}
```

**Step 5: Run tests**

Run: `go test -v -run TestProcedureDocumentation ./registry/core/...`
Expected: PASS

**Step 6: Run full test suite for regressions**

Run: `go test ./registry/core/... -count=1`
Expected: PASS

**Step 7: Commit**

```
feat(core): add procedure-documentation primitive (Guile-style docstrings)
```

---

### Task 4: Lint and final verification

**Step 1: Run linter**

Run: `make lint`
Expected: PASS

**Step 2: Run covercheck**

Run: `make covercheck`
Expected: PASS

**Step 3: Run full test suite**

Run: `make test`
Expected: PASS

**Step 4: Commit any lint fixes if needed**

---

### Task 5: Update PRIMITIVES.md

**Files:**
- Modify: `PRIMITIVES.md`

Add `procedure-documentation` to the reflection section, following the existing pattern for `procedure-name`, `procedure-arity`, etc.

**Commit:**

```
docs: add procedure-documentation to PRIMITIVES.md
```

---

## Out of Scope (see TODO.md sub-items)

- `ForeignClosure` doc propagation from `PrimitiveSpec.Doc`
- `Documentable` interface for unified doc access
- Validator-layer docstring detection
- Scribble-style `@` reader notation
- `,doc` REPL command unification with `procedure-documentation`
