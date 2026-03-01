# Apply Tail Position Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Make `apply` a compile-time special form so it produces proper tail calls (R7RS §3.5), eliminating Go stack overflow on recursive `(apply f (list (- n 1)))` patterns.

**Architecture:** Dual registration (same pattern as `dynamic-wind`): compile-time binding in `specialforms.go` + validator + compiler emit `OpUnpackListToStack` + `OpApply`. Runtime `PrimApply` ForeignClosure unchanged for first-class uses. New zero-operand opcode `OpUnpackListToStack` reads a list from the value register and pushes each element to the eval stack.

**Tech Stack:** Go, bytecode VM (machine package), forms registry, validate package.

**Design doc:** `plans/H1-APPLY-TAIL-POSITION.md`

---

### Task 1: Add OpUnpackListToStack opcode constant

**Files:**
- Modify: `machine/opcode.go`

**Step 1: Add the opcode constant**

In `machine/opcode.go`, add `OpUnpackListToStack` as a zero-operand opcode in Wave 1 (before `OpRestoreContinuation`):

```go
// In the const block, after OpApply:
OpUnpackListToStack
OpRestoreContinuation
```

And add the name in `opcodeNames`:

```go
OpUnpackListToStack:    "UnpackListToStack",
```

**Step 2: Verify it compiles**

Run: `go build ./machine/...`
Expected: PASS

**Step 3: Commit**

```
feat: add OpUnpackListToStack opcode constant

Zero-operand opcode for H1 (apply tail position). Will read a proper
list from the value register and push each element to the eval stack.
```

---

### Task 2: Create OperationUnpackListToStack type

**Files:**
- Create: `machine/operation_unpack_list_to_stack.go`

**Step 1: Write the failing test**

Add to `machine/operation_test.go` (find existing operation constructor tests):

```go
func TestOperationUnpackListToStack_EqualTo(t *testing.T) {
	c := qt.New(t)
	op1 := NewOperationUnpackListToStack()
	op2 := NewOperationUnpackListToStack()
	c.Assert(op1.EqualTo(op2), qt.IsTrue)
	c.Assert(op1.EqualTo(NewOperationPush()), qt.IsFalse)
}
```

Run: `go test -run TestOperationUnpackListToStack_EqualTo ./machine/...`
Expected: FAIL (type doesn't exist)

**Step 2: Write the operation type**

Create `machine/operation_unpack_list_to_stack.go`:

```go
package machine

import (
	"github.com/aalpar/wile/values"
)

// OperationUnpackListToStack reads a proper list from the value register
// and pushes each element to the eval stack in order. Used by compiled
// (apply proc arg1 ... args) to flatten the final arg list onto the stack
// before Pull + OpApply.
//
// Errors if the value is not a proper list (improper list or non-list).
type OperationUnpackListToStack struct {
	OperationBase
}

func NewOperationUnpackListToStack() *OperationUnpackListToStack {
	return &OperationUnpackListToStack{
		OperationBase: NewOperationBase("operation-unpack-list-to-stack"),
	}
}

func (p *OperationUnpackListToStack) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationUnpackListToStack)
	return sameType(p, v, ok)
}
```

**Step 3: Run test to verify it passes**

Run: `go test -run TestOperationUnpackListToStack_EqualTo ./machine/...`
Expected: PASS

**Step 4: Commit**

```
feat: add OperationUnpackListToStack type

Operation type for the new opcode. Reads a proper list from the value
register and pushes each element to the eval stack.
```

---

### Task 3: Wire OpUnpackListToStack into the VM loop and template converters

**Files:**
- Modify: `machine/machine_context.go` (Run loop)
- Modify: `machine/native_template.go` (operationToInstruction, instructionToOperation)

**Step 1: Write the failing test**

Add to `machine/machine_context_test.go`:

```go
func TestOpUnpackListToStack_ProperList(t *testing.T) {
	c := qt.New(t)
	engine := newTestEngine(t)
	result, err := engine.Eval(`(apply + '(1 2 3))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, values.NewInteger(6))
}
```

This test won't pass until apply is compiled as a special form (Task 6+). For now, we wire the VM loop and template converters so the opcode is mechanically functional.

**Step 2: Add the case to `instructionToOperation` in `native_template.go`**

In the Wave 1 section (after `OpApply`):

```go
case OpUnpackListToStack:
	return NewOperationUnpackListToStack()
```

**Step 3: Add the case to `operationToInstruction` in `native_template.go`**

In the Wave 1 section (after `*OperationApply`):

```go
case *OperationUnpackListToStack:
	return Instruction{Op: OpUnpackListToStack}, true
```

**Step 4: Add the case to `Run()` in `machine_context.go`**

In the Wave 1 section (after the `OpApply` case, before `OpRestoreContinuation`):

```go
case OpUnpackListToStack:
	v := mc.GetValue()
	if values.IsEmptyList(v) {
		mc.pc++
		continue
	}
	tup, ok := v.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList,
			"apply: final argument must be a list, got %s", v.SchemeString())
	}
	_, err := tup.ForEach(mc.ctx, func(_ context.Context, _ int, _ bool, elem values.Value) error {
		mc.evals.Push(elem)
		return nil
	})
	if err != nil {
		return err
	}
	mc.pc++
```

Note: `ForEach` returns the sentinel value (empty list for proper lists, non-empty for improper). Check the sentinel:

Actually, check the return value — `ForEach` returns the cdr-sentinel. For proper lists it returns the empty list. For improper lists it returns the non-list cdr. We need to error on improper:

```go
case OpUnpackListToStack:
	v := mc.GetValue()
	if values.IsEmptyList(v) {
		mc.pc++
		continue
	}
	tup, ok := v.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList,
			"apply: final argument must be a list, got %s", v.SchemeString())
	}
	sentinel, err := tup.ForEach(mc.ctx, func(_ context.Context, _ int, _ bool, elem values.Value) error {
		mc.evals.Push(elem)
		return nil
	})
	if err != nil {
		return err
	}
	if !values.IsEmptyList(sentinel) {
		return values.WrapForeignErrorf(values.ErrNotAList,
			"apply: final argument is an improper list")
	}
	mc.pc++
```

**Step 5: Verify it compiles**

Run: `go build ./machine/...`
Expected: PASS

**Step 6: Commit**

```
feat: wire OpUnpackListToStack into VM loop and template converters

The opcode iterates a proper list from the value register and pushes
each element to the eval stack. Errors on non-list or improper list.
```

---

### Task 4: Add ValidatedApply node

**Files:**
- Modify: `internal/validate/validated_forms.go`

**Step 1: Write the failing test**

Add to `internal/validate/validated_forms_test.go` (follow `TestValidatedDynamicWind_Getters` pattern):

```go
func TestValidatedApply_Getters(t *testing.T) {
	c := qt.New(t)
	proc := newLiteralExpr(nil, syntax.WrapValue(values.NewSymbol("f")))
	prefix := []ValidatedExpr{
		newLiteralExpr(nil, syntax.WrapValue(values.NewInteger(1))),
	}
	finalList := newLiteralExpr(nil, syntax.WrapValue(values.NewSymbol("args")))

	va := &ValidatedApply{
		validatedBase: validatedBase{formName: "apply"},
		Proc:          proc,
		PrefixArgs:    prefix,
		FinalList:     finalList,
	}
	c.Assert(va.FormName(), qt.Equals, "apply")
	c.Assert(va.Proc, qt.Equals, proc)
	c.Assert(va.PrefixArgs, qt.HasLen, 1)
	c.Assert(va.FinalList, qt.Equals, finalList)
}
```

Run: `go test -run TestValidatedApply_Getters ./internal/validate/...`
Expected: FAIL (type doesn't exist)

**Step 2: Add the type**

In `internal/validate/validated_forms.go`, after `ValidatedDynamicWind`:

```go
// ValidatedApply represents (apply proc arg1 ... args)
//
// R7RS §6.10: apply calls proc with arguments arg1 ... concatenated with
// the elements of args (the final argument, which must be a list).
type ValidatedApply struct {
	validatedBase
	Proc       ValidatedExpr
	PrefixArgs []ValidatedExpr
	FinalList  ValidatedExpr
}
```

**Step 3: Run test to verify it passes**

Run: `go test -run TestValidatedApply_Getters ./internal/validate/...`
Expected: PASS

**Step 4: Commit**

```
feat: add ValidatedApply node type

Typed validated expression for (apply proc arg1 ... args) with
separate fields for procedure, prefix arguments, and final list.
```

---

### Task 5: Add validateApply and register it

**Files:**
- Create: `internal/validate/validate_apply.go`
- Modify: `internal/validate/register.go`

**Step 1: Write the failing test**

Add to `internal/validate/validate_test.go`:

```go
func TestValidateApply(t *testing.T) {
	c := qt.New(t)
	env := testEnv(t)

	tcs := []struct {
		name    string
		code    string
		wantErr bool
	}{
		{"basic", "(apply f args)", false},
		{"prefix args", "(apply f 1 2 args)", false},
		{"too few args", "(apply)", true},
		{"one arg only", "(apply f)", true},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			expr := parseOne(t, tc.code)
			result := ValidateExpression(context.Background(), env, expr)
			if tc.wantErr {
				c.Assert(result.Ok(), qt.IsFalse)
			} else {
				c.Assert(result.Ok(), qt.IsTrue)
				_, ok := result.Expr.(*ValidatedApply)
				c.Assert(ok, qt.IsTrue)
			}
		})
	}
}
```

Note: Check how `testEnv` and `parseOne` work in existing validate tests — use the same helpers.

Run: `go test -run TestValidateApply ./internal/validate/...`
Expected: FAIL

**Step 2: Create `validate_apply.go`**

```go
package validate

import (
	"context"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
)

// validateApply validates (apply proc arg1 ... args)
//
// R7RS §6.10: apply calls proc with the arguments arg1 ... concatenated
// with the elements of args. At least two arguments are required (proc
// and the final list).
func validateApply(ctx context.Context, env *environment.EnvironmentFrame, pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source, elements, ok := formPrologue(pair, "apply", 2, -1, result)
	if !ok {
		return nil
	}

	// elements[0] = "apply" keyword
	// elements[1] = proc
	// elements[2..n-1] = prefix args (optional)
	// elements[n] = final list (last argument)

	proc := validateExpr(ctx, env, elements[1], result)

	// Prefix args: everything between proc and the final element
	var prefixArgs []ValidatedExpr
	for i := 2; i < len(elements)-1; i++ {
		arg := validateExpr(ctx, env, elements[i], result)
		prefixArgs = append(prefixArgs, arg)
	}

	finalList := validateExpr(ctx, env, elements[len(elements)-1], result)

	// Check all sub-validations succeeded
	if proc == nil || finalList == nil {
		return nil
	}
	for _, arg := range prefixArgs {
		if arg == nil {
			return nil
		}
	}

	return &ValidatedApply{
		validatedBase: validatedBase{formName: "apply", source: source},
		Proc:          proc,
		PrefixArgs:    prefixArgs,
		FinalList:     finalList,
	}
}
```

**Step 3: Register the validator in `register.go`**

Add after the `dynamic-wind` line:

```go
registerValidator("dynamic-wind", validateDynamicWind)
registerValidator("apply", validateApply)
```

**Step 4: Run test to verify it passes**

Run: `go test -run TestValidateApply ./internal/validate/...`
Expected: PASS

**Step 5: Commit**

```
feat: add validateApply validator

Validates (apply proc arg1 ... args) forms. Requires at least 2 args
(proc and final list). Separates prefix args from the final list arg.
```

---

### Task 6: Register apply as a compile-time binding

**Files:**
- Modify: `registry/core/specialforms.go`

**Step 1: Add apply to the special forms list**

In `specialforms.go`, find the `specialFormNames` slice (or equivalent) and add `"apply"`:

```go
// R7RS §6.10: apply for procedure application with argument list
"apply",
```

This adds `apply` to the compile-time environment so the validator recognizes it as a special form (not just a regular function call).

**Step 2: Verify it compiles and existing tests pass**

Run: `go test ./registry/core/... -count=1 -timeout 60s`
Expected: PASS (but some apply tests may now fail because the compiler doesn't handle ValidatedApply yet — that's Task 7)

Actually, this is a critical ordering concern. Once `apply` is a compile-time binding, the validator will produce `ValidatedApply` instead of `ValidatedCall`, but the compiler doesn't handle `ValidatedApply` yet. This will break existing apply usage.

**IMPORTANT: Tasks 6 and 7 must be done together in one commit.** The compile-time binding and the compiler handler must land atomically.

Move to Task 7 before committing.

---

### Task 7: Add compileValidatedApply and register it

**Files:**
- Modify: `machine/compile_validated.go` (add method)
- Modify: `machine/register.go` (register compiler)

**Step 1: Write the compiler method**

Add to `machine/compile_validated.go`:

```go
// CompileValidatedApply compiles a validated (apply proc arg1 ... args) form.
//
// R7RS §6.10: apply calls proc with the arguments arg1 ... concatenated
// with the elements of args (the final argument, which must be a list).
//
// Bytecode (non-tail):
//
//	SaveContinuation →after
//	<compile proc>          PUSH
//	<compile arg1>          PUSH
//	...
//	<compile argN>          PUSH
//	<compile finalList>              ; value = finalList
//	OpUnpackListToStack              ; stack: [proc, arg1, ..., argN, x1, x2, ...]
//	Pull                             ; value = proc
//	Apply                            ; calls proc(arg1, ..., argN, x1, x2, ...)
//	after:
//
// Tail position: same without SaveContinuation/patch.
func (p *CompileTimeContinuation) CompileValidatedApply(ctctx CompileTimeCallContext, v *validate.ValidatedApply) error {
	var saveContinuationIndex int
	if !ctctx.inTail {
		saveContinuationIndex = p.emitPatchableSaveContinuation()
	}

	// Compile proc and push to stack
	err := p.compileValidated(ctctx.NotInTail(), v.Proc)
	if err != nil {
		return err
	}
	p.AppendOperations(NewOperationPush())

	// Compile prefix args and push each
	for _, arg := range v.PrefixArgs {
		err = p.compileValidated(ctctx.NotInTail(), arg)
		if err != nil {
			return err
		}
		p.AppendOperations(NewOperationPush())
	}

	// Compile final list (stays in value register)
	err = p.compileValidated(ctctx.NotInTail(), v.FinalList)
	if err != nil {
		return err
	}

	// Flatten the list onto the eval stack
	p.AppendOperations(NewOperationUnpackListToStack())

	// Pull proc from bottom of stack, then apply
	p.AppendOperations(
		NewOperationPull(),
		NewOperationApply(),
	)

	if !ctctx.inTail {
		p.patchSaveContinuationOffset(saveContinuationIndex)
	}

	return nil
}
```

Note: Check that `ctctx.inTail` is the correct field access. Looking at the existing code, `compileValidatedCall` uses `ctctx.inTail` directly since `CompileTimeCallContext` is in the same package.

**Step 2: Register the compiler in `machine/register.go`**

Add after the `dynamic-wind` registration:

```go
registerTypedCompiler("apply", func(ctc *CompileTimeContinuation, ctctx CompileTimeCallContext, v *validate.ValidatedApply) error {
	return ctc.CompileValidatedApply(ctctx, v)
})
```

**Step 3: Now commit Tasks 6 + 7 together**

Run: `make lint && go test ./... -count=1 -timeout 120s`
Expected: PASS (all existing tests should still pass — `(apply f args)` now compiles to bytecode instead of calling PrimApply)

```
feat: compile apply as special form for proper tail recursion

Register apply as a compile-time binding with a validator and compiler.
Direct (apply proc arg1 ... args) calls now emit:
  <args> OpUnpackListToStack Pull Apply
instead of calling PrimApply via sub-context.

Tail-position apply no longer grows the Go stack. Fixes H1.
```

---

### Task 8: Add the tail recursion depth test (H1 verification)

**Files:**
- Modify: `registry/core/prim_control_test.go` (or appropriate test file)

**Step 1: Write the H1 verification test**

```go
func TestApplyTailRecursion_NoStackOverflow(t *testing.T) {
	// H1: apply in tail position must not grow the Go stack.
	// Before this fix, Go stack overflow occurred at ~300K iterations.
	// With compiled apply, this runs in constant Go stack space.
	code := `
		(define (f n) (if (zero? n) 'done (apply f (list (- n 1)))))
		(f 1000000)
	`
	result := runSchemeCodeWithTimeout(t, code, 30*time.Second)
	c := qt.New(t)
	c.Assert(result, qt.Equals, values.NewSymbol("done"))
}
```

**Step 2: Run the test**

Run: `go test -run TestApplyTailRecursion_NoStackOverflow ./registry/core/... -timeout 60s -count=1`
Expected: PASS (this is the primary H1 fix verification)

**Step 3: Commit**

```
test: verify apply tail recursion doesn't overflow Go stack (H1)

Runs 1M iterations of recursive tail-apply. Before the fix, this
caused Go stack overflow at ~300K iterations.
```

---

### Task 9: Comprehensive apply correctness tests

**Files:**
- Modify: appropriate test file (registry/core/prim_control_test.go or machine test)

**Step 1: Write the test table**

```go
func TestCompiledApply(t *testing.T) {
	c := qt.New(t)
	tcs := []struct {
		name string
		code string
		want string
	}{
		{"basic foreign", `(apply + '(1 2 3))`, "6"},
		{"case-lambda", `(apply (case-lambda ((x) x) ((x y) (+ x y))) '(3 4))`, "7"},
		{"prefix args", `(apply + 1 2 '(3 4))`, "10"},
		{"empty final list", `(apply + 1 2 '())`, "3"},
		{"single arg list", `(apply car '((1 2 3)))`, "1"},
		{"non-tail position", `(+ 1 (apply + '(2 3)))`, "6"},
		{"nested apply", `(apply apply (list + '(1 2)))`, "3"},
		{"first-class apply", `(let ((a apply)) (a + '(1 2)))`, "3"},
		{"apply lambda", `(apply (lambda (x y) (+ x y)) '(3 4))`, "7"},
		{"apply with call/cc",
			`(call-with-current-continuation (lambda (k) (apply k '(42))))`, "42"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := runSchemeCode(t, tc.code)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

func TestCompiledApply_Errors(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"improper list", `(apply + '(1 . 2))`},
		{"non-list final arg", `(apply + 42)`},
		{"too few args", `(apply +)`},
		{"non-procedure", `(apply 42 '(1 2))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeCodeExpectError(t, tc.code)
		})
	}
}
```

Note: Adapt `runSchemeCode` / `runSchemeCodeExpectError` to match the actual test helpers available in the test file. Check `registry/core/test_helpers_test.go` for exact signatures.

The `"too few args"` test needs special attention: `(apply +)` has only one argument to apply (just proc, no list). The validator requires at least 2 args (proc + final list). Verify this produces a compile-time error, not a runtime error.

**Step 2: Run tests**

Run: `go test -run "TestCompiledApply" ./registry/core/... -timeout 60s -count=1`
Expected: PASS

**Step 3: Commit**

```
test: comprehensive compiled apply correctness tests

Table-driven tests for compiled apply: basic, prefix args, empty list,
nested, first-class, call/cc, error cases.
```

---

### Task 10: Run full test suite and lint

**Step 1: Run lint**

Run: `make lint`
Expected: PASS

**Step 2: Run full tests**

Run: `make test`
Expected: PASS

**Step 3: Run coverage check**

Run: `make covercheck`
Expected: PASS

If any failures, fix and commit individually.

---

## Summary of changes by file

| File | Change |
|------|--------|
| `machine/opcode.go` | Add `OpUnpackListToStack` constant + name |
| `machine/operation_unpack_list_to_stack.go` | New operation type |
| `machine/machine_context.go` | Add `OpUnpackListToStack` case in `Run()` |
| `machine/native_template.go` | Add cases in both converter functions |
| `internal/validate/validated_forms.go` | Add `ValidatedApply` struct |
| `internal/validate/validate_apply.go` | New validator function |
| `internal/validate/register.go` | Register `validateApply` |
| `registry/core/specialforms.go` | Add `"apply"` to compile-time bindings |
| `machine/compile_validated.go` | Add `CompileValidatedApply` method |
| `machine/register.go` | Register typed compiler for `"apply"` |
| Test files | H1 depth test + correctness table |
