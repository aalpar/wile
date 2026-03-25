# Core `let` Compilation — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Make `let`, `let*`, `letrec`, `letrec*` core compiled forms with `ValidatedLet`/`ValidatedLetStar`/`ValidatedLetrec` types, eliminating lambda overhead for all binding forms.

**Architecture:** New `OpPushEnv` opcode allocates a local env frame; `StoreLocal` populates slots; existing `OpPopEnv` cleans up. Expander, validator, and compiler each get new handlers following the `dynamic-wind` precedent (latest core form added via this pipeline).

**Tech Stack:** Go, Scheme (bootstrap macros), Wile's expander/validator/compiler/VM pipeline

**Design doc:** `plans/CORE-LET.md`

---

### Task 1: Add `OpPushEnv` Opcode

**Files:**
- Modify: `machine/opcode.go` (add constant + table entry)
- Modify: `machine/machine_context.go` (add `Run()` case)
- Modify: `machine/operations_winding.go` (add operation struct + constructor)
- Test: `machine/operation_test.go`

**Step 1: Write the failing test**

Add to `machine/operation_test.go`:

```go
func TestOperationPushEnvEqualTo(t *testing.T) {
	a := NewOperationPushEnv(3)
	b := NewOperationPushEnv(3)
	qt.Assert(t, a.EqualTo(b), qt.IsTrue)
	qt.Assert(t, a.EqualTo(NewOperationPopEnv()), qt.IsFalse)
}
```

**Step 2: Run test to verify it fails**

Run: `go test -run TestOperationPushEnvEqualTo -v ./machine/`
Expected: FAIL — `NewOperationPushEnv` not defined

**Step 3: Implement OpPushEnv**

In `machine/opcode.go`, add `OpPushEnv` in the **Wave 2 block** (single-operand operations, after `OpPeekK`):

```go
OpPeekK
OpPushEnv // Push new env frame with Arg local slots
```

`OpPushEnv` carries an `Arg` (slot count), so it belongs in Wave 2 (single-operand), not Wave 1 (zero-operand) where `OpPopEnv` lives.

Add table entry in `opcodeTable`:

```go
OpPushEnv: {name: "PushEnv"},
```

In `machine/operations_winding.go`, add after `OperationPopEnv`:

```go
// --- PushEnv ---

// OperationPushEnv allocates a new environment frame with the specified
// number of local binding slots and chains it to the current environment.
// Paired with OpPopEnv which restores the parent.
type OperationPushEnv struct {
	OperationBase
	SlotCount int
}

func NewOperationPushEnv(slotCount int) *OperationPushEnv {
	return &OperationPushEnv{
		OperationBase: NewOperationBaseWithGoName("operation:push-env", "PushEnv"),
		SlotCount:     slotCount,
	}
}

func (p *OperationPushEnv) EqualTo(other values.Value) bool {
	v, ok := other.(*OperationPushEnv)
	return sameType(p, v, ok) && fieldMatches(ok, p.SlotCount, v.SlotCount)
}
```

In `machine/machine_context.go` `Run()`, add case in the Wave 2 section (near `OpPeekK`):

```go
case OpPushEnv:
	slotCount := int(instr.Arg)
	lenv := environment.NewLocalEnvironment(slotCount)
	mc.env = environment.NewEnvironmentFrameWithParent(lenv, mc.env)
	mc.envPooled = false
	mc.pc++
```

In `machine/native_template.go`, add conversion cases:

In `operationToInstruction()`, add in the Wave 2 section:

```go
case *OperationPushEnv:
	return Instruction{Op: OpPushEnv, Arg: int32(v.SlotCount)}, true
```

In `instructionToOperation()`, add in the Wave 2 section:

```go
case OpPushEnv:
	return NewOperationPushEnv(int(instr.Arg))
```

**Step 4: Run test to verify it passes**

Run: `go test -run TestOperationPushEnvEqualTo -v ./machine/`
Expected: PASS

**Step 5: Run full test suite**

Run: `make lint && go test ./machine/ ./environment/`
Expected: PASS (no regressions)

---

### Task 2: Add `ValidatedLet`, `ValidatedLetStar`, and `ValidatedLetrec` Types

**Files:**
- Modify: `internal/validate/validated_forms.go` (add types)
- Test: `internal/validate/validated_forms_test.go`

**Step 1: Write the failing test**

Add to `internal/validate/validated_forms_test.go`:

```go
func TestValidatedLet(t *testing.T) {
	sym := syntax.NewSyntaxSymbol(values.NewSymbol("x"), nil)
	init := &ValidatedLiteral{
		validatedBase: validatedBase{formName: "@literal"},
		Value:         syntax.NewSyntaxObject(values.NewInteger(1), nil),
	}
	body := &ValidatedSymbol{
		validatedBase: validatedBase{formName: "@symbol"},
		Symbol:        sym,
	}

	vl := &ValidatedLet{
		validatedBase: validatedBase{formName: "let"},
		Bindings:      []ValidatedLetBinding{{Name: sym, Init: init}},
		body:          []ValidatedExpr{body},
	}
	c := qt.New(t)
	c.Assert(vl.FormName(), qt.Equals, "let")
	c.Assert(len(vl.Bindings), qt.Equals, 1)
	c.Assert(vl.Bindings[0].Name, qt.Equals, sym)
	c.Assert(vl.Bindings[0].Mutable, qt.IsFalse)
	c.Assert(len(vl.Body()), qt.Equals, 1)
}

func TestValidatedLetStar(t *testing.T) {
	sym := syntax.NewSyntaxSymbol(values.NewSymbol("x"), nil)
	init := &ValidatedLiteral{
		validatedBase: validatedBase{formName: "@literal"},
		Value:         syntax.NewSyntaxObject(values.NewInteger(1), nil),
	}
	body := &ValidatedSymbol{
		validatedBase: validatedBase{formName: "@symbol"},
		Symbol:        sym,
	}

	vls := &ValidatedLetStar{
		validatedBase: validatedBase{formName: "let*"},
		Bindings:      []ValidatedLetBinding{{Name: sym, Init: init}},
		body:          []ValidatedExpr{body},
	}
	c := qt.New(t)
	c.Assert(vls.FormName(), qt.Equals, "let*")
	c.Assert(len(vls.Body()), qt.Equals, 1)
}

func TestValidatedLetrec(t *testing.T) {
	sym := syntax.NewSyntaxSymbol(values.NewSymbol("f"), nil)
	init := &ValidatedLiteral{
		validatedBase: validatedBase{formName: "@literal"},
		Value:         syntax.NewSyntaxObject(values.NewInteger(1), nil),
	}
	body := &ValidatedSymbol{
		validatedBase: validatedBase{formName: "@symbol"},
		Symbol:        sym,
	}

	vl := &ValidatedLetrec{
		validatedBase: validatedBase{formName: "letrec"},
		Bindings:      []ValidatedLetBinding{{Name: sym, Init: init}},
		body:          []ValidatedExpr{body},
	}
	c := qt.New(t)
	c.Assert(vl.FormName(), qt.Equals, "letrec")
	c.Assert(vl.LetrecStar, qt.IsFalse)
	c.Assert(len(vl.Body()), qt.Equals, 1)

	vls := &ValidatedLetrec{
		validatedBase: validatedBase{formName: "letrec*"},
		Bindings:      []ValidatedLetBinding{{Name: sym, Init: init}},
		LetrecStar:    true,
		body:          []ValidatedExpr{body},
	}
	c.Assert(vls.LetrecStar, qt.IsTrue)
}
```

**Step 2: Run test to verify it fails**

Run: `go test -run 'TestValidatedLet' -v ./internal/validate/`
Expected: FAIL — types not defined

**Step 3: Implement types**

Add to `internal/validate/validated_forms.go` after `ValidatedApply`:

```go
// ValidatedLetBinding represents a single (name init-expr) binding pair.
// Mutable is true if the binding is targeted by set! in the body.
type ValidatedLetBinding struct {
	Name    *syntax.SyntaxSymbol
	Init    ValidatedExpr
	Mutable bool
}

// ValidatedLet represents (let ((name val) ...) body ...).
type ValidatedLet struct {
	validatedBase
	Bindings []ValidatedLetBinding
	body     []ValidatedExpr
}

// Body returns the body expressions.
func (p *ValidatedLet) Body() []ValidatedExpr {
	return p.body
}

// ValidatedLetStar represents (let* ((name val) ...) body ...).
type ValidatedLetStar struct {
	validatedBase
	Bindings []ValidatedLetBinding
	body     []ValidatedExpr
}

// Body returns the body expressions.
func (p *ValidatedLetStar) Body() []ValidatedExpr {
	return p.body
}

// ValidatedLetrec represents (letrec ((name val) ...) body ...)
// and (letrec* ((name val) ...) body ...).
// LetrecStar distinguishes the two: false = letrec, true = letrec*.
// Tag is non-nil for named let (compiled as letrec).
type ValidatedLetrec struct {
	validatedBase
	Bindings   []ValidatedLetBinding
	LetrecStar bool
	Tag        *syntax.SyntaxSymbol
	body       []ValidatedExpr
}

// Body returns the body expressions.
func (p *ValidatedLetrec) Body() []ValidatedExpr {
	return p.body
}
```

**Step 4: Run test to verify it passes**

Run: `go test -run 'TestValidatedLet' -v ./internal/validate/`
Expected: PASS

---

### Task 3: Add Validators for All Binding Forms

**Files:**
- Create: `internal/validate/validate_let.go`
- Modify: `internal/validate/register.go` (register validators)
- Test: `internal/validate/validate_test.go` (add test cases to existing table-driven structure, or new section)

**Step 1: Write the failing test**

Add a new test function in `internal/validate/validate_test.go` (following existing patterns — use `testhelpers` if the file already uses end-to-end validation, or add a unit test):

```go
func TestValidateLet(t *testing.T) {
	tcs := []struct {
		name     string
		code     string
		wantForm string
		wantErr  bool
	}{
		{name: "basic let", code: `(let ((x 1)) x)`, wantForm: "let"},
		{name: "multiple bindings", code: `(let ((x 1) (y 2)) (+ x y))`, wantForm: "let"},
		{name: "let star", code: `(let* ((x 1) (y x)) y)`, wantForm: "let*"},
		{name: "empty bindings", code: `(let () 1)`, wantForm: "let"},
		{name: "missing body", code: `(let ((x 1)))`, wantErr: true},
		{name: "malformed binding", code: `(let (x) x)`, wantErr: true},
		{name: "non-symbol name", code: `(let ((1 2)) 3)`, wantErr: true},
		{name: "letrec", code: `(letrec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))) (f 5))`, wantForm: "letrec"},
		{name: "letrec star", code: `(letrec* ((x 1) (y (+ x 1))) y)`, wantForm: "letrec*"},
		{name: "letrec missing body", code: `(letrec ((x 1)))`, wantErr: true},
		{name: "named let", code: `(let loop ((n 5) (acc 1)) (if (= n 0) acc (loop (- n 1) (* acc n))))`, wantForm: "letrec"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			// Parse, expand, validate — use existing test infrastructure
			// The exact pattern depends on what's already in validate_test.go
			// Follow the existing pattern for validateIf/validateDefine tests
		})
	}
}
```

NOTE: The exact test infrastructure depends on what `validate_test.go` already uses. Read the file and follow its pattern. The tests above are the cases to cover; adapt the test body to match.

**Step 2: Run test to verify it fails**

Run: `go test -run TestValidateLet -v ./internal/validate/`
Expected: FAIL — validator not registered, `let` falls through as a call

**Step 3: Implement the validator**

Create `internal/validate/validate_let.go`:

```go
package validate

import (
	"context"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
)

// validateLetBindings parses and validates a ((name val) ...) binding list.
// Each binding must be a two-element list with a symbol as the first element.
// Init expressions are validated in the provided env.
//
// NOTE: bindingsPair must already be type-asserted from the original
// syntax.SyntaxValue. The caller handles the empty-list check.
// The list-collection function is collectList (takes *syntax.SyntaxPair).
func validateLetBindings(
	ctx context.Context,
	env *environment.EnvironmentFrame,
	bindingsPair *syntax.SyntaxPair,
	formName string,
	result *ValidationResult,
) ([]ValidatedLetBinding, bool) {
	bindingsList, improper := collectList(bindingsPair)
	if improper {
		result.addError(getSourceContext(bindingsPair), formName,
			formName+" bindings must be a proper list")
		return nil, false
	}

	var bindings []ValidatedLetBinding
	allOk := true
	for _, bindingExpr := range bindingsList {
		pair, ok := bindingExpr.(*syntax.SyntaxPair)
		if !ok || syntax.IsSyntaxEmptyList(pair) {
			result.addError(getSourceContext(bindingExpr), formName,
				formName+" binding must be (name init)")
			allOk = false
			continue
		}

		elems, imp := collectList(pair)
		if imp || len(elems) != 2 {
			result.addError(getSourceContext(bindingExpr), formName,
				formName+" binding must be (name init)")
			allOk = false
			continue
		}

		nameSym := asSyntaxSymbol(elems[0])
		if nameSym == nil {
			result.addError(getSourceContext(elems[0]), formName,
				formName+" binding name must be a symbol")
			allOk = false
			continue
		}

		init := validateExpr(ctx, env, elems[1], result)
		if init == nil {
			allOk = false
			continue
		}

		bindings = append(bindings, ValidatedLetBinding{Name: nameSym, Init: init})
	}

	if !allOk {
		return nil, false
	}
	return bindings, true
}

// createLetValidationEnv creates a child environment with let bindings
// for body validation. Mirrors createLambdaValidationEnv.
func createLetValidationEnv(
	env *environment.EnvironmentFrame,
	bindings []ValidatedLetBinding,
) *environment.EnvironmentFrame {
	if env == nil || len(bindings) == 0 {
		return env
	}
	lenv := environment.NewLocalEnvironment(0)
	childEnv := environment.NewEnvironmentFrameWithParent(lenv, env)
	for _, b := range bindings {
		childEnv.MaybeCreateLocalBindingWithScopes(
			b.Name.Sym,
			environment.BindingTypeVariable,
			b.Name.Scopes(),
			b.Name.SourceContext(),
		)
	}
	return childEnv
}

// validateLet validates (let ((name val) ...) body ...).
func validateLet(
	ctx context.Context,
	env *environment.EnvironmentFrame,
	pair *syntax.SyntaxPair,
	result *ValidationResult,
) ValidatedExpr {
	source, elements, ok := formPrologue(pair, "let", 2, -1, result)
	if !ok {
		return nil
	}

	// elements[0] = "let", elements[1] = bindings or tag, elements[2..] = body

	// Detect named let: (let tag ((bindings...) ...) body ...)
	if sym := asSyntaxSymbol(elements[1]); sym != nil {
		return validateNamedLet(ctx, env, source, sym, elements, result)
	}

	// Body requires at least one expression
	if len(elements) < 3 {
		result.addError(source, "let", "let requires at least one body expression")
		return nil
	}

	// Validate bindings — init exprs validated in current env (not child).
	// Type-assert to *SyntaxPair before calling collectList.
	var bindings []ValidatedLetBinding
	if syntax.IsSyntaxEmptyList(elements[1]) {
		// (let () body ...) — empty bindings, no-op
	} else {
		bindingsPair, pairOk := elements[1].(*syntax.SyntaxPair)
		if !pairOk {
			result.addError(getSourceContext(elements[1]), "let",
				"let bindings must be a list")
			return nil
		}
		var bindOk bool
		bindings, bindOk = validateLetBindings(ctx, env, bindingsPair, "let", result)
		if !bindOk {
			return nil
		}
	}

	// Create child env for body validation
	childEnv := createLetValidationEnv(env, bindings)

	body, ok := validateBodySlice(ctx, childEnv, elements, 2, result)
	if !ok {
		return nil
	}

	return &ValidatedLet{
		validatedBase: validatedBase{formName: "let", source: source},
		Bindings:      bindings,
		body:          body,
	}
}

// validateNamedLet validates (let tag ((name val) ...) body ...)
// by producing a ValidatedLetrec equivalent to
// (letrec ((tag (lambda (names...) body...))) (tag vals...)).
//
// The body of the returned ValidatedLetrec is a single ValidatedCall
// of the tag with the original init values as arguments. The compiler
// does not need special-case logic for named let — standard body
// compilation handles the call naturally.
func validateNamedLet(
	ctx context.Context,
	env *environment.EnvironmentFrame,
	source *syntax.SourceContext,
	tag *syntax.SyntaxSymbol,
	elements []syntax.SyntaxValue,
	result *ValidationResult,
) ValidatedExpr {
	if len(elements) < 4 {
		result.addError(source, "let",
			"named let requires bindings and at least one body expression")
		return nil
	}

	// elements[2] = bindings, elements[3..] = body

	// Validate bindings in OUTER env (init exprs don't see tag).
	// We need the raw binding list to type-assert before calling collectList.
	bindingsStx := elements[2]
	bindingsPair, pairOk := bindingsStx.(*syntax.SyntaxPair)
	if !pairOk && !syntax.IsSyntaxEmptyList(bindingsStx) {
		result.addError(getSourceContext(bindingsStx), "let",
			"named let bindings must be a list")
		return nil
	}
	var bindings []ValidatedLetBinding
	if pairOk && !syntax.IsSyntaxEmptyList(bindingsPair) {
		var ok bool
		bindings, ok = validateLetBindings(ctx, env, bindingsPair, "let", result)
		if !ok {
			return nil
		}
	}

	// Create child env with tag visible (for recursive calls in body)
	lenv := environment.NewLocalEnvironment(0)
	tagEnv := environment.NewEnvironmentFrameWithParent(lenv, env)
	tagEnv.MaybeCreateLocalBindingWithScopes(
		tag.Sym,
		environment.BindingTypeVariable,
		tag.Scopes(),
		tag.SourceContext(),
	)

	// Create body env with tag + binding names visible
	bodyEnv := createLetValidationEnv(tagEnv, bindings)

	body, ok := validateBodySlice(ctx, bodyEnv, elements, 3, result)
	if !ok {
		return nil
	}

	// Build the lambda init: (lambda (names...) body...)
	lambdaInit := buildNamedLetLambda(bindings, body, source)

	// Build the call: (tag val1 val2 ...)
	// The callee is a symbol reference to tag (resolved in the letrec frame).
	// The args are the original init values (already validated in outer env).
	callArgs := make([]ValidatedExpr, len(bindings))
	for i, b := range bindings {
		callArgs[i] = b.Init
	}
	callExpr := &ValidatedCall{
		validatedBase: validatedBase{formName: "@call", source: source},
		proc: &ValidatedSymbol{
			validatedBase: validatedBase{formName: "@symbol"},
			Symbol:        tag,
		},
		args: callArgs,
	}

	return &ValidatedLetrec{
		validatedBase: validatedBase{formName: "letrec", source: source},
		Bindings:      []ValidatedLetBinding{{Name: tag, Init: lambdaInit}},
		Tag:           tag,
		body:          []ValidatedExpr{callExpr},
	}
}

// buildNamedLetLambda constructs a ValidatedLambda from the binding names
// (as parameters) and the validated body. No validation is needed —
// the bindings and body were already validated by the caller.
func buildNamedLetLambda(
	bindings []ValidatedLetBinding,
	body []ValidatedExpr,
	source *syntax.SourceContext,
) ValidatedExpr {
	params := &ValidatedParams{
		Required: make([]*syntax.SyntaxSymbol, len(bindings)),
	}
	for i, b := range bindings {
		params.Required[i] = b.Name
	}
	return &ValidatedLambda{
		validatedBase: validatedBase{formName: "lambda", source: source},
		params:        params,
		body:          body,
	}
}

// validateLetStar validates (let* ((name val) ...) body ...).
func validateLetStar(
	ctx context.Context,
	env *environment.EnvironmentFrame,
	pair *syntax.SyntaxPair,
	result *ValidationResult,
) ValidatedExpr {
	source, elements, ok := formPrologue(pair, "let*", 2, -1, result)
	if !ok {
		return nil
	}

	if len(elements) < 3 {
		result.addError(source, "let*", "let* requires at least one body expression")
		return nil
	}

	// For let*, we need to validate each init with preceding bindings visible.
	// Parse the bindings list first, then validate incrementally.
	// Type-assert before calling collectList (*syntax.SyntaxPair required).
	if syntax.IsSyntaxEmptyList(elements[1]) {
		// (let* () body ...) — empty bindings, skip to body
		body, ok := validateBodySlice(ctx, env, elements, 2, result)
		if !ok {
			return nil
		}
		return &ValidatedLetStar{
			validatedBase: validatedBase{formName: "let*", source: source},
			body:          body,
		}
	}
	bindingsListPair, pairOk := elements[1].(*syntax.SyntaxPair)
	if !pairOk {
		result.addError(getSourceContext(elements[1]), "let*",
			"let* bindings must be a list")
		return nil
	}
	bindingsListRaw, improper := collectList(bindingsListPair)
	if improper {
		result.addError(getSourceContext(elements[1]), "let*",
			"let* bindings must be a proper list")
		return nil
	}

	// Build child env incrementally
	lenv := environment.NewLocalEnvironment(0)
	childEnv := environment.NewEnvironmentFrameWithParent(lenv, env)

	var bindings []ValidatedLetBinding
	allOk := true
	for _, bindingExpr := range bindingsListRaw {
		bPair, ok := bindingExpr.(*syntax.SyntaxPair)
		if !ok || syntax.IsSyntaxEmptyList(bPair) {
			result.addError(getSourceContext(bindingExpr), "let*",
				"let* binding must be (name init)")
			allOk = false
			continue
		}

		elems, imp := collectList(bPair)
		if imp || len(elems) != 2 {
			result.addError(getSourceContext(bindingExpr), "let*",
				"let* binding must be (name init)")
			allOk = false
			continue
		}

		nameSym := asSyntaxSymbol(elems[0])
		if nameSym == nil {
			result.addError(getSourceContext(elems[0]), "let*",
				"let* binding name must be a symbol")
			allOk = false
			continue
		}

		// Validate init in current childEnv (sees preceding bindings)
		init := validateExpr(ctx, childEnv, elems[1], result)
		if init == nil {
			allOk = false
			continue
		}

		bindings = append(bindings, ValidatedLetBinding{Name: nameSym, Init: init})

		// Add this binding to childEnv for subsequent inits
		childEnv.MaybeCreateLocalBindingWithScopes(
			nameSym.Sym,
			environment.BindingTypeVariable,
			nameSym.Scopes(),
			nameSym.SourceContext(),
		)
	}

	if !allOk {
		return nil
	}

	// Validate body with all bindings visible
	body, ok := validateBodySlice(ctx, childEnv, elements, 2, result)
	if !ok {
		return nil
	}

	return &ValidatedLetStar{
		validatedBase: validatedBase{formName: "let*", source: source},
		Bindings:      bindings,
		body:          body,
	}
}

// validateLetrec validates (letrec ((name val) ...) body ...).
func validateLetrec(
	ctx context.Context,
	env *environment.EnvironmentFrame,
	pair *syntax.SyntaxPair,
	result *ValidationResult,
) ValidatedExpr {
	return validateLetrecCommon(ctx, env, pair, "letrec", false, result)
}

// validateLetrecStar validates (letrec* ((name val) ...) body ...).
func validateLetrecStar(
	ctx context.Context,
	env *environment.EnvironmentFrame,
	pair *syntax.SyntaxPair,
	result *ValidationResult,
) ValidatedExpr {
	return validateLetrecCommon(ctx, env, pair, "letrec*", true, result)
}

// validateLetrecCommon is shared logic for letrec and letrec*.
func validateLetrecCommon(
	ctx context.Context,
	env *environment.EnvironmentFrame,
	pair *syntax.SyntaxPair,
	formName string,
	isStar bool,
	result *ValidationResult,
) ValidatedExpr {
	source, elements, ok := formPrologue(pair, formName, 2, -1, result)
	if !ok {
		return nil
	}

	if len(elements) < 3 {
		result.addError(source, formName,
			formName+" requires at least one body expression")
		return nil
	}

	// Create child env with ALL bindings visible BEFORE validating inits.
	// This is the key difference from let/let*: recursive refs are allowed.
	// Type-assert before calling collectList (*syntax.SyntaxPair required).
	if syntax.IsSyntaxEmptyList(elements[1]) {
		body, ok := validateBodySlice(ctx, env, elements, 2, result)
		if !ok {
			return nil
		}
		return &ValidatedLetrec{
			validatedBase: validatedBase{formName: formName, source: source},
			LetrecStar:    isStar,
			body:          body,
		}
	}
	bindingsListPair, pairOk := elements[1].(*syntax.SyntaxPair)
	if !pairOk {
		result.addError(getSourceContext(elements[1]), formName,
			formName+" bindings must be a list")
		return nil
	}
	bindingsListRaw, improper := collectList(bindingsListPair)
	if improper {
		result.addError(getSourceContext(elements[1]), formName,
			formName+" bindings must be a proper list")
		return nil
	}

	// First pass: collect names and create child env
	lenv := environment.NewLocalEnvironment(0)
	childEnv := environment.NewEnvironmentFrameWithParent(lenv, env)

	var nameSyms []*syntax.SyntaxSymbol
	var initExprs []syntax.SyntaxValue
	allOk := true
	for _, bindingExpr := range bindingsListRaw {
		bPair, ok := bindingExpr.(*syntax.SyntaxPair)
		if !ok || syntax.IsSyntaxEmptyList(bPair) {
			result.addError(getSourceContext(bindingExpr), formName,
				formName+" binding must be (name init)")
			allOk = false
			continue
		}

		elems, imp := collectList(bPair)
		if imp || len(elems) != 2 {
			result.addError(getSourceContext(bindingExpr), formName,
				formName+" binding must be (name init)")
			allOk = false
			continue
		}

		nameSym := asSyntaxSymbol(elems[0])
		if nameSym == nil {
			result.addError(getSourceContext(elems[0]), formName,
				formName+" binding name must be a symbol")
			allOk = false
			continue
		}

		nameSyms = append(nameSyms, nameSym)
		initExprs = append(initExprs, elems[1])

		childEnv.MaybeCreateLocalBindingWithScopes(
			nameSym.Sym,
			environment.BindingTypeVariable,
			nameSym.Scopes(),
			nameSym.SourceContext(),
		)
	}

	if !allOk {
		return nil
	}

	// Second pass: validate init expressions in child env (all names visible)
	var bindings []ValidatedLetBinding
	for i, initExpr := range initExprs {
		init := validateExpr(ctx, childEnv, initExpr, result)
		if init == nil {
			allOk = false
			continue
		}
		bindings = append(bindings, ValidatedLetBinding{Name: nameSyms[i], Init: init})
	}

	if !allOk {
		return nil
	}

	// Validate body in child env
	body, ok := validateBodySlice(ctx, childEnv, elements, 2, result)
	if !ok {
		return nil
	}

	return &ValidatedLetrec{
		validatedBase: validatedBase{formName: formName, source: source},
		Bindings:      bindings,
		LetrecStar:    isStar,
		body:          body,
	}
}
```

NOTE: The list-collection function is `collectList(pair *syntax.SyntaxPair)` in `validate.go:127`. All call sites must type-assert `syntax.SyntaxValue` to `*syntax.SyntaxPair` before calling. `asSyntaxSymbol` and `getSourceContext` are in `validate.go` and accessible from any file in the `validate` package. `buildNamedLetLambda` is defined alongside `validateNamedLet` above — it constructs a `ValidatedLambda` from binding names and validated body.

Register in `internal/validate/register.go`, add to the core forms block:

```go
registerValidator("let", validateLet)
registerValidator("let*", validateLetStar)
registerValidator("letrec", validateLetrec)
registerValidator("letrec*", validateLetrecStar)
```

**Step 4: Run test to verify it passes**

Run: `go test -run TestValidateLet -v ./internal/validate/`
Expected: PASS

**Step 5: Run full validate tests**

Run: `go test -v ./internal/validate/`
Expected: PASS

---

### Task 3b: Add Mutability Tracking to Validators

**Depends on:** Task 3 (validators must exist first)

**Files:**
- Modify: `internal/validate/errors.go` (extend `ValidationResult`)
- Modify: `internal/validate/validate_set.go` (resolve and mark)
- Modify: `internal/validate/validate_let.go` (check after body validation)
- Test: `internal/validate/validate_test.go` or `internal/validate/validate_let_test.go`

**Step 1: Write the failing test**

```go
func TestLetMutabilityTracking(t *testing.T) {
	tcs := []struct {
		name        string
		code        string
		wantMutable []bool // per binding, in order
	}{
		{
			name:        "no set",
			code:        `(let ((x 1)) x)`,
			wantMutable: []bool{false},
		},
		{
			name:        "set target",
			code:        `(let ((x 1)) (set! x 2) x)`,
			wantMutable: []bool{true},
		},
		{
			name:        "partial mutation",
			code:        `(let ((x 1) (y 2)) (set! x 3) y)`,
			wantMutable: []bool{true, false},
		},
		{
			name:        "nested set targets outer",
			code:        `(let ((x 1)) (let ((y 2)) (set! x 3)) x)`,
			wantMutable: []bool{true}, // outer x is mutated
		},
		{
			name:        "shadow same name",
			code:        `(let ((x 1)) (let ((x 2)) (set! x 3)) x)`,
			wantMutable: []bool{false}, // outer x is NOT mutated; inner x is
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			// Parse, expand, validate using existing test infrastructure.
			// Extract the ValidatedLet from the result.
			// Check each binding's Mutable flag against tc.wantMutable.
			// Follow the pattern used by TestValidateLet in Task 3.
		})
	}
}
```

NOTE: The exact test body depends on how the existing test infrastructure accesses validated expressions. The key is to extract the `ValidatedLet` and check `Bindings[i].Mutable` for each binding.

**Step 2: Extend `ValidationResult`**

In `internal/validate/errors.go`, add:

```go
type ValidationResult struct {
	Expr            ValidatedExpr
	Errors          []ValidationError
	mutatedBindings map[*environment.Binding]bool
}

// markMutated records that a binding is targeted by set!.
func (p *ValidationResult) markMutated(b *environment.Binding) {
	if p.mutatedBindings == nil {
		p.mutatedBindings = make(map[*environment.Binding]bool)
	}
	p.mutatedBindings[b] = true
}

// isMutated returns true if the binding was targeted by set!.
func (p *ValidationResult) isMutated(b *environment.Binding) bool {
	return p.mutatedBindings[b]
}
```

**Step 3: Resolve and mark in `validateSetBang`**

In `internal/validate/validate_set.go`, add binding resolution after the existing validation:

```go
func validateSetBang(ctx context.Context, env *environment.EnvironmentFrame,
	pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	// ... existing code up to return ...

	// Resolve the target binding for mutability tracking.
	// Opportunistic: if resolution fails, the compiler catches the error.
	binding := env.GetBindingWithScopes(name.Sym, name.Scopes())
	if binding != nil {
		result.markMutated(binding)
	}

	return &ValidatedSetBang{
		validatedBase: validatedBase{formName: "set!", source: source},
		Name:          name,
		subExp:        value,
	}
}
```

**Step 4: Mark mutable bindings in let validators**

In `internal/validate/validate_let.go`, add a shared helper and call it after body validation in each validator:

```go
// markMutableBindings checks which let bindings were targeted by set!
// in the body and marks them accordingly.
func markMutableBindings(
	childEnv *environment.EnvironmentFrame,
	bindings []ValidatedLetBinding,
	result *ValidationResult,
) {
	for i, b := range bindings {
		binding := childEnv.GetBindingWithScopes(b.Name.Sym, b.Name.Scopes())
		if binding != nil && result.isMutated(binding) {
			bindings[i].Mutable = true
		}
	}
}
```

Call `markMutableBindings(childEnv, bindings, result)` after `validateBodySlice` in:
- `validateLet`
- `validateLetStar`
- `validateLetrecCommon`

**Step 5: Run tests**

Run: `go test -run TestLetMutabilityTracking -v ./internal/validate/`
Expected: PASS

Run: `go test -v ./internal/validate/`
Expected: PASS (no regressions)

---

### Task 4: Add Expanders for All Binding Forms

**Files:**
- Create: `machine/expander_let.go`
- Modify: `machine/primitive_expanders_registry.go` (register expanders)
- Test: `machine/expander_let_test.go`

**Step 1: Write the failing test**

Create `machine/expander_let_test.go`. Follow the pattern in existing expander tests (e.g., `machine/hygiene_test.go` or `machine/let_shadow_macro_test.go`). The key tests:

```go
func TestExpandLetForm(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{name: "basic let", code: `(let ((x 1)) x)`, want: values.NewInteger(1)},
		{name: "multiple bindings", code: `(let ((x 1) (y 2)) (+ x y))`, want: values.NewInteger(3)},
		{name: "let bindings dont see each other", code: `(let ((x 10)) (let ((x 1) (y x)) y))`, want: values.NewInteger(10)},
		{name: "empty bindings", code: `(let () 42)`, want: values.NewInteger(42)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.want)
		})
	}
}

func TestExpandLetStarForm(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{name: "basic let*", code: `(let* ((x 1)) x)`, want: values.NewInteger(1)},
		{name: "sequential visibility", code: `(let* ((x 1) (y (+ x 1))) y)`, want: values.NewInteger(2)},
		{name: "empty bindings", code: `(let* () 42)`, want: values.NewInteger(42)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.want)
		})
	}
}

func TestExpandLetrecForm(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{name: "recursive", code: `(letrec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))) (f 5))`, want: values.NewInteger(120)},
		{name: "mutual recursion", code: `(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))) (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))) (even? 10))`, want: values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.want)
		})
	}
}

func TestExpandLetrecStarForm(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{name: "sequential with forward ref", code: `(letrec* ((x 1) (y (+ x 1))) y)`, want: values.NewInteger(2)},
		{name: "forward ref via closure", code: `(letrec* ((f (lambda () g)) (g 42)) (f))`, want: values.NewInteger(42)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.want)
		})
	}
}

func TestExpandNamedLet(t *testing.T) {
	// Named let still works (now compiled as letrec directly)
	result, err := testhelpers.RunSchemeCode(t, `(let loop ((n 5) (acc 1)) (if (= n 0) acc (loop (- n 1) (* acc n))))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewInteger(120))
}
```

NOTE: These tests will initially fail because the expanders aren't registered yet. You may need to implement Tasks 3-5 together before the end-to-end tests pass.

**Step 2: Implement expanders**

Create `machine/expander_let.go`:

The expander for each form needs to:
1. Parse the form
2. For `let`: detect named let → expand to `(letrec ...)`; plain let → scope names+body, expand inits in outer env, body in child env
3. For `let*`: sequential binding scoping
4. For `letrec`/`letrec*`: scope everything (names + inits + body), expand in child env

Follow the pattern of `expandLambdaForm` in `machine/expander_lambda.go`:
- Create a fresh scope via `syntax.NewScopeWithLabel("let")` (or "letrec" etc.)
- Add scope to bound names and body (and inits for letrec) via `syntax.AddScopeToSyntax`
- Create child env with `environment.NewLocalEnvironment` + `NewEnvironmentFrameWithParent`
- Expand body via `ExpandBodyWithDefineSyntax`
- Reconstruct syntax using `syntax.NewSyntaxCons`, `syntax.NewSyntaxList`, etc.

Register in `machine/primitive_expanders_registry.go`, add to the primitives list:

```go
{"let", (*ExpanderTimeContinuation).expandLetForm},
{"let*", (*ExpanderTimeContinuation).expandLetStarForm},
{"letrec", (*ExpanderTimeContinuation).expandLetrecForm},
{"letrec*", (*ExpanderTimeContinuation).expandLetrecStarForm},
```

**Step 3: Run tests**

Run: `go test -run 'TestExpandLet' -v ./machine/`
Expected: Will fail until compiler is registered (Task 5)

---

### Task 5: Add Compilers for All Binding Forms

**Files:**
- Create: `machine/compile_let.go`
- Modify: `machine/register.go` (register compilers)
- Test: `machine/compile_let_test.go`

**Step 1: Write the failing test**

Create `machine/compile_let_test.go`:

```go
func TestCompileLetBasic(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "single binding", Code: `(let ((x 1)) x)`, Expected: values.NewInteger(1)},
		{Name: "multiple bindings", Code: `(let ((x 1) (y 2)) (+ x y))`, Expected: values.NewInteger(3)},
		{Name: "nested let", Code: `(let ((x 1)) (let ((y 2)) (+ x y)))`, Expected: values.NewInteger(3)},
		{Name: "let in tail position", Code: `((lambda () (let ((x 42)) x)))`, Expected: values.NewInteger(42)},
		{Name: "let with side effects", Code: `(let ((x 1)) (set! x 2) x)`, Expected: values.NewInteger(2)},
		{Name: "let closure capture", Code: `(let ((x 1)) (let ((f (lambda () x))) (f)))`, Expected: values.NewInteger(1)},
		{Name: "empty bindings", Code: `(let () 42)`, Expected: values.NewInteger(42)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestCompileLetStarBasic(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "sequential visibility", Code: `(let* ((x 1) (y (+ x 1))) y)`, Expected: values.NewInteger(2)},
		{Name: "chain of three", Code: `(let* ((a 1) (b (+ a 1)) (c (+ b 1))) c)`, Expected: values.NewInteger(3)},
		{Name: "empty bindings", Code: `(let* () 42)`, Expected: values.NewInteger(42)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestCompileLetrecBasic(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "recursive factorial", Code: `(letrec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))) (f 5))`, Expected: values.NewInteger(120)},
		{Name: "mutual recursion", Code: `(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))) (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))) (even? 10))`, Expected: values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestCompileLetrecStarBasic(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "sequential with values", Code: `(letrec* ((x 1) (y (+ x 1))) y)`, Expected: values.NewInteger(2)},
		{Name: "forward ref via closure", Code: `(letrec* ((f (lambda () g)) (g 42)) (f))`, Expected: values.NewInteger(42)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestLetBindingsDontSeeEachOther(t *testing.T) {
	// In (let ((x 1) (y unbound)) ...), y's init should NOT see x's binding
	_, err := testhelpers.RunSchemeCode(t, `(let ((x 1) (y unbound-var-for-test)) y)`)
	qt.Assert(t, err, qt.IsNotNil)
}
```

**Step 2: Implement compiler**

Create `machine/compile_let.go`:

```go
package machine

import (
	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/validate"
)

// CompileValidatedLet compiles (let ((name val) ...) body ...).
//
// Bytecode:
//
//	<compile init-1> Push    ; init exprs in parent env
//	<compile init-2> Push
//	OpPushEnv(N)             ; new env frame with N slots
//	StoreLocal name-N        ; pop from stack into slots (LIFO)
//	...
//	StoreLocal name-1
//	<compile body>           ; last expr inherits tail position
//	OpPopEnv                 ; only if let is NOT in tail position
func (p *CompileTimeContinuation) CompileValidatedLet(
	ctctx CompileTimeCallContext,
	v *validate.ValidatedLet,
) error {
	n := len(v.Bindings)

	// Phase 1: Compile all init expressions in the CURRENT env and push to stack.
	for _, b := range v.Bindings {
		err := p.compileValidated(ctctx.NotInTail(), b.Init)
		if err != nil {
			return err
		}
		p.AppendOperations(NewOperationPush())
	}

	// Phase 2: Create new env frame with local slots.
	if n > 0 {
		lenv := environment.NewLocalEnvironment(0)
		childEnv := environment.NewEnvironmentFrameWithParent(lenv, p.env)

		for _, b := range v.Bindings {
			childEnv.MaybeCreateLocalBindingWithScopes(
				b.Name.Sym,
				environment.BindingTypeVariable,
				b.Name.Scopes(),
				b.Name.SourceContext(),
			)
		}

		p.AppendOperations(NewOperationPushEnv(n))

		// Store values from stack into local slots (reverse order — LIFO).
		for i := n - 1; i >= 0; i-- {
			li := childEnv.GetLocalIndex(v.Bindings[i].Name.Sym)
			p.AppendOperations(NewOperationStoreLocalByLocalIndexImmediate(li))
		}

		savedEnv := p.env
		p.env = childEnv

		err := p.compileLetBody(ctctx, v.Body())
		p.env = savedEnv

		if err != nil {
			return err
		}

		if !ctctx.inTail {
			p.AppendOperations(NewOperationPopEnv())
		}
	} else {
		return p.compileLetBody(ctctx, v.Body())
	}

	return nil
}

// CompileValidatedLetStar compiles (let* ((name val) ...) body ...).
//
// Bytecode:
//
//	OpPushEnv(N)             ; all slots upfront
//	<compile init-1>
//	StoreLocal name-1        ; name-1 now visible
//	<compile init-2>         ; can reference name-1
//	StoreLocal name-2
//	<compile body>
//	OpPopEnv                 ; only if not tail
func (p *CompileTimeContinuation) CompileValidatedLetStar(
	ctctx CompileTimeCallContext,
	v *validate.ValidatedLetStar,
) error {
	n := len(v.Bindings)

	if n > 0 {
		lenv := environment.NewLocalEnvironment(0)
		childEnv := environment.NewEnvironmentFrameWithParent(lenv, p.env)

		p.AppendOperations(NewOperationPushEnv(n))

		savedEnv := p.env
		p.env = childEnv

		for _, b := range v.Bindings {
			err := p.compileValidated(ctctx.NotInTail(), b.Init)
			if err != nil {
				p.env = savedEnv
				return err
			}

			childEnv.MaybeCreateLocalBindingWithScopes(
				b.Name.Sym,
				environment.BindingTypeVariable,
				b.Name.Scopes(),
				b.Name.SourceContext(),
			)
			li := childEnv.GetLocalIndex(b.Name.Sym)
			p.AppendOperations(NewOperationPush())
			p.AppendOperations(NewOperationStoreLocalByLocalIndexImmediate(li))
		}

		err := p.compileLetBody(ctctx, v.Body())
		p.env = savedEnv

		if err != nil {
			return err
		}

		if !ctctx.inTail {
			p.AppendOperations(NewOperationPopEnv())
		}
	} else {
		return p.compileLetBody(ctctx, v.Body())
	}

	return nil
}

// CompileValidatedLetrec compiles (letrec ...) and (letrec* ...).
//
// letrec (delayed assignment):
//
//	OpPushEnv(N)             ; all bindings in scope
//	<compile init-1> Push    ; all inits evaluated first
//	<compile init-2> Push
//	StoreLocal name-N        ; then assigned (LIFO)
//	StoreLocal name-1
//	<compile body>
//	OpPopEnv
//
// letrec* (sequential assignment):
//
//	OpPushEnv(N)             ; all bindings in scope
//	<compile init-1>
//	StoreLocal name-1        ; assigned immediately
//	<compile init-2>         ; sees name-1's value
//	StoreLocal name-2
//	<compile body>
//	OpPopEnv
func (p *CompileTimeContinuation) CompileValidatedLetrec(
	ctctx CompileTimeCallContext,
	v *validate.ValidatedLetrec,
) error {
	n := len(v.Bindings)

	if n == 0 {
		return p.compileLetBody(ctctx, v.Body())
	}

	// Create child env with ALL bindings visible before compiling any init.
	lenv := environment.NewLocalEnvironment(0)
	childEnv := environment.NewEnvironmentFrameWithParent(lenv, p.env)

	for _, b := range v.Bindings {
		childEnv.MaybeCreateLocalBindingWithScopes(
			b.Name.Sym,
			environment.BindingTypeVariable,
			b.Name.Scopes(),
			b.Name.SourceContext(),
		)
	}

	p.AppendOperations(NewOperationPushEnv(n))

	savedEnv := p.env
	p.env = childEnv

	var err error
	if v.LetrecStar {
		// letrec*: compile and store each init sequentially
		for _, b := range v.Bindings {
			err = p.compileValidated(ctctx.NotInTail(), b.Init)
			if err != nil {
				p.env = savedEnv
				return err
			}
			li := childEnv.GetLocalIndex(b.Name.Sym)
			p.AppendOperations(NewOperationPush())
			p.AppendOperations(NewOperationStoreLocalByLocalIndexImmediate(li))
		}
	} else {
		// letrec: compile all inits first, then store all (delayed assignment)
		for _, b := range v.Bindings {
			err = p.compileValidated(ctctx.NotInTail(), b.Init)
			if err != nil {
				p.env = savedEnv
				return err
			}
			p.AppendOperations(NewOperationPush())
		}
		for i := n - 1; i >= 0; i-- {
			li := childEnv.GetLocalIndex(v.Bindings[i].Name.Sym)
			p.AppendOperations(NewOperationStoreLocalByLocalIndexImmediate(li))
		}
	}

	// Compile body (works for both regular letrec and named let).
	// Named let's body is a ValidatedCall — no special-casing needed.
	err = p.compileLetBody(ctctx, v.Body())
	if err != nil {
		p.env = savedEnv
		return err
	}

	p.env = savedEnv

	if !ctctx.inTail {
		p.AppendOperations(NewOperationPopEnv())
	}

	return nil
}

// compileLetBody compiles a sequence of body expressions with
// letrec* pre-declaration and tail position semantics.
func (p *CompileTimeContinuation) compileLetBody(
	ctctx CompileTimeCallContext,
	body []validate.ValidatedExpr,
) error {
	for _, expr := range body {
		p.predeclareDefineBindingFromValidated(expr)
	}
	return p.compileValidatedSequence(ctctx, body)
}

// NOTE: No compileNamedLetCall needed. Named let produces a ValidatedLetrec
// whose body is a ValidatedCall — the standard compileLetBody handles it.
```

NOTE: The compiler emits `NewOperationPushEnv(n)` through `AppendOperations`, which calls `operationToInstruction` to convert to `Instruction{Op: OpPushEnv, Arg: int32(n)}`. This matches the pattern used by all other operations. The named let body is a `ValidatedCall` compiled by `compileLetBody` — no special compiler function needed.

Register in `machine/register.go`:

```go
registerTypedCompiler("let", func(ctc *CompileTimeContinuation, ctctx CompileTimeCallContext, v *validate.ValidatedLet) error {
	return ctc.CompileValidatedLet(ctctx, v)
})
registerTypedCompiler("let*", func(ctc *CompileTimeContinuation, ctctx CompileTimeCallContext, v *validate.ValidatedLetStar) error {
	return ctc.CompileValidatedLetStar(ctctx, v)
})
registerTypedCompiler("letrec", func(ctc *CompileTimeContinuation, ctctx CompileTimeCallContext, v *validate.ValidatedLetrec) error {
	return ctc.CompileValidatedLetrec(ctctx, v)
})
registerTypedCompiler("letrec*", func(ctc *CompileTimeContinuation, ctctx CompileTimeCallContext, v *validate.ValidatedLetrec) error {
	return ctc.CompileValidatedLetrec(ctctx, v)
})
```

**Step 3: Run tests**

Run: `go test -run 'TestCompileLet' -v ./machine/`
Expected: PASS once expander + validator + compiler are all wired

---

### Task 6: Register Compile-Time Bindings and Remove Macros

**Files:**
- Modify: `registry/core/specialforms.go` (add compile-time bindings)
- Modify: `registry/core/bootstrap_macros.scm` (remove macro definitions)

**Step 1: Add compile-time bindings**

In `registry/core/specialforms.go`, add to `compileTimeBindings`:

```go
"let",
"let*",
"letrec",
"letrec*",
```

**Step 2: Remove macros**

In `registry/core/bootstrap_macros.scm`, remove:
- The `let` define-syntax (both plain and named clauses)
- The `let*` define-syntax
- The `letrec` define-syntax
- The `letrec*` define-syntax

Keep `with-binding-scope` — still used by user-defined binding macros.

**Step 3: Run full test suite**

Run: `make lint && make test`
Expected: PASS — all existing tests pass with the new compilation path

---

### Task 7: Integration Tests and Edge Cases

**Files:**
- Create or extend: `machine/compile_let_test.go` (add edge cases)

**Step 1: Add comprehensive integration tests**

```go
func TestLetIntegration(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Named let
		{Name: "named let factorial", Code: `(let loop ((n 5) (acc 1)) (if (= n 0) acc (loop (- n 1) (* acc n))))`, Expected: values.NewInteger(120)},
		// Shadowing special forms
		{Name: "shadow if", Code: `(let ((if 42)) if)`, Expected: values.NewInteger(42)},
		// Nested binding forms
		{Name: "let inside let*", Code: `(let* ((x 1)) (let ((y (+ x 1))) y))`, Expected: values.NewInteger(2)},
		{Name: "let* inside let", Code: `(let ((x 10)) (let* ((y x) (z (+ y 1))) z))`, Expected: values.NewInteger(11)},
		{Name: "letrec inside let", Code: `(let ((x 10)) (letrec ((f (lambda () x))) (f)))`, Expected: values.NewInteger(10)},
		// let with lambda
		{Name: "let returns lambda", Code: `((let ((x 1)) (lambda () x)))`, Expected: values.NewInteger(1)},
		// All four forms work
		{Name: "letrec factorial", Code: `(letrec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))) (f 5))`, Expected: values.NewInteger(120)},
		{Name: "letrec mutual", Code: `(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))) (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))) (even? 10))`, Expected: values.TrueValue},
		{Name: "letrec* sequential", Code: `(letrec* ((x 1) (y (+ x 1))) y)`, Expected: values.NewInteger(2)},
		{Name: "letrec* forward ref", Code: `(letrec* ((f (lambda () g)) (g 42)) (f))`, Expected: values.NewInteger(42)},
		// Multiple body expressions
		{Name: "multiple body", Code: `(let ((x 1)) (+ x 1) (+ x 2))`, Expected: values.NewInteger(3)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestLetMacroGenerated(t *testing.T) {
	// Macros that expand TO let internally must continue working
	// when let is a core form handled by the expander.
	tcs := []testhelpers.SchemeCodeTestCase{
		// cond creates let bindings for temp values
		{Name: "cond true", Code: `(cond (#t 42))`, Expected: values.NewInteger(42)},
		{Name: "cond multi", Code: `(cond (#f 1) (#t 2))`, Expected: values.NewInteger(2)},
		{Name: "cond =>", Code: `(cond ((assv 2 '((1 one) (2 two) (3 three))) => cdr))`, Expected: values.NewSymbol("two")},
		// case creates let binding for key
		{Name: "case", Code: `(case (+ 1 1) ((1) 'one) ((2) 'two) ((3) 'three))`, Expected: values.NewSymbol("two")},
		// do uses named let internally
		{Name: "do loop", Code: `(do ((i 0 (+ i 1))) ((= i 5) i))`, Expected: values.NewInteger(5)},
		// and/or may expand through let
		{Name: "and", Code: `(and 1 2 3)`, Expected: values.NewInteger(3)},
		{Name: "or", Code: `(or #f #f 42)`, Expected: values.NewInteger(42)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestLetInternalDefine(t *testing.T) {
	// R7RS §5.3: internal definitions at beginning of let body
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "define in let body", Code: `(let ((x 1)) (define y 2) (+ x y))`, Expected: values.NewInteger(3)},
		{Name: "define in let* body", Code: `(let* ((x 1)) (define y (+ x 1)) y)`, Expected: values.NewInteger(2)},
		{Name: "define in letrec body", Code: `(letrec ((f (lambda () 1))) (define x (f)) x)`, Expected: values.NewInteger(1)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestLetShadowingLet(t *testing.T) {
	// let shadowing the let keyword itself
	result, err := testhelpers.RunSchemeCode(t, `(let ((let 42)) let)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewInteger(42))
}

func TestLetCallCC(t *testing.T) {
	// call/cc inside let captures the env correctly
	code := `(call-with-current-continuation (lambda (k) (let ((x 42)) (k x))))`
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewInteger(42))
}

func TestLetrecCallCC(t *testing.T) {
	// call/cc inside letrec captures recursive binding correctly
	code := `(call-with-current-continuation
		(lambda (k)
			(letrec ((f (lambda (n) (if (= n 0) (k 42) (f (- n 1))))))
				(f 3))))`
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewInteger(42))
}
```

**Step 2: Run all tests**

Run: `make test`
Expected: PASS

**Step 3: Run lint**

Run: `make lint && make covercheck`
Expected: PASS

---

### Task 8: Benchmark

**Step 1: Run Gabriel benchmarks before/after**

Before making changes (on master):
```bash
make bench-gabriel 2>&1 | tee /tmp/bench-before.txt
```

After all changes (on feature branch):
```bash
make bench-gabriel 2>&1 | tee /tmp/bench-after.txt
```

Compare results. Expect 2-5% improvement on binding-heavy benchmarks (fib, ackermann, nqueens, destruct). `letrec`-heavy code (recursive functions) may show additional improvement.

**Step 2: Run extended benchmarks**

```bash
make bench-extended 2>&1 | tee /tmp/bench-extended-after.txt
```

---

### Task Order and Dependencies

```
Task 1 (OpPushEnv) ──────────────────────────────┐
Task 2 (Validated types: Let, LetStar, Letrec) ──┤
Task 3 (Validators: all four forms) ──┐          │
Task 3b (Mutability tracking) ────────┤          │
Task 4 (Expanders: all four forms) ───┤──────────┼─→ Task 6 (Registration + macro removal)
Task 5 (Compilers: all four forms) ───┘          │   │
                                                     └─→ Task 7 (Integration tests)
                                                          └─→ Task 8 (Benchmark)
```

Task 3b depends on Task 3 (needs the let validators to exist). All other tasks in 1-5 are independent. Task 6 wires everything together. Task 7 validates end-to-end. Task 8 measures impact.

### Implementation Notes

- **`collectList` (not `collectSyntaxList`)**: The validator's list-collection function is `collectList(pair *syntax.SyntaxPair)` in `validate.go:127`. It takes `*syntax.SyntaxPair`, not `syntax.SyntaxValue`. Callers must type-assert before calling.
- **Operation emission**: The compiler emits `Operation` interface values via `AppendOperations`. `operationToInstruction` in `native_template.go` converts to `Instruction`. Never pass raw `Instruction` to `AppendOperations` — it doesn't implement the `Operation` interface. `Instruction.Arg` is `int32`.
- **`GetLocalIndex` return**: `GetLocalIndex` returns `*environment.LocalIndex` (a `[2]int`: slot + depth). `EncodeLocalIndex` packs it into `int32` for `Instruction.Arg`. `DecodeLocalIndex` unpacks in `Run()`. The encoding matches.
- **Named let representation**: `validateNamedLet` produces a `ValidatedLetrec` with `Tag` set (diagnostic) and body = `[ValidatedCall]`. The call has the tag as callee and the original init values as args. No special compiler field or method needed — `compileLetBody` handles it.
- **letrec delayed assignment**: For non-star letrec, all inits are pushed to the stack before any StoreLocal. This prevents any init from observing a partially-initialized binding. Verify that the stack has sufficient capacity for large binding lists.
- **`letrec` initial value changes from `#f` to `void`**: The current macro initializes bindings to `#f`: `(let ((f #f) ...) (set! f init) ...)`. The core form uses `NewLocalEnvironment(N)` where slots default to `values.Void`. Accessing a binding before its init completes is an error per R7RS, so neither value is "correct" — but `void` is arguably better (more likely to cause a visible error). Document this behavioral change in `docs/dev/R7RS_SEMANTIC_DIFFERENCES.md`.
- **Mutability tracking uses pointer identity**: `ValidationResult.mutatedBindings` maps `*environment.Binding` pointers to `bool`. The same `*Binding` object is created by `createLetValidationEnv` and resolved by `validateSetBang` through the shared env tree. No serialization or scope comparison needed — just pointer equality. The map is lazily allocated (nil until first `set!` encountered).
- **`validateSetBang` binding resolution is opportunistic**: If `GetBindingWithScopes` returns nil (e.g., `set!` of an undefined variable), the binding is not marked. Unresolved bindings default to `Mutable: false`. This is safe because the compiler catches undefined variables before any optimization runs. Not a false negative — just deferred error reporting.
- **Mutability scope**: The `mutatedBindings` set is shared across the entire `ValidationResult` (one per `ValidateExpression` call). `set!` targets from any nesting depth are collected in the same set. Scope isolation is achieved through pointer identity — different `let` scopes create different `*Binding` objects, even for the same variable name.
