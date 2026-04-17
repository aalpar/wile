# Extension Contracts Phase 2+ — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Status:** 3/8 tasks complete (Tasks 1-3 only)

> **Completed:** Task 1 (ForeignClosure.validate field), Task 2 (callForeignCached dispatch), Task 3 (applyForeign dispatch).
> **Incomplete:** Task 4 (buildValidator in registry/contract.go), Task 5 (WithContractEnforcement + Apply threading), Task 6 (integration tests), Task 7 (files extension ParamTypes/ReturnType), Task 8 (lint/verify).

**Goal:** Add opt-in runtime contract enforcement to the dispatch path and annotate
`extensions/files/` as proof-of-concept. This proves the full stack (annotation →
registration → enforcement → error) end-to-end with the smallest extension package.

**Architecture:** `ForeignClosure` gets a `validate` field (nil = no validation).
`buildValidator` in `registry/contract.go` builds a closure from `PrimitiveSpec.ParamTypes`.
`WithContractEnforcement()` engine option threads a bool through `Registry.Apply()` to
gate validator installation. Dispatch path (`callForeignCached`, `applyForeign`) calls
the validator before the function body. See `plans/2026-03-26-extension-contracts-phase2-design.md`.

**Tech Stack:** Go 1.24, quicktest (`qt`), table-driven tests, `machine/`, `registry/`,
`values/`, `extensions/files/`

---

## Task 1: ForeignClosure.validate Field + SetValidator

**Files:**
- Modify: `machine/foreign_closure.go:60-66`
- Modify: `machine/foreign_closure_test.go`

**Step 1: Write the failing test**

Add to `machine/foreign_closure_test.go`:

```go
func TestForeignClosure_SetValidator(t *testing.T) {
	c := qt.New(t)

	env := environment.NewEnvironmentFrame(
		environment.NewLocalEnvironment(1),
	)
	cls := newTestForeignClosure(env, 1, false, func(mc *MachineContext) error {
		return nil
	})

	// No validator by default
	c.Assert(cls.Validator(), qt.IsNil)

	// Set a validator
	called := false
	cls.SetValidator(func(mc *MachineContext) error {
		called = true
		return nil
	})
	c.Assert(cls.Validator(), qt.IsNotNil)

	// Verify it's callable (actual dispatch integration tested separately)
	err := cls.Validator()(nil)
	c.Assert(err, qt.IsNil)
	c.Assert(called, qt.IsTrue)
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestForeignClosure_SetValidator ./machine/...`
Expected: FAIL — `Validator` and `SetValidator` undefined

**Step 3: Write minimal implementation**

In `machine/foreign_closure.go`, add the `validate` field and methods:

```go
type ForeignClosure struct {
	fn         ForeignFunction
	validate   ForeignFunction // nil = no validation; set via SetValidator
	env        *environment.EnvironmentFrame
	paramCount int
	isVariadic bool
	name       string
}
```

Add after `SetName`:

```go
// SetValidator installs a contract validation function that runs before
// the implementation. Called during registration when contract enforcement
// is enabled. The validator has access to mc.Arg(i) because it runs
// after argument binding.
func (p *ForeignClosure) SetValidator(v ForeignFunction) {
	p.validate = v
}

// Validator returns the installed contract validator, or nil if none.
func (p *ForeignClosure) Validator() ForeignFunction {
	return p.validate
}
```

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestForeignClosure_SetValidator ./machine/...`
Expected: PASS

**Step 5: Commit**

```
feat(machine): add validate field and SetValidator to ForeignClosure

Adds an optional contract validation function that will be called
before the implementation in the dispatch path. Nil by default
(no validation, zero cost). Part of extension contracts Phase 2.
```

---

## Task 2: Dispatch Path — callForeignCached

**Files:**
- Modify: `machine/call_foreign_cached.go:72-75`
- Modify: `machine/call_foreign_cached_test.go`

**Step 1: Write the failing test**

Add to `machine/call_foreign_cached_test.go`:

```go
func TestCallForeignCached_ValidatorCalled(t *testing.T) {
	c := qt.New(t)

	validatorCalls := 0
	env, mc := setupForeignCachedTest(t, 2, false, func(mc *MachineContext) error {
		mc.SetValue(values.TrueValue)
		return nil
	})
	_ = env

	// Get the ForeignClosure from cached bindings and install a validator
	binding := mc.Template().CachedBindings()[0]
	fcls := binding.Value().(*ForeignClosure)
	fcls.SetValidator(func(mc *MachineContext) error {
		validatorCalls++
		return nil
	})

	// Push two args and run
	mc.Evals().Push(values.NewInteger(1))
	mc.Evals().Push(values.NewInteger(2))
	_, err := callForeignCached(mc, Instruction{Op: OpCallForeignCached, Arg: 0}, false)
	c.Assert(err, qt.IsNil)
	c.Assert(validatorCalls, qt.Equals, 1)
}

func TestCallForeignCached_ValidatorRejectsArg(t *testing.T) {
	c := qt.New(t)

	env, mc := setupForeignCachedTest(t, 1, false, func(mc *MachineContext) error {
		mc.SetValue(values.TrueValue)
		return nil
	})
	_ = env

	binding := mc.Template().CachedBindings()[0]
	fcls := binding.Value().(*ForeignClosure)
	fcls.SetValidator(func(mc *MachineContext) error {
		return werr.WrapForeignErrorf(werr.ErrNotAString, "test: argument 0")
	})

	mc.Evals().Push(values.NewInteger(42))
	_, err := callForeignCached(mc, Instruction{Op: OpCallForeignCached, Arg: 0}, false)
	c.Assert(err, qt.IsNotNil)
}
```

**Note:** If `setupForeignCachedTest` doesn't exist, adapt the test setup from the existing
test patterns in `call_foreign_cached_test.go`. Read the file first to find the setup helpers.

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestCallForeignCached_Validator ./machine/...`
Expected: FAIL — validator not called (test passes but `validatorCalls == 0`)

**Step 3: Write minimal implementation**

In `machine/call_foreign_cached.go`, insert after `mc.env = env` (line ~72) and before
`savedTemplate := mc.template` (line ~73):

```go
	if fcls.validate != nil {
		if err := fcls.validate(mc); err != nil {
			return nil, applyCallableError(mc, err)
		}
	}
```

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestCallForeignCached_Validator ./machine/...`
Expected: PASS

**Step 5: Run full machine tests**

Run: `go test -v ./machine/...`
Expected: PASS — existing tests unaffected (all closures have nil validate)

**Step 6: Commit**

```
feat(machine): call contract validator in callForeignCached

When a ForeignClosure has a non-nil validator, callForeignCached
calls it after argument binding and before the implementation.
Nil validators (the default) are skipped with a single branch.
```

---

## Task 3: Dispatch Path — applyForeign

**Files:**
- Modify: `machine/machine_context_apply.go:110-124`
- Modify: `machine/foreign_closure_apply_test.go`

**Step 1: Write the failing test**

Add to `machine/foreign_closure_apply_test.go`:

```go
func TestApplyForeign_ValidatorCalled(t *testing.T) {
	c := qt.New(t)

	validatorCalls := 0
	env := setupApplyForeignEnv(t)
	cls := NewForeignClosure(env, 1, false, func(mc *MachineContext) error {
		mc.SetValue(values.TrueValue)
		return nil
	})
	cls.SetValidator(func(mc *MachineContext) error {
		validatorCalls++
		return nil
	})

	mc := newTestMachineContext(t, env)
	_, err := mc.applyForeign(cls, values.NewInteger(42))
	c.Assert(err, qt.IsNil)
	c.Assert(validatorCalls, qt.Equals, 1)
}

func TestApplyForeign_ValidatorRejectsArg(t *testing.T) {
	c := qt.New(t)

	env := setupApplyForeignEnv(t)
	cls := NewForeignClosure(env, 1, false, func(mc *MachineContext) error {
		mc.SetValue(values.TrueValue)
		return nil
	})
	cls.SetValidator(func(mc *MachineContext) error {
		return werr.WrapForeignErrorf(werr.ErrNotAString, "test: argument 0")
	})

	mc := newTestMachineContext(t, env)
	_, err := mc.applyForeign(cls, values.NewInteger(42))
	c.Assert(err, qt.IsNotNil)
}
```

**Note:** Adapt setup helpers from existing patterns in `foreign_closure_apply_test.go`.
Read the file first.

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestApplyForeign_Validator ./machine/...`
Expected: FAIL — validator not called

**Step 3: Write minimal implementation**

In `machine/machine_context_apply.go`, insert after `p.env = env` (line ~110) and before
`p.counters.ForeignCalls++` (line ~112):

```go
	if fcls.validate != nil {
		if err := fcls.validate(p); err != nil {
			return nil, err
		}
	}
```

**Note:** `applyForeign` does NOT wrap errors with `applyCallableError` at this insertion
point — it returns raw errors for `ErrPromptAbort`/`ErrExceptionEscape` handling further
down. The contract error from the validator will be caught by the existing error handling
after `fcls.fn(p)` which wraps via `goErrorToSchemeException`. Verify this is correct by
checking whether a plain `error` return from this point gets properly handled.

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestApplyForeign_Validator ./machine/...`
Expected: PASS

**Step 5: Run full machine tests**

Run: `go test -v ./machine/...`
Expected: PASS

**Step 6: Commit**

```
feat(machine): call contract validator in applyForeign

Same pattern as callForeignCached: validator called after binding,
before implementation. Nil check skips when no validator installed.
```

---

## Task 4: Validator Builder

**Files:**
- Create: `registry/contract.go`
- Create: `registry/contract_test.go`

**Step 1: Write the failing test**

Create `registry/contract_test.go`:

```go
package registry

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
)

func TestBuildValidator_NoParamTypes(t *testing.T) {
	c := qt.New(t)
	spec := PrimitiveSpec{Name: "test", ParamCount: 1}
	v := buildValidator(spec)
	c.Assert(v, qt.IsNil)
}

func TestBuildValidator_NonVariadic(t *testing.T) {
	c := qt.New(t)
	spec := PrimitiveSpec{
		Name:       "string-length",
		ParamCount: 1,
		ParamTypes: []values.ValueType{values.TypeString},
	}
	v := buildValidator(spec)
	c.Assert(v, qt.IsNotNil)

	// Pass correct type — should succeed
	mc := newValidatorTestMC(t, values.NewString("hello"))
	err := v(mc)
	c.Assert(err, qt.IsNil)

	// Pass wrong type — should fail
	mc = newValidatorTestMC(t, values.NewInteger(42))
	err = v(mc)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Matches, ".*string-length.*argument 0.*")
}

func TestBuildValidator_TypeAnySkipped(t *testing.T) {
	c := qt.New(t)
	spec := PrimitiveSpec{
		Name:       "test",
		ParamCount: 2,
		ParamTypes: []values.ValueType{values.TypeAny, values.TypeString},
	}
	v := buildValidator(spec)
	c.Assert(v, qt.IsNotNil)

	// First arg is TypeAny — anything passes. Second must be string.
	mc := newValidatorTestMC(t, values.NewInteger(42), values.NewString("ok"))
	err := v(mc)
	c.Assert(err, qt.IsNil)

	// Second arg wrong type
	mc = newValidatorTestMC(t, values.NewInteger(42), values.NewInteger(99))
	err = v(mc)
	c.Assert(err, qt.IsNotNil)
}

func TestBuildValidator_Variadic(t *testing.T) {
	c := qt.New(t)
	// string-append: ParamCount=1, IsVariadic=true, ParamTypes=[TypeString]
	// After binding: mc.Arg(0) = rest list of strings
	spec := PrimitiveSpec{
		Name:       "string-append",
		ParamCount: 1,
		IsVariadic: true,
		ParamTypes: []values.ValueType{values.TypeString},
	}
	v := buildValidator(spec)
	c.Assert(v, qt.IsNotNil)

	// Rest list of strings — should pass
	rest := values.NewPair(values.NewString("a"),
		values.NewPair(values.NewString("b"), values.NewEmptyList()))
	mc := newValidatorTestMC(t, rest)
	err := v(mc)
	c.Assert(err, qt.IsNil)

	// Rest list with non-string — should fail
	rest = values.NewPair(values.NewString("a"),
		values.NewPair(values.NewInteger(42), values.NewEmptyList()))
	mc = newValidatorTestMC(t, rest)
	err = v(mc)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Matches, ".*string-append.*argument 1.*")
}

func TestBuildValidator_VariadicWithFixedArgs(t *testing.T) {
	c := qt.New(t)
	// log: ParamCount=2, IsVariadic=true, ParamTypes=[TypeNumber, TypeNumber]
	// After binding: mc.Arg(0) = z, mc.Arg(1) = rest list
	spec := PrimitiveSpec{
		Name:       "log",
		ParamCount: 2,
		IsVariadic: true,
		ParamTypes: []values.ValueType{values.TypeNumber, values.TypeNumber},
	}
	v := buildValidator(spec)

	// Fixed arg correct, empty rest list — should pass
	mc := newValidatorTestMC(t, values.NewInteger(10), values.NewEmptyList())
	err := v(mc)
	c.Assert(err, qt.IsNil)

	// Fixed arg wrong type — should fail
	mc = newValidatorTestMC(t, values.NewString("bad"), values.NewEmptyList())
	err = v(mc)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Matches, ".*log.*argument 0.*")
}

// newValidatorTestMC creates a MachineContext with args bound to an environment
// for testing validators. This mirrors how callForeignCached sets up the env
// before calling the validator.
func newValidatorTestMC(t *testing.T, args ...values.Value) *machine.MachineContext {
	t.Helper()
	// Build an environment with bindings set to the given args
	// This needs to match the bindArgs layout used by callForeignCached.
	// Read machine/util.go and machine/call_foreign_cached.go for the exact
	// setup. The test helper should create an env frame with len(args) bindings
	// and set each to the corresponding value, then create a MachineContext
	// with that env.
	//
	// IMPORTANT: The implementation of this helper depends on the exact
	// MachineContext construction patterns in machine/. Read
	// machine/foreign_closure_apply_test.go for prior art.
	panic("implement based on existing test patterns in machine/")
}
```

**Important:** The `newValidatorTestMC` helper is a placeholder. During implementation,
read `machine/foreign_closure_apply_test.go` and `machine/call_foreign_cached_test.go` to
find the correct setup pattern. The helper must create a `MachineContext` where `mc.Arg(i)`
returns the expected values — this depends on how environment bindings are laid out after
`bindArgs`. Since `buildValidator` lives in `registry/` (external test package boundary),
you may need to use `registry/testhelpers/` to run Scheme code instead, or use the
integration test approach from Task 6.

**Alternative test strategy:** If creating a raw `MachineContext` is too complex from
`registry/`, test `buildValidator` via integration tests that run Scheme code with
enforcement enabled (Task 6). In that case, this task's unit tests should focus on:
- `buildValidator` returns nil for empty `ParamTypes`
- `buildValidator` returns non-nil for populated `ParamTypes`
- The returned closure captures the right name (test via error message)

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestBuildValidator ./registry/...`
Expected: FAIL — `buildValidator` undefined

**Step 3: Write minimal implementation**

Create `registry/contract.go`:

```go
package registry

import (
	"slices"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// buildValidator creates a contract validation function from a PrimitiveSpec's
// ParamTypes. Returns nil if the spec has no type contracts (zero overhead).
//
// The returned function is installed on ForeignClosure.validate and runs after
// argument binding but before the implementation. It checks each argument
// against the declared ValueType contract.
//
// For variadic primitives:
//   - Fixed args (positions 0..paramCount-2) are checked directly via mc.Arg(i)
//   - Rest args (the Tuple at mc.Arg(paramCount-1)) are iterated and each
//     element is checked against the last ParamTypes entry
//
// For non-variadic primitives:
//   - All positions 0..len(ParamTypes)-1 are checked via mc.Arg(i)
func buildValidator(spec PrimitiveSpec) machine.ForeignFunction {
	if len(spec.ParamTypes) == 0 {
		return nil
	}
	types := slices.Clone(spec.ParamTypes)
	name := spec.Name
	paramCount := spec.ParamCount
	isVariadic := spec.IsVariadic
	return func(mc *machine.MachineContext) error {
		if isVariadic {
			return validateVariadic(mc, types, name, paramCount)
		}
		return validateFixed(mc, types, name)
	}
}

// validateFixed checks arguments for non-variadic primitives.
func validateFixed(mc *machine.MachineContext, types []values.ValueType, name string) error {
	for i, vt := range types {
		if vt == values.TypeAny {
			continue
		}
		_, ok, checkErr := vt.Check(mc.Arg(i))
		if !ok {
			return werr.WrapForeignErrorf(checkErr, "%s: argument %d", name, i)
		}
	}
	return nil
}

// validateVariadic checks arguments for variadic primitives.
// Fixed args are at positions 0..paramCount-2. The rest list is at
// mc.Arg(paramCount-1). Each rest element is checked against the last
// entry in types.
func validateVariadic(mc *machine.MachineContext, types []values.ValueType, name string, paramCount int) error {
	// Check fixed args
	fixedCount := paramCount - 1
	for i := 0; i < fixedCount && i < len(types); i++ {
		vt := types[i]
		if vt == values.TypeAny {
			continue
		}
		_, ok, checkErr := vt.Check(mc.Arg(i))
		if !ok {
			return werr.WrapForeignErrorf(checkErr, "%s: argument %d", name, i)
		}
	}

	// Check rest list elements
	restType := types[len(types)-1]
	if restType == values.TypeAny {
		return nil
	}
	rest := mc.Arg(paramCount - 1)
	tuple, isTuple := rest.(values.Tuple)
	if !isTuple || tuple.IsEmptyList() {
		return nil
	}

	argIdx := fixedCount
	cursor := tuple
	for !cursor.IsEmptyList() {
		_, ok, checkErr := restType.Check(cursor.Car())
		if !ok {
			return werr.WrapForeignErrorf(checkErr, "%s: argument %d", name, argIdx)
		}
		cdr := cursor.Cdr()
		next, nextOk := cdr.(values.Tuple)
		if !nextOk {
			break
		}
		cursor = next
		argIdx++
	}
	return nil
}
```

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestBuildValidator ./registry/...`
Expected: PASS

**Step 5: Commit**

```
feat(registry): add buildValidator for contract enforcement

Builds a validation closure from PrimitiveSpec.ParamTypes that checks
each argument against its declared ValueType. Handles both fixed and
variadic parameter lists. Returns nil for uncontracted specs.
```

---

## Task 5: Engine Option + Apply Threading

**Files:**
- Modify: `options.go:49-60`
- Modify: `registry/apply.go:28,51-53,60-63,97-114,127-145`
- Modify: `engine.go:395-407,564-565`
- Modify: `internal/bootstrap/environment_tiny.go:105`
- Modify: `registry/apply_test.go` (7 call sites)

**Step 1: Write the failing test**

Add to `registry/apply_test.go`:

```go
func TestApply_ContractEnforcement(t *testing.T) {
	c := qt.New(t)

	reg := NewRegistry()
	reg.AddPrimitive(PrimitiveSpec{
		Name:       "test-enforced",
		ParamCount: 1,
		Impl:       func(mc *machine.MachineContext) error { return nil },
		ParamTypes: []values.ValueType{values.TypeString},
	}, PhaseRuntime)

	env := createTestEnv(t)

	// Without enforcement — validator should be nil
	err := reg.Apply(context.Background(), env, false)
	c.Assert(err, qt.IsNil)
	binding, _ := env.GetOwnGlobalValue(environment.NewGlobalIndex(values.NewSymbol("test-enforced")))
	fcls := binding.(*machine.ForeignClosure)
	c.Assert(fcls.Validator(), qt.IsNil)

	// With enforcement — validator should be set
	env2 := createTestEnv(t)
	err = reg.Apply(context.Background(), env2, true)
	c.Assert(err, qt.IsNil)
	binding2, _ := env2.GetOwnGlobalValue(environment.NewGlobalIndex(values.NewSymbol("test-enforced")))
	fcls2 := binding2.(*machine.ForeignClosure)
	c.Assert(fcls2.Validator(), qt.IsNotNil)
}
```

**Note:** `createTestEnv` should follow existing test patterns in `apply_test.go`.
Read the file first.

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestApply_ContractEnforcement ./registry/...`
Expected: FAIL — `Apply` has wrong number of arguments

**Step 3: Write minimal implementation**

**3a. Add `contractEnforcement` to `engineConfig`** (`options.go`):

Add field to `engineConfig` struct:

```go
type engineConfig struct {
	// ... existing fields ...
	contractEnforcement bool // true if WithContractEnforcement was called
}
```

Add the engine option function:

```go
// WithContractEnforcement enables runtime type validation for primitives
// that declare ParamTypes contracts. When enabled, each contracted primitive
// validates its arguments against declared types before calling the
// implementation. This is a dry-run tool for verifying annotation correctness,
// not a production feature.
func WithContractEnforcement() EngineOption {
	return func(cfg *engineConfig) {
		cfg.contractEnforcement = true
	}
}
```

**3b. Update `Registry.Apply` signature** (`registry/apply.go:28`):

```go
func (p *Registry) Apply(ctx context.Context, env *environment.EnvironmentFrame, contractEnforcement bool) error {
```

Thread the bool to `registerRuntimePrimitive` and `registerExpandTimePrimitive`:

```go
	// Register runtime primitives
	for _, reg := range p.primitives {
		if reg.Phases.HasRuntime() {
			err := registerRuntimePrimitive(env, reg.Spec, contractEnforcement)
			if err != nil {
				return err
			}
		}
	}

	// Register expand-time primitives
	for _, reg := range p.primitives {
		if reg.Phases.HasExpand() {
			err := registerExpandTimePrimitive(env, reg.Spec, contractEnforcement)
			if err != nil {
				return err
			}
		}
	}
```

**3c. Update `registerRuntimePrimitive`** (`registry/apply.go:97`):

```go
func registerRuntimePrimitive(env *environment.EnvironmentFrame, spec PrimitiveSpec, contractEnforcement bool) error {
	sym := values.NewSymbol(spec.Name)
	env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)

	closure := machine.NewForeignClosure(
		env,
		spec.ParamCount,
		spec.IsVariadic,
		spec.Impl,
	)
	closure.SetName(spec.Name)

	if contractEnforcement {
		closure.SetValidator(buildValidator(spec))
	}

	err := env.SetOwnGlobalValue(environment.NewGlobalIndex(sym), closure)
	if err != nil {
		return werr.WrapForeignErrorf(err, "error registering %s", spec.Name)
	}
	return nil
}
```

**3d. Update `registerExpandTimePrimitive`** (`registry/apply.go:127`):

Same pattern — add `contractEnforcement bool` parameter and conditionally install validator.

**3e. Update all callers of `Apply`:**

- `engine.go:565`: `reg.Apply(ctx, env, cfg.contractEnforcement)`
  (Need to thread `cfg` to `applyBaseEnvironment` — add `contractEnforcement bool` param)
- `internal/bootstrap/environment_tiny.go:105`: `reg.Apply(ctx, env, false)`
  (Bootstrap never uses enforcement)
- `registry/apply_test.go` (7 sites): `reg.Apply(context.Background(), env, false)`

**3f. Update `Engine.RegisterPrimitive`** (`engine.go:395-407`):

The engine needs to store whether enforcement is enabled. Add a field to `Engine`:

```go
type Engine struct {
	// ... existing fields ...
	contractEnforcement bool
}
```

Set it during construction and use it in `RegisterPrimitive`:

```go
func (p *Engine) RegisterPrimitive(spec PrimitiveSpec) error {
	sym := values.NewSymbol(spec.Name)
	p.env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)

	closure := machine.NewForeignClosure(
		p.env,
		spec.ParamCount,
		spec.IsVariadic,
		spec.Impl,
	)

	if p.contractEnforcement {
		closure.SetValidator(buildValidator(spec))
	}

	return p.env.SetOwnGlobalValue(environment.NewGlobalIndex(sym), closure)
}
```

**Note:** `buildValidator` is in `registry/`, but `Engine.RegisterPrimitive` is in `wile/`
(the root package). The root package imports `registry/`, so it has access. But
`buildValidator` is unexported. Either:
- Export it as `BuildValidator` in `registry/`, or
- Have the engine call through registry (e.g., `registry.BuildValidator(spec)`)

Choose whichever matches the project's export conventions. Read existing patterns.

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestApply_ContractEnforcement ./registry/...`
Expected: PASS

**Step 5: Run all affected tests**

Run: `go test -v ./registry/... ./machine/... && go test -v .`
Expected: PASS

**Step 6: Commit**

```
feat(wile): add WithContractEnforcement engine option

Threads a contractEnforcement bool through Registry.Apply() to
registerRuntimePrimitive and registerExpandTimePrimitive. When true,
buildValidator() installs a type-checking closure on each ForeignClosure
that has ParamTypes. Default: false (no validation, zero overhead).
```

---

## Task 6: Integration Test — Full Stack Enforcement

**Files:**
- Modify: `engine_unit_test.go` or create a test in the root package

**Step 1: Write the integration test**

```go
func TestContractEnforcement_RejectsWrongType(t *testing.T) {
	c := qt.New(t)

	// Engine with enforcement enabled and files extension
	eng, err := NewEngine(context.Background(),
		WithContractEnforcement(),
		WithExtension(files.Extension),
	)
	c.Assert(err, qt.IsNil)

	// file-exists? expects a string. Passing an integer should fail.
	_, err = eng.EvalMultiple(context.Background(), `(file-exists? 42)`)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Matches, ".*file-exists\\?.*argument 0.*expected string.*")
}

func TestContractEnforcement_AcceptsCorrectType(t *testing.T) {
	c := qt.New(t)

	eng, err := NewEngine(context.Background(),
		WithContractEnforcement(),
		WithExtension(files.Extension),
	)
	c.Assert(err, qt.IsNil)

	// file-exists? with a valid string should work
	result, err := eng.EvalMultiple(context.Background(), `(file-exists? "/nonexistent-path")`)
	c.Assert(err, qt.IsNil)
	// Should return #f for nonexistent file
	c.Assert(result, valuestest.SchemeEquals, values.FalseValue)
}

func TestContractEnforcement_DisabledByDefault(t *testing.T) {
	c := qt.New(t)

	// Engine WITHOUT enforcement — wrong type caught by RequireArg, not by contract
	eng, err := NewEngine(context.Background(),
		WithExtension(files.Extension),
	)
	c.Assert(err, qt.IsNil)

	// file-exists? with integer still fails (RequireArg catches it),
	// but the error message should NOT contain "argument 0"
	_, err = eng.EvalMultiple(context.Background(), `(file-exists? 42)`)
	c.Assert(err, qt.IsNotNil)
}
```

**Note:** The third test verifies enforcement is off by default. The error still occurs
(from `RequireArg`), but the error message format differs. Check the actual error messages
during implementation to write the right assertion.

**Step 2: Run test**

Run: `go test -v -run TestContractEnforcement .`
Expected: PASS (if Tasks 1-5 are done and files annotations from Task 7 are in place)

**Important:** This test depends on Task 7 (files annotations). If running tasks in order,
run this test after Task 7 is complete.

**Step 3: Commit**

```
test(wile): add integration tests for contract enforcement

Tests the full enforcement stack: engine option -> Apply -> validator
installation -> dispatch path -> type check -> error.
```

---

## Task 7: Annotate Files Extension

**Files:**
- Modify: `extensions/files/register.go:43-71`

**Step 1: Read the implementations to verify types**

Read `extensions/files/prim_files.go`. Verify each primitive's actual type expectations
against `RequireArg` calls:

| Primitive | RequireArg in impl | ParamTypes | ReturnType |
|-----------|-------------------|------------|------------|
| `open-input-file` | `RequireArg[*values.String]` | `[TypeString]` | `TypeTextualInputPort` |
| `open-output-file` | `RequireArg[*values.String]` | `[TypeString]` | `TypeTextualOutputPort` |
| `open-binary-input-file` | `RequireArg[*values.String]` | `[TypeString]` | `TypeBinaryInputPort` |
| `open-binary-output-file` | `RequireArg[*values.String]` | `[TypeString]` | `TypeBinaryOutputPort` |
| `file-exists?` | `RequireArg[*values.String]` | `[TypeString]` | `TypeBoolean` |
| `delete-file` | `RequireArg[*values.String]` | `[TypeString]` | `TypeVoid` |
| `call-with-input-file` | `RequireType[*values.String]`, `RequireType[machine.Closure]` | `[TypeString, TypeProcedure]` | `TypeAny` |
| `call-with-output-file` | `RequireType[*values.String]`, `RequireType[machine.Closure]` | `[TypeString, TypeProcedure]` | `TypeAny` |
| `create-directory` | `RequireArg[*values.String]` | `[TypeString]` | `TypeVoid` |
| `delete-directory` | `RequireArg[*values.String]` | `[TypeString]` | `TypeVoid` |
| `directory-files` | `RequireArg[*values.String]` | `[TypeString]` | `TypeList` |
| `current-directory` | none (0 params) | `[]` (empty) | `TypeString` |
| `set-current-directory!` | `RequireArg[*values.String]` | `[TypeString]` | `TypeVoid` |

**Important:** Read the POSIX primitives at the end of the file (create-directory,
delete-directory, directory-files, current-directory, set-current-directory!) to confirm
their types. These were added in PR #565.

**Step 2: Add annotations**

Update `extensions/files/register.go`. Add import for `values`:

```go
import (
	_ "embed"

	"github.com/aalpar/wile/registry"
	"github.com/aalpar/wile/values"
)
```

Update each spec with `ParamTypes` and `ReturnType`. Example:

```go
{Name: "open-input-file", ParamCount: 1, Impl: PrimOpenInputFile,
    Doc: "Opens a file for textual input.", ParamNames: []string{"filename"}, Category: "files",
    ParamTypes: []values.ValueType{values.TypeString},
    ReturnType: values.TypeTextualInputPort},
```

Apply to all 13 primitives per the table above.

**Step 3: Run tests**

Run: `go test -v ./extensions/files/...`
Expected: PASS — annotations are metadata, no behavior change

**Step 4: Verify `,doc` output**

Run: `go run ./cmd/wile/`

At the REPL:
```
> ,doc open-input-file
```

Expected (approximately):
```
(open-input-file filename) → textual input port
  Opens a file for textual input.
  Parameters:
    filename : string
  Returns: textual input port
  Category: files
```

**Step 5: Commit**

```
feat(files): add type contracts to file extension primitives

Annotates all 13 file primitives with ParamTypes and ReturnType.
Types verified against RequireArg calls in prim_files.go.
```

---

## Task 8: Lint and Final Verification

**Step 1: Run linter**

Run: `make lint`
Expected: PASS

**Step 2: Run full test suite**

Run: `make test`
Expected: PASS

**Step 3: Run cover check**

Run: `make covercheck`
Expected: PASS

If any failures, fix them before marking this task complete.

**Step 4: Commit any fixes**

---

## Summary

| Task | Component | Files | Key Risk |
|------|-----------|-------|----------|
| 1 | ForeignClosure.validate | `machine/foreign_closure.go` | None — additive |
| 2 | callForeignCached dispatch | `machine/call_foreign_cached.go` | Hot path change — verify benchmarks |
| 3 | applyForeign dispatch | `machine/machine_context_apply.go` | Same as Task 2 |
| 4 | buildValidator | `registry/contract.go` | Variadic rest-list iteration |
| 5 | Engine option + Apply threading | `options.go`, `registry/apply.go`, `engine.go`, `bootstrap` | 12+ call sites |
| 6 | Integration test | root package test | Depends on Tasks 1-5 + 7 |
| 7 | Files annotations | `extensions/files/register.go` | Type verification against impl |
| 8 | Lint + final check | — | — |
