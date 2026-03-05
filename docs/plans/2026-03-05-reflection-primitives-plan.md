# Reflection Primitives Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add five runtime primitives (`procedure-arity`, `procedure-name`, `procedure-source-location`, `procedure-bound-symbols`, `procedure-type`) that inspect procedure metadata, returning plain Scheme data.

**Architecture:** All five primitives share the same pattern — type-switch on `values.Callable` to extract metadata from the concrete closure type. One infrastructure change is needed first: `ForeignClosure` needs a `name` field so `(procedure-name +)` returns `"+"`. The primitives live in `registry/core/` as runtime-only registrations.

**Tech Stack:** Go, existing `machine` and `registry` packages, `werr` error sentinels.

**Design doc:** `docs/plans/2026-03-05-reflection-primitives-design.md`

---

### Task 1: Add `name` field to `ForeignClosure`

**Files:**
- Modify: `machine/foreign_closure.go` (add `name` field, `Name()` accessor, `SetName()` mutator)
- Modify: `machine/foreign_closure_test.go` (test the new field)

**Step 1: Write the failing test**

In `machine/foreign_closure_test.go`, add a test for the `Name()` method:

```go
func TestForeignClosure_Name(t *testing.T) {
	cls := newTestForeignClosure()
	qt.Assert(t, cls.Name(), qt.Equals, "")

	cls.SetName("test-fn")
	qt.Assert(t, cls.Name(), qt.Equals, "test-fn")
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestForeignClosure_Name ./machine/`
Expected: FAIL — `Name()` and `SetName()` don't exist yet.

**Step 3: Write minimal implementation**

In `machine/foreign_closure.go`, add the `name` field to the struct and two methods:

```go
// In ForeignClosure struct, add field:
name       string

// New methods:
func (p *ForeignClosure) Name() string {
	return p.name
}

func (p *ForeignClosure) SetName(name string) {
	p.name = name
}
```

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestForeignClosure_Name ./machine/`
Expected: PASS

**Step 5: Commit**

```
git add machine/foreign_closure.go machine/foreign_closure_test.go
git commit -m "feat(machine): add name field to ForeignClosure"
```

---

### Task 2: Thread `PrimitiveSpec.Name` into `ForeignClosure` during registry application

**Files:**
- Modify: `registry/apply.go` (call `SetName` after creating closure)

**Step 1: Write the failing test**

This is best verified via the `procedure-name` primitive later (Task 5). For now, make the change and verify existing tests still pass.

**Step 2: Modify `registerRuntimePrimitive` and `registerExpandTimePrimitive`**

In `registry/apply.go`, after creating the closure in both functions, add:

```go
closure.SetName(spec.Name)
```

This goes on the line after `closure := machine.NewForeignClosure(...)` in both `registerRuntimePrimitive` (line ~100) and `registerExpandTimePrimitive` (line ~130).

**Step 3: Run existing tests to verify no regressions**

Run: `go test ./registry/...`
Expected: PASS (no behavior change, just populating a new field)

**Step 4: Commit**

```
git add registry/apply.go
git commit -m "feat(registry): populate ForeignClosure name from PrimitiveSpec"
```

---

### Task 3: Create registration file `registry/core/reflection.go`

**Files:**
- Create: `registry/core/reflection.go`
- Modify: `registry/core/register.go` (add `addReflection` to Builder)

**Step 1: Create the registration file**

Create `registry/core/reflection.go` with the `addReflection` function registering all five primitives at `PhaseRuntime`:

```go
package core

import (
	"github.com/aalpar/wile/registry"
)

func addReflection(r *registry.Registry) error {
	//nolint:govet
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "procedure-arity", ParamCount: 1, Impl: PrimProcedureArity,
			Doc: "Returns the arity of a procedure.", ParamNames: []string{"proc"}, Category: "reflection"},
		{Name: "procedure-name", ParamCount: 1, Impl: PrimProcedureName,
			Doc: "Returns the name of a procedure, or #f if anonymous.", ParamNames: []string{"proc"}, Category: "reflection"},
		{Name: "procedure-source-location", ParamCount: 1, Impl: PrimProcedureSourceLocation,
			Doc: "Returns (file line column) for a procedure, or #f if unavailable.", ParamNames: []string{"proc"}, Category: "reflection"},
		{Name: "procedure-bound-symbols", ParamCount: 1, Impl: PrimProcedureBoundSymbols,
			Doc: "Returns the list of symbols bound in a closure's environment, or #f.", ParamNames: []string{"proc"}, Category: "reflection"},
		{Name: "procedure-type", ParamCount: 1, Impl: PrimProcedureType,
			Doc: "Returns a symbol classifying the procedure type.", ParamNames: []string{"proc"}, Category: "reflection"},
	}, registry.PhaseRuntime)

	return nil
}
```

**Step 2: Add `addReflection` to the Builder**

In `registry/core/register.go`, add `addReflection` to the Builder slice, after `addControl`:

```go
var Builder = registry.NewRegistryBuilder(
	addSpecialForms,
	addPredicates,
	addEquality,
	addBoolean,
	addPairs,
	addLists,
	addArithmetic,
	addControl,
	addReflection,   // <-- new
	addVectors,
	...
)
```

**Step 3: This won't compile until Task 4 creates the Prim* functions. Skip running tests for now.**

**Step 4: No commit yet — continue to Task 4.**

---

### Task 4: Implement the five primitives in `registry/core/prim_reflection.go`

**Files:**
- Create: `registry/core/prim_reflection.go`

**Step 1: Create `prim_reflection.go` with all five primitives**

All five share the same guard: check `values.Callable`, type-switch on the concrete type. The sentinel `werr.ErrNotAProcedure` already exists in `werr/werr.go:65`.

```go
package core

import (
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// closureArity returns the Scheme arity value for a single closure.
// Fixed arity → integer, variadic → (min . #f).
func closureArity(paramCount int, isVariadic bool) values.Value {
	if isVariadic {
		min := values.NewInteger(int64(paramCount - 1))
		return values.NewPair(min, values.FalseValue)
	}
	return values.NewInteger(int64(paramCount))
}

// PrimProcedureArity implements (procedure-arity proc).
func PrimProcedureArity(mc *machine.MachineContext) error {
	o := mc.Arg(0)
	callable, ok := o.(values.Callable)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAProcedure,
			"procedure-arity: expected procedure")
	}
	switch v := callable.(type) {
	case *machine.MachineClosure:
		tpl := v.Template()
		mc.SetValue(closureArity(tpl.ParameterCount(), tpl.IsVariadic()))
	case *machine.ForeignClosure:
		mc.SetValue(closureArity(v.ParameterCount(), v.IsVariadic()))
	case *machine.CaseLambdaClosure:
		clauses := v.Clauses()
		items := make([]values.Value, len(clauses))
		for i, clause := range clauses {
			tpl := clause.Template()
			items[i] = closureArity(tpl.ParameterCount(), tpl.IsVariadic())
		}
		mc.SetValue(values.NewList(items...))
	case *machine.Parameter:
		mc.SetValue(closureArity(1, true))
	case *machine.ComposableContinuation:
		mc.SetValue(values.NewInteger(1))
	default:
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// PrimProcedureName implements (procedure-name proc).
func PrimProcedureName(mc *machine.MachineContext) error {
	o := mc.Arg(0)
	callable, ok := o.(values.Callable)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAProcedure,
			"procedure-name: expected procedure")
	}
	switch v := callable.(type) {
	case *machine.MachineClosure:
		name := v.Template().Name()
		if name == "" {
			mc.SetValue(values.FalseValue)
		} else {
			mc.SetValue(values.NewString(name))
		}
	case *machine.ForeignClosure:
		name := v.Name()
		if name == "" {
			mc.SetValue(values.FalseValue)
		} else {
			mc.SetValue(values.NewString(name))
		}
	case *machine.CaseLambdaClosure:
		clauses := v.Clauses()
		if len(clauses) > 0 {
			name := clauses[0].Template().Name()
			if name != "" {
				mc.SetValue(values.NewString(name))
				return nil
			}
		}
		mc.SetValue(values.FalseValue)
	default:
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// PrimProcedureSourceLocation implements (procedure-source-location proc).
func PrimProcedureSourceLocation(mc *machine.MachineContext) error {
	o := mc.Arg(0)
	callable, ok := o.(values.Callable)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAProcedure,
			"procedure-source-location: expected procedure")
	}

	switch v := callable.(type) {
	case *machine.MachineClosure:
		mc.SetValue(templateSourceLocation(v.Template()))
	case *machine.CaseLambdaClosure:
		clauses := v.Clauses()
		if len(clauses) > 0 {
			mc.SetValue(templateSourceLocation(clauses[0].Template()))
		} else {
			mc.SetValue(values.FalseValue)
		}
	default:
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// templateSourceLocation extracts the first non-nil source context from
// a NativeTemplate and returns it as a (file line column) list, or #f.
func templateSourceLocation(tpl *machine.NativeTemplate) values.Value {
	src := tpl.SourceAt(0)
	if src == nil {
		return values.FalseValue
	}
	if src.File == "" {
		return values.FalseValue
	}
	return values.NewList(
		values.NewString(src.File),
		values.NewInteger(int64(src.Start.Line())),
		values.NewInteger(int64(src.Start.Column())),
	)
}

// PrimProcedureBoundSymbols implements (procedure-bound-symbols proc).
func PrimProcedureBoundSymbols(mc *machine.MachineContext) error {
	o := mc.Arg(0)
	callable, ok := o.(values.Callable)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAProcedure,
			"procedure-bound-symbols: expected procedure")
	}

	switch v := callable.(type) {
	case *machine.MachineClosure:
		mc.SetValue(closureBoundSymbols(v))
	case *machine.CaseLambdaClosure:
		clauses := v.Clauses()
		if len(clauses) > 0 {
			mc.SetValue(closureBoundSymbols(clauses[0]))
		} else {
			mc.SetValue(values.FalseValue)
		}
	default:
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// closureBoundSymbols extracts the symbol names from a MachineClosure's
// captured local environment.
func closureBoundSymbols(cls *machine.MachineClosure) values.Value {
	// MachineClosure.env is unexported — need accessor.
	// TODO: This requires a MachineClosure.Env() accessor or
	// an alternative approach. See Task 4 notes.
	return values.FalseValue
}

// PrimProcedureType implements (procedure-type proc).
func PrimProcedureType(mc *machine.MachineContext) error {
	o := mc.Arg(0)
	callable, ok := o.(values.Callable)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAProcedure,
			"procedure-type: expected procedure")
	}

	var typeName string
	switch callable.(type) {
	case *machine.MachineClosure:
		typeName = "lambda"
	case *machine.ForeignClosure:
		typeName = "foreign"
	case *machine.CaseLambdaClosure:
		typeName = "case-lambda"
	case *machine.Parameter:
		typeName = "parameter"
	case *machine.ComposableContinuation:
		typeName = "continuation"
	default:
		typeName = "unknown"
	}
	mc.SetValue(values.NewSymbol(typeName))
	return nil
}
```

**OPEN QUESTION for implementer: `MachineClosure.env` is unexported.**

`closureBoundSymbols` needs to access the closure's environment to call `LocalEnvironment().Keys()`. Two options:

**Option A:** Add `Env() *environment.EnvironmentFrame` accessor to `MachineClosure` (mirroring `ForeignClosure.Env()`). Simple, consistent.

**Option B:** Add a `BoundSymbols() []*values.Symbol` method directly on `MachineClosure` that does the key extraction internally. Keeps env access encapsulated.

Recommend **Option A** — `ForeignClosure` already exposes `Env()`, so this is prior art in the codebase.

**Step 2: Verify compilation**

Run: `go build ./registry/core/...`
Expected: Should compile (modulo the `closureBoundSymbols` TODO).

**Step 3: Commit**

```
git add registry/core/reflection.go registry/core/prim_reflection.go registry/core/register.go
git commit -m "feat(core): add five reflection primitives"
```

---

### Task 5: Add `Env()` accessor to `MachineClosure` and complete `closureBoundSymbols`

**Files:**
- Modify: `machine/machine_closure.go` (add `Env()` accessor)
- Modify: `machine/machine_closure_test.go` (test accessor)
- Modify: `registry/core/prim_reflection.go` (complete `closureBoundSymbols`)

**Step 1: Write the failing test for `Env()`**

In `machine/machine_closure_test.go`:

```go
func TestMachineClosure_Env(t *testing.T) {
	env := // use existing test env setup pattern from this file
	tpl := NewNativeTemplate(0, 0, false)
	cls := NewClosureWithTemplate(tpl, env)
	qt.Assert(t, cls.Env(), qt.Equals, env)
}
```

Follow the existing test pattern in this file for environment setup.

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestMachineClosure_Env ./machine/`
Expected: FAIL — `Env()` doesn't exist.

**Step 3: Add `Env()` to `MachineClosure`**

In `machine/machine_closure.go`:

```go
func (p *MachineClosure) Env() *environment.EnvironmentFrame {
	return p.env
}
```

**Step 4: Complete `closureBoundSymbols` in `prim_reflection.go`**

Replace the placeholder:

```go
func closureBoundSymbols(cls *machine.MachineClosure) values.Value {
	env := cls.Env()
	if env == nil {
		return values.FalseValue
	}
	local := env.LocalEnvironment()
	if local == nil {
		return values.FalseValue
	}
	keys := local.Keys()
	if len(keys) == 0 {
		return values.EmptyList
	}
	syms := make([]values.Value, 0, len(keys))
	for sym := range keys {
		s := sym
		syms = append(syms, &s)
	}
	return values.NewList(syms...)
}
```

**Note:** `local.Keys()` returns `map[values.Symbol]int`. We iterate the map and convert each key to a `*values.Symbol` for `NewList`. The order is non-deterministic (map iteration), which is acceptable for this introspection primitive.

**Step 5: Run compilation**

Run: `go build ./...`
Expected: PASS

**Step 6: Commit**

```
git add machine/machine_closure.go machine/machine_closure_test.go registry/core/prim_reflection.go
git commit -m "feat(machine): add Env accessor to MachineClosure, complete closureBoundSymbols"
```

---

### Task 6: Write table-driven tests for all five primitives

**Files:**
- Create: `registry/core/prim_reflection_test.go`

**Step 1: Write tests using `testhelpers.RunSchemeCode`**

Follow the project's table-driven test convention using `testhelpers.SchemeCodeTestCase`.

```go
package core_test

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
)

func TestProcedureArity(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Fixed arity — foreign
		{Name: "fixed foreign", Code: `(procedure-arity car)`, Expected: values.NewInteger(1)},
		{Name: "fixed foreign 2", Code: `(procedure-arity cons)`, Expected: values.NewInteger(2)},
		// Fixed arity — lambda
		{Name: "fixed lambda", Code: `(procedure-arity (lambda (x y) x))`, Expected: values.NewInteger(2)},
		// Variadic — foreign
		{Name: "variadic foreign", Code: `(procedure-arity +)`,
			Expected: values.NewPair(values.NewInteger(0), values.FalseValue)},
		// Variadic — lambda
		{Name: "variadic lambda", Code: `(procedure-arity (lambda (x . rest) x))`,
			Expected: values.NewPair(values.NewInteger(1), values.FalseValue)},
		// case-lambda
		{Name: "case-lambda", Code: `(procedure-arity (case-lambda ((x) x) ((x y) x)))`,
			Expected: values.NewList(values.NewInteger(1), values.NewInteger(2))},
		// Continuation
		{Name: "continuation", Code: `
			(call-with-composable-continuation
			  (lambda (k) (procedure-arity k))
			  (default-continuation-prompt-tag))`,
			Expected: values.NewInteger(1)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.SchemeString(), qt.Equals, tc.Expected.SchemeString())
		})
	}
}

func TestProcedureArityErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "not a procedure", Code: `(procedure-arity 42)`},
		{Name: "string", Code: `(procedure-arity "hello")`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

func TestProcedureName(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "foreign", Code: `(procedure-name car)`, Expected: values.NewString("car")},
		{Name: "named define", Code: `(begin (define (foo x) x) (procedure-name foo))`,
			Expected: values.NewString("foo")},
		{Name: "anonymous lambda", Code: `(procedure-name (lambda (x) x))`,
			Expected: values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.SchemeString(), qt.Equals, tc.Expected.SchemeString())
		})
	}
}

func TestProcedureNameErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "not a procedure", Code: `(procedure-name 42)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

func TestProcedureSourceLocation(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Foreign closures have no source
		{Name: "foreign", Code: `(procedure-source-location car)`, Expected: values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.SchemeString(), qt.Equals, tc.Expected.SchemeString())
		})
	}
}

func TestProcedureSourceLocationErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "not a procedure", Code: `(procedure-source-location 42)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

func TestProcedureType(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "lambda", Code: `(procedure-type (lambda (x) x))`,
			Expected: values.NewSymbol("lambda")},
		{Name: "foreign", Code: `(procedure-type car)`,
			Expected: values.NewSymbol("foreign")},
		{Name: "case-lambda", Code: `(procedure-type (case-lambda ((x) x) ((x y) x)))`,
			Expected: values.NewSymbol("case-lambda")},
		{Name: "parameter", Code: `(procedure-type (make-parameter 0))`,
			Expected: values.NewSymbol("parameter")},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.SchemeString(), qt.Equals, tc.Expected.SchemeString())
		})
	}
}

func TestProcedureTypeErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "not a procedure", Code: `(procedure-type 42)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

func TestProcedureBoundSymbols(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Foreign closures have no meaningful bound symbols
		{Name: "foreign", Code: `(procedure-bound-symbols car)`, Expected: values.FalseValue},
		// Continuations have no bound symbols
		{Name: "parameter", Code: `(procedure-bound-symbols (make-parameter 0))`,
			Expected: values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.SchemeString(), qt.Equals, tc.Expected.SchemeString())
		})
	}
}

func TestProcedureBoundSymbolsErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "not a procedure", Code: `(procedure-bound-symbols 42)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
```

**Notes for implementer:**
- `procedure-source-location` for lambdas requires testing with `include` or a file-backed evaluation to get real source info. The `RunSchemeCode` helper uses string input which may or may not populate source contexts. Check what `SourceAt(0)` returns for string-evaluated lambdas and adjust test expectations accordingly.
- `procedure-bound-symbols` for lambdas with closures: test with `(let ((x 1)) (procedure-bound-symbols (lambda () x)))` — but verify what `LocalEnvironment().Keys()` actually returns for the closure's env. The keys include the lambda's own parameters, not the closed-over variables from parent scopes. Adjust the test to match actual behavior.
- `procedure-arity` for `Parameter`: The design says `(0 . #f)` since parameters accept 0 or 1 args. Verify `Parameter.AcceptsArity` behavior — it returns true for 0 and 1.

**Step 2: Run all tests**

Run: `go test -v -run "TestProcedure" ./registry/core/...`
Expected: All PASS

**Step 3: Run lint**

Run: `make lint`
Expected: PASS

**Step 4: Commit**

```
git add registry/core/prim_reflection_test.go
git commit -m "test(core): add table-driven tests for reflection primitives"
```

---

### Task 7: Run full test suite and lint

**Step 1: Run full tests**

Run: `make test`
Expected: All PASS

**Step 2: Run lint**

Run: `make lint`
Expected: PASS

**Step 3: Run covercheck**

Run: `make covercheck`
Expected: PASS

**Step 4: Fix any issues discovered, re-run until clean**

---

### Task 8: Update TODO.md

**Files:**
- Modify: `TODO.md`

**Step 1: Mark the reflection primitives item as done**

Change `- [ ] **Reflection primitives**` to `- [x] **Reflection primitives**`

**Step 2: Commit**

```
git add TODO.md
git commit -m "chore: mark reflection primitives as done"
```
