# Opaque Values Implementation Plan

**Status:** Complete (PR #566)

**Goal:** Add a two-layer opaque value system — an `Opaque` interface for capability checking and an `OpaqueValue` convenience struct for wrapping arbitrary Go objects.

**Architecture:** Interface (`Opaque`) in `values/opaque.go` checked by `opaque?` predicate. Convenience struct (`OpaqueValue`) in `values/opaque_value.go` with atomic ID counter, identity equality, `#<tag:id>` display. Two predicates (`opaque?`, `opaque-tag`) registered in `registry/core/`.

**Tech Stack:** Go standard library only. `sync/atomic` for ID counter. `fmt` for SchemeString.

**Design doc:** `plans/2026-03-24-opaque-values-design.md`

---

### Task 1: Opaque Interface

**Files:**
- Create: `values/opaque.go`

**Step 1: Write the interface file**

```go
// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// ...standard Apache 2.0 header...

package values

// Opaque marks a Value as opaque to Scheme code.
// Any Value type can opt in by implementing this single method.
// The opaque? predicate checks this interface (capability check, not type check).
type Opaque interface {
	OpaqueTag() string
}
```

**Step 2: Verify it compiles**

Run: `go build ./values/...`
Expected: PASS (no compilation errors)

**Step 3: Commit**

```
git add values/opaque.go
git commit -m "values: add Opaque interface"
```

---

### Task 2: OpaqueValue Convenience Struct — Tests First

**Files:**
- Create: `values/opaque_value_test.go`

**Step 1: Write the failing tests**

```go
package values_test

import (
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
)

func TestOpaqueValue_SchemeString(t *testing.T) {
	tcs := []struct {
		name string
		tag  string
		val  any
		want string // prefix only — ID is unpredictable
	}{
		{name: "db-conn tag", tag: "db-conn", val: "fake-db", want: "#<db-conn:"},
		{name: "session tag", tag: "session", val: 42, want: "#<session:"},
		{name: "nil value", tag: "empty", val: nil, want: "#<empty:"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			v := values.NewOpaqueValue(tc.tag, tc.val)
			s := v.SchemeString()
			qt.Assert(t, strings.HasPrefix(s, tc.want), qt.IsTrue, qt.Commentf("got %q", s))
			qt.Assert(t, strings.HasSuffix(s, ">"), qt.IsTrue)
		})
	}
}

func TestOpaqueValue_SchemeString_NilReceiver(t *testing.T) {
	var v *values.OpaqueValue
	qt.Assert(t, v.SchemeString(), qt.Equals, "#<opaque:void>")
}

func TestOpaqueValue_EqualTo(t *testing.T) {
	a := values.NewOpaqueValue("tag", "inner")
	b := values.NewOpaqueValue("tag", "inner")

	tcs := []struct {
		name string
		lhs  values.Value
		rhs  values.Value
		want bool
	}{
		{name: "same object", lhs: a, rhs: a, want: true},
		{name: "different objects same contents", lhs: a, rhs: b, want: false},
		{name: "different type", lhs: a, rhs: values.TrueValue, want: false},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, tc.lhs.EqualTo(tc.rhs), qt.Equals, tc.want)
		})
	}
}

func TestOpaqueValue_IsVoid(t *testing.T) {
	v := values.NewOpaqueValue("tag", "val")
	qt.Assert(t, v.IsVoid(), qt.IsFalse)

	var nilOpaque *values.OpaqueValue
	qt.Assert(t, nilOpaque.IsVoid(), qt.IsTrue)
}

func TestOpaqueValue_OpaqueTag(t *testing.T) {
	v := values.NewOpaqueValue("my-tag", nil)
	qt.Assert(t, v.OpaqueTag(), qt.Equals, "my-tag")
}

func TestOpaqueValue_Unwrap(t *testing.T) {
	type myDB struct{ name string }
	db := &myDB{name: "test"}
	v := values.NewOpaqueValue("db", db)

	got, ok := v.Unwrap().(*myDB)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, got.name, qt.Equals, "test")
}

func TestOpaqueValue_UniqueIDs(t *testing.T) {
	a := values.NewOpaqueValue("tag", nil)
	b := values.NewOpaqueValue("tag", nil)
	qt.Assert(t, a.SchemeString() != b.SchemeString(), qt.IsTrue,
		qt.Commentf("expected different IDs: %s vs %s", a.SchemeString(), b.SchemeString()))
}

func TestOpaqueValue_ImplementsOpaque(t *testing.T) {
	v := values.NewOpaqueValue("tag", nil)
	var o values.Opaque = v
	qt.Assert(t, o.OpaqueTag(), qt.Equals, "tag")
}
```

**Step 2: Run tests to verify they fail**

Run: `go test -v -run TestOpaqueValue ./values/...`
Expected: FAIL — `NewOpaqueValue` undefined

---

### Task 3: OpaqueValue Convenience Struct — Implementation

**Files:**
- Create: `values/opaque_value.go`

**Step 1: Write the implementation**

```go
// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// ...standard Apache 2.0 header...

package values

import (
	"fmt"
	"sync/atomic"
)

var (
	_ Value  = (*OpaqueValue)(nil)
	_ Opaque = (*OpaqueValue)(nil)

	opaqueValueIDCounter atomic.Uint64
)

// OpaqueValue wraps an arbitrary Go object as a Scheme value.
// Construction is Go-only via NewOpaqueValue. The inner value
// is accessible only from Go via Unwrap.
type OpaqueValue struct {
	tag string
	id  uint64
	val any
}

// NewOpaqueValue creates a new opaque value with the given tag and inner value.
func NewOpaqueValue(tag string, val any) *OpaqueValue {
	id := opaqueValueIDCounter.Add(1)
	return &OpaqueValue{
		tag: tag,
		id:  id,
		val: val,
	}
}

// OpaqueTag returns the tag string identifying this opaque value's kind.
func (p *OpaqueValue) OpaqueTag() string {
	return p.tag
}

// Unwrap returns the inner Go value. Go-only — not exposed to Scheme.
func (p *OpaqueValue) Unwrap() any {
	return p.val
}

// SchemeString returns the Scheme representation of this opaque value.
func (p *OpaqueValue) SchemeString() string {
	if p == nil {
		return "#<opaque:void>"
	}
	return fmt.Sprintf("#<%s:%d>", p.tag, p.id)
}

// IsVoid returns true if this opaque value is nil.
func (p *OpaqueValue) IsVoid() bool {
	return p == nil
}

// EqualTo returns true only if both are the same object (identity equality).
func (p *OpaqueValue) EqualTo(v Value) bool {
	other, ok := v.(*OpaqueValue)
	if !ok {
		return false
	}
	return p == other
}
```

**Step 2: Run tests to verify they pass**

Run: `go test -v -run TestOpaqueValue ./values/...`
Expected: PASS

**Step 3: Run lint**

Run: `make lint`
Expected: PASS

**Step 4: Commit**

```
git add values/opaque_value.go values/opaque_value_test.go
git commit -m "values: add OpaqueValue convenience struct"
```

---

### Task 4: Error Sentinel

**Files:**
- Modify: `werr/werr.go`

**Step 1: Add sentinel**

Add to the sentinel block in `werr/werr.go` near the other `ErrNotA*` entries:

```go
ErrNotAnOpaqueValue = NewStaticError("not an opaque value")
```

**Step 2: Verify it compiles**

Run: `go build ./werr/...`
Expected: PASS

**Step 3: Commit**

```
git add werr/werr.go
git commit -m "werr: add ErrNotAnOpaqueValue sentinel"
```

---

### Task 5: Predicates — Tests First

**Files:**
- Create: `registry/core/prim_opaque_test.go`

**Step 1: Write the failing tests**

```go
package core_test

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"
)

func TestPrimOpaqueQ(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "number is not opaque", Code: `(opaque? 42)`, Expected: values.FalseValue},
		{Name: "string is not opaque", Code: `(opaque? "hello")`, Expected: values.FalseValue},
		{Name: "boolean is not opaque", Code: `(opaque? #t)`, Expected: values.FalseValue},
		{Name: "list is not opaque", Code: `(opaque? '(1 2 3))`, Expected: values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestPrimOpaqueTag(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "number", Code: `(opaque-tag 42)`},
		{Name: "string", Code: `(opaque-tag "hello")`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
```

Note: Testing `opaque?` returning `#t` and `opaque-tag` on actual opaque values requires Go-side injection — this is covered in Task 7 (integration tests). These tests validate the negative cases via pure Scheme.

**Step 2: Run tests to verify they fail**

Run: `go test -v -run "TestPrimOpaque" ./registry/core/...`
Expected: FAIL — `opaque?` is undefined

---

### Task 6: Predicates — Implementation and Registration

**Files:**
- Create: `registry/core/prim_opaque.go`
- Create: `registry/core/opaque.go`
- Modify: `registry/core/register.go`

**Step 1: Write the predicate implementations** (`prim_opaque.go`)

```go
// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// ...standard Apache 2.0 header...

package core

import (
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// PrimOpaqueQ implements the opaque? predicate.
// Returns #t if the argument satisfies the Opaque interface.
func PrimOpaqueQ(mc *machine.MachineContext) error {
	_, ok := mc.Arg(0).(values.Opaque)
	mc.SetValue(values.BoolToBoolean(ok))
	return nil
}

// PrimOpaqueTag implements the opaque-tag primitive.
// Returns the tag string of an opaque value.
func PrimOpaqueTag(mc *machine.MachineContext) error {
	o, ok := mc.Arg(0).(values.Opaque)
	if !ok {
		return werr.WrapForeignErrorf(
			werr.ErrNotAnOpaqueValue,
			"opaque-tag: expected an opaque value but got %T",
			mc.Arg(0),
		)
	}
	mc.SetValue(values.NewString(o.OpaqueTag()))
	return nil
}
```

**Step 2: Write the registration file** (`opaque.go`)

```go
// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// ...standard Apache 2.0 header...

package core

import (
	"github.com/aalpar/wile/registry"
)

func addOpaque(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "opaque?", ParamCount: 1, Impl: PrimOpaqueQ,
			Doc: "Returns #t if obj is an opaque value.", ParamNames: []string{"obj"}, Category: "predicates"},
		{Name: "opaque-tag", ParamCount: 1, Impl: PrimOpaqueTag,
			Doc: "Returns the tag string of an opaque value.", ParamNames: []string{"obj"}, Category: "predicates"},
	}, registry.PhaseRuntime|registry.PhaseExpand)

	return nil
}
```

**Step 3: Add `addOpaque` to Builder in `register.go`**

Add `addOpaque` to the Builder list, after `addBoxes` and before `addHashtables`:

```go
var Builder = registry.NewRegistryBuilder(
	// ... existing entries ...
	addBoxes,
	addOpaque,      // <-- new
	addHashtables,
	// ... rest ...
)
```

**Step 4: Run predicate tests**

Run: `go test -v -run "TestPrimOpaque" ./registry/core/...`
Expected: PASS

**Step 5: Run lint**

Run: `make lint`
Expected: PASS

**Step 6: Commit**

```
git add registry/core/prim_opaque.go registry/core/opaque.go registry/core/register.go
git commit -m "registry/core: add opaque? and opaque-tag predicates"
```

---

### Task 7: Integration Tests — Go-Injected Opaque Values

**Files:**
- Create: `registry/core/prim_opaque_test.go` (extend from Task 5)

**Step 1: Add integration tests with Go-injected opaque values**

Append to `prim_opaque_test.go`. These tests use `RunSchemeCodeWithEnv` to inject
an `OpaqueValue` binding, then test from the Scheme side:

```go
func TestPrimOpaqueQ_WithOpaqueValue(t *testing.T) {
	env := testhelpers.NewTestEnvWithBindings(t, map[string]values.Value{
		"my-opaque": values.NewOpaqueValue("test-tag", "inner"),
	})

	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "opaque value is opaque", Code: `(opaque? my-opaque)`, Expected: values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCodeWithEnv(t, tc.Code, env)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestPrimOpaqueTag_WithOpaqueValue(t *testing.T) {
	env := testhelpers.NewTestEnvWithBindings(t, map[string]values.Value{
		"my-opaque": values.NewOpaqueValue("test-tag", "inner"),
	})

	result, err := testhelpers.RunSchemeCodeWithEnv(t, `(opaque-tag my-opaque)`, env)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewString("test-tag"))
}
```

Note: The exact API for injecting bindings depends on what `testhelpers` provides. Check
`testhelpers.RunSchemeCodeWithEnv` or `testhelpers.RunProgramASTWithEnv` for the correct
signature. If no `NewTestEnvWithBindings` helper exists, inject via
`RunSchemeCodeWithEnv(t, code, env)` where `env` has been set up with
`env.SetOwnGlobalValue(symbol, value)`.

**Step 2: Run all opaque tests**

Run: `go test -v -run "TestPrimOpaque" ./registry/core/...`
Expected: PASS

**Step 3: Commit**

```
git add registry/core/prim_opaque_test.go
git commit -m "registry/core: add integration tests for opaque predicates"
```

---

### Task 8: Full Test Suite + Lint

**Step 1: Run full test suite**

Run: `make test`
Expected: PASS — no regressions

**Step 2: Run lint**

Run: `make lint`
Expected: PASS

**Step 3: Run covercheck**

Run: `make covercheck`
Expected: PASS

---

### Task 9: Update TODO.md

**Files:**
- Modify: `TODO.md`

**Step 1: Mark the OpaqueValue item as done**

Change line 70 from:
```
- [ ] **OpaqueValue type** [Values, Embedding]: ...
```
to:
```
- [x] **OpaqueValue type** [Values, Embedding]: ...
```

**Step 2: Commit**

```
git add TODO.md
git commit -m "docs: mark OpaqueValue as complete"
```
