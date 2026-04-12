# Extensible Type Constraints Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Replace the closed `ValueType` enum with a `TypeConstraint` interface so documentation and validation can express record-specific and user-defined types.

**Architecture:** Introduce a `TypeConstraint` interface in `values/` that `ValueType` implements. Add `NamedTypeConstraint` (unresolved docstring names) and `RecordTypeConstraint` (record type checking with parent-chain walk). Change all consumers (`PrimitiveSpec`, `DocInfo`, `docparse`) from `ValueType` to `TypeConstraint`. Add `parent *RecordType` field for future SRFI-99/131 inheritance.

**Tech Stack:** Go, no new dependencies. Pure refactoring + new types.

**Design doc:** `plans/EXTENSIBLE-TYPE-CONSTRAINTS.md`

---

### Task 1: Define TypeConstraint Interface and Make ValueType Implement It

**Files:**
- Modify: `values/value_type.go`
- Test: `values/value_type_test.go`

**Step 1: Write the failing test**

Add to `values/value_type_test.go`:

```go
func TestValueTypeImplementsTypeConstraint(t *testing.T) {
	c := qt.New(t)
	var tc values.TypeConstraint = values.TypeInteger
	c.Assert(tc.Name(), qt.Equals, "integer")
	c.Assert(tc.Description(), qt.Equals, "exact integer")
	narrowed, ok, err := tc.Check(values.NewInteger(42))
	c.Assert(err, qt.IsNil)
	c.Assert(ok, qt.IsTrue)
	c.Assert(narrowed, qt.IsNotNil)
}

func TestTypeAnyVsNil(t *testing.T) {
	c := qt.New(t)
	// TypeAny is a valid TypeConstraint that accepts anything
	var tc values.TypeConstraint = values.TypeAny
	c.Assert(tc.Name(), qt.Equals, "any")
	_, ok, err := tc.Check(values.NewInteger(1))
	c.Assert(err, qt.IsNil)
	c.Assert(ok, qt.IsTrue)

	// nil TypeConstraint means "unspecified" — distinct from TypeAny
	var unspecified values.TypeConstraint
	c.Assert(unspecified == nil, qt.IsTrue)
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run 'TestValueTypeImplementsTypeConstraint|TestTypeAnyVsNil' ./values/...`
Expected: FAIL — `TypeConstraint` is not defined yet.

**Step 3: Write minimal implementation**

In `values/value_type.go`, add the interface and `Name()` method:

```go
// TypeConstraint describes a type expectation for documentation and validation.
// Built-in types are represented by ValueType constants.
// User-defined types (e.g., record types) implement this interface directly.
//
// A nil TypeConstraint means "unspecified" (no type info declared).
// TypeAny means "explicitly accepts any value."
type TypeConstraint interface {
	// Name returns the Scheme-facing type name (e.g., "integer", "point").
	Name() string
	// Description returns a human-readable description.
	Description() string
	// Check tests whether v satisfies this constraint.
	// On success, returns the narrowed value and true.
	// On failure, returns nil, false, and an error describing the mismatch.
	Check(Value) (any, bool, error)
}

// Name returns the Scheme-facing type name. Alias for String().
// Satisfies the TypeConstraint interface.
func (p ValueType) Name() string {
	return p.String()
}
```

**Step 4: Run test to verify it passes**

Run: `go test -v -run 'TestValueTypeImplementsTypeConstraint|TestTypeAnyVsNil' ./values/...`
Expected: PASS

**Step 5: Run full values test suite**

Run: `go test -v ./values/...`
Expected: PASS — no regressions.

---

### Task 2: Add NamedTypeConstraint

**Files:**
- Modify: `values/value_type.go`
- Test: `values/value_type_test.go`

**Step 1: Write the failing test**

Add to `values/value_type_test.go`:

```go
func TestNamedTypeConstraint(t *testing.T) {
	c := qt.New(t)
	tc := values.NewNamedTypeConstraint("point")

	// Implements TypeConstraint
	var _ values.TypeConstraint = tc

	c.Assert(tc.Name(), qt.Equals, "point")
	c.Assert(tc.Description(), qt.Equals, "point")

	// Check always fails — documentation only
	_, ok, err := tc.Check(values.NewInteger(42))
	c.Assert(ok, qt.IsFalse)
	c.Assert(err, qt.IsNotNil)
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestNamedTypeConstraint ./values/...`
Expected: FAIL — `NewNamedTypeConstraint` is not defined.

**Step 3: Write minimal implementation**

In `values/value_type.go`:

```go
// NamedTypeConstraint represents an unresolved type name from a docstring.
// It preserves the name for documentation display but cannot validate values.
// Use NewNamedTypeConstraint to create instances.
type NamedTypeConstraint struct {
	name string
}

// NewNamedTypeConstraint creates a TypeConstraint for an unresolved type name.
func NewNamedTypeConstraint(name string) *NamedTypeConstraint {
	return &NamedTypeConstraint{name: name}
}

// Name returns the unresolved type name.
func (p *NamedTypeConstraint) Name() string {
	return p.name
}

// Description returns the unresolved type name.
func (p *NamedTypeConstraint) Description() string {
	return p.name
}

// Check always fails — NamedTypeConstraint is documentation-only.
func (p *NamedTypeConstraint) Check(v Value) (any, bool, error) {
	return nil, false, werr.WrapForeignErrorf(werr.ErrInvalidArgument,
		"unresolved type constraint %q", p.name)
}
```

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestNamedTypeConstraint ./values/...`
Expected: PASS

---

### Task 3: Add RecordTypeConstraint and Parent Pointer to RecordType

**Files:**
- Modify: `values/record_type.go`
- Modify: `values/value_type.go`
- Test: `values/value_type_test.go`
- Test: `values/record_test.go`

**Step 1: Write the failing tests**

Add to `values/record_test.go`:

```go
func TestRecordTypeParent(t *testing.T) {
	c := qt.New(t)
	base := values.NewRecordType(values.NewSymbol("point"),
		[]*values.Symbol{values.NewSymbol("x"), values.NewSymbol("y")})
	child := values.NewDerivedRecordType(values.NewSymbol("point3d"), base,
		[]*values.Symbol{values.NewSymbol("z")})

	c.Assert(base.Parent(), qt.IsNil)
	c.Assert(child.Parent(), qt.Equals, base)
	c.Assert(child.FieldCount(), qt.Equals, 1) // only new fields
	c.Assert(child.FieldNames()[0].Key, qt.Equals, "z")
}
```

Add to `values/value_type_test.go`:

```go
func TestRecordTypeConstraint(t *testing.T) {
	c := qt.New(t)
	pointRTD := values.NewRecordType(values.NewSymbol("point"),
		[]*values.Symbol{values.NewSymbol("x"), values.NewSymbol("y")})
	point3dRTD := values.NewDerivedRecordType(values.NewSymbol("point3d"), pointRTD,
		[]*values.Symbol{values.NewSymbol("z")})

	tc := values.NewRecordTypeConstraint(pointRTD)
	var _ values.TypeConstraint = tc

	c.Assert(tc.Name(), qt.Equals, "point")
	c.Assert(tc.Description(), qt.Equals, "point record")

	// Direct match
	pointRec := values.NewRecord(pointRTD, []values.Value{values.NewInteger(1), values.NewInteger(2)})
	narrowed, ok, err := tc.Check(pointRec)
	c.Assert(err, qt.IsNil)
	c.Assert(ok, qt.IsTrue)
	c.Assert(narrowed.(*values.Record), qt.Equals, pointRec)

	// Subtype match — point3d satisfies point constraint
	point3dRec := values.NewRecord(point3dRTD, []values.Value{values.NewInteger(3)})
	narrowed, ok, err = tc.Check(point3dRec)
	c.Assert(err, qt.IsNil)
	c.Assert(ok, qt.IsTrue)
	c.Assert(narrowed.(*values.Record), qt.Equals, point3dRec)

	// Non-record fails
	_, ok, err = tc.Check(values.NewInteger(42))
	c.Assert(ok, qt.IsFalse)
	c.Assert(err, qt.IsNotNil)

	// Wrong record type fails
	otherRTD := values.NewRecordType(values.NewSymbol("color"),
		[]*values.Symbol{values.NewSymbol("r"), values.NewSymbol("g"), values.NewSymbol("b")})
	otherRec := values.NewRecord(otherRTD, []values.Value{values.NewInteger(255), values.NewInteger(0), values.NewInteger(0)})
	_, ok, err = tc.Check(otherRec)
	c.Assert(ok, qt.IsFalse)
	c.Assert(err, qt.IsNotNil)
}
```

**Step 2: Run tests to verify they fail**

Run: `go test -v -run 'TestRecordTypeParent|TestRecordTypeConstraint' ./values/...`
Expected: FAIL — `NewDerivedRecordType`, `Parent`, `NewRecordTypeConstraint` not defined.

**Step 3: Write implementations**

In `values/record_type.go`, add parent field:

```go
type RecordType struct {
	name       *Symbol
	fieldNames []*Symbol
	parent     *RecordType
}

// Parent returns the parent record type, or nil for base types.
func (p *RecordType) Parent() *RecordType {
	return p.parent
}

// NewDerivedRecordType creates a record type that inherits from parent.
// fieldNames contains only the new fields declared by this type.
func NewDerivedRecordType(name *Symbol, parent *RecordType, fieldNames []*Symbol) *RecordType {
	return &RecordType{
		name:       name,
		parent:     parent,
		fieldNames: fieldNames,
	}
}
```

In `values/value_type.go`, add RecordTypeConstraint:

```go
// RecordTypeConstraint validates that a value is a Record of a specific type
// (or a subtype via parent-chain walk).
type RecordTypeConstraint struct {
	rtd *RecordType
}

// NewRecordTypeConstraint creates a TypeConstraint for a specific record type.
func NewRecordTypeConstraint(rtd *RecordType) *RecordTypeConstraint {
	return &RecordTypeConstraint{rtd: rtd}
}

// Name returns the record type's name.
func (p *RecordTypeConstraint) Name() string {
	return p.rtd.Name().Key
}

// Description returns a human-readable description.
func (p *RecordTypeConstraint) Description() string {
	return p.rtd.Name().Key + " record"
}

// Check tests whether v is a Record of this type or a subtype.
func (p *RecordTypeConstraint) Check(v Value) (any, bool, error) {
	rec, ok := v.(*Record)
	if !ok {
		return nil, false, werr.WrapForeignErrorf(werr.ErrInvalidArgument,
			"expected %s record, got %s", p.rtd.Name().Key, SchemeTypeName(v))
	}
	for rt := rec.RecordType(); rt != nil; rt = rt.Parent() {
		if rt == p.rtd {
			return rec, true, nil
		}
	}
	return nil, false, werr.WrapForeignErrorf(werr.ErrInvalidArgument,
		"expected %s record, got %s record",
		p.rtd.Name().Key, rec.RecordType().Name().Key)
}
```

**Step 4: Run tests to verify they pass**

Run: `go test -v -run 'TestRecordTypeParent|TestRecordTypeConstraint' ./values/...`
Expected: PASS

**Step 5: Run full values test suite**

Run: `go test -v ./values/...`
Expected: PASS

---

### Task 4: Migrate docparse to TypeConstraint

**Files:**
- Modify: `docparse/docparse.go`
- Modify: `docparse/docparse_test.go`

**Step 1: Update DocInfo and ParseValueType**

In `docparse/docparse.go`:

1. Change `DocInfo.ParamTypes` from `[]values.ValueType` to `[]values.TypeConstraint`.
2. Change `DocInfo.ReturnType` from `values.ValueType` to `values.TypeConstraint`.
3. Change `ParseValueType` return type from `values.ValueType` to `values.TypeConstraint`.
4. Unknown names return `values.NewNamedTypeConstraint(name)` instead of `values.TypeAny`.
5. `HasStructuredMetadata`: change `p.ReturnType != values.TypeAny` to `p.ReturnType != nil`.

The `ParseDocstring` function sets `ReturnType` via `ParseValueType(val)` for known types. For the unspecified case (no `Returns:` line), `ReturnType` stays `nil` (zero value of interface).

**Step 2: Update tests**

In `docparse/docparse_test.go`:

1. Change `expected values.ValueType` to `expected values.TypeConstraint` in `TestParseValueType`.
2. Change `wantTypes []values.ValueType` to `wantTypes []values.TypeConstraint` and `wantReturn values.ValueType` to `wantReturn values.TypeConstraint` in `TestParseDocstring`.
3. The "unknown type" and "empty string" test cases in `TestParseValueType` now expect a `*values.NamedTypeConstraint` instead of `values.TypeAny`. Update assertions to check `tc.Name()` since `NamedTypeConstraint` is a pointer type (not comparable with `qt.Equals` to a `ValueType`).
4. Test cases where `wantReturn` was `values.TypeAny` as a placeholder for "not specified" now use `nil`.
5. The test case `"unknown param type becomes TypeAny"` changes name to `"unknown param type becomes NamedTypeConstraint"` and the assertion checks `tc.Name() == "frobnicate"`.

**Important change in assertion strategy**: `qt.Equals` works for `ValueType` (comparable) but `TypeConstraint` is an interface. For built-in types, `qt.Equals` still works (comparing `ValueType` values). For `NamedTypeConstraint`, assert on `.Name()` instead. For `nil` return types, use `qt.IsNil`.

**Step 3: Run tests**

Run: `go test -v ./docparse/...`
Expected: PASS

---

### Task 5: Migrate registry.PrimitiveSpec to TypeConstraint

**Files:**
- Modify: `registry/registry.go`
- Modify: `registry/registry_test.go`

**Step 1: Update PrimitiveSpec**

In `registry/registry.go`:

1. Change `ParamTypes []values.ValueType` to `ParamTypes []values.TypeConstraint`.
2. Change `ReturnType values.ValueType` to `ReturnType values.TypeConstraint`.
3. Update `validateParamTypes` — no logic changes needed, just the type in the function signature is already compatible (it accesses `len(spec.ParamTypes)` which works for both).

**Step 2: Update tests**

In `registry/registry_test.go`:

Change `ParamTypes: []values.ValueType{...}` to `ParamTypes: []values.TypeConstraint{...}`.
Change `ReturnType: values.TypeFoo` — this still compiles because `ValueType` implements `TypeConstraint`.

Update assertions: where tests compare `ParamTypes[i]` with `qt.Equals`, these still work because the underlying values are `ValueType` constants.

**Step 3: Run tests**

Run: `go test -v ./registry/...`
Expected: FAIL — registry/core tests still use old literal types. That's expected; they're fixed in Task 6.

Run just registry (not core): `go test -v -run . ./registry/ -count=1`
Expected: PASS (only registry_test.go, not subdirectories).

---

### Task 6: Mechanical Migration of registry/core/*.go

**Files:**
- Modify: all 20 files in `registry/core/` that reference `[]values.ValueType`

**Step 1: Sed-style replacement**

In every file listed by `grep -l 'values\.ValueType' registry/core/*.go`, change:
- `[]values.ValueType{` → `[]values.TypeConstraint{`

The values inside the braces (`values.TypeInteger`, `values.TypeBoolean`, etc.) remain unchanged — `ValueType` implements `TypeConstraint`.

Also change any `ReturnType: values.TypeFoo` lines — these compile as-is because `ValueType` satisfies `TypeConstraint`, but verify the implicit conversion works for the `ReturnType TypeConstraint` field.

Files to change (20):
- `arithmetic.go`, `boxes.go`, `byte_vectors.go`, `characters.go`, `cont_marks.go`
- `control.go`, `equality.go`, `exceptions.go`, `hashtables.go`, `lists.go`
- `opaque.go`, `pairs.go`, `parameters.go`, `predicates.go`, `prompts.go`
- `reflection.go`, `strings.go`, `syntax.go`, `syntax_loc.go`, `vectors.go`

**Step 2: Run full test suite**

Run: `go test ./registry/...`
Expected: PASS

---

### Task 7: Migrate repl DocInfo and Rendering

**Files:**
- Modify: `repl/doc_provider.go`
- Modify: `repl/meta.go`
- Modify: `repl/registry_doc_provider.go`
- Modify: `repl/registry_doc_provider_test.go`
- Modify: `repl/meta_test.go`

**Step 1: Update DocInfo**

In `repl/doc_provider.go`:
1. Change `ParamTypes []values.ValueType` to `ParamTypes []values.TypeConstraint`.
2. Change `ReturnType values.ValueType` to `ReturnType values.TypeConstraint`.

**Step 2: Update meta.go rendering**

In `repl/meta.go`:

1. `paramTypeForDoc` — change return type from `values.ValueType` to `values.TypeConstraint`. Change the fallback from `return values.TypeAny` to `return nil`.

2. Line 418: `if info.ReturnType != values.TypeAny` → `if info.ReturnType != nil`.
   Line 419: `info.ReturnType.String()` → `info.ReturnType.Name()`.

3. Line 442: `vt.String()` → `vt.Name()` (inside the parameter type rendering).

4. Line 448: same pattern as 418 — check nil, use `.Name()`.

5. `hasTypes` check (around line 439): `len(info.ParamTypes) > 0` — unchanged, still works.

**Step 3: Update registry_doc_provider.go**

In `repl/registry_doc_provider.go`:
No code changes needed — it copies `ParamTypes` and `ReturnType` from `PrimitiveSpec` to `DocInfo`, and both now use `TypeConstraint`. The assignment is compatible.

**Step 4: Update tests**

In `repl/meta_test.go`:
- Change `ParamTypes: []values.ValueType{...}` to `ParamTypes: []values.TypeConstraint{...}`.
- Change `ReturnType: values.TypeFoo` — compiles because ValueType satisfies TypeConstraint.

In `repl/registry_doc_provider_test.go`:
- Same literal type changes.
- Assertions `c.Assert(info.ParamTypes[0], qt.Equals, values.TypeString)` still work — the underlying value is a `ValueType` constant.
- Assertion `c.Assert(info.ReturnType, qt.Equals, values.TypeCharacter)` still works for the same reason.

**Step 5: Run tests**

Run: `go test -v ./repl/...`
Expected: PASS

---

### Task 8: Final Validation

**Step 1: Run full test suite**

Run: `go test ./...`
Expected: PASS — all packages.

**Step 2: Run linter**

Run: `make lint`
Expected: PASS — no import issues, no formatting issues.

**Step 3: Run coverage check**

Run: `make covercheck`
Expected: PASS.

**Step 4: Smoke test documentation rendering**

Build and test interactively:
```bash
make build
./dist/wile -e '(doc +)'
./dist/wile -e '(doc car)'
```
Verify parameter types still display correctly.
