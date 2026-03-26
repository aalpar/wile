# Extension API Contract System — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add type contracts to PrimitiveSpec that declare parameter and return types for foreign functions, enabling richer `,doc` output, MCP tool introspection, and future runtime enforcement.

**Architecture:** `ValueType` enum in `values/` (avoids import cycle: `machine/` → `values/` ← `registry/`). Two new optional fields on `PrimitiveSpec`. Documentation plumbing through `DocInfo` → `formatPrimitiveDoc`. Enforcement deferred to Phase 4. See `plans/2026-03-26-extension-contracts-design.md`.

**Tech Stack:** Go 1.24, quicktest (`qt`), table-driven tests, `values/`, `registry/`, `internal/repl/`

---

## Phase 1: Infrastructure

### Task 1: ValueType Enum — Constants and String()

**Files:**
- Create: `values/value_type.go`
- Test: `values/value_type_test.go`

**Step 1: Write the failing test**

```go
package values

import (
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestValueType_String(t *testing.T) {
	tcs := []struct {
		name string
		vt   ValueType
		want string
	}{
		{"any", TypeAny, "any"},
		{"void", TypeVoid, "void"},
		{"boolean", TypeBoolean, "boolean"},
		{"number", TypeNumber, "number"},
		{"complex", TypeComplex, "complex"},
		{"real", TypeReal, "real"},
		{"rational", TypeRational, "rational"},
		{"integer", TypeInteger, "integer"},
		{"exact-integer", TypeExactInteger, "exact integer"},
		{"flonum", TypeFlonum, "flonum"},
		{"string", TypeString, "string"},
		{"character", TypeCharacter, "character"},
		{"symbol", TypeSymbol, "symbol"},
		{"byte", TypeByte, "byte"},
		{"pair", TypePair, "pair"},
		{"list", TypeList, "list"},
		{"vector", TypeVector, "vector"},
		{"bytevector", TypeByteVector, "bytevector"},
		{"hashtable", TypeHashtable, "hashtable"},
		{"procedure", TypeProcedure, "procedure"},
		{"port", TypePort, "port"},
		{"input-port", TypeInputPort, "input port"},
		{"output-port", TypeOutputPort, "output port"},
		{"textual-input-port", TypeTextualInputPort, "textual input port"},
		{"textual-output-port", TypeTextualOutputPort, "textual output port"},
		{"binary-input-port", TypeBinaryInputPort, "binary input port"},
		{"binary-output-port", TypeBinaryOutputPort, "binary output port"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			c.Assert(tc.vt.String(), qt.Equals, tc.want)
		})
	}
}

func TestValueType_StringUnknown(t *testing.T) {
	c := qt.New(t)
	// Out-of-range ValueType should return "unknown"
	c.Assert(ValueType(255).String(), qt.Equals, "unknown")
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestValueType_String ./values/...`
Expected: FAIL — `ValueType` undefined

**Step 3: Write minimal implementation**

Create `values/value_type.go`:

```go
package values

// ValueType identifies a type contract for primitive parameters and return values.
// Each constant is a named predicate — a scalar that identifies a type-checking
// concept, not a 1:1 mapping to Go types or Scheme types.
type ValueType uint8

const (
	TypeAny          ValueType = iota // no constraint
	TypeVoid                          // void

	// Booleans
	TypeBoolean // *Boolean

	// Numeric tower
	TypeNumber       // Number interface (any numeric)
	TypeComplex      // ComplexNumber interface
	TypeReal         // RealNumber interface
	TypeRational     // *Rational
	TypeInteger      // exact integer: *Integer | *BigInteger
	TypeExactInteger // alias for TypeInteger (clarity)
	TypeFlonum       // inexact real: *Float | *BigFloat

	// Text
	TypeString    // *String
	TypeCharacter // *Character
	TypeSymbol    // *Symbol
	TypeByte      // *Byte

	// Collections
	TypePair      // *Pair
	TypeList      // Tuple (pair or empty list)
	TypeVector    // *Vector
	TypeByteVector // *ByteVector
	TypeHashtable // *Hashtable

	// Procedures
	TypeProcedure // Callable interface

	// Ports
	TypePort              // Port interface
	TypeInputPort         // InputPort interface
	TypeOutputPort        // OutputPort interface
	TypeTextualInputPort  // TextualReader interface
	TypeTextualOutputPort // TextualWriter interface
	TypeBinaryInputPort   // BinaryReader interface
	TypeBinaryOutputPort  // BinaryWriter interface

	typeCount // must be last — used to size lookup tables
)

var valueTypeNames = [typeCount]string{
	TypeAny:               "any",
	TypeVoid:              "void",
	TypeBoolean:           "boolean",
	TypeNumber:            "number",
	TypeComplex:           "complex",
	TypeReal:              "real",
	TypeRational:          "rational",
	TypeInteger:           "integer",
	TypeExactInteger:      "exact integer",
	TypeFlonum:            "flonum",
	TypeString:            "string",
	TypeCharacter:         "character",
	TypeSymbol:            "symbol",
	TypeByte:              "byte",
	TypePair:              "pair",
	TypeList:              "list",
	TypeVector:            "vector",
	TypeByteVector:        "bytevector",
	TypeHashtable:         "hashtable",
	TypeProcedure:         "procedure",
	TypePort:              "port",
	TypeInputPort:         "input port",
	TypeOutputPort:        "output port",
	TypeTextualInputPort:  "textual input port",
	TypeTextualOutputPort: "textual output port",
	TypeBinaryInputPort:   "binary input port",
	TypeBinaryOutputPort:  "binary output port",
}

// String returns the Scheme-facing name for this type (e.g., "string", "exact integer").
func (vt ValueType) String() string {
	if vt >= typeCount {
		return "unknown"
	}
	return valueTypeNames[vt]
}
```

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestValueType_String ./values/...`
Expected: PASS

**Step 5: Commit**

```
feat(values): add ValueType enum with String() method

Part of extension API contract system. ValueType is a named predicate
identifying type contracts for primitive parameters and return values.
```

---

### Task 2: ValueType.Description()

**Files:**
- Modify: `values/value_type.go`
- Modify: `values/value_type_test.go`

**Step 1: Write the failing test**

Add to `values/value_type_test.go`:

```go
func TestValueType_Description(t *testing.T) {
	c := qt.New(t)
	// Spot-check a few descriptions
	c.Assert(TypeAny.Description(), qt.Not(qt.Equals), "")
	c.Assert(TypeInteger.Description(), qt.Matches, ".*exact integer.*")
	c.Assert(TypeNumber.Description(), qt.Matches, ".*numeric.*")
	c.Assert(TypeList.Description(), qt.Matches, ".*proper list.*|.*pair or empty list.*")

	// Unknown type
	c.Assert(ValueType(255).Description(), qt.Equals, "unknown type")
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestValueType_Description ./values/...`
Expected: FAIL — `Description` undefined

**Step 3: Write minimal implementation**

Add to `values/value_type.go`:

```go
var valueTypeDescriptions = [typeCount]string{
	TypeAny:               "Any value, no type constraint",
	TypeVoid:              "The void value (unspecified result)",
	TypeBoolean:           "A boolean (#t or #f)",
	TypeNumber:            "Any numeric value",
	TypeComplex:           "A complex number",
	TypeReal:              "A real number",
	TypeRational:          "An exact rational number",
	TypeInteger:           "An exact integer (fixnum or bignum)",
	TypeExactInteger:      "An exact integer (fixnum or bignum)",
	TypeFlonum:            "An inexact real number (flonum)",
	TypeString:            "A string",
	TypeCharacter:         "A character",
	TypeSymbol:            "A symbol",
	TypeByte:              "A byte (exact integer 0-255)",
	TypePair:              "A pair (cons cell)",
	TypeList:              "A proper list (pair or empty list)",
	TypeVector:            "A vector",
	TypeByteVector:        "A bytevector",
	TypeHashtable:         "A hashtable",
	TypeProcedure:         "A procedure",
	TypePort:              "A port",
	TypeInputPort:         "An input port",
	TypeOutputPort:        "An output port",
	TypeTextualInputPort:  "A textual input port",
	TypeTextualOutputPort: "A textual output port",
	TypeBinaryInputPort:   "A binary input port",
	TypeBinaryOutputPort:  "A binary output port",
}

// Description returns a human-readable description for REPL documentation.
func (vt ValueType) Description() string {
	if vt >= typeCount {
		return "unknown type"
	}
	return valueTypeDescriptions[vt]
}
```

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestValueType_Description ./values/...`
Expected: PASS

**Step 5: Commit**

```
feat(values): add ValueType.Description() for REPL help text
```

---

### Task 3: ValueType.Check()

**Files:**
- Modify: `values/value_type.go`
- Modify: `values/value_type_test.go`

**Step 1: Write the failing test**

Add to `values/value_type_test.go`:

```go
func TestValueType_Check(t *testing.T) {
	str := NewString("hello")
	integer := NewInteger(42)
	char := NewCharacter('x')
	sym := NewSymbol("foo")
	boolT := TrueValue
	vec := NewVector(nil)
	bvec := NewByteVector(nil)
	pair := NewPair(integer, NewEmptyList())

	tcs := []struct {
		name    string
		vt      ValueType
		val     Value
		wantOK  bool
	}{
		// TypeAny matches everything
		{"any/string", TypeAny, str, true},
		{"any/integer", TypeAny, integer, true},

		// TypeString
		{"string/string", TypeString, str, true},
		{"string/integer", TypeString, integer, false},

		// TypeInteger matches *Integer
		{"integer/integer", TypeInteger, integer, true},
		{"integer/string", TypeInteger, str, false},

		// TypeNumber matches any numeric
		{"number/integer", TypeNumber, integer, true},
		{"number/string", TypeNumber, str, false},

		// TypeBoolean
		{"boolean/true", TypeBoolean, boolT, true},
		{"boolean/string", TypeBoolean, str, false},

		// TypeCharacter
		{"char/char", TypeCharacter, char, true},
		{"char/string", TypeCharacter, str, false},

		// TypeSymbol
		{"symbol/symbol", TypeSymbol, sym, true},
		{"symbol/string", TypeSymbol, str, false},

		// TypeVector
		{"vector/vector", TypeVector, vec, true},
		{"vector/string", TypeVector, str, false},

		// TypeByteVector
		{"bytevector/bytevector", TypeByteVector, bvec, true},
		{"bytevector/string", TypeByteVector, str, false},

		// TypePair
		{"pair/pair", TypePair, pair, true},
		{"pair/string", TypePair, str, false},

		// TypeList matches Tuple (pair or empty list)
		{"list/pair", TypeList, pair, true},
		{"list/empty", TypeList, NewEmptyList(), true},
		{"list/string", TypeList, str, false},

		// TypeProcedure
		{"procedure/string", TypeProcedure, str, false},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			narrowed, ok, err := tc.vt.Check(tc.val)
			c.Assert(ok, qt.Equals, tc.wantOK)
			if tc.wantOK {
				c.Assert(err, qt.IsNil)
				c.Assert(narrowed, qt.IsNotNil)
			} else {
				if tc.vt != TypeAny {
					c.Assert(err, qt.IsNotNil)
				}
			}
		})
	}
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestValueType_Check ./values/...`
Expected: FAIL — `Check` undefined

**Step 3: Write minimal implementation**

Add to `values/value_type.go`:

```go
import "fmt"

// checkFunc is the signature for a type predicate that narrows a Value.
// Returns the narrowed value (at the contract's interface level), whether
// the value matched, and a descriptive error on mismatch.
type checkFunc func(Value) (any, bool, error)

var valueTypeChecks [typeCount]checkFunc

func init() {
	valueTypeChecks[TypeAny] = func(v Value) (any, bool, error) {
		return v, true, nil
	}
	valueTypeChecks[TypeVoid] = makeCheck[*voidType]("void")
	valueTypeChecks[TypeBoolean] = makeCheck[*Boolean]("boolean")

	// Numeric tower — interface checks
	valueTypeChecks[TypeNumber] = makeInterfaceCheck[Number]("number")
	valueTypeChecks[TypeComplex] = makeInterfaceCheck[ComplexNumber]("complex")
	valueTypeChecks[TypeReal] = makeInterfaceCheck[RealNumber]("real")
	valueTypeChecks[TypeRational] = makeCheck[*Rational]("rational")
	valueTypeChecks[TypeInteger] = makeExactIntegerCheck()
	valueTypeChecks[TypeExactInteger] = makeExactIntegerCheck()
	valueTypeChecks[TypeFlonum] = makeFlonumCheck()

	// Text
	valueTypeChecks[TypeString] = makeCheck[*String]("string")
	valueTypeChecks[TypeCharacter] = makeCheck[*Character]("character")
	valueTypeChecks[TypeSymbol] = makeCheck[*Symbol]("symbol")
	valueTypeChecks[TypeByte] = makeCheck[*Byte]("byte")

	// Collections
	valueTypeChecks[TypePair] = makeCheck[*Pair]("pair")
	valueTypeChecks[TypeList] = makeInterfaceCheck[Tuple]("list")
	valueTypeChecks[TypeVector] = makeCheck[*Vector]("vector")
	valueTypeChecks[TypeByteVector] = makeCheck[*ByteVector]("bytevector")
	valueTypeChecks[TypeHashtable] = makeCheck[*Hashtable]("hashtable")

	// Procedures
	valueTypeChecks[TypeProcedure] = makeInterfaceCheck[Callable]("procedure")

	// Ports
	valueTypeChecks[TypePort] = makeInterfaceCheck[Port]("port")
	valueTypeChecks[TypeInputPort] = makeInterfaceCheck[InputPort]("input port")
	valueTypeChecks[TypeOutputPort] = makeInterfaceCheck[OutputPort]("output port")
	valueTypeChecks[TypeTextualInputPort] = makeInterfaceCheck[TextualReader]("textual input port")
	valueTypeChecks[TypeTextualOutputPort] = makeInterfaceCheck[TextualWriter]("textual output port")
	valueTypeChecks[TypeBinaryInputPort] = makeInterfaceCheck[BinaryReader]("binary input port")
	valueTypeChecks[TypeBinaryOutputPort] = makeInterfaceCheck[BinaryWriter]("binary output port")
}

// makeCheck creates a check function for a concrete pointer type.
func makeCheck[T any](typeName string) checkFunc {
	return func(v Value) (any, bool, error) {
		result, ok := v.(T)
		if !ok {
			return nil, false, fmt.Errorf("expected %s, got %T", typeName, v)
		}
		return result, true, nil
	}
}

// makeInterfaceCheck creates a check function for an interface type.
func makeInterfaceCheck[T any](typeName string) checkFunc {
	return func(v Value) (any, bool, error) {
		result, ok := v.(T)
		if !ok {
			return nil, false, fmt.Errorf("expected %s, got %T", typeName, v)
		}
		return result, true, nil
	}
}

// makeExactIntegerCheck handles *Integer | *BigInteger.
func makeExactIntegerCheck() checkFunc {
	return func(v Value) (any, bool, error) {
		switch v := v.(type) {
		case *Integer:
			return v, true, nil
		case *BigInteger:
			return v, true, nil
		default:
			return nil, false, fmt.Errorf("expected exact integer, got %T", v)
		}
	}
}

// makeFlonumCheck handles *Float | *BigFloat.
func makeFlonumCheck() checkFunc {
	return func(v Value) (any, bool, error) {
		switch v := v.(type) {
		case *Float:
			return v, true, nil
		case *BigFloat:
			return v, true, nil
		default:
			return nil, false, fmt.Errorf("expected flonum, got %T", v)
		}
	}
}

// Check validates v against this type contract.
//
// Returns:
//   - narrowed: the value narrowed to the contract's interface level.
//   - ok: whether the value matched the predicate.
//   - err: on failure, a descriptive error. On success, nil.
func (vt ValueType) Check(v Value) (any, bool, error) {
	if vt >= typeCount {
		return nil, false, fmt.Errorf("unknown ValueType %d", vt)
	}
	fn := valueTypeChecks[vt]
	if fn == nil {
		return v, true, nil
	}
	return fn(v)
}
```

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestValueType_Check ./values/...`
Expected: PASS

**Step 5: Run full values package tests**

Run: `go test -v ./values/...`
Expected: PASS — no regressions

**Step 6: Commit**

```
feat(values): add ValueType.Check() type predicate with narrowing

Each ValueType constant carries a check function that validates a Value
and returns the narrowed result at the contract's interface level.
Handles concrete types, interface types, and multi-type predicates
(exact integer = *Integer | *BigInteger, flonum = *Float | *BigFloat).
```

---

### Task 4: PrimitiveSpec Field Additions

**Files:**
- Modify: `registry/registry.go` (lines 24-33)
- Modify: `registry/registry_test.go`

**Step 1: Write the failing test**

Add to `registry/registry_test.go`:

```go
func TestRegistry_PrimitiveSpecWithContract(t *testing.T) {
	c := qt.New(t)

	r := NewRegistry()
	spec := PrimitiveSpec{
		Name:       "test-contracted",
		ParamCount: 2,
		Impl:       func(_ *machine.MachineContext) error { return nil },
		ParamTypes: []values.ValueType{values.TypeString, values.TypeInteger},
		ReturnType: values.TypeCharacter,
		Doc:        "Test primitive with contract.",
		ParamNames: []string{"s", "k"},
		Category:   "test",
	}

	r.AddPrimitive(spec, PhaseRuntime)

	prims := r.Primitives()
	c.Assert(len(prims), qt.Equals, 1)
	c.Assert(prims[0].Spec.ParamTypes, qt.HasLen, 2)
	c.Assert(prims[0].Spec.ParamTypes[0], qt.Equals, values.TypeString)
	c.Assert(prims[0].Spec.ParamTypes[1], qt.Equals, values.TypeInteger)
	c.Assert(prims[0].Spec.ReturnType, qt.Equals, values.TypeCharacter)
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestRegistry_PrimitiveSpecWithContract ./registry/...`
Expected: FAIL — `ParamTypes` field unknown

**Step 3: Write minimal implementation**

In `registry/registry.go`, add two fields to `PrimitiveSpec` (after `Category`):

```go
type PrimitiveSpec struct {
	Name       string
	ParamCount int
	IsVariadic bool
	Impl       machine.ForeignFunction
	Doc        string           // optional: one-line description
	ParamNames []string         // optional: parameter names
	Category   string           // optional: grouping category
	ParamTypes []values.ValueType // optional: type contract per parameter
	ReturnType values.ValueType   // optional: return type (zero = TypeAny)
}
```

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestRegistry_PrimitiveSpecWithContract ./registry/...`
Expected: PASS

**Step 5: Run full registry tests**

Run: `go test -v ./registry/...`
Expected: PASS — existing tests unaffected (new fields have zero values)

**Step 6: Commit**

```
feat(registry): add ParamTypes and ReturnType to PrimitiveSpec

Optional fields for declaring type contracts on primitive parameters
and return values. Zero values (nil/TypeAny) preserve existing behavior.
```

---

### Task 5: DocInfo Type Updates

**Files:**
- Modify: `internal/repl/doc_provider.go` (lines 3-10)
- Modify: `internal/repl/registry_doc_provider.go` (lines 20-32)
- Modify: `internal/repl/registry_doc_provider_test.go`

**Step 1: Write the failing test**

Add to `internal/repl/registry_doc_provider_test.go`. First, read the existing test
to understand the setup pattern, then add a test that checks ParamTypes/ReturnType
are passed through.

```go
func TestRegistryDocProvider_ContractFields(t *testing.T) {
	c := qt.New(t)

	reg := registry.NewRegistry()
	reg.AddPrimitive(registry.PrimitiveSpec{
		Name:       "test-contracted",
		ParamCount: 2,
		Impl:       func(_ *machine.MachineContext) error { return nil },
		Doc:        "A test.",
		ParamNames: []string{"s", "k"},
		Category:   "test",
		ParamTypes: []values.ValueType{values.TypeString, values.TypeInteger},
		ReturnType: values.TypeCharacter,
	}, registry.PhaseRuntime)

	prov := NewRegistryDocProvider(reg)
	info, found := prov.LookupDoc("test-contracted")
	c.Assert(found, qt.IsTrue)
	c.Assert(info.ParamTypes, qt.HasLen, 2)
	c.Assert(info.ParamTypes[0], qt.Equals, values.TypeString)
	c.Assert(info.ParamTypes[1], qt.Equals, values.TypeInteger)
	c.Assert(info.ReturnType, qt.Equals, values.TypeCharacter)
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestRegistryDocProvider_ContractFields ./internal/repl/...`
Expected: FAIL — `ParamTypes` field unknown on DocInfo

**Step 3: Write minimal implementation**

In `internal/repl/doc_provider.go`, add fields to `DocInfo`:

```go
type DocInfo struct {
	Doc        string
	ParamNames []string
	Category   string
	ParamCount int
	IsVariadic bool
	ParamTypes []values.ValueType // type contract per parameter
	ReturnType values.ValueType   // return type (zero = TypeAny)
}
```

In `internal/repl/registry_doc_provider.go`, update `LookupDoc` to pass through:

```go
func (p *RegistryDocProvider) LookupDoc(name string) (DocInfo, bool) {
	pr, found := p.reg.FindPrimitive(name, 0)
	if !found {
		return DocInfo{}, false
	}
	return DocInfo{
		Doc:        pr.Spec.Doc,
		ParamNames: pr.Spec.ParamNames,
		Category:   pr.Spec.Category,
		ParamCount: pr.Spec.ParamCount,
		IsVariadic: pr.Spec.IsVariadic,
		ParamTypes: pr.Spec.ParamTypes,
		ReturnType: pr.Spec.ReturnType,
	}, true
}
```

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestRegistryDocProvider_ContractFields ./internal/repl/...`
Expected: PASS

**Step 5: Run full repl tests**

Run: `go test -v ./internal/repl/...`
Expected: PASS

**Step 6: Commit**

```
feat(repl): add ParamTypes and ReturnType to DocInfo

Passes type contract fields through from PrimitiveSpec to DocProvider,
making them available to the ,doc command formatter.
```

---

### Task 6: formatPrimitiveDoc with Type Annotations

**Files:**
- Modify: `internal/repl/meta.go` (lines 221-238)
- Modify: `internal/repl/meta_test.go`

**Step 1: Write the failing test**

Add to `internal/repl/meta_test.go`:

```go
func TestFormatPrimitiveDoc_WithTypes(t *testing.T) {
	c := qt.New(t)

	var buf strings.Builder
	info := DocInfo{
		Doc:        "Returns the kth character of string.",
		ParamNames: []string{"string", "k"},
		Category:   "strings",
		ParamCount: 2,
		ParamTypes: []values.ValueType{values.TypeString, values.TypeExactInteger},
		ReturnType: values.TypeCharacter,
	}
	formatPrimitiveDoc(&buf, "string-ref", info)
	output := buf.String()

	// Should contain typed signature with return type
	c.Assert(strings.Contains(output, "→ character") || strings.Contains(output, "-> character"),
		qt.IsTrue, qt.Commentf("output: %s", output))
	// Should contain parameter type annotations
	c.Assert(strings.Contains(output, "string : string"), qt.IsTrue,
		qt.Commentf("output: %s", output))
	c.Assert(strings.Contains(output, "k : exact integer"), qt.IsTrue,
		qt.Commentf("output: %s", output))
}

func TestFormatPrimitiveDoc_WithoutTypes(t *testing.T) {
	c := qt.New(t)

	var buf strings.Builder
	info := DocInfo{
		Doc:        "Returns the length of string.",
		ParamNames: []string{"string"},
		Category:   "strings",
		ParamCount: 1,
	}
	formatPrimitiveDoc(&buf, "string-length", info)
	output := buf.String()

	// Without ParamTypes, no type annotations or return type
	c.Assert(strings.Contains(output, " : "), qt.IsFalse,
		qt.Commentf("output: %s", output))
	c.Assert(strings.Contains(output, "→"), qt.IsFalse,
		qt.Commentf("output: %s", output))
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestFormatPrimitiveDoc ./internal/repl/...`
Expected: FAIL — no type annotations in output

**Step 3: Write minimal implementation**

Replace `formatPrimitiveDoc` in `internal/repl/meta.go`:

```go
func formatPrimitiveDoc(w *strings.Builder, name string, info DocInfo) {
	hasTypes := len(info.ParamTypes) > 0

	// Signature line
	fmt.Fprintf(w, "(%s", name)
	for _, pn := range info.ParamNames {
		fmt.Fprintf(w, " %s", pn)
	}
	if info.IsVariadic {
		fmt.Fprint(w, " ...")
	}
	fmt.Fprint(w, ")")

	// Return type on signature line
	if hasTypes && info.ReturnType != values.TypeAny {
		fmt.Fprintf(w, " → %s", info.ReturnType.String())
	}
	fmt.Fprintln(w)

	// Description
	if info.Doc != "" {
		fmt.Fprintf(w, "  %s\n", info.Doc)
	}

	// Parameter types
	if hasTypes && len(info.ParamNames) > 0 {
		fmt.Fprintln(w, "  Parameters:")
		for i, pn := range info.ParamNames {
			vt := paramTypeAt(info.ParamTypes, i)
			fmt.Fprintf(w, "    %s : %s\n", pn, vt.String())
		}
	}

	// Return type detail (if present)
	if hasTypes && info.ReturnType != values.TypeAny {
		fmt.Fprintf(w, "  Returns: %s\n", info.ReturnType.String())
	}

	// Category
	if info.Category != "" {
		fmt.Fprintf(w, "  Category: %s\n", info.Category)
	}
}

// paramTypeAt returns the ValueType for parameter index i.
// For variadic primitives, positions beyond len(types)-1 use the last entry.
func paramTypeAt(types []values.ValueType, i int) values.ValueType {
	if i < len(types) {
		return types[i]
	}
	if len(types) > 0 {
		return types[len(types)-1]
	}
	return values.TypeAny
}
```

**Note:** Add `"github.com/aalpar/wile/values"` to the import block in `meta.go`.

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestFormatPrimitiveDoc ./internal/repl/...`
Expected: PASS

**Step 5: Run full repl tests**

Run: `go test -v ./internal/repl/...`
Expected: PASS — existing tests unaffected (they don't set ParamTypes)

**Step 6: Commit**

```
feat(repl): show type annotations in ,doc output

When a primitive has ParamTypes, ,doc now shows parameter type annotations
and return type. Primitives without contracts show unchanged output.
```

---

### Task 7: Annotate String Primitives (Proof of Concept)

**Files:**
- Modify: `registry/core/strings.go`
- Read: `registry/core/prim_strings.go` (verify type expectations match)

**Step 1: Read the existing string primitive implementations**

Read `registry/core/prim_strings.go` to verify what types each primitive
actually checks. For example, verify `PrimStringRef` calls
`RequireArg[*values.String]` for param 0 and `RequireIndex` for param 1.

**Step 2: Add contracts to string primitives**

Update `registry/core/strings.go` registrations. Example entries:

```go
{Name: "string-length", ParamCount: 1, Impl: PrimStringLength,
    Doc: "Returns the length of string.",
    ParamNames: []string{"string"}, Category: "strings",
    ParamTypes: []values.ValueType{values.TypeString},
    ReturnType: values.TypeExactInteger},

{Name: "string-ref", ParamCount: 2, Impl: PrimStringRef,
    Doc: "Returns the kth character of string.",
    ParamNames: []string{"string", "k"}, Category: "strings",
    ParamTypes: []values.ValueType{values.TypeString, values.TypeExactInteger},
    ReturnType: values.TypeCharacter},

{Name: "string-set!", ParamCount: 3, Impl: PrimStringSet,
    Doc: "Stores char in element k of string.",
    ParamNames: []string{"string", "k", "char"}, Category: "strings",
    ParamTypes: []values.ValueType{values.TypeString, values.TypeExactInteger, values.TypeCharacter},
    ReturnType: values.TypeVoid},

{Name: "string-append", ParamCount: 1, IsVariadic: true, Impl: PrimStringAppend,
    Doc: "Returns a new string from concatenating its arguments.",
    ParamNames: []string{"string"}, Category: "strings",
    ParamTypes: []values.ValueType{values.TypeString},
    ReturnType: values.TypeString},

{Name: "substring", ParamCount: 3, Impl: PrimSubstring,
    Doc: "Returns a substring.",
    ParamNames: []string{"string", "start", "end"}, Category: "strings",
    ParamTypes: []values.ValueType{values.TypeString, values.TypeExactInteger, values.TypeExactInteger},
    ReturnType: values.TypeString},

{Name: "string-copy", ParamCount: 1, Impl: PrimStringCopy,
    Doc: "Returns a copy of string.",
    ParamNames: []string{"string"}, Category: "strings",
    ParamTypes: []values.ValueType{values.TypeString},
    ReturnType: values.TypeString},
```

**Important:** Read each `Prim*` implementation to verify the types match before
annotating. Do NOT guess — check the actual `RequireArg` calls.

**Step 3: Run tests**

Run: `go test -v ./registry/...`
Expected: PASS — adding metadata doesn't change behavior

**Step 4: Verify `,doc` output manually**

Run: `go run ./cmd/wile/`

At the REPL:
```
> ,doc string-ref
```

Expected output (approximately):
```
(string-ref string k) → character
  Returns the kth character of string.
  Parameters:
    string : string
    k : exact integer
  Returns: character
  Category: strings
```

**Step 5: Commit**

```
feat(core): add type contracts to string primitives

Proof-of-concept: annotate string primitives with ParamTypes and
ReturnType. Visible via ,doc in the REPL.
```

---

### Task 8: Lint and Final Verification

**Step 1: Run linter**

Run: `make lint`
Expected: PASS

**Step 2: Run full test suite**

Run: `make test`
Expected: PASS

**Step 3: Run cover check**

Run: `make covercheck`
Expected: PASS

---

## Phase 2: Core Primitive Contracts (Mechanical)

Add `ParamTypes` and `ReturnType` to all remaining `registry/core/` primitives (~172).
Work through one file at a time:

| File | Primitives | Notes |
|------|-----------|-------|
| `predicates.go` | ~19 | All: `(obj) → boolean` |
| `arithmetic.go` | ~21 | Numeric tower types; some variadic |
| `pairs.go` | ~5 | `car`/`cdr` return `TypeAny` |
| `lists.go` | ~13 | Mixed: some `TypeList`, some `TypeAny` |
| `vectors.go` | ~13 | Similar to strings pattern |
| `byte_vectors.go` | ~10 | Similar to vectors pattern |
| `characters.go` | ~3 | `TypeCharacter` param/return |
| `control.go` | ~7 | `TypeProcedure` params |
| `equality.go` | ~3 | `(obj obj) → boolean` |
| `strings.go` | remaining | Already done in Task 7 |
| `exceptions.go` | ~9 | Mixed types |
| `hashtables.go` | ~10 | `TypeHashtable` first param |
| `boxes.go` | ~4 | Custom opaque types → `TypeAny` |
| `parameters.go` | ~4 | `TypeProcedure` or `TypeAny` |
| `prompts.go` | ~7 | `TypeProcedure` params |
| `syntax.go` | ~6 | Expand-time, `TypeAny` |
| `syntax_loc.go` | ~6 | Expand-time, `TypeAny` |
| `opaque.go` | ~2 | `TypeAny` |
| `reflection.go` | ~5 | `TypeProcedure` first param |
| `cont_marks.go` | ~8 | Mixed |

**Process per file:**
1. Read the `Prim*` implementations to verify actual type expectations
2. Add `ParamTypes` and `ReturnType` to each spec
3. Run `go test -v ./registry/core/...`
4. Commit per file or per logical group

---

## Phase 3: Extension Primitive Contracts (Mechanical)

Same process for `extensions/` (~133 primitives):

| Package | Primitives |
|---------|-----------|
| `extensions/files/` | ~13 |
| `extensions/math/` | ~35 |
| `extensions/system/` | ~8 |
| `extensions/process/` | ~8 |
| `extensions/threads/` | ~30 |
| `extensions/gointerop/` | ~33 |
| `extensions/introspection/` | ~6 |

Plus `internal/extensions/{io,eval,namespace,all}/`.

---

## Phase 4: Runtime Enforcement (Separate PR)

**Prerequisite:** All primitives contracted (Phases 2-3 complete).

This phase requires its own implementation plan. Key design decisions:

1. **ForeignClosure change**: Add `validate func(*MachineContext) error` field
   and `SetValidator` method. `ValueType` is in `values/` so no import cycle.
2. **Registration path**: `registry/apply.go:registerRuntimePrimitive` and
   `registerExpandTimePrimitive` build a validator closure from `spec.ParamTypes`
   and set it on the `ForeignClosure` via the setter.
3. **Dispatch path**: `callForeignCached` (line 75) and `applyForeign` (line 124)
   call `fcls.validate(mc)` before `fcls.fn(mc)` when validator is non-nil.
4. **Engine option**: `WithContractEnforcement()` gates whether validators are
   installed during registration. Default: off initially.
5. **`engine.go:RegisterPrimitive`**: Same treatment for the public API path.

**ValidateArgs function** (in `registry/helpers/`):

```go
func BuildValidator(spec PrimitiveSpec) func(*machine.MachineContext) error {
    if len(spec.ParamTypes) == 0 {
        return nil
    }
    types := slices.Clone(spec.ParamTypes)
    name := spec.Name
    return func(mc *machine.MachineContext) error {
        for i := 0; i < mc.ArgCount(); i++ {
            vt := paramTypeAt(types, i)
            if vt == values.TypeAny {
                continue
            }
            _, ok, err := vt.Check(mc.Arg(i))
            if !ok {
                return werr.WrapForeignErrorf(err, "%s: argument %d", name, i)
            }
        }
        return nil
    }
}
```

This captures `types` and `name` in the closure. The closure is stored on
`ForeignClosure` and called from the dispatch path. No import of `registry/`
from `machine/` — the validator is a plain `func(*MachineContext) error`.
