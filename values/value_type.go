// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package values

import (
	"fmt"

	"github.com/aalpar/wile/werr"
)

// Compile-time interface assertions.
var (
	_ TypeConstraint = ValueType(0)
	_ TypeConstraint = (*NamedTypeConstraint)(nil)
	_ TypeConstraint = (*RecordTypeConstraint)(nil)
)

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

// ValueType represents a Scheme type constraint for extension API contracts.
// Each constant maps to either a concrete Go type or an interface in the
// values package.
type ValueType uint8

const (
	TypeAny               ValueType = iota // any Value
	TypeVoid                               // void singleton
	TypeBoolean                            // *Boolean
	TypeNumber                             // Number interface
	TypeComplex                            // ComplexNumber interface
	TypeReal                               // RealNumber interface
	TypeRational                           // *Rational
	TypeInteger                            // *Integer | *BigInteger (all Wile integers are exact)
	TypeFlonum                             // *Float | *BigFloat
	TypeString                             // *String
	TypeCharacter                          // *Character
	TypeSymbol                             // *Symbol
	TypeByte                               // *Byte
	TypePair                               // *Pair
	TypeList                               // Tuple interface
	TypeVector                             // *Vector
	TypeByteVector                         // *ByteVector
	TypeHashtable                          // *Hashtable
	TypeProcedure                          // Callable interface
	TypePort                               // Port interface
	TypeInputPort                          // InputPort interface
	TypeOutputPort                         // OutputPort interface
	TypeTextualInputPort                   // TextualReader interface
	TypeTextualOutputPort                  // TextualWriter interface
	TypeBinaryInputPort                    // BinaryReader interface
	TypeBinaryOutputPort                   // BinaryWriter interface
	TypeCount                              // sentinel — must be last
)

// typeNames maps each ValueType to its display name.
var typeNames = [TypeCount]string{
	TypeAny:               "any",
	TypeVoid:              "void",
	TypeBoolean:           "boolean",
	TypeNumber:            "number",
	TypeComplex:           "complex",
	TypeReal:              "real",
	TypeRational:          "rational",
	TypeInteger:           "integer",
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
	TypeInputPort:         "input-port",
	TypeOutputPort:        "output-port",
	TypeTextualInputPort:  "textual-input-port",
	TypeTextualOutputPort: "textual-output-port",
	TypeBinaryInputPort:   "binary-input-port",
	TypeBinaryOutputPort:  "binary-output-port",
}

// String returns the Scheme-style name for the type (e.g., "integer", "pair").
func (p ValueType) String() string {
	if p >= TypeCount {
		return "unknown"
	}
	return typeNames[p]
}

// Name returns the Scheme-facing type name, satisfying the TypeConstraint interface.
func (p ValueType) Name() string {
	return p.String()
}

// typeDescriptions maps each ValueType to a human-readable description.
var typeDescriptions = [TypeCount]string{
	TypeAny:               "any value",
	TypeVoid:              "void (no meaningful return value)",
	TypeBoolean:           "boolean (#t or #f)",
	TypeNumber:            "number (any numeric type)",
	TypeComplex:           "complex number",
	TypeReal:              "real number",
	TypeRational:          "exact rational number",
	TypeInteger:           "exact integer",
	TypeFlonum:            "inexact floating-point number",
	TypeString:            "string",
	TypeCharacter:         "character",
	TypeSymbol:            "symbol",
	TypeByte:              "exact integer in [0, 255]",
	TypePair:              "pair (cons cell)",
	TypeList:              "proper list (pair or empty list)",
	TypeVector:            "vector",
	TypeByteVector:        "bytevector",
	TypeHashtable:         "hash table",
	TypeProcedure:         "procedure",
	TypePort:              "port",
	TypeInputPort:         "input port",
	TypeOutputPort:        "output port",
	TypeTextualInputPort:  "textual input port",
	TypeTextualOutputPort: "textual output port",
	TypeBinaryInputPort:   "binary input port",
	TypeBinaryOutputPort:  "binary output port",
}

// Description returns a human-readable description of the type constraint.
func (p ValueType) Description() string {
	if p >= TypeCount {
		return "unknown type"
	}
	return typeDescriptions[p]
}

// checkFunc is the signature for per-type check functions.
// Returns (narrowed value, matched, error). When matched is false, error
// describes the type mismatch.
type checkFunc func(Value) (any, bool, error)

// checks is populated in init() with a checker for each ValueType.
var checks [TypeCount]checkFunc

func init() {
	checks[TypeAny] = func(v Value) (any, bool, error) {
		return v, true, nil
	}
	checks[TypeVoid] = func(v Value) (any, bool, error) {
		if v == nil || v.IsVoid() {
			return v, true, nil
		}
		return nil, false, werr.WrapForeignErrorf(werr.ErrInvalidArgument,
			"expected void, got %s", SchemeTypeName(v))
	}
	checks[TypeBoolean] = makeCheck[*Boolean]("boolean")
	checks[TypeNumber] = makeCheck[Number]("number")
	checks[TypeComplex] = makeCheck[ComplexNumber]("complex")
	checks[TypeReal] = makeCheck[RealNumber]("real")
	checks[TypeRational] = makeCheck[*Rational]("rational")
	checks[TypeInteger] = func(v Value) (any, bool, error) {
		switch t := v.(type) {
		case *Integer:
			return t, true, nil
		case *BigInteger:
			return t, true, nil
		default:
			return nil, false, werr.WrapForeignErrorf(werr.ErrInvalidArgument,
				"expected integer, got %s", SchemeTypeName(v))
		}
	}
	checks[TypeFlonum] = func(v Value) (any, bool, error) {
		switch t := v.(type) {
		case *Float:
			return t, true, nil
		case *BigFloat:
			return t, true, nil
		default:
			return nil, false, werr.WrapForeignErrorf(werr.ErrInvalidArgument,
				"expected flonum, got %s", SchemeTypeName(v))
		}
	}
	checks[TypeString] = makeCheck[*String]("string")
	checks[TypeCharacter] = makeCheck[*Character]("character")
	checks[TypeSymbol] = makeCheck[*Symbol]("symbol")
	checks[TypeByte] = makeCheck[*Byte]("byte")
	checks[TypePair] = makeCheck[*Pair]("pair")
	checks[TypeList] = makeCheck[Tuple]("list")
	checks[TypeVector] = makeCheck[*Vector]("vector")
	checks[TypeByteVector] = makeCheck[*ByteVector]("bytevector")
	checks[TypeHashtable] = makeCheck[*Hashtable]("hashtable")
	checks[TypeProcedure] = makeCheck[Callable]("procedure")
	checks[TypePort] = makeCheck[Port]("port")
	checks[TypeInputPort] = makeCheck[InputPort]("input-port")
	checks[TypeOutputPort] = makeCheck[OutputPort]("output-port")
	checks[TypeTextualInputPort] = makeCheck[TextualReader]("textual-input-port")
	checks[TypeTextualOutputPort] = makeCheck[TextualWriter]("textual-output-port")
	checks[TypeBinaryInputPort] = makeCheck[BinaryReader]("binary-input-port")
	checks[TypeBinaryOutputPort] = makeCheck[BinaryWriter]("binary-output-port")

	// Verify all slots are populated — catches missing entries when new types are added.
	for i := range TypeCount {
		if typeNames[i] == "" {
			panic("values: missing typeNames entry for ValueType " + fmt.Sprint(i))
		}
		if typeDescriptions[i] == "" {
			panic("values: missing typeDescriptions entry for ValueType " + fmt.Sprint(i))
		}
		if checks[i] == nil {
			panic("values: missing Check function for ValueType " + typeNames[i])
		}
	}
}

// Check tests whether v satisfies this type constraint.
// On success, returns the narrowed value and true.
// On failure, returns nil, false, and an error describing the mismatch.
func (p ValueType) Check(v Value) (any, bool, error) {
	if p >= TypeCount {
		return nil, false, werr.WrapForeignErrorf(werr.ErrInvalidArgument, "invalid ValueType %d", p)
	}
	return checks[p](v)
}

// SchemeTypeName returns the Scheme-facing type name for a value.
// Used in error messages so users see "integer" instead of "*values.Integer".
func SchemeTypeName(v Value) string {
	if v == nil || v.IsVoid() {
		return "void"
	}
	switch v.(type) {
	case *Boolean:
		return "boolean"
	case *Integer, *BigInteger:
		return "integer"
	case *Rational:
		return "rational"
	case *Float, *BigFloat:
		return "flonum"
	case *Complex, *BigComplex:
		return "complex"
	case *String:
		return "string"
	case *Character:
		return "character"
	case *Symbol:
		return "symbol"
	case *Byte:
		return "byte"
	case *Pair:
		return "pair"
	case *Vector:
		return "vector"
	case *ByteVector:
		return "bytevector"
	case *Hashtable:
		return "hashtable"
	case *Record:
		return "record"
	case *Box:
		return "box"
	case *Promise:
		return "promise"
	default:
		// Fall back to interface checks for port types and other interfaces.
		switch {
		case IsEmptyList(v):
			return "empty-list"
		case IsList(v):
			return "list"
		default:
			return fmt.Sprintf("%T", v)
		}
	}
}

// makeCheck creates a checkFunc for type T via a Go type assertion. T may be
// either a concrete pointer type (e.g., *Boolean) or an interface type
// (e.g., Number, Port) — the assertion semantics handle both uniformly.
// typeName is the Scheme-facing name used in mismatch errors.
func makeCheck[T any](typeName string) checkFunc {
	return func(v Value) (any, bool, error) {
		t, ok := v.(T)
		if ok {
			return t, true, nil
		}
		return nil, false, werr.WrapForeignErrorf(werr.ErrInvalidArgument,
			"expected %s, got %s", typeName, SchemeTypeName(v))
	}
}

// NamedTypeConstraint represents an unresolved type name from a docstring
// (e.g., "point"). It is documentation-only — Check always fails because the
// constraint has not been resolved to a concrete type.
type NamedTypeConstraint struct {
	name string
}

// NewNamedTypeConstraint creates a NamedTypeConstraint with the given name.
func NewNamedTypeConstraint(name string) *NamedTypeConstraint {
	return &NamedTypeConstraint{name: name}
}

// Name returns the unresolved type name.
func (p *NamedTypeConstraint) Name() string {
	return p.name
}

// Description returns the unresolved type name as its description.
func (p *NamedTypeConstraint) Description() string {
	return p.name
}

// Check always fails — the constraint is unresolved and cannot validate values.
func (p *NamedTypeConstraint) Check(v Value) (any, bool, error) {
	return nil, false, werr.WrapForeignErrorf(werr.ErrInvalidArgument,
		"unresolved type constraint %q", p.name)
}

// RecordTypeConstraint validates that a value is a Record whose RecordType
// matches (or inherits from) a specific record type descriptor.
type RecordTypeConstraint struct {
	rtd *RecordType
}

// NewRecordTypeConstraint creates a RecordTypeConstraint for the given
// record type descriptor. Panics if rtd is nil.
func NewRecordTypeConstraint(rtd *RecordType) *RecordTypeConstraint {
	if rtd == nil {
		panic(werr.WrapForeignErrorf(werr.ErrInvalidArgument,
			"NewRecordTypeConstraint: rtd must not be nil"))
	}
	return &RecordTypeConstraint{rtd: rtd}
}

// Name returns the Scheme-facing name of the record type.
func (p *RecordTypeConstraint) Name() string {
	return p.rtd.Name().Key
}

// Description returns a human-readable description of the record type constraint.
func (p *RecordTypeConstraint) Description() string {
	return p.rtd.Name().Key + " record"
}

// Check tests whether v is a Record whose type matches (or inherits from) the
// target record type descriptor. Walks the parent chain for subtype matching.
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
	gotName := "unknown"
	if rec.RecordType() != nil {
		gotName = rec.RecordType().Name().Key
	}
	return nil, false, werr.WrapForeignErrorf(werr.ErrInvalidArgument,
		"expected %s record, got %s record",
		p.rtd.Name().Key, gotName)
}
