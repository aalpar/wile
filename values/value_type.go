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
	"reflect"

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
	TypePort                               // *PortObject (Port marker interface)
	TypeInputPort                          // *PortObject with rdr slot non-nil
	TypeOutputPort                         // *PortObject with wrt slot non-nil
	TypeTextualInputPort                   // *PortObject with rr slot non-nil
	TypeTextualOutputPort                  // *PortObject with wr slot non-nil
	TypeBinaryInputPort                    // *PortObject with rb slot non-nil
	TypeBinaryOutputPort                   // *PortObject with wb slot non-nil
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

// goTypeToValueType maps each concrete Go runtime type to its
// corresponding ValueType. Populated in init(). Used by SchemeTypeName
// to translate a runtime value's Go type to its Scheme-facing name
// without a hand-maintained switch. Acts as the Go-type → ValueType
// bridge that lets a single authoritative table serve both directions
// of translation.
//
// Multiple Go types can map to the same ValueType when they share a
// Scheme identity — e.g., *Integer and *BigInteger both → TypeInteger
// because Scheme exposes a single "integer" tower regardless of the
// underlying machine representation.
//
// Types whose Scheme name has no ValueType counterpart (Record, Box,
// Promise) are handled by a small explicit switch in SchemeTypeName —
// these three are first-class Scheme values with predicates (record?,
// box?, promise?) but the extension-API vocabulary does not currently
// expose them as ValueType constants. Extending ValueType to cover
// them is tracked alongside Finding 3 in
// plans/2026-05-13-values-structural-reduction.md.
//
// The empty-list singleton (emptyListType) is resolved via IsEmptyList
// in SchemeTypeName. Other Value-implementing types that are neither
// in this map nor in the explicit switch (Thread, Channel, EOF, port
// types, closures, etc.) fall through to fmt.Sprintf("%T", v) — a
// pre-existing leak documented as Opportunity 1 of the same plan, to
// be closed via a roster-completeness test in a follow-up.
var goTypeToValueType map[reflect.Type]ValueType

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
	checks[TypeInputPort] = makePortCapCheck("input-port",
		func(p *PortObject) bool { return p.rdr != nil })
	checks[TypeOutputPort] = makePortCapCheck("output-port",
		func(p *PortObject) bool { return p.wrt != nil })
	checks[TypeTextualInputPort] = makePortCapCheck("textual-input-port",
		func(p *PortObject) bool { return p.rr != nil })
	checks[TypeTextualOutputPort] = makePortCapCheck("textual-output-port",
		func(p *PortObject) bool { return p.wr != nil })
	checks[TypeBinaryInputPort] = makePortCapCheck("binary-input-port",
		func(p *PortObject) bool { return p.rb != nil })
	checks[TypeBinaryOutputPort] = makePortCapCheck("binary-output-port",
		func(p *PortObject) bool { return p.wb != nil })

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

	// Build the Go-type → ValueType reverse map. Listed in the same order
	// as the ValueType constants for greppability. Each pair declares
	// "instances of this Go type render as this Scheme type name."
	goTypeToValueType = map[reflect.Type]ValueType{
		reflect.TypeFor[*Boolean]():    TypeBoolean,
		reflect.TypeFor[*Rational]():   TypeRational,
		reflect.TypeFor[*Integer]():    TypeInteger,
		reflect.TypeFor[*BigInteger](): TypeInteger,
		reflect.TypeFor[*Float]():      TypeFlonum,
		reflect.TypeFor[*BigFloat]():   TypeFlonum,
		reflect.TypeFor[*Complex]():    TypeComplex,
		reflect.TypeFor[*BigComplex](): TypeComplex,
		reflect.TypeFor[*String]():     TypeString,
		reflect.TypeFor[*Character]():  TypeCharacter,
		reflect.TypeFor[*Symbol]():     TypeSymbol,
		reflect.TypeFor[*Byte]():       TypeByte,
		reflect.TypeFor[*Pair]():       TypePair,
		reflect.TypeFor[*Vector]():     TypeVector,
		reflect.TypeFor[*ByteVector](): TypeByteVector,
		reflect.TypeFor[*Hashtable]():  TypeHashtable,
		// *PortObject collapses 9 former concrete port types into one
		// Go type. The reverse map is necessarily asymmetric: the
		// narrow ValueTypes (TypeInputPort, TypeBinaryOutputPort, …)
		// cannot be reverse-mapped from *PortObject alone, since
		// *PortObject covers all of them. The narrow distinctions
		// require capability-slot inspection (use *PortObject.PortKind
		// or the narrow ValueType.Check predicates).
		reflect.TypeFor[*PortObject](): TypePort,
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
//
// Resolution proceeds in three layers:
//  1. The goTypeToValueType reverse map covers concrete types backed by
//     a ValueType constant — one lookup, no per-type case.
//  2. A small explicit switch covers types whose Scheme name has no
//     ValueType counterpart (records, boxes, promises).
//  3. IsEmptyList catches the empty-list singleton.
//
// Unrecognized types fall through to fmt.Sprintf("%T", v) as a
// debugging-grade name. Adding a new Value type with a ValueType
// constant means adding one row to goTypeToValueType, not editing this
// function.
func SchemeTypeName(v Value) string {
	if v == nil || v.IsVoid() {
		return "void"
	}
	vt, ok := goTypeToValueType[reflect.TypeOf(v)]
	if ok {
		return vt.String()
	}
	switch v.(type) {
	case *Record:
		return "record"
	case *Box:
		return "box"
	case *Promise:
		return "promise"
	}
	if IsEmptyList(v) {
		return "empty-list"
	}
	return fmt.Sprintf("%T", v)
}

// makePortCapCheck creates a checkFunc that asserts v is a *PortObject
// whose capability slot (selected by hasCap) is populated. Used to
// build the narrow port ValueType predicates (TypeInputPort,
// TypeBinaryInputPort, etc.) that previously satisfied a Go interface
// but now inspect the slot directly.
//
// When v IS a *PortObject but lacks the required capability, the
// mismatch error reports v.PortKind() rather than SchemeTypeName(v) —
// SchemeTypeName collapses all 9 port flavors to "port" since port
// unification, which would lose the actionable detail.
func makePortCapCheck(typeName string, hasCap func(*PortObject) bool) checkFunc {
	return func(v Value) (any, bool, error) {
		p, ok := v.(*PortObject)
		if ok && hasCap(p) {
			return p, true, nil
		}
		got := SchemeTypeName(v)
		if ok {
			got = p.PortKind()
		}
		return nil, false, werr.WrapForeignErrorf(werr.ErrInvalidArgument,
			"expected %s, got %s", typeName, got)
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
