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

import "github.com/aalpar/wile/werr"

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
	TypeInteger                            // *Integer | *BigInteger
	TypeExactInteger                       // alias for TypeInteger
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
	typeCount                              // sentinel — must be last
)

// typeNames maps each ValueType to its display name.
var typeNames = [typeCount]string{
	TypeAny:               "any",
	TypeVoid:              "void",
	TypeBoolean:           "boolean",
	TypeNumber:            "number",
	TypeComplex:           "complex",
	TypeReal:              "real",
	TypeRational:          "rational",
	TypeInteger:           "integer",
	TypeExactInteger:      "exact-integer",
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
	if p >= typeCount {
		return "unknown"
	}
	return typeNames[p]
}

// typeDescriptions maps each ValueType to a human-readable description.
var typeDescriptions = [typeCount]string{
	TypeAny:               "any value",
	TypeVoid:              "void (no meaningful return value)",
	TypeBoolean:           "boolean (#t or #f)",
	TypeNumber:            "number (any numeric type)",
	TypeComplex:           "complex number",
	TypeReal:              "real number",
	TypeRational:          "exact rational number",
	TypeInteger:           "exact integer",
	TypeExactInteger:      "exact integer",
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
	if p >= typeCount {
		return "unknown type"
	}
	return typeDescriptions[p]
}

// checkFunc is the signature for per-type check functions.
// Returns (narrowed value, matched, error). When matched is false, error
// describes the type mismatch.
type checkFunc func(Value) (any, bool, error)

// checks is populated in init() with a checker for each ValueType.
var checks [typeCount]checkFunc

func init() {
	checks[TypeAny] = func(v Value) (any, bool, error) {
		return v, true, nil
	}
	checks[TypeVoid] = func(v Value) (any, bool, error) {
		if v.IsVoid() {
			return v, true, nil
		}
		return nil, false, werr.WrapForeignErrorf(werr.ErrInvalidArgument, "expected void, got %T", v)
	}
	checks[TypeBoolean] = makeCheck[*Boolean]("boolean")
	checks[TypeNumber] = makeInterfaceCheck[Number]("number")
	checks[TypeComplex] = makeInterfaceCheck[ComplexNumber]("complex")
	checks[TypeReal] = makeInterfaceCheck[RealNumber]("real")
	checks[TypeRational] = makeCheck[*Rational]("rational")
	checks[TypeInteger] = func(v Value) (any, bool, error) {
		switch t := v.(type) {
		case *Integer:
			return t, true, nil
		case *BigInteger:
			return t, true, nil
		default:
			return nil, false, werr.WrapForeignErrorf(werr.ErrInvalidArgument, "expected integer, got %T", v)
		}
	}
	checks[TypeExactInteger] = checks[TypeInteger]
	checks[TypeFlonum] = func(v Value) (any, bool, error) {
		switch t := v.(type) {
		case *Float:
			return t, true, nil
		case *BigFloat:
			return t, true, nil
		default:
			return nil, false, werr.WrapForeignErrorf(werr.ErrInvalidArgument, "expected flonum, got %T", v)
		}
	}
	checks[TypeString] = makeCheck[*String]("string")
	checks[TypeCharacter] = makeCheck[*Character]("character")
	checks[TypeSymbol] = makeCheck[*Symbol]("symbol")
	checks[TypeByte] = makeCheck[*Byte]("byte")
	checks[TypePair] = makeCheck[*Pair]("pair")
	checks[TypeList] = makeInterfaceCheck[Tuple]("list")
	checks[TypeVector] = makeCheck[*Vector]("vector")
	checks[TypeByteVector] = makeCheck[*ByteVector]("bytevector")
	checks[TypeHashtable] = makeCheck[*Hashtable]("hashtable")
	checks[TypeProcedure] = makeInterfaceCheck[Callable]("procedure")
	checks[TypePort] = makeInterfaceCheck[Port]("port")
	checks[TypeInputPort] = makeInterfaceCheck[InputPort]("input-port")
	checks[TypeOutputPort] = makeInterfaceCheck[OutputPort]("output-port")
	checks[TypeTextualInputPort] = makeInterfaceCheck[TextualReader]("textual-input-port")
	checks[TypeTextualOutputPort] = makeInterfaceCheck[TextualWriter]("textual-output-port")
	checks[TypeBinaryInputPort] = makeInterfaceCheck[BinaryReader]("binary-input-port")
	checks[TypeBinaryOutputPort] = makeInterfaceCheck[BinaryWriter]("binary-output-port")
}

// Check tests whether v satisfies this type constraint.
// On success, returns the narrowed value and true.
// On failure, returns nil, false, and an error describing the mismatch.
func (p ValueType) Check(v Value) (any, bool, error) {
	if p >= typeCount {
		return nil, false, werr.WrapForeignErrorf(werr.ErrInvalidArgument, "invalid ValueType %d", p)
	}
	return checks[p](v)
}

// makeCheck creates a checkFunc for a concrete pointer type T.
func makeCheck[T any](typeName string) checkFunc {
	return func(v Value) (any, bool, error) {
		t, ok := v.(T)
		if ok {
			return t, true, nil
		}
		return nil, false, werr.WrapForeignErrorf(werr.ErrInvalidArgument, "expected %s, got %T", typeName, v)
	}
}

// makeInterfaceCheck creates a checkFunc for an interface type T.
// The implementation is identical to makeCheck — both use Go type assertions —
// but keeping them separate documents the intent: concrete type vs interface.
func makeInterfaceCheck[T any](typeName string) checkFunc {
	return func(v Value) (any, bool, error) {
		t, ok := v.(T)
		if ok {
			return t, true, nil
		}
		return nil, false, werr.WrapForeignErrorf(werr.ErrInvalidArgument, "expected %s, got %T", typeName, v)
	}
}
