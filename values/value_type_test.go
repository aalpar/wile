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

package values_test

import (
	"bufio"
	"bytes"
	"io"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
)

func TestValueType_String(t *testing.T) {
	tcs := []struct {
		name string
		in   values.ValueType
		out  string
	}{
		{name: "TypeAny", in: values.TypeAny, out: "any"},
		{name: "TypeVoid", in: values.TypeVoid, out: "void"},
		{name: "TypeBoolean", in: values.TypeBoolean, out: "boolean"},
		{name: "TypeNumber", in: values.TypeNumber, out: "number"},
		{name: "TypeComplex", in: values.TypeComplex, out: "complex"},
		{name: "TypeReal", in: values.TypeReal, out: "real"},
		{name: "TypeRational", in: values.TypeRational, out: "rational"},
		{name: "TypeInteger", in: values.TypeInteger, out: "integer"},
		{name: "TypeExactInteger", in: values.TypeExactInteger, out: "exact-integer"},
		{name: "TypeFlonum", in: values.TypeFlonum, out: "flonum"},
		{name: "TypeString", in: values.TypeString, out: "string"},
		{name: "TypeCharacter", in: values.TypeCharacter, out: "character"},
		{name: "TypeSymbol", in: values.TypeSymbol, out: "symbol"},
		{name: "TypeByte", in: values.TypeByte, out: "byte"},
		{name: "TypePair", in: values.TypePair, out: "pair"},
		{name: "TypeList", in: values.TypeList, out: "list"},
		{name: "TypeVector", in: values.TypeVector, out: "vector"},
		{name: "TypeByteVector", in: values.TypeByteVector, out: "bytevector"},
		{name: "TypeHashtable", in: values.TypeHashtable, out: "hashtable"},
		{name: "TypeProcedure", in: values.TypeProcedure, out: "procedure"},
		{name: "TypePort", in: values.TypePort, out: "port"},
		{name: "TypeInputPort", in: values.TypeInputPort, out: "input-port"},
		{name: "TypeOutputPort", in: values.TypeOutputPort, out: "output-port"},
		{name: "TypeTextualInputPort", in: values.TypeTextualInputPort, out: "textual-input-port"},
		{name: "TypeTextualOutputPort", in: values.TypeTextualOutputPort, out: "textual-output-port"},
		{name: "TypeBinaryInputPort", in: values.TypeBinaryInputPort, out: "binary-input-port"},
		{name: "TypeBinaryOutputPort", in: values.TypeBinaryOutputPort, out: "binary-output-port"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, tc.in.String(), qt.Equals, tc.out)
		})
	}
}

func TestValueType_StringUnknown(t *testing.T) {
	tcs := []struct {
		name string
		in   values.ValueType
	}{
		{name: "255", in: values.ValueType(255)},
		{name: "200", in: values.ValueType(200)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, tc.in.String(), qt.Equals, "unknown")
		})
	}
}

func TestValueType_Description(t *testing.T) {
	tcs := []struct {
		name string
		in   values.ValueType
		out  string
	}{
		{name: "TypeAny", in: values.TypeAny, out: "any value"},
		{name: "TypeBoolean", in: values.TypeBoolean, out: "boolean (#t or #f)"},
		{name: "TypeInteger", in: values.TypeInteger, out: "exact integer"},
		{name: "TypeExactInteger", in: values.TypeExactInteger, out: "exact integer"},
		{name: "TypeList", in: values.TypeList, out: "proper list (pair or empty list)"},
		{name: "TypeProcedure", in: values.TypeProcedure, out: "procedure"},
		{name: "TypeByte", in: values.TypeByte, out: "exact integer in [0, 255]"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, tc.in.Description(), qt.Equals, tc.out)
		})
	}
}

func TestValueType_DescriptionUnknown(t *testing.T) {
	qt.Assert(t, values.ValueType(255).Description(), qt.Equals, "unknown type")
}

func TestValueType_Check(t *testing.T) {
	// Test values.
	str := values.NewString("hello")
	integer := values.NewInteger(42)
	bigInt := values.NewBigIntegerFromInt64(100)
	flt := values.NewFloat(3.14)
	bigFlt := values.NewBigFloatFromFloat64(2.71)
	rat := values.NewRational(1, 3)
	char := values.NewCharacter('x')
	sym := values.NewSymbol("foo")
	boolT := values.TrueValue
	byt := values.NewByte(7)
	vec := values.NewVector()
	bvec := values.NewByteVector()
	pair := values.NewCons(integer, values.EmptyList)
	emptyLst := values.EmptyList
	ht := values.NewEmptyHashtable()
	voidVal := values.Void
	cplx := values.NewComplex(1 + 2i)
	callable := newStubCallable(values.NewSymbol("proc"))

	// Ports: construct minimal instances for each port interface.
	textInPort := values.NewStringInputPortWithBuffer(bytes.NewBufferString(""))
	textOutPort := values.NewStringOutputPort()
	binInPort := values.NewBinaryInputPort(bufio.NewReader(bytes.NewReader(nil)))
	binOutPort := values.NewBinaryOutputPortFromWriter(io.Discard)

	tcs := []struct {
		name    string
		typ     values.ValueType
		val     values.Value
		match   bool
		wantErr bool
	}{
		// TypeAny matches everything.
		{name: "any/string", typ: values.TypeAny, val: str, match: true},
		{name: "any/integer", typ: values.TypeAny, val: integer, match: true},
		{name: "any/void", typ: values.TypeAny, val: voidVal, match: true},

		// TypeVoid.
		{name: "void/void", typ: values.TypeVoid, val: voidVal, match: true},
		{name: "void/integer", typ: values.TypeVoid, val: integer, match: false, wantErr: true},

		// TypeBoolean.
		{name: "boolean/true", typ: values.TypeBoolean, val: boolT, match: true},
		{name: "boolean/string", typ: values.TypeBoolean, val: str, match: false, wantErr: true},

		// TypeNumber — Integer implements Number.
		{name: "number/integer", typ: values.TypeNumber, val: integer, match: true},
		{name: "number/float", typ: values.TypeNumber, val: flt, match: true},
		{name: "number/complex", typ: values.TypeNumber, val: cplx, match: true},
		{name: "number/string", typ: values.TypeNumber, val: str, match: false, wantErr: true},

		// TypeComplex.
		{name: "complex/complex", typ: values.TypeComplex, val: cplx, match: true},
		{name: "complex/integer", typ: values.TypeComplex, val: integer, match: false, wantErr: true},

		// TypeReal — Integer and Float implement RealNumber.
		{name: "real/integer", typ: values.TypeReal, val: integer, match: true},
		{name: "real/float", typ: values.TypeReal, val: flt, match: true},
		{name: "real/complex", typ: values.TypeReal, val: cplx, match: false, wantErr: true},

		// TypeRational.
		{name: "rational/rational", typ: values.TypeRational, val: rat, match: true},
		{name: "rational/integer", typ: values.TypeRational, val: integer, match: false, wantErr: true},

		// TypeInteger — matches *Integer and *BigInteger.
		{name: "integer/integer", typ: values.TypeInteger, val: integer, match: true},
		{name: "integer/bigint", typ: values.TypeInteger, val: bigInt, match: true},
		{name: "integer/float", typ: values.TypeInteger, val: flt, match: false, wantErr: true},

		// TypeExactInteger — alias for TypeInteger.
		{name: "exact-integer/integer", typ: values.TypeExactInteger, val: integer, match: true},
		{name: "exact-integer/bigint", typ: values.TypeExactInteger, val: bigInt, match: true},

		// TypeFlonum — matches *Float and *BigFloat.
		{name: "flonum/float", typ: values.TypeFlonum, val: flt, match: true},
		{name: "flonum/bigfloat", typ: values.TypeFlonum, val: bigFlt, match: true},
		{name: "flonum/integer", typ: values.TypeFlonum, val: integer, match: false, wantErr: true},

		// TypeString.
		{name: "string/string", typ: values.TypeString, val: str, match: true},
		{name: "string/symbol", typ: values.TypeString, val: sym, match: false, wantErr: true},

		// TypeCharacter.
		{name: "character/char", typ: values.TypeCharacter, val: char, match: true},
		{name: "character/string", typ: values.TypeCharacter, val: str, match: false, wantErr: true},

		// TypeSymbol.
		{name: "symbol/symbol", typ: values.TypeSymbol, val: sym, match: true},
		{name: "symbol/string", typ: values.TypeSymbol, val: str, match: false, wantErr: true},

		// TypeByte.
		{name: "byte/byte", typ: values.TypeByte, val: byt, match: true},
		{name: "byte/integer", typ: values.TypeByte, val: integer, match: false, wantErr: true},

		// TypePair — only *Pair, not empty list.
		{name: "pair/pair", typ: values.TypePair, val: pair, match: true},
		{name: "pair/emptylist", typ: values.TypePair, val: emptyLst, match: false, wantErr: true},

		// TypeList — Tuple interface: matches *Pair and empty list.
		{name: "list/pair", typ: values.TypeList, val: pair, match: true},
		{name: "list/emptylist", typ: values.TypeList, val: emptyLst, match: true},
		{name: "list/vector", typ: values.TypeList, val: vec, match: false, wantErr: true},

		// TypeVector.
		{name: "vector/vector", typ: values.TypeVector, val: vec, match: true},
		{name: "vector/pair", typ: values.TypeVector, val: pair, match: false, wantErr: true},

		// TypeByteVector.
		{name: "bytevector/bvec", typ: values.TypeByteVector, val: bvec, match: true},
		{name: "bytevector/vector", typ: values.TypeByteVector, val: vec, match: false, wantErr: true},

		// TypeHashtable.
		{name: "hashtable/ht", typ: values.TypeHashtable, val: ht, match: true},
		{name: "hashtable/pair", typ: values.TypeHashtable, val: pair, match: false, wantErr: true},

		// TypeProcedure — Callable interface.
		{name: "procedure/callable", typ: values.TypeProcedure, val: callable, match: true},
		{name: "procedure/string", typ: values.TypeProcedure, val: str, match: false, wantErr: true},

		// TypePort — Port interface: all ports match.
		{name: "port/textinput", typ: values.TypePort, val: textInPort, match: true},
		{name: "port/textoutput", typ: values.TypePort, val: textOutPort, match: true},
		{name: "port/string", typ: values.TypePort, val: str, match: false, wantErr: true},

		// TypeInputPort.
		{name: "input-port/textinput", typ: values.TypeInputPort, val: textInPort, match: true},
		{name: "input-port/textoutput", typ: values.TypeInputPort, val: textOutPort, match: false, wantErr: true},

		// TypeOutputPort.
		{name: "output-port/textoutput", typ: values.TypeOutputPort, val: textOutPort, match: true},
		{name: "output-port/textinput", typ: values.TypeOutputPort, val: textInPort, match: false, wantErr: true},

		// TypeTextualInputPort — TextualReader.
		{name: "textual-input/stringinput", typ: values.TypeTextualInputPort, val: textInPort, match: true},
		{name: "textual-input/bininput", typ: values.TypeTextualInputPort, val: binInPort, match: false, wantErr: true},

		// TypeTextualOutputPort — TextualWriter.
		{name: "textual-output/stringoutput", typ: values.TypeTextualOutputPort, val: textOutPort, match: true},
		{name: "textual-output/binoutput", typ: values.TypeTextualOutputPort, val: binOutPort, match: false, wantErr: true},

		// TypeBinaryInputPort — BinaryReader.
		{name: "binary-input/bininput", typ: values.TypeBinaryInputPort, val: binInPort, match: true},
		{name: "binary-input/stringinput", typ: values.TypeBinaryInputPort, val: textInPort, match: false, wantErr: true},

		// TypeBinaryOutputPort — BinaryWriter.
		{name: "binary-output/binoutput", typ: values.TypeBinaryOutputPort, val: binOutPort, match: true},
		{name: "binary-output/stringoutput", typ: values.TypeBinaryOutputPort, val: textOutPort, match: false, wantErr: true},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, ok, err := tc.typ.Check(tc.val)
			qt.Assert(t, ok, qt.Equals, tc.match)
			if tc.match {
				qt.Assert(t, err, qt.IsNil)
				qt.Assert(t, result, qt.IsNotNil)
			}
			if tc.wantErr {
				qt.Assert(t, err, qt.IsNotNil)
			}
		})
	}
}

func TestValueType_CheckOutOfRange(t *testing.T) {
	_, ok, err := values.ValueType(255).Check(values.NewInteger(1))
	qt.Assert(t, ok, qt.Equals, false)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Matches, "invalid ValueType.*")
}

func TestValueTypeImplementsTypeConstraint(t *testing.T) {
	// ValueType must satisfy TypeConstraint via Name(), Description(), Check().
	var tc values.TypeConstraint = values.TypeInteger
	qt.Assert(t, tc.Name(), qt.Equals, "integer")
	qt.Assert(t, tc.Description(), qt.Equals, "exact integer")

	result, ok, err := tc.Check(values.NewInteger(42))
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, qt.IsNotNil)

	_, ok, err = tc.Check(values.NewString("hello"))
	qt.Assert(t, ok, qt.IsFalse)
	qt.Assert(t, err, qt.IsNotNil)
}

func TestTypeAnyVsNil(t *testing.T) {
	// nil TypeConstraint means "unspecified" (no type info declared).
	var unspecified values.TypeConstraint
	qt.Assert(t, unspecified, qt.IsNil)

	// TypeAny means "explicitly accepts any value."
	var anyType values.TypeConstraint = values.TypeAny
	qt.Assert(t, anyType, qt.IsNotNil)
	qt.Assert(t, anyType.Name(), qt.Equals, "any")

	result, ok, err := anyType.Check(values.NewString("hello"))
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, qt.IsNotNil)
}

func TestNamedTypeConstraint(t *testing.T) {
	nc := values.NewNamedTypeConstraint("point")

	qt.Assert(t, nc.Name(), qt.Equals, "point")
	qt.Assert(t, nc.Description(), qt.Equals, "point")

	// Check always fails — this is an unresolved type name.
	_, ok, err := nc.Check(values.NewInteger(1))
	qt.Assert(t, ok, qt.IsFalse)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Matches, `.*unresolved type constraint "point".*`)
}

func TestRecordTypeConstraint(t *testing.T) {
	pointName := values.NewSymbol("point")
	pointRT := values.NewRecordType(pointName, []*values.Symbol{
		values.NewSymbol("x"),
		values.NewSymbol("y"),
	})

	colorPointName := values.NewSymbol("color-point")
	colorPointRT := values.NewDerivedRecordType(colorPointName, pointRT, []*values.Symbol{
		values.NewSymbol("color"),
	})

	otherName := values.NewSymbol("other")
	otherRT := values.NewRecordType(otherName, []*values.Symbol{
		values.NewSymbol("a"),
	})

	pointConstraint := values.NewRecordTypeConstraint(pointRT)

	qt.Assert(t, pointConstraint.Name(), qt.Equals, "point")
	qt.Assert(t, pointConstraint.Description(), qt.Equals, "point record")

	tcs := []struct {
		name    string
		val     values.Value
		match   bool
		wantErr bool
	}{
		{
			name:  "direct match",
			val:   mustNewRecord(pointRT, []values.Value{values.NewInteger(1), values.NewInteger(2)}),
			match: true,
		},
		{
			name:  "subtype match via parent chain",
			val:   mustNewRecord(colorPointRT, []values.Value{values.NewString("red")}),
			match: true,
		},
		{
			name:    "non-record fails",
			val:     values.NewInteger(42),
			match:   false,
			wantErr: true,
		},
		{
			name:    "wrong record type fails",
			val:     mustNewRecord(otherRT, []values.Value{values.NewInteger(1)}),
			match:   false,
			wantErr: true,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, ok, err := pointConstraint.Check(tc.val)
			qt.Assert(t, ok, qt.Equals, tc.match)
			if tc.match {
				qt.Assert(t, err, qt.IsNil)
				qt.Assert(t, result, qt.IsNotNil)
			}
			if tc.wantErr {
				qt.Assert(t, err, qt.IsNotNil)
			}
		})
	}
}

func TestNewRecordTypeConstraintNilPanics(t *testing.T) {
	qt.Assert(t, func() {
		values.NewRecordTypeConstraint(nil)
	}, qt.PanicMatches, `.*rtd must not be nil.*`)
}
