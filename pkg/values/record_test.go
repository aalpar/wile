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
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
)

// mustNewRecord calls NewRecord and panics on error. Test-only helper.
func mustNewRecord(rt *values.RecordType, fields []values.Value) *values.Record {
	r, err := values.NewRecord(rt, fields)
	if err != nil {
		panic(err)
	}
	return r
}

func TestRecordTypeParent(t *testing.T) {
	pointName := values.NewSymbol("point")
	fieldX := values.NewSymbol("x")
	fieldY := values.NewSymbol("y")
	pointRT := values.NewRecordType(pointName, []*values.Symbol{fieldX, fieldY})

	// Base type has nil parent.
	qt.Assert(t, pointRT.Parent(), qt.IsNil)

	colorPointName := values.NewSymbol("color-point")
	fieldColor := values.NewSymbol("color")
	colorPointRT := values.NewDerivedRecordType(colorPointName, pointRT, []*values.Symbol{fieldColor})

	// Derived type has the expected parent.
	qt.Assert(t, colorPointRT.Parent(), qt.Equals, pointRT)
	qt.Assert(t, colorPointRT.Name(), qt.Equals, colorPointName)
	qt.Assert(t, colorPointRT.FieldCount(), qt.Equals, 1)
	qt.Assert(t, colorPointRT.FieldNames()[0], qt.Equals, fieldColor)
}

func TestRecordTypeCreation(t *testing.T) {
	name := values.NewSymbol("point")
	fieldX := values.NewSymbol("x")
	fieldY := values.NewSymbol("y")
	rt := values.NewRecordType(name, []*values.Symbol{fieldX, fieldY})

	qt.Assert(t, rt, qt.Not(qt.IsNil))
	qt.Assert(t, rt.Name(), qt.Equals, name)
	qt.Assert(t, rt.FieldCount(), qt.Equals, 2)
	qt.Assert(t, rt.FieldNames()[0], qt.Equals, fieldX)
	qt.Assert(t, rt.FieldNames()[1], qt.Equals, fieldY)
}

func TestRecordTypeFieldIndex(t *testing.T) {
	name := values.NewSymbol("point")
	fieldX := values.NewSymbol("x")
	fieldY := values.NewSymbol("y")
	fieldZ := values.NewSymbol("z")
	rt := values.NewRecordType(name, []*values.Symbol{fieldX, fieldY})

	qt.Assert(t, rt.FieldIndex(fieldX), qt.Equals, 0)
	qt.Assert(t, rt.FieldIndex(fieldY), qt.Equals, 1)
	qt.Assert(t, rt.FieldIndex(fieldZ), qt.Equals, -1)
}

func TestRecordTypeIsVoid(t *testing.T) {
	tcs := []struct {
		name string
		in   *values.RecordType
		out  bool
	}{
		{
			name: "nil record type is void",
			in:   nil,
			out:  true,
		},
		{
			name: "valid record type is not void",
			in:   values.NewRecordType(values.NewSymbol("test"), []*values.Symbol{}),
			out:  false,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, tc.in.IsVoid(), qt.Equals, tc.out)
		})
	}
}

func TestRecordTypeEqualTo(t *testing.T) {
	name := values.NewSymbol("point")
	fieldX := values.NewSymbol("x")
	rt1 := values.NewRecordType(name, []*values.Symbol{fieldX})
	rt2 := values.NewRecordType(name, []*values.Symbol{fieldX})

	// Record types use identity equality
	qt.Assert(t, rt1.EqualTo(rt1), qt.IsTrue)
	qt.Assert(t, rt1.EqualTo(rt2), qt.IsFalse) // Different objects
	qt.Assert(t, rt1.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

func TestRecordTypeSchemeString(t *testing.T) {
	name := values.NewSymbol("point")
	rt := values.NewRecordType(name, []*values.Symbol{})
	qt.Assert(t, rt.SchemeString(), qt.Equals, "#<record-type:point>")

	var nilRT *values.RecordType
	qt.Assert(t, nilRT.SchemeString(), qt.Equals, "#<record-type>")
}

func TestRecordCreation(t *testing.T) {
	name := values.NewSymbol("point")
	fieldX := values.NewSymbol("x")
	fieldY := values.NewSymbol("y")
	rt := values.NewRecordType(name, []*values.Symbol{fieldX, fieldY})

	r := mustNewRecord(rt, []values.Value{values.NewInteger(3), values.NewInteger(4)})

	qt.Assert(t, r, qt.Not(qt.IsNil))
	qt.Assert(t, r.RecordType(), qt.Equals, rt)
	qt.Assert(t, r.Field(0), valuestest.SchemeEquals, values.NewInteger(3))
	qt.Assert(t, r.Field(1), valuestest.SchemeEquals, values.NewInteger(4))
}

func TestRecordFieldAccess(t *testing.T) {
	name := values.NewSymbol("point")
	fieldX := values.NewSymbol("x")
	fieldY := values.NewSymbol("y")
	rt := values.NewRecordType(name, []*values.Symbol{fieldX, fieldY})

	r := mustNewRecord(rt, []values.Value{values.NewInteger(3), values.NewInteger(4)})

	// By index
	qt.Assert(t, r.Field(0), valuestest.SchemeEquals, values.NewInteger(3))
	qt.Assert(t, r.Field(1), valuestest.SchemeEquals, values.NewInteger(4))
	qt.Assert(t, r.Field(-1), qt.IsNil)
	qt.Assert(t, r.Field(2), qt.IsNil)

	// By name
	qt.Assert(t, r.FieldByName(fieldX), valuestest.SchemeEquals, values.NewInteger(3))
	qt.Assert(t, r.FieldByName(fieldY), valuestest.SchemeEquals, values.NewInteger(4))
	qt.Assert(t, r.FieldByName(values.NewSymbol("z")), qt.IsNil)
}

func TestRecordFieldMutation(t *testing.T) {
	name := values.NewSymbol("point")
	fieldX := values.NewSymbol("x")
	fieldY := values.NewSymbol("y")
	rt := values.NewRecordType(name, []*values.Symbol{fieldX, fieldY})

	r := mustNewRecord(rt, []values.Value{values.NewInteger(3), values.NewInteger(4)})

	// Mutate by index
	r.SetField(0, values.NewInteger(10))
	qt.Assert(t, r.Field(0), valuestest.SchemeEquals, values.NewInteger(10))

	// Mutate by name
	r.SetFieldByName(fieldY, values.NewInteger(20))
	qt.Assert(t, r.Field(1), valuestest.SchemeEquals, values.NewInteger(20))

	// Out of bounds mutation does nothing
	r.SetField(-1, values.NewInteger(100))
	r.SetField(10, values.NewInteger(100))
}

func TestRecordIsVoid(t *testing.T) {
	rt := values.NewRecordType(values.NewSymbol("test"), []*values.Symbol{})

	tcs := []struct {
		name string
		in   *values.Record
		out  bool
	}{
		{
			name: "nil record is void",
			in:   nil,
			out:  true,
		},
		{
			name: "valid record is not void",
			in:   mustNewRecord(rt, []values.Value{}),
			out:  false,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, tc.in.IsVoid(), qt.Equals, tc.out)
		})
	}
}

func TestRecordEqualTo(t *testing.T) {
	name := values.NewSymbol("point")
	fieldX := values.NewSymbol("x")
	fieldY := values.NewSymbol("y")
	rt := values.NewRecordType(name, []*values.Symbol{fieldX, fieldY})
	rt2 := values.NewRecordType(values.NewSymbol("point2"), []*values.Symbol{fieldX, fieldY})
	rtEmpty := values.NewRecordType(values.NewSymbol("empty"), []*values.Symbol{})

	tcs := []struct {
		name string
		a    *values.Record
		b    values.Value
		out  bool
	}{
		{
			name: "equal records same type and fields",
			a:    mustNewRecord(rt, []values.Value{values.NewInteger(1), values.NewInteger(2)}),
			b:    mustNewRecord(rt, []values.Value{values.NewInteger(1), values.NewInteger(2)}),
			out:  true,
		},
		{
			name: "different field values",
			a:    mustNewRecord(rt, []values.Value{values.NewInteger(1), values.NewInteger(2)}),
			b:    mustNewRecord(rt, []values.Value{values.NewInteger(1), values.NewInteger(3)}),
			out:  false,
		},
		{
			name: "different record types",
			a:    mustNewRecord(rt, []values.Value{values.NewInteger(1), values.NewInteger(2)}),
			b:    mustNewRecord(rt2, []values.Value{values.NewInteger(1), values.NewInteger(2)}),
			out:  false,
		},
		{
			name: "comparison with non-record",
			a:    mustNewRecord(rt, []values.Value{values.NewInteger(1), values.NewInteger(2)}),
			b:    values.NewInteger(1),
			out:  false,
		},
		{
			name: "comparison with nil record",
			a:    mustNewRecord(rt, []values.Value{values.NewInteger(1), values.NewInteger(2)}),
			b:    (*values.Record)(nil),
			out:  false,
		},
		{
			name: "nil records equal",
			a:    nil,
			b:    (*values.Record)(nil),
			out:  true,
		},
		{
			name: "empty records same type equal",
			a:    mustNewRecord(rtEmpty, []values.Value{}),
			b:    mustNewRecord(rtEmpty, []values.Value{}),
			out:  true,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, tc.a.EqualTo(tc.b), qt.Equals, tc.out)
		})
	}
}

func TestRecordSchemeString(t *testing.T) {
	name := values.NewSymbol("point")
	fieldX := values.NewSymbol("x")
	rt := values.NewRecordType(name, []*values.Symbol{fieldX})

	r := mustNewRecord(rt, []values.Value{values.NewInteger(42)})
	qt.Assert(t, r.SchemeString(), qt.Equals, "#<record:point>")

	var nilR *values.Record
	qt.Assert(t, nilR.SchemeString(), qt.Equals, "#<record>")
}

func TestOpaqueRecordType(t *testing.T) {
	name := values.NewSymbol("stack")
	fieldItems := values.NewSymbol("items")
	rt := values.NewOpaqueRecordType(name, []*values.Symbol{fieldItems})

	qt.Assert(t, rt.IsOpaque(), qt.IsTrue)
	qt.Assert(t, rt.Name(), qt.Equals, name)
	qt.Assert(t, rt.FieldCount(), qt.Equals, 1)

	// Non-opaque record type is not opaque
	normalRT := values.NewRecordType(name, []*values.Symbol{fieldItems})
	qt.Assert(t, normalRT.IsOpaque(), qt.IsFalse)

	// Nil receiver
	var nilRT *values.RecordType
	qt.Assert(t, nilRT.IsOpaque(), qt.IsFalse)
}

func TestOpaqueRecordSchemeString(t *testing.T) {
	name := values.NewSymbol("stack")
	fieldItems := values.NewSymbol("items")
	rt := values.NewOpaqueRecordType(name, []*values.Symbol{fieldItems})

	// Opaque type descriptor hides record-type nature
	qt.Assert(t, rt.SchemeString(), qt.Equals, "#<type:stack>")

	// Opaque instance hides record nature
	r := mustNewRecord(rt, []values.Value{values.EmptyList})
	qt.Assert(t, r.SchemeString(), qt.Equals, "#<stack>")
}

func TestOpaqueRecordFieldAccess(t *testing.T) {
	name := values.NewSymbol("stack")
	fieldItems := values.NewSymbol("items")
	rt := values.NewOpaqueRecordType(name, []*values.Symbol{fieldItems})

	r := mustNewRecord(rt, []values.Value{values.NewInteger(42)})

	// Field access still works — opacity doesn't block Go-level access
	qt.Assert(t, r.Field(0), valuestest.SchemeEquals, values.NewInteger(42))
	qt.Assert(t, r.FieldByName(fieldItems), valuestest.SchemeEquals, values.NewInteger(42))
}

func TestNewRecordFieldCountValidation(t *testing.T) {
	rt := values.NewRecordType(values.NewSymbol("point"), []*values.Symbol{
		values.NewSymbol("x"),
		values.NewSymbol("y"),
	})

	// Correct count succeeds
	r, err := values.NewRecord(rt, []values.Value{values.NewInteger(1), values.NewInteger(2)})
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, r, qt.Not(qt.IsNil))

	// Wrong count errors
	_, err = values.NewRecord(rt, []values.Value{values.NewInteger(1)})
	qt.Assert(t, err, qt.Not(qt.IsNil))

	_, err = values.NewRecord(rt, []values.Value{values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)})
	qt.Assert(t, err, qt.Not(qt.IsNil))

	// Nil record type errors
	_, err = values.NewRecord(nil, []values.Value{})
	qt.Assert(t, err, qt.Not(qt.IsNil))
}

func TestNewOpaqueRecordTypeNilNamePanics(t *testing.T) {
	qt.Assert(t, func() {
		values.NewOpaqueRecordType(nil, []*values.Symbol{})
	}, qt.PanicMatches, `.*name must not be nil.*`)
}

func TestDerivedRecordTypeInheritsOpaque(t *testing.T) {
	parent := values.NewOpaqueRecordType(
		values.NewSymbol("base"),
		[]*values.Symbol{values.NewSymbol("x")},
	)
	child := values.NewDerivedRecordType(
		values.NewSymbol("derived"),
		parent,
		[]*values.Symbol{values.NewSymbol("y")},
	)
	qt.Assert(t, child.IsOpaque(), qt.IsTrue)

	// Non-opaque parent produces non-opaque child
	normalParent := values.NewRecordType(
		values.NewSymbol("normal"),
		[]*values.Symbol{values.NewSymbol("a")},
	)
	normalChild := values.NewDerivedRecordType(
		values.NewSymbol("normal-derived"),
		normalParent,
		[]*values.Symbol{values.NewSymbol("b")},
	)
	qt.Assert(t, normalChild.IsOpaque(), qt.IsFalse)
}
