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

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"
)

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

	r := values.NewRecord(rt, []values.Value{values.NewInteger(3), values.NewInteger(4)})

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

	r := values.NewRecord(rt, []values.Value{values.NewInteger(3), values.NewInteger(4)})

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

	r := values.NewRecord(rt, []values.Value{values.NewInteger(3), values.NewInteger(4)})

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
			in:   values.NewRecord(rt, []values.Value{}),
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

	tcs := []struct {
		name string
		a    *values.Record
		b    values.Value
		out  bool
	}{
		{
			name: "equal records same type and fields",
			a:    values.NewRecord(rt, []values.Value{values.NewInteger(1), values.NewInteger(2)}),
			b:    values.NewRecord(rt, []values.Value{values.NewInteger(1), values.NewInteger(2)}),
			out:  true,
		},
		{
			name: "different field values",
			a:    values.NewRecord(rt, []values.Value{values.NewInteger(1), values.NewInteger(2)}),
			b:    values.NewRecord(rt, []values.Value{values.NewInteger(1), values.NewInteger(3)}),
			out:  false,
		},
		{
			name: "different record types",
			a:    values.NewRecord(rt, []values.Value{values.NewInteger(1), values.NewInteger(2)}),
			b:    values.NewRecord(rt2, []values.Value{values.NewInteger(1), values.NewInteger(2)}),
			out:  false,
		},
		{
			name: "comparison with non-record",
			a:    values.NewRecord(rt, []values.Value{values.NewInteger(1), values.NewInteger(2)}),
			b:    values.NewInteger(1),
			out:  false,
		},
		{
			name: "comparison with nil record",
			a:    values.NewRecord(rt, []values.Value{values.NewInteger(1), values.NewInteger(2)}),
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
			a:    values.NewRecord(rt, []values.Value{}),
			b:    values.NewRecord(rt, []values.Value{}),
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

	r := values.NewRecord(rt, []values.Value{values.NewInteger(42)})
	qt.Assert(t, r.SchemeString(), qt.Equals, "#<record:point>")

	var nilR *values.Record
	qt.Assert(t, nilR.SchemeString(), qt.Equals, "#<record>")
}
