// Copyright 2025 Aaron Alpar
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
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestRecordTypeCreation(t *testing.T) {
	name := NewSymbol("point")
	fieldX := NewSymbol("x")
	fieldY := NewSymbol("y")
	rt := NewRecordType(name, []*Symbol{fieldX, fieldY})

	qt.Assert(t, rt, qt.Not(qt.IsNil))
	qt.Assert(t, rt.Name(), qt.Equals, name)
	qt.Assert(t, rt.FieldCount(), qt.Equals, 2)
	qt.Assert(t, rt.FieldNames()[0], qt.Equals, fieldX)
	qt.Assert(t, rt.FieldNames()[1], qt.Equals, fieldY)
}

func TestRecordTypeFieldIndex(t *testing.T) {
	name := NewSymbol("point")
	fieldX := NewSymbol("x")
	fieldY := NewSymbol("y")
	fieldZ := NewSymbol("z")
	rt := NewRecordType(name, []*Symbol{fieldX, fieldY})

	qt.Assert(t, rt.FieldIndex(fieldX), qt.Equals, 0)
	qt.Assert(t, rt.FieldIndex(fieldY), qt.Equals, 1)
	qt.Assert(t, rt.FieldIndex(fieldZ), qt.Equals, -1)
}

func TestRecordTypeIsVoid(t *testing.T) {
	tcs := []struct {
		name string
		in   *RecordType
		out  bool
	}{
		{
			name: "nil record type is void",
			in:   nil,
			out:  true,
		},
		{
			name: "valid record type is not void",
			in:   NewRecordType(NewSymbol("test"), []*Symbol{}),
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
	name := NewSymbol("point")
	fieldX := NewSymbol("x")
	rt1 := NewRecordType(name, []*Symbol{fieldX})
	rt2 := NewRecordType(name, []*Symbol{fieldX})

	// Record types use identity equality
	qt.Assert(t, rt1.EqualTo(rt1), qt.IsTrue)
	qt.Assert(t, rt1.EqualTo(rt2), qt.IsFalse) // Different objects
	qt.Assert(t, rt1.EqualTo(NewInteger(1)), qt.IsFalse)
}

func TestRecordTypeSchemeString(t *testing.T) {
	name := NewSymbol("point")
	rt := NewRecordType(name, []*Symbol{})
	qt.Assert(t, rt.SchemeString(), qt.Equals, "#<record-type:point>")

	var nilRT *RecordType
	qt.Assert(t, nilRT.SchemeString(), qt.Equals, "#<record-type>")
}

func TestRecordCreation(t *testing.T) {
	name := NewSymbol("point")
	fieldX := NewSymbol("x")
	fieldY := NewSymbol("y")
	rt := NewRecordType(name, []*Symbol{fieldX, fieldY})

	r := NewRecord(rt, []Value{NewInteger(3), NewInteger(4)})

	qt.Assert(t, r, qt.Not(qt.IsNil))
	qt.Assert(t, r.RecordType(), qt.Equals, rt)
	qt.Assert(t, r.Field(0), SchemeEquals, NewInteger(3))
	qt.Assert(t, r.Field(1), SchemeEquals, NewInteger(4))
}

func TestRecordFieldAccess(t *testing.T) {
	name := NewSymbol("point")
	fieldX := NewSymbol("x")
	fieldY := NewSymbol("y")
	rt := NewRecordType(name, []*Symbol{fieldX, fieldY})

	r := NewRecord(rt, []Value{NewInteger(3), NewInteger(4)})

	// By index
	qt.Assert(t, r.Field(0), SchemeEquals, NewInteger(3))
	qt.Assert(t, r.Field(1), SchemeEquals, NewInteger(4))
	qt.Assert(t, r.Field(-1), qt.IsNil)
	qt.Assert(t, r.Field(2), qt.IsNil)

	// By name
	qt.Assert(t, r.FieldByName(fieldX), SchemeEquals, NewInteger(3))
	qt.Assert(t, r.FieldByName(fieldY), SchemeEquals, NewInteger(4))
	qt.Assert(t, r.FieldByName(NewSymbol("z")), qt.IsNil)
}

func TestRecordFieldMutation(t *testing.T) {
	name := NewSymbol("point")
	fieldX := NewSymbol("x")
	fieldY := NewSymbol("y")
	rt := NewRecordType(name, []*Symbol{fieldX, fieldY})

	r := NewRecord(rt, []Value{NewInteger(3), NewInteger(4)})

	// Mutate by index
	r.SetField(0, NewInteger(10))
	qt.Assert(t, r.Field(0), SchemeEquals, NewInteger(10))

	// Mutate by name
	r.SetFieldByName(fieldY, NewInteger(20))
	qt.Assert(t, r.Field(1), SchemeEquals, NewInteger(20))

	// Out of bounds mutation does nothing
	r.SetField(-1, NewInteger(100))
	r.SetField(10, NewInteger(100))
}

func TestRecordIsVoid(t *testing.T) {
	rt := NewRecordType(NewSymbol("test"), []*Symbol{})

	tcs := []struct {
		name string
		in   *Record
		out  bool
	}{
		{
			name: "nil record is void",
			in:   nil,
			out:  true,
		},
		{
			name: "valid record is not void",
			in:   NewRecord(rt, []Value{}),
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
	name := NewSymbol("point")
	fieldX := NewSymbol("x")
	fieldY := NewSymbol("y")
	rt := NewRecordType(name, []*Symbol{fieldX, fieldY})
	rt2 := NewRecordType(NewSymbol("point2"), []*Symbol{fieldX, fieldY})

	tcs := []struct {
		name string
		a    *Record
		b    Value
		out  bool
	}{
		{
			name: "equal records same type and fields",
			a:    NewRecord(rt, []Value{NewInteger(1), NewInteger(2)}),
			b:    NewRecord(rt, []Value{NewInteger(1), NewInteger(2)}),
			out:  true,
		},
		{
			name: "different field values",
			a:    NewRecord(rt, []Value{NewInteger(1), NewInteger(2)}),
			b:    NewRecord(rt, []Value{NewInteger(1), NewInteger(3)}),
			out:  false,
		},
		{
			name: "different record types",
			a:    NewRecord(rt, []Value{NewInteger(1), NewInteger(2)}),
			b:    NewRecord(rt2, []Value{NewInteger(1), NewInteger(2)}),
			out:  false,
		},
		{
			name: "comparison with non-record",
			a:    NewRecord(rt, []Value{NewInteger(1), NewInteger(2)}),
			b:    NewInteger(1),
			out:  false,
		},
		{
			name: "comparison with nil record",
			a:    NewRecord(rt, []Value{NewInteger(1), NewInteger(2)}),
			b:    (*Record)(nil),
			out:  false,
		},
		{
			name: "nil records equal",
			a:    nil,
			b:    (*Record)(nil),
			out:  true,
		},
		{
			name: "empty records same type equal",
			a:    NewRecord(rt, []Value{}),
			b:    NewRecord(rt, []Value{}),
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
	name := NewSymbol("point")
	fieldX := NewSymbol("x")
	rt := NewRecordType(name, []*Symbol{fieldX})

	r := NewRecord(rt, []Value{NewInteger(42)})
	qt.Assert(t, r.SchemeString(), qt.Equals, "#<record:point>")

	var nilR *Record
	qt.Assert(t, nilR.SchemeString(), qt.Equals, "#<record>")
}
