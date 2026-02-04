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

func TestByteVector_SchemeString(t *testing.T) {
	tcs := []struct {
		in  Value
		out string
	}{
		{
			in:  NewByteVectorFromIntegers(NewInteger(10)),
			out: "#u8( 10 )",
		},
		{
			in:  NewByteVectorFromIntegers(NewInteger(10), NewInteger(20)),
			out: "#u8( 10 20 )",
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			qt.Assert(t, tc.in.SchemeString(), qt.Equals, tc.out)
		})
	}
}

func TestByteVector_EqualTo(t *testing.T) {
	tcs := []struct {
		in0 Value
		in1 Value
		out bool
	}{
		{
			in0: NewByteVectorFromIntegers(NewInteger(10)),
			in1: NewByteVectorFromIntegers(NewInteger(20)),
			out: false,
		},
		{
			in0: NewByteVectorFromIntegers(NewInteger(10)),
			in1: NewByteVectorFromIntegers(NewInteger(10)),
			out: true,
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			qt.Assert(t, tc.in0.EqualTo(tc.in1), qt.Equals, tc.out)
		})
	}
}

func TestByteVector_Get(t *testing.T) {
	c := qt.New(t)
	bv := NewByteVectorFromIntegers(NewInteger(10), NewInteger(20), NewInteger(30))
	c.Assert(bv.Get(0), SchemeEquals, NewByte(10))
	c.Assert(bv.Get(1), SchemeEquals, NewByte(20))
	c.Assert(bv.Get(2), SchemeEquals, NewByte(30))
}

func TestByteVector_Set(t *testing.T) {
	c := qt.New(t)
	bv := NewByteVectorFromIntegers(NewInteger(10), NewInteger(20), NewInteger(30))
	bv.Set(1, NewByte(99))
	c.Assert(bv.Get(1), SchemeEquals, NewByte(99))
	c.Assert(bv.SchemeString(), qt.Equals, "#u8( 10 99 30 )")
}

func TestByteVector_Set_Panic(t *testing.T) {
	c := qt.New(t)
	bv := NewByteVectorFromIntegers(NewInteger(10))
	c.Assert(func() { bv.Set(0, NewInteger(42)) }, qt.PanicMatches, ".*bytevector element must be a byte.*")
}

func TestByteVector_AsList(t *testing.T) {
	c := qt.New(t)

	tests := []struct {
		name     string
		bv       *ByteVector
		expected string // SchemeString representation
	}{
		{
			name:     "empty bytevector",
			bv:       NewByteVectorFromIntegers(),
			expected: "()",
		},
		{
			name:     "single element",
			bv:       NewByteVectorFromIntegers(NewInteger(42)),
			expected: "(42)",
		},
		{
			name:     "multiple elements",
			bv:       NewByteVectorFromIntegers(NewInteger(10), NewInteger(20), NewInteger(30)),
			expected: "(10 20 30)",
		},
	}

	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			list := tt.bv.AsList()
			c.Assert(list.SchemeString(), qt.Equals, tt.expected)
		})
	}
}

func TestByteVector_AsList_Void(t *testing.T) {
	c := qt.New(t)
	var bv *ByteVector
	list := bv.AsList()
	c.Assert(list, qt.IsNil)
}

func TestByteVector_Datum(t *testing.T) {
	c := qt.New(t)
	bv := NewByteVectorFromIntegers(NewInteger(10), NewInteger(20), NewInteger(30))
	datum := bv.Datum()
	c.Assert(len(datum), qt.Equals, 3)
	c.Assert(datum[0], SchemeEquals, NewByte(10))
	c.Assert(datum[1], SchemeEquals, NewByte(20))
	c.Assert(datum[2], SchemeEquals, NewByte(30))
}

func TestByteVector_IsVoid(t *testing.T) {
	c := qt.New(t)

	tests := []struct {
		name     string
		bv       *ByteVector
		expected bool
	}{
		{
			name:     "nil bytevector",
			bv:       nil,
			expected: true,
		},
		{
			name:     "empty bytevector",
			bv:       NewByteVectorFromIntegers(),
			expected: false,
		},
		{
			name:     "non-empty bytevector",
			bv:       NewByteVectorFromIntegers(NewInteger(42)),
			expected: false,
		},
	}

	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			c.Assert(tt.bv.IsVoid(), qt.Equals, tt.expected)
		})
	}
}
