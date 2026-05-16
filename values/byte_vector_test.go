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
	"errors"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"
	"github.com/aalpar/wile/werr"
)

func TestByteVector_SchemeString(t *testing.T) {
	tcs := []struct {
		in  values.Value
		out string
	}{
		{
			in:  values.NewByteVector(values.NewByte(10)),
			out: "#u8(10)",
		},
		{
			in:  values.NewByteVector(values.NewByte(10), values.NewByte(20)),
			out: "#u8(10 20)",
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
		in0 values.Value
		in1 values.Value
		out bool
	}{
		{
			in0: values.NewByteVector(values.NewByte(10)),
			in1: values.NewByteVector(values.NewByte(20)),
			out: false,
		},
		{
			in0: values.NewByteVector(values.NewByte(10)),
			in1: values.NewByteVector(values.NewByte(10)),
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
	bv := values.NewByteVector(values.NewByte(10), values.NewByte(20), values.NewByte(30))
	c.Assert(bv.Get(0), valuestest.SchemeEquals, values.NewByte(10))
	c.Assert(bv.Get(1), valuestest.SchemeEquals, values.NewByte(20))
	c.Assert(bv.Get(2), valuestest.SchemeEquals, values.NewByte(30))
}

func TestByteVector_Set(t *testing.T) {
	c := qt.New(t)
	bv := values.NewByteVector(values.NewByte(10), values.NewByte(20), values.NewByte(30))
	bv.Set(1, values.NewByte(99))
	c.Assert(bv.Get(1), valuestest.SchemeEquals, values.NewByte(99))
	c.Assert(bv.SchemeString(), qt.Equals, "#u8(10 99 30)")
}

func TestByteVector_Set_Error(t *testing.T) {
	c := qt.New(t)
	bv := values.NewByteVector(values.NewByte(10))
	err := bv.Set(0, values.NewInteger(42))
	c.Assert(err, qt.Not(qt.IsNil))
	c.Assert(err, qt.ErrorMatches, ".*bytevector element must be a byte.*")
}

func TestByteVector_AsList(t *testing.T) {
	c := qt.New(t)

	tests := []struct {
		name     string
		bv       *values.ByteVector
		expected string // SchemeString representation
	}{
		{
			name:     "empty bytevector",
			bv:       values.NewByteVector(),
			expected: "()",
		},
		{
			name:     "single element",
			bv:       values.NewByteVector(values.NewByte(42)),
			expected: "(42)",
		},
		{
			name:     "multiple elements",
			bv:       values.NewByteVector(values.NewByte(10), values.NewByte(20), values.NewByte(30)),
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
	var bv *values.ByteVector
	list := bv.AsList()
	c.Assert(list, qt.IsNil)
}

func TestByteVector_IsVoid(t *testing.T) {
	c := qt.New(t)

	tests := []struct {
		name     string
		bv       *values.ByteVector
		expected bool
	}{
		{
			name:     "nil bytevector",
			bv:       nil,
			expected: true,
		},
		{
			name:     "empty bytevector",
			bv:       values.NewByteVector(),
			expected: false,
		},
		{
			name:     "non-empty bytevector",
			bv:       values.NewByteVector(values.NewByte(42)),
			expected: false,
		},
	}

	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			c.Assert(tt.bv.IsVoid(), qt.Equals, tt.expected)
		})
	}
}

func TestNewByteVectorFromIntegers(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		ints []*values.Integer
		want string
	}{
		{
			name: "empty",
			ints: nil,
			want: "#u8()",
		},
		{
			name: "single byte",
			ints: []*values.Integer{values.NewInteger(42)},
			want: "#u8(42)",
		},
		{
			name: "boundary values",
			ints: []*values.Integer{values.NewInteger(0), values.NewInteger(255)},
			want: "#u8(0 255)",
		},
	}
	for _, tc := range tcs {
		c.Run(tc.name, func(c *qt.C) {
			bv, err := values.NewByteVectorFromIntegers(tc.ints...)
			c.Assert(err, qt.IsNil)
			c.Assert(bv.SchemeString(), qt.Equals, tc.want)
		})
	}
}

func TestNewByteVectorFromIntegers_Overflow(t *testing.T) {
	tcs := []struct {
		name string
		ints []*values.Integer
	}{
		{
			name: "negative value",
			ints: []*values.Integer{values.NewInteger(-1)},
		},
		{
			name: "above 255",
			ints: []*values.Integer{values.NewInteger(256)},
		},
		{
			name: "large positive",
			ints: []*values.Integer{values.NewInteger(1000)},
		},
		{
			name: "valid then invalid",
			ints: []*values.Integer{values.NewInteger(10), values.NewInteger(300)},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := values.NewByteVectorFromIntegers(tc.ints...)
			qt.Assert(t, err, qt.IsNotNil)
			qt.Assert(t, errors.Is(err, werr.ErrNotAByte), qt.IsTrue)
		})
	}
}
