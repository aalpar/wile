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

package helpers

import (
	"errors"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"
	"github.com/aalpar/wile/werr"
)

// ── SequenceLength ───────────────────────────────────────────────────

func TestSequenceLength_Vector(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		vec  *values.Vector
		want values.Value
	}{
		{
			"empty vector",
			values.NewVector(),
			values.NewInteger(0),
		},
		{
			"three elements",
			values.NewVector(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
			values.NewInteger(3),
		},
		{
			"single element",
			values.NewVector(values.NewString("x")),
			values.NewInteger(1),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.vec)
			err := SequenceLength[*values.Vector](mc, werr.ErrNotAVector, "vector-length")
			c.Assert(err, qt.IsNil)
			c.Assert(mc.GetValue(), valuestest.SchemeEquals, tc.want)
		})
	}
}

func TestSequenceLength_ByteVector(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		bv   *values.ByteVector
		want values.Value
	}{
		{
			"empty bytevector",
			values.NewByteVectorFromBytes(),
			values.NewInteger(0),
		},
		{
			"three bytes",
			values.NewByteVectorFromBytes(1, 2, 3),
			values.NewInteger(3),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.bv)
			err := SequenceLength[*values.ByteVector](mc, werr.ErrNotAByteVector, "bytevector-length")
			c.Assert(err, qt.IsNil)
			c.Assert(mc.GetValue(), valuestest.SchemeEquals, tc.want)
		})
	}
}

func TestSequenceLength_Errors(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name     string
		arg      values.Value
		sentinel error
	}{
		{
			"not a vector",
			values.NewInteger(42),
			werr.ErrNotAVector,
		},
		{
			"string not a vector",
			values.NewString("hello"),
			werr.ErrNotAVector,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.arg)
			err := SequenceLength[*values.Vector](mc, werr.ErrNotAVector, "vector-length")
			c.Assert(err, qt.IsNotNil)
			c.Assert(errors.Is(err, tc.sentinel), qt.IsTrue)
		})
	}
}

// ── SequenceRef ──────────────────────────────────────────────────────

func vectorGet(v *values.Vector, i int) values.Value {
	return v.Get(i)
}

func TestSequenceRef_Vector(t *testing.T) {
	c := qt.New(t)

	vec := values.NewVector(values.NewString("a"), values.NewString("b"), values.NewString("c"))

	tcs := []struct {
		name string
		idx  values.Value
		want values.Value
	}{
		{"index 0", values.NewInteger(0), values.NewString("a")},
		{"index 1", values.NewInteger(1), values.NewString("b")},
		{"index 2", values.NewInteger(2), values.NewString("c")},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(vec, tc.idx)
			err := SequenceRef[*values.Vector](mc, werr.ErrNotAVector, "vector-ref", vectorGet)
			c.Assert(err, qt.IsNil)
			c.Assert(mc.GetValue(), valuestest.SchemeEquals, tc.want)
		})
	}
}

func TestSequenceRef_Errors(t *testing.T) {
	c := qt.New(t)

	vec := values.NewVector(values.NewString("a"), values.NewString("b"))

	tcs := []struct {
		name     string
		arg0     values.Value
		arg1     values.Value
		sentinel error
	}{
		{
			"not a vector",
			values.NewInteger(42),
			values.NewInteger(0),
			werr.ErrNotAVector,
		},
		{
			"index out of range positive",
			vec,
			values.NewInteger(5),
			werr.ErrIndexOutOfRange,
		},
		{
			"index out of range negative",
			vec,
			values.NewInteger(-1),
			werr.ErrIndexOutOfRange,
		},
		{
			"index not an integer",
			vec,
			values.NewString("0"),
			werr.ErrNotAnInteger,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.arg0, tc.arg1)
			err := SequenceRef[*values.Vector](mc, werr.ErrNotAVector, "vector-ref", vectorGet)
			c.Assert(err, qt.IsNotNil)
			c.Assert(errors.Is(err, tc.sentinel), qt.IsTrue,
				qt.Commentf("expected %v, got %v", tc.sentinel, err))
		})
	}
}

// ── SequenceSet ──────────────────────────────────────────────────────

func vectorSet(v *values.Vector, i int, mc *machine.MachineContext) error {
	val := mc.Arg(2)
	return v.Set(i, val)
}

func TestSequenceSet_Vector(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name    string
		initial []values.Value
		idx     int64
		newVal  values.Value
		wantAt  values.Value
	}{
		{
			"set first element",
			[]values.Value{values.NewInteger(1), values.NewInteger(2)},
			0,
			values.NewString("x"),
			values.NewString("x"),
		},
		{
			"set last element",
			[]values.Value{values.NewInteger(1), values.NewInteger(2)},
			1,
			values.NewString("y"),
			values.NewString("y"),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			vec := values.NewVector(tc.initial...)
			mc := makeMC(vec, values.NewInteger(tc.idx), tc.newVal)
			err := SequenceSet[*values.Vector](mc, werr.ErrNotAVector, "vector-set!", vectorSet)
			c.Assert(err, qt.IsNil)
			c.Assert(mc.GetValue(), qt.Equals, values.Void)
			c.Assert(vec.Get(int(tc.idx)), valuestest.SchemeEquals, tc.wantAt)
		})
	}
}

func TestSequenceSet_Errors(t *testing.T) {
	c := qt.New(t)

	vec := values.NewVector(values.NewInteger(1), values.NewInteger(2))

	tcs := []struct {
		name     string
		arg0     values.Value
		arg1     values.Value
		arg2     values.Value
		sentinel error
	}{
		{
			"not a vector",
			values.NewInteger(42),
			values.NewInteger(0),
			values.NewString("x"),
			werr.ErrNotAVector,
		},
		{
			"index out of range",
			vec,
			values.NewInteger(5),
			values.NewString("x"),
			werr.ErrIndexOutOfRange,
		},
		{
			"index negative",
			vec,
			values.NewInteger(-1),
			values.NewString("x"),
			werr.ErrIndexOutOfRange,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.arg0, tc.arg1, tc.arg2)
			err := SequenceSet[*values.Vector](mc, werr.ErrNotAVector, "vector-set!", vectorSet)
			c.Assert(err, qt.IsNotNil)
			c.Assert(errors.Is(err, tc.sentinel), qt.IsTrue,
				qt.Commentf("expected %v, got %v", tc.sentinel, err))
		})
	}
}
