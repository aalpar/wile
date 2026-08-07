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

package environment_test

import (
	"math"
	"math/big"
	"testing"
	"unsafe"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"
)

// TestUnboxSelectsArm pins which values take the scalar arms and which stay
// boxed. The unboxable set is exactly {float64, int64}; everything else in the
// numeric tower is wider than 64 bits and must fall to the ref arm, because the
// fast path's whole safety argument is that a value it holds can be
// reconstructed exactly.
func TestUnboxSelectsArm(t *testing.T) {
	tests := []struct {
		name string
		val  values.Value
		tag  uint8
	}{
		{name: "float takes the float arm", val: values.NewFloat(1.5), tag: environment.CellFloat},
		{name: "in-cache integer takes the fixnum arm", val: values.NewInteger(7), tag: environment.CellFixnum},
		{name: "out-of-cache integer takes the fixnum arm", val: values.NewInteger(1 << 40), tag: environment.CellFixnum},
		{name: "bigint stays boxed", val: values.NewBigInteger(new(big.Int).Lsh(big.NewInt(1), 200)), tag: environment.CellRef},
		{name: "rational stays boxed", val: values.NewRational(1, 3), tag: environment.CellRef},
		{name: "symbol stays boxed", val: values.NewSymbol("x"), tag: environment.CellRef},
		{name: "boolean stays boxed", val: values.TrueValue, tag: environment.CellRef},
		{name: "empty list stays boxed", val: values.EmptyList, tag: environment.CellRef},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := environment.Unbox(tt.val).Tag()
			if got != tt.tag {
				t.Errorf("Unbox(%s).Tag() = %d, want %d", tt.val.SchemeString(), got, tt.tag)
			}
		})
	}
}

// TestBoxUnboxRoundTripsValue checks the property the fast path depends on:
// a value that leaves the lane is numerically the value that entered it.
func TestBoxUnboxRoundTripsValue(t *testing.T) {
	tests := []struct {
		name string
		val  values.Value
	}{
		{name: "positive float", val: values.NewFloat(1.5)},
		{name: "negative float", val: values.NewFloat(-2.25)},
		{name: "zero", val: values.NewFloat(0.0)},
		{name: "negative zero", val: values.NewFloat(math.Copysign(0, -1))},
		{name: "positive infinity", val: values.NewFloat(math.Inf(1))},
		{name: "negative infinity", val: values.NewFloat(math.Inf(-1))},
		{name: "smallest subnormal", val: values.NewFloat(math.SmallestNonzeroFloat64)},
		{name: "max float", val: values.NewFloat(math.MaxFloat64)},
		{name: "in-cache integer", val: values.NewInteger(7)},
		{name: "integer cache lower edge", val: values.NewInteger(-32768)},
		{name: "integer cache upper edge", val: values.NewInteger(32767)},
		{name: "out-of-cache integer", val: values.NewInteger(1 << 40)},
		{name: "int64 min", val: values.NewInteger(math.MinInt64)},
		{name: "int64 max", val: values.NewInteger(math.MaxInt64)},
		{name: "symbol via the ref arm", val: values.NewSymbol("x")},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := environment.Box(environment.Unbox(tt.val))
			if !values.EqualTo(got, tt.val) {
				t.Errorf("Box(Unbox(%s)) = %s, want equal", tt.val.SchemeString(), got.SchemeString())
			}
		})
	}
}

// TestBoxUnboxPreservesNaN is separate because NaN is not equal to itself, so
// the round-trip has to be checked on the bits rather than through equality.
// The lane must not quietly canonicalize a NaN payload.
func TestBoxUnboxPreservesNaN(t *testing.T) {
	in := values.NewFloat(math.NaN())
	out, ok := environment.Box(environment.Unbox(in)).(*values.Float)
	if !ok {
		t.Fatalf("Box(Unbox(NaN)) is not a *Float")
	}
	if math.Float64bits(out.Value) != math.Float64bits(in.Value) {
		t.Errorf("NaN bits = %#x, want %#x", math.Float64bits(out.Value), math.Float64bits(in.Value))
	}
}

// TestRefArmRoundTripsIdentically pins that a boxed value crossing the lane
// comes back as the SAME object. This is the eq? guarantee: pointer identity is
// observable for every type where R7RS specifies eq?, so the ref arm must never
// reconstruct.
//
// The scalar arms deliberately do NOT promise this — a computed float is
// already not eq? to another computed float of the same value today
// ((eq? (+ 1.0 0.5) (+ 1.0 0.5)) is #f), and R7RS §6.1 leaves eq? on numbers
// unspecified. In-cache integers come back identical anyway, via NewInteger's
// flyweight, and that IS relied on elsewhere, so it is pinned below.
func TestRefArmRoundTripsIdentically(t *testing.T) {
	sym := values.NewSymbol("x")
	if environment.Box(environment.Unbox(sym)) != values.Value(sym) {
		t.Errorf("symbol lost pointer identity crossing the ref arm")
	}

	inCache := values.NewInteger(7)
	if environment.Box(environment.Unbox(inCache)) != values.Value(inCache) {
		t.Errorf("in-cache integer lost pointer identity; the flyweight should return the same object")
	}
}

// TestZeroCellIsRef pins that the zero Cell reads as a nil ref rather than the
// float 0.0. An uninitialized slot must not be indistinguishable from a
// legitimately-stored zero: the tag ordering (CellRef == 0) is what makes a
// forgotten initialization a nil-deref rather than silent wrong arithmetic.
func TestZeroCellIsRef(t *testing.T) {
	var c environment.Cell
	if c.Tag() != environment.CellRef {
		t.Errorf("zero Cell tag = %d, want CellRef (%d)", c.Tag(), environment.CellRef)
	}
	if c.Ref() != nil {
		t.Errorf("zero Cell ref = %v, want nil", c.Ref())
	}
}

// TestCellConstructorsSetTagAndPayload covers the direct constructors, which the
// VM uses when a scalar enters the lane from a literal or a computation rather
// than from an existing values.Value.
func TestCellConstructorsSetTagAndPayload(t *testing.T) {
	f := environment.FloatCell(1.5)
	if f.Tag() != environment.CellFloat || f.Float() != 1.5 {
		t.Errorf("FloatCell(1.5) = {tag %d, %v}, want {CellFloat, 1.5}", f.Tag(), f.Float())
	}

	i := environment.FixnumCell(-9)
	if i.Tag() != environment.CellFixnum || i.Fixnum() != -9 {
		t.Errorf("FixnumCell(-9) = {tag %d, %v}, want {CellFixnum, -9}", i.Tag(), i.Fixnum())
	}

	sym := values.NewSymbol("x")
	r := environment.RefCell(sym)
	if r.Tag() != environment.CellRef || r.Ref() != values.Value(sym) {
		t.Errorf("RefCell(sym) = {tag %d, %v}, want {CellRef, sym}", r.Tag(), r.Ref())
	}
}

// TestCellSizeIsThirtyTwoBytes pins the layout claim in cell.go. The bits lane
// is shared by the two scalar arms specifically to keep this at 32 rather than
// the 40 that separate float64 and int64 fields would cost; if a future field
// pushes it wider, the size argument in the doc comment needs rewriting rather
// than silently becoming false.
func TestCellSizeIsThirtyTwoBytes(t *testing.T) {
	got := unsafe.Sizeof(environment.Cell{})
	if got != 32 {
		t.Errorf("unsafe.Sizeof(Cell{}) = %d, want 32", got)
	}
}
