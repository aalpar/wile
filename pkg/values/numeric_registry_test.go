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
	"math"
	"math/big"
	"reflect"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/werr"
)

// TestNumericRegistryAllKindsRegistered verifies that every NumericKind has a
// complete spec entry after package initialization: non-empty schemeName and
// non-nil function fields. The reflection-based field walk mirrors the
// completeness check in TestAllDispatchEntriesPopulated.
func TestNumericRegistryAllKindsRegistered(t *testing.T) {
	c := qt.New(t)
	funcFieldNames := []string{"simplifyDown", "toFloat64WithAccuracy", "toComplex128WithAccuracy"}
	for k := range numKinds {
		spec := LookupNumericSpec(k)
		c.Assert(spec.SchemeName(), qt.Not(qt.Equals), "",
			qt.Commentf("kind %d has empty schemeName", k))
		sv := reflect.ValueOf(*spec)
		for _, name := range funcFieldNames {
			f := sv.FieldByName(name)
			c.Assert(f.IsValid(), qt.IsTrue, qt.Commentf("kind %d: field %s missing", k, name))
			c.Assert(f.IsNil(), qt.IsFalse,
				qt.Commentf("kind %d (%s): function field %s is nil", k, spec.SchemeName(), name))
		}
	}
}

// TestNumericRegistrySmoke drives a representative exemplar for each kind
// through SimplifyDown, ToFloat64, and ToComplex128 and asserts no panic
// and sensible (non-NaN) results where expected.
func TestNumericRegistrySmoke(t *testing.T) {
	cases := []struct {
		name           string
		value          Number
		wantFloat64    float64
		wantComplex    complex128
		expectF64NotOK bool
	}{
		{
			name:        "Integer(3)",
			value:       NewInteger(3),
			wantFloat64: 3,
			wantComplex: complex(3, 0),
		},
		{
			name:        "BigInteger(3)",
			value:       NewBigIntegerFromInt64(3),
			wantFloat64: 3,
			wantComplex: complex(3, 0),
		},
		{
			name:        "Float(3.5)",
			value:       NewFloat(3.5),
			wantFloat64: 3.5,
			wantComplex: complex(3.5, 0),
		},
		{
			name:        "BigFloat(3.5)",
			value:       NewBigFloatFromFloat64(3.5),
			wantFloat64: 3.5,
			wantComplex: complex(3.5, 0),
		},
		{
			name:        "Rational(7/2)",
			value:       NewRational(7, 2),
			wantFloat64: 3.5,
			wantComplex: complex(3.5, 0),
		},
		{
			name:           "Complex(3+4i)",
			value:          NewComplex(complex(3, 4)),
			expectF64NotOK: true,
			wantComplex:    complex(3, 4),
		},
		{
			name:        "Complex(3+0i)",
			value:       NewComplex(complex(3, 0)),
			wantFloat64: 3,
			wantComplex: complex(3, 0),
		},
		{
			name:           "BigComplex(3,4)",
			value:          NewBigComplex(NewBigIntegerFromInt64(3), NewBigIntegerFromInt64(4)),
			expectF64NotOK: true,
			wantComplex:    complex(3, 4),
		},
		{
			name:        "BigComplex(3,0)",
			value:       NewBigComplex(NewBigIntegerFromInt64(3), NewBigIntegerFromInt64(0)),
			wantFloat64: 3,
			wantComplex: complex(3, 0),
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			spec := LookupNumericSpec(tc.value.Kind())

			// SimplifyDown must return a non-nil Number of the same or
			// simpler kind. Returning the input unchanged is allowed
			// (identity step for bottom-of-chain kinds).
			simplified := spec.SimplifyDown(tc.value)
			c.Assert(simplified, qt.Not(qt.IsNil),
				qt.Commentf("SimplifyDown(%s) returned nil", tc.name))

			// ToFloat64WithAccuracy
			f, _, ok := spec.ToFloat64WithAccuracy(tc.value)
			if tc.expectF64NotOK {
				c.Assert(ok, qt.IsFalse,
					qt.Commentf("ToFloat64WithAccuracy(%s) expected ok=false for complex with nonzero imag", tc.name))
			} else {
				c.Assert(ok, qt.IsTrue,
					qt.Commentf("ToFloat64WithAccuracy(%s) unexpected ok=false", tc.name))
				c.Assert(math.IsNaN(f), qt.IsFalse,
					qt.Commentf("ToFloat64WithAccuracy(%s) returned NaN", tc.name))
				c.Assert(f, qt.Equals, tc.wantFloat64)
			}

			// ToComplex128WithAccuracy is universal.
			res := spec.ToComplex128WithAccuracy(tc.value)
			c.Assert(real(res.Value), qt.Equals, real(tc.wantComplex))
			c.Assert(imag(res.Value), qt.Equals, imag(tc.wantComplex))
		})
	}
}

// TestEnsureNumericRegistryInitPanics verifies that validateNumericSpecs
// panics with ErrNumericRegistry when given incomplete or zero-filled arrays.
// Tests the validator itself, not just the live registry state.
func TestEnsureNumericRegistryInitPanics(t *testing.T) {
	t.Run("missing kind", func(t *testing.T) {
		var specs [numKinds]NumericTypeSpec
		var filled [numKinds]bool
		// Leave all entries zero/false — every kind is "missing".
		c := qt.New(t)
		c.Assert(func() { validateNumericSpecs(specs, filled) }, qt.PanicMatches, ".*numeric registry violation.*")
	})

	t.Run("partial: one missing kind", func(t *testing.T) {
		var specs [numKinds]NumericTypeSpec
		var filled [numKinds]bool
		id := func(n Number) Number { return n }
		f64 := func(n Number) (float64, big.Accuracy, bool) { return 0, big.Exact, true }
		c128 := func(n Number) Complex128Result { return Complex128Result{} }
		// Fill all but KindBigComplex.
		for k := range numKinds {
			if k == KindBigComplex {
				continue
			}
			specs[k] = NumericTypeSpec{
				schemeName: "x", simplifyDown: id, toFloat64WithAccuracy: f64, toComplex128WithAccuracy: c128,
			}
			filled[k] = true
		}
		c := qt.New(t)
		c.Assert(func() { validateNumericSpecs(specs, filled) }, qt.PanicMatches, ".*numeric registry violation.*")
	})
}

// TestRegisterNumericSpecDuplicateRejected verifies that registering the
// same kind twice panics. Tests two kinds to guard against KindInteger==0
// zero-value false negatives.
func TestRegisterNumericSpecDuplicateRejected(t *testing.T) {
	id := func(n Number) Number { return n }
	f64 := func(n Number) (float64, big.Accuracy, bool) { return 0, big.Exact, true }
	c128 := func(n Number) Complex128Result { return Complex128Result{} }
	spec := NumericTypeSpec{
		schemeName: "test", simplifyDown: id, toFloat64WithAccuracy: f64, toComplex128WithAccuracy: c128,
	}

	for _, kind := range []NumericKind{KindInteger, KindBigComplex} {
		t.Run(LookupNumericSpec(kind).SchemeName(), func(t *testing.T) {
			c := qt.New(t)
			// The kind is already registered from package init() — a second call must panic.
			c.Assert(func() { registerNumericSpec(kind, spec) }, qt.PanicMatches, ".*numeric registry violation.*")
		})
	}
}

// --- Behavioral-equivalence golden tests ---
//
// These tests capture the pre-migration (switch-based) output for a fixed
// roster of numeric exemplars and assert that the new registry path produces
// identical results.

// simplifyGolden is the pre-migration Simplify logic captured inline.
// It mirrors exactly the 7-arm switch in the original Simplify function
// so we can compare old vs. new behavior on every exemplar.
func simplifyGolden(n Number) Number {
	switch v := n.(type) {
	case *BigComplex:
		// EXACT zero imag only. An inexact 0.0 imaginary part does not make a
		// number real (R7RS §6.2.6), and descending on it would also flip the
		// exactness class. Spelled out rather than calling IsReal(), so this stays
		// an independent restatement of the rule instead of a mirror of the code
		// under test.
		if v.Imag().IsZero() && v.Imag().IsExact() {
			return simplifyGolden(v.Real())
		}
	// *Complex never descends: its parts are float64, so a zero imag is always an
	// INEXACT zero. There is no exact-zero-imag *Complex to descend from.
	// The INEXACT tier does not descend, for the same reason the BigComplex arm
	// above refuses an inexact zero imaginary: Simplify must not change a number's
	// exactness class. An integer-valued 2.0 is still inexact, and returning
	// Integer(2) for it would make (exact? 2.0) answer #t.
	//
	// These two arms used to demote, and so did the code, so this golden agreed
	// with the implementation while both contradicted the rule the arm three lines
	// up spells out. A golden that mirrors the code instead of restating the rule
	// cannot catch the code being wrong.
	case *BigFloat, *Float:
		return n
	case *Rational:
		if v.IsInteger() {
			return simplifyGolden(&BigInteger{value: new(big.Int).Set(v.Num())})
		}
	case *BigInteger:
		if v.value.IsInt64() {
			return NewInteger(v.value.Int64())
		}
	}
	return n
}

// exactnessOfGolden is the pre-migration ExactnessOf logic.
func exactnessOfGolden(n Number) Exactness {
	switch v := n.(type) {
	case *Integer, *BigInteger, *Rational:
		return Exact
	case *Float, *BigFloat, *Complex:
		return Inexact
	case *BigComplex:
		if v.IsExact() {
			return Exact
		}
		return Inexact
	}
	panic(werr.WrapForeignErrorf(werr.ErrNotANumber, "exactnessOfGolden: unsupported type %T", n))
}

// numberToFloat64Golden is the pre-migration NumberToFloat64 logic.
func numberToFloat64Golden(n Number) float64 {
	switch v := n.(type) {
	case *Integer:
		return float64(v.Value)
	case *BigInteger:
		return float64FromBigInt(v.value)
	case *Float:
		return v.Value
	case *BigFloat:
		f, _ := v.value.Float64()
		return f
	case *Rational:
		return v.Float64Truncated()
	case *Complex:
		return real(v.Value)
	case *BigComplex:
		return toBigFloat(v.real).Float64Truncated()
	}
	panic(werr.WrapForeignErrorf(werr.ErrNotANumber, "numberToFloat64Golden: unsupported type %T", n))
}

// numberToComplex128Golden is the pre-migration NumberToComplex128 logic.
func numberToComplex128Golden(n Number) complex128 {
	switch v := n.(type) {
	case *Integer:
		return complex(float64(v.Value), 0)
	case *BigInteger:
		return complex(float64FromBigInt(v.value), 0)
	case *Float:
		return complex(v.Value, 0)
	case *BigFloat:
		return complex(v.Float64Truncated(), 0)
	case *Rational:
		return complex(v.Float64Truncated(), 0)
	case *Complex:
		return v.Value
	case *BigComplex:
		return complex(toBigFloat(v.real).Float64Truncated(), toBigFloat(v.imag).Float64Truncated())
	}
	panic(werr.WrapForeignErrorf(werr.ErrNotANumber, "numberToComplex128Golden: unsupported type %T", n))
}

// equivalenceExemplars covers all 7 kinds with boundary cases that exercise
// the corners of each per-kind helper (overflow, magnitude limits, IEEE 754
// specials, complex with zero imag).
func equivalenceExemplars() []Number {
	bigBeyondInt64 := new(big.Int).Lsh(big.NewInt(1), 100) // 2^100, beyond int64 and beyond float64 mantissa
	return []Number{
		// Integer corners
		NewInteger(3),
		NewInteger(0),
		NewInteger(math.MinInt64),
		NewInteger(math.MaxInt64),
		// BigInteger corners
		NewBigIntegerFromInt64(3),
		NewBigIntegerFromInt64(math.MaxInt64),
		NewBigInteger(bigBeyondInt64),                   // does not fit int64
		NewBigInteger(new(big.Int).Neg(bigBeyondInt64)), // negative, does not fit
		// Float corners
		NewFloat(3.0),                  // whole — simplifies to Integer
		NewFloat(3.5),                  // non-whole — stays Float
		NewFloat(math.Copysign(0, -1)), // negative zero (literal -0.0 is folded to 0.0 by Go)
		// BigFloat corners
		NewBigFloatFromFloat64(2.0), // whole — simplifies to Integer
		NewBigFloatFromFloat64(2.5), // non-whole — stays BigFloat
		// Rational corners
		NewRational(6, 2), // IsInteger → demotes
		NewRational(7, 2), // non-integer — stays Rational
		NewRational(1, 3), // not exactly representable in float64
		// Complex corners. A *Complex is always inexact, so a zero imag is an
		// INEXACT zero and none of these descend to a real (R7RS §6.2.6).
		NewComplex(3 + 0i),
		NewComplex(3.5 + 0i),
		NewComplex(3 + 4i),
		// BigComplex corners — exact and inexact mixes
		NewBigComplex(NewBigIntegerFromInt64(3), NewBigIntegerFromInt64(0)),
		NewBigComplex(NewBigIntegerFromInt64(3), NewBigIntegerFromInt64(4)),
		NewBigComplex(NewBigFloatFromFloat64(3.5), NewBigIntegerFromInt64(0)), // inexact real + EXACT zero imag: descends
		// Inexact zero imag: must NOT descend. This is the corner the old
		// magnitude-only rule got wrong, and no exemplar covered it.
		NewBigComplex(NewBigFloatFromFloat64(3.5), NewBigFloatFromFloat64(0.0)),
		NewBigComplex(NewBigFloatFromFloat64(5.0), NewBigFloatFromFloat64(math.Copysign(0, -1))),
	}
}

func TestSimplifyEquivalence(t *testing.T) {
	c := qt.New(t)
	for _, n := range equivalenceExemplars() {
		golden := simplifyGolden(n)
		got := Simplify(n)
		c.Assert(got.SchemeString(), qt.Equals, golden.SchemeString(),
			qt.Commentf("Simplify(%s): registry path != golden path", n.SchemeString()))
	}
}

func TestExactnessOfEquivalence(t *testing.T) {
	c := qt.New(t)
	for _, n := range equivalenceExemplars() {
		golden := exactnessOfGolden(n)
		got := ExactnessOf(n)
		c.Assert(got, qt.Equals, golden,
			qt.Commentf("ExactnessOf(%s): registry path != golden path", n.SchemeString()))
	}
}

// TestNumberToFloat64Equivalence compares registry path against the golden
// switch for the 5 real kinds; asserts ok=false for the 2 complex kinds with
// nonzero imaginary part.
func TestNumberToFloat64Equivalence(t *testing.T) {
	c := qt.New(t)
	for _, n := range equivalenceExemplars() {
		kind := n.Kind()
		spec := LookupNumericSpec(kind)
		f, _, ok := spec.ToFloat64WithAccuracy(n)
		// Complex/BigComplex: ok=false only when imag != 0; with imag == 0
		// the real part is returned losslessly (loss-signals design).
		if kind == KindComplex || kind == KindBigComplex {
			if hasNonzeroImag(n) {
				c.Assert(ok, qt.IsFalse,
					qt.Commentf("ToFloat64WithAccuracy(%s) expected ok=false for complex with nonzero imag", n.SchemeString()))
				continue
			}
			c.Assert(ok, qt.IsTrue,
				qt.Commentf("ToFloat64WithAccuracy(%s) unexpected ok=false for complex with zero imag", n.SchemeString()))
			c.Assert(f, qt.Equals, realPartOfComplex(n),
				qt.Commentf("ToFloat64WithAccuracy(%s): real-part extraction wrong", n.SchemeString()))
			continue
		}
		// For real kinds, registry must match golden switch.
		c.Assert(ok, qt.IsTrue)
		golden := numberToFloat64Golden(n)
		if math.IsNaN(golden) {
			c.Assert(math.IsNaN(f), qt.IsTrue,
				qt.Commentf("ToFloat64WithAccuracy(%s): expected NaN", n.SchemeString()))
		} else {
			c.Assert(f, qt.Equals, golden,
				qt.Commentf("ToFloat64WithAccuracy(%s): registry path != golden path", n.SchemeString()))
		}
	}
}

// hasNonzeroImag reports whether a complex-kind Number carries a non-zero
// imaginary component. Used by ToFloat64 equivalence assertions.
func hasNonzeroImag(n Number) bool {
	switch v := n.(type) {
	case *Complex:
		return imag(v.Value) != 0
	case *BigComplex:
		return !v.Imag().IsZero()
	}
	return false
}

// realPartOfComplex returns the real component of a Complex/BigComplex as
// float64. Caller must ensure the value is one of those types.
func realPartOfComplex(n Number) float64 {
	switch v := n.(type) {
	case *Complex:
		return real(v.Value)
	case *BigComplex:
		return toBigFloat(v.Real()).Float64Truncated()
	}
	return 0
}

func TestNumberToComplex128Equivalence(t *testing.T) {
	c := qt.New(t)
	for _, n := range equivalenceExemplars() {
		golden := numberToComplex128Golden(n)
		spec := LookupNumericSpec(n.Kind())
		got := spec.ToComplex128WithAccuracy(n)
		c.Assert(real(got.Value), qt.Equals, real(golden),
			qt.Commentf("ToComplex128WithAccuracy(%s) real: registry != golden", n.SchemeString()))
		c.Assert(imag(got.Value), qt.Equals, imag(golden),
			qt.Commentf("ToComplex128WithAccuracy(%s) imag: registry != golden", n.SchemeString()))
	}
}
