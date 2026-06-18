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
	"math"
	"math/big"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
)

func TestEqv(t *testing.T) {
	c := qt.New(t)

	// Helper values for identity tests. These are declared here so we can
	// take the address and verify pointer-identity (the a == b fast path).
	sharedInteger := values.NewInteger(100000)
	sharedString := values.NewString("shared")
	sharedFloat := values.NewFloat(3.14)

	tcs := []struct {
		name string
		a    values.Value
		b    values.Value
		want bool
	}{
		// ── Identity (pointer equality fast path) ──────────────────────
		{
			name: "identity/same integer pointer",
			a:    sharedInteger,
			b:    sharedInteger,
			want: true,
		},
		{
			name: "identity/same string pointer",
			a:    sharedString,
			b:    sharedString,
			want: true,
		},
		{
			name: "identity/same float pointer",
			a:    sharedFloat,
			b:    sharedFloat,
			want: true,
		},
		{
			name: "identity/true singleton",
			a:    values.TrueValue,
			b:    values.TrueValue,
			want: true,
		},
		{
			name: "identity/false singleton",
			a:    values.FalseValue,
			b:    values.FalseValue,
			want: true,
		},
		{
			name: "identity/cached small integer",
			a:    values.NewInteger(42),
			b:    values.NewInteger(42),
			want: true, // cached range -32768..32767, same pointer
		},

		// ── Integer == Integer ──────────────────────────────────────────
		{
			name: "integer/same value outside cache",
			a:    values.NewInteger(100000),
			b:    values.NewInteger(100000),
			want: true,
		},
		{
			name: "integer/different values",
			a:    values.NewInteger(1),
			b:    values.NewInteger(2),
			want: false,
		},
		{
			name: "integer/zero and zero",
			a:    values.NewInteger(0),
			b:    values.NewInteger(0),
			want: true,
		},
		{
			name: "integer/negative",
			a:    values.NewInteger(-42),
			b:    values.NewInteger(-42),
			want: true,
		},
		{
			name: "integer/min int64",
			a:    values.NewInteger(math.MinInt64),
			b:    values.NewInteger(math.MinInt64),
			want: true,
		},

		// ── BigInteger == BigInteger ────────────────────────────────────
		{
			name: "bigint/same value",
			a:    values.NewBigIntegerFromInt64(999999999999),
			b:    values.NewBigIntegerFromInt64(999999999999),
			want: true,
		},
		{
			name: "bigint/different values",
			a:    values.NewBigIntegerFromInt64(111),
			b:    values.NewBigIntegerFromInt64(222),
			want: false,
		},
		{
			name: "bigint/truly big values equal",
			a:    values.NewBigInteger(new(big.Int).Exp(big.NewInt(2), big.NewInt(128), nil)),
			b:    values.NewBigInteger(new(big.Int).Exp(big.NewInt(2), big.NewInt(128), nil)),
			want: true,
		},

		// ── Integer == BigInteger (cross-type exact) ───────────────────
		{
			name: "int-bigint/same numeric value",
			a:    values.NewInteger(42),
			b:    values.NewBigIntegerFromInt64(42),
			want: true,
		},
		{
			name: "int-bigint/different numeric value",
			a:    values.NewInteger(42),
			b:    values.NewBigIntegerFromInt64(99),
			want: false,
		},
		{
			name: "int-bigint/zero",
			a:    values.NewInteger(0),
			b:    values.NewBigIntegerFromInt64(0),
			want: true,
		},
		{
			name: "int-bigint/negative",
			a:    values.NewInteger(-7),
			b:    values.NewBigIntegerFromInt64(-7),
			want: true,
		},

		// ── BigInteger == Integer (symmetry) ───────────────────────────
		{
			name: "bigint-int/same numeric value",
			a:    values.NewBigIntegerFromInt64(42),
			b:    values.NewInteger(42),
			want: true,
		},
		{
			name: "bigint-int/different numeric value",
			a:    values.NewBigIntegerFromInt64(99),
			b:    values.NewInteger(42),
			want: false,
		},

		// ── Float == Float ─────────────────────────────────────────────
		{
			name: "float/same value",
			a:    values.NewFloat(3.14),
			b:    values.NewFloat(3.14),
			want: true,
		},
		{
			name: "float/different values",
			a:    values.NewFloat(1.0),
			b:    values.NewFloat(2.0),
			want: false,
		},
		{
			name: "float/positive zero",
			a:    values.NewFloat(0.0),
			b:    values.NewFloat(0.0),
			want: true,
		},
		{
			name: "float/negative zero",
			a:    values.NewFloat(math.Copysign(0, -1)),
			b:    values.NewFloat(math.Copysign(0, -1)),
			want: true,
		},
		{
			name: "float/positive infinity",
			a:    values.NewFloat(math.Inf(1)),
			b:    values.NewFloat(math.Inf(1)),
			want: true,
		},
		{
			name: "float/negative infinity",
			a:    values.NewFloat(math.Inf(-1)),
			b:    values.NewFloat(math.Inf(-1)),
			want: true,
		},
		{
			name: "float/NaN not equal to NaN",
			a:    values.NewFloat(math.NaN()),
			b:    values.NewFloat(math.NaN()),
			want: false, // IEEE 754: NaN != NaN
		},
		{
			name: "float/positive vs negative zero",
			a:    values.NewFloat(0.0),
			b:    values.NewFloat(math.Copysign(0, -1)),
			want: true, // Go: 0.0 == -0.0
		},

		// ── BigFloat == BigFloat ───────────────────────────────────────
		{
			name: "bigfloat/same value",
			a:    values.NewBigFloatFromFloat64(3.14),
			b:    values.NewBigFloatFromFloat64(3.14),
			want: true,
		},
		{
			name: "bigfloat/different values",
			a:    values.NewBigFloatFromFloat64(1.0),
			b:    values.NewBigFloatFromFloat64(2.0),
			want: false,
		},
		{
			name: "bigfloat/high precision equal",
			a:    values.NewBigFloat(new(big.Float).SetPrec(256).SetFloat64(1.0)),
			b:    values.NewBigFloat(new(big.Float).SetPrec(256).SetFloat64(1.0)),
			want: true,
		},
		// E2: NaN is not equal to anything, including itself. BigFloat stores NaN
		// as a separate flag; without a guard, Cmp on the zero backing *big.Float
		// returns 0 (incorrectly treating NaN == NaN as true).
		{
			name: "bigfloat/NaN not equal to NaN",
			a:    values.NewBigFloatNaN(),
			b:    values.NewBigFloatNaN(),
			want: false,
		},
		{
			name: "bigfloat/NaN not equal to finite",
			a:    values.NewBigFloatNaN(),
			b:    values.NewBigFloatFromFloat64(1.0),
			want: false,
		},
		{
			name: "bigfloat/finite not equal to NaN",
			a:    values.NewBigFloatFromFloat64(1.0),
			b:    values.NewBigFloatNaN(),
			want: false,
		},

		// ── Rational == Rational ───────────────────────────────────────
		{
			name: "rational/same value",
			a:    values.NewRational(1, 2),
			b:    values.NewRational(1, 2),
			want: true,
		},
		{
			name: "rational/equivalent fractions",
			a:    values.NewRational(2, 4),
			b:    values.NewRational(1, 2),
			want: true, // both normalize to 1/2
		},
		{
			name: "rational/different values",
			a:    values.NewRational(1, 3),
			b:    values.NewRational(2, 3),
			want: false,
		},

		// ── Complex == Complex ─────────────────────────────────────────
		{
			name: "complex/same value",
			a:    values.NewComplex(complex(1, 2)),
			b:    values.NewComplex(complex(1, 2)),
			want: true,
		},
		{
			name: "complex/different real part",
			a:    values.NewComplex(complex(1, 2)),
			b:    values.NewComplex(complex(3, 2)),
			want: false,
		},
		{
			name: "complex/different imaginary part",
			a:    values.NewComplex(complex(1, 2)),
			b:    values.NewComplex(complex(1, 4)),
			want: false,
		},
		{
			name: "complex/zero",
			a:    values.NewComplex(complex(0, 0)),
			b:    values.NewComplex(complex(0, 0)),
			want: true,
		},

		// ── BigComplex == BigComplex ───────────────────────────────────
		{
			name: "bigcomplex/same value",
			a:    values.NewBigComplexFromBigIntegers(values.NewBigIntegerFromInt64(1), values.NewBigIntegerFromInt64(2)),
			b:    values.NewBigComplexFromBigIntegers(values.NewBigIntegerFromInt64(1), values.NewBigIntegerFromInt64(2)),
			want: true,
		},
		{
			name: "bigcomplex/different real part",
			a:    values.NewBigComplexFromBigIntegers(values.NewBigIntegerFromInt64(1), values.NewBigIntegerFromInt64(2)),
			b:    values.NewBigComplexFromBigIntegers(values.NewBigIntegerFromInt64(3), values.NewBigIntegerFromInt64(2)),
			want: false,
		},
		{
			name: "bigcomplex/different imaginary part",
			a:    values.NewBigComplexFromBigIntegers(values.NewBigIntegerFromInt64(1), values.NewBigIntegerFromInt64(2)),
			b:    values.NewBigComplexFromBigIntegers(values.NewBigIntegerFromInt64(1), values.NewBigIntegerFromInt64(4)),
			want: false,
		},
		{
			name: "bigcomplex/bigfloat parts equal",
			a:    values.NewBigComplexFromBigFloats(values.NewBigFloatFromFloat64(1.5), values.NewBigFloatFromFloat64(2.5)),
			b:    values.NewBigComplexFromBigFloats(values.NewBigFloatFromFloat64(1.5), values.NewBigFloatFromFloat64(2.5)),
			want: true,
		},

		// ── Character == Character ─────────────────────────────────────
		{
			name: "character/same value",
			a:    values.NewCharacter('A'),
			b:    values.NewCharacter('A'),
			want: true,
		},
		{
			name: "character/different values",
			a:    values.NewCharacter('A'),
			b:    values.NewCharacter('B'),
			want: false,
		},
		{
			name: "character/unicode",
			a:    values.NewCharacter('\u03BB'),
			b:    values.NewCharacter('\u03BB'),
			want: true, // lambda
		},

		// ── Cross-type mismatches (expect false) ───────────────────────
		{
			name: "mismatch/integer vs float",
			a:    values.NewInteger(1),
			b:    values.NewFloat(1.0),
			want: false,
		},
		{
			name: "mismatch/float vs integer",
			a:    values.NewFloat(1.0),
			b:    values.NewInteger(1),
			want: false,
		},
		{
			name: "mismatch/integer vs string",
			a:    values.NewInteger(1),
			b:    values.NewString("1"),
			want: false,
		},
		{
			name: "mismatch/integer vs boolean",
			a:    values.NewInteger(1),
			b:    values.TrueValue,
			want: false,
		},
		{
			name: "mismatch/integer vs character",
			a:    values.NewInteger(65),
			b:    values.NewCharacter('A'),
			want: false,
		},
		{
			name: "mismatch/integer vs rational",
			a:    values.NewInteger(1),
			b:    values.NewRational(1, 1),
			want: false,
		},
		{
			name: "mismatch/integer vs complex",
			a:    values.NewInteger(1),
			b:    values.NewComplex(complex(1, 0)),
			want: false,
		},
		{
			name: "mismatch/float vs bigfloat",
			a:    values.NewFloat(1.0),
			b:    values.NewBigFloatFromFloat64(1.0),
			want: false,
		},
		{
			name: "mismatch/float vs rational",
			a:    values.NewFloat(0.5),
			b:    values.NewRational(1, 2),
			want: false,
		},
		{
			name: "mismatch/complex vs bigcomplex",
			a:    values.NewComplex(complex(1, 2)),
			b:    values.NewBigComplexFromBigIntegers(values.NewBigIntegerFromInt64(1), values.NewBigIntegerFromInt64(2)),
			want: false,
		},
		{
			name: "mismatch/string vs symbol",
			a:    values.NewString("foo"),
			b:    values.NewSymbol("foo"),
			want: false,
		},
		{
			name: "mismatch/character vs integer",
			a:    values.NewCharacter('A'),
			b:    values.NewInteger(65),
			want: false,
		},
		{
			name: "mismatch/bigint vs float",
			a:    values.NewBigIntegerFromInt64(1),
			b:    values.NewFloat(1.0),
			want: false,
		},
		{
			name: "mismatch/bigint vs rational",
			a:    values.NewBigIntegerFromInt64(1),
			b:    values.NewRational(1, 1),
			want: false,
		},
		{
			name: "mismatch/rational vs bigfloat",
			a:    values.NewRational(1, 2),
			b:    values.NewBigFloatFromFloat64(0.5),
			want: false,
		},

		// ── Non-numeric types (no type-switch case, fall through) ──────
		{
			name: "fallthrough/string vs string same value different pointer",
			a:    values.NewMutableString("hello"),
			b:    values.NewMutableString("hello"),
			want: false, // mutable strings are never interned; Eqv has no string case
		},
		{
			name: "fallthrough/boolean true vs false",
			a:    values.TrueValue,
			b:    values.FalseValue,
			want: false,
		},
		{
			name: "symbol/same name different pointer",
			a:    values.NewSymbol("foo"),
			b:    values.NewSymbol("foo"),
			want: true, // symbols compare by name, not pointer identity
		},

		// ── Nil handling ───────────────────────────────────────────────
		{
			name: "nil/both nil",
			a:    nil,
			b:    nil,
			want: true, // Go interface nil == nil
		},
		{
			name: "nil/a nil b integer",
			a:    nil,
			b:    values.NewInteger(0),
			want: false,
		},
		{
			name: "nil/a integer b nil",
			a:    values.NewInteger(0),
			b:    nil,
			want: false,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got := Eqv(tc.a, tc.b)
			c.Assert(got, qt.Equals, tc.want)
		})
	}
}

func TestEqIdentity(t *testing.T) {
	c := qt.New(t)

	sym1 := values.NewSymbol("foo")
	sym2 := values.NewSymbol("foo")
	sym3 := values.NewSymbol("bar")
	int1 := values.NewInteger(42)
	int2 := values.NewInteger(99999)

	tcs := []struct {
		name string
		a    values.Value
		b    values.Value
		want bool
	}{
		{
			name: "same symbol pointer",
			a:    sym1,
			b:    sym1,
			want: true,
		},
		{
			name: "different symbol pointers same name",
			a:    sym1,
			b:    sym2,
			want: true,
		},
		{
			name: "different symbol names",
			a:    sym1,
			b:    sym3,
			want: false,
		},
		{
			name: "symbol vs non-symbol",
			a:    sym1,
			b:    int1,
			want: false,
		},
		{
			name: "non-symbol vs symbol",
			a:    int1,
			b:    sym1,
			want: false,
		},
		{
			name: "same integer pointer",
			a:    int1,
			b:    int1,
			want: true,
		},
		{
			name: "different integer pointers different value",
			a:    int1,
			b:    int2,
			want: false,
		},
		{
			name: "true true",
			a:    values.TrueValue,
			b:    values.TrueValue,
			want: true,
		},
		{
			name: "true false",
			a:    values.TrueValue,
			b:    values.FalseValue,
			want: false,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got := EqIdentity(tc.a, tc.b)
			c.Assert(got, qt.Equals, tc.want)
		})
	}
}
