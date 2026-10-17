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

package parser

import (
	"context"
	"math/big"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

// longDigits is 100 significant digits. It ends in '1', not '0': BigFloat
// renders with Text('g', -1), the shortest unique form, which drops a trailing
// zero, so a literal ending in '0' would fail a textual round trip about 9% of
// the time for a reason unrelated to the precision under test.
const longDigits = "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567891"

func readBigFloat(c *qt.C, src string) *values.BigFloat {
	env := environment.NewNamespace().Runtime()
	syn, err := NewParser(env, true, strings.NewReader(src)).ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	bf, ok := syn.Unwrap().(*values.BigFloat)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected BigFloat for %q, got %T", src, syn.Unwrap()))
	return bf
}

// TestLongPrecisionIntegerLiteralIsExact is the value oracle for I192-R6b: a
// 100-digit #m integer literal must denote exactly that integer. At the fixed
// 256-bit precision it lost the low ~22 digits. P6.13's decimal-magnitude
// threshold must stay above 1e99 or this pin goes red.
func TestLongPrecisionIntegerLiteralIsExact(t *testing.T) {
	c := qt.New(t)
	want, ok := new(big.Int).SetString(longDigits, 10)
	c.Assert(ok, qt.IsTrue)

	bf := readBigFloat(c, "#m"+longDigits)
	got, acc := bf.BigFloatValue().Int(nil)
	c.Assert(acc, qt.Equals, big.Exact)
	c.Assert(got.Cmp(want), qt.Equals, 0, qt.Commentf("got %v", got))
}

// TestLongPrecisionLiteralRoundTrips is I192-R6a: 100 significant digits in
// through #m, or through the R7RS l marker, come back out unchanged.
func TestLongPrecisionLiteralRoundTrips(t *testing.T) {
	const mantissa = "1." + longDigits
	const nine = "9." + longDigits
	const matula = "8.350121711170800908915733448512723488856342658525180108977596578509172053145559"
	tcs := []struct {
		name, src, want string
	}{
		{"hash-m", "#m" + mantissa, mantissa},
		{"l-marker", mantissa + "l0", mantissa},
		// float64 overflows, so this reaches NewBigFloatFromString via ErrRange.
		{"range-overflow", mantissa + "e400", mantissa + "l+400"},
		{"leading-9", "#m" + nine, nine},
		// 79 digits, leading 8: fails a +1 bit bound (Matula), found by crosscheck.
		{"matula-79", "#m" + matula, matula},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			bf := readBigFloat(c, tc.src)
			c.Assert(strings.HasPrefix(bf.SchemeString(), tc.want), qt.IsTrue,
				qt.Commentf("digits lost: %s", bf.SchemeString()))
		})
	}
}

// TestShortBigFloatLiteralKeepsDefaultPrecision pins the floor: the change only
// widens, so short literals and widened small operands stay at 256 bits.
func TestShortBigFloatLiteralKeepsDefaultPrecision(t *testing.T) {
	for _, src := range []string{"#m1.5", "#m5", "#m#z5", "#m#e5", "1.5l0", "#m#e5/3"} {
		t.Run(src, func(t *testing.T) {
			c := qt.New(t)
			bf := readBigFloat(c, src)
			c.Assert(bf.BigFloatValue().Prec(), qt.Equals, uint(values.DefaultBigFloatPrecision))
		})
	}
}

// TestWidenToBigFloatBigIntegerIsExact covers the introducer spelling: #m over
// a #z BigInteger datum widens at no less than the operand's bit length.
func TestWidenToBigFloatBigIntegerIsExact(t *testing.T) {
	c := qt.New(t)
	want, ok := new(big.Int).SetString(longDigits, 10)
	c.Assert(ok, qt.IsTrue)
	bf := readBigFloat(c, "#m#z"+longDigits)
	got, acc := bf.BigFloatValue().Int(nil)
	c.Assert(acc, qt.Equals, big.Exact)
	c.Assert(got.Cmp(want), qt.Equals, 0)
}

// TestWidenToBigFloatRationalKeepsOperandBits: a ratio over a 100-digit
// denominator must widen at no fewer bits than the denominator (or numerator).
func TestWidenToBigFloatRationalKeepsOperandBits(t *testing.T) {
	bits := uint(len(longDigits)) * 332 / 100
	for _, src := range []string{"#m#e1/" + longDigits, "#m#e" + longDigits + "/7"} {
		t.Run(src[:12], func(t *testing.T) {
			c := qt.New(t)
			bf := readBigFloat(c, src)
			c.Assert(bf.BigFloatValue().Prec() >= bits, qt.IsTrue, qt.Commentf("prec %d", bf.BigFloatValue().Prec()))
		})
	}
}

// TestLongPrecisionLiteralPastCapIsRefused: derived precision is bounded, and
// past the bound the reader errors instead of truncating.
func TestLongPrecisionLiteralPastCapIsRefused(t *testing.T) {
	over := strings.Repeat("7", 20000)
	for _, src := range []string{"#m" + over, over + ".5l0", "#m#z" + over, "#m#e1/" + over} {
		t.Run(src[:8], func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewNamespace().Runtime()
			_, err := NewParser(env, true, strings.NewReader(src)).ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNotNil)
		})
	}
	c := qt.New(t)
	c.Assert(readBigFloat(c, "#m"+strings.Repeat("7", 19000)), qt.IsNotNil)
}
