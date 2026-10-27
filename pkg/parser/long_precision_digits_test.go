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

// longDigits is 100 significant digits, past the ~77 that 256 bits hold.
const longDigits = "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567891"

func readBigFloat(c *qt.C, src string) *values.BigFloat {
	env := environment.NewNamespace().Runtime()
	syn, err := NewParser(env, true, strings.NewReader(src)).ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	bf, ok := syn.Unwrap().(*values.BigFloat)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected BigFloat for %q, got %T", src, syn.Unwrap()))
	return bf
}

// at256 is x rounded once to DefaultBigFloatPrecision, the reader's answer
// for every BigFloat spelling.
func at256(c *qt.C, x any) *big.Float {
	q := new(big.Float).SetPrec(values.DefaultBigFloatPrecision)
	switch x := x.(type) {
	case string:
		_, ok := q.SetString(x)
		c.Assert(ok, qt.IsTrue, qt.Commentf("%.40s", x))
	case *big.Int:
		q.SetInt(x)
	case *big.Rat:
		q.SetRat(x)
	}
	return q
}

// TestLongBigFloatLiteralRoundsTo256Bits pins the fixed precision class: a
// BigFloat literal longer than 256 bits hold rounds to 256 bits once, whatever
// its spelling. It used to grow its precision with the digit count, so it kept
// every digit but did not survive write/read (TestBigFloatWriteReadRoundTrip).
func TestLongBigFloatLiteralRoundsTo256Bits(t *testing.T) {
	const mantissa = "1." + longDigits
	const nine = "9." + longDigits
	over := strings.Repeat("7", 20000)
	tcs := []struct {
		name, src, want string
	}{
		{"hash-m integer", "#m" + longDigits, longDigits},
		{"hash-m", "#m" + mantissa, mantissa},
		{"l-marker", mantissa + "l0", mantissa},
		// float64 overflows, so this reaches NewBigFloatFromString via ErrRange.
		{"range-overflow", mantissa + "e400", mantissa + "e400"},
		{"leading-9", "#m" + nine, nine},
		// Past the former 65536-bit literal cap: rounds rather than refuses.
		{"hash-m past old cap", "#m" + over, over},
		{"l-marker past old cap", over + ".5l0", over + ".5"},
		{"trailing fraction zeros", "1." + strings.Repeat("0", 20000) + "l0", "1"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			bf := readBigFloat(c, tc.src)
			c.Assert(bf.BigFloatValue().Prec(), qt.Equals, uint(values.DefaultBigFloatPrecision))
			c.Assert(bf.BigFloatValue().Cmp(at256(c, tc.want)), qt.Equals, 0, qt.Commentf("got %s", bf.SchemeString()))
		})
	}
}

// TestShortBigFloatLiteralKeepsDefaultPrecision: short literals and widened
// small operands are at 256 bits too.
func TestShortBigFloatLiteralKeepsDefaultPrecision(t *testing.T) {
	for _, src := range []string{"#m1.5", "#m5", "#m#z5", "#m#e5", "1.5l0", "#m#e5/3"} {
		t.Run(src, func(t *testing.T) {
			c := qt.New(t)
			bf := readBigFloat(c, src)
			c.Assert(bf.BigFloatValue().Prec(), qt.Equals, uint(values.DefaultBigFloatPrecision))
		})
	}
}

// TestWidenToBigFloatWideExactRoundsTo256Bits covers the introducer spelling:
// #m over an exact operand wider than 256 bits rounds it to 256 bits, as
// inexact rounds one to a float64. It used to widen to the operand's bit length.
func TestWidenToBigFloatWideExactRoundsTo256Bits(t *testing.T) {
	n, ok := new(big.Int).SetString(longDigits, 10)
	qt.Assert(t, ok, qt.IsTrue)
	over, ok := new(big.Int).SetString(strings.Repeat("7", 20000), 10)
	qt.Assert(t, ok, qt.IsTrue)
	tcs := []struct {
		src  string
		want any
	}{
		{"#m#z" + longDigits, n},
		{"#m#e1/" + longDigits, new(big.Rat).SetFrac(big.NewInt(1), n)},
		{"#m#e" + longDigits + "/7", new(big.Rat).SetFrac(n, big.NewInt(7))},
		{"#m#z" + over.String(), over},
		{"#m#e1/" + over.String(), new(big.Rat).SetFrac(big.NewInt(1), over)},
	}
	for _, tc := range tcs {
		t.Run(tc.src[:12], func(t *testing.T) {
			c := qt.New(t)
			bf := readBigFloat(c, tc.src)
			c.Assert(bf.BigFloatValue().Prec(), qt.Equals, uint(values.DefaultBigFloatPrecision))
			c.Assert(bf.BigFloatValue().Cmp(at256(c, tc.want)), qt.Equals, 0)
		})
	}
}

// TestLongExactAndUnmarkedLiteralsStillRead: #e re-reads the text as a ratio,
// so it keeps every digit, and an unmarked decimal past float64 range is a
// 256-bit BigFloat (TestReaderBigFloatIsDefaultPrecision in pkg/wile).
func TestLongExactAndUnmarkedLiteralsStillRead(t *testing.T) {
	over := strings.Repeat("7", 20000)
	for _, src := range []string{"#e" + over + ".5", over + ".5", "#e1." + over + "e400"} {
		t.Run(src[:8], func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewNamespace().Runtime()
			_, err := NewParser(env, true, strings.NewReader(src)).ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)
		})
	}
}
