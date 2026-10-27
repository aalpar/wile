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
	"errors"
	"fmt"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

// TestReadSyntaxRadixDecimalFraction pins the first clause of the TODO "Reader
// fixes" item "Floats (including BigFloats): radix for floating point number
// reading set by radix tag (eg #d)".
//
// Today only #d works, and only because #d is the identity: the parser's
// TokenizerStateMarkerBase10 arm re-enters readSyntax on the following datum,
// while #b/#o/#x route to parseBaseWithExactness, which admits only integers
// and rationals. So "#x1.8" fails with "invalid base-16 integer: 1.8".
//
// This is an extension, not conformance. R7RS §7.1.1 defines ⟨decimal R⟩ only
// for R = 10, so a radix-prefixed fraction is outside the standard grammar; the
// behaviour pinned here is Racket's, where #x1.8 reads as 1.5. Values are
// chosen so the fractional digit is exactly representable in binary and the
// expected result is exact in float64 — a wrong radix produces a visibly
// different number, not a rounding difference.
//
// RED for every case except #d.
func TestReadSyntaxRadixDecimalFraction(t *testing.T) {
	tcs := []struct {
		input  string
		expect float64
	}{
		// 1 + 8/16, 1 + 4/8, 1 + 1/2 — the same value in three bases.
		{"#x1.8", 1.5},
		{"#o1.4", 1.5},
		{"#b1.1", 1.5},
		{"#d1.5", 1.5},
		// Digits above 9 must still be digits, not an exponent marker or a symbol.
		{"#xA.8", 10.5},
		{"#xa.8", 10.5},
		{"#xF.C", 15.75},
		// Sign and a leading/trailing dot, matching the decimal forms the
		// reader already accepts (".5", "-1.5").
		{"#x-1.8", -1.5},
		{"#x+1.8", 1.5},
		{"#x.8", 0.5},
		{"#b-.1", -0.5},
		// More than one fractional digit, so the place values must actually be
		// weighted by the radix rather than merely accepted.
		{"#b101.101", 5.625},
		{"#o7.77", 7.984375},
		{"#x2.44", 2.265625},
	}

	for _, tc := range tcs {
		t.Run(tc.input, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.input))

			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)

			obj := syn.Unwrap()
			// R7RS §6.2.4: a literal written with a decimal point is inexact.
			f, ok := obj.(*values.Float)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected Float, got %T: %v", obj, obj))
			c.Assert(f.Value, qt.Equals, tc.expect)
		})
	}
}

// TestReadSyntaxRadixDecimalFractionDigitsAreValidated guards the other half:
// the radix tag must constrain the fractional digit set too. Without this, an
// implementation that scanned decimal digits after the point would pass every
// case above (they all use in-range digits) while reading #b1.9 as 1.9.
//
// Each case pairs the out-of-radix literal with an in-radix control read in the
// same subtest, for the same reason as the #z guard: a lone "must be rejected"
// assertion passes today only because radix fractions are rejected wholesale.
func TestReadSyntaxRadixDecimalFractionDigitsAreValidated(t *testing.T) {
	tcs := []struct {
		bad string
		// control is the same prefix with digits inside the radix.
		control string
		expect  float64
	}{
		{"#b1.9", "#b1.1", 1.5},   // 9 is not a binary digit
		{"#o1.8", "#o1.4", 1.5},   // 8 is not an octal digit
		{"#b1.a", "#b1.01", 1.25}, // a is not a binary digit
	}

	for _, tc := range tcs {
		t.Run(tc.bad, func(t *testing.T) {
			c := qt.New(t)

			ctlEnv := environment.NewNamespace().Runtime()
			ctlSyn, err := NewParser(ctlEnv, true, strings.NewReader(tc.control)).ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil, qt.Commentf("control %q must read", tc.control))
			ctl, ok := ctlSyn.Unwrap().(*values.Float)
			c.Assert(ok, qt.IsTrue, qt.Commentf("control: expected Float, got %T", ctlSyn.Unwrap()))
			c.Assert(ctl.Value, qt.Equals, tc.expect)

			env := environment.NewNamespace().Runtime()
			syn, err := NewParser(env, true, strings.NewReader(tc.bad)).ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNotNil, qt.Commentf("out-of-radix digits must be rejected, got %v", syn))
		})
	}
}

// TestReadSyntaxRadixBigFloat pins the "(including BigFloats)" half of the same
// TODO item: the #m arbitrary-precision float prefix must honour a radix tag
// the same way #z must (see TestReadSyntaxBigIntegerWithRadix). Today #m is
// decimal-only — readBigNum scans base 10 unconditionally
// (tokenizer_numbers.go) and parseBigFloat hands the digits to
// NewBigFloatFromString, which is base 10.
//
// RED: today readBigNum stops at the '#' of the radix tag, so the token text is
// a bare "#m" and the parser reports "invalid big float: #m".
func TestReadSyntaxRadixBigFloat(t *testing.T) {
	tcs := []struct {
		input  string
		expect float64
	}{
		{"#m#x1.8", 1.5},
		{"#m#o1.4", 1.5},
		{"#m#b1.1", 1.5},
		{"#m#d1.5", 1.5},
		{"#m#b101.101", 5.625},
		{"#M#X-F.C", -15.75},
	}

	for _, tc := range tcs {
		t.Run(tc.input, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.input))

			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)

			obj := syn.Unwrap()
			bf, ok := obj.(*values.BigFloat)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected BigFloat, got %T: %v", obj, obj))
			c.Assert(bf.Float64Truncated(), qt.Equals, tc.expect)
		})
	}
}

// TestReadSyntaxRadixFractionPastFloat64RoundsTo256Bits pins the fixed
// precision class on a radix fraction past float64 range: it reads at
// DefaultBigFloatPrecision, as a decimal one does, so #x1<255 zeros>1.0
// (16^256+1, 1025 bits) rounds to 16^256 rather than keeping its digits.
func TestReadSyntaxRadixFractionPastFloat64RoundsTo256Bits(t *testing.T) {
	tcs := []struct{ prefix, digits, rounded string }{
		{"#x", "1" + strings.Repeat("0", 255) + "1", "1" + strings.Repeat("0", 256)},
		{"#b", "1" + strings.Repeat("0", 1024) + "1", "1" + strings.Repeat("0", 1025)},
		{"#o", "7" + strings.Repeat("0", 400) + "7", "7" + strings.Repeat("0", 401)},
		{"#x", "-F" + strings.Repeat("0", 300) + "F", "-F" + strings.Repeat("0", 301)},
		// 260 one bits round up, to nearest, to 16^265; truncation would not.
		{"#x", strings.Repeat("F", 65) + strings.Repeat("0", 200), "1" + strings.Repeat("0", 265)},
	}
	read := func(c *qt.C, s string) values.Value {
		env := environment.NewNamespace().Runtime()
		syn, err := NewParser(env, true, strings.NewReader(s)).ReadSyntax(context.TODO())
		c.Assert(err, qt.IsNil, qt.Commentf("%.40s", s))
		return syn.Unwrap()
	}
	for i, tc := range tcs {
		t.Run(fmt.Sprintf("%02d", i), func(t *testing.T) {
			c := qt.New(t)
			bf, ok := read(c, tc.prefix+tc.digits+".0").(*values.BigFloat)
			c.Assert(ok, qt.IsTrue)
			c.Assert(bf.BigFloatValue().Prec(), qt.Equals, uint(values.DefaultBigFloatPrecision))
			bi, ok := read(c, tc.prefix+tc.rounded).(*values.BigInteger)
			c.Assert(ok, qt.IsTrue)
			got, _ := bf.BigFloatValue().Int(nil)
			c.Assert(got.Cmp(bi.BigInt()), qt.Equals, 0)
		})
	}
}

// TestReadSyntaxRadixFractionRoundsToFloat64Once: an in-range radix fraction
// longer than 256 bits rounds to float64 once, from its exact value. Here the
// 2^-53 tie between 1 and 1+2^-52 is broken upward by a 2^-300 bit; parsing at
// 256 bits first would drop that bit and round the tie to even, 1.0.
func TestReadSyntaxRadixFractionRoundsToFloat64Once(t *testing.T) {
	c := qt.New(t)
	src := "#x1." + strings.Repeat("0", 13) + "8" + strings.Repeat("0", 60) + "1"
	env := environment.NewNamespace().Runtime()
	syn, err := NewParser(env, true, strings.NewReader(src)).ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	f, ok := syn.Unwrap().(*values.Float)
	c.Assert(ok, qt.IsTrue, qt.Commentf("%T", syn.Unwrap()))
	c.Assert(f.Value, qt.Equals, 1.0000000000000002)
}

// TestReadSyntaxExactRadixFractionKeepsItsDigits pins #e over a radix fraction:
// the digits, read exactly in their radix, are authoritative in either prefix
// order, as a decimal's are. Past float64 range the fraction alone reads as a
// 256-bit BigFloat, and converting that value dropped 16^256+1's trailing 1; in
// range the float64 dropped #x1.00000000000000001's (it read as 1).
func TestReadSyntaxExactRadixFractionKeepsItsDigits(t *testing.T) {
	past := "1" + strings.Repeat("0", 255) + "1"
	tcs := []struct{ src, want string }{
		{"#e#x" + past + ".0", "#x" + past},
		{"#x#e" + past + ".0", "#x" + past},
		{"#e#x1.00000000000000001", "#x100000000000000001/100000000000000000"},
		{"#x#e1.00000000000000001", "#x100000000000000001/100000000000000000"},
		{"#e#b-101.101", "-45/8"},
		{"#e#x1.8", "3/2"},
		{"#x#e10.0", "16"},
	}
	read := func(c *qt.C, s string) values.Value {
		env := environment.NewNamespace().Runtime()
		syn, err := NewParser(env, true, strings.NewReader(s)).ReadSyntax(context.TODO())
		c.Assert(err, qt.IsNil, qt.Commentf("%.40s", s))
		return syn.Unwrap()
	}
	for i, tc := range tcs {
		t.Run(fmt.Sprintf("%02d", i), func(t *testing.T) {
			c := qt.New(t)
			got := read(c, tc.src)
			want := read(c, tc.want)
			c.Assert(values.EqvNumber(got.(values.Number), want.(values.Number)), qt.IsTrue,
				qt.Commentf("%.40s read as %.60s", tc.src, got.SchemeString()))
		})
	}
}

// TestRadixFractionRefusesDecimalExponent: in base 2 or 8 'e' is not a digit,
// and big.ParseFloat would take it as a decimal exponent whose digits the
// precision count mistook for mantissa digits ("1e100" in base 2 read as
// 9.84e99). In base 16 'e' is a digit.
func TestRadixFractionRefusesDecimalExponent(t *testing.T) {
	for _, base := range []int{2, 8} {
		for _, text := range []string{"1e100", "1.1E1"} {
			_, err := ParseRealFloatStringWithBase(text, base)
			qt.Assert(t, errors.Is(err, werr.ErrInvalidFormat), qt.IsTrue, qt.Commentf("%s base %d: %v", text, base, err))
		}
	}
	n, err := ParseRealFloatStringWithBase("1e.8", 16)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, n.(*values.Float).Value, qt.Equals, 30.5)
}

// TestBigFloatExponentOverflowIsNamed: a well-formed literal whose exponent
// big.Float cannot hold is refused with that reason, not as malformed.
func TestBigFloatExponentOverflowIsNamed(t *testing.T) {
	for _, src := range []string{"#m1e999999999999", "1l999999999999", "1e999999999999"} {
		t.Run(src, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewNamespace().Runtime()
			_, err := NewParser(env, true, strings.NewReader(src)).ReadSyntax(context.TODO())
			c.Assert(errors.Is(err, werr.ErrInvalidFormat), qt.IsTrue, qt.Commentf("%v", err))
			c.Assert(err, qt.ErrorMatches, ".*exponent overflow.*")
		})
	}
}
