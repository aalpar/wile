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
	"fmt"
	"math/big"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

// TestReadSyntaxExponentMarkerPrecision pins the second clause of the TODO
// "Reader fixes" float item: "Exponent markers on read and output denote
// precision (s: Short, f: Single, d: Double, l: Long, e: Default)".
//
// Spec status: R7RS §6.2.5 makes this optional — "implementations may accept
// numerical constants written with an exponent marker that indicates the
// desired precision ... the letter s, f, d, or l ... can be used in place of
// e". The §7.1.1 grammar lists only e. R6RS §4.2.8 and R5RS §6.2.4 carry the
// same prose and put s/f/d/l in the grammar. All three add: "when fewer than
// four internal inexact representations exist, the four size specifications are
// mapped onto those available."
//
// Wile has two inexact representations, Float (float64) and BigFloat (256-bit),
// so the mapping is s/f/d/e -> Float, l -> BigFloat. Today all five markers are
// folded to 'e' by schemeutil.NormalizeExponentMarker and every literal becomes
// a Float, which is *conformant* under the "mapped onto those available" clause
// but discards the one distinction Wile can actually represent.
//
// RED for the l cases only. The s/f/d/e cases pass today and are kept as
// controls: they are what must NOT change when l is split out, and a fix that
// promoted every marker to BigFloat would satisfy the l cases alone.
func TestReadSyntaxExponentMarkerPrecision(t *testing.T) {
	tcs := []struct {
		input string
		// long is true when the marker asks for Wile's long representation.
		long   bool
		expect float64
	}{
		// e is the default precision: at least as much as double (R7RS §6.2.5).
		{"1e3", false, 1000},
		// short and single map onto double, the smallest representation Wile has.
		{"1s3", false, 1000},
		{"1f3", false, 1000},
		// double is Float exactly.
		{"1d3", false, 1000},
		// long is the only marker that names a representation distinct from the
		// default, so it is the only one that changes type.
		{"1l3", true, 1000},
		{"1.5l0", true, 1.5},
		{"-2.5l2", true, -250},
		{"+1.25l1", true, 12.5},
		// Markers are case-insensitive, as e/E already are.
		{"1L3", true, 1000},
		{"1D3", false, 1000},
	}

	for _, tc := range tcs {
		t.Run(tc.input, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.input))

			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)

			obj := syn.Unwrap()
			if tc.long {
				bf, ok := obj.(*values.BigFloat)
				c.Assert(ok, qt.IsTrue, qt.Commentf("marker l asks for long precision: expected BigFloat, got %T: %v", obj, obj))
				c.Assert(bf.Float64Truncated(), qt.Equals, tc.expect)
				return
			}
			f, ok := obj.(*values.Float)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected Float, got %T: %v", obj, obj))
			c.Assert(f.Value, qt.Equals, tc.expect)
		})
	}
}

// TestReadSyntaxLongPrecisionMarkerPreservesDigits is the discriminator behind
// the type split: l has to buy actual precision, not just a different type tag.
// The same mantissa is read twice, once with d and once with l, and the two
// must disagree: d rounds to the nearest float64, l to 256 bits, which keeps
// all 29 digits.
//
// Without this, an implementation that routed l to BigFloat *after* parsing
// through float64 would satisfy TestReadSyntaxExponentMarkerPrecision while
// silently having thrown the extra digits away.
//
// RED: today the l form reads as Float 1.2345678901234567.
func TestReadSyntaxLongPrecisionMarkerPreservesDigits(t *testing.T) {
	const mantissa = "1.2345678901234567890123456789"

	c := qt.New(t)

	longEnv := environment.NewNamespace().Runtime()
	longSyn, err := NewParser(longEnv, true, strings.NewReader(mantissa+"l0")).ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)

	bf, ok := longSyn.Unwrap().(*values.BigFloat)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected BigFloat, got %T: %v", longSyn.Unwrap(), longSyn.Unwrap()))

	want, _, err := big.ParseFloat(mantissa, 10, values.DefaultBigFloatPrecision, big.ToNearestEven)
	c.Assert(err, qt.IsNil)
	c.Assert(bf.BigFloatValue().Cmp(want), qt.Equals, 0,
		qt.Commentf("long precision must keep the digits as written: got %v, want %v", bf.BigFloatValue(), want))

	// The double-precision twin must still round, or "l keeps the digits" is
	// not saying anything about l.
	dblEnv := environment.NewNamespace().Runtime()
	dblSyn, err := NewParser(dblEnv, true, strings.NewReader(mantissa+"d0")).ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	dbl, ok := dblSyn.Unwrap().(*values.Float)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected Float, got %T", dblSyn.Unwrap()))
	c.Assert(big.NewFloat(dbl.Value).Cmp(want), qt.Not(qt.Equals), 0,
		qt.Commentf("the d twin must lose digits, otherwise this test proves nothing about l"))
}

// TestBigFloatWritesLongPrecisionMarker pins the "and output" half of the TODO
// item. A BigFloat carries long precision, so its written exponent marker must
// be l; emitting e would claim default precision and read back as a Float.
//
// RED: BigFloat.SchemeString delegates to big.Float.Text('g', -1)
// (values/big_float.go), which always writes 'e'. This is a deliberate change
// to existing output — "1e+1000" becomes "1l+1000".
func TestBigFloatWritesLongPrecisionMarker(t *testing.T) {
	tcs := []string{
		"1e1000",
		"1e-1000",
		"-2.5e500",
		"123456789012345678901234567890",
	}

	for _, tc := range tcs {
		t.Run(tc, func(t *testing.T) {
			c := qt.New(t)
			bf := values.NewBigFloatFromString(tc)
			c.Assert(bf, qt.IsNotNil)

			written, err := values.WriteValueToString(bf)
			c.Assert(err, qt.IsNil)

			c.Assert(strings.ContainsAny(written, "lL"), qt.IsTrue,
				qt.Commentf("BigFloat must write its precision marker, got %q", written))
			c.Assert(strings.ContainsAny(written, "eE"), qt.IsFalse,
				qt.Commentf("e claims default precision, which reads back as Float, got %q", written))
		})
	}
}

// TestBigFloatWriteReadRoundTrip states the property the marker exists to
// deliver: a BigFloat written and read back is a BigFloat of the same value,
// without needing the #m prefix. This is the payoff of choosing l over e, and
// it is stated as a property rather than a spelling so the writer keeps its
// freedom of format.
//
// The 1.5 case is the demanding one: it has no exponent today ("1.5"), so
// round-tripping it forces the writer to emit a marker where it currently emits
// none. Reading "1.5" back yields a Float, which is a different type from the
// value written.
//
// RED until both the read and write halves land.
func TestBigFloatWriteReadRoundTrip(t *testing.T) {
	// "1e1000" already passes, but not because of anything this item adds: it
	// writes as "1e+1000" and reads back as a BigFloat only because the
	// magnitude overflows float64 (TestScientificNotationBigFloatOverflow). It
	// is kept as the case that must survive the writer switching to l.
	tcs := []string{
		"1.5",
		"-2.5",
		"1e1000",
		"1e-1000",
		"1.2345678901234567890123456789",
	}

	// A 256-bit power of ten writes as 1l+k, which must read back as the same
	// 256-bit neighbour, not as exact 10^k.
	ten := func(k int64) *values.BigFloat {
		n := new(big.Int).Exp(big.NewInt(10), big.NewInt(k), nil)
		return values.NewBigFloat(new(big.Float).SetPrec(values.DefaultBigFloatPrecision).SetInt(n))
	}
	// The 256-bit 1/3 writes 81 digits; a reader deriving precision from the
	// digit count read them back at 271 bits as another value.
	third := values.NewBigFloat(new(big.Float).SetPrec(values.DefaultBigFloatPrecision).Quo(big.NewFloat(1), big.NewFloat(3)))
	built := map[string]*values.BigFloat{"256-bit 10^120": ten(120), "256-bit 10^400": ten(400), "256-bit 1/3": third}
	for _, tc := range tcs {
		built[tc] = values.NewBigFloatFromString(tc)
	}

	for tc, bf := range built {
		t.Run(tc, func(t *testing.T) {
			c := qt.New(t)
			c.Assert(bf, qt.IsNotNil)

			written, err := values.WriteValueToString(bf)
			c.Assert(err, qt.IsNil)

			env := environment.NewNamespace().Runtime()
			syn, err := NewParser(env, true, strings.NewReader(written)).ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil, qt.Commentf("writer emitted %q, which the reader rejects", written))

			got := syn.Unwrap()
			back, ok := got.(*values.BigFloat)
			c.Assert(ok, qt.IsTrue, qt.Commentf("round trip through %q: expected BigFloat, got %T: %v", written, got, got))
			c.Assert(back.BigFloatValue().Cmp(bf.BigFloatValue()), qt.Equals, 0,
				qt.Commentf("round trip through %q changed the value", written))
			c.Assert(back.BigFloatValue().Prec(), qt.Equals, bf.BigFloatValue().Prec(),
				qt.Commentf("round trip through %q changed the precision", written))
		})
	}
}

// TestExponentMarkersStayDecimalOnly is a regression guard, not a RED test: it
// passes today and must keep passing. R7RS §7.1.1 puts ⟨suffix⟩ only inside
// ⟨decimal 10⟩, and isExponentMarkerForRadix (tokenizer_predicates.go) enforces
// that. MIT Scheme states the same rule for string->number: "a numeric
// representation using a decimal point or an exponent marker is not recognized
// unless radix is 10."
//
// It is here because the radix-float work (TestReadSyntaxRadixDecimalFraction)
// reaches into exactly this code path, and in base 16 the letters d, e, and f
// are digits. A change that started honouring exponent markers in hex would
// turn #x1e2 from 482 into 100.0 without any other test noticing.
func TestExponentMarkersStayDecimalOnly(t *testing.T) {
	tcs := []struct {
		input  string
		expect int64
	}{
		{"#x1e2", 482}, // 0x1e2, not 1 * 10^2
		{"#x1d2", 466}, // 0x1d2, not 1 * 10^2
		{"#x1f2", 498}, // 0x1f2, not 1 * 10^2
	}

	for _, tc := range tcs {
		t.Run(tc.input, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewNamespace().Runtime()
			p := NewParser(env, true, strings.NewReader(tc.input))

			syn, err := p.ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)

			obj := syn.Unwrap()
			i, ok := obj.(*values.Integer)
			c.Assert(ok, qt.IsTrue, qt.Commentf("hex letters are digits, not exponent markers: expected Integer, got %T: %v", obj, obj))
			c.Assert(i.Value, qt.Equals, tc.expect)
		})
	}
}

// TestReadComplexLongPrecisionMarker pins the l marker inside a complex literal:
// a part marked l reads as a BigFloat that keeps its digits, the result is a
// BigComplex, and the marker does not reach the other part, which is read as
// it would be standing alone and then made inexact (both parts BigFloat, as
// make-rectangular builds them). Unmarked literals are the controls.
func TestReadComplexLongPrecisionMarker(t *testing.T) {
	const digits = "1.2345678901234567890123456789"
	longDigits, _, err := big.ParseFloat(digits, 10, values.DefaultBigFloatPrecision, big.ToNearestEven)
	qt.Assert(t, err, qt.IsNil)
	third := new(big.Float).SetPrec(values.DefaultBigFloatPrecision).SetRat(big.NewRat(1, 3))
	tcs := []struct {
		input    string
		re, im   *big.Float
		realOnly bool
	}{
		{input: "0.0+1.5l0i", re: big.NewFloat(0), im: big.NewFloat(1.5)},
		{input: "1.5l0+2l0i", re: big.NewFloat(1.5), im: big.NewFloat(2)},
		{input: "1.5l0+2i", re: big.NewFloat(1.5), im: big.NewFloat(2)},
		{input: "1.5l0+1/3i", re: big.NewFloat(1.5), im: third},
		{input: "2.5+1.5l0i", re: big.NewFloat(2.5), im: big.NewFloat(1.5)},
		{input: "1l0-i", re: big.NewFloat(1), im: big.NewFloat(-1)},
		{input: "+1.5l0i", re: big.NewFloat(0), im: big.NewFloat(1.5)},
		{input: "-1.5L0i", re: big.NewFloat(0), im: big.NewFloat(-1.5)},
		{input: "1.5l0+0.0i", re: big.NewFloat(1.5), im: big.NewFloat(0)},
		{input: "+inf.0+1.5l0i", re: new(big.Float).SetInf(false), im: big.NewFloat(1.5)},
		{input: digits + "l0+1i", re: longDigits, im: big.NewFloat(1)},
		{input: "1+" + digits + "l0i", re: big.NewFloat(1), im: longDigits},
		// An exact zero imaginary part collapses to the BigFloat real.
		{input: "1.5l0+0i", re: big.NewFloat(1.5), realOnly: true},
	}
	for _, tc := range tcs {
		t.Run(tc.input, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewNamespace().Runtime()
			syn, err := NewParser(env, true, strings.NewReader(tc.input)).ReadSyntax(context.TODO())
			c.Assert(err, qt.IsNil)
			got := syn.Unwrap()
			if tc.realOnly {
				bf, ok := got.(*values.BigFloat)
				c.Assert(ok, qt.IsTrue, qt.Commentf("expected BigFloat, got %T: %v", got, got))
				c.Assert(bf.BigFloatValue().Cmp(tc.re), qt.Equals, 0)
				return
			}
			bc, ok := got.(*values.BigComplex)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected BigComplex, got %T: %v", got, got))
			for _, part := range []struct {
				name string
				n    values.Number
				want *big.Float
			}{{"real", bc.Real(), tc.re}, {"imag", bc.Imag(), tc.im}} {
				bf, ok := part.n.(*values.BigFloat)
				c.Assert(ok, qt.IsTrue, qt.Commentf("%s part: expected BigFloat, got %T", part.name, part.n))
				c.Assert(bf.BigFloatValue().Cmp(part.want), qt.Equals, 0,
					qt.Commentf("%s part: got %v, want %v", part.name, bf.BigFloatValue(), part.want))
			}
		})
	}

	// Controls: no l, no BigComplex.
	for input, want := range map[string]string{"1.5+2.5i": "*values.Complex", "1+2i": "*values.BigComplex"} {
		env := environment.NewNamespace().Runtime()
		syn, err := NewParser(env, true, strings.NewReader(input)).ReadSyntax(context.TODO())
		qt.Assert(t, err, qt.IsNil)
		got := syn.Unwrap()
		qt.Assert(t, fmt.Sprintf("%T", got), qt.Equals, want, qt.Commentf("%s", input))
		bc, ok := got.(*values.BigComplex)
		if ok {
			qt.Assert(t, bc.IsExact(), qt.IsTrue)
		}
	}
}

// TestInexactPrefixLeavesAnInexactBigComplex pins that #i is the identity on
// an inexact BigComplex, as it is on a BigFloat: #i1.5l0+2i keeps its BigFloat
// parts rather than narrowing to a float64 Complex.
func TestInexactPrefixLeavesAnInexactBigComplex(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	syn, err := NewParser(env, true, strings.NewReader("#i1.5l0+2i")).ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)
	got := syn.Unwrap()
	_, ok := got.(*values.BigComplex)
	c.Assert(ok, qt.IsTrue, qt.Commentf("#i1.5l0+2i read as %T", got))
}
