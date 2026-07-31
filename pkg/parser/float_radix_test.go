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
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"

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
