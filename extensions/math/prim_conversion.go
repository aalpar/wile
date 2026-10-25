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

package math

import (
	"math"
	"math/big"
	"strconv"
	"strings"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/parser"
	"github.com/aalpar/wile/pkg/registry/helpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// ensureInexactDecimal ensures a float string has a decimal point, even in
// scientific notation. "5e-324" becomes "5.0e-324", "1" becomes "1.0".
func ensureInexactDecimal(s string) string {
	eIdx := strings.IndexAny(s, "eE")
	if eIdx >= 0 {
		mantissa := s[:eIdx]
		if !strings.ContainsRune(mantissa, '.') {
			return mantissa + ".0" + s[eIdx:]
		}
		return s
	}
	if !strings.ContainsRune(s, '.') {
		return s + ".0"
	}
	return s
}

// PrimNumberToString implements the number->string primitive.
//
// Every result is a MUTABLE string. R7RS §3.4's storage model makes a newly
// allocated object's locations mutable unless a procedure's own description says
// otherwise, and number->string has no such clause — unlike command-line and the
// get-environment-variable family (§6.14, "it is an error to mutate any of these
// strings"), which correctly refuse.
//
// values.NewString defaults to immutable, so mutability is not the absence of a
// flag here: an allocator that wants it must call NewMutableString explicitly.
// That polarity is why every one of these arms was wrong the same way.
func PrimNumberToString(mc machine.CallContext) error {
	n := mc.Arg(0)
	rest := mc.Arg(1)
	radix := 10
	if !values.IsEmptyList(rest) {
		r, restAfter, err := helpers.UnconsTyped[*values.Integer](rest, werr.ErrNotANumber, "number->string", "radix")
		if err != nil {
			return err
		}
		if !values.IsEmptyList(restAfter) {
			return werr.WrapForeignErrorf(werr.ErrWrongNumberOfArguments, "number->string: expected 1 or 2 arguments")
		}
		radix = int(r.Value)
		if radix != 2 && radix != 8 && radix != 10 && radix != 16 {
			return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "number->string: radix must be 2, 8, 10, or 16")
		}
	}
	switch v := n.(type) {
	case *values.Integer:
		mc.SetValue(values.NewMutableString(strconv.FormatInt(v.Value, radix)))
	case *values.Float:
		switch {
		case math.IsInf(v.Value, 1):
			mc.SetValue(values.NewMutableString(values.PositiveInfinityString))
		case math.IsInf(v.Value, -1):
			mc.SetValue(values.NewMutableString(values.NegativeInfinityString))
		case math.IsNaN(v.Value):
			mc.SetValue(values.NewMutableString(values.NaNString))
		default:
			s := strconv.FormatFloat(v.Value, 'g', -1, 64)
			s = ensureInexactDecimal(s)
			mc.SetValue(values.NewMutableString(s))
		}
	case *values.Rational:
		mc.SetValue(values.NewMutableString(v.SchemeString()))
	case *values.Complex:
		mc.SetValue(values.NewMutableString(v.SchemeString()))
	case *values.BigComplex:
		mc.SetValue(values.NewMutableString(v.SchemeString()))
	case *values.BigInteger:
		mc.SetValue(values.NewMutableString(v.BigInt().Text(radix)))
	case *values.BigFloat:
		mc.SetValue(values.NewMutableString(v.SchemeString()))
	default:
		return werr.WrapForeignErrorf(werr.ErrNotANumber, "number->string: expected a number but got %T", n)
	}
	return nil
}

// PrimStringToNumber implements the string->number primitive.
//
// R7RS §6.2.7: string->number returns a number of the maximally precise
// representation expressed by the given string. It is an error if radix
// is not 2, 8, 10, or 16.
//
// R7RS §7.1.1: The string may contain prefix directives #b, #o, #d, #x
// (radix) and #e, #i (exactness), in either order, up to one of each.
// A radix prefix in the string overrides the radix argument. Wile's #z and #m
// read as they do in a literal; see parsePrefixedNumber.
func PrimStringToNumber(mc machine.CallContext) error {
	s := mc.Arg(0)
	rest := mc.Arg(1)
	str, ok := s.(*values.String)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAString, "string->number: expected a string but got %T", s)
	}
	radix := 10
	if !values.IsEmptyList(rest) {
		r, restAfter, err := helpers.UnconsTyped[*values.Integer](rest, werr.ErrNotANumber, "string->number", "radix")
		if err != nil {
			return err
		}
		if !values.IsEmptyList(restAfter) {
			return werr.WrapForeignErrorf(werr.ErrWrongNumberOfArguments, "string->number: expected 1 or 2 arguments")
		}
		radix = int(r.Value)
		if radix != 2 && radix != 8 && radix != 10 && radix != 16 {
			return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "string->number: radix must be 2, 8, 10, or 16")
		}
	}

	result := parsePrefixedNumber(str.Value, radix)
	if result == nil {
		mc.SetValue(values.FalseValue)
		return nil
	}
	mc.SetValue(result)
	return nil
}

// parsePrefixedNumber reads s as the reader reads a number literal, with radix
// as the default radix, and returns nil wherever the reader would not produce a
// number. R7RS §6.2.7 has no error channel, so every refusal is nil.
//
// The prefixes follow the reader's `#` dispatch (CLAUDE.local.md): #b #o #d #x
// and #e #i form one product, at most one prefix per slot, in either order;
// #z and #m are introducers that end the product and take one number, which
// may start a product of its own (#z#e9). Radix is lexical, so it cannot
// precede an introducer (#x#z1f); exactness converts a finished value, so it
// can, and applies after the widening (#i#z9 is 9.0).
func parsePrefixedNumber(s string, radix int) values.Number {
	exactness := 0 // 0 = unspecified, 1 = exact (#e), -1 = inexact (#i)
	radixSet := false
	for len(s) >= 2 && s[0] == '#' {
		marker := s[1] | 0x20 // ASCII lower case; a non-letter matches no arm
		switch marker {
		case 'b', 'o', 'd', 'x':
			if radixSet {
				return nil
			}
			radixSet = true
			radix = radixOfMarker[marker]
		case 'e', 'i':
			if exactness != 0 {
				return nil
			}
			exactness = 1
			if marker == 'i' {
				exactness = -1
			}
		case 'z', 'm':
			if radixSet {
				return nil
			}
			return applyExactness(parseIntroduced(marker, s[2:], radix), "", radix, exactness)
		default:
			return nil
		}
		s = s[2:]
	}
	return applyExactness(parseStringToNumber(s, radix), s, radix, exactness)
}

// radixOfMarker maps a lower-case radix prefix letter to its radix.
var radixOfMarker = map[byte]int{'b': 2, 'o': 8, 'd': 10, 'x': 16}

// parseIntroduced reads the operand of #z or #m (marker) and widens it.
//
// The reader has two spellings. A bare marker followed by '#' (#z#x1f) takes a
// whole prefixed number and widens its value. A marker with digits attached
// (#z42, #m1.5) is one decimal token, read from its text, which is how #m keeps
// every digit of #m1.2345678901234567890123456789; its grammar is a decimal
// integer or real, so #z10/2 and #m1/3 are refused there although #z#d10/2 is
// 5. Under a non-decimal default radix the attached digits are read in that
// radix and widened by value, with the same refusal of a ratio.
func parseIntroduced(marker byte, operand string, radix int) values.Number {
	widen := parser.WidenToBigInteger
	if marker == 'm' {
		widen = parser.WidenToBigFloat
	}
	switch {
	case strings.HasPrefix(operand, "#"):
		return widenOrNil(widen, parsePrefixedNumber(operand, radix))
	case strings.Contains(operand, "/"):
		return nil
	case radix != 10:
		return widenOrNil(widen, parseStringToNumber(operand, radix))
	case marker == 'z':
		q := values.NewBigIntegerFromString(operand, 10)
		if q == nil {
			return nil
		}
		return q
	}
	q, err := parser.ParseBigFloatLiteral(operand)
	if err != nil {
		return nil //nolint:nilerr // §6.2.7 has no error channel: a malformed literal answers #f
	}
	return q
}

// widenOrNil applies widen to n, mapping a missing operand or a refused
// widening to nil.
func widenOrNil(widen func(values.Value) (values.Number, error), n values.Number) values.Number {
	if n == nil {
		return nil
	}
	q, err := widen(n)
	if err != nil {
		return nil //nolint:nilerr // §6.2.7 has no error channel: #z1.5 answers #f
	}
	return q
}

// applyExactness applies an #e (1) or #i (-1) prefix to n, read from text in
// radix. Both conversions are the reader's, shared via pkg/parser so #e/#i
// cannot mean one thing in a literal and another in a string. #e converts a
// decimal literal from its digits (#e0.1 is 1/10) and anything else from its
// value: re-reading non-decimal digits as decimal would make #x#e1.8 9/5. A
// magnitude with no exact representation (#e+inf.0) yields nil.
func applyExactness(n values.Number, text string, radix, exactness int) values.Number {
	if n == nil {
		return nil
	}
	switch exactness {
	case 1:
		exact, err := makeExact(n, text, radix)
		if err != nil {
			return nil //nolint:nilerr // §6.2.7 has no error channel: unrepresentable answers #f
		}
		return exact
	case -1:
		return parser.MakeInexactNumber(n)
	}
	return n
}

// makeExact is the #e conversion, choosing the digits path for decimal text.
func makeExact(n values.Number, text string, radix int) (values.Number, error) {
	if radix != 10 {
		return parser.MakeExactNumber(n)
	}
	return parser.MakeExactFromLiteral(text, n)
}

// parseStringToNumber parses a numeric string in the given radix.
// Returns nil if the string is not a valid number.
// R7RS §6.2.7: handles integers, rationals, floats, complex, and special values.
func parseStringToNumber(input string, radix int) values.Number {
	if len(input) == 0 {
		return nil
	}

	// Special float values (+inf.0, -inf.0, +nan.0, -nan.0) are always decimal.
	sf, ok := parser.ParseSpecialFloat(input)
	if ok {
		return sf
	}

	// Complex and imaginary numbers (only for radix 10).
	if radix == 10 && len(input) > 1 && (input[len(input)-1] == 'i' || input[len(input)-1] == 'I') {
		// Try pure imaginary first (no real part separator). A reject is a
		// "not this grammar" signal here — discard the cause and try the next
		// form, ultimately yielding #f per R7RS §6.2.7.
		n, err := parser.ParseImaginaryStringNumber(input)
		if err == nil {
			return n
		}
		// Try full complex (real + imaginary parts).
		n, err = parser.ParseComplexStringNumber(input)
		if err == nil {
			return n
		}
	}

	// Try integer first.
	i, err := strconv.ParseInt(input, radix, 64)
	if err == nil {
		return values.NewInteger(i)
	}

	// Try big integer for overflow.
	bi := new(big.Int)
	_, bigOK := bi.SetString(input, radix)
	if bigOK {
		return values.NewBigInteger(bi)
	}

	// Try rational (only if radix applies to both parts).
	idx := strings.Index(input, "/")
	if idx > 0 && idx < len(input)-1 {
		numStr := input[:idx]
		denStr := input[idx+1:]
		num := new(big.Int)
		den := new(big.Int)
		_, ok := num.SetString(numStr, radix)
		if ok {
			_, ok := den.SetString(denStr, radix)
			if ok {
				if den.Sign() == 0 {
					return nil
				}
				r := new(big.Rat).SetFrac(num, den)
				return values.Simplify(values.NewRationalFromRat(r))
			}
		}
	}

	// Float and scientific notation only for radix 10. Delegates to the shared
	// reader helper so string->number and the reader stay in lockstep, including
	// BigFloat promotion for magnitudes beyond float64 range.
	if radix == 10 {
		n, err := parser.ParseRealFloatString(input)
		if err == nil {
			return n
		}
		return nil
	}

	// A non-decimal radix still reads a decimal point (#x1.8 is 1.5), as the
	// reader's parseDecimalFractionWithBase does. The shape is checked first
	// because big.ParseFloat, underneath, also takes an exponent and "inf",
	// neither of which the reader scans under a radix prefix.
	if isRadixFraction(input, radix) {
		n, err := parser.ParseRealFloatStringWithBase(input, radix)
		if err == nil {
			return n
		}
	}
	return nil
}

// isRadixFraction reports whether s is an optionally signed run of radix digits
// with exactly one '.', and at least one digit.
func isRadixFraction(s string, radix int) bool {
	if s != "" && (s[0] == '+' || s[0] == '-') {
		s = s[1:]
	}
	whole, frac, ok := strings.Cut(s, ".")
	digits := whole + frac
	if !ok || digits == "" || strings.ContainsAny(digits, "+-") {
		return false
	}
	// SetString with an explicit base admits only that base's digits (and a
	// leading sign, excluded above); a second '.' fails it too.
	_, ok = new(big.Int).SetString(digits, radix)
	return ok
}

// --- Loss-signal primitives ---
//
// These four primitives surface Go's big.Accuracy three-valued enum
// (Below / Exact / Above) to Scheme via 'below / 'exact / 'above
// symbols. They predict or report the accuracy of an
// (exact->inexact n) conversion at the float64 / complex128 boundary
// without erroring on loss — they make the loss visible instead.
//
// Domain dispatch uses the values.ComplexNumber interface (matches
// Hashable/Tuple/Indexable precedent in values/) to avoid enumerating
// *Complex and *BigComplex by name.
//
// The defensive `if err != nil { return err }` branch after every
// values.ToFloat64WithAccuracy / ToComplex128WithAccuracy call is
// statically unreachable: those helpers only error on a nil Number,
// and each primitive type-asserts mc.Arg(0).(values.Number) before
// the call. The check is retained as a contract-boundary safety net
// in case the helpers ever grow new error paths.

// PrimInexactLosslessQ implements (inexact-lossless? n).
//
// Returns #t iff (exact->inexact n) would lose no information. For
// real input, this means the float64 conversion is big.Exact. For
// complex input, BOTH real and imaginary components must be big.Exact.
// (For real-only inputs the imaginary accuracy is trivially big.Exact,
// so the complex predicate collapses to the real-part check — hence
// the unified ToComplex128WithAccuracy path.)
func PrimInexactLosslessQ(mc machine.CallContext) error {
	n, ok := mc.Arg(0).(values.Number)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotANumber,
			"inexact-lossless?: expected a number but got %s", mc.Arg(0).SchemeString())
	}
	res, err := values.ToComplex128WithAccuracy(n)
	if err != nil {
		return err
	}
	lossless := res.RealAcc == big.Exact && res.ImagAcc == big.Exact
	mc.SetValue(values.BoolToBoolean(lossless))
	return nil
}

// PrimInexactAccuracy implements (inexact-accuracy n).
//
// Polymorphic return shape: real input → one symbol; complex input →
// two symbols via mc.SetValues. R7RS-style multi-value protocol.
func PrimInexactAccuracy(mc machine.CallContext) error {
	n, ok := mc.Arg(0).(values.Number)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotANumber,
			"inexact-accuracy: expected a number but got %s", mc.Arg(0).SchemeString())
	}
	_, isComplex := n.(values.ComplexNumber)
	if isComplex {
		res, err := values.ToComplex128WithAccuracy(n)
		if err != nil {
			return err
		}
		mc.SetValues(
			values.BigAccuracyToSymbol(res.RealAcc),
			values.BigAccuracyToSymbol(res.ImagAcc),
		)
		return nil
	}
	_, acc, _, err := values.ToFloat64WithAccuracy(n)
	if err != nil {
		return err
	}
	mc.SetValue(values.BigAccuracyToSymbol(acc))
	return nil
}

// PrimInexactWithAccuracy implements (inexact-with-accuracy n).
//
// Polymorphic return: real input → (values inexact-n acc-sym); complex
// input → (values inexact-c real-acc-sym imag-acc-sym). The inexact
// value is the actual float64 / complex128 produced by the conversion;
// the accuracy symbols indicate the rounding direction.
func PrimInexactWithAccuracy(mc machine.CallContext) error {
	n, ok := mc.Arg(0).(values.Number)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotANumber,
			"inexact-with-accuracy: expected a number but got %s", mc.Arg(0).SchemeString())
	}
	_, isComplex := n.(values.ComplexNumber)
	if isComplex {
		res, err := values.ToComplex128WithAccuracy(n)
		if err != nil {
			return err
		}
		mc.SetValues(
			values.NewComplex(res.Value),
			values.BigAccuracyToSymbol(res.RealAcc),
			values.BigAccuracyToSymbol(res.ImagAcc),
		)
		return nil
	}
	f, acc, _, err := values.ToFloat64WithAccuracy(n)
	if err != nil {
		return err
	}
	mc.SetValues(
		values.NewFloat(f),
		values.BigAccuracyToSymbol(acc),
	)
	return nil
}

// PrimComplexInexactWithAccuracy implements
// (complex-inexact-with-accuracy n) — the uniform 3-value variant.
//
// Always returns (values inexact-c real-acc-sym imag-acc-sym),
// regardless of whether the input is real or complex. For real input
// N, the imaginary accuracy is trivially 'exact. Useful when callers
// want a single arity regardless of input domain.
func PrimComplexInexactWithAccuracy(mc machine.CallContext) error {
	n, ok := mc.Arg(0).(values.Number)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotANumber,
			"complex-inexact-with-accuracy: expected a number but got %s", mc.Arg(0).SchemeString())
	}
	res, err := values.ToComplex128WithAccuracy(n)
	if err != nil {
		return err
	}
	mc.SetValues(
		values.NewComplex(res.Value),
		values.BigAccuracyToSymbol(res.RealAcc),
		values.BigAccuracyToSymbol(res.ImagAcc),
	)
	return nil
}
