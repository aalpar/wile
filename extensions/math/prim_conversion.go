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
	"github.com/aalpar/wile/pkg/schemeutil"
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
		mc.SetValue(values.NewString(strconv.FormatInt(v.Value, radix)))
	case *values.Float:
		switch {
		case math.IsInf(v.Value, 1):
			mc.SetValue(values.NewString(values.PositiveInfinityString))
		case math.IsInf(v.Value, -1):
			mc.SetValue(values.NewString(values.NegativeInfinityString))
		case math.IsNaN(v.Value):
			mc.SetValue(values.NewString(values.NaNString))
		default:
			s := strconv.FormatFloat(v.Value, 'g', -1, 64)
			s = ensureInexactDecimal(s)
			mc.SetValue(values.NewString(s))
		}
	case *values.Rational:
		mc.SetValue(values.NewString(v.SchemeString()))
	case *values.Complex:
		mc.SetValue(values.NewString(v.SchemeString()))
	case *values.BigComplex:
		mc.SetValue(values.NewString(v.SchemeString()))
	case *values.BigInteger:
		mc.SetValue(values.NewString(v.BigInt().Text(radix)))
	case *values.BigFloat:
		mc.SetValue(values.NewString(v.SchemeString()))
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
// A radix prefix in the string overrides the radix argument.
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

	input := str.Value
	exactness := 0 // 0 = unspecified, 1 = exact (#e), -1 = inexact (#i)

	// Parse up to two R7RS prefix directives.
	for range 2 {
		if len(input) < 2 || input[0] != '#' {
			break
		}
		switch input[1] {
		case 'b', 'B':
			radix = 2
			input = input[2:]
		case 'o', 'O':
			radix = 8
			input = input[2:]
		case 'd', 'D':
			radix = 10
			input = input[2:]
		case 'x', 'X':
			radix = 16
			input = input[2:]
		case 'e', 'E':
			exactness = 1
			input = input[2:]
		case 'i', 'I':
			exactness = -1
			input = input[2:]
		default:
			// Unknown prefix — not a valid number.
			mc.SetValue(values.FalseValue)
			return nil
		}
	}

	result := parseStringToNumber(input, radix)
	if result == nil {
		mc.SetValue(values.FalseValue)
		return nil
	}

	// Apply exactness conversion if a prefix was specified.
	switch exactness {
	case 1:
		result = stringToNumberMakeExact(result)
	case -1:
		result = stringToNumberMakeInexact(result)
	}

	mc.SetValue(result)
	return nil
}

// parseStringToNumber parses a numeric string in the given radix.
// Returns nil if the string is not a valid number.
// R7RS §6.2.7: handles integers, rationals, floats, complex, and special values.
func parseStringToNumber(input string, radix int) values.Value {
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

	// Float and scientific notation only for radix 10.
	if radix == 10 {
		f, err := strconv.ParseFloat(schemeutil.NormalizeExponentMarker(input), 64)
		if err == nil {
			return values.NewFloat(f)
		}
	}

	return nil
}

// stringToNumberMakeExact converts a number to its exact representation.
//
// R7RS §6.2.6: exact returns an exact representation of z.
func stringToNumberMakeExact(n values.Value) values.Value {
	switch v := n.(type) {
	case *values.Integer, *values.BigInteger, *values.Rational:
		return v
	case *values.Float:
		f := v.Value
		if f == math.Trunc(f) && f >= math.MinInt64 && f <= math.MaxInt64 {
			return values.NewInteger(int64(f))
		}
		r := new(big.Rat).SetFloat64(f)
		if r == nil {
			return n // inf/nan — cannot convert
		}
		return values.Simplify(values.NewRationalFromRat(r))
	default:
		return n
	}
}

// stringToNumberMakeInexact converts a number to its inexact representation.
//
// R7RS §6.2.6: inexact returns an inexact representation of z.
func stringToNumberMakeInexact(n values.Value) values.Value {
	switch v := n.(type) {
	case *values.Float:
		return v
	case *values.Integer:
		return values.NewFloat(float64(v.Value))
	case *values.BigInteger:
		f, _ := new(big.Float).SetInt(v.BigInt()).Float64()
		return values.NewFloat(f)
	case *values.Rational:
		f, _ := v.Rat().Float64()
		return values.NewFloat(f)
	default:
		return n
	}
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
