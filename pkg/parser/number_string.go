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
	"errors"
	"math"
	"math/big"
	"strconv"
	"strings"

	"github.com/aalpar/wile/pkg/schemeutil"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// ParseRealFloatString parses a real decimal or scientific-notation string as an
// inexact number. In-range magnitudes yield a float64-backed Float; magnitudes
// beyond float64 range (strconv reports ErrRange as ±Inf) promote to a BigFloat
// rather than failing. This mirrors the int64 -> BigInteger promotion in
// parseIntegerWithBase and, crucially, keeps the reader and string->number
// symmetric with the writer, which renders an out-of-range bigfloat in
// scientific notation (e.g. (write (* 1.0 (expt 10 1000))) -> "1e+1000").
// Underflow to zero (float64 rounds a tiny magnitude to 0 without ErrRange) is
// left as the float64 0.0 it already produces, matching float64-based Schemes.
// The exponent marker is normalized to 'e' first. Returns a wrapped
// ErrInvalidFormat sentinel on a malformed string.
func ParseRealFloatString(s string) (values.Number, error) {
	normalized := schemeutil.NormalizeExponentMarker(s)
	f, err := strconv.ParseFloat(normalized, 64)
	if err == nil {
		return values.NewFloat(f), nil
	}

	var numErr *strconv.NumError
	if errors.As(err, &numErr) && errors.Is(numErr.Err, strconv.ErrRange) {
		bf := values.NewBigFloatFromString(normalized)
		if bf != nil {
			return bf, nil
		}
	}
	return nil, werr.WrapForeignErrorf(werr.ErrInvalidFormat, "ParseRealFloatString: invalid real number: %s", s)
}

// ParseSpecialFloat checks if s is +inf.0, -inf.0, +nan.0, or -nan.0
// and returns the corresponding Float value.
// Returns (nil, false) if s is not a special-value string.
func ParseSpecialFloat(s string) (*values.Float, bool) {
	switch strings.ToLower(s) {
	case values.PositiveInfinityString:
		return values.NewFloat(math.Inf(1)), true
	case values.NegativeInfinityString:
		return values.NewFloat(math.Inf(-1)), true
	case values.NaNString, values.NegativeNaNString:
		return values.NewFloat(math.NaN()), true
	}
	return nil, false
}

// ParseImaginaryStringNumber parses a pure imaginary string (ending in 'i')
// and returns the resulting complex number.
// Handles "+3i", "-2.5i", "+i", "-i", "+inf.0i", "-nan.0i", etc.
//
// On reject it returns a non-nil error carrying a werr sentinel (ErrInvalidFormat
// for a shape that is not this grammar; the coefficient parser's own sentinel —
// ErrInvalidFormat / ErrDivisionByZero — for a malformed coefficient). Callers
// choose how to treat it: string->number discards it and tries the next grammar
// (R7RS §6.2.7 returns #f), while the reader wraps it with a source location so
// the cause stays reachable via errors.Is / errors.Unwrap.
func ParseImaginaryStringNumber(s string) (values.Number, error) {
	if !hasSuffixFoldedI(s) {
		return nil, werr.WrapForeignErrorf(werr.ErrInvalidFormat, "parseImaginary: missing imaginary suffix: %s", s)
	}
	trim := s[:len(s)-1]

	// Handle pure sign cases: "+i" and "-i"
	if trim == "+" || trim == "" {
		return values.NewBigComplex(
			values.NewBigIntegerFromInt64(0),
			values.NewBigIntegerFromInt64(1),
		), nil
	}
	if trim == "-" {
		return values.NewBigComplex(
			values.NewBigIntegerFromInt64(0),
			values.NewBigIntegerFromInt64(-1),
		), nil
	}

	// Special imaginary values: +inf.0i, -inf.0i, +nan.0i, -nan.0i
	sf, ok := ParseSpecialFloat(trim)
	if ok {
		return values.NewComplexFromParts(0, sf.Value), nil
	}

	// Exact coefficient — integer OR rational (R7RS §6.2.5: the coefficient of
	// a pure imaginary is an exact <ureal R>, so "3/4" in "+3/4i" stays exact).
	// parseExactPart yields a BigInteger or Rational as appropriate.
	//
	// values.Simplify collapses an exact-zero imaginary part to the exact real
	// (R7RS §6.2.1: (real? 0+0i) => #t, so "0i" reads as 0, eqv? to the integer
	// 0); for a nonzero coefficient it is the identity. This mirrors
	// make-rectangular (prim_complex.go), keeping the two construction paths
	// consistent.
	if isExactPartString(trim) {
		iam, err := parseExactPart(trim)
		if err != nil {
			return nil, err
		}
		return values.Simplify(values.NewBigComplex(values.NewBigIntegerFromInt64(0), iam)), nil
	}

	// Inexact coefficient (decimal / exponent / rational-with-decimal).
	f, err := parseFloatOrInfnan(trim)
	if err != nil {
		return nil, err
	}
	return values.NewComplexFromParts(0, f), nil
}

// findComplexSignSplit returns the index of the +/- that separates the real and
// imaginary parts of a rectangular complex string s (with the trailing 'i'
// already removed), or -1 if there is none. The scan starts at index 1 to skip a
// leading sign on the real part, ignores exponent-marker signs (e/s/f/d/l, per
// R7RS §7.1.1), and ignores the signs embedded in inf.0 / nan.0. Shared by
// parseComplex (parser_number.go) and ParseComplexStringNumber.
func findComplexSignSplit(s string) int {
	for i := 1; i < len(s); i++ {
		if s[i] != '+' && s[i] != '-' {
			continue
		}
		prev := s[i-1]
		if prev == 'e' || prev == 'E' ||
			prev == 's' || prev == 'S' ||
			prev == 'f' || prev == 'F' ||
			prev == 'd' || prev == 'D' ||
			prev == 'l' || prev == 'L' {
			continue
		}
		rest := s[i:]
		if strings.HasPrefix(rest, values.PositiveInfinityString) || strings.HasPrefix(rest, values.NegativeInfinityString) ||
			strings.HasPrefix(rest, values.NaNString) || strings.HasPrefix(rest, values.NegativeNaNString) ||
			rest == "+" || rest == "-" ||
			(len(rest) > 1 && (rest[1] >= '0' && rest[1] <= '9' || rest[1] == '.' || rest[1] == '/')) {
			return i
		}
	}
	return -1
}

// ParseComplexStringNumber parses a rectangular complex number string ending in 'i'.
// Handles "3+4i", "1.5-2.5i", "1+inf.0i", "0+3/4i", etc.
//
// On reject it returns a non-nil error carrying a werr sentinel (ErrInvalidFormat
// for a shape that is not this grammar; the part parser's own sentinel for a
// malformed real/imaginary part). See ParseImaginaryStringNumber for how the two
// callers treat the error differently.
func ParseComplexStringNumber(s string) (values.Number, error) {
	if !hasSuffixFoldedI(s) {
		return nil, werr.WrapForeignErrorf(werr.ErrInvalidFormat, "parseComplex: missing imaginary suffix: %s", s)
	}
	// Remove trailing 'i'
	trim := s[:len(s)-1]

	// Find the sign separating real and imaginary parts.
	signPos := findComplexSignSplit(trim)
	if signPos == -1 {
		return nil, werr.WrapForeignErrorf(werr.ErrInvalidFormat, "parseComplex: no real/imaginary separator: %s", s)
	}

	realStr := trim[:signPos]
	imagStr := trim[signPos:] // includes the sign

	// Exact path: both parts integer or rational → exact BigComplex.
	//
	// values.Simplify collapses an exact-zero imaginary part to the exact real
	// (R7RS §6.2.1: (real? 2+0i) => #t, so "2+0i" reads as the integer 2, eqv? to
	// 2; "3/4+0i" reads as 3/4); for a nonzero imaginary part it is the identity.
	// This matches make-rectangular (prim_complex.go), so the reader,
	// string->number, and make-rectangular all agree. An INEXACT zero imaginary
	// (e.g. "2+0.0i") takes the inexact path below and does NOT collapse, per the
	// same R7RS clause ((real? 2+0.0i) => #f).
	if isExactPartString(realStr) && isExactPartString(imagStr) {
		realNum, err := parseExactPart(realStr)
		if err != nil {
			return nil, err
		}
		imagNum, err := parseExactPart(imagStr)
		if err != nil {
			return nil, err
		}
		return values.Simplify(values.NewBigComplex(realNum, imagNum)), nil
	}

	// Inexact path.
	rel, err := parseFloatOrInfnan(realStr)
	if err != nil {
		return nil, err
	}

	// Exact zero imaginary → collapse to real.
	if isExactPartString(imagStr) {
		parsedImag, parseErr := parseExactPart(imagStr)
		if parseErr == nil && parsedImag.IsZero() {
			return values.NewFloat(rel), nil
		}
	}

	var img float64
	switch imagStr {
	case "+":
		img = 1
	case "-":
		img = -1
	default:
		img, err = parseFloatOrInfnan(imagStr)
		if err != nil {
			return nil, err
		}
	}
	return values.NewComplexFromParts(rel, img), nil
}

// hasSuffixFoldedI returns true if s ends with 'i' or 'I'.
func hasSuffixFoldedI(s string) bool {
	return len(s) > 0 && (s[len(s)-1] == 'i' || s[len(s)-1] == 'I')
}

// MakeExactNumber returns the exact representation of n, implementing the #e
// prefix (R7RS §7.1.1) and the exact procedure (R7RS §6.2.6).
//
// It is the single source of truth for the conversion, shared by the reader
// (parser_number.go makeExact) and string->number (extensions/math). The two
// must not diverge: a type handled by one and not the other silently drops the
// caller's #e prefix, which is exactly how *BigFloat came to be honored by the
// reader and ignored by string->number.
//
// On a magnitude with no exact representation (inf, NaN, an inexact BigComplex)
// it returns a wrapped ErrExactnessConversion. Callers choose the policy: the
// reader wraps it with a source location, string->number discards it and yields
// #f per R7RS §6.2.7.
func MakeExactNumber(n values.Number) (values.Number, error) {
	switch v := n.(type) {
	case *values.Integer, *values.BigInteger, *values.Rational:
		return v, nil // already exact
	case *values.Float:
		f := v.Value
		if math.IsNaN(f) || math.IsInf(f, 0) {
			return nil, werr.WrapForeignErrorf(werr.ErrExactnessConversion, "MakeExactNumber: cannot convert inf or nan to exact")
		}
		if f == math.Trunc(f) && f >= math.MinInt64 && f <= math.MaxInt64 {
			return values.NewInteger(int64(f)), nil
		}
		return values.Simplify(values.NewRationalFromRat(new(big.Rat).SetFloat64(f))), nil
	case *values.BigFloat:
		bf := v.BigFloatValue()
		if bf.IsInf() {
			return nil, werr.WrapForeignErrorf(werr.ErrExactnessConversion, "MakeExactNumber: cannot convert inf to exact")
		}
		if bf.IsInt() {
			i, _ := bf.Int(nil)
			return values.NewBigInteger(i), nil
		}
		r, _ := bf.Rat(nil)
		return values.Simplify(values.NewRationalFromRat(r)), nil
	case *values.Complex:
		re := v.Real()
		im := v.Imag()
		if math.IsNaN(re) || math.IsNaN(im) || math.IsInf(re, 0) || math.IsInf(im, 0) {
			return nil, werr.WrapForeignErrorf(werr.ErrExactnessConversion, "MakeExactNumber: cannot convert complex with inf or nan to exact")
		}
		reNum := values.NewRationalFromRat(new(big.Rat).SetFloat64(re))
		imNum := values.NewRationalFromRat(new(big.Rat).SetFloat64(im))
		return values.NewBigComplex(reNum, imNum), nil
	case *values.BigComplex:
		if v.IsExact() {
			return v, nil
		}
		return nil, werr.WrapForeignErrorf(werr.ErrExactnessConversion, "MakeExactNumber: cannot convert inexact BigComplex to exact")
	default:
		return nil, werr.WrapForeignErrorf(werr.ErrExactnessConversion, "MakeExactNumber: unsupported number type %T", n)
	}
}

// MakeExactFromLiteral implements the #e prefix (R7RS §7.1.1), which applies to
// the number **as written** rather than to the value it would otherwise denote.
//
// That distinction is the whole point of this function. A decimal literal is
// notation for a decimal value, so #e must convert its digits:
//
//	#e1e400  =>  10^400 exactly
//	#e0.1    =>  1/10
//
// Reading the literal first and converting the result instead gives the nearest
// binary float's exact value, which is a different number:
//
//	(exact 1e400) =>  the 256-bit BigFloat neighbour of 10^400
//	(exact 0.1)   =>  3602879701896397/36028797018963968
//
// Both answers are correct for their own question; only the first is #e. The
// procedure `exact` keeps the second, since by then the decimal is already gone.
//
// text is the literal's source text with '#' digit placeholders already
// substituted; parsed is what reading it produced. Only Float and BigFloat
// (i.e. decimal or scientific notation) take the digits path; every other shape
// — already exact, radix-prefixed, complex — defers to MakeExactNumber, as does
// any text big.Rat rejects (inf, nan), which MakeExactNumber then refuses.
func MakeExactFromLiteral(text string, parsed values.Number) (values.Number, error) {
	switch parsed.(type) {
	case *values.Float, *values.BigFloat:
		r, ok := new(big.Rat).SetString(schemeutil.NormalizeExponentMarker(text))
		if ok {
			return values.Simplify(values.NewRationalFromRat(r)), nil
		}
	}
	return MakeExactNumber(parsed)
}

// MakeInexactNumber returns the inexact representation of n, implementing the
// #i prefix (R7RS §7.1.1), the '#' digit placeholder (§7.1.1), and the inexact
// procedure (§6.2.6). Companion to MakeExactNumber and likewise the single
// source of truth, shared by the reader and string->number.
//
// It cannot fail: every exact number has an inexact image, and the already-
// inexact types (Float, BigFloat, Complex) are the identity. §6.2.6 sanctions
// silent precision loss here, so the accuracy/exact bools returned by
// big.Float and big.Rat are deliberately discarded.
func MakeInexactNumber(n values.Number) values.Number {
	switch v := n.(type) {
	case *values.Integer:
		return values.NewFloat(float64(v.Value))
	case *values.BigInteger:
		f, _ := new(big.Float).SetInt(v.BigInt()).Float64()
		return values.NewFloat(f)
	case *values.Rational:
		return values.NewFloat(v.Float64Truncated())
	case *values.BigComplex:
		reFloat := v.RealAsBigFloat().Float64Truncated()
		imFloat := v.ImagAsBigFloat().Float64Truncated()
		return values.NewComplexFromParts(reFloat, imFloat)
	default:
		return n // Float, BigFloat, Complex are already inexact
	}
}
