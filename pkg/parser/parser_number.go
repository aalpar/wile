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

	"github.com/aalpar/wile/pkg/internal/tokenizer"
	"github.com/aalpar/wile/pkg/schemeutil"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// parseIntegerWithBase parses the current token as an integer in the given base.
// Handles hash digit substitution, overflow promotion to BigInteger,
// and forces inexact (Float) when hash digits are present.
//
// R7RS §7.1.1: <uinteger R> -> <digit R>+ #*
// When # appears in a number literal, each # represents an unknown digit
// (treated as 0) and the result is inexact.
func (p *Parser) parseIntegerWithBase(base int) (syntax.SyntaxValue, tokenizer.Token, error) {
	s := replaceHashDigits(p.cur.String())
	a, err := strconv.ParseInt(s, base, 64)
	if err != nil {
		// If overflow, promote to BigInteger
		var numErr *strconv.NumError
		isNumErr := errors.As(err, &numErr)
		if isNumErr && errors.Is(numErr.Err, strconv.ErrRange) {
			bigInt := new(big.Int)
			_, ok := bigInt.SetString(s, base)
			if ok {
				if p.cur.HasHashDigit() {
					f, _ := bigInt.Float64()
					q := p.wrapSyntax(values.NewFloat(f), p.cur)
					return q, p.cur, nil
				}
				q := p.wrapSyntax(values.NewBigInteger(bigInt), p.cur)
				return q, p.cur, nil
			}
		}
		return nil, p.cur, NewParserErrorWithWrapf(err, p.cur, "invalid base-%d integer: %s", base, p.cur.String())
	}
	if p.cur.HasHashDigit() {
		q := p.wrapSyntax(values.NewFloat(float64(a)), p.cur)
		return q, p.cur, nil
	}
	q := p.wrapSyntax(values.NewInteger(a), p.cur)
	return q, p.cur, nil
}

// parseRationalWithBase parses the current token as a rational number in the given base.
// Handles hash digit substitution and forces inexact when hash digits are present.
//
// R7RS §7.1.1: <urational R> -> <uinteger R> / <uinteger R>
func (p *Parser) parseRationalWithBase(base int) (syntax.SyntaxValue, tokenizer.Token, error) {
	s := replaceHashDigits(p.cur.String())

	// Strip leading sign for parsing, track it separately
	sign := int64(1)
	raw := s
	if len(raw) > 0 && (raw[0] == '+' || raw[0] == '-') {
		if raw[0] == '-' {
			sign = -1
		}
		raw = raw[1:]
	}

	parts := strings.SplitN(raw, "/", 2)
	if len(parts) != 2 || parts[0] == "" || parts[1] == "" {
		return nil, p.cur, NewParserErrorf(p.cur, "invalid rational number: %s", p.cur.String())
	}

	num, err := strconv.ParseInt(parts[0], base, 64)
	if err != nil {
		// Try big.Int on overflow
		var numErr *strconv.NumError
		if errors.As(err, &numErr) && errors.Is(numErr.Err, strconv.ErrRange) {
			bigNum := new(big.Int)
			_, ok := bigNum.SetString(parts[0], base)
			if !ok {
				return nil, p.cur, NewParserErrorf(p.cur, "invalid rational numerator: %s", parts[0])
			}
			bigDen := new(big.Int)
			_, ok = bigDen.SetString(parts[1], base)
			if !ok {
				return nil, p.cur, NewParserErrorf(p.cur, "invalid rational denominator: %s", parts[1])
			}
			if sign == -1 {
				bigNum.Neg(bigNum)
			}
			if bigDen.Sign() == 0 {
				return nil, p.cur, NewParserErrorWithWrap(werr.ErrDivisionByZero, p.cur,
					"rational denominator is zero")
			}
			r := new(big.Rat).SetFrac(bigNum, bigDen)
			q1 := values.Simplify(values.NewRationalFromRat(r))
			if p.cur.HasHashDigit() {
				q := p.wrapSyntax(p.numberToInexact(q1), p.cur)
				return q, p.cur, nil
			}
			q := p.wrapSyntax(q1, p.cur)
			return q, p.cur, nil
		}
		return nil, p.cur, NewParserErrorf(p.cur, "invalid rational numerator: %s", parts[0])
	}

	den, err := strconv.ParseInt(parts[1], base, 64)
	if err != nil {
		var numErr *strconv.NumError
		if errors.As(err, &numErr) && errors.Is(numErr.Err, strconv.ErrRange) {
			bigNum := big.NewInt(num * sign)
			bigDen := new(big.Int)
			_, ok := bigDen.SetString(parts[1], base)
			if !ok {
				return nil, p.cur, NewParserErrorf(p.cur, "invalid rational denominator: %s", parts[1])
			}
			r := new(big.Rat).SetFrac(bigNum, bigDen)
			q1 := values.Simplify(values.NewRationalFromRat(r))
			if p.cur.HasHashDigit() {
				q := p.wrapSyntax(p.numberToInexact(q1), p.cur)
				return q, p.cur, nil
			}
			q := p.wrapSyntax(q1, p.cur)
			return q, p.cur, nil
		}
		return nil, p.cur, NewParserErrorf(p.cur, "invalid rational denominator: %s", parts[1])
	}

	if den == 0 {
		return nil, p.cur, NewParserErrorWithWrap(werr.ErrDivisionByZero, p.cur,
			"rational denominator is zero")
	}
	num *= sign
	r := new(big.Rat).SetFrac64(num, den)
	q1 := values.Simplify(values.NewRationalFromRat(r))
	if p.cur.HasHashDigit() {
		q := p.wrapSyntax(p.numberToInexact(q1), p.cur)
		return q, p.cur, nil
	}
	q := p.wrapSyntax(q1, p.cur)
	return q, p.cur, nil
}

// parseBigIntegerWithBase parses the current token as a big integer with the given base.
// It strips the #z or #Z prefix before parsing.
func (p *Parser) parseBigIntegerWithBase(base int) (syntax.SyntaxValue, tokenizer.Token, error) {
	s := schemeutil.TrimPrefixCI(p.cur.String(), "#z")
	q1 := values.NewBigIntegerFromString(s, base)
	if q1 == nil {
		return nil, p.cur, NewParserErrorf(p.cur, "invalid big integer: %s", p.cur.String())
	}
	q := p.wrapSyntax(q1, p.cur)
	return q, p.cur, nil
}

// parseScientificNotation parses a number in scientific notation (e.g., "1e10", "+2e-5").
// Per R7RS §7.1.1, the exponent marker indicates inexact notation, so all scientific
// notation produces Float (inexact). The #e prefix can convert to exact after parsing.
func (p *Parser) parseScientificNotation() (syntax.SyntaxValue, tokenizer.Token, error) {
	s := replaceHashDigits(p.cur.String())

	// A scientific-notation token must carry an exponent marker (e/E or a short
	// s/f/d/l form); its absence means a plain number was mis-routed here.
	if schemeutil.IndexExponentMarker(s) == -1 {
		return nil, p.cur, NewParserErrorf(p.cur, "invalid scientific notation: %s", s)
	}

	// NormalizeExponentMarker folds any short marker to 'e'; e/E pass through
	// since strconv.ParseFloat accepts them directly.
	f, err := strconv.ParseFloat(schemeutil.NormalizeExponentMarker(s), 64)
	if err != nil {
		return nil, p.cur, NewParserErrorf(p.cur, "invalid scientific notation: %s", s)
	}
	q := p.wrapSyntax(values.NewFloat(f), p.cur)
	return q, p.cur, nil
}

// replaceHashDigits replaces all '#' inexact digit placeholders with '0'.
// R7RS §7.1.1: '#' represents an unknown digit treated as 0; its presence
// forces the number to be inexact (handled by the caller checking HasHashDigit).
func replaceHashDigits(s string) string {
	return strings.ReplaceAll(s, "#", "0")
}

// parseBaseWithExactness handles #b, #o, #x prefixes followed by an optional
// #e/#i exactness marker. R7RS §7.1.1: prefix ordering is either (#e|#i)(#b|#o|#x)
// or (#b|#o|#x)(#e|#i). The base-marker cases (#b/#o/#x) may see an exactness
// marker as their next token; this method handles that transparently.
func (p *Parser) parseBaseWithExactness(base int) (syntax.SyntaxValue, tokenizer.Token, error) {
	p.cur, p.err = p.toks.Next()
	if p.err != nil {
		return nil, p.cur, p.err
	}

	// Check for trailing exactness prefix: #x#e or #x#i.
	exactness := 0 // 0 = unspecified, 1 = exact (#e), -1 = inexact (#i)
	switch p.cur.Type() {
	case tokenizer.TokenizerStateMarkerNumberExact:
		exactness = 1
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
	case tokenizer.TokenizerStateMarkerNumberInexact:
		exactness = -1
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
	}

	// Parse the number in the given base.
	var q syntax.SyntaxValue
	var tok tokenizer.Token
	var err error
	if p.cur.Type() == tokenizer.TokenizerStateUnsignedRationalFraction ||
		p.cur.Type() == tokenizer.TokenizerStateSignedRationalFraction {
		q, tok, err = p.parseRationalWithBase(base)
	} else {
		q, tok, err = p.parseIntegerWithBase(base)
	}
	if err != nil {
		return nil, tok, err
	}

	// Apply exactness if a trailing prefix was present.
	switch exactness {
	case 1:
		exact, convErr := p.makeExact(q)
		if convErr != nil {
			return nil, tok, NewParserErrorf(tok, "cannot convert to exact: %v", convErr)
		}
		return exact, tok, nil
	case -1:
		inexact, convErr := p.makeInexact(q)
		if convErr != nil {
			return nil, tok, NewParserErrorf(tok, "cannot convert to inexact: %v", convErr)
		}
		return inexact, tok, nil
	}
	return q, tok, nil
}

// parseRational parses a rational number string like "3/4" or "-1/2".
// Reduces the result via Simplify, so "10/2" becomes Integer(5).
func (p *Parser) parseRational(s string) (values.Number, error) {
	r := new(big.Rat)
	_, ok := r.SetString(s)
	if !ok {
		return nil, NewParserErrorf(p.cur, "invalid rational number: %s", s)
	}
	q := values.NewRationalFromRat(r)
	return values.Simplify(q), nil
}

// parseImaginary parses a pure imaginary number string like "+3i", "-2i",
// "+i", "-i", "+3/4i". R7RS §6.2.2: exact BigComplex for integer/rational
// coefficients, inexact Complex for floating-point ones.
//
// Delegates to ParseImaginaryStringNumber — the single source of truth for the
// imaginary-number grammar, shared with string->number (number_string.go). This
// method adds only the reader's contribution: a source-located error on reject.
func (p *Parser) parseImaginary(s string) (values.Number, error) {
	q, ok := ParseImaginaryStringNumber(s)
	if !ok {
		return nil, NewParserErrorf(p.cur, "invalid imaginary number: %s", s)
	}
	return q, nil
}

// parsePolarComplex parses a polar complex number string like "1@1.5708", "+2@0.5", "-3@1.0"
// and converts it to rectangular form using: real = r*cos(θ), imag = r*sin(θ)
func (p *Parser) parsePolarComplex(s string) (*values.Complex, error) {
	// Find the @ separator
	before, after, ok := strings.Cut(s, "@")
	if !ok {
		return nil, NewParserErrorf(p.cur, "invalid polar complex number: %s (no @ separator found)", s)
	}

	// Split into magnitude and angle parts
	magPart := before
	anglePart := after

	// Parse magnitude
	mag, err := parseFloatOrInfnan(magPart)
	if err != nil {
		return nil, NewParserErrorf(p.cur, "invalid magnitude in polar complex: %s", magPart)
	}

	// Parse angle
	angle, err := parseFloatOrInfnan(anglePart)
	if err != nil {
		return nil, NewParserErrorf(p.cur, "invalid angle in polar complex: %s", anglePart)
	}

	// Convert polar to rectangular: real = r*cos(θ), imag = r*sin(θ)
	rel := mag * math.Cos(angle)
	iam := mag * math.Sin(angle)

	return values.NewComplexFromParts(rel, iam), nil
}

// isRationalString checks if a string represents a rational number (contains /).
func isRationalString(s string) bool {
	return strings.Contains(s, "/")
}

// isIntegerString checks if a string represents an integer (no . or /).
func isIntegerString(s string) bool {
	// Handle signed numbers
	start := 0
	if len(s) > 0 && (s[0] == '+' || s[0] == '-') {
		start = 1
	}
	if start >= len(s) {
		return false
	}
	for i := start; i < len(s); i++ {
		if s[i] < '0' || s[i] > '9' {
			return false
		}
	}
	return true
}

// isExactPartString checks if a string represents an exact number (integer or rational).
func isExactPartString(s string) bool {
	// Handle pure sign cases for imaginary: +i and -i map to exact 1 or -1
	if s == "+" || s == "-" {
		return true
	}
	return isIntegerString(s) || isRationalString(s)
}

// parseExactPart parses an exact number string (integer or rational) and returns
// a BigInteger or Rational suitable for use as a BigComplex part.
func parseExactPart(s string) (values.Number, error) {
	// Handle pure sign cases for imaginary: +i -> 1, -i -> -1
	if s == "+" {
		return values.NewBigIntegerFromInt64(1), nil
	}
	if s == "-" {
		return values.NewBigIntegerFromInt64(-1), nil
	}

	if isRationalString(s) {
		r := new(big.Rat)
		_, ok := r.SetString(s)
		if !ok {
			return nil, werr.WrapForeignErrorf(werr.ErrInvalidFormat, "parseDecimalInteger: invalid rational: %s", s)
		}
		return values.NewRationalFromRat(r), nil
	}

	// Integer
	i := new(big.Int)
	_, ok := i.SetString(s, 10)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrInvalidFormat, "parseDecimalInteger: invalid integer: %s", s)
	}
	return values.NewBigInteger(i), nil
}

// parseComplex parses a rectangular complex number string like "1+2i", "3-4i",
// "1.5+2.5i", "1+i", "5-i", and infnan forms "1+inf.0i", "3+nan.0i".
// R7RS §6.2.2: exact BigComplex if both parts are exact (integer/rational),
// otherwise inexact Complex; an exact-zero imaginary part collapses to a real.
//
// Delegates to ParseComplexStringNumber — the single source of truth for the
// rectangular-complex grammar, shared with string->number (number_string.go).
// This method adds only the reader's contribution: a source-located error on
// reject.
func (p *Parser) parseComplex(s string) (values.Number, error) {
	q, ok := ParseComplexStringNumber(s)
	if !ok {
		return nil, NewParserErrorf(p.cur, "invalid complex number: %s", s)
	}
	return q, nil
}

// parseFloatOrInfnan parses a float that may be inf.0, nan.0, or a rational
func parseFloatOrInfnan(s string) (float64, error) {
	switch s {
	case "+inf.0":
		return math.Inf(1), nil
	case "-inf.0":
		return math.Inf(-1), nil
	case "+nan.0", "-nan.0":
		return math.NaN(), nil
	}

	// Check for rational number (contains '/')
	before, after, ok := strings.Cut(s, "/")
	if ok {
		numStr := before
		denStr := after

		num, err := strconv.ParseFloat(numStr, 64)
		if err != nil {
			return 0, err
		}
		den, err := strconv.ParseFloat(denStr, 64)
		if err != nil {
			return 0, err
		}
		if den == 0 {
			return 0, werr.WrapForeignErrorf(werr.ErrDivisionByZero, "parseFloatOrInfnan: division by zero in rational")
		}
		return num / den, nil
	}

	return strconv.ParseFloat(schemeutil.NormalizeExponentMarker(s), 64)
}

// convertWrappedNumber unwraps stx to a Number, applies convert, and re-wraps the
// result with stx's original source context. op names the calling conversion
// ("makeExact" / "makeInexact") for the not-a-number error message. It is the
// shared scaffolding of makeExact and makeInexact: both differ only in the
// per-type conversion body supplied by convert.
func (p *Parser) convertWrappedNumber(stx syntax.SyntaxValue, op string, convert func(values.Number) (values.Value, error)) (syntax.SyntaxValue, error) {
	num, ok := stx.Unwrap().(values.Number)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrNotANumber, "%s: value is not numeric", op)
	}
	result, err := convert(num)
	if err != nil {
		return nil, err
	}
	return p.rewrapSyntax(stx, result), nil
}

// makeExact converts a syntax-wrapped number to its exact representation.
// R7RS §6.2.6: exact converts an inexact number to exact.
// For integers and rationals, they are already exact.
// For floats, they are converted to rationals or integers if they represent whole numbers.
func (p *Parser) makeExact(stx syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return p.convertWrappedNumber(stx, "makeExact", func(num values.Number) (values.Value, error) {
		switch v := num.(type) {
		case *values.Integer, *values.BigInteger, *values.Rational:
			return v, nil // already exact
		case *values.Float:
			f := v.Value
			if math.IsNaN(f) || math.IsInf(f, 0) {
				return nil, werr.WrapForeignErrorf(werr.ErrExactnessConversion, "makeExact: cannot convert inf or nan to exact")
			}
			if f == math.Trunc(f) && f >= math.MinInt64 && f <= math.MaxInt64 {
				return values.NewInteger(int64(f)), nil
			}
			return values.NewRationalFromRat(new(big.Rat).SetFloat64(f)), nil
		case *values.BigFloat:
			bf := v.BigFloatValue()
			if bf.IsInf() {
				return nil, werr.WrapForeignErrorf(werr.ErrExactnessConversion, "makeExact: cannot convert inf to exact")
			}
			if bf.IsInt() {
				i, _ := bf.Int(nil)
				return values.NewBigInteger(i), nil
			}
			r, _ := bf.Rat(nil)
			return values.NewRationalFromRat(r), nil
		case *values.Complex:
			re := v.Real()
			im := v.Imag()
			if math.IsNaN(re) || math.IsNaN(im) || math.IsInf(re, 0) || math.IsInf(im, 0) {
				return nil, werr.WrapForeignErrorf(werr.ErrExactnessConversion, "makeExact: cannot convert complex with inf or nan to exact")
			}
			reNum := values.NewRationalFromRat(new(big.Rat).SetFloat64(re))
			imNum := values.NewRationalFromRat(new(big.Rat).SetFloat64(im))
			return values.NewBigComplex(reNum, imNum), nil
		case *values.BigComplex:
			if v.IsExact() {
				return v, nil
			}
			return nil, werr.WrapForeignErrorf(werr.ErrExactnessConversion, "makeExact: cannot convert inexact BigComplex to exact")
		default:
			return nil, werr.WrapForeignErrorf(werr.ErrExactnessConversion, "makeExact: unsupported number type")
		}
	})
}

// numberToInexact converts a Number to its inexact representation.
//
// R7RS §7.1.1: Numbers containing # digit placeholders are inexact.
// R7RS §6.2.6: Inexact numbers use floating-point representation.
//
// The accuracy/exact-bool returns from big.Float/big.Rat are intentionally
// discarded: R7RS §6.2.6 sanctions silent precision loss when constructing
// an inexact number from an exact one. The loss-signal helpers in
// values/conversion.go are not used here for the same reason.
func (p *Parser) numberToInexact(num values.Number) values.Number {
	switch v := num.(type) {
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
		// Float, BigFloat, Complex are already inexact
		return num
	}
}

// makeInexact converts a syntax-wrapped number to its inexact representation.
// R7RS §6.2.6: inexact converts an exact number to inexact. Per R7RS, the
// conversion is sanctioned to lose precision silently — the discarded
// accuracy/exact bools below are deliberate.
func (p *Parser) makeInexact(stx syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return p.convertWrappedNumber(stx, "makeInexact", func(num values.Number) (values.Value, error) {
		switch v := num.(type) {
		case *values.Float, *values.BigFloat, *values.Complex:
			return v, nil // already inexact
		case *values.Integer:
			return values.NewFloat(float64(v.Value)), nil
		case *values.BigInteger:
			f, _ := new(big.Float).SetInt(v.BigInt()).Float64()
			return values.NewFloat(f), nil
		case *values.Rational:
			return values.NewFloat(v.Float64Truncated()), nil
		case *values.BigComplex:
			reFloat := v.RealAsBigFloat().Float64Truncated()
			imFloat := v.ImagAsBigFloat().Float64Truncated()
			return values.NewComplexFromParts(reFloat, imFloat), nil
		default:
			return nil, werr.WrapForeignErrorf(werr.ErrExactnessConversion, "makeInexact: unsupported number type")
		}
	})
}

// parseDecimalFraction parses a decimal fraction token (e.g., "1.5", "-0.3").
func (p *Parser) parseDecimalFraction() (syntax.SyntaxValue, tokenizer.Token, error) {
	a, err := strconv.ParseFloat(schemeutil.NormalizeExponentMarker(replaceHashDigits(p.cur.String())), 64)
	if err != nil {
		return nil, p.cur, NewParserErrorWithWrapf(err, p.cur, "invalid decimal fraction: %s", p.cur.String())
	}
	q := p.wrapSyntax(values.NewFloat(a), p.cur)
	return q, p.cur, nil
}

// parseBigFloat parses a big float token (e.g., "#m1.23456789012345678901234567890").
func (p *Parser) parseBigFloat() (syntax.SyntaxValue, tokenizer.Token, error) {
	s := schemeutil.TrimPrefixCI(p.cur.String(), "#m")
	q1 := values.NewBigFloatFromString(s)
	if q1 == nil {
		return nil, p.cur, NewParserErrorf(p.cur, "invalid big float: %s", p.cur.String())
	}
	q := p.wrapSyntax(q1, p.cur)
	return q, p.cur, nil
}

// parseSignedInf parses a signed infinity token (+inf.0 or -inf.0).
// It cannot fail, so it returns no error.
func (p *Parser) parseSignedInf() (syntax.SyntaxValue, tokenizer.Token) {
	s := p.cur.String()
	var f float64
	if strings.HasPrefix(s, "-") {
		f = math.Inf(-1)
	} else {
		f = math.Inf(1)
	}
	q := p.wrapSyntax(values.NewFloat(f), p.cur)
	return q, p.cur
}

// parseSignedNan parses a signed NaN token (+nan.0 or -nan.0).
// It cannot fail, so it returns no error.
func (p *Parser) parseSignedNan() (syntax.SyntaxValue, tokenizer.Token) {
	q := p.wrapSyntax(values.NewFloat(math.NaN()), p.cur)
	return q, p.cur
}

// parseImaginaryInf parses an imaginary infinity token (+inf.0i or -inf.0i).
// It cannot fail, so it returns no error.
func (p *Parser) parseImaginaryInf() (syntax.SyntaxValue, tokenizer.Token) {
	s := p.cur.String()
	var img float64
	if strings.HasPrefix(s, "-") {
		img = math.Inf(-1)
	} else {
		img = math.Inf(1)
	}
	q := p.wrapSyntax(values.NewComplexFromParts(0, img), p.cur)
	return q, p.cur
}

// parseImaginaryNan parses an imaginary NaN token (+nan.0i or -nan.0i).
// It cannot fail, so it returns no error.
func (p *Parser) parseImaginaryNan() (syntax.SyntaxValue, tokenizer.Token) {
	q := p.wrapSyntax(values.NewComplexFromParts(0, math.NaN()), p.cur)
	return q, p.cur
}
