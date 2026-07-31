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
				q := p.wrapSyntax(MakeInexactNumber(q1), p.cur)
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
				q := p.wrapSyntax(MakeInexactNumber(q1), p.cur)
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
		q := p.wrapSyntax(MakeInexactNumber(q1), p.cur)
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

// widenToBigInteger implements #z as a datum introducer: read one number datum,
// then widen it to BigInteger.
//
// This is the whole of #z's contribution, and deliberately so. Radix, exactness,
// and digit-set validation are all inherited, because the datum was read by the
// ordinary number reader — #z never looks at a digit. #z#x1f is 31, #z#e#x1f
// works without #z knowing #e exists, and #z#b19 fails because #b19 does.
//
// The one constraint #z adds is that the datum denote an exact integer, which is
// what a BigInteger is; #m is the prefix for the inexact side. That guard is the
// failure mode of this rework: replacing the old inline base-10 scan with "read
// a datum and widen it" invites accepting any number and truncating it, so
// #z1.5, #z1e3, #z#x1.8, and #z#i#x1f all have to stay errors.
//
// #z is a coercion, not a container, so it does not nest: #z#z5 is 5, where the
// container introducer #& gives #&#&5 two boxes deep. That falls out of the
// BigInteger arm being the identity.
func (p *Parser) widenToBigInteger(stx syntax.SyntaxValue, _ tokenizer.Token) (syntax.SyntaxValue, error) {
	switch v := stx.Unwrap().(type) {
	case *values.BigInteger:
		return stx, nil
	case *values.Integer:
		return p.rewrapSyntax(stx, values.NewBigIntegerFromInt64(v.Value)), nil
	default:
		return nil, werr.WrapForeignErrorf(werr.ErrNotANumber,
			"widenToBigInteger: #z requires an exact integer datum, got %T", v)
	}
}

// widenToBigFloat implements #m as a datum introducer, the inexact counterpart
// of widenToBigInteger: read one real datum, then widen it to BigFloat.
//
// The datum must be real — a complex number has no BigFloat image — but need not
// be inexact: #m5 is 5.0 at 256-bit precision, as it was before #m became an
// introducer.
//
// Precision is the value's, not the literal's. #m#x1.8 widens the Float 1.5, so
// it carries float64 precision rather than 256 bits, while the one-token decimal
// spelling #m1.2345678901234567890123456789 keeps every digit. Consulting how
// the operand was spelled is exactly what the introducer model gives up, and
// re-reading the inner token to recover it would make #m inspect its operand's
// notation. Precision beyond float64 is available through the unprefixed decimal
// spelling or the l marker.
func (p *Parser) widenToBigFloat(stx syntax.SyntaxValue, _ tokenizer.Token) (syntax.SyntaxValue, error) {
	switch v := stx.Unwrap().(type) {
	case *values.BigFloat:
		return stx, nil
	case *values.Float:
		return p.rewrapSyntax(stx, values.NewBigFloatFromFloat64(v.Value)), nil
	case *values.Integer:
		bf := new(big.Float).SetPrec(values.DefaultBigFloatPrecision).SetInt64(v.Value)
		return p.rewrapSyntax(stx, values.NewBigFloat(bf)), nil
	case *values.BigInteger:
		bf := new(big.Float).SetPrec(values.DefaultBigFloatPrecision).SetInt(v.BigInt())
		return p.rewrapSyntax(stx, values.NewBigFloat(bf)), nil
	case *values.Rational:
		bf := new(big.Float).SetPrec(values.DefaultBigFloatPrecision).SetRat(v.Rat())
		return p.rewrapSyntax(stx, values.NewBigFloat(bf)), nil
	default:
		return nil, werr.WrapForeignErrorf(werr.ErrNotANumber,
			"widenToBigFloat: #m requires a real datum, got %T", v)
	}
}

// parseScientificNotation parses a number in scientific notation (e.g., "1e10", "+2e-5").
// Per R7RS §7.1.1, the exponent marker indicates inexact notation, so scientific
// notation always produces an inexact number: a Float, or a BigFloat when the
// magnitude is outside float64 range. The #e prefix can convert to exact after
// parsing.
func (p *Parser) parseScientificNotation() (syntax.SyntaxValue, tokenizer.Token, error) {
	s := replaceHashDigits(p.cur.String())

	// A scientific-notation token must carry an exponent marker (e/E or a short
	// s/f/d/l form); its absence means a plain number was mis-routed here.
	if schemeutil.IndexExponentMarker(s) == -1 {
		return nil, p.cur, NewParserErrorf(p.cur, "invalid scientific notation: %s", s)
	}

	// Out-of-range magnitudes promote to BigFloat inside ParseRealFloatString;
	// wrapping keeps the ErrInvalidFormat cause reachable via errors.Is/Unwrap.
	n, err := ParseRealFloatString(s)
	if err != nil {
		return nil, p.cur, NewParserErrorWithWrapf(err, p.cur, "invalid scientific notation: %s", s)
	}
	q := p.wrapSyntax(n, p.cur)
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
	// A radix prefix requires a number after it, so input ending on the prefix
	// (e.g. "#b") is malformed, not a clean EOF. The prefix token is also the
	// only located token still in hand — p.cur is nil once the tokenizer hits
	// EOF — so it carries the error's provenance.
	markerTok := p.curr()
	err := p.advance()
	if err != nil {
		return nil, p.cur, wrapMidParseEOF(err, markerTok, "radix number marker")
	}

	// Check for trailing exactness prefix: #x#e or #x#i.
	exactness := 0 // 0 = unspecified, 1 = exact (#e), -1 = inexact (#i)
	switch p.cur.Type() {
	case tokenizer.TokenizerStateMarkerNumberExact:
		exactness = 1
		err = p.advance()
		if err != nil {
			return nil, p.cur, wrapMidParseEOF(err, markerTok, "radix number marker")
		}
	case tokenizer.TokenizerStateMarkerNumberInexact:
		exactness = -1
		err = p.advance()
		if err != nil {
			return nil, p.cur, wrapMidParseEOF(err, markerTok, "radix number marker")
		}
	}

	// Parse the number in the given base.
	var q syntax.SyntaxValue
	var tok tokenizer.Token
	switch p.cur.Type() {
	case tokenizer.TokenizerStateUnsignedRationalFraction,
		tokenizer.TokenizerStateSignedRationalFraction:
		q, tok, err = p.parseRationalWithBase(base)
	case tokenizer.TokenizerStateUnsignedDecimalFraction,
		tokenizer.TokenizerStateSignedDecimalFraction:
		q, tok, err = p.parseDecimalFractionWithBase(base)
	default:
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
// method adds only the reader's contribution: a source-located error that wraps
// the grammar's reject cause, keeping it reachable via errors.Is / errors.Unwrap.
func (p *Parser) parseImaginary(s string) (values.Number, error) {
	q, err := ParseImaginaryStringNumber(s)
	if err != nil {
		return nil, NewParserErrorWithWrapf(err, p.cur, "invalid imaginary number: %s", s)
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

// isRationalString reports whether s has the coarse shape of a rational literal:
// exactly one '/' with a non-empty part on each side. It is a necessary, NOT a
// sufficient, gate — parseExactPart (big.Rat.SetString) stays the authority on
// rational validity, so double-checking there is still required.
//
// The tightening (over a bare strings.Contains(s, "/")) only prunes structurally
// broken shapes that the inexact fallback also rejects: multiple slashes
// ("1/2/3"), and empty parts ("/", "3/", "/4"). This shrinks the set of strings
// that pass isExactPartString yet fail parseExactPart. It deliberately does NOT
// require integer parts: a single-slash string with a float part ("1.5/3") must
// stay on the exact path so parseExactPart rejects it — routing it to the inexact
// float path (parseFloatOrInfnan) would WRONGLY accept it as 0.5.
func isRationalString(s string) bool {
	num, den, ok := strings.Cut(s, "/")
	if !ok {
		return false
	}
	if num == "" || den == "" {
		return false
	}
	return !strings.Contains(den, "/")
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
// This method adds only the reader's contribution: a source-located error that
// wraps the grammar's reject cause, keeping it reachable via errors.Is / Unwrap.
func (p *Parser) parseComplex(s string) (values.Number, error) {
	q, err := ParseComplexStringNumber(s)
	if err != nil {
		return nil, NewParserErrorWithWrapf(err, p.cur, "invalid complex number: %s", s)
	}
	return q, nil
}

// parseFloatOrInfnan parses a float that may be inf.0, nan.0, or a rational
func parseFloatOrInfnan(s string) (float64, error) {
	switch s {
	case values.PositiveInfinityString:
		return math.Inf(1), nil
	case values.NegativeInfinityString:
		return math.Inf(-1), nil
	case values.NaNString, values.NegativeNaNString:
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
func (p *Parser) convertWrappedNumber(stx syntax.SyntaxValue, op string, convert func(values.Number) (values.Number, error)) (syntax.SyntaxValue, error) {
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

// makeExact converts a syntax-wrapped number to its exact representation,
// implementing the reader's #e prefix. The conversion itself lives in
// MakeExactNumber (number_string.go), shared with string->number; this method
// contributes only the syntax unwrap/rewrap and the reader's error policy.
func (p *Parser) makeExact(stx syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return p.convertWrappedNumber(stx, "makeExact", MakeExactNumber)
}

// makeExactLiteral implements the reader's #e prefix on a bare (non-radix-
// prefixed) datum, where the literal's source text is still available and
// therefore authoritative: #e1e400 is 10^400, not the binary float 1e400 names.
// See MakeExactFromLiteral. tok is the datum's own token, whose '#' digit
// placeholders are substituted before the text is re-read.
//
// A non-decimal datum is routed to the value instead, because MakeExactFromLiteral
// re-reads the text as a *decimal* rational: it would turn #e#x1.8 into 9/5 where
// #x#e1.8 (the same prefix pair, the other order) gives 3/2. The two orders name
// one number, so the digits path is only correct where the digits are decimal.
// Bases 2, 8, and 16 lose nothing by it — they are powers of two, so the literal
// is already exactly the float that was parsed.
func (p *Parser) makeExactLiteral(stx syntax.SyntaxValue, tok tokenizer.Token) (syntax.SyntaxValue, error) {
	base := tok.Radix()
	if base != 0 && base != 10 {
		return p.makeExact(stx)
	}
	return p.convertWrappedNumber(stx, "makeExact", func(num values.Number) (values.Number, error) {
		return MakeExactFromLiteral(replaceHashDigits(tok.String()), num)
	})
}

// makeInexactLiteral implements the reader's #i prefix. Unlike #e, inexactness
// discards information rather than recovering it, so the source text adds
// nothing and the token is ignored.
func (p *Parser) makeInexactLiteral(stx syntax.SyntaxValue, _ tokenizer.Token) (syntax.SyntaxValue, error) {
	return p.makeInexact(stx)
}

// makeInexact converts a syntax-wrapped number to its inexact representation,
// implementing the reader's #i prefix. Delegates to MakeInexactNumber
// (number_string.go), which cannot fail.
func (p *Parser) makeInexact(stx syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return p.convertWrappedNumber(stx, "makeInexact", func(num values.Number) (values.Number, error) {
		return MakeInexactNumber(num), nil
	})
}

// parseDecimalFraction parses a decimal fraction token (e.g., "1.5", "-0.3").
func (p *Parser) parseDecimalFraction() (syntax.SyntaxValue, tokenizer.Token, error) {
	n, err := ParseRealFloatString(replaceHashDigits(p.cur.String()))
	if err != nil {
		return nil, p.cur, NewParserErrorWithWrapf(err, p.cur, "invalid decimal fraction: %s", p.cur.String())
	}
	q := p.wrapSyntax(n, p.cur)
	return q, p.cur, nil
}

// parseDecimalFractionWithBase parses a fraction token whose digits are in the
// given base, e.g. "1.8" under #x (1.5). The tokenizer already scanned the
// digits against that radix and recorded it on the token, so the digit set is
// enforced upstream; this only converts.
//
// R7RS §6.2.4: a literal written with a decimal point is inexact, whatever its
// radix, so this always yields a Float (or a BigFloat past float64 range).
func (p *Parser) parseDecimalFractionWithBase(base int) (syntax.SyntaxValue, tokenizer.Token, error) {
	n, err := ParseRealFloatStringWithBase(replaceHashDigits(p.cur.String()), base)
	if err != nil {
		return nil, p.cur, NewParserErrorWithWrapf(err, p.cur, "invalid base-%d decimal fraction: %s", base, p.cur.String())
	}
	q := p.wrapSyntax(n, p.cur)
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
