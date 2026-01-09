// Copyright 2025 Aaron Alpar
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
	"io"
	"math"
	"math/big"
	"math/bits"
	"strconv"
	"strings"
	"unicode"
	"unicode/utf8"

	"wile/environment"
	"wile/syntax"
	"wile/tokenizer"
	"wile/values"
)

const (
	ParserNumberDefaultBase = 10
)

// Quote form identifiers.
const (
	ConstQuote            = "quote"
	ConstQuasiquote       = "quasiquote"
	ConstUnquote          = "unquote"
	ConstUnquoteSplicing  = "unquote-splicing"
	ConstSyntax           = "syntax"
	ConstQuasisyntax      = "quasisyntax"
	ConstUnsyntax         = "unsyntax"
	ConstUnsyntaxSplicing = "unsyntax-splicing"
)

// Character mnemonic runes.
const (
	RuneAlarm       = rune('\a')
	RuneSpace       = rune(' ')
	RuneBackspace   = rune('\b')
	RuneFormFeed    = rune('\f')
	RuneRubout      = rune(127)
	RuneEscape      = rune(27)
	RuneNewline     = rune('\n')
	RuneNull        = rune(0)
	RuneReturn      = rune('\r')
	RuneTab         = rune('\t')
	RuneVerticalTab = rune('\v')
)

// Parser represents a R7RS compliant Scheme syntax parser.
type Parser struct {
	rdr         io.RuneReader // the rune reader
	env         *environment.EnvironmentFrame
	toks        *tokenizer.Tokenizer
	cur         tokenizer.Token
	err         error
	skipComment bool
	file        string // source file name for error reporting
}

// NewParser creates a new parser for the given reader and environment.
func NewParser(env *environment.EnvironmentFrame, skipComments bool, rdr io.RuneReader) *Parser {
	return NewParserWithFile(env, skipComments, rdr, "")
}

// NewParserWithFile creates a new parser with a specified source filename.
func NewParserWithFile(env *environment.EnvironmentFrame, skipComments bool, rdr io.RuneReader, file string) *Parser {
	q := &Parser{
		env:         env,
		rdr:         rdr,
		skipComment: skipComments,
		file:        file,
	}
	return q
}

func (p *Parser) curr() tokenizer.Token {
	return p.cur
}

// Text returns the current text being parsed.
func (p *Parser) Text() string {
	return p.toks.Text()
}

// ReadSyntax reads and returns the next syntax value from the input.
func (p *Parser) ReadSyntax(_ context.Context) (syntax.SyntaxValue, error) {
	if p.toks == nil {
		p.toks = tokenizer.NewTokenizerWithComments(p.rdr, false)
		p.cur, p.err = p.toks.Next()
	}
	if p.err != nil {
		return nil, p.err
	}
	var (
		q   syntax.SyntaxValue
		err error
	)

	for {
		q, _, err = p.readSyntax()
		if err != nil {
			p.toks = nil
			p.err = err
			return nil, p.err
		}
		// Advance to the next token for the next ReadSyntax() call
		p.cur, p.err = p.toks.Next()
		// EOF is fine - it means there's nothing more to read
		if p.err != nil && p.err != io.EOF {
			p.toks = nil
			return nil, p.err
		}
		if !p.skipComment {
			return q, nil
		}
		switch q.(type) {
		case *syntax.SyntaxComment, *syntax.SyntaxDatumComment:
		default:
			return q, nil
		}
	}
}

// newSourceContext creates a SourceContext from a token.
func (p *Parser) newSourceContext(t tokenizer.Token) *syntax.SourceContext {
	return syntax.NewSourceContext(t.String(), p.file, t.Start(), t.End())
}

func (p *Parser) wrapSyntax(o values.Value, t tokenizer.Token) *syntax.SyntaxObject {
	return syntax.NewSyntaxObject(o, p.newSourceContext(t))
}

func (p *Parser) wrapSyntaxSymbol(o string, t tokenizer.Token) *syntax.SyntaxSymbol {
	// Intern the symbol at parse time, colocating interning with syntax occurrence
	sym := p.env.InternSymbol(values.NewSymbol(o))
	return syntax.NewSyntaxSymbolForSymbol(sym, p.newSourceContext(t))
}

func (p *Parser) wrapSyntaxVector(os []values.Value, t tokenizer.Token) *syntax.SyntaxVector {
	sc := p.newSourceContext(t)
	svs := make([]syntax.SyntaxValue, len(os))
	for i, v := range os {
		svs[i] = p.wrapSyntax(v, t)
	}
	return syntax.NewSyntaxVector(sc, svs...)
}

func (p *Parser) wrapSyntaxEmptyList(t tokenizer.Token) *syntax.SyntaxPair {
	return syntax.NewSyntaxEmptyList(p.newSourceContext(t))
}

func (p *Parser) wrapSyntaxPair(v0, v1 syntax.SyntaxValue, t tokenizer.Token) *syntax.SyntaxPair {
	return syntax.NewSyntaxCons(v0, v1, p.newSourceContext(t))
}

func (p *Parser) wrapSyntaxComment(v0 string, t tokenizer.Token) *syntax.SyntaxComment {
	return syntax.NewSyntaxComment(v0, p.newSourceContext(t))
}

func (p *Parser) wrapSyntaxDatumComment(v0 string, v1 syntax.SyntaxValue, t tokenizer.Token) *syntax.SyntaxDatumComment {
	return syntax.NewSyntaxDatumComment(v0, v1, p.newSourceContext(t))
}

func (p *Parser) wrapSyntaxDatumLabel(v0 int, t tokenizer.Token) *syntax.SyntaxDatumLabel {
	return syntax.NewSyntaxDatumLabel(v0, p.newSourceContext(t))
}

func (p *Parser) wrapSyntaxDatumLabelAssignment(v0 int, v1 syntax.SyntaxValue, t tokenizer.Token) *syntax.SyntaxDatumLabelAssignment {
	return syntax.NewSyntaxDatumLabelAssignment(v0, v1, p.newSourceContext(t))
}

func (p *Parser) wrapSyntaxDirective(v0 string, t tokenizer.Token) *syntax.SyntaxDirective {
	return syntax.NewSyntaxDirective(v0, p.newSourceContext(t))
}

// readQuoteForm reads a quote-like form (quote, unquote, quasiquote, etc.).
// It advances the tokenizer, reads the next datum, and wraps it in a list
// with the given keyword symbol.
func (p *Parser) readQuoteForm(keyword string) (syntax.SyntaxValue, tokenizer.Token, error) {
	p.cur, p.err = p.toks.Next()
	if p.err != nil {
		return nil, p.cur, p.err
	}
	q, _, err := p.readSyntax()
	if err != nil {
		return nil, p.cur, err
	}
	sym := p.wrapSyntaxSymbol(keyword, p.cur)
	result := p.listSyntax(p.cur, sym, q)
	return result, p.cur, nil
}

// parseIntegerWithBase parses the current token as an integer with the given base.
func (p *Parser) parseIntegerWithBase(base int) (syntax.SyntaxValue, tokenizer.Token, error) {
	a, err := strconv.ParseInt(p.cur.String(), base, 64)
	if err != nil {
		return nil, p.cur, err
	}
	q := p.wrapSyntax(values.NewInteger(a), p.cur)
	return q, p.cur, nil
}

// parseBigIntegerWithBase parses the current token as a big integer with the given base.
// It strips the #z or #Z prefix before parsing.
func (p *Parser) parseBigIntegerWithBase(base int) (syntax.SyntaxValue, tokenizer.Token, error) {
	s := TrimPrefix(p.cur.String(), "#z")
	s = TrimPrefix(s, "#Z")
	q1 := values.NewBigIntegerFromString(s, base)
	if q1 == nil {
		return nil, p.cur, NewParserErrorf(p.cur, "invalid big integer: %s", p.cur.String())
	}
	q := p.wrapSyntax(q1, p.cur)
	return q, p.cur, nil
}

func (p *Parser) readSyntax() (syntax.SyntaxValue, tokenizer.Token, error) {
	var q syntax.SyntaxValue
	cur := p.curr()
	switch cur.Type() {
	case tokenizer.TokenizerStateCons:
		return nil, p.cur, NewParserErrorWithWrap(values.ErrNotACons, p.cur, "unexpected '.' token")
	case tokenizer.TokenizerStateLabelAssignment:
		s := TrimPrefix(p.cur.String(), "#")
		is := strings.TrimSuffix(s, "=")
		var i int64
		i, p.err = strconv.ParseInt(is, 10, bits.UintSize)
		if p.err != nil {
			return nil, p.cur, p.err
		}
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		var v syntax.SyntaxValue
		v, _, p.err = p.readSyntax()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		q0 := p.wrapSyntaxDatumLabelAssignment(int(i), v, p.cur)
		return q0, p.cur, nil
	case tokenizer.TokenizerStateLabelReference:
		s := strings.Trim(p.cur.String(), "#")
		var i int64
		i, p.err = strconv.ParseInt(s, 10, bits.UintSize)
		if p.err != nil {
			return nil, p.cur, p.err
		}
		q = p.wrapSyntaxDatumLabel(int(i), p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateDirective:
		q = p.wrapSyntaxDirective(TrimPrefix(p.cur.String(), "#!"), p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateLineCommentBody:
		q = p.wrapSyntaxComment(p.cur.String(), p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateBlockCommentBody:
		q = p.wrapSyntaxComment(p.cur.String(), p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateDatumCommentBegin:
		beginTok := p.cur
		// Skip the datum comment begin token
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		// Read the syntax value being commented
		var v syntax.SyntaxValue
		v, _, p.err = p.readSyntax()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		// Use beginTok.String() for correct label, but p.cur for source context (matches old behavior)
		q = p.wrapSyntaxDatumComment(beginTok.String(), v, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateOpenParen:
		var pr syntax.SyntaxValue
		pr = p.wrapSyntaxPair(nil, nil, p.cur)
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		q0 := pr
		pr0 := p.wrapSyntaxPair(nil, nil, p.cur)
		for p.cur.Type() != tokenizer.TokenizerStateCloseParen && p.cur.Type() != tokenizer.TokenizerStateCons {
			var v syntax.SyntaxValue
			v, _, p.err = p.readSyntax()
			if p.err != nil {
				return nil, p.cur, p.err
			}
			pr0 = pr.(*syntax.SyntaxPair)
			pr0.SetCar(v)
			pr0.SetCdr(p.wrapSyntaxPair(nil, nil, p.cur))
			pr = pr0.Cdr().(*syntax.SyntaxPair)
			p.cur, p.err = p.toks.Next()
			if p.err != nil {
				return nil, p.cur, p.err
			}
		}
		if p.cur.Type() == tokenizer.TokenizerStateCons {
			// skip the '.' token
			p.cur, p.err = p.toks.Next()
			if p.err != nil {
				return nil, p.cur, p.err
			}
			// read first value in list
			var v syntax.SyntaxValue
			v, _, p.err = p.readSyntax()
			if p.err != nil {
				return nil, p.cur, p.err
			}
			pr = v.(syntax.SyntaxValue)
			// ??
			p.cur, p.err = p.toks.Next()
			if p.err != nil {
				return nil, p.cur, p.err
			}
			if p.cur.Type() != tokenizer.TokenizerStateCloseParen {
				return nil, p.cur, NewParserErrorWithWrapf(values.ErrNotACloseParen, p.cur, "expected close parenthesis after dotted pair, got %s", p.cur.String())
			}
			pr0.SetCdr(pr)
		} else {
			// CloseParen was not found, so we assume the list is empty.
			pr = p.wrapSyntaxEmptyList(p.cur)
			pr0.SetCdr(pr)
		}
		return q0, p.cur, nil
	case tokenizer.TokenizerStateOpenVector:
		q0 := p.wrapSyntaxVector(nil, p.cur)
		var v syntax.SyntaxValue
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		v, _, p.err = p.readSyntax()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		for p.err == nil && p.curr().Type() != tokenizer.TokenizerStateCloseParen {
			q0.Values = append(q0.Values, v)
			p.cur, p.err = p.toks.Next()
			if p.err != nil {
				return nil, p.cur, p.err
			}
			v, _, p.err = p.readSyntax()
			if p.err != nil {
				return nil, p.cur, p.err
			}
		}
		if p.err != nil {
			return nil, p.cur, p.err
		}
		return q0, p.cur, nil
	case tokenizer.TokenizerStateOpenVectorUnsignedByteMarker:
		var stx syntax.SyntaxValue
		q0 := values.NewByteVector()
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		stx, _, p.err = p.readSyntax()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		if p.curr().Type() == tokenizer.TokenizerStateCloseParen {
			break
		}
		i, ok := stx.Unwrap().(*values.Integer)
		for {
			if !ok {
				return nil, p.cur, NewParserErrorWithWrapf(values.ErrNotAnInteger, p.cur, "expected unsigned byte integer in byte vector, got %T", stx.Unwrap())
			}
			*q0 = append(*q0, *values.NewByte(uint8(i.Value)))
			p.cur, p.err = p.toks.Next()
			if p.err != nil {
				return nil, p.cur, p.err
			}
			stx, _, p.err = p.readSyntax()
			if p.err != nil {
				return nil, p.cur, p.err
			}
			if p.curr().Type() == tokenizer.TokenizerStateCloseParen {
				break
			}
			i, ok = stx.Unwrap().(*values.Integer)
		}
		if p.err != nil {
			return nil, p.cur, p.err
		}
		q = p.wrapSyntax(q0, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateUnquote:
		return p.readQuoteForm(ConstUnquote)
	case tokenizer.TokenizerStateQuasiquote:
		return p.readQuoteForm(ConstQuasiquote)
	case tokenizer.TokenizerStateUnquoteSplicing:
		return p.readQuoteForm(ConstUnquoteSplicing)
	case tokenizer.TokenizerStateQuote:
		// Quote uses the pre-advance token for source location
		t := p.curr()
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		q, _, p.err = p.readSyntax()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		q1 := p.wrapSyntaxSymbol(ConstQuote, t)
		q2 := p.listSyntax(t, q1, q)
		return q2, p.cur, nil
	case tokenizer.TokenizerStateUnsyntax:
		return p.readQuoteForm(ConstUnsyntax)
	case tokenizer.TokenizerStateQuasisyntax:
		return p.readQuoteForm(ConstQuasisyntax)
	case tokenizer.TokenizerStateUnsyntaxSplicing:
		return p.readQuoteForm(ConstUnsyntaxSplicing)
	case tokenizer.TokenizerStateSyntax:
		return p.readQuoteForm(ConstSyntax)
	case tokenizer.TokenizerStateSymbol:
		q = p.wrapSyntaxSymbol(p.cur.String(), p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateUnsignedInteger:
		var a int64
		a, p.err = strconv.ParseInt(p.cur.String(), 10, 64)
		if p.err != nil {
			return nil, p.cur, p.err
		}
		q1 := values.NewInteger(a)
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateUnsignedDecimalFraction:
		var a float64
		a, p.err = strconv.ParseFloat(p.cur.String(), 64)
		if p.err != nil {
			return nil, p.cur, p.err
		}
		q1 := values.NewFloat(a)
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateSignedInteger:
		// Signed integers like "-40", "+40"
		var a int64
		a, p.err = strconv.ParseInt(p.cur.String(), 10, 64)
		if p.err != nil {
			return nil, p.cur, p.err
		}
		q1 := values.NewInteger(a)
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateSignedDecimalFraction:
		// Signed decimal fractions like "-3.24", "+3.24"
		var a float64
		a, p.err = strconv.ParseFloat(p.cur.String(), 64)
		if p.err != nil {
			return nil, p.cur, p.err
		}
		q1 := values.NewFloat(a)
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateUnsignedRationalFraction:
		// Unsigned rational fractions like "3/4"
		var q1 *values.Rational
		q1, p.err = p.parseRational(p.cur.String())
		if p.err != nil {
			return nil, p.cur, p.err
		}
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateSignedRationalFraction:
		// Signed rational fractions like "-1/2", "+3/4"
		var q1 *values.Rational
		q1, p.err = p.parseRational(p.cur.String())
		if p.err != nil {
			return nil, p.cur, p.err
		}
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateMarkerBase2:
		// #b prefix - next token is base 2 integer
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		var a int64
		a, p.err = strconv.ParseInt(p.cur.String(), 2, 64)
		if p.err != nil {
			return nil, p.cur, p.err
		}
		q1 := values.NewInteger(a)
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateMarkerBase8:
		// #o prefix - next token is base 8 integer
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		var a int64
		a, p.err = strconv.ParseInt(p.cur.String(), 8, 64)
		if p.err != nil {
			return nil, p.cur, p.err
		}
		q1 := values.NewInteger(a)
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateMarkerBase10:
		// #d prefix - next token is base 10 integer
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		return p.readSyntax()
	case tokenizer.TokenizerStateMarkerBase16:
		// #x prefix - next token is base 16 integer
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		return p.parseIntegerWithBase(16)
	case tokenizer.TokenizerStateSignedIntegerBase2, tokenizer.TokenizerStateUnsignedIntegerBase2:
		return p.parseIntegerWithBase(2)
	case tokenizer.TokenizerStateSignedIntegerBase8, tokenizer.TokenizerStateUnsignedIntegerBase8:
		return p.parseIntegerWithBase(8)
	case tokenizer.TokenizerStateSignedIntegerBase10, tokenizer.TokenizerStateUnsignedIntegerBase10:
		return p.parseIntegerWithBase(10)
	case tokenizer.TokenizerStateSignedIntegerBase16, tokenizer.TokenizerStateUnsignedIntegerBase16:
		return p.parseIntegerWithBase(16)
	case tokenizer.TokenizerStateSignedInf:
		// +inf.0 or -inf.0
		s := p.cur.String()
		var f float64
		if strings.HasPrefix(s, "-") {
			f = math.Inf(-1)
		} else {
			f = math.Inf(1)
		}
		q1 := values.NewFloat(f)
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateSignedNan:
		// +nan.0 or -nan.0
		q1 := values.NewFloat(math.NaN())
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateSignedImaginary:
		// Pure imaginary numbers like +3i, -2i, +i, -i
		var q1 *values.Complex
		q1, p.err = p.parseImaginary(p.cur.String())
		if p.err != nil {
			return nil, p.cur, p.err
		}
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateSignedImaginaryInf:
		// +inf.0i or -inf.0i
		s := p.cur.String()
		var img float64
		if strings.HasPrefix(s, "-") {
			img = math.Inf(-1)
		} else {
			img = math.Inf(1)
		}
		q1 := values.NewComplexFromParts(0, img)
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateSignedImaginaryNan:
		// +nan.0i or -nan.0i
		q1 := values.NewComplexFromParts(0, math.NaN())
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateUnsignedComplex, tokenizer.TokenizerStateSignedComplex:
		// Full complex numbers like 1+2i, 3-4i, 1.5+2.5i, +1+2i, -3-4i
		var q1 *values.Complex
		q1, p.err = p.parseComplex(p.cur.String())
		if p.err != nil {
			return nil, p.cur, p.err
		}
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateUnsignedComplexPolar, tokenizer.TokenizerStateSignedComplexPolar:
		// Polar complex numbers like 1@1.5708, +2@0.5, -3@1.0
		var q1 *values.Complex
		q1, p.err = p.parsePolarComplex(p.cur.String())
		if p.err != nil {
			return nil, p.cur, p.err
		}
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateUnsignedImaginary:
		// Pure imaginary numbers (unsigned, typically after radix prefix)
		var q1 *values.Complex
		q1, p.err = p.parseImaginary(p.cur.String())
		if p.err != nil {
			return nil, p.cur, p.err
		}
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateUnsignedImaginaryInf:
		// Unsigned imaginary infinity (typically after radix prefix)
		s := p.cur.String()
		var img float64
		if strings.HasPrefix(s, "-") {
			img = math.Inf(-1)
		} else {
			img = math.Inf(1)
		}
		q1 := values.NewComplexFromParts(0, img)
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateUnsignedImaginaryNan:
		// Unsigned imaginary NaN (typically after radix prefix)
		q1 := values.NewComplexFromParts(0, math.NaN())
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateMarkerNumberExact, tokenizer.TokenizerStateMarkerNumberInexact:
		// #e or #i prefix - just read the next number
		// For now, we don't track exactness in the value types
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		return p.readSyntax()
	case tokenizer.TokenizerStateBigIntegerDefaultBase:
		return p.parseBigIntegerWithBase(ParserNumberDefaultBase)
	case tokenizer.TokenizerStateBigIntegerBase10:
		return p.parseBigIntegerWithBase(10)
	case tokenizer.TokenizerStateBigIntegerBase16:
		return p.parseBigIntegerWithBase(16)
	case tokenizer.TokenizerStateBigIntegerBase8:
		return p.parseBigIntegerWithBase(8)
	case tokenizer.TokenizerStateBigIntegerBase2:
		return p.parseBigIntegerWithBase(2)
	case tokenizer.TokenizerStateBigFloat:
		// #M prefix for arbitrary-precision float
		s := TrimPrefix(p.cur.String(), "#m")
		s = TrimPrefix(s, "#M")
		q1 := values.NewBigFloatFromString(s)
		if q1 == nil {
			return nil, p.cur, NewParserErrorf(cur, "invalid big float: %s", p.cur.String())
		}
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateMarkerBooleanTrue:
		q1 := values.TrueValue
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateMarkerBooleanFalse:
		q1 := values.FalseValue
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateEmptyList:
		q = p.wrapSyntaxEmptyList(p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateCharGraphic:
		s := TrimPrefix(p.cur.String(), values.PrefixCharacter)
		rs := []rune(s)
		q1 := values.NewCharacter(rs[0])
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateCharMnemonic:
		var q0 *values.Character
		s := TrimPrefix(p.cur.String(), values.PrefixCharacter)
		switch s {
		case "alarm":
			q0 = values.NewCharacter(RuneAlarm)
		case "space":
			q0 = values.NewCharacter(RuneSpace)
		case "backspace":
			q0 = values.NewCharacter(RuneBackspace)
		case "form-feed":
			q0 = values.NewCharacter(RuneFormFeed)
		case "delete":
			q0 = values.NewCharacter(RuneRubout)
		case "escape":
			q0 = values.NewCharacter(RuneEscape)
		case "newline":
			q0 = values.NewCharacter(RuneNewline)
		case "null":
			q0 = values.NewCharacter(RuneNull)
		case "return":
			q0 = values.NewCharacter(RuneReturn)
		case "tab":
			q0 = values.NewCharacter(RuneTab)
		case "vertical-tab":
			q0 = values.NewCharacter(RuneVerticalTab)
		default:
			return nil, nil, NewParserErrorWithWrapf(values.ErrUnknownCharacterMnemonic, p.cur, "unknown character mnemonic: %s", s)
		}
		q = p.wrapSyntax(q0, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateCharHexEscape:
		s := TrimPrefix(p.cur.String(), "#\\x")
		var i int64
		i, p.err = strconv.ParseInt(s, 16, 32)
		if p.err != nil {
			return nil, p.cur, NewParserErrorWithWrapf(p.err, p.cur, "invalid character hex escape: %s", s)
		}
		q1 := values.NewCharacter(rune(i))
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateString:
		// Use Value() to get the processed string (escapes converted, quotes stripped)
		q1 := values.NewString(p.cur.Value())
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateCloseParen:
		return q, p.cur, nil
	}
	return q, nil, NewParserErrorWithWrapf(ErrUnknownTokenType, p.cur, "unknown token type: %s", p.cur.String())
}

func (p *Parser) listSyntax(t tokenizer.Token, os ...syntax.SyntaxValue) syntax.SyntaxValue {
	l := len(os)
	switch l {
	case 0:
		return p.wrapSyntaxEmptyList(t)
	case 1:
		return p.wrapSyntaxPair(os[0], p.wrapSyntaxEmptyList(t), t)
	}
	q := p.wrapSyntaxPair(os[0], p.wrapSyntaxPair(nil, nil, t), t)
	curr := q
	for _, v := range os[1:] {
		curr = curr.Cdr().(*syntax.SyntaxPair)
		curr.SetCar(v)
		curr.SetCdr(p.wrapSyntaxPair(nil, nil, t))
	}
	curr.SetCdr(p.wrapSyntaxEmptyList(t))
	return q
}

// Close closes the parser and releases resources.
func (p *Parser) Close() error {
	if p.toks == nil {
		return NewParserErrorWithWrapf(ErrAlreadyClosed, nil, "parser already closed")
	}
	err := p.toks.Close()
	p.toks = nil
	return err
}

// parseRational parses a rational number string like "3/4" or "-1/2"
func (p *Parser) parseRational(s string) (*values.Rational, error) {
	r := new(big.Rat)
	_, ok := r.SetString(s)
	if !ok {
		return nil, NewParserErrorf(p.cur, "invalid rational number: %s", s)
	}
	return values.NewRationalFromRat(r), nil
}

// parseImaginary parses an imaginary number string like "+3i", "-2i", "+i", "-i"
func (p *Parser) parseImaginary(s string) (*values.Complex, error) {
	// Remove the trailing 'i'
	s = strings.TrimSuffix(s, "i")

	// Handle "+i" and "-i" cases
	if s == "+" || s == "" {
		return values.NewComplexFromParts(0, 1), nil
	}
	if s == "-" {
		return values.NewComplexFromParts(0, -1), nil
	}

	// Parse the numeric part
	imag, err := strconv.ParseFloat(s, 64)
	if err != nil {
		return nil, err
	}
	return values.NewComplexFromParts(0, imag), nil
}

// parsePolarComplex parses a polar complex number string like "1@1.5708", "+2@0.5", "-3@1.0"
// and converts it to rectangular form using: real = r*cos(θ), imag = r*sin(θ)
func (p *Parser) parsePolarComplex(s string) (*values.Complex, error) {
	// Find the @ separator
	atPos := strings.Index(s, "@")
	if atPos == -1 {
		return nil, NewParserErrorf(p.cur, "invalid polar complex number: %s (no @ separator found)", s)
	}

	// Split into magnitude and angle parts
	magPart := s[:atPos]
	anglePart := s[atPos+1:]

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
	real := mag * math.Cos(angle)
	imag := mag * math.Sin(angle)

	return values.NewComplexFromParts(real, imag), nil
}

// parseComplex parses a complex number string like "1+2i", "3-4i", "1.5+2.5i", "1+i", "5-i"
// Also handles infnan: "+inf.0+inf.0i", "1+inf.0i", "3+nan.0i"
func (p *Parser) parseComplex(s string) (*values.Complex, error) {
	// Remove the trailing 'i'
	s = strings.TrimSuffix(s, "i")

	// Find the position of the sign separating real and imaginary parts
	// Skip position 0 since the real part might start with a sign
	// Also need to handle infnan which contains '.' before the sign
	signPos := -1
	for i := 1; i < len(s); i++ {
		if s[i] == '+' || s[i] == '-' {
			// Make sure this isn't part of an exponent (e.g., 1e+10)
			if i > 0 && (s[i-1] == 'e' || s[i-1] == 'E') {
				continue
			}
			// Make sure this isn't the sign in inf.0 or nan.0 (after the '0')
			// Check if this could be the start of the imaginary part
			// by looking at what follows
			rest := s[i:]
			if strings.HasPrefix(rest, "+inf.0") || strings.HasPrefix(rest, "-inf.0") ||
				strings.HasPrefix(rest, "+nan.0") || strings.HasPrefix(rest, "-nan.0") ||
				strings.HasPrefix(rest, "+i") || strings.HasPrefix(rest, "-i") ||
				len(rest) > 1 && (rest[1] >= '0' && rest[1] <= '9' || rest[1] == '.') {
				signPos = i
				break
			}
			signPos = i
			break
		}
	}

	if signPos == -1 {
		return nil, NewParserErrorf(p.cur, "invalid complex number: %s (no sign separator found)", s+"i")
	}

	// Split into real and imaginary parts
	realPart := s[:signPos]
	imagPart := s[signPos:] // includes the sign

	// Parse real part
	real, err := p.parseRealPart(realPart)
	if err != nil {
		return nil, NewParserErrorf(p.cur, "invalid real part in complex number: %s", realPart)
	}

	// Parse imaginary part
	imag, err := p.parseImagPart(imagPart)
	if err != nil {
		return nil, NewParserErrorf(p.cur, "invalid imaginary part in complex number: %s", imagPart)
	}

	return values.NewComplexFromParts(real, imag), nil
}

// parseFloatOrInfnan parses a float that may be inf.0 or nan.0
func parseFloatOrInfnan(s string) (float64, error) {
	switch s {
	case "+inf.0":
		return math.Inf(1), nil
	case "-inf.0":
		return math.Inf(-1), nil
	case "+nan.0", "-nan.0":
		return math.NaN(), nil
	}
	return strconv.ParseFloat(s, 64)
}

// parseRealPart parses a real number that may be a float or infnan
func (p *Parser) parseRealPart(s string) (float64, error) {
	return parseFloatOrInfnan(s)
}

// parseImagPart parses an imaginary coefficient that may be a float, infnan, or just a sign
func (p *Parser) parseImagPart(s string) (float64, error) {
	switch s {
	case "+":
		return 1, nil
	case "-":
		return -1, nil
	}
	return parseFloatOrInfnan(s)
}

// runesEqualFold returns true if two runes are equal under case-folding.
// Returns false if either rune is an error.
func runesEqualFold(r0, r1 rune) bool {
	return r0 != utf8.RuneError && r1 != utf8.RuneError &&
		unicode.ToLower(r0) == unicode.ToLower(r1)
}

// TrimPrefix performs a case-insensitive trim of the specified prefix from the string s.
// If s does not start with the prefix (case-insensitive), s is returned unchanged.
// functionally,its identical to strings.TrimPrefix but case-insensitive.
func TrimPrefix(s, prefix string) string {
	if len(prefix) == 0 || len(prefix) > len(s) {
		return s
	}
	i := 0
	j := 0
	for j < len(prefix) {
		if i >= len(s) {
			return s
		}
		r0, n0 := utf8.DecodeRuneInString(s[i:])
		r1, n1 := utf8.DecodeRuneInString(prefix[j:])
		if !runesEqualFold(r0, r1) {
			return s
		}
		i += n0
		j += n1
	}
	return s[i:]
}

// TrimSuffix performs a case-insensitive trim of the specified suffix from the string s.
// If s does not end with the suffix (case-insensitive), s is returned unchanged.
// functionally,its identical to strings.TrimSuffix but case-insensitive.
func TrimSuffix(s, suffix string) string {
	if len(suffix) > len(s) || len(suffix) == 0 {
		return s
	}
	lasti := len(s)
	lastj := len(suffix)
	for lastj > 0 {
		r0, n0 := utf8.DecodeLastRuneInString(s[:lasti])
		r1, n1 := utf8.DecodeLastRuneInString(suffix[:lastj])
		if !runesEqualFold(r0, r1) {
			return s
		}
		lasti -= n0
		lastj -= n1
	}
	return s[:lasti]
}
