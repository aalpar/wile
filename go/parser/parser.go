package parser

import (
	"io"
	"math"
	"math/big"
	"math/bits"
	"skeme/environment"
	"skeme/syntax"
	"skeme/values"
	"strconv"
	"strings"

	"skeme/tokenizer"
)

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

var (
	ErrUnknownTokenType = values.NewStaticError("unknown token type")
)

type Parser struct {
	rdr         io.RuneReader // the rune reader
	env         *environment.EnvironmentFrame
	toks        *tokenizer.Tokenizer
	cur         tokenizer.Token
	err         error
	skipComment bool
}

func Parse(env *environment.EnvironmentFrame, rdr io.RuneReader) (syntax.SyntaxValue, error) {
	p := NewParser(env, rdr)
	return p.ReadSyntax()
}

func NewParser(env *environment.EnvironmentFrame, rdr io.RuneReader) *Parser {
	q := &Parser{
		env:         env,
		rdr:         rdr,
		skipComment: true,
	}
	return q
}

func (p *Parser) curr() tokenizer.Token {
	return p.cur
}

// func (p *Parser) next() (tokenizer.Token, error) {
func (p *Parser) next() {
brk:
	for p.skipComment && p.err == nil {
		switch p.curr().Type() {
		case tokenizer.TokenizerStateBlockComment, tokenizer.TokenizerStateLineCommentStart:
			p.cur, p.err = p.toks.Next()
		case tokenizer.TokenizerStateDatumComment:
			p.cur, p.err = p.toks.Next()
		default:
			break brk
		}
	}
	p.cur, p.err = p.toks.Next()
}

func (p *Parser) Text() string {
	return p.toks.Text()
}

func (p *Parser) ReadSyntax() (syntax.SyntaxValue, error) {
	if p.toks == nil {
		p.toks = tokenizer.NewTokenizerWithComments(p.rdr, false, !p.skipComment)
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.err
		}
	}
	q, _, err := p.readSyntax()
	if err != nil {
		p.toks = nil
		return nil, err
	}
	// Advance to the next token for the next ReadSyntax() call
	p.cur, p.err = p.toks.Next()
	// EOF is fine - it means there's nothing more to read
	if p.err != nil && p.err != io.EOF {
		p.toks = nil
		return nil, p.err
	}
	return q, nil
}

func (p *Parser) wrapSyntax(o values.Value, t tokenizer.Token) *syntax.SyntaxObject {
	sti := t.Start()
	eni := t.End()
	sc := syntax.NewSourceContext(t.String(), "", sti, eni)
	return syntax.NewSyntaxObject(o, sc)
}

func (p *Parser) wrapSyntaxSymbol(o string, t tokenizer.Token) *syntax.SyntaxSymbol {
	sti := t.Start()
	eni := t.End()
	sc := syntax.NewSourceContext(t.String(), "", sti, eni)
	return syntax.NewSyntaxSymbol(o, sc)
}

func (p *Parser) wrapSyntaxVector(os []values.Value, t tokenizer.Token) *syntax.SyntaxVector {
	sti := t.Start()
	eni := t.End()
	sc := syntax.NewSourceContext(t.String(), "", sti, eni)
	svs := make([]syntax.SyntaxValue, len(os))
	for i, v := range os {
		svs[i] = p.wrapSyntax(v, t)
	}
	return syntax.NewSyntaxVector(sc, svs...)
}

func (p *Parser) wrapSyntaxEmptyList(t tokenizer.Token) *syntax.SyntaxPair {
	sti := t.Start()
	eni := t.End()
	sc := syntax.NewSourceContext(t.String(), "", sti, eni)
	q := syntax.NewSyntaxEmptyList(sc)
	return q
}

func (p *Parser) wrapSyntaxPair(v0, v1 syntax.SyntaxValue, t tokenizer.Token) *syntax.SyntaxPair {
	sti := t.Start()
	eni := t.End()
	sc := syntax.NewSourceContext(t.String(), "", sti, eni)
	q := syntax.NewSyntaxCons(v0, v1, sc)
	return q
}

func (p *Parser) wrapSyntaxComment(v0 string, t tokenizer.Token) *syntax.SyntaxComment {
	sti := t.Start()
	eni := t.End()
	sc := syntax.NewSourceContext(t.String(), "", sti, eni)
	q := syntax.NewSyntaxComment(v0, sc)
	return q
}

func (p *Parser) wrapSyntaxDatumComment(v0 string, v1 syntax.SyntaxValue, t tokenizer.Token) *syntax.SyntaxDatumComment {
	sti := t.Start()
	eni := t.End()
	sc := syntax.NewSourceContext(t.String(), "", sti, eni)
	q := syntax.NewSyntaxDatumComment(v0, v1, sc)
	return q
}

func (p *Parser) wrapSyntaxDatumLabel(v0 int, t tokenizer.Token) *syntax.SyntaxDatumLabel {
	sti := t.Start()
	eni := t.End()
	sc := syntax.NewSourceContext(t.String(), "", sti, eni)
	q := syntax.NewSyntaxDatumLabel(v0, sc)
	return q
}

func (p *Parser) wrapSyntaxDatumLabelAssignment(v0 int, v1 syntax.SyntaxValue, t tokenizer.Token) *syntax.SyntaxDatumLabelAssignment {
	sti := t.Start()
	eni := t.End()
	sc := syntax.NewSourceContext(t.String(), "", sti, eni)
	q := syntax.NewSyntaxDatumLabelAssignment(v0, v1, sc)
	return q
}

func (p *Parser) wrapSyntaxDirective(v0 string, t tokenizer.Token) *syntax.SyntaxDirective {
	sti := t.Start()
	eni := t.End()
	sc := syntax.NewSourceContext(t.String(), "", sti, eni)
	q := syntax.NewSyntaxDirective(v0, sc)
	return q
}

func (p *Parser) readSyntax() (syntax.SyntaxValue, tokenizer.Token, error) {
	var q syntax.SyntaxValue
	if p.err != nil {
		return nil, nil, p.err
	}
	cur := p.curr()
	switch cur.Type() {
	case tokenizer.TokenizerStateCons:
		return nil, p.cur, values.ErrNotACons
	case tokenizer.TokenizerStateLabelAssignment:
		s := strings.TrimPrefix(p.cur.String(), "#")
		is := strings.TrimSuffix(s, "=")
		var i int64
		i, err := strconv.ParseInt(is, 10, bits.UintSize)
		if err != nil {
			return nil, p.cur, err
		}
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		v, _, err := p.readSyntax()
		if err != nil {
			return nil, p.cur, err
		}
		q0 := p.wrapSyntaxDatumLabelAssignment(int(i), v, p.cur)
		return q0, p.cur, nil
	case tokenizer.TokenizerStateLabelReference:
		s := strings.Trim(p.cur.String(), "#")
		var i int64
		i, err := strconv.ParseInt(s, 10, bits.UintSize)
		if err != nil {
			return nil, p.cur, err
		}
		q = p.wrapSyntaxDatumLabel(int(i), p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateDirective:
		q = p.wrapSyntaxDirective(strings.TrimPrefix(p.cur.String(), "#!"), p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateLineCommentStart:
		q = p.wrapSyntaxComment(strings.TrimPrefix(p.cur.String(), ";"), p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateLineCommentBegin:
		beginTok := p.cur
		body := ""
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		if p.cur.Type() == tokenizer.TokenizerStateLineCommentBody {
			body = p.cur.String()
			p.cur, p.err = p.toks.Next()
			// EOF is acceptable here - the comment ended at EOF without a newline
			if p.err != nil && p.err != io.EOF {
				return nil, p.cur, p.err
			}
		}
		// End token is present but we don't advance past it here
		q = p.wrapSyntaxComment(body, beginTok)
		return q, p.cur, nil
	case tokenizer.TokenizerStateBlockComment:
		q = p.wrapSyntaxComment(strings.TrimPrefix(p.cur.String(), "#|"), p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateBlockCommentBegin:
		beginTok := p.cur
		body := ""
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		if p.cur.Type() == tokenizer.TokenizerStateBlockCommentBody {
			body = p.cur.String()
			p.cur, p.err = p.toks.Next()
			if p.err != nil {
				return nil, p.cur, p.err
			}
		}
		// End token is present but we don't advance past it here
		q = p.wrapSyntaxComment(body, beginTok)
		return q, p.cur, nil
	case tokenizer.TokenizerStateDatumComment:
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		v, _, err := p.readSyntax()
		if err != nil {
			return nil, p.cur, err
		}
		q = p.wrapSyntaxDatumComment(p.cur.String(), v, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateDatumCommentBegin:
		beginTok := p.cur
		// Skip the datum comment begin token
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		// Read the syntax value being commented
		v, _, err := p.readSyntax()
		if err != nil {
			return nil, p.cur, err
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
			v, _, err := p.readSyntax()
			if err != nil {
				return nil, p.cur, err
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
			v, _, err := p.readSyntax()
			if err != nil {
				return nil, p.cur, err
			}
			pr = v.(syntax.SyntaxValue)
			// ??
			p.cur, p.err = p.toks.Next()
			if p.err != nil {
				return nil, p.cur, p.err
			}
			if p.cur.Type() != tokenizer.TokenizerStateCloseParen {
				return nil, p.cur, values.ErrNotACloseParen
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
		v, _, err := p.readSyntax()
		if err != nil {
			return nil, p.cur, err
		}
		for p.err == nil && p.curr().Type() != tokenizer.TokenizerStateCloseParen {
			q0.Values = append(q0.Values, v)
			p.cur, p.err = p.toks.Next()
			if p.err != nil {
				return nil, p.cur, p.err
			}
			v, _, err = p.readSyntax()
			if err != nil {
				return nil, p.cur, err
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
		stx, _, err := p.readSyntax()
		if err != nil {
			return nil, p.cur, err
		}
		if p.curr().Type() == tokenizer.TokenizerStateCloseParen {
			break
		}
		i, ok := stx.Unwrap().(*values.Integer)
		for {
			if !ok {
				return nil, p.cur, values.ErrNotAnInteger
			}
			*q0 = append(*q0, *values.NewByte(uint8(i.Value)))
			p.cur, p.err = p.toks.Next()
			if p.err != nil {
				return nil, p.cur, p.err
			}
			stx, _, err = p.readSyntax()
			if err != nil {
				return nil, p.cur, err
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
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		q, _, err := p.readSyntax()
		if err != nil {
			return nil, p.cur, err
		}
		q1 := p.wrapSyntaxSymbol(ConstUnquote, p.cur)
		q2 := p.listSyntax(p.cur, q1, q)
		return q2, p.cur, nil
	case tokenizer.TokenizerStateQuasiquote:
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		q0, _, err := p.readSyntax()
		if err != nil {
			return nil, p.cur, err
		}
		q2 := p.wrapSyntaxSymbol(ConstQuasiquote, p.cur)
		q3 := p.listSyntax(p.cur, q2, q0)
		return q3, p.cur, nil
	case tokenizer.TokenizerStateUnquoteSplicing:
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		q0, _, err := p.readSyntax()
		if err != nil {
			return nil, p.cur, err
		}
		q2 := p.wrapSyntaxSymbol(ConstUnquoteSplicing, p.cur)
		q3 := p.listSyntax(p.cur, q2, q0)
		return q3, p.cur, nil
	case tokenizer.TokenizerStateQuote:
		t := p.curr()
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		q, _, err := p.readSyntax()
		if err != nil {
			return nil, p.cur, err
		}
		q1 := p.wrapSyntaxSymbol(ConstQuote, t)
		q2 := p.listSyntax(t, q1, q)
		return q2, p.cur, nil
	case tokenizer.TokenizerStateUnsyntax:
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		q, _, err := p.readSyntax()
		if err != nil {
			return nil, p.cur, err
		}
		q1 := p.wrapSyntaxSymbol(ConstUnsyntax, p.cur)
		q2 := p.listSyntax(p.cur, q1, q)
		return q2, p.cur, nil
	case tokenizer.TokenizerStateQuasisyntax:
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		q, _, err := p.readSyntax()
		if err != nil {
			return nil, p.cur, err
		}
		q1 := p.wrapSyntaxSymbol(ConstQuasisyntax, p.cur)
		q2 := p.listSyntax(p.cur, q1, q)
		return q2, p.cur, nil
	case tokenizer.TokenizerStateUnsyntaxSplicing:
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		q, _, err := p.readSyntax()
		if err != nil {
			return nil, p.cur, err
		}
		q1 := p.wrapSyntaxSymbol(ConstUnsyntaxSplicing, p.cur)
		q2 := p.listSyntax(p.cur, q1, q)
		return q2, p.cur, nil
	case tokenizer.TokenizerStateSyntax:
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		q, _, err := p.readSyntax()
		if err != nil {
			return nil, p.cur, err
		}
		q1 := p.wrapSyntaxSymbol(ConstSyntax, p.cur)
		q2 := p.listSyntax(p.cur, q1, q)
		return q2, p.cur, nil
	case tokenizer.TokenizerStateSymbol:
		q = p.wrapSyntaxSymbol(p.cur.String(), p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateUnsignedInteger:
		var a int64
		a, err := strconv.ParseInt(p.cur.String(), 10, 64)
		if err != nil {
			return nil, p.cur, err
		}
		q1 := values.NewInteger(a)
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateUnsignedDecimalFraction:
		var a float64
		a, err := strconv.ParseFloat(p.cur.String(), 64)
		if err != nil {
			return nil, p.cur, err
		}
		q1 := values.NewFloat(a)
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateSignedInteger:
		// Signed integers like "-40", "+40"
		a, err := strconv.ParseInt(p.cur.String(), 10, 64)
		if err != nil {
			return nil, p.cur, err
		}
		q1 := values.NewInteger(a)
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateSignedDecimalFraction:
		// Signed decimal fractions like "-3.24", "+3.24"
		a, err := strconv.ParseFloat(p.cur.String(), 64)
		if err != nil {
			return nil, p.cur, err
		}
		q1 := values.NewFloat(a)
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateUnsignedRationalFraction:
		// Unsigned rational fractions like "3/4"
		q1, err := p.parseRational(p.cur.String())
		if err != nil {
			return nil, p.cur, err
		}
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateSignedRationalFraction:
		// Signed rational fractions like "-1/2", "+3/4"
		q1, err := p.parseRational(p.cur.String())
		if err != nil {
			return nil, p.cur, err
		}
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateIntegerBase2, tokenizer.TokenizerStateIntegerBase8,
		tokenizer.TokenizerStateIntegerBase10, tokenizer.TokenizerStateIntegerBase16:
		// Base-prefixed integers - the base is already set in the tokenizer
		// For now, just parse as base 10 since the prefix is handled separately
		a, err := strconv.ParseInt(p.cur.String(), 10, 64)
		if err != nil {
			return nil, p.cur, err
		}
		q1 := values.NewInteger(a)
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateMarkerBase2:
		// #b prefix - next token is base 2 integer
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		a, err := strconv.ParseInt(p.cur.String(), 2, 64)
		if err != nil {
			return nil, p.cur, err
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
		a, err := strconv.ParseInt(p.cur.String(), 8, 64)
		if err != nil {
			return nil, p.cur, err
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
		a, err := strconv.ParseInt(p.cur.String(), 16, 64)
		if err != nil {
			return nil, p.cur, err
		}
		q1 := values.NewInteger(a)
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
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
		q1, err := p.parseImaginary(p.cur.String())
		if err != nil {
			return nil, p.cur, err
		}
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateSignedImaginaryInf:
		// +inf.0i or -inf.0i
		s := p.cur.String()
		var imag float64
		if strings.HasPrefix(s, "-") {
			imag = math.Inf(-1)
		} else {
			imag = math.Inf(1)
		}
		q1 := values.NewComplexFromParts(0, imag)
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateSignedImaginaryNan:
		// +nan.0i or -nan.0i
		q1 := values.NewComplexFromParts(0, math.NaN())
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateUnsignedComplex, tokenizer.TokenizerStateSignedComplex:
		// Full complex numbers like 1+2i, 3-4i, 1.5+2.5i, +1+2i, -3-4i
		q1, err := p.parseComplex(p.cur.String())
		if err != nil {
			return nil, p.cur, err
		}
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateUnsignedComplexPolar, tokenizer.TokenizerStateSignedComplexPolar:
		// Polar complex numbers like 1@1.5708, +2@0.5, -3@1.0
		q1, err := p.parsePolarComplex(p.cur.String())
		if err != nil {
			return nil, p.cur, err
		}
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateUnsignedImaginary:
		// Pure imaginary numbers (unsigned, typically after radix prefix)
		q1, err := p.parseImaginary(p.cur.String())
		if err != nil {
			return nil, p.cur, err
		}
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateUnsignedImaginaryInf:
		// Unsigned imaginary infinity (typically after radix prefix)
		s := p.cur.String()
		var imag float64
		if strings.HasPrefix(s, "-") {
			imag = math.Inf(-1)
		} else {
			imag = math.Inf(1)
		}
		q1 := values.NewComplexFromParts(0, imag)
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
		s := strings.TrimPrefix(p.cur.String(), "#\\")
		rs := []rune(s)
		q1 := values.NewCharacter(rs[0])
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateCharMnemonic:
		var q0 *values.Character
		s := strings.TrimPrefix(p.cur.String(), "#\\")
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
			return nil, nil, values.ErrUnknownCharacterMnemonic
		}
		q = p.wrapSyntax(q0, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateCharHexEscape:
		s := strings.TrimPrefix(p.cur.String(), "#\\x")
		i, err := strconv.ParseInt(s, 16, 32)
		if err != nil {
			return nil, p.cur, err
		}
		q1 := values.NewCharacter(rune(i))
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateStringEnd:
		// Use Value() to get the processed string (escapes converted, quotes stripped)
		q1 := values.NewString(p.cur.Value())
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateCloseParen:
		return q, p.cur, nil
	}
	return q, nil, ErrUnknownTokenType
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

var (
	ErrAlreadyClosed = values.NewStaticError("parser already closed")
)

func (p *Parser) Close() error {
	if p.toks == nil {
		return ErrAlreadyClosed
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
		return nil, values.NewForeignErrorf("invalid rational number: %s", s)
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
		return nil, values.NewForeignErrorf("invalid polar complex number: %s (no @ separator found)", s)
	}

	// Split into magnitude and angle parts
	magPart := s[:atPos]
	anglePart := s[atPos+1:]

	// Parse magnitude
	mag, err := strconv.ParseFloat(magPart, 64)
	if err != nil {
		// Try parsing as inf/nan
		switch magPart {
		case "+inf.0":
			mag = math.Inf(1)
		case "-inf.0":
			mag = math.Inf(-1)
		case "+nan.0", "-nan.0":
			mag = math.NaN()
		default:
			return nil, values.NewForeignErrorf("invalid magnitude in polar complex: %s", magPart)
		}
	}

	// Parse angle
	angle, err := strconv.ParseFloat(anglePart, 64)
	if err != nil {
		// Try parsing as inf/nan
		switch anglePart {
		case "+inf.0":
			angle = math.Inf(1)
		case "-inf.0":
			angle = math.Inf(-1)
		case "+nan.0", "-nan.0":
			angle = math.NaN()
		default:
			return nil, values.NewForeignErrorf("invalid angle in polar complex: %s", anglePart)
		}
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
		return nil, values.NewForeignErrorf("invalid complex number: %s (no sign separator found)", s+"i")
	}

	// Split into real and imaginary parts
	realPart := s[:signPos]
	imagPart := s[signPos:] // includes the sign

	// Parse real part
	real, err := p.parseRealPart(realPart)
	if err != nil {
		return nil, values.NewForeignErrorf("invalid real part in complex number: %s", realPart)
	}

	// Parse imaginary part
	imag, err := p.parseImagPart(imagPart)
	if err != nil {
		return nil, values.NewForeignErrorf("invalid imaginary part in complex number: %s", imagPart)
	}

	return values.NewComplexFromParts(real, imag), nil
}

// parseRealPart parses a real number that may be a float or infnan
func (p *Parser) parseRealPart(s string) (float64, error) {
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

// parseImagPart parses an imaginary coefficient that may be a float, infnan, or just a sign
func (p *Parser) parseImagPart(s string) (float64, error) {
	switch s {
	case "+":
		return 1, nil
	case "-":
		return -1, nil
	case "+inf.0":
		return math.Inf(1), nil
	case "-inf.0":
		return math.Inf(-1), nil
	case "+nan.0", "-nan.0":
		return math.NaN(), nil
	}
	return strconv.ParseFloat(s, 64)
}
