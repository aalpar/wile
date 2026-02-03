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
	"errors"
	"io"
	"math"
	"math/big"
	"math/bits"
	"strconv"
	"strings"
	"unicode"
	"unicode/utf8"

	"github.com/aalpar/wile/go/environment"
	"github.com/aalpar/wile/go/syntax"
	"github.com/aalpar/wile/go/tokenizer"
	"github.com/aalpar/wile/go/values"
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
	foldCase    bool                       // R7RS §2.1: #!fold-case mode for identifiers
	file        string                     // source file name for error reporting
	datumLabels map[int]syntax.SyntaxValue // R7RS §2.4 datum labels (#n= and #n#)
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
		// R7RS: unexpected close parenthesis at top level is a read error
		if q == nil && p.cur != nil && p.cur.Type() == tokenizer.TokenizerStateCloseParen {
			p.toks = nil
			p.err = NewParserErrorf(p.cur, "unexpected close parenthesis")
			return nil, p.err
		}
		// Advance to the next token for the next ReadSyntax() call
		p.cur, p.err = p.toks.Next()
		// EOF is fine - it means there's nothing more to read
		// FIXME: rework error return.
		if p.err != nil && p.err != io.EOF {
			p.toks = nil
			return nil, p.err
		}
		if !p.skipComment {
			return q, nil
		}
		switch d := q.(type) {
		case *syntax.SyntaxComment, *syntax.SyntaxDatumComment:
			if p.err == io.EOF {
				return nil, p.err
			}
			continue
		case *syntax.SyntaxDirective:
			// R7RS §2.1: Process fold-case directives
			p.processFoldCaseDirective(d)
			if p.err == io.EOF {
				return nil, p.err
			}
			continue
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
	// R7RS §2.1: Apply fold-case if directive is active
	if p.foldCase {
		o = strings.ToLower(o)
	}
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

// processFoldCaseDirective handles R7RS §2.1 fold-case directives.
// #!fold-case causes subsequent identifiers to be case-folded to lowercase.
// #!no-fold-case restores case-sensitive reading.
func (p *Parser) processFoldCaseDirective(d *syntax.SyntaxDirective) {
	if strings.EqualFold(d.Name, "fold-case") {
		p.foldCase = true
	} else if strings.EqualFold(d.Name, "no-fold-case") {
		p.foldCase = false
	}
}

// readLabeledList reads a list into a pre-created placeholder pair.
// This enables circular references where the list refers to itself via datum labels.
// The placeholder must already be registered in datumLabels before calling this.
//
// R7RS §2.4: Datum labels enable representing shared/circular structures.
// For #0=(1 . #0#), we:
//  1. Pre-create a pair and register it as label 0
//  2. Read the list contents, which may include #0# references
//  3. Populate the pair with the read values
func (p *Parser) readLabeledList(placeholder *syntax.SyntaxPair) (syntax.SyntaxValue, error) {
	// Skip the opening paren
	p.cur, p.err = p.toks.Next()
	if p.err != nil {
		return nil, p.err
	}

	// Handle empty list: ()
	if p.cur.Type() == tokenizer.TokenizerStateCloseParen {
		// Empty list - return the placeholder unchanged (nil . nil)
		return placeholder, nil
	}

	// Read the first element
	first, _, err := p.readSyntax()
	if err != nil {
		return nil, err
	}
	placeholder.SetCar(first)

	// Advance to next token
	p.cur, p.err = p.toks.Next()
	if p.err != nil {
		return nil, p.err
	}

	// Check for improper list: (a . b)
	if p.cur.Type() == tokenizer.TokenizerStateCons {
		// Skip the dot
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.err
		}
		// Read the cdr
		cdr, _, err := p.readSyntax()
		if err != nil {
			return nil, err
		}
		placeholder.SetCdr(cdr)
		// Advance past the cdr and expect close paren
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.err
		}
		if p.cur.Type() != tokenizer.TokenizerStateCloseParen {
			return nil, NewParserErrorf(p.cur, "expected ')' after improper list cdr")
		}
		return placeholder, nil
	}

	// Check for end of list
	if p.cur.Type() == tokenizer.TokenizerStateCloseParen {
		// Single element list - set cdr to empty list
		placeholder.SetCdr(syntax.NewSyntaxEmptyList(p.newSourceContext(p.cur)))
		return placeholder, nil
	}

	// Continue reading remaining elements
	current := placeholder
	for p.cur.Type() != tokenizer.TokenizerStateCloseParen && p.cur.Type() != tokenizer.TokenizerStateCons {
		// Create a new pair for this element
		nextPair := p.wrapSyntaxPair(nil, nil, p.cur)

		// Read the element
		elem, _, err := p.readSyntax()
		if err != nil {
			return nil, err
		}
		nextPair.SetCar(elem)

		// Link to previous
		current.SetCdr(nextPair)
		current = nextPair

		// Advance to next token
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.err
		}
	}

	// Handle improper list ending: (a b . c)
	if p.cur.Type() == tokenizer.TokenizerStateCons {
		// Skip the dot
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.err
		}
		// Read the final cdr
		cdr, _, err := p.readSyntax()
		if err != nil {
			return nil, err
		}
		current.SetCdr(cdr)
		// Advance past the cdr and expect close paren
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.err
		}
		if p.cur.Type() != tokenizer.TokenizerStateCloseParen {
			return nil, NewParserErrorf(p.cur, "expected ')' after improper list cdr")
		}
	} else {
		// Proper list - terminate with empty list
		current.SetCdr(syntax.NewSyntaxEmptyList(p.newSourceContext(p.cur)))
	}

	return placeholder, nil
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

// parseDecimalInteger parses an unsigned or signed base-10 integer token.
// Handles hash digit substitution, overflow promotion to BigInteger,
// and forces inexact (Float) when hash digits are present.
//
// R7RS §7.1.1: When # appears in a number literal, each # represents an
// unknown digit (treated as 0) and the result is inexact.
func (p *Parser) parseDecimalInteger() (syntax.SyntaxValue, tokenizer.Token, error) {
	s := replaceHashDigits(p.cur.String())
	a, err := strconv.ParseInt(s, 10, 64)
	if err != nil {
		// If overflow, promote to BigInteger
		var numErr *strconv.NumError
		isNumErr := errors.As(err, &numErr)
		if isNumErr && errors.Is(numErr.Err, strconv.ErrRange) {
			bigInt := new(big.Int)
			_, ok := bigInt.SetString(s, 10)
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
		return nil, p.cur, err
	}
	if p.cur.HasHashDigit() {
		q := p.wrapSyntax(values.NewFloat(float64(a)), p.cur)
		return q, p.cur, nil
	}
	q := p.wrapSyntax(values.NewInteger(a), p.cur)
	return q, p.cur, nil
}

// parseIntegerWithBase parses the current token as an integer in the given base.
// Handles hash digit substitution and forces inexact when hash digits are present.
//
// R7RS §7.1.1: <uinteger R> -> <digit R>+ #*
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
		return nil, p.cur, err
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
	s := TrimPrefixFolded(p.cur.String(), "#z")
	s = TrimPrefixFolded(s, "#Z")
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

	// Find the exponent marker
	expIdx := strings.IndexAny(s, "eEsSfFdDlL")
	if expIdx == -1 {
		return nil, p.cur, NewParserErrorf(p.cur, "invalid scientific notation: %s", s)
	}

	// Normalize exponent marker to 'e' for strconv.ParseFloat
	normalized := s[:expIdx] + "e" + s[expIdx+1:]
	f, err := strconv.ParseFloat(normalized, 64)
	if err != nil {
		return nil, p.cur, NewParserErrorf(p.cur, "invalid scientific notation: %s", s)
	}
	q := p.wrapSyntax(values.NewFloat(f), p.cur)
	return q, p.cur, nil
}

// normalizeExponentMarker replaces R7RS exponent markers (s, f, d, l) with 'e'
// so that strconv.ParseFloat can parse the number.
func normalizeExponentMarker(s string) string {
	idx := strings.IndexAny(s, "sSfFdDlL")
	if idx == -1 {
		return s
	}
	q := []byte(s)
	q[idx] = 'e'
	return string(q)
}

// replaceHashDigits replaces all '#' inexact digit placeholders with '0'.
// R7RS §7.1.1: '#' represents an unknown digit treated as 0; its presence
// forces the number to be inexact (handled by the caller checking HasHashDigit).
func replaceHashDigits(s string) string {
	return strings.ReplaceAll(s, "#", "0")
}

func (p *Parser) readSyntax() (syntax.SyntaxValue, tokenizer.Token, error) {
	var q syntax.SyntaxValue

	// Skip comments when skipComment is enabled
	// This handles comments inside lists, vectors, and other compound structures
	for p.skipComment {
		cur := p.curr()
		switch cur.Type() {
		case tokenizer.TokenizerStateLineCommentBody, tokenizer.TokenizerStateBlockCommentBody:
			// Skip the comment and advance to next token
			p.cur, p.err = p.toks.Next()
			if p.err != nil {
				return nil, p.cur, p.err
			}
			continue
		case tokenizer.TokenizerStateDatumCommentBegin:
			// Skip the datum comment begin token
			p.cur, p.err = p.toks.Next()
			if p.err != nil {
				return nil, p.cur, p.err
			}
			// Read and discard the commented datum
			_, _, p.err = p.readSyntax()
			if p.err != nil {
				return nil, p.cur, p.err
			}
			// Advance past the commented datum
			p.cur, p.err = p.toks.Next()
			if p.err != nil {
				return nil, p.cur, p.err
			}
			continue
		case tokenizer.TokenizerStateDirective:
			// Process fold-case directives even when skipping
			d := p.wrapSyntaxDirective(TrimPrefixFolded(p.cur.String(), "#!"), p.cur)
			p.processFoldCaseDirective(d)
			p.cur, p.err = p.toks.Next()
			if p.err != nil {
				return nil, p.cur, p.err
			}
			continue
		}
		break
	}

	cur := p.curr()
	switch cur.Type() {
	case tokenizer.TokenizerStateCons:
		return nil, p.cur, NewParserErrorWithWrap(values.ErrNotACons, p.cur, "unexpected '.' token")
	case tokenizer.TokenizerStateLabelAssignment:
		// R7RS §2.4: #n=<datum> defines datum label n
		// For circular/shared structures, we must register the container before reading its contents
		s := TrimPrefixFolded(p.cur.String(), "#")
		is := strings.TrimSuffix(s, "=")
		var i int64
		i, p.err = strconv.ParseInt(is, 10, bits.UintSize)
		if p.err != nil {
			return nil, p.cur, p.err
		}
		labelNum := int(i)
		if p.datumLabels == nil {
			p.datumLabels = make(map[int]syntax.SyntaxValue)
		}
		assignTok := p.cur
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		// Check if the next token starts a compound structure (list or vector)
		// For these, we pre-register a placeholder to support circular references
		var v syntax.SyntaxValue
		switch p.cur.Type() {
		case tokenizer.TokenizerStateOpenParen:
			// Pre-register an empty pair for potential circular references
			placeholder := p.wrapSyntaxPair(nil, nil, p.cur)
			p.datumLabels[labelNum] = placeholder
			// Now read the list contents, which can reference this label
			v, p.err = p.readLabeledList(placeholder)
			if p.err != nil {
				return nil, p.cur, p.err
			}
		case tokenizer.TokenizerStateOpenVector:
			// For vectors, read normally (circular vector references are rare)
			v, _, p.err = p.readSyntax()
			if p.err != nil {
				return nil, p.cur, p.err
			}
			p.datumLabels[labelNum] = v
		default:
			// Non-compound datum: read normally and store
			v, _, p.err = p.readSyntax()
			if p.err != nil {
				return nil, p.cur, p.err
			}
			p.datumLabels[labelNum] = v
		}
		q0 := p.wrapSyntaxDatumLabelAssignment(labelNum, v, assignTok)
		return q0, p.cur, nil
	case tokenizer.TokenizerStateLabelReference:
		// R7RS §2.4: #n# references previously defined datum label n
		s := strings.Trim(p.cur.String(), "#")
		var i int64
		i, p.err = strconv.ParseInt(s, 10, bits.UintSize)
		if p.err != nil {
			return nil, p.cur, p.err
		}
		// Look up the datum in the label table
		if p.datumLabels != nil {
			if labeled, ok := p.datumLabels[int(i)]; ok {
				// Return the actual datum, not a label reference
				return labeled, p.cur, nil
			}
		}
		// Label not found - return a SyntaxDatumLabel (will error at compile time)
		q = p.wrapSyntaxDatumLabel(int(i), p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateDirective:
		q = p.wrapSyntaxDirective(TrimPrefixFolded(p.cur.String(), "#!"), p.cur)
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
			// After skipping comments, we may have landed on a delimiter (close paren or cons)
			// In that case, readSyntax returns nil and we should exit the loop
			if v == nil {
				break
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
			pr = v
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
		// Advance past #( token
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		// Read vector elements - match the list parsing pattern:
		// check token type BEFORE reading, advance AFTER reading
		for p.cur.Type() != tokenizer.TokenizerStateCloseParen {
			var v syntax.SyntaxValue
			v, _, p.err = p.readSyntax()
			if p.err != nil {
				return nil, p.cur, p.err
			}
			q0.Values = append(q0.Values, v)
			// Advance to next element (or close paren)
			p.cur, p.err = p.toks.Next()
			if p.err != nil {
				return nil, p.cur, p.err
			}
		}
		return q0, p.cur, nil
	case tokenizer.TokenizerStateOpenVectorUnsignedByteMarker:
		var stx syntax.SyntaxValue
		q0 := values.NewByteVectorFromIntegers()
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		stx, _, p.err = p.readSyntax()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		if p.curr().Type() == tokenizer.TokenizerStateCloseParen {
			// Empty bytevector case: #u8()
			q = p.wrapSyntax(q0, p.cur)
			return q, p.cur, nil
		}
		i, ok := stx.Unwrap().(*values.Integer)
		for {
			if !ok {
				return nil, p.cur, NewParserErrorWithWrapf(values.ErrNotAnInteger, p.cur, "expected unsigned byte integer in byte vector, got %T", stx.Unwrap())
			}
			*q0 = append(*q0, values.NewByte(uint8(i.Value)))
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
		q = p.wrapSyntaxSymbol(p.cur.Value(), p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateUnsignedInteger, tokenizer.TokenizerStateSignedInteger:
		return p.parseDecimalInteger()
	case tokenizer.TokenizerStateUnsignedDecimalFraction:
		// R7RS §7.1.1: # digit placeholders replaced with 0
		var a float64
		a, p.err = strconv.ParseFloat(normalizeExponentMarker(replaceHashDigits(p.cur.String())), 64)
		if p.err != nil {
			return nil, p.cur, p.err
		}
		q1 := values.NewFloat(a)
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateSignedDecimalFraction:
		// Signed decimal fractions like "-3.24", "+3.24"
		// R7RS §7.1.1: # digit placeholders replaced with 0
		var a float64
		a, p.err = strconv.ParseFloat(normalizeExponentMarker(replaceHashDigits(p.cur.String())), 64)
		if p.err != nil {
			return nil, p.cur, p.err
		}
		q1 := values.NewFloat(a)
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateSignedScientificNotation,
		tokenizer.TokenizerStateUnsignedScientificNotation:
		// Scientific notation like "1e10", "+2e-5", "100000e-4"
		// Parser determines if result is integer or float
		return p.parseScientificNotation()
	case tokenizer.TokenizerStateUnsignedRationalFraction:
		// Unsigned rational fractions like "3/4"
		// R7RS §7.1.1: # placeholders → 0; presence forces inexact
		var q1 values.Number
		q1, p.err = p.parseRational(replaceHashDigits(p.cur.String()))
		if p.err != nil {
			return nil, p.cur, p.err
		}
		if p.cur.HasHashDigit() {
			q = p.wrapSyntax(p.numberToInexact(q1), p.cur)
		} else {
			q = p.wrapSyntax(q1, p.cur)
		}
		return q, p.cur, nil
	case tokenizer.TokenizerStateSignedRationalFraction:
		// Signed rational fractions like "-1/2", "+3/4"
		// R7RS §7.1.1: # placeholders → 0; presence forces inexact
		var q1 values.Number
		q1, p.err = p.parseRational(replaceHashDigits(p.cur.String()))
		if p.err != nil {
			return nil, p.cur, p.err
		}
		if p.cur.HasHashDigit() {
			q = p.wrapSyntax(p.numberToInexact(q1), p.cur)
		} else {
			q = p.wrapSyntax(q1, p.cur)
		}
		return q, p.cur, nil
	case tokenizer.TokenizerStateMarkerBase2:
		// #b prefix - next token is base 2 integer or rational
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		if p.cur.Type() == tokenizer.TokenizerStateUnsignedRationalFraction ||
			p.cur.Type() == tokenizer.TokenizerStateSignedRationalFraction {
			return p.parseRationalWithBase(2)
		}
		return p.parseIntegerWithBase(2)
	case tokenizer.TokenizerStateMarkerBase8:
		// #o prefix - next token is base 8 integer or rational
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		if p.cur.Type() == tokenizer.TokenizerStateUnsignedRationalFraction ||
			p.cur.Type() == tokenizer.TokenizerStateSignedRationalFraction {
			return p.parseRationalWithBase(8)
		}
		return p.parseIntegerWithBase(8)
	case tokenizer.TokenizerStateMarkerBase10:
		// #d prefix - next token is base 10 integer
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		return p.readSyntax()
	case tokenizer.TokenizerStateMarkerBase16:
		// #x prefix - next token is base 16 integer or rational
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		if p.cur.Type() == tokenizer.TokenizerStateUnsignedRationalFraction ||
			p.cur.Type() == tokenizer.TokenizerStateSignedRationalFraction {
			return p.parseRationalWithBase(16)
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
		// R7RS §7.1.1: # placeholders → 0; presence forces inexact
		var q1 values.Number
		q1, p.err = p.parseImaginary(replaceHashDigits(p.cur.String()))
		if p.err != nil {
			return nil, p.cur, p.err
		}
		if p.cur.HasHashDigit() {
			q1 = p.numberToInexact(q1)
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
		// R7RS §6.2.2: Exact if both parts are integer/rational, inexact otherwise
		// R7RS §7.1.1: # placeholders → 0; presence forces inexact
		var q1 values.Number
		q1, p.err = p.parseComplex(replaceHashDigits(p.cur.String()))
		if p.err != nil {
			return nil, p.cur, p.err
		}
		if p.cur.HasHashDigit() {
			q1 = p.numberToInexact(q1)
		}
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateUnsignedComplexPolar, tokenizer.TokenizerStateSignedComplexPolar:
		// Polar complex numbers like 1@1.5708, +2@0.5, -3@1.0
		// R7RS §7.1.1: # placeholders → 0
		var q1 *values.Complex
		q1, p.err = p.parsePolarComplex(replaceHashDigits(p.cur.String()))
		if p.err != nil {
			return nil, p.cur, p.err
		}
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateUnsignedImaginary:
		// Pure imaginary numbers (unsigned, typically after radix prefix)
		// R7RS §7.1.1: # placeholders → 0; presence forces inexact
		var q1 values.Number
		q1, p.err = p.parseImaginary(replaceHashDigits(p.cur.String()))
		if p.err != nil {
			return nil, p.cur, p.err
		}
		if p.cur.HasHashDigit() {
			q1 = p.numberToInexact(q1)
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
	case tokenizer.TokenizerStateMarkerNumberExact:
		// #e prefix - read the next number and convert to exact
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		q, tok, err := p.readSyntax()
		if err != nil {
			return nil, tok, err
		}
		// Convert to exact
		exactVal, err := p.makeExact(q)
		if err != nil {
			return nil, tok, NewParserErrorf(tok, "cannot convert to exact: %v", err)
		}
		return exactVal, tok, nil
	case tokenizer.TokenizerStateMarkerNumberInexact:
		// #i prefix - read the next number and convert to inexact
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		q, tok, err := p.readSyntax()
		if err != nil {
			return nil, tok, err
		}
		// Convert to inexact
		inexactVal, err := p.makeInexact(q)
		if err != nil {
			return nil, tok, NewParserErrorf(tok, "cannot convert to inexact: %v", err)
		}
		return inexactVal, tok, nil
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
		s := TrimPrefixFolded(p.cur.String(), "#m")
		s = TrimPrefixFolded(s, "#M")
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
		s := TrimPrefixFolded(p.cur.String(), values.PrefixCharacter)
		rs := []rune(s)
		q1 := values.NewCharacter(rs[0])
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateCharMnemonic:
		var q0 *values.Character
		s := TrimPrefixFolded(p.cur.String(), values.PrefixCharacter)
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
		s := TrimPrefixFolded(p.cur.String(), "#\\x")
		var i int64
		i, p.err = strconv.ParseInt(s, 16, 32)
		if p.err != nil {
			return nil, p.cur, NewParserErrorWithWrapf(p.err, p.cur, "invalid character hex escape: %s", s)
		}
		q1 := values.NewCharacter(rune(i))
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateString:
		// Use wrt() to get the processed string (escapes converted, quotes stripped)
		q1 := values.NewString(p.cur.Value())
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateCloseParen:
		return q, p.cur, nil
	}
	return q, nil, NewParserErrorWithWrapf(ErrUnknownTokenType, p.cur, "unknown token type: %q", p.cur.String())
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

// parseImaginary parses an imaginary number string like "+3i", "-2i", "+i", "-i"
// R7RS §6.2.2: Returns exact BigComplex for integer imaginary parts,
// inexact Complex for floating-point imaginary parts.
func (p *Parser) parseImaginary(s string) (values.Number, error) {
	// Remove the trailing 'i'
	s = TrimSuffixFolded(s, "i")

	// Handle "+i" and "-i" cases - these are exact
	if s == "+" || s == "" {
		return values.NewBigComplex(
			values.NewBigIntegerFromInt64(0),
			values.NewBigIntegerFromInt64(1),
		), nil
	}
	if s == "-" {
		return values.NewBigComplex(
			values.NewBigIntegerFromInt64(0),
			values.NewBigIntegerFromInt64(-1),
		), nil
	}

	// Check if the imaginary coefficient is an integer (exact) or float (inexact)
	if isIntegerString(s) {
		// Parse as exact integer
		iam, err := parseExactPart(s)
		if err != nil {
			return nil, err
		}
		return values.NewBigComplex(
			values.NewBigIntegerFromInt64(0),
			iam,
		), nil
	}

	// Parse the numeric part as inexact float
	iam, err := strconv.ParseFloat(s, 64)
	if err != nil {
		return nil, err
	}
	return values.NewComplexFromParts(0, iam), nil
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
			return nil, errors.New("invalid rational: " + s)
		}
		return values.NewRationalFromRat(r), nil
	}

	// Integer
	i := new(big.Int)
	_, ok := i.SetString(s, 10)
	if !ok {
		return nil, errors.New("invalid integer: " + s)
	}
	return values.NewBigInteger(i), nil
}

// parseComplex parses a complex number string like "1+2i", "3-4i", "1.5+2.5i", "1+i", "5-i"
// Also handles infnan: "+inf.0+inf.0i", "1+inf.0i", "3+nan.0i"
// R7RS §6.2.2: Returns an exact BigComplex if both parts are exact (integer/rational),
// otherwise returns an inexact Complex.
func (p *Parser) parseComplex(s string) (values.Number, error) {
	// Remove the trailing 'i'
	s = TrimSuffixFolded(s, "i")

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
				len(rest) > 1 && (rest[1] >= '0' && rest[1] <= '9' || rest[1] == '.' || rest[1] == '/') {
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

	// Check if both parts are exact (integer or rational)
	// If so, create an exact BigComplex; otherwise use inexact Complex
	if isExactPartString(realPart) && isExactPartString(imagPart) {
		realNum, err := parseExactPart(realPart)
		if err != nil {
			return nil, NewParserErrorf(p.cur, "invalid real part in complex number: %s", realPart)
		}
		imagNum, err := parseExactPart(imagPart)
		if err != nil {
			return nil, NewParserErrorf(p.cur, "invalid imaginary part in complex number: %s", imagPart)
		}
		return values.NewBigComplex(realNum, imagNum), nil
	}

	// Parse as inexact complex
	rel, err := p.parseRealPart(realPart)
	if err != nil {
		return nil, NewParserErrorf(p.cur, "invalid real part in complex number: %s", realPart)
	}

	// R7RS: If the imaginary part is exact zero, treat the number as real.
	// e.g., -2.5+0i should be just -2.5 (a Float), not a Complex.
	// This is because exact 0 means "definitely zero", while inexact 0.0 means
	// "approximately zero" and could have rounding errors.
	if isExactPartString(imagPart) {
		parsedImag, parseErr := parseExactPart(imagPart)
		if parseErr == nil && parsedImag.IsZero() {
			return values.NewFloat(rel), nil
		}
	}

	img, err := p.parseImagPart(imagPart)
	if err != nil {
		return nil, NewParserErrorf(p.cur, "invalid imaginary part in complex number: %s", imagPart)
	}

	return values.NewComplexFromParts(rel, img), nil
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
	slashIdx := strings.Index(s, "/")
	if slashIdx != -1 {
		numStr := s[:slashIdx]
		denStr := s[slashIdx+1:]

		num, err := strconv.ParseFloat(numStr, 64)
		if err != nil {
			return 0, err
		}
		den, err := strconv.ParseFloat(denStr, 64)
		if err != nil {
			return 0, err
		}
		if den == 0 {
			return 0, errors.New("division by zero in rational")
		}
		return num / den, nil
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
	if r0 == utf8.RuneError || r1 == utf8.RuneError {
		return false
	}
	return unicode.ToLower(r0) == unicode.ToLower(r1)
}

// TrimPrefixFolded performs a case-insensitive trim of the specified prefix from the string s.
// If s does not start with the prefix (case-insensitive), s is returned unchanged.
// functionally,its identical to strings.TrimPrefix but case-insensitive.
func TrimPrefixFolded(s, prefix string) string {
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

// TrimSuffixFolded performs a case-insensitive trim of the specified suffix from the string s.
// If s does not end with the suffix (case-insensitive), s is returned unchanged.
// functionally,its identical to strings.TrimSuffix but case-insensitive.
func TrimSuffixFolded(s, suffix string) string {
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

// makeExact converts a syntax-wrapped number to its exact representation.
// R7RS §6.2.6: exact converts an inexact number to exact.
// For integers and rationals, they are already exact.
// For floats, they are converted to rationals or integers if they represent whole numbers.
func (p *Parser) makeExact(stx syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	val := stx.Unwrap()
	num, ok := val.(values.Number)
	if !ok {
		return nil, errors.New("not a number")
	}

	var exactNum values.Value
	switch v := num.(type) {
	case *values.Integer, *values.BigInteger, *values.Rational:
		// Already exact
		exactNum = v
	case *values.Float:
		// Convert float to exact
		f := v.Value
		if math.IsNaN(f) || math.IsInf(f, 0) {
			return nil, errors.New("cannot convert inf or nan to exact")
		}
		// Check if it's a whole number
		if f == math.Trunc(f) && f >= math.MinInt64 && f <= math.MaxInt64 {
			exactNum = values.NewInteger(int64(f))
		} else {
			// Convert to rational using big.Rat
			r := new(big.Rat).SetFloat64(f)
			exactNum = values.NewRationalFromRat(r)
		}
	case *values.BigFloat:
		// Convert BigFloat to exact
		bf := v.BigFloatValue()
		if bf.IsInf() {
			return nil, errors.New("cannot convert inf to exact")
		}
		// Try to convert to integer first
		if bf.IsInt() {
			i, _ := bf.Int(nil)
			exactNum = values.NewBigInteger(i)
		} else {
			// Convert to rational
			r, _ := bf.Rat(nil)
			exactNum = values.NewRationalFromRat(r)
		}
	case *values.Complex:
		// Convert complex to exact BigComplex
		re := v.Real()
		im := v.Imag()
		if math.IsNaN(re) || math.IsNaN(im) || math.IsInf(re, 0) || math.IsInf(im, 0) {
			return nil, errors.New("cannot convert complex with inf or nan to exact")
		}
		reRat := new(big.Rat).SetFloat64(re)
		imRat := new(big.Rat).SetFloat64(im)
		reNum := values.NewRationalFromRat(reRat)
		imNum := values.NewRationalFromRat(imRat)
		exactNum = values.NewBigComplex(reNum, imNum)
	case *values.BigComplex:
		// Already exact or convert to exact
		if v.IsExact() {
			exactNum = v
		} else {
			return nil, errors.New("cannot convert inexact BigComplex to exact")
		}
	default:
		return nil, errors.New("unsupported number type for exact conversion")
	}

	// Re-wrap with the same syntax context
	return p.rewrapSyntax(stx, exactNum), nil
}

// numberToInexact converts a Number to its inexact representation.
//
// R7RS §7.1.1: Numbers containing # digit placeholders are inexact.
// R7RS §6.2.6: Inexact numbers use floating-point representation.
func (p *Parser) numberToInexact(num values.Number) values.Number {
	switch v := num.(type) {
	case *values.Integer:
		return values.NewFloat(float64(v.Value))
	case *values.BigInteger:
		f, _ := new(big.Float).SetInt(v.BigInt()).Float64()
		return values.NewFloat(f)
	case *values.Rational:
		f, _ := v.Rat().Float64()
		return values.NewFloat(f)
	case *values.BigComplex:
		reFloat := v.RealAsBigFloat().Float64()
		imFloat := v.ImagAsBigFloat().Float64()
		return values.NewComplexFromParts(reFloat, imFloat)
	default:
		// Float, BigFloat, Complex are already inexact
		return num
	}
}

// makeInexact converts a syntax-wrapped number to its inexact representation.
// R7RS §6.2.6: inexact converts an exact number to inexact.
func (p *Parser) makeInexact(stx syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	val := stx.Unwrap()
	num, ok := val.(values.Number)
	if !ok {
		return nil, errors.New("not a number")
	}

	var inexactNum values.Value
	switch v := num.(type) {
	case *values.Float, *values.BigFloat:
		// Already inexact
		inexactNum = v
	case *values.Complex:
		// Already inexact
		inexactNum = v
	case *values.Integer:
		inexactNum = values.NewFloat(float64(v.Value))
	case *values.BigInteger:
		f, _ := new(big.Float).SetInt(v.BigInt()).Float64()
		inexactNum = values.NewFloat(f)
	case *values.Rational:
		f, _ := v.Rat().Float64()
		inexactNum = values.NewFloat(f)
	case *values.BigComplex:
		// Convert to inexact Complex
		reFloat := v.RealAsBigFloat().Float64()
		imFloat := v.ImagAsBigFloat().Float64()
		inexactNum = values.NewComplexFromParts(reFloat, imFloat)
	default:
		return nil, errors.New("unsupported number type for inexact conversion")
	}

	return p.rewrapSyntax(stx, inexactNum), nil
}

// rewrapSyntax wraps a new value with the same syntax context as the original.
func (p *Parser) rewrapSyntax(orig syntax.SyntaxValue, newVal values.Value) syntax.SyntaxValue {
	// Get the source context from the original syntax value
	var sctx *syntax.SourceContext
	if so, ok := orig.(*syntax.SyntaxObject); ok {
		sctx = so.SourceContext()
	}
	return syntax.NewSyntaxObject(newVal, sctx)
}
