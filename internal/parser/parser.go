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
	"errors"
	"io"
	"math"
	"math/bits"
	"strconv"
	"strings"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/internal/tokenizer"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
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

// isListOpener returns true if the token type is ( or [.
// Provided for symmetry with isListCloser; may be useful for future code.
func (p *Parser) isListOpener(t tokenizer.TokenizerState) bool { //nolint:unused
	return t == tokenizer.TokenizerStateOpenParen || t == tokenizer.TokenizerStateOpenBracket
}

// isListCloser returns true if the token type is ) or ].
func (p *Parser) isListCloser(t tokenizer.TokenizerState) bool {
	return t == tokenizer.TokenizerStateCloseParen || t == tokenizer.TokenizerStateCloseBracket
}

// matchingClose returns the expected close delimiter for the given opener.
// R7RS §2.1: ( must match ), and [ must match ].
func (p *Parser) matchingClose(opener tokenizer.TokenizerState) tokenizer.TokenizerState {
	if opener == tokenizer.TokenizerStateOpenBracket {
		return tokenizer.TokenizerStateCloseBracket
	}
	return tokenizer.TokenizerStateCloseParen
}

// delimiterString returns a human-readable string for a delimiter token type.
func (p *Parser) delimiterString(t tokenizer.TokenizerState) string {
	switch t {
	case tokenizer.TokenizerStateOpenParen:
		return "("
	case tokenizer.TokenizerStateCloseParen:
		return ")"
	case tokenizer.TokenizerStateOpenBracket:
		return "["
	case tokenizer.TokenizerStateCloseBracket:
		return "]"
	default:
		return "unknown"
	}
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
		// R7RS: unexpected close delimiter at top level is a read error
		if q == nil && p.cur != nil && p.isListCloser(p.cur.Type()) {
			p.toks = nil
			p.err = NewParserErrorf(p.cur, "unexpected close %s", p.delimiterString(p.cur.Type()))
			return nil, p.err
		}
		// Advance to the next token for the next ReadSyntax() call
		p.cur, p.err = p.toks.Next()
		// EOF is fine - it means there's nothing more to read
		if p.err != nil && !errors.Is(p.err, io.EOF) {
			p.toks = nil
			return nil, p.err
		}
		if !p.skipComment {
			return q, nil
		}
		switch d := q.(type) {
		case *syntax.SyntaxComment, *syntax.SyntaxDatumComment:
			if errors.Is(p.err, io.EOF) {
				return nil, p.err
			}
			continue
		case *syntax.SyntaxDirective:
			// R7RS §2.1: Process fold-case directives
			p.processFoldCaseDirective(d)
			if errors.Is(p.err, io.EOF) {
				return nil, p.err
			}
			continue
		default:
			return q, nil
		}
	}
}

// readLabeledList reads a list into a pre-created placeholder pair.
// This enables circular references where the list refers to itself via datum labels.
// The placeholder must already be registered in datumLabels before calling this.
// The opener parameter indicates whether ( or [ was used, for bracket matching.
//
// R7RS §2.4: Datum labels enable representing shared/circular structures.
// For #0=(1 . #0#), we:
//  1. Pre-create a pair and register it as label 0
//  2. Read the list contents, which may include #0# references
//  3. Populate the pair with the read values
func (p *Parser) readLabeledList(placeholder *syntax.SyntaxPair, opener tokenizer.TokenizerState) (syntax.SyntaxValue, error) {
	expectedClose := p.matchingClose(opener)

	// Skip the opening paren/bracket
	p.cur, p.err = p.toks.Next()
	if p.err != nil {
		return nil, p.err
	}

	// Handle empty list: () or []
	if p.cur.Type() == expectedClose {
		// Empty list - return the placeholder unchanged (nil . nil)
		return placeholder, nil
	}
	// Check for bracket mismatch on close
	if p.isListCloser(p.cur.Type()) && p.cur.Type() != expectedClose {
		return nil, NewParserErrorf(p.cur, "mismatched delimiters: opened with %s but closed with %s",
			p.delimiterString(opener), p.delimiterString(p.cur.Type()))
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

	// Check for improper list: (a . b) or [a . b]
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
		// Advance past the cdr and expect matching close delimiter
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.err
		}
		if p.cur.Type() != expectedClose {
			if p.isListCloser(p.cur.Type()) {
				return nil, NewParserErrorf(p.cur, "mismatched delimiters: opened with %s but closed with %s",
					p.delimiterString(opener), p.delimiterString(p.cur.Type()))
			}
			return nil, NewParserErrorf(p.cur, "expected %s after improper list cdr",
				p.delimiterString(expectedClose))
		}
		return placeholder, nil
	}

	// Check for end of list
	if p.cur.Type() == expectedClose {
		// Single element list - set cdr to empty list
		placeholder.SetCdr(syntax.SyntaxEmptyList)
		return placeholder, nil
	}
	// Check for bracket mismatch
	if p.isListCloser(p.cur.Type()) && p.cur.Type() != expectedClose {
		return nil, NewParserErrorf(p.cur, "mismatched delimiters: opened with %s but closed with %s",
			p.delimiterString(opener), p.delimiterString(p.cur.Type()))
	}

	// Continue reading remaining elements
	current := placeholder
	for !p.isListCloser(p.cur.Type()) && p.cur.Type() != tokenizer.TokenizerStateCons {
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

	// Handle improper list ending: (a b . c) or [a b . c]
	switch {
	case p.cur.Type() == tokenizer.TokenizerStateCons:
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
		// Advance past the cdr and expect matching close delimiter
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.err
		}
		if p.cur.Type() != expectedClose {
			if p.isListCloser(p.cur.Type()) {
				return nil, NewParserErrorf(p.cur, "mismatched delimiters: opened with %s but closed with %s",
					p.delimiterString(opener), p.delimiterString(p.cur.Type()))
			}
			return nil, NewParserErrorf(p.cur, "expected %s after improper list cdr",
				p.delimiterString(expectedClose))
		}
	case p.cur.Type() == expectedClose:
		// Proper list - terminate with empty list
		current.SetCdr(syntax.SyntaxEmptyList)
	case p.isListCloser(p.cur.Type()):
		// Bracket mismatch
		return nil, NewParserErrorf(p.cur, "mismatched delimiters: opened with %s but closed with %s",
			p.delimiterString(opener), p.delimiterString(p.cur.Type()))
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
		return nil, p.cur, NewParserErrorWithWrap(werr.ErrNotACons, p.cur, "unexpected '.' token")
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
		case tokenizer.TokenizerStateOpenParen, tokenizer.TokenizerStateOpenBracket:
			// Pre-register an empty pair for potential circular references
			opener := p.cur.Type()
			placeholder := p.wrapSyntaxPair(nil, nil, p.cur)
			p.datumLabels[labelNum] = placeholder
			// Now read the list contents, which can reference this label
			v, p.err = p.readLabeledList(placeholder, opener)
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
			labeled, ok := p.datumLabels[int(i)]
			if ok {
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
	case tokenizer.TokenizerStateOpenParen, tokenizer.TokenizerStateOpenBracket:
		// R7RS §2.1: ( and [ are equivalent for opening lists, but must match
		opener := p.cur.Type()
		expectedClose := p.matchingClose(opener)
		var pr syntax.SyntaxValue
		pr = p.wrapSyntaxPair(nil, nil, p.cur)
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		q0 := pr
		pr0 := p.wrapSyntaxPair(nil, nil, p.cur)
		for !p.isListCloser(p.cur.Type()) && p.cur.Type() != tokenizer.TokenizerStateCons {
			var v syntax.SyntaxValue
			v, _, p.err = p.readSyntax()
			if p.err != nil {
				return nil, p.cur, p.err
			}
			// After skipping comments, we may have landed on a delimiter (close paren/bracket or cons)
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
		switch {
		case p.cur.Type() == tokenizer.TokenizerStateCons:
			// skip the '.' token
			p.cur, p.err = p.toks.Next()
			if p.err != nil {
				return nil, p.cur, p.err
			}
			// read cdr value in improper list
			var v syntax.SyntaxValue
			v, _, p.err = p.readSyntax()
			if p.err != nil {
				return nil, p.cur, p.err
			}
			pr = v
			p.cur, p.err = p.toks.Next()
			if p.err != nil {
				return nil, p.cur, p.err
			}
			// Check for bracket mismatch
			if p.cur.Type() != expectedClose {
				if p.isListCloser(p.cur.Type()) {
					return nil, p.cur, NewParserErrorf(p.cur, "mismatched delimiters: opened with %s but closed with %s",
						p.delimiterString(opener), p.delimiterString(p.cur.Type()))
				}
				return nil, p.cur, NewParserErrorWithWrapf(werr.ErrNotACloseParen, p.cur, "expected %s after dotted pair, got %s",
					p.delimiterString(expectedClose), p.cur.String())
			}
			pr0.SetCdr(pr)
		case p.cur.Type() == expectedClose:
			// Proper list terminated with matching delimiter
			pr = p.wrapSyntaxEmptyList(p.cur)
			pr0.SetCdr(pr)
		case p.isListCloser(p.cur.Type()):
			// Bracket mismatch
			return nil, p.cur, NewParserErrorf(p.cur, "mismatched delimiters: opened with %s but closed with %s",
				p.delimiterString(opener), p.delimiterString(p.cur.Type()))
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
			// Empty bytevector case: #u8()
			q = p.wrapSyntax(q0, p.cur)
			return q, p.cur, nil
		}
		i, ok := stx.Unwrap().(*values.Integer)
		for {
			if !ok {
				return nil, p.cur, NewParserErrorWithWrapf(werr.ErrNotAnInteger, p.cur, "expected unsigned byte integer in byte vector, got %T", stx.Unwrap())
			}
			if i.Value < 0 || i.Value > 255 {
				return nil, p.cur, NewParserErrorWithWrapf(werr.ErrNotAByte, p.cur, "byte value out of range (0-255): %d", i.Value)
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
		// #b prefix — next token is optional exactness marker then base-2 number
		return p.parseBaseWithExactness(2)
	case tokenizer.TokenizerStateMarkerBase8:
		// #o prefix — next token is optional exactness marker then base-8 number
		return p.parseBaseWithExactness(8)
	case tokenizer.TokenizerStateMarkerBase10:
		// #d prefix - next token is base 10 integer
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		return p.readSyntax()
	case tokenizer.TokenizerStateMarkerBase16:
		// #x prefix — next token is optional exactness marker then base-16 number
		return p.parseBaseWithExactness(16)
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
			return nil, nil, NewParserErrorWithWrapf(werr.ErrUnknownCharacterMnemonic, p.cur, "unknown character mnemonic: %s", s)
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
	case tokenizer.TokenizerStateCloseParen, tokenizer.TokenizerStateCloseBracket:
		// Close delimiters return nil to signal end of compound form
		return q, p.cur, nil
	}
	return q, nil, NewParserErrorWithWrapf(ErrUnknownTokenType, p.cur, "unknown token type: %q", p.cur.String())
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
