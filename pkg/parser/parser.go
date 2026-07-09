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
	"strconv"
	"strings"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/internal/tokenizer"
	"github.com/aalpar/wile/pkg/schemeutil"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
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

// DefaultMaxParseDepth bounds structural nesting depth during parsing.
// Without a bound, adversarial input such as deeply nested parentheses
// triggers a fatal, unrecoverable Go stack overflow that kills the host
// process. 0 means unlimited. Mirrors the VM's DefaultMaxCallDepth.
const DefaultMaxParseDepth int = 10000

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
	depth       int                        // current structural nesting depth
	maxDepth    int                        // max nesting depth; 0 = unlimited
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
		maxDepth:    DefaultMaxParseDepth,
	}
	return q
}

// SetMaxDepth sets the maximum structural nesting depth allowed during
// parsing. A value of 0 (or negative, clamped to 0) disables the limit.
// Mirrors MachineContext.SetMaxCallDepth.
func (p *Parser) SetMaxDepth(n int) {
	if n < 0 {
		n = 0
	}
	p.maxDepth = n
}

// SetFoldCase enables or disables R7RS §2.1 fold-case mode at construction time,
// before any forms are read. This is the programmatic equivalent of a leading
// #!fold-case directive; it lets a caller (e.g. include-ci) read an entire file
// case-insensitively without the file itself carrying the directive. An in-file
// #!fold-case / #!no-fold-case directive still toggles the mode mid-stream.
func (p *Parser) SetFoldCase(on bool) {
	p.foldCase = on
}

func (p *Parser) curr() tokenizer.Token {
	return p.cur
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

// checkDelimiterMatch returns an error if the current token is a closing
// delimiter that doesn't match the given opener. Returns nil if the current
// token is either the correct closer or not a closer at all.
func (p *Parser) checkDelimiterMatch(opener tokenizer.TokenizerState) error {
	if !p.isListCloser(p.cur.Type()) {
		return nil
	}
	expectedClose := p.matchingClose(opener)
	if p.cur.Type() == expectedClose {
		return nil
	}
	return NewParserErrorf(p.cur, "mismatched delimiters: opened with %s but closed with %s",
		p.delimiterString(opener), p.delimiterString(p.cur.Type()))
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
// It honors ctx: a cancelled or deadline-exceeded context returns its error
// before reading the next form, so a long multi-form parse can be interrupted.
func (p *Parser) ReadSyntax(ctx context.Context) (syntax.SyntaxValue, error) {
	cerr := ctx.Err()
	if cerr != nil {
		return nil, cerr
	}
	if p.toks == nil {
		p.toks = tokenizer.NewTokenizer(p.rdr, false)
		p.cur, p.err = p.toks.Next()
	}
	if p.err != nil {
		return nil, p.locateReaderErr(p.err)
	}
	var (
		q   syntax.SyntaxValue
		err error
	)

	for {
		q, _, err = p.readSyntax()
		// R7RS: an unexpected close delimiter at top level is a read error.
		// errNoDatum uniquely means readSyntax stopped on a closer (p.cur is it).
		if errors.Is(err, errNoDatum) {
			p.toks = nil
			// Route through locateReaderErr so the file name is stamped for
			// location reporting, exactly as for errors escaping readSyntax.
			p.err = p.locateReaderErr(NewParserErrorf(p.cur, "unexpected close %s", p.delimiterString(p.cur.Type())))
			return nil, p.err
		}
		if err != nil {
			p.toks = nil
			p.err = p.locateReaderErr(err)
			return nil, p.err
		}
		// Advance to the next token for the next ReadSyntax() call
		p.cur, p.err = p.toks.Next()
		// EOF is fine - it means there's nothing more to read
		if p.err != nil && !errors.Is(p.err, io.EOF) {
			p.toks = nil
			return nil, p.locateReaderErr(p.err)
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

// locateReaderErr lifts any lower-layer error escaping a read — a lexical
// "rune error" from invalid UTF-8, a strconv failure parsing a datum-label
// number, and so on — into a located *ParserError, so ReadSyntax presents
// callers a single error surface: a value, io.EOF, or a *ParserError. Wrapping
// specific error types is whack-a-mole (the reader bottoms out in tokenizer,
// strconv, and math/big calls); this guarantees the contract for whatever
// fails. A clean io.EOF and already-located errors pass through, and the
// original cause stays matchable via errors.As/Is through the wrap chain.
func (p *Parser) locateReaderErr(err error) error {
	if err == nil || errors.Is(err, io.EOF) {
		return err
	}
	var perr *ParserError
	if errors.As(err, &perr) {
		// Stamp the source file so ParserError.Location reports file:line:col.
		// The token already carries line/col; the file name lives only on the
		// parser, so this choke point is where the two are joined.
		perr.file = p.file
		return err
	}
	wrapped := NewParserErrorWithWrap(err, p.cur, "malformed input")
	wrapped.file = p.file
	return wrapped
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
func (p *Parser) readLabeledList(placeholder *syntax.SyntaxPair, labelNum int, opener tokenizer.TokenizerState) (syntax.SyntaxValue, error) {
	expectedClose := p.matchingClose(opener)

	// Skip the opening paren/bracket
	p.cur, p.err = p.toks.Next()
	if p.err != nil {
		return nil, wrapMidParseEOF(p.err, p.cur, "labeled list")
	}

	// Handle empty list: () or []
	if p.cur.Type() == expectedClose {
		// Empty labeled list (#0=()): bind the label to the empty list and
		// return it. Returning the pre-created placeholder pair unchanged would
		// leave a malformed (nil . nil) that renders as the non-re-readable
		// "(#<void>)" — a #n# back-reference must resolve to () as well.
		empty := p.wrapSyntaxEmptyList(p.cur)
		p.datumLabels[labelNum] = empty
		return empty, nil
	}
	// Check for bracket mismatch on close
	err := p.checkDelimiterMatch(opener)
	if err != nil {
		return nil, wrapMidParseEOF(err, p.cur, "labeled list")
	}

	// Read the first element
	first, _, err := p.readSyntax()
	// A #0= assignment needs a datum; stopping on a close delimiter is a located
	// error, not a SetCar(nil) that later panics on a nil interface conversion.
	if errors.Is(err, errNoDatum) {
		return nil, NewParserErrorWithWrap(werr.ErrNotACons, p.cur,
			"expected a datum in labeled list")
	}
	if err != nil {
		return nil, wrapMidParseEOF(err, p.cur, "labeled list")
	}
	placeholder.SetCar(first)

	// Advance to next token
	p.cur, p.err = p.toks.Next()
	if p.err != nil {
		return nil, wrapMidParseEOF(p.err, p.cur, "labeled list")
	}

	// Check for improper list: (a . b) or [a . b]
	if p.cur.Type() == tokenizer.TokenizerStateCons {
		// Skip the dot
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, wrapMidParseEOF(p.err, p.cur, "labeled list")
		}
		// Read the cdr
		cdr, _, err := p.readSyntax()
		// R7RS §7.1.2: the dot must be followed by a datum; stopping on a close
		// delimiter is a located error, not a SetCdr(nil) panic.
		if errors.Is(err, errNoDatum) {
			return nil, NewParserErrorWithWrap(werr.ErrNotACons, p.cur,
				"expected datum after '.' in dotted pair")
		}
		if err != nil {
			return nil, wrapMidParseEOF(err, p.cur, "labeled list")
		}
		placeholder.SetCdr(cdr)
		// Advance past the cdr and expect matching close delimiter
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, wrapMidParseEOF(p.err, p.cur, "labeled list")
		}
		if p.cur.Type() != expectedClose {
			err := p.checkDelimiterMatch(opener)
			if err != nil {
				return nil, wrapMidParseEOF(err, p.cur, "labeled list")
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
	err = p.checkDelimiterMatch(opener)
	if err != nil {
		return nil, wrapMidParseEOF(err, p.cur, "labeled list")
	}

	// Continue reading remaining elements
	current := placeholder
	for !p.isListCloser(p.cur.Type()) && p.cur.Type() != tokenizer.TokenizerStateCons {
		// Create a new pair for this element
		nextPair := p.wrapSyntaxPair(nil, nil, p.cur)

		// Read the element
		elem, _, err := p.readSyntax()
		// Stopping on a close delimiter mid-read must be a located error, not a
		// SetCar(nil) panic — see the first-element guard above.
		if errors.Is(err, errNoDatum) {
			return nil, NewParserErrorWithWrap(werr.ErrNotACons, p.cur,
				"expected a datum in labeled list")
		}
		if err != nil {
			return nil, wrapMidParseEOF(err, p.cur, "labeled list")
		}
		nextPair.SetCar(elem)

		// Link to previous
		current.SetCdr(nextPair)
		current = nextPair

		// Advance to next token
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, wrapMidParseEOF(p.err, p.cur, "labeled list")
		}
	}

	// Handle improper list ending: (a b . c) or [a b . c]
	switch {
	case p.cur.Type() == tokenizer.TokenizerStateCons:
		// Skip the dot
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, wrapMidParseEOF(p.err, p.cur, "labeled list")
		}
		// Read the final cdr
		cdr, _, err := p.readSyntax()
		// R7RS §7.1.2: the dot must be followed by a datum; stopping on a close
		// delimiter is a located error, not a SetCdr(nil) panic.
		if errors.Is(err, errNoDatum) {
			return nil, NewParserErrorWithWrap(werr.ErrNotACons, p.cur,
				"expected datum after '.' in dotted pair")
		}
		if err != nil {
			return nil, wrapMidParseEOF(err, p.cur, "labeled list")
		}
		current.SetCdr(cdr)
		// Advance past the cdr and expect matching close delimiter
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, wrapMidParseEOF(p.err, p.cur, "labeled list")
		}
		if p.cur.Type() != expectedClose {
			err := p.checkDelimiterMatch(opener)
			if err != nil {
				return nil, wrapMidParseEOF(err, p.cur, "labeled list")
			}
			return nil, NewParserErrorf(p.cur, "expected %s after improper list cdr",
				p.delimiterString(expectedClose))
		}
	case p.cur.Type() == expectedClose:
		// Proper list - terminate with empty list
		current.SetCdr(syntax.SyntaxEmptyList)
	case p.isListCloser(p.cur.Type()):
		// Bracket mismatch
		return nil, p.checkDelimiterMatch(opener)
	}

	return placeholder, nil
}

// readQuoteForm reads a quote-like form (quote, unquote, quasiquote, etc.).
// It saves the current token (the quote mark) for source location, advances
// the tokenizer, reads the next datum, and wraps it in a list with the given
// keyword symbol. The source location is the quote mark's position, not the
// datum's position.
func (p *Parser) readQuoteForm(keyword string) (syntax.SyntaxValue, tokenizer.Token, error) {
	t := p.curr()
	p.cur, p.err = p.toks.Next()
	if p.err != nil {
		return nil, p.cur, p.err
	}
	q, _, err := p.readSyntax()
	// A quote form with no datum (e.g. "' )") must be a located error, not a
	// quote wrapped around nil that panics in listSyntax. Mirrors the
	// datum-label and dotted-pair guards.
	if errors.Is(err, errNoDatum) {
		return nil, p.cur, NewParserErrorWithWrapf(werr.ErrNotACons, p.cur,
			"%s form requires a datum", keyword)
	}
	if err != nil {
		return nil, p.cur, err
	}
	sym := p.wrapSyntaxSymbol(keyword, t)
	result := p.listSyntax(t, sym, q)
	return result, p.cur, nil
}

// readExactnessMarker handles #e and #i exactness prefixes.
// It advances the tokenizer, reads the next datum, and applies the conversion
// function (makeExact or makeInexact) to it.
func (p *Parser) readExactnessMarker(label string, convert func(syntax.SyntaxValue) (syntax.SyntaxValue, error)) (syntax.SyntaxValue, tokenizer.Token, error) {
	p.cur, p.err = p.toks.Next()
	if p.err != nil {
		return nil, p.cur, p.err
	}
	q, tok, err := p.readSyntax()
	// An exactness marker with no number (e.g. "#e)") must be a located error,
	// not a nil deref in convert. Covers both #e and #i.
	if errors.Is(err, errNoDatum) {
		return nil, tok, NewParserErrorWithWrapf(werr.ErrNotANumber, tok,
			"%s number marker requires a datum", label)
	}
	if err != nil {
		return nil, tok, err
	}
	result, err := convert(q)
	if err != nil {
		return nil, tok, NewParserErrorWithWrapf(err, tok, "cannot convert to %s", label)
	}
	return result, tok, nil
}

// readLabelAssignment handles #n=<datum> — defining a datum label.
// R7RS §2.4: For circular/shared structures, we pre-register the container
// before reading its contents so back-references via #n# resolve correctly.
func (p *Parser) readLabelAssignment() (syntax.SyntaxValue, tokenizer.Token, error) {
	s := schemeutil.TrimPrefixCI(p.cur.String(), "#")
	is := strings.TrimSuffix(s, "=")
	var labelNum int
	labelNum, p.err = strconv.Atoi(is)
	if p.err != nil {
		return nil, p.cur, p.err
	}
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
		v, p.err = p.readLabeledList(placeholder, labelNum, opener)
		if p.err != nil {
			return nil, p.cur, p.err
		}
	case tokenizer.TokenizerStateOpenVector:
		// Pre-register a placeholder vector so a #n# inside resolves to the
		// vector itself (circular vectors, R7RS §2.4) — mirrors the list path.
		placeholder := p.wrapSyntaxVector(nil, p.cur)
		p.datumLabels[labelNum] = placeholder
		v, _, p.err = p.readVectorInto(placeholder)
		if p.err != nil {
			return nil, p.cur, p.err
		}
	default:
		// Non-compound datum: read normally and store.
		var err error
		v, _, err = p.readSyntax()
		// A label assignment with no datum (e.g. "#0=)") must be a located error,
		// not a silently stored nil. Mirrors the dotted-pair guard above. Keep the
		// sentinel out of the sticky p.err — it is a control signal, not a failure.
		if errors.Is(err, errNoDatum) {
			return nil, p.cur, NewParserErrorWithWrap(werr.ErrNotACons, p.cur,
				"datum label assignment requires a datum")
		}
		if err != nil {
			p.err = err
			return nil, p.cur, p.err
		}
		p.datumLabels[labelNum] = v
	}
	q := p.wrapSyntaxDatumLabelAssignment(labelNum, v, assignTok)
	return q, p.cur, nil
}

// readLabelReference handles #n# — referencing a previously defined datum label.
func (p *Parser) readLabelReference() (syntax.SyntaxValue, tokenizer.Token, error) {
	s := strings.Trim(p.cur.String(), "#")
	var parsed uint64
	parsed, p.err = strconv.ParseUint(s, 10, 31)
	if p.err != nil {
		return nil, p.cur, p.err
	}
	labelNum := int(parsed)
	// Look up the datum in the label table
	if p.datumLabels != nil {
		labeled, ok := p.datumLabels[labelNum]
		if ok {
			// Return the actual datum, not a label reference
			return labeled, p.cur, nil
		}
	}
	// R7RS §2.4: a #n# reference must follow its #n= definition. An undefined or
	// forward reference is a read error — not the integer n that the old
	// SyntaxDatumLabel placeholder silently produced via Unwrap.
	return nil, p.cur, NewParserErrorWithWrapf(werr.ErrDatumLabelUndefined, p.cur,
		"undefined datum label #%d#", labelNum)
}

// readDatumComment handles #; — reading and wrapping a datum comment.
func (p *Parser) readDatumComment() (syntax.SyntaxValue, tokenizer.Token, error) {
	beginTok := p.cur
	// Skip the datum comment begin token
	p.cur, p.err = p.toks.Next()
	if p.err != nil {
		return nil, p.cur, p.err
	}
	// Read the syntax value being commented
	var v syntax.SyntaxValue
	var err error
	v, _, err = p.readSyntax()
	// errNoDatum (a #; immediately before a close delimiter) leaves v nil,
	// preserving the prior literal-nil behavior; a real error propagates.
	if err != nil && !errors.Is(err, errNoDatum) {
		p.err = err
		return nil, p.cur, p.err
	}
	// Use beginTok.String() for correct label, but p.cur for source context (matches old behavior)
	q := p.wrapSyntaxDatumComment(beginTok.String(), v, p.cur)
	return q, p.cur, nil
}

// wrapMidParseEOF converts io.EOF (returned by the tokenizer at end of
// input, or propagated by nested readSyntax) into a ParserError wrapping
// io.ErrUnexpectedEOF. Callers inside compound readers (readList,
// readLabeledList, readVector, readByteVector) use this to distinguish
// mid-parse EOF ("source ended inside an unclosed form") from clean EOF
// at a token boundary. R7RS §6.13.2 requires mid-parse EOF to raise a
// read-error, not return the EOF object — PrimRead wraps the returned
// io.ErrUnexpectedEOF via WrapForeignReadErrorf. Non-EOF errors pass
// through unchanged.
//
// Free function, not a method: the subject of the operation is err (and
// the location tok at which it occurred), not the Parser itself. Taking
// both as explicit parameters makes the call-site synchronization
// contract visible; a receiver-held token would hide it.
func wrapMidParseEOF(err error, tok tokenizer.Token, form string) error {
	if errors.Is(err, io.EOF) {
		return NewParserErrorWithWrapf(io.ErrUnexpectedEOF, tok,
			"unterminated %s: unexpected end of input", form)
	}
	return err
}

// readList reads a list form opened by ( or [.
// Handles proper lists (a b c), improper lists (a b . c), and bracket matching.
func (p *Parser) readList(opener tokenizer.TokenizerState) (syntax.SyntaxValue, tokenizer.Token, error) {
	expectedClose := p.matchingClose(opener)
	var pr syntax.SyntaxValue
	pr = p.wrapSyntaxPair(nil, nil, p.cur)
	p.cur, p.err = p.toks.Next()
	if p.err != nil {
		return nil, p.cur, wrapMidParseEOF(p.err, p.cur, "list")
	}
	q0 := pr
	pr0 := p.wrapSyntaxPair(nil, nil, p.cur)
	// gotCar records whether the element loop read at least one datum, so the
	// dotted-pair branch below can reject a '.' in head position (a . b needs a
	// car) instead of mis-threading an orphan pair into (#<void>).
	gotCar := false
	for !p.isListCloser(p.cur.Type()) && p.cur.Type() != tokenizer.TokenizerStateCons {
		var v syntax.SyntaxValue
		var err error
		v, _, err = p.readSyntax()
		// Comment-skipping may have landed on a close delimiter → no datum; exit
		// the loop and let the post-loop check validate the closer. The sentinel
		// is a control signal, so it never enters the sticky p.err.
		if errors.Is(err, errNoDatum) {
			break
		}
		if err != nil {
			return nil, p.cur, wrapMidParseEOF(err, p.cur, "list")
		}
		pr0 = pr.(*syntax.SyntaxPair)
		pr0.SetCar(v)
		gotCar = true
		pr0.SetCdr(p.wrapSyntaxPair(nil, nil, p.cur))
		pr = pr0.Cdr().(*syntax.SyntaxPair)
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, wrapMidParseEOF(p.err, p.cur, "list")
		}
	}
	switch {
	case p.cur.Type() == tokenizer.TokenizerStateCons:
		// R7RS §7.1.2: a dotted pair (a . b) requires a car before the dot.
		// With no datum read, the '.' is in head position — a located read
		// error, not a silently mis-parsed (#<void>) with an orphaned tail.
		if !gotCar {
			return nil, p.cur, NewParserErrorWithWrap(werr.ErrNotACons, p.cur,
				"unexpected '.' with no preceding datum")
		}
		// skip the '.' token
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, wrapMidParseEOF(p.err, p.cur, "list")
		}
		// read cdr value in improper list
		var v syntax.SyntaxValue
		var err error
		v, _, err = p.readSyntax()
		// R7RS §7.1.2: the dot must be followed by a datum; stopping on a close
		// delimiter is a located error, not a SetCdr(nil) panic.
		if errors.Is(err, errNoDatum) {
			return nil, p.cur, NewParserErrorWithWrap(werr.ErrNotACons, p.cur,
				"expected datum after '.' in dotted pair")
		}
		if err != nil {
			return nil, p.cur, wrapMidParseEOF(err, p.cur, "list")
		}
		pr = v
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, wrapMidParseEOF(p.err, p.cur, "list")
		}
		// Check for bracket mismatch
		if p.cur.Type() != expectedClose {
			err := p.checkDelimiterMatch(opener)
			if err != nil {
				return nil, p.cur, err
			}
			return nil, p.cur, NewParserErrorWithWrapf(werr.ErrNotACloseParen, p.cur, "expected %s after dotted pair, got %s",
				p.delimiterString(expectedClose), p.cur.String())
		}
		pr0.SetCdr(pr)
	case p.cur.Type() == expectedClose:
		// R7RS §7.1.2: a zero-element list form — "( )" with whitespace, or a
		// comment-only form — never ran the element loop, so q0/pr0 are
		// unpopulated placeholders. Return the empty list directly rather than
		// the (#<void>) orphan pair q0 would otherwise carry.
		if !gotCar {
			return p.wrapSyntaxEmptyList(p.cur), p.cur, nil
		}
		// Proper list terminated with matching delimiter.
		pr = p.wrapSyntaxEmptyList(p.cur)
		pr0.SetCdr(pr)
	case p.isListCloser(p.cur.Type()):
		// Bracket mismatch
		return nil, p.cur, p.checkDelimiterMatch(opener)
	}
	return q0, p.cur, nil
}

// readVector reads a vector form opened by #(.
func (p *Parser) readVector() (syntax.SyntaxValue, tokenizer.Token, error) {
	return p.readVectorInto(p.wrapSyntaxVector(nil, p.cur))
}

// readVectorInto reads vector elements into q, a pre-created (and possibly
// already label-registered) vector. Filling a pre-registered placeholder lets a
// datum label inside the vector resolve to the vector itself, so circular
// vectors like #0=#(1 #0#) read correctly — mirroring readLabeledList for pairs.
func (p *Parser) readVectorInto(q *syntax.SyntaxVector) (syntax.SyntaxValue, tokenizer.Token, error) {
	// Advance past #( token
	p.cur, p.err = p.toks.Next()
	if p.err != nil {
		return nil, p.cur, wrapMidParseEOF(p.err, p.cur, "vector")
	}
	// Read vector elements - match the list parsing pattern:
	// check token type BEFORE reading, advance AFTER reading
	for p.cur.Type() != tokenizer.TokenizerStateCloseParen {
		var v syntax.SyntaxValue
		var err error
		v, _, err = p.readSyntax()
		// No datum means comment-skipping landed on a close delimiter; stop and
		// let the post-loop check validate the closer, mirroring readList. Without
		// this, a non-) closer (e.g. ]) is appended as a nil element and the close
		// is consumed, yielding a malformed vector + EOF error.
		if errors.Is(err, errNoDatum) {
			break
		}
		if err != nil {
			return nil, p.cur, wrapMidParseEOF(err, p.cur, "vector")
		}
		q.Values = append(q.Values, v)
		// Advance to next element (or close paren)
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, wrapMidParseEOF(p.err, p.cur, "vector")
		}
	}
	// A vector opened with #( must close with ). The loop exits on ) (its
	// condition) or breaks after readSyntax landed on another closer; anything
	// but ) is a located mismatch, R7RS §2.1.
	if p.cur.Type() != tokenizer.TokenizerStateCloseParen {
		return nil, p.cur, NewParserErrorWithWrapf(werr.ErrNotACloseParen, p.cur,
			"vector closed with %q, expected )", p.cur.String())
	}
	return q, p.cur, nil
}

// checkByteVectorClose guards the two stx.Unwrap sites in readByteVector.
// readSyntax reports errNoDatum only when it lands on a close delimiter, and the
// caller checks for ) (and any real error) before calling this. A bytevector
// opened with #u8( must close with ), so errNoDatum here means a non-) closer
// (e.g. ]) — a located mismatch error, not a nil-pointer dereference that
// crashes the host. A nil err means a datum was read.
func (p *Parser) checkByteVectorClose(err error) error {
	if !errors.Is(err, errNoDatum) {
		return nil
	}
	return NewParserErrorWithWrapf(werr.ErrNotACloseParen, p.cur,
		"bytevector closed with %q, expected )", p.cur.String())
}

// readByteVector reads a byte vector form opened by #u8(.
func (p *Parser) readByteVector() (syntax.SyntaxValue, tokenizer.Token, error) {
	q0 := values.NewByteVector()
	p.cur, p.err = p.toks.Next()
	if p.err != nil {
		return nil, p.cur, wrapMidParseEOF(p.err, p.cur, "bytevector")
	}
	var stx syntax.SyntaxValue
	var err error
	stx, _, err = p.readSyntax()
	if err != nil && !errors.Is(err, errNoDatum) {
		return nil, p.cur, wrapMidParseEOF(err, p.cur, "bytevector")
	}
	if p.curr().Type() == tokenizer.TokenizerStateCloseParen {
		// Empty bytevector case: #u8()
		return p.wrapSyntax(q0, p.cur), p.cur, nil
	}
	cerr := p.checkByteVectorClose(err)
	if cerr != nil {
		return nil, p.cur, cerr
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
			return nil, p.cur, wrapMidParseEOF(p.err, p.cur, "bytevector")
		}
		stx, _, err = p.readSyntax()
		if err != nil && !errors.Is(err, errNoDatum) {
			return nil, p.cur, wrapMidParseEOF(err, p.cur, "bytevector")
		}
		if p.curr().Type() == tokenizer.TokenizerStateCloseParen {
			break
		}
		cerr := p.checkByteVectorClose(err)
		if cerr != nil {
			return nil, p.cur, cerr
		}
		i, ok = stx.Unwrap().(*values.Integer)
	}
	return p.wrapSyntax(q0, p.cur), p.cur, nil
}

// parseCharacter parses a character literal from the current token.
// Handles three character token types: graphic (#\a), mnemonic (#\space),
// and hex escape (#\x41).
func (p *Parser) parseCharacter() (syntax.SyntaxValue, tokenizer.Token, error) {
	switch p.cur.Type() {
	case tokenizer.TokenizerStateCharGraphic:
		s := schemeutil.TrimPrefixCI(p.cur.String(), values.PrefixCharacter)
		rs := []rune(s)
		// A graphic char token with no rune (malformed input, e.g. "#\<NUL>")
		// must be a located error, not an rs[0] index-out-of-range panic.
		if len(rs) == 0 {
			return nil, p.cur, NewParserErrorWithWrap(werr.ErrNotACharacter, p.cur,
				"empty character literal")
		}
		q := p.wrapSyntax(values.NewCharacter(rs[0]), p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateCharMnemonic:
		s := strings.ToLower(schemeutil.TrimPrefixCI(p.cur.String(), values.PrefixCharacter))
		r, ok := tokenizer.CharMnemonics[s]
		if !ok {
			return nil, p.cur, NewParserErrorWithWrapf(werr.ErrUnknownCharacterMnemonic, p.cur, "unknown character mnemonic: %s", s)
		}
		q := p.wrapSyntax(values.NewCharacter(r), p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateCharHexEscape:
		s := schemeutil.TrimPrefixCI(p.cur.String(), "#\\x")
		var i int64
		i, p.err = strconv.ParseInt(s, 16, 32)
		if p.err != nil {
			return nil, p.cur, NewParserErrorWithWrapf(p.err, p.cur, "invalid character hex escape: %s", s)
		}
		q := p.wrapSyntax(values.NewCharacter(rune(i)), p.cur)
		return q, p.cur, nil
	default:
		return nil, p.cur, NewParserErrorWithWrapf(ErrUnknownTokenType, p.cur, "parseCharacter: unexpected token type: %q", p.cur.String())
	}
}

// readSyntax reads one datum, dispatching on the current token type.
//
// When it stops on a close delimiter instead of a datum, it returns errNoDatum
// in the error channel (with a nil value). Compound readers detect that with
// errors.Is(err, errNoDatum) — either exiting an element loop or turning it into
// a located "expected a datum" error. This sentinel replaces an older
// literal-nil return whose "never a typed-nil" invariant was enforced only by
// prose and re-checked at every call site.
func (p *Parser) readSyntax() (syntax.SyntaxValue, tokenizer.Token, error) {
	p.depth++
	defer func() {
		p.depth--
	}()
	if p.maxDepth > 0 && p.depth > p.maxDepth {
		p.err = NewParserErrorWithWrapf(werr.ErrParseDepthExceeded, p.cur,
			"nesting depth exceeds maximum of %d", p.maxDepth)
		return nil, p.cur, p.err
	}

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
			// Read and discard the commented datum. errNoDatum (a #; before a
			// close delimiter) falls through, matching the prior nil-return
			// behavior; only a real error aborts.
			_, _, cerr := p.readSyntax()
			if cerr != nil && !errors.Is(cerr, errNoDatum) {
				p.err = cerr
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
			d := p.wrapSyntaxDirective(schemeutil.TrimPrefixCI(p.cur.String(), "#!"), p.cur)
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
		return p.readLabelAssignment()
	case tokenizer.TokenizerStateLabelReference:
		return p.readLabelReference()
	case tokenizer.TokenizerStateDirective:
		q = p.wrapSyntaxDirective(schemeutil.TrimPrefixCI(p.cur.String(), "#!"), p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateLineCommentBody, tokenizer.TokenizerStateBlockCommentBody:
		q = p.wrapSyntaxComment(p.cur.String(), p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateDatumCommentBegin:
		return p.readDatumComment()
	case tokenizer.TokenizerStateOpenParen, tokenizer.TokenizerStateOpenBracket:
		return p.readList(p.cur.Type())
	case tokenizer.TokenizerStateOpenVector:
		return p.readVector()
	case tokenizer.TokenizerStateOpenVectorUnsignedByteMarker:
		return p.readByteVector()
	case tokenizer.TokenizerStateUnquote:
		return p.readQuoteForm(ConstUnquote)
	case tokenizer.TokenizerStateQuasiquote:
		return p.readQuoteForm(ConstQuasiquote)
	case tokenizer.TokenizerStateUnquoteSplicing:
		return p.readQuoteForm(ConstUnquoteSplicing)
	case tokenizer.TokenizerStateQuote:
		return p.readQuoteForm(ConstQuote)
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
		return p.parseIntegerWithBase(p.cur.Radix())
	case tokenizer.TokenizerStateUnsignedDecimalFraction, tokenizer.TokenizerStateSignedDecimalFraction:
		return p.parseDecimalFraction()
	case tokenizer.TokenizerStateSignedScientificNotation,
		tokenizer.TokenizerStateUnsignedScientificNotation:
		return p.parseScientificNotation()
	case tokenizer.TokenizerStateUnsignedRationalFraction, tokenizer.TokenizerStateSignedRationalFraction:
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
		return p.parseBaseWithExactness(2)
	case tokenizer.TokenizerStateMarkerBase8:
		return p.parseBaseWithExactness(8)
	case tokenizer.TokenizerStateMarkerBase10:
		p.cur, p.err = p.toks.Next()
		if p.err != nil {
			return nil, p.cur, p.err
		}
		return p.readSyntax()
	case tokenizer.TokenizerStateMarkerBase16:
		return p.parseBaseWithExactness(16)
	case tokenizer.TokenizerStateSignedInf:
		stx, tok := p.parseSignedInf()
		return stx, tok, nil
	case tokenizer.TokenizerStateSignedNan:
		stx, tok := p.parseSignedNan()
		return stx, tok, nil
	case tokenizer.TokenizerStateSignedImaginary, tokenizer.TokenizerStateUnsignedImaginary:
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
	case tokenizer.TokenizerStateSignedImaginaryInf, tokenizer.TokenizerStateUnsignedImaginaryInf:
		stx, tok := p.parseImaginaryInf()
		return stx, tok, nil
	case tokenizer.TokenizerStateSignedImaginaryNan, tokenizer.TokenizerStateUnsignedImaginaryNan:
		stx, tok := p.parseImaginaryNan()
		return stx, tok, nil
	case tokenizer.TokenizerStateUnsignedComplex, tokenizer.TokenizerStateSignedComplex:
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
		var q1 *values.Complex
		q1, p.err = p.parsePolarComplex(replaceHashDigits(p.cur.String()))
		if p.err != nil {
			return nil, p.cur, p.err
		}
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateMarkerNumberExact:
		return p.readExactnessMarker("exact", p.makeExact)
	case tokenizer.TokenizerStateMarkerNumberInexact:
		return p.readExactnessMarker("inexact", p.makeInexact)
	case tokenizer.TokenizerStateBigInteger:
		// #z is always decimal.
		return p.parseBigIntegerWithBase(10)
	case tokenizer.TokenizerStateBigFloat:
		return p.parseBigFloat()
	case tokenizer.TokenizerStateMarkerBooleanTrue:
		q = p.wrapSyntax(values.TrueValue, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateMarkerBooleanFalse:
		q = p.wrapSyntax(values.FalseValue, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateEmptyList:
		q = p.wrapSyntaxEmptyList(p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateCharGraphic,
		tokenizer.TokenizerStateCharMnemonic,
		tokenizer.TokenizerStateCharHexEscape:
		return p.parseCharacter()
	case tokenizer.TokenizerStateString:
		q = p.wrapSyntax(values.NewString(p.cur.Value()), p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateCloseParen, tokenizer.TokenizerStateCloseBracket:
		// A close delimiter is not a datum. Signal that through the error channel
		// with errNoDatum (io.EOF-style sentinel) rather than a fragile literal-nil
		// value; compound readers match it with errors.Is. p.cur stays on the
		// delimiter for the caller to inspect.
		return nil, p.cur, errNoDatum
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
