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
	err         error // ReadSyntax's cross-call EOF lookahead cache; written only there (see advance)
	dead        error // terminal state: nil means alive, non-nil is what every further read returns
	skipComment bool
	foldCase    bool                       // R7RS §2.1: #!fold-case mode for identifiers
	file        string                     // source file name for error reporting
	datumLabels map[int]syntax.SyntaxValue // R7RS §2.4 datum labels (#n= and #n#)
	// labelBackrefs counts how many times each label has been resolved by a #n#.
	// readLabelAssignment brackets its datum's read with the count to decide
	// whether that datum refers to itself — a shared datum is referenced only
	// AFTER its assignment returns, a cyclic one during. Scoped with datumLabels.
	labelBackrefs map[int]int
	depth         int // current structural nesting depth
	maxDepth      int // max nesting depth; 0 = unlimited
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
	// A closed or failed parser stays that way. This has to be consulted before
	// the rebuild below, because "no tokenizer" is the state both Close and a
	// read error leave behind — rebuilding resurrects the parser straight off
	// the underlying reader, which is how Close then ReadSyntax kept reading and
	// how "(1 2] (3 4)" reported the mismatched delimiter once and then handed
	// back (3 4).
	if p.dead != nil {
		return nil, p.dead
	}
	if p.toks == nil {
		p.toks = tokenizer.NewTokenizer(p.rdr, false)
		p.err = p.advance()
	}
	if p.err != nil {
		return nil, p.locateReaderErr(p.err)
	}
	// R7RS §2.4: a datum label's scope is the datum in which it appears. Carrying
	// the table across calls let a #n# in one top-level form resolve to a #n=
	// defined in an earlier one.
	p.datumLabels = nil
	p.labelBackrefs = nil
	var (
		q   syntax.SyntaxValue
		err error
	)

	for {
		q, _, err = p.readSyntax()
		// R7RS: an unexpected close delimiter at top level is a read error.
		// errNoDatum uniquely means readSyntax stopped on a closer (p.cur is it).
		if errors.Is(err, errNoDatum) {
			return nil, p.die(NewParserErrorf(p.cur, "unexpected close %s", p.delimiterString(p.cur.Type())))
		}
		if err != nil {
			return nil, p.die(err)
		}
		// Advance to the next token for the next ReadSyntax() call. This is the
		// one advance whose error is intentionally retained: a trailing io.EOF
		// stored here is read at the top of the next ReadSyntax call so it
		// returns EOF immediately. This is p.err's sole reason to exist.
		p.err = p.advance()
		// EOF is fine - it means there's nothing more to read
		if p.err != nil && !errors.Is(p.err, io.EOF) {
			return nil, p.die(p.err)
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
	// p.cur is usually nil here: Tokenizer.Next reports a token and the error
	// that ended it on separate calls, so by the time the error surfaces the
	// token has been consumed. ParserError.Location recovers the position from
	// the tokenizer error instead — the message is generic, the cause is not.
	wrapped := NewParserErrorWithWrap(err, p.cur, messageReadFailed)
	wrapped.file = p.file
	return wrapped
}

// die puts the parser in its terminal state and returns the located error every
// later ReadSyntax will return. Death is permanent: the tokenizer is abandoned
// mid-token and its bytes are already consumed, so resuming would silently
// accept the remainder of an input the reader has already diagnosed as
// malformed — "(1 2] (3 4) (5 6)" reported the mismatched delimiter once and
// then returned (3 4) and (5 6).
//
// The flag is what carries that state, not the nil tokenizer and not p.err.
// Dropping the tokenizer is only the release; ReadSyntax rebuilds one on demand.
func (p *Parser) die(err error) error {
	p.toks = nil
	p.dead = p.locateReaderErr(err)
	return p.dead
}

// advance reads the next token into p.cur and returns any tokenizer error.
//
// It deliberately does NOT write p.err. Every compound reader (readList,
// readLabeledList, readVector, readByteVector, the number parsers) advances
// through this chokepoint and propagates the error via its own return value,
// so a transient read error never lands in the cross-call field. p.err holds
// ReadSyntax's cross-call EOF lookahead — the trailing io.EOF the parser must
// remember between calls so the next call reports it immediately. It is NOT the
// "parser is dead" flag: that is p.dead, set only by die, and it is what makes
// the invariant true by construction rather than by convention.
func (p *Parser) advance() error {
	var err error
	p.cur, err = p.toks.Next()
	return err
}

// readLabeledList reads a list into a pre-created placeholder pair, enabling
// circular references where the list refers to itself via a datum label. The
// placeholder must already be registered in datumLabels before calling this, so
// a #n# encountered while reading elements resolves to the eventual list head.
//
// R7RS §2.4: for #0=(1 . #0#) the caller pre-creates a pair, registers it as
// label 0, and calls here; the shared readListInto loop fills that exact pair,
// so the inner #0# resolves back to it. The list grammar itself (proper,
// improper, empty, bracket matching) lives once in readListInto — this wrapper
// only supplies the placeholder and re-points the label afterward.
func (p *Parser) readLabeledList(placeholder *syntax.SyntaxPair, labelNum int, opener tokenizer.TokenizerState) (syntax.SyntaxValue, error) {
	q, _, err := p.readListInto(placeholder, opener)
	if err != nil {
		return nil, err
	}
	// An empty list (#0=(), or one whose only elements were elided by #;) yields
	// the empty-list singleton, not the placeholder. Re-point the label to the
	// actual result so a later #n# resolves to () rather than the abandoned
	// (nil . nil) placeholder. For a non-empty list q == placeholder, so this is
	// a harmless re-store.
	p.datumLabels[labelNum] = q
	return q, nil
}

// readQuoteForm reads a quote-like form (quote, unquote, quasiquote, etc.).
// It saves the current token (the quote mark) for source location, advances
// the tokenizer, reads the next datum, and wraps it in a list with the given
// keyword symbol. The source location is the quote mark's position, not the
// datum's position.
func (p *Parser) readQuoteForm(keyword string) (syntax.SyntaxValue, tokenizer.Token, error) {
	t := p.curr()
	err := p.advance()
	if err != nil {
		// EOF here means the input ended ON the quote mark. R7RS §7.1.2: a quote
		// form is not a datum without the datum it quotes, so this is malformed
		// input, not a clean end of input — a bare io.EOF would let the caller
		// treat "'" as an empty file.
		return nil, p.cur, wrapMidParseEOF(err, t, keyword+" form")
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

// readNumericIntroducer handles the four "#" forms that dispatch once and then
// take one complete datum: #e, #i (exactness) and #z, #m (widening). It advances
// past the marker, reads the datum, and applies convert.
//
// This shape is the whole reason radix and exactness composition is inherited
// rather than enumerated: the inner datum is read by the ordinary number reader,
// so #z#e#x1f works without #z knowing that #e exists, and #z#b19 fails because
// #b19 fails. See CLAUDE.local.md → "`#` Reader Dispatch".
//
// convert receives the datum's own token as well as its value, because #e must
// consult the literal's source text — it converts the number as written, and the
// token's radix decides whether those digits are authoritative (makeExactLiteral).
// The other three ignore it.
//
// label names the marker in diagnostics, e.g. "exact number marker".
func (p *Parser) readNumericIntroducer(label string, convert func(syntax.SyntaxValue, tokenizer.Token) (syntax.SyntaxValue, error)) (syntax.SyntaxValue, tokenizer.Token, error) {
	markerTok := p.curr()
	err := p.advance()
	if err != nil {
		// A prefix with nothing after it is malformed input, not a clean EOF —
		// same rule as the errNoDatum guard below, which rejects "#e)".
		return nil, p.cur, wrapMidParseEOF(err, markerTok, label)
	}
	q, tok, err := p.readSyntax()
	// A marker with no number (e.g. "#e)", "(#z)") must be a located error, not
	// a nil deref in convert.
	if errors.Is(err, errNoDatum) {
		return nil, tok, NewParserErrorWithWrapf(werr.ErrNotANumber, tok,
			"%s requires a datum", label)
	}
	if err != nil {
		return nil, tok, err
	}
	result, err := convert(q, tok)
	if err != nil {
		return nil, tok, NewParserErrorWithWrapf(err, tok, "%s: cannot convert datum", label)
	}
	return result, tok, nil
}

// readLabelAssignment handles #n=<datum> — defining a datum label.
// R7RS §2.4: For circular/shared structures, we pre-register the container
// before reading its contents so back-references via #n# resolve correctly.
func (p *Parser) readLabelAssignment() (syntax.SyntaxValue, tokenizer.Token, error) {
	s := schemeutil.TrimPrefixCI(p.cur.String(), "#")
	is := strings.TrimSuffix(s, "=")
	labelNum, err := strconv.Atoi(is)
	if err != nil {
		return nil, p.cur, err
	}
	if p.datumLabels == nil {
		p.datumLabels = make(map[int]syntax.SyntaxValue)
	}
	if p.labelBackrefs == nil {
		p.labelBackrefs = make(map[int]int)
	}
	// Snapshot before the datum is read: any #n# that resolves to THIS label
	// while its own datum is still being read makes the result cyclic.
	backrefsBefore := p.labelBackrefs[labelNum]
	assignTok := p.cur
	err = p.advance()
	if err != nil {
		// "#0=" with nothing after it: a label assignment needs its datum, so an
		// EOF here is malformed input rather than a clean end of input.
		return nil, p.cur, wrapMidParseEOF(err, assignTok, "datum label assignment")
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
		v, err = p.readLabeledList(placeholder, labelNum, opener)
		if err != nil {
			return nil, p.cur, err
		}
	case tokenizer.TokenizerStateOpenVector:
		// Pre-register a placeholder vector so a #n# inside resolves to the
		// vector itself (circular vectors, R7RS §2.4) — mirrors the list path.
		placeholder := p.wrapSyntaxVector(nil, p.cur)
		p.datumLabels[labelNum] = placeholder
		v, _, err = p.readVectorInto(placeholder)
		if err != nil {
			return nil, p.cur, err
		}
	case tokenizer.TokenizerStateBoxBegin:
		// A box is a container, so it can hold itself — and the writer emits
		// exactly that for a cyclic box (writeBox assigns a label), which makes
		// this the case that keeps Wile's own output readable (design C6). The
		// placeholder therefore has to be registered before the content is read,
		// as in the list and vector arms; the default arm below reads the whole
		// datum first and would report "undefined datum label" for #0=#&#0#.
		placeholder := p.wrapSyntaxBox(nil, p.cur)
		p.datumLabels[labelNum] = placeholder
		v, _, err = p.readBoxInto(placeholder)
		if err != nil {
			return nil, p.cur, err
		}
	default:
		// Non-compound datum: read normally and store.
		v, _, err = p.readSyntax()
		// A label assignment with no datum (e.g. "#0=)") must be a located error,
		// not a silently stored nil. Mirrors the dotted-pair guard above.
		if errors.Is(err, errNoDatum) {
			return nil, p.cur, NewParserErrorWithWrap(werr.ErrNotACons, p.cur,
				"datum label assignment requires a datum")
		}
		if err != nil {
			return nil, p.cur, err
		}
		p.datumLabels[labelNum] = v
	}
	// An ACYCLIC labeled datum is returned as itself, not as a wrapper. Every arm
	// above has already entered v into p.datumLabels, so a #n# back-reference
	// resolves without the wrapper; it carried nothing the reader needed and
	// everything downstream needed not to see — it reached the compiler as an
	// opaque literal, which is why "#0=(+ 1 2)" evaluated to the list rather than
	// to 3, "#0=x" to the symbol x, and "(#0=+ 1 2)" to "expected a procedure".
	//
	// A CYCLIC one keeps its wrapper, and that is not a leftover: the wrapper is
	// the fence that stops the EXPANDER walking a self-referential spine. Wile
	// already refuses a circular datum label as code (validateQuotedLiteral), but
	// that check runs in the compiler, one stage after the expander's argument
	// walk — so "(car #0=(a . #0#))" without the fence is an unbounded expansion,
	// not a diagnostic. Reading circular data still works either way: the label
	// table holds the real datum, and quote/read reach it through Unwrap. The
	// writer emits "#0=" for cyclic structure, so this round-trip is load-bearing
	// (design C6), which is why refusing cyclic labels at READ time is not an
	// option.
	if p.labelBackrefs[labelNum] > backrefsBefore {
		return p.wrapSyntaxDatumLabelAssignment(labelNum, v, assignTok), p.cur, nil
	}
	return v, p.cur, nil
}

// readLabelReference handles #n# — referencing a previously defined datum label.
func (p *Parser) readLabelReference() (syntax.SyntaxValue, tokenizer.Token, error) {
	s := strings.Trim(p.cur.String(), "#")
	parsed, err := strconv.ParseUint(s, 10, 31)
	if err != nil {
		return nil, p.cur, err
	}
	labelNum := int(parsed)
	// Look up the datum in the label table
	if p.datumLabels != nil {
		labeled, ok := p.datumLabels[labelNum]
		if ok {
			// Count the resolution so readLabelAssignment can tell a datum that
			// refers to ITSELF from one that is merely shared. Only the former
			// makes the structure cyclic, and only the former has to keep its
			// wrapper.
			p.labelBackrefs[labelNum]++
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
	err := p.advance()
	if err != nil {
		return nil, p.cur, err
	}
	// Read the syntax value being commented
	var v syntax.SyntaxValue
	v, _, err = p.readSyntax()
	// "#;)" must be a located error rather than a node whose Value is nil:
	// SchemeString and EqualTo both dereference Value, so such a node panics on
	// any use, and at top level nothing stops it escaping ReadSyntax. Same guard
	// readBoxInto and the numeric introducers carry.
	//
	// This arm is the comment-*preserving* mode (skipComment false), where a
	// datum comment with no datum has nothing to preserve. It is not the
	// leniency ReadSyntax's skip loop keeps for "(1 #;)".
	if errors.Is(err, errNoDatum) {
		return nil, p.cur, NewParserErrorWithWrap(werr.ErrInvalidSyntax, p.cur,
			"datum comment requires a datum")
	}
	if err != nil {
		return nil, p.cur, err
	}
	// Use beginTok.String() for correct label, but p.cur for source context (matches old behavior)
	q := p.wrapSyntaxDatumComment(beginTok.String(), v, p.cur)
	return q, p.cur, nil
}

// readBox handles #&<datum>, the box read syntax.
//
// #& is a container introducer, not a coercion: it dispatches once and then
// reads one complete datum, which may carry its own prefixes (#&#x1f is a box
// holding 31), and it nests (#&#&5 is a box holding a box). That is the
// distinction from #z, which widens rather than wraps.
//
// This exists because the write side already did: values.PrefixBox and writeBox
// emit "#&", so before this a box printed in a syntax the reader rejected.
func (p *Parser) readBox() (syntax.SyntaxValue, tokenizer.Token, error) {
	return p.readBoxInto(p.wrapSyntaxBox(nil, p.cur))
}

// readBoxInto fills a box that already exists, which is what makes a
// self-referential box readable: readLabelAssignment registers the placeholder
// under its label before the content is read, so a #n# inside resolves to the
// box itself. Same two entry points as readList / readListInto.
func (p *Parser) readBoxInto(placeholder *syntax.SyntaxBox) (syntax.SyntaxValue, tokenizer.Token, error) {
	beginTok := p.curr()
	err := p.advance()
	if err != nil {
		// "#&" with nothing after it needs its datum, so EOF here is malformed
		// input rather than a clean end of input.
		return nil, p.cur, wrapMidParseEOF(err, beginTok, "box")
	}
	v, tok, err := p.readSyntax()
	// "(#&)" must be a located error rather than a leaked errNoDatum that the
	// enclosing list reader silently treats as "no element" — the same guard the
	// numeric introducers carry.
	if errors.Is(err, errNoDatum) {
		return nil, tok, NewParserErrorWithWrap(werr.ErrInvalidSyntax, tok,
			"box marker requires a datum")
	}
	if err != nil {
		return nil, tok, err
	}
	placeholder.Value = v
	return placeholder, tok, nil
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
//
// Callers must pass the form's OPENING token, not p.cur: at EOF the tokenizer
// hands back a nil token, so a p.cur here would leave ParserError.Location
// empty and drop the error's provenance (REVIEW.md "Error Chain Losslessness").
// The opener is also the location a reader wants — it names the delimiter that
// was never closed.
func wrapMidParseEOF(err error, tok tokenizer.Token, form string) error {
	if errors.Is(err, io.EOF) {
		return NewParserErrorWithWrapf(io.ErrUnexpectedEOF, tok,
			"unterminated %s: unexpected end of input", form)
	}
	return err
}

// readList reads a fresh list form opened by ( or [. It allocates its own head
// placeholder wrapping the opener token, then fills it via the shared element
// loop in readListInto. readLabeledList takes the other entry point, passing a
// pre-registered placeholder so #n# back-references resolve to the list head.
func (p *Parser) readList(opener tokenizer.TokenizerState) (syntax.SyntaxValue, tokenizer.Token, error) {
	return p.readListInto(p.wrapSyntaxPair(nil, nil, p.cur), opener)
}

// readListInto reads a list form into head, a caller-supplied placeholder pair
// already wrapping the opener token. It handles proper lists (a b c), improper
// lists (a b . c), the empty list, and bracket matching. Filling a caller-
// supplied head is what lets readLabeledList pre-register that exact pair so a
// #n# inside the list resolves to it (R7RS §2.4 circular structures) — mirroring
// readVectorInto for vectors. The head's SourceContext (the opener position) is
// preserved as the returned list's provenance.
func (p *Parser) readListInto(head *syntax.SyntaxPair, opener tokenizer.TokenizerState) (syntax.SyntaxValue, tokenizer.Token, error) {
	expectedClose := p.matchingClose(opener)
	// The opening delimiter is the only token still in hand once the input ends
	// mid-form — p.cur is nil at EOF — and it is the position worth reporting.
	openTok := p.cur
	var pr syntax.SyntaxValue = head
	err := p.advance()
	if err != nil {
		return nil, p.cur, wrapMidParseEOF(err, openTok, "list")
	}
	q0 := pr
	pr0 := p.wrapSyntaxPair(nil, nil, p.cur)
	// gotCar records whether the element loop read at least one datum, so the
	// dotted-pair branch below can reject a '.' in head position (a . b needs a
	// car) instead of mis-threading an orphan pair into (#<void>).
	gotCar := false
	for !p.isListCloser(p.cur.Type()) && p.cur.Type() != tokenizer.TokenizerStateCons {
		var v syntax.SyntaxValue
		v, _, err = p.readSyntax()
		// Comment-skipping may have landed on a close delimiter → no datum; exit
		// the loop and let the post-loop check validate the closer. The sentinel
		// is a control signal, so it never enters the sticky p.err.
		if errors.Is(err, errNoDatum) {
			break
		}
		if err != nil {
			return nil, p.cur, wrapMidParseEOF(err, openTok, "list")
		}
		pr0 = pr.(*syntax.SyntaxPair)
		pr0.SetCar(v)
		gotCar = true
		pr0.SetCdr(p.wrapSyntaxPair(nil, nil, p.cur))
		pr = pr0.Cdr().(*syntax.SyntaxPair)
		err = p.advance()
		if err != nil {
			return nil, p.cur, wrapMidParseEOF(err, openTok, "list")
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
		err = p.advance()
		if err != nil {
			return nil, p.cur, wrapMidParseEOF(err, openTok, "list")
		}
		// read cdr value in improper list
		var v syntax.SyntaxValue
		v, _, err = p.readSyntax()
		// R7RS §7.1.2: the dot must be followed by a datum; stopping on a close
		// delimiter is a located error, not a SetCdr(nil) panic.
		if errors.Is(err, errNoDatum) {
			return nil, p.cur, NewParserErrorWithWrap(werr.ErrNotACons, p.cur,
				"expected datum after '.' in dotted pair")
		}
		if err != nil {
			return nil, p.cur, wrapMidParseEOF(err, openTok, "list")
		}
		pr = v
		err = p.advance()
		if err != nil {
			return nil, p.cur, wrapMidParseEOF(err, openTok, "list")
		}
		// Check for bracket mismatch
		if p.cur.Type() != expectedClose {
			err = p.checkDelimiterMatch(opener)
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
	// See readListInto: the opener is the location to report on a mid-parse EOF.
	openTok := p.cur
	// Advance past #( token
	err := p.advance()
	if err != nil {
		return nil, p.cur, wrapMidParseEOF(err, openTok, "vector")
	}
	// Read vector elements - match the list parsing pattern:
	// check token type BEFORE reading, advance AFTER reading
	for p.cur.Type() != tokenizer.TokenizerStateCloseParen {
		var v syntax.SyntaxValue
		v, _, err = p.readSyntax()
		// No datum means comment-skipping landed on a close delimiter; stop and
		// let the post-loop check validate the closer, mirroring readList. Without
		// this, a non-) closer (e.g. ]) is appended as a nil element and the close
		// is consumed, yielding a malformed vector + EOF error.
		if errors.Is(err, errNoDatum) {
			break
		}
		if err != nil {
			return nil, p.cur, wrapMidParseEOF(err, openTok, "vector")
		}
		q.Values = append(q.Values, v)
		// Advance to next element (or close paren)
		err = p.advance()
		if err != nil {
			return nil, p.cur, wrapMidParseEOF(err, openTok, "vector")
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
	// See readListInto: the opener is the location to report on a mid-parse EOF.
	openTok := p.cur
	q0 := values.NewByteVector()
	err := p.advance()
	if err != nil {
		return nil, p.cur, wrapMidParseEOF(err, openTok, "bytevector")
	}
	var stx syntax.SyntaxValue
	stx, _, err = p.readSyntax()
	if err != nil && !errors.Is(err, errNoDatum) {
		return nil, p.cur, wrapMidParseEOF(err, openTok, "bytevector")
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
		err = p.advance()
		if err != nil {
			return nil, p.cur, wrapMidParseEOF(err, openTok, "bytevector")
		}
		stx, _, err = p.readSyntax()
		if err != nil && !errors.Is(err, errNoDatum) {
			return nil, p.cur, wrapMidParseEOF(err, openTok, "bytevector")
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
		i, err := strconv.ParseInt(s, 16, 32)
		if err != nil {
			return nil, p.cur, NewParserErrorWithWrapf(err, p.cur, "invalid character hex escape: %s", s)
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
		return nil, p.cur, NewParserErrorWithWrapf(werr.ErrParseDepthExceeded, p.cur,
			"nesting depth exceeds maximum of %d", p.maxDepth)
	}

	var q syntax.SyntaxValue
	var err error

	// Skip comments when skipComment is enabled
	// This handles comments inside lists, vectors, and other compound structures
	for p.skipComment {
		cur := p.curr()
		switch cur.Type() {
		case tokenizer.TokenizerStateLineCommentBody, tokenizer.TokenizerStateBlockCommentBody:
			// Skip the comment and advance to next token
			err = p.advance()
			if err != nil {
				return nil, p.cur, err
			}
			continue
		case tokenizer.TokenizerStateDatumCommentBegin:
			// Skip the datum comment begin token
			err = p.advance()
			if err != nil {
				return nil, p.cur, err
			}
			// Read and discard the commented datum.
			_, _, cerr := p.readSyntax()
			// errNoDatum (a #; before a close delimiter) is tolerated: R7RS
			// §7.1.1 requires a datum after #;, and refusing one here would make
			// "(1 #;)" a read error, which this reader deliberately does not do.
			//
			// Tolerating it must NOT advance, though. readSyntax consumed
			// nothing and p.cur is the close delimiter itself, so advancing
			// would hand the enclosing reader the token after it and the closer
			// would never reach checkDelimiterMatch — which is how "[1 #;) 2]"
			// read as (1 2) where the control "[1 2)" correctly errors. Leaving
			// p.cur on the closer keeps the leniency and restores the check.
			if errors.Is(cerr, errNoDatum) {
				continue
			}
			if cerr != nil {
				return nil, p.cur, cerr
			}
			// Advance past the commented datum.
			err = p.advance()
			if err != nil {
				return nil, p.cur, err
			}
			continue
		case tokenizer.TokenizerStateDirective:
			// Process fold-case directives even when skipping
			d := p.wrapSyntaxDirective(schemeutil.TrimPrefixCI(p.cur.String(), "#!"), p.cur)
			p.processFoldCaseDirective(d)
			err = p.advance()
			if err != nil {
				return nil, p.cur, err
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
	case tokenizer.TokenizerStateBoxBegin:
		return p.readBox()
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
		q1, err = p.parseRational(replaceHashDigits(p.cur.String()))
		if err != nil {
			return nil, p.cur, err
		}
		if p.cur.HasHashDigit() {
			q = p.wrapSyntax(MakeInexactNumber(q1), p.cur)
		} else {
			q = p.wrapSyntax(q1, p.cur)
		}
		return q, p.cur, nil
	case tokenizer.TokenizerStateMarkerBase2:
		return p.parseBaseWithExactness(2)
	case tokenizer.TokenizerStateMarkerBase8:
		return p.parseBaseWithExactness(8)
	case tokenizer.TokenizerStateMarkerBase10:
		// #d is the default (decimal) radix, so the following datum reads normally.
		markerTok := p.curr()
		err = p.advance()
		if err != nil {
			// "#d" alone: the radix prefix requires a number, so EOF is malformed
			// input — mirroring the errNoDatum guard below.
			return nil, p.cur, wrapMidParseEOF(err, markerTok, "decimal number marker")
		}
		var tok tokenizer.Token
		q, tok, err = p.readSyntax()
		// A radix marker with no number (e.g. "#d)") must be a located error, not a
		// leaked errNoDatum that a compound reader silently treats as "no datum" —
		// mirroring readExactnessMarker (#e/#i) and the #b/#o/#x arms, which all
		// reject a missing number. Without this guard "(#d)" mis-parsed to "()".
		if errors.Is(err, errNoDatum) {
			return nil, tok, NewParserErrorWithWrapf(werr.ErrNotANumber, tok,
				"decimal number marker requires a datum")
		}
		return q, tok, err
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
		q1, err = p.parseImaginary(replaceHashDigits(p.cur.String()))
		if err != nil {
			return nil, p.cur, err
		}
		if p.cur.HasHashDigit() {
			q1 = MakeInexactNumber(q1)
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
		q1, err = p.parseComplex(replaceHashDigits(p.cur.String()))
		if err != nil {
			return nil, p.cur, err
		}
		if p.cur.HasHashDigit() {
			q1 = MakeInexactNumber(q1)
		}
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateUnsignedComplexPolar, tokenizer.TokenizerStateSignedComplexPolar:
		var q1 *values.Complex
		q1, err = p.parsePolarComplex(replaceHashDigits(p.cur.String()))
		if err != nil {
			return nil, p.cur, err
		}
		q = p.wrapSyntax(q1, p.cur)
		return q, p.cur, nil
	case tokenizer.TokenizerStateMarkerNumberExact:
		return p.readNumericIntroducer("exact number marker", p.makeExactLiteral)
	case tokenizer.TokenizerStateMarkerNumberInexact:
		return p.readNumericIntroducer("inexact number marker", p.makeInexactLiteral)
	case tokenizer.TokenizerStateBigInteger:
		// A bare "#z" token is the introducer: the tokenizer's readBigNum scans
		// nothing when the next character is '#', so token text longer than the
		// prefix means the digits are attached (#z123) and stay base 10.
		if schemeutil.TrimPrefixCI(p.cur.String(), "#z") != "" {
			return p.parseBigIntegerWithBase(10)
		}
		return p.readNumericIntroducer("big integer marker", p.widenToBigInteger)
	case tokenizer.TokenizerStateBigFloat:
		if schemeutil.TrimPrefixCI(p.cur.String(), "#m") != "" {
			return p.parseBigFloat()
		}
		return p.readNumericIntroducer("big float marker", p.widenToBigFloat)
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
	return q, nil, NewParserErrorWithWrapf(p.unknownTokenCause(), p.cur, "unknown token type: %q", p.cur.String())
}

// unknownTokenCause builds the cause for a token the dispatch does not
// recognize, joining ErrUnknownTokenType with the tokenizer's pending lexical
// error when there is one.
//
// A TokenizerStateFailed token means the scanner already diagnosed the fault
// and stashed it (Tokenizer.Next hands back the token now and the error on the
// following call, which never comes because the parser stops here). Joining
// rather than replacing is deliberate: ErrUnknownTokenType is the proxy signal
// IsIncompleteInput matches on for a truncated token, so dropping it would
// change REPL continuation behaviour. errors.Is traverses both arms, and
// ParserError.Error renders the specific arm — see causeText.
func (p *Parser) unknownTokenCause() error {
	if p.toks == nil {
		return ErrUnknownTokenType
	}
	terr := p.toks.Err()
	// io.EOF is a clean terminator, not a diagnosis; it adds nothing here.
	if terr == nil || errors.Is(terr, io.EOF) {
		return ErrUnknownTokenType
	}
	return errors.Join(ErrUnknownTokenType, terr)
}

// Close closes the parser and releases resources. Every later ReadSyntax
// returns an error wrapping ErrAlreadyClosed, including on a parser that was
// never read from: releasing the tokenizer alone did not close anything, since
// ReadSyntax rebuilds one from the underlying reader whenever there is none.
//
// Close's own return is unchanged: ErrAlreadyClosed if the parser is already
// closed or was never read from (the tokenizer is created lazily on the first
// ReadSyntax), otherwise the tokenizer's Close error.
func (p *Parser) Close() error {
	p.dead = NewParserErrorWithWrapf(ErrAlreadyClosed, nil, "parser is closed")
	if p.toks == nil {
		return NewParserErrorWithWrapf(ErrAlreadyClosed, nil, "parser already closed")
	}
	err := p.toks.Close()
	p.toks = nil
	return err
}
