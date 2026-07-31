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
	"fmt"
	"strings"

	"github.com/aalpar/wile/pkg/internal/tokenizer"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// messageReadFailed heads a read error whose specifics come from the layer
// below (see Parser.locateReaderErr). It names the operation only; the rule
// that was violated, and the position, character, and tokenizer state it was
// violated at, are rendered from the cause by ParserError.Error.
const messageReadFailed = "read failed"

var (
	// ErrUnknownTokenType is returned when the parser encounters an unrecognized token.
	ErrUnknownTokenType = werr.NewStaticError("unknown token type")
	ErrAlreadyClosed    = werr.NewStaticError("parser already closed")

	// errNoDatum is an internal control signal: readSyntax returns it (in the
	// error channel) when it stops on a close delimiter instead of producing a
	// datum. It is the io.EOF-style sentinel that replaces the old literal-nil
	// return — matched by errors.Is, so a compound reader distinguishes "no datum
	// here" from a real read error without relying on a fragile nil value. It is
	// always caught and either turned into a located error or used as a loop exit;
	// it must never escape to a caller of ReadSyntax.
	errNoDatum = werr.NewStaticError("no datum at close delimiter")
)

// ParserError represents an error that occurred during parsing.
//
// The offending token (tok) carries the source position; file is the source
// name (empty for unnamed input such as a REPL stream). Location formats the
// two so the error's provenance survives to the final error chain — see
// REVIEW.md "Error Chain Losslessness".
type ParserError struct {
	err  error
	mess string
	tok  tokenizer.Token
	file string
}

// NewParserError creates a new parser error for the given token.
func NewParserError(tok tokenizer.Token, mess string) *ParserError {
	q := &ParserError{
		mess: mess,
		tok:  tok,
	}
	return q
}

// NewParserErrorf creates a new parser error for the given token.
func NewParserErrorf(tok tokenizer.Token, mess string, vs ...any) *ParserError {
	q := NewParserError(tok, fmt.Sprintf(mess, vs...))
	return q
}

// NewParserErrorWithWrap creates a new parser error wrapping another error.
func NewParserErrorWithWrap(err error, tok tokenizer.Token, mess string) *ParserError {
	q := &ParserError{
		err:  err,
		mess: mess,
		tok:  tok,
	}
	return q
}

// NewParserErrorWithWrapf creates a new parser error wrapping another error.
func NewParserErrorWithWrapf(err error, tok tokenizer.Token, mess string, vs ...any) *ParserError {
	q := NewParserErrorWithWrap(err, tok, fmt.Sprintf(mess, vs...))
	return q
}

// Is implements errors.Is for ParserError. Two parser errors match when they
// carry the same message (the token is positional and is not part of error
// identity — see EqualTo for value-level equality that includes it), so
// errors.Is can distinguish parser failures; a wrapped cause is still reached
// through Unwrap.
func (p *ParserError) Is(err error) bool {
	other, ok := err.(*ParserError)
	if !ok {
		return false
	}
	return p.mess == other.mess
}

// Error renders the message and, when the failure came from a lower layer, the
// cause's own text after it.
//
// Rendering the cause is not cosmetic. locateReaderErr's fallback message is a
// fixed generic phrase; the entire diagnostic — which lexical rule was
// violated, at which index, on which character, in which tokenizer state —
// lives on the cause. Returning only mess left that reachable through
// errors.Unwrap but absent from every rendering, which is exactly the
// text-chain/unwrap-chain divergence REVIEW.md forbids.
//
// A cause the message already contains is not repeated: a wrap that only adds a
// sentinel identity must not render as "x: x".
func (p *ParserError) Error() string {
	q := p.mess
	causeStr := p.causeText()
	// Containment, not equality: the message typically embeds the sentinel's
	// text plus the offending input, so ErrUnknownTokenType ("unknown token
	// type") under the message `unknown token type: "abc"` is a restatement
	// even though the two strings differ.
	switch {
	case causeStr == "" || strings.Contains(p.mess, causeStr):
	case p.mess == "":
		// An empty message would render as ": cause"; the cause is the whole
		// diagnostic in that case.
		q = causeStr
	default:
		q = fmt.Sprintf("%s: %s", q, causeStr)
	}
	return q + p.contextSuffix()
}

// contextSuffix renders the offending token's position, text, and type, so a
// parser-level failure is as locatable as a lexical one.
//
// A parser error faults on a token rather than on a rune, so the token is the
// analogue of the tokenizer's offending character: its start index is where the
// failure is, its text is what failed, and its type is the state the reader was
// in. It returns "" when a located tokenizer error is already in the chain,
// because causeText has then rendered a more precise position — the exact rune,
// not the token containing it — and repeating a coarser one would only mislead.
func (p *ParserError) contextSuffix() string {
	var terr *tokenizer.TokenizerError
	if errors.As(p.err, &terr) {
		_, located := terr.At()
		if located {
			return ""
		}
	}
	if p.tok == nil {
		return ""
	}
	// Line is 1-based, so 0 means the token carries no position. The tokenizer
	// seeds line 1 and never emits such a token, so this only fires for a
	// hand-built one in a test — but rendering "line 0" would read as a real
	// location, and a diagnostic that invents a position is worse than one that
	// omits it.
	start := p.tok.Start()
	if start.Line() == 0 {
		return ""
	}
	return fmt.Sprintf(" (at index %d, line %d, column %d, token %q, tokenizer state %s)",
		start.Index(), start.Line(), start.Column(), p.tok.String(), p.tok.Type())
}

// causeText renders the most specific cause in the chain.
//
// A located tokenizer error wins over anything wrapped around it: it is the
// layer that actually detected the fault and the only one that knows the index,
// character, and lexical state. This mirrors SourcedError's innermost-location
// rule — the deepest layer that can name the failure is the most useful one.
// It also keeps the rendering single-line when the cause is an errors.Join,
// whose own Error() is newline-separated.
func (p *ParserError) causeText() string {
	if p.err == nil {
		return ""
	}
	var terr *tokenizer.TokenizerError
	if errors.As(p.err, &terr) {
		return terr.Error()
	}
	return p.err.Error()
}

func (p *ParserError) Unwrap() error {
	return p.err
}

// Location returns the offending token's position as "file:line:col", or
// "line:col" when the input is unnamed (e.g. a REPL stream, where the parser
// has no file name). It returns "" when the error has no located token.
//
// Unlike SourceContext.Location, the unnamed-input form omits the leading colon
// ("1:0" rather than ":1:0"); line is 1-based and column 0-based, as in
// SourceContext.Location.
func (p *ParserError) Location() string {
	start, ok := p.startIndexes()
	if !ok {
		return ""
	}
	if p.file == "" {
		return fmt.Sprintf("%d:%d", start.Line(), start.Column())
	}
	return fmt.Sprintf("%s:%d:%d", p.file, start.Line(), start.Column())
}

// startIndexes resolves the error's position, preferring the offending token
// and falling back to a tokenizer error in the cause chain.
//
// The fallback is load-bearing, not defensive. Tokenizer.Next reports a token
// and the error that ended it on *separate* calls, so a lexical fault surfaces
// to the parser one advance after the token that provoked it — by which point
// p.cur is nil and the token-carried position is gone. Before the fallback,
// every error raised through that path (a bad radix digit, an unterminated
// construct) reached the user with no file:line:col at all.
func (p *ParserError) startIndexes() (syntax.SourceIndexes, bool) {
	if p.tok != nil {
		return p.tok.Start(), true
	}
	var terr *tokenizer.TokenizerError
	if errors.As(p.err, &terr) {
		return terr.At()
	}
	return syntax.SourceIndexes{}, false
}

// SchemeString renders the error as a Scheme value. It goes through Error() so
// the cause's diagnostic reaches this surface too — a Scheme program that
// displays a read error must not see less than a Go caller does.
//
// Only the location is added, and only because it is the one fact Error() does
// not carry: Error() reports index/line/column but not the file name. The
// offending token is deliberately NOT repeated here — Error() already names it.
func (p *ParserError) SchemeString() string {
	loc := p.Location()
	if loc == "" {
		return fmt.Sprintf("ParserError: %s", p.Error())
	}
	return fmt.Sprintf("ParserError at %s: %s", loc, p.Error())
}

func (p *ParserError) IsVoid() bool {
	return p == nil
}

func (p *ParserError) EqualTo(v values.Value) bool {
	other, ok := v.(*ParserError)
	return ok && p.mess == other.mess && p.tok == other.tok
}
