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

package tokenizer

import (
	"errors"
	"fmt"
	"io"
	"strconv"
	"unicode/utf8"

	"github.com/aalpar/wile/pkg/syntax"
)

// TokenizerError represents an error that occurred during tokenization.
//
// Beyond the message it carries the scanner's state at the moment the fault was
// detected: where in the input it stopped, the rune it stopped on, and which
// lexical state it was scanning in. That triple is the whole reason the type
// exists rather than a bare sentinel — the parser sits above a streaming
// scanner and cannot recover any of it after the fact, because it drops the
// tokenizer on error (Parser.locateReaderErr). Stamping at the throw site is
// what makes the facts survive.
//
// Identity is still the message alone (see Is): position is provenance, not
// classification, so errors.Is against a bare NewTokenizerError sentinel keeps
// working.
type TokenizerError struct {
	err   error
	mess  string
	at    syntax.SourceIndexes // position of the offending rune
	r     rune                 // the rune the scanner stopped on
	state TokenizerState       // lexical state being scanned when it stopped
	eof   bool                 // r is the EOF sentinel, not a real character
}

// NewTokenizerError creates an unlocated tokenizer error with the given message.
//
// Unlocated is the point: this constructor builds the comparison sentinels for
// errors.Is. Errors raised while scanning go through Tokenizer.fail, which
// stamps position, rune, and state.
func NewTokenizerError(mess string) *TokenizerError {
	q := &TokenizerError{
		mess: mess,
	}
	return q
}

// NewTokenizerErrorWithWrap creates an unlocated tokenizer error that wraps
// another error. See NewTokenizerError on why unlocated.
func NewTokenizerErrorWithWrap(err error, mess string) *TokenizerError {
	q := &TokenizerError{
		err:  err,
		mess: mess,
	}
	return q
}

// At returns the position of the offending rune and whether the error is
// located at all. Sentinels built by the New* constructors are not.
//
// Located-ness is decided by the line being 1-based, matching
// SourceContext.Location: the zero SourceIndexes has line 0, which no real
// position ever has.
func (p *TokenizerError) At() (syntax.SourceIndexes, bool) {
	return p.at, p.at.Line() > 0
}

// State returns the lexical state the scanner was in when it stopped.
func (p *TokenizerError) State() TokenizerState {
	return p.state
}

// Rune returns the rune the scanner stopped on, and whether it is a real
// character. It is false at end of input, where the scanner reports
// utf8.RuneError as a sentinel rather than as a character — a distinction the
// tokenizer has to preserve because a literal U+FFFD is writable source text
// (see Tokenizer.Next).
func (p *TokenizerError) Rune() (rune, bool) {
	return p.r, !p.eof
}

// describeRune renders the offending character for a human: the character
// itself plus its code point, or an end-of-input marker.
func (p *TokenizerError) describeRune() string {
	if p.eof {
		return "<end of input>"
	}
	// A control or non-printable rune has no useful glyph; the code point alone
	// is the whole signal, and quoting it would emit a raw control byte into
	// the diagnostic.
	if !strconv.IsPrint(p.r) {
		return fmt.Sprintf("U+%04X", p.r)
	}
	return fmt.Sprintf("%q (U+%04X)", p.r, p.r)
}

// Is implements errors.Is for TokenizerError. Two tokenizer errors match
// when they carry the same message, so errors.Is can distinguish the message
// constants (e.g. errors.Is(err, NewTokenizerError(MessageUnterminatedString)));
// a wrapped cause is still reached through Unwrap.
//
// Position, rune, and state are deliberately excluded: they describe where a
// failure happened, not which failure it is, and including them would make
// every sentinel comparison fail.
func (p *TokenizerError) Is(err error) bool {
	other, ok := err.(*TokenizerError)
	if !ok {
		return false
	}
	return p.mess == other.mess
}

// Error renders the message followed by the scanner facts that make it
// actionable: where it stopped, what it stopped on, and the state it was in.
//
// The context is appended rather than prefixed so the message stays the
// leading text under further wrapping, and so an unlocated sentinel renders as
// the bare message it always did.
func (p *TokenizerError) Error() string {
	if p.at.Line() == 0 {
		return p.mess
	}
	return fmt.Sprintf("%s (at index %d, line %d, column %d, character %s, tokenizer state %s)",
		p.mess, p.at.Index(), p.at.Line(), p.at.Column(), p.describeRune(), p.state)
}

func (p *TokenizerError) Unwrap() error {
	return p.err
}

// fail records mess as the tokenizer's error, stamped with the scanner's
// position, the rune it stopped on, and the state it was scanning in.
//
// Every lexical fault raises through here (or failWrap) so the stamp cannot be
// forgotten at a call site. runeStart, not runeEnd, is the position: it points
// AT the offending rune rather than past it, which is what a caret would mark.
func (p *Tokenizer) fail(mess string) {
	p.err = p.stampError(nil, mess)
}

// failWrap is fail for a fault that has a cause worth keeping — typically the
// io.EOF or rune error that interrupted a longer construct. Call it with the
// incoming p.err as cause; the stamp is read before p.err is overwritten.
func (p *Tokenizer) failWrap(err error, mess string) {
	p.err = p.stampError(err, mess)
}

// stampError builds the located error. It reads p.err (the error being
// replaced) only to decide whether the recorded rune is a real character:
// readNextRune reports end of input as (utf8.RuneError, io.EOF), and a literal
// U+FFFD in the source is indistinguishable from it by rune alone.
func (p *Tokenizer) stampError(cause error, mess string) *TokenizerError {
	q := &TokenizerError{
		err:   cause,
		mess:  mess,
		at:    p.runeStart,
		r:     p.cur,
		state: p.state,
		eof:   p.cur == utf8.RuneError && errors.Is(p.err, io.EOF),
	}
	return q
}
