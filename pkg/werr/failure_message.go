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

package werr

import (
	"errors"
	"strings"
)

// foreignWrap is the ForeignError family seen through the three things this file
// needs and the package does not export: a wrap's own contextual message, and its
// two nesting slots.
//
// An interface with unexported methods rather than a *ForeignError type
// assertion, because ForeignFileError and ForeignReadError EMBED *ForeignError
// and promote all three — an assertion on the concrete pointer type walks straight
// past both, and a file error is exactly the case that makes this function
// interesting.
type foreignWrap interface {
	error
	ownMessage() string
	sentinelSlot() error
	causeSlot() error
}

func (p *ForeignError) ownMessage() string {
	return p.message
}

// sentinelSlot returns the err slot, named for the role WrapForeignErrorf's doc
// gives it rather than for what it always holds. It is overloaded three ways — a
// category sentinel, the next wrap inward, or an underlying operation's root
// error — and telling those apart is the whole job of FailureMessage.
func (p *ForeignError) sentinelSlot() error {
	return p.err
}

func (p *ForeignError) causeSlot() error {
	return p.cause
}

// FailureMessage renders err's chain with its category sentinels omitted: every
// wrap's own contextual message and every root cause, and nothing that exists
// only for errors.Is to match. It returns "" when the chain is nothing BUT a
// sentinel, which is the one case where a caller has to fall back to Error().
//
// A rendered chain fuses two different things, and only the first is a message in
// R7RS §6.11's sense:
//
//	open-input-file: /nope: open /nope: no such file or directory   the FAILURE
//	file does not exist                                            its CATEGORY
//
// The category is duplicated information — the same fact errors.Is answers
// structurally — and it reads as a non-sequitur appended to a complete sentence.
// Everything else stays. In particular a root cause stays: an OS error arrives in
// the same slot a sentinel would occupy (WrapForeignFileError), so stripping by
// slot rather than by type would delete "no such file or directory", the only
// substantive fact a file error carries.
//
// It is also why this strips only the category and does NOT reduce to the innermost
// wrap. Every wrap message is supposed to name where and what, so at a primitive
// the outer ones are load-bearing: "file-exists?: argument 0" is the operation, not
// a breadcrumb. A caller that also wants to drop traversal breadcrumbs has to cut
// the chain on a boundary it can recognise before calling this — see
// machine.failureChain, which cuts at the compiler's provenance stamp.
//
// Nothing is lost either way. This is a projection for the one accessor R7RS
// specifies; the caller still holds the whole error and renders it as before.
func FailureMessage(err error) string {
	return strings.Join(messageParts(err, nil), ": ")
}

// messageParts appends err's substantive text, outermost first.
//
// The descent cannot go through errors.Unwrap at a ForeignError: it deliberately
// has none — Is and As fan out over both slots instead — so from outside this
// package everything below one is invisible. That is why a compile error presents
// only three links to errors.Unwrap while rendering five layers of text.
//
// Both slots are followed, in rendering order, because the err slot is overloaded:
// it holds a category sentinel, the next wrap inward, or an underlying operation's
// root error, depending on which constructor built the wrap.
func messageParts(err error, acc []string) []string {
	if err == nil || isCategorySentinel(err) {
		return acc
	}
	fw, ok := err.(foreignWrap)
	if !ok {
		inner := errors.Unwrap(err)
		if inner != nil && err.Error() == inner.Error() {
			// A transparent wrapper: it decorates the error with something other
			// than text (compilation.syntaxError carries Scheme values) and forwards
			// the rendering unchanged. Descending is lossless BY the equality just
			// tested, and not descending would leave its cause's category label in.
			return messageParts(inner, acc)
		}
		// Anything else — a generic wrapper that adds its own text, or a leaf such
		// as an OS error. Render it whole. Descending would strip that added text,
		// and over-keeping one redundant category label is the safe direction to
		// err in: the label is duplicated information, the text is not.
		return append(acc, err.Error())
	}
	if fw.ownMessage() != "" {
		acc = append(acc, fw.ownMessage())
	}
	acc = messageParts(fw.sentinelSlot(), acc)
	return messageParts(fw.causeSlot(), acc)
}

// isCategorySentinel reports whether err IS a static sentinel, by direct type
// assertion and not errors.As. As would also match an error that merely has a
// sentinel somewhere beneath it — which is what a wrapped root cause looks like —
// and stripping that would delete the file-error case above. Sentinels are placed
// bare in the slot, so the strict test is also the accurate one.
func isCategorySentinel(err error) bool {
	_, ok := err.(*StaticError)
	return ok
}
