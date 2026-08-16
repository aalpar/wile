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

package werr_test

import (
	"io/fs"
	"testing"

	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

// transparentWrapper decorates an error with something other than text and
// forwards the rendering unchanged — the shape compilation.syntaxError has, which
// carries the irritants as Scheme values and adds no message of its own.
type transparentWrapper struct {
	cause error
}

func (p *transparentWrapper) Error() string {
	return p.cause.Error()
}

func (p *transparentWrapper) Unwrap() error {
	return p.cause
}

// opaqueWrapper is the contrasting shape: it adds text, so descending through it
// would delete that text.
type opaqueWrapper struct {
	cause error
}

func (p *opaqueWrapper) Error() string {
	return "load: in f.scm: " + p.cause.Error()
}

func (p *opaqueWrapper) Unwrap() error {
	return p.cause
}

// TestFailureMessage pins the projection R7RS §6.11's error-object-message reads:
// the chain's substantive text with its category sentinels omitted.
//
// Each row also names what a wrong rule would produce, because two plausible
// simplifications each break exactly one row here:
//
//   - reducing to the innermost wrap deletes the operation ("read: truncated at
//     byte 4", "/: division error") and, in the tree, the primitive name from the
//     contract validator's message;
//   - asserting *ForeignError instead of an interface misses the two types that
//     EMBED it, and their categories leak back into the text.
func TestFailureMessage(t *testing.T) {
	pathErr := &fs.PathError{Op: "open", Path: "/nope", Err: fs.ErrNotExist}

	tcs := []struct {
		name string
		err  error
		want string
	}{
		{
			name: "a sentinel-terminated wrap keeps its message and drops the category",
			err:  werr.WrapForeignErrorf(werr.ErrNotAPair, "car: expected pair, got 5"),
			want: "car: expected pair, got 5",
		},
		{
			// Both wraps survive. The outer one names the primitive the program
			// called and the inner one the operation that failed inside it; only a
			// rule that reduced to the innermost would drop the former.
			name: "nested wraps render outermost first",
			err: werr.WrapForeignErrorf(
				werr.WrapForeignErrorf(werr.ErrDivisionByZero, "Integer.Divide: division by exact zero"),
				"/: division error"),
			want: "/: division error: Integer.Divide: division by exact zero",
		},
		{
			// The row that rules out stripping by SLOT rather than by type: an OS
			// error arrives where a sentinel would, and it is the only substantive
			// fact the diagnostic carries.
			name: "a root cause in the sentinel slot is not a category",
			err:  werr.WrapForeignFileError(pathErr, "open-input-file", "/nope"),
			want: "open-input-file: /nope: open /nope: file does not exist",
		},
		{
			name: "a root cause in the cause slot survives too",
			err:  werr.WrapForeignErrorWithCause(werr.ErrFileOpen, pathErr, "stat %s", "/nope"),
			want: "stat /nope: open /nope: file does not exist",
		},
		{
			// ForeignReadError EMBEDS *ForeignError and promotes its methods. A
			// concrete *ForeignError assertion walks past it, falls through to
			// Error(), and lets "incomplete input" back in.
			name: "an embedding wrap is still a wrap",
			err:  werr.WrapForeignReadErrorf(werr.ErrIncompleteInput, "read: truncated at byte 4"),
			want: "read: truncated at byte 4",
		},
		{
			// The documented "" case, and the reason callers keep an Error()
			// fallback: there is no message here, only a category.
			name: "a bare sentinel has no message of its own",
			err:  werr.ErrNotAPair,
			want: "",
		},
		{
			name: "an error from outside the project is its own text",
			err:  pathErr,
			want: "open /nope: file does not exist",
		},
		{
			// A wrapper that is not one of this package's is descended through when
			// it adds no text, so the category beneath it still comes off.
			name: "a transparent wrapper does not block the descent",
			err: &transparentWrapper{
				cause: werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "syntax-error: expected a pair: hello, 42"),
			},
			want: "syntax-error: expected a pair: hello, 42",
		},
		{
			// And one that DOES add text is rendered whole, keeping that text at the
			// cost of the category label. Over-keeping is the safe direction: the
			// label is duplicated information, the added text is not.
			name: "a wrapper that adds text is rendered whole",
			err: &opaqueWrapper{
				cause: werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "syntax-error: expected a pair"),
			},
			want: "load: in f.scm: syntax-error: expected a pair: invalid syntax",
		},
		{
			name: "nil is empty",
			err:  nil,
			want: "",
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			c.Assert(werr.FailureMessage(tc.err), qt.Equals, tc.want)
		})
	}
}
