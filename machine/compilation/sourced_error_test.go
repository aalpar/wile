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

package compilation

import (
	"errors"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/werr"
)

func TestSourcedError(t *testing.T) {
	c := qt.New(t)

	t.Run("with source context", func(t *testing.T) {
		// NewSourceIndexes(index, column, line)
		src := &syntax.SourceContext{
			File:  "test.scm",
			Start: syntax.NewSourceIndexes(10, 3, 5),
		}
		inner := werr.WrapForeignErrorf(werr.ErrNoSuchBinding, "no such binding %q", "foo")
		se := &SourcedError{Source: src, Cause: inner}

		c.Assert(se.Error(), qt.Matches, `test\.scm:5:3: .*no such binding.*`)
		c.Assert(errors.Is(se, werr.ErrNoSuchBinding), qt.IsTrue)
		c.Assert(errors.Unwrap(se), qt.Equals, inner)
	})

	t.Run("with nil source", func(t *testing.T) {
		inner := errors.New("some error")
		se := &SourcedError{Source: nil, Cause: inner}

		c.Assert(se.Error(), qt.Equals, "some error")
		c.Assert(errors.Unwrap(se), qt.Equals, inner)
	})

	t.Run("with empty file", func(t *testing.T) {
		src := &syntax.SourceContext{
			File:  "",
			Start: syntax.NewSourceIndexes(0, 1, 0),
		}
		inner := errors.New("error")
		se := &SourcedError{Source: src, Cause: inner}

		// No file → no prefix
		c.Assert(se.Error(), qt.Equals, "error")
	})

	t.Run("errors.As extraction", func(t *testing.T) {
		// NewSourceIndexes(index, column, line)
		src := &syntax.SourceContext{
			File:  "lib.scm",
			Start: syntax.NewSourceIndexes(20, 5, 10),
		}
		inner := errors.New("inner")
		se := &SourcedError{Source: src, Cause: inner}

		// Wrap further to simulate real error chains
		wrapped := werr.WrapForeignErrorf(se, "compilation failed")

		var extracted *SourcedError
		c.Assert(errors.As(wrapped, &extracted), qt.IsTrue)
		c.Assert(extracted.Source.File, qt.Equals, "lib.scm")
	})
}
