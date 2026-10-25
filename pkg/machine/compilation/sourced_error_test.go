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
	"fmt"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/internal/validate"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/werr"
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

	t.Run("nested SourcedError shows only innermost location", func(t *testing.T) {
		innerSrc := &syntax.SourceContext{
			File:  "inner.scm",
			Start: syntax.NewSourceIndexes(20, 5, 10),
		}
		outerSrc := &syntax.SourceContext{
			File:  "outer.scm",
			Start: syntax.NewSourceIndexes(0, 1, 1),
		}
		sentinel := werr.WrapForeignErrorf(werr.ErrNoSuchBinding, "no such binding %q", "x")
		inner := &SourcedError{Source: innerSrc, Cause: sentinel}
		middle := werr.WrapForeignErrorf(inner, "compilation failed")
		outer := &SourcedError{Source: outerSrc, Cause: middle}

		// Only the innermost location should appear in the string
		c.Assert(outer.Error(), qt.Matches, `compilation failed: inner\.scm:10:5: .*no such binding.*`)
		// The outer location must NOT appear
		c.Assert(outer.Error(), qt.Not(qt.Matches), `outer\.scm.*`)

		// Programmatic extraction still finds both
		var se *SourcedError
		c.Assert(errors.As(outer, &se), qt.IsTrue)
		c.Assert(se.Source.File, qt.Equals, "outer.scm")
		// Walk to inner
		c.Assert(errors.As(se.Cause, &se), qt.IsTrue)
		c.Assert(se.Source.File, qt.Equals, "inner.scm")

		// errors.Is still reaches the sentinel
		c.Assert(errors.Is(outer, werr.ErrNoSuchBinding), qt.IsTrue)
	})

	t.Run("wrapSourcedError nil error returns nil", func(t *testing.T) {
		src := &syntax.SourceContext{File: "test.scm", Start: syntax.NewSourceIndexes(0, 1, 1)}
		c.Assert(wrapSourcedError(src, nil), qt.IsNil)
	})

	t.Run("wrapSourcedError nil src returns error unchanged", func(t *testing.T) {
		err := errors.New("some error")
		c.Assert(wrapSourcedError(nil, err), qt.Equals, err)
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

// TestSourcedErrorOriginChain: the innermost SourcedError renders the chain of
// the node it was stamped on, once, whether or not that node has a position,
// and truncates at validate.DefaultMaxOriginDepth. Ported from validate's
// ValidationError, which rendered the chain before the compiler carried it.
func TestSourcedErrorOriginChain(t *testing.T) {
	at := func(line int) *syntax.SourceContext {
		return syntax.NewSourceContext("x", "f.scm", syntax.NewSourceIndexes(0, 0, line), syntax.NewSourceIndexes(1, 1, line))
	}
	chain := &syntax.OriginInfo{Identifier: "inner", Location: at(2),
		Parent: &syntax.OriginInfo{Identifier: "mid", Location: at(3),
			Parent: &syntax.OriginInfo{Identifier: "outer", Location: at(4)}}}
	cause := werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "bad in let form")
	wantChain := "\n  expanded from 'inner' at " + at(2).Location() +
		"\n  expanded from 'mid' at " + at(3).Location() +
		"\n  expanded from 'outer' at " + at(4).Location()

	t.Run("position-less node still prints its chain", func(t *testing.T) {
		se := &SourcedError{Source: &syntax.SourceContext{Origin: chain}, Cause: cause}
		qt.Assert(t, se.Error(), qt.Equals, "bad in let form: invalid syntax"+wantChain)
	})
	t.Run("outer links add neither location nor chain", func(t *testing.T) {
		inner := &SourcedError{Source: at(1).WithOrigin(chain), Cause: cause}
		outer := &SourcedError{Source: at(9).WithOrigin(&syntax.OriginInfo{Identifier: "enclosing", Location: at(9)}), Cause: inner}
		qt.Assert(t, outer.Error(), qt.Equals, at(1).Location()+": bad in let form: invalid syntax"+wantChain)
	})
	t.Run("max depth truncates", func(t *testing.T) {
		var deep *syntax.OriginInfo
		for i := validate.DefaultMaxOriginDepth + 2; i > 0; i-- {
			deep = &syntax.OriginInfo{Identifier: fmt.Sprintf("m%d", i), Parent: deep}
		}
		se := &SourcedError{Source: at(1).WithOrigin(deep), Cause: cause}
		text := se.Error()
		qt.Assert(t, strings.Count(text, "expanded from"), qt.Equals, validate.DefaultMaxOriginDepth)
		qt.Assert(t, strings.HasSuffix(text, "\n  ... and 2 more expansion(s)"), qt.IsTrue, qt.Commentf("%s", text))
	})
}
