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

package syntaxtest

import (
	"errors"
	"fmt"
	"testing"

	"github.com/aalpar/wile/internal/syntax"

	qt "github.com/frankban/quicktest"
)

func TestSyntaxEquals(t *testing.T) {
	checker := SyntaxEquals
	qt.Assert(t, checker.ArgNames(), qt.DeepEquals, []string{"got", "want"})

	sctx1 := syntax.NewSourceContext("test", "file.scm", syntax.NewSourceIndexes(0, 0, 1), syntax.NewSourceIndexes(5, 5, 1))
	sctx2 := syntax.NewSourceContext("test", "file.scm", syntax.NewSourceIndexes(0, 0, 1), syntax.NewSourceIndexes(5, 5, 1))
	sctx3 := syntax.NewSourceContext("other", "file.scm", syntax.NewSourceIndexes(0, 0, 1), syntax.NewSourceIndexes(5, 5, 1))

	sym1 := syntax.NewSyntaxSymbol("foo", sctx1)
	sym2 := syntax.NewSyntaxSymbol("foo", sctx2)
	sym3 := syntax.NewSyntaxSymbol("foo", sctx3)

	err := checker.Check(sym1, []any{sym2}, nil)
	qt.Assert(t, err, qt.IsNil)

	err = checker.Check(sym1, []any{sym3}, nil)
	qt.Assert(t, err, qt.IsNotNil)
}

func TestSyntaxEqualsEdgeCases(t *testing.T) {
	c := qt.New(t)
	checker := SyntaxEquals

	sctx := syntax.NewSourceContext("test", "file.scm", syntax.NewSourceIndexes(0, 0, 1), syntax.NewSourceIndexes(5, 5, 1))

	t.Run("panic recovery on nil args", func(t *testing.T) {
		err := checker.Check(syntax.NewSyntaxSymbol("x", sctx), nil, nil)
		c.Assert(err, qt.IsNotNil)
	})

	t.Run("got is error want is error different types", func(t *testing.T) {
		gotErr := fmt.Errorf("wrapped: %w", errors.New("inner"))
		wantErr := errors.New("plain")
		var noteKeys []string
		note := func(key string, value any) {
			noteKeys = append(noteKeys, key)
		}
		err := checker.Check(gotErr, []any{wantErr}, note)
		c.Assert(err, qt.IsNotNil)
		c.Assert(err.Error(), qt.Equals, "values are not equal")
		c.Assert(noteKeys, qt.DeepEquals, []string{"got type", "want type"})
	})

	t.Run("got is error want is error same type", func(t *testing.T) {
		gotErr := errors.New("a")
		wantErr := errors.New("b")
		var noteKeys []string
		note := func(key string, value any) {
			noteKeys = append(noteKeys, key)
		}
		err := checker.Check(gotErr, []any{wantErr}, note)
		c.Assert(err, qt.IsNotNil)
		c.Assert(err.Error(), qt.Equals, "values are not equal")
		c.Assert(len(noteKeys), qt.Equals, 0)
	})

	t.Run("got is error want is non-error", func(t *testing.T) {
		err := checker.Check(errors.New("oops"), []any{syntax.NewSyntaxSymbol("x", sctx)}, nil)
		c.Assert(err, qt.IsNotNil)
		c.Assert(err.Error(), qt.Equals, "values are not equal")
	})

	t.Run("non-SyntaxValue arguments", func(t *testing.T) {
		err := checker.Check("not syntax", []any{"also not"}, nil)
		c.Assert(err, qt.IsNotNil)
		c.Assert(err.Error(), qt.Equals, "got and want must be of type Datum")
	})

	t.Run("same context different values", func(t *testing.T) {
		sym1 := syntax.NewSyntaxSymbol("foo", sctx)
		sym2 := syntax.NewSyntaxSymbol("bar", sctx)
		err := checker.Check(sym1, []any{sym2}, nil)
		c.Assert(err, qt.IsNotNil)
		c.Assert(err.Error(), qt.Equals, "values are not equal")
	})
}
