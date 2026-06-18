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

package valuestest

import (
	"errors"
	"testing"

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

func TestSchemeEquals(t *testing.T) {
	checker := SchemeEquals
	qt.Assert(t, checker.ArgNames(), qt.DeepEquals, []string{"got", "want"})

	a := values.NewInteger(42)
	b := values.NewInteger(42)
	c := values.NewInteger(99)

	err := checker.Check(a, []any{b}, nil)
	qt.Assert(t, err, qt.IsNil)

	err = checker.Check(a, []any{c}, nil)
	qt.Assert(t, err, qt.IsNotNil)
}

func TestSchemeEqualsEdgeCases(t *testing.T) {
	c := qt.New(t)
	checker := SchemeEquals

	t.Run("panic recovery on nil args", func(t *testing.T) {
		err := checker.Check(values.NewInteger(1), nil, nil)
		c.Assert(err, qt.IsNotNil)
	})

	t.Run("got is error want is error different types", func(t *testing.T) {
		gotErr := werr.WrapForeignErrorf(werr.ErrNotANumber, "test")
		wantErr := errors.New("test")
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
		err := checker.Check(errors.New("oops"), []any{values.NewInteger(1)}, nil)
		c.Assert(err, qt.IsNotNil)
		c.Assert(err.Error(), qt.Equals, "values are not equal")
	})

	t.Run("non-Value arguments", func(t *testing.T) {
		err := checker.Check("not a value", []any{"also not"}, nil)
		c.Assert(err, qt.IsNotNil)
		c.Assert(err.Error(), qt.Equals, "got and want must be of type Datum")
	})
}
