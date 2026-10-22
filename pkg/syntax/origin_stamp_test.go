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

package syntax

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
)

func TestStampOrigin(t *testing.T) {
	origin := &OriginInfo{Identifier: "m"}
	originOf := func(v SyntaxValue) *OriginInfo {
		return v.SourceContext().Origin
	}
	sym := func(s string) *SyntaxSymbol {
		return NewSyntaxSymbol(s, &SourceContext{Text: s})
	}

	t.Run("stamps every node kind", func(t *testing.T) {
		c := qt.New(t)
		lit := NewSyntaxObject(values.NewInteger(1), &SourceContext{})
		vec := NewSyntaxVector(&SourceContext{}, sym("a"), lit)
		box := NewSyntaxBox(sym("b"), &SourceContext{})
		list := SyntaxList(&SourceContext{}, sym("x"), vec, box, lit)

		out := StampOrigin(list, origin, nil)

		pair := out.(*SyntaxPair)
		c.Assert(originOf(pair), qt.Equals, origin)
		c.Assert(originOf(pair.Car().(SyntaxValue)), qt.Equals, origin)
		v := pair.Cdr().(*SyntaxPair).Car().(*SyntaxVector)
		c.Assert(originOf(v), qt.Equals, origin)
		c.Assert(originOf(v.Values[0]), qt.Equals, origin)
		c.Assert(originOf(v.Values[1]), qt.Equals, origin)
		b := pair.Cdr().(*SyntaxPair).Cdr().(*SyntaxPair).Car().(*SyntaxBox)
		c.Assert(originOf(b), qt.Equals, origin)
		c.Assert(originOf(b.Value), qt.Equals, origin)
		c.Assert(originOf(list), qt.IsNil, qt.Commentf("input is not mutated"))
	})

	t.Run("nodes of input pass through untouched", func(t *testing.T) {
		c := qt.New(t)
		user := SyntaxList(&SourceContext{}, sym("let"), sym("z"))
		input := SyntaxList(&SourceContext{}, sym("m"), user)
		built := SyntaxList(&SourceContext{}, sym("begin"), user)

		out := StampOrigin(built, origin, input).(*SyntaxPair)

		c.Assert(originOf(out), qt.Equals, origin)
		c.Assert(out.Cdr().(*SyntaxPair).Car(), qt.Equals, user)
		c.Assert(originOf(user), qt.IsNil)
	})

	t.Run("nil origin and nil node", func(t *testing.T) {
		c := qt.New(t)
		s := sym("a")
		c.Assert(StampOrigin(s, nil, nil), qt.Equals, SyntaxValue(s))
		c.Assert(StampOrigin(nil, origin, nil), qt.IsNil)
	})
}
