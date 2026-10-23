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

func TestFlipScopeStampOrigin(t *testing.T) {
	origin := &OriginInfo{Identifier: "m"}
	intro := NewScope()
	originOf := func(v values.Value) *OriginInfo {
		return v.(SyntaxValue).SourceContext().Origin
	}
	sym := func(s string) *SyntaxSymbol {
		return NewSyntaxSymbol(s, &SourceContext{Text: s})
	}
	// inputSym is a symbol that came in through the input: the expander put the
	// introduction scope on it.
	inputSym := func(s string) *SyntaxSymbol {
		return NewSyntaxSymbol(s, &SourceContext{Text: s}).AddScope(intro).(*SyntaxSymbol)
	}
	hasIntro := func(v values.Value) bool {
		return v.(*SyntaxSymbol).SourceContext().Scopes.Has(intro)
	}

	t.Run("stamps and flips every node the transformer built", func(t *testing.T) {
		c := qt.New(t)
		lit := NewSyntaxObject(values.NewInteger(1), &SourceContext{})
		vec := NewSyntaxVector(&SourceContext{}, sym("a"), lit)
		box := NewSyntaxBox(sym("b"), &SourceContext{})
		list := SyntaxList(&SourceContext{}, sym("x"), vec, box, lit)

		out := FlipScopeStampOrigin(list, intro, origin, NewPassThrough(nil))

		pair := out.(*SyntaxPair)
		c.Assert(originOf(pair), qt.Equals, origin)
		c.Assert(originOf(pair.Car()), qt.Equals, origin)
		c.Assert(hasIntro(pair.Car()), qt.IsTrue)
		rest := pair.Cdr().(*SyntaxPair)
		v := rest.Car().(*SyntaxVector)
		c.Assert(originOf(v), qt.Equals, origin)
		c.Assert(originOf(v.Values[0]), qt.Equals, origin)
		c.Assert(hasIntro(v.Values[0]), qt.IsTrue)
		c.Assert(originOf(v.Values[1]), qt.Equals, origin)
		b := rest.Cdr().(*SyntaxPair).Car().(*SyntaxBox)
		c.Assert(originOf(b), qt.Equals, origin)
		c.Assert(originOf(b.Value), qt.Equals, origin)
		// FlipScope does not descend into a box, and neither does the fused walk.
		c.Assert(hasIntro(b.Value), qt.IsFalse)
		c.Assert(originOf(list), qt.IsNil, qt.Commentf("input is not mutated"))
	})

	t.Run("nodes of the input pass through unstamped", func(t *testing.T) {
		c := qt.New(t)
		user := SyntaxList(&SourceContext{}, inputSym("let"), inputSym("z"))
		input := SyntaxList(&SourceContext{}, inputSym("m"), user)
		built := SyntaxList(&SourceContext{}, sym("begin"), user)

		out := FlipScopeStampOrigin(built, intro, origin, NewPassThrough(input)).(*SyntaxPair)

		c.Assert(originOf(out), qt.Equals, origin)
		got := out.Cdr().(*SyntaxPair).Car().(*SyntaxPair)
		c.Assert(originOf(got), qt.IsNil)
		// The pass-through subtree is flipped: the introduction scope comes off.
		c.Assert(hasIntro(got.Car()), qt.IsFalse)
		c.Assert(originOf(got.Car()), qt.IsNil)
	})

	t.Run("an input symbol is the user's, a built one carrying the scope is not", func(t *testing.T) {
		c := qt.New(t)
		u := inputSym("u")
		// built carries the scope, as a symbol made in the keyword's context
		// does, but it is not an object of the input.
		built := inputSym("v")
		input := SyntaxList(&SourceContext{}, inputSym("m"), u)
		out := FlipScopeStampOrigin(SyntaxList(&SourceContext{}, u, built, sym("t")), intro, origin, NewPassThrough(input)).(*SyntaxPair)
		c.Assert(originOf(out.Car()), qt.IsNil)
		c.Assert(hasIntro(out.Car()), qt.IsFalse)
		rest := out.Cdr().(*SyntaxPair)
		c.Assert(originOf(rest.Car()), qt.Equals, origin)
		c.Assert(originOf(rest.Cdr().(*SyntaxPair).Car()), qt.Equals, origin)
	})

	t.Run("elements of an input vector and contents of an input box pass through", func(t *testing.T) {
		c := qt.New(t)
		user := SyntaxList(&SourceContext{}, inputSym("let"), inputSym("z"))
		// One container per input, so each is the only way user reaches the set.
		for _, container := range []SyntaxValue{
			NewSyntaxVector(&SourceContext{}, user),
			NewSyntaxBox(user, &SourceContext{}),
		} {
			input := SyntaxList(&SourceContext{}, inputSym("m"), container)
			for _, built := range []SyntaxValue{
				SyntaxList(&SourceContext{}, sym("begin"), user),
				NewSyntaxBox(user, &SourceContext{}),
			} {
				out := FlipScopeStampOrigin(built, intro, origin, NewPassThrough(input))
				var got values.Value
				pair, ok := out.(*SyntaxPair)
				if ok {
					got = pair.Cdr().(*SyntaxPair).Car()
				} else {
					got = out.(*SyntaxBox).Value
				}
				c.Assert(originOf(got), qt.IsNil)
			}
		}
	})

	t.Run("an input vector and an input box returned whole pass through", func(t *testing.T) {
		c := qt.New(t)
		vec := NewSyntaxVector(&SourceContext{}, inputSym("a"))
		box := NewSyntaxBox(inputSym("b"), &SourceContext{})
		input := SyntaxList(&SourceContext{}, inputSym("m"), vec, box)
		out := FlipScopeStampOrigin(SyntaxList(&SourceContext{}, sym("begin"), vec, box), intro, origin, NewPassThrough(input)).(*SyntaxPair)
		rest := out.Cdr().(*SyntaxPair)
		c.Assert(originOf(rest.Car()), qt.IsNil)
		c.Assert(originOf(rest.Cdr().(*SyntaxPair).Car()), qt.IsNil)
		c.Assert(rest.Cdr().(*SyntaxPair).Car(), qt.Equals, values.Value(box))
	})

	t.Run("a literal of the input passes through", func(t *testing.T) {
		c := qt.New(t)
		lit := NewSyntaxObject(values.NewInteger(7), &SourceContext{})
		input := SyntaxList(&SourceContext{}, inputSym("m"), lit)
		out := FlipScopeStampOrigin(SyntaxList(&SourceContext{}, sym("b"), lit), intro, origin, NewPassThrough(input)).(*SyntaxPair)
		c.Assert(out.Cdr().(*SyntaxPair).Car(), qt.Equals, values.Value(lit))
	})

	t.Run("distinct source contexts stay distinct", func(t *testing.T) {
		c := qt.New(t)
		a := &SourceContext{Text: "a"}
		b := &SourceContext{Text: "b"}
		list := SyntaxList(&SourceContext{Text: "top"},
			NewSyntaxObject(values.NewInteger(1), a),
			NewSyntaxObject(values.NewInteger(2), b),
			NewSyntaxObject(values.NewInteger(3), a))
		out := FlipScopeStampOrigin(list, intro, origin, nil).(*SyntaxPair)
		texts := []string{}
		for cur := out; ; {
			texts = append(texts, cur.Car().(SyntaxValue).SourceContext().Text)
			next, ok := cur.Cdr().(*SyntaxPair)
			if !ok || IsSyntaxEmptyList(next) {
				break
			}
			cur = next
		}
		c.Assert(texts, qt.DeepEquals, []string{"a", "b", "a"})
	})

	t.Run("one clone per source context", func(t *testing.T) {
		c := qt.New(t)
		shared := &SourceContext{Text: "t"}
		out := FlipScopeStampOrigin(SyntaxList(shared, sym("a"), NewSyntaxObject(values.NewInteger(1), shared), NewSyntaxObject(values.NewInteger(2), shared)), intro, origin, NewPassThrough(nil)).(*SyntaxPair)
		rest := out.Cdr().(*SyntaxPair)
		c.Assert(rest.Car().(*SyntaxObject).SourceContext(), qt.Equals, rest.Cdr().(*SyntaxPair).Car().(*SyntaxObject).SourceContext())
	})

	t.Run("a cyclic input terminates", func(t *testing.T) {
		c := qt.New(t)
		loop := SyntaxList(&SourceContext{}, inputSym("a"), inputSym("b")).(*SyntaxPair)
		loop.Cdr().(*SyntaxPair).Values[1] = loop
		set := NewPassThrough(loop)
		c.Assert(set, qt.IsNotNil)
	})

	t.Run("nil origin degrades to FlipScope, nil node to nil", func(t *testing.T) {
		c := qt.New(t)
		s := inputSym("a")
		got := FlipScopeStampOrigin(s, intro, nil, nil)
		c.Assert(hasIntro(got), qt.IsFalse)
		c.Assert(FlipScopeStampOrigin(nil, intro, origin, nil), qt.IsNil)
	})

	t.Run("nil scope stamps without flipping", func(t *testing.T) {
		c := qt.New(t)
		got := FlipScopeStampOrigin(sym("a"), nil, origin, nil)
		c.Assert(originOf(got), qt.Equals, origin)
	})

	t.Run("a large input and a template with many contexts", func(t *testing.T) {
		c := qt.New(t)
		// Past the linear-scan size of the sets and of the context memo.
		var user, built []SyntaxValue
		for i := range 3 * smallSetMax {
			user = append(user, SyntaxList(&SourceContext{}, inputSym("u"+string(rune('a'+i%26)))))
			built = append(built, NewSyntaxObject(values.NewInteger(int64(i)), &SourceContext{Text: "t"}))
		}
		input := SyntaxList(&SourceContext{}, append([]SyntaxValue{inputSym("m")}, user...)...)
		out := FlipScopeStampOrigin(SyntaxList(&SourceContext{}, append(user, built...)...), intro, origin, NewPassThrough(input))
		n := 0
		for cur := out.(*SyntaxPair); ; {
			car := cur.Car().(SyntaxValue)
			if n < len(user) {
				c.Assert(originOf(car), qt.IsNil)
			} else {
				c.Assert(originOf(car), qt.Equals, origin)
			}
			n++
			next, ok := cur.Cdr().(*SyntaxPair)
			if !ok || IsSyntaxEmptyList(next) {
				break
			}
			cur = next
		}
		c.Assert(n, qt.Equals, 6*smallSetMax)
	})

	t.Run("recording a vector or box containing itself terminates", func(t *testing.T) {
		c := qt.New(t)
		vec := NewSyntaxVector(&SourceContext{})
		vec.Values = append(vec.Values, vec)
		box := NewSyntaxBox(nil, &SourceContext{})
		box.Value = box
		for _, cyclic := range []SyntaxValue{vec, box} {
			// Recording the input must visit the cycle once.
			pass := NewPassThrough(SyntaxList(&SourceContext{}, inputSym("m"), cyclic))
			c.Assert(pass.other.has(cyclic), qt.IsTrue)
		}
	})
}
