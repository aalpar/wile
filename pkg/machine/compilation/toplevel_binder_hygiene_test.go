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
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// sym builds a syntax symbol carrying the given scopes.
func sym(key string, scopes ...*syntax.Scope) *syntax.SyntaxSymbol {
	q := syntax.NewSyntaxSymbol(key, nil)
	for _, s := range scopes {
		q = q.AddScope(s).(*syntax.SyntaxSymbol)
	}
	return q
}

// lst builds a proper syntax list from the elements.
func lst(elems ...syntax.SyntaxValue) syntax.SyntaxValue {
	return syntax.SyntaxList(nil, elems...)
}

// vec builds a syntax vector from the elements.
func vec(elems ...syntax.SyntaxValue) syntax.SyntaxValue {
	return syntax.NewSyntaxVector(nil, elems...)
}

// symbolKeys returns every symbol key in the form, in traversal order —
// including symbols in quoted / vector positions, so a test can assert that
// those are NOT rewritten.
func symbolKeys(stx syntax.SyntaxValue) []string {
	var keys []string
	var walk func(v syntax.SyntaxValue)
	walk = func(v syntax.SyntaxValue) {
		switch s := v.(type) {
		case *syntax.SyntaxSymbol:
			keys = append(keys, s.Key())
		case *syntax.SyntaxPair:
			if syntax.IsSyntaxEmptyList(s) {
				return
			}
			walk(s.SyntaxCar())
			walk(s.SyntaxCdr())
		case *syntax.SyntaxVector:
			for _, e := range s.Values {
				walk(e)
			}
		}
	}
	walk(stx)
	return keys
}

func countKey(keys []string, key string) int {
	q := 0
	for _, k := range keys {
		if k == key {
			q++
		}
	}
	return q
}

// TestRenameMacroIntroducedTopLevelBinders exercises the pass directly: a
// macro-introduced (scoped) top-level define binder and its references are
// renamed to one fresh name; user (empty-scope) binders are untouched; a
// reference carrying extra binding-form scopes resolves to its binder by the
// maximal subset rule.
func TestRenameMacroIntroducedTopLevelBinders(t *testing.T) {
	one := syntax.NewSyntaxObject(values.NewInteger(1), nil)

	t.Run("scoped binder and refs renamed together, user binder untouched", func(t *testing.T) {
		s1 := syntax.NewScopeWithLabel("intro")
		form := lst(
			sym("begin"),
			lst(sym("define"), sym("tmp", s1), one),
			lst(sym("define"), sym("x"), sym("tmp", s1)),
			lst(sym("define"), sym("y"), sym("tmp", s1)),
		)
		got := renameMacroIntroducedTopLevelBinders(form)
		keys := symbolKeys(got)

		qt.Assert(t, countKey(keys, "tmp"), qt.Equals, 0,
			qt.Commentf("original macro-introduced name must be gone: %v", keys))
		qt.Assert(t, countKey(keys, "x"), qt.Equals, 1, qt.Commentf("%v", keys))
		qt.Assert(t, countKey(keys, "y"), qt.Equals, 1, qt.Commentf("%v", keys))

		var fresh string
		for _, k := range keys {
			if strings.HasPrefix(k, "tmp.") {
				fresh = k
				break
			}
		}
		qt.Assert(t, fresh, qt.Not(qt.Equals), "",
			qt.Commentf("expected a fresh tmp.<n> name in %v", keys))
		qt.Assert(t, countKey(keys, fresh), qt.Equals, 3,
			qt.Commentf("binder + two refs share one fresh name: %v", keys))
	})

	t.Run("empty-scope top-level binder is left alone", func(t *testing.T) {
		form := lst(sym("define"), sym("foo"), one)
		got := renameMacroIntroducedTopLevelBinders(form)
		// No macro-introduced binders → identity (same value returned).
		qt.Assert(t, got, qt.Equals, form)
	})

	t.Run("maximal subset resolution across nested same-name binders", func(t *testing.T) {
		s1 := syntax.NewScopeWithLabel("intro")
		s2 := syntax.NewScopeWithLabel("intro")
		// Two macro-introduced binders named tmp: one {s1}, one {s1,s2}. A
		// reference {s1,s2} must resolve to the more-specific {s1,s2} binder.
		form := lst(
			sym("begin"),
			lst(sym("define"), sym("tmp", s1), one),
			lst(sym("define"), sym("tmp", s1, s2), one),
			lst(sym("define"), sym("r"), sym("tmp", s1, s2)),
		)
		got := renameMacroIntroducedTopLevelBinders(form)
		keys := symbolKeys(got)

		qt.Assert(t, countKey(keys, "tmp"), qt.Equals, 0, qt.Commentf("%v", keys))

		// Collect the distinct fresh names in order of appearance.
		var freshNames []string
		for _, k := range keys {
			if strings.HasPrefix(k, "tmp.") && countKey(freshNames, k) == 0 {
				freshNames = append(freshNames, k)
			}
		}
		qt.Assert(t, len(freshNames), qt.Equals, 2,
			qt.Commentf("two distinct binders → two fresh names: %v", keys))

		// The {s1,s2} binder occurrence and the {s1,s2} reference share a name;
		// the lone {s1} binder has the other. So one fresh name appears twice
		// (maximal binder + its ref) and the other once.
		twice := 0
		once := 0
		for _, f := range freshNames {
			switch countKey(keys, f) {
			case 2:
				twice++
			case 1:
				once++
			}
		}
		qt.Assert(t, twice, qt.Equals, 1,
			qt.Commentf("the maximal binder and its reference share a name: %v", keys))
		qt.Assert(t, once, qt.Equals, 1,
			qt.Commentf("the less-specific binder stands alone: %v", keys))
	})

	t.Run("quoted datum is left literal; a live reference is renamed", func(t *testing.T) {
		s1 := syntax.NewScopeWithLabel("intro")
		// (begin (define tmp 1) (define x (quote tmp)) (define y tmp))
		// The quoted tmp is data (R7RS §4.1.2) and must NOT be rewritten; the
		// binder and the y reference must be.
		form := lst(
			sym("begin"),
			lst(sym("define"), sym("tmp", s1), one),
			lst(sym("define"), sym("x"), lst(sym("quote"), sym("tmp", s1))),
			lst(sym("define"), sym("y"), sym("tmp", s1)),
		)
		got := renameMacroIntroducedTopLevelBinders(form)
		keys := symbolKeys(got)

		qt.Assert(t, countKey(keys, "tmp"), qt.Equals, 1,
			qt.Commentf("the quoted tmp must survive as the literal symbol: %v", keys))
		var fresh string
		for _, k := range keys {
			if strings.HasPrefix(k, "tmp.") {
				fresh = k
				break
			}
		}
		qt.Assert(t, countKey(keys, fresh), qt.Equals, 2,
			qt.Commentf("binder + live reference renamed, quoted one excluded: %v", keys))
	})

	t.Run("quasiquote literal is data, unquote is a reference", func(t *testing.T) {
		s1 := syntax.NewScopeWithLabel("intro")
		// (begin (define tmp 1) (define z `(tmp ,tmp)))
		// The quasiquote-literal tmp stays; the unquoted tmp is evaluated and
		// must be renamed.
		qq := lst(sym("quasiquote"),
			lst(sym("tmp", s1), lst(sym("unquote"), sym("tmp", s1))))
		form := lst(
			sym("begin"),
			lst(sym("define"), sym("tmp", s1), one),
			lst(sym("define"), sym("z"), qq),
		)
		got := renameMacroIntroducedTopLevelBinders(form)
		keys := symbolKeys(got)

		qt.Assert(t, countKey(keys, "tmp"), qt.Equals, 1,
			qt.Commentf("the quasiquote-literal tmp must survive: %v", keys))
		var fresh string
		for _, k := range keys {
			if strings.HasPrefix(k, "tmp.") {
				fresh = k
				break
			}
		}
		qt.Assert(t, countKey(keys, fresh), qt.Equals, 2,
			qt.Commentf("binder + unquoted reference renamed: %v", keys))
	})

	t.Run("literal vector elements are data, never renamed", func(t *testing.T) {
		s1 := syntax.NewScopeWithLabel("intro")
		// (begin (define tmp 1) (define x #(tmp)))
		// #(tmp) is a self-evaluating literal vector; its tmp is the symbol datum.
		form := lst(
			sym("begin"),
			lst(sym("define"), sym("tmp", s1), one),
			lst(sym("define"), sym("x"), vec(sym("tmp", s1))),
		)
		got := renameMacroIntroducedTopLevelBinders(form)
		keys := symbolKeys(got)
		// Only the binder is renamed; the vector's tmp stays literal.
		qt.Assert(t, countKey(keys, "tmp"), qt.Equals, 1,
			qt.Commentf("literal vector element must survive: %v", keys))
	})

	t.Run("vector inside quasiquote: unquote renamed, literal element not", func(t *testing.T) {
		s1 := syntax.NewScopeWithLabel("intro")
		// (begin (define tmp 1) (define z `#(tmp ,tmp)))
		qq := lst(sym("quasiquote"),
			vec(sym("tmp", s1), lst(sym("unquote"), sym("tmp", s1))))
		form := lst(
			sym("begin"),
			lst(sym("define"), sym("tmp", s1), one),
			lst(sym("define"), sym("z"), qq),
		)
		got := renameMacroIntroducedTopLevelBinders(form)
		keys := symbolKeys(got)
		qt.Assert(t, countKey(keys, "tmp"), qt.Equals, 1,
			qt.Commentf("the literal vector element stays: %v", keys))
		var fresh string
		for _, k := range keys {
			if strings.HasPrefix(k, "tmp.") {
				fresh = k
				break
			}
		}
		qt.Assert(t, countKey(keys, fresh), qt.Equals, 2,
			qt.Commentf("binder + unquoted vector element renamed: %v", keys))
	})

	t.Run("quote inside quasiquote is ordinary structure, not a barrier", func(t *testing.T) {
		s1 := syntax.NewScopeWithLabel("intro")
		// (begin (define tmp 1) (define z `(quote tmp ,tmp)))
		// Under quasiquote, quote is data structure: the literal tmp stays, the
		// unquoted tmp is a live reference and is renamed.
		qq := lst(sym("quasiquote"),
			lst(sym("quote"), sym("tmp", s1), lst(sym("unquote"), sym("tmp", s1))))
		form := lst(
			sym("begin"),
			lst(sym("define"), sym("tmp", s1), one),
			lst(sym("define"), sym("z"), qq),
		)
		got := renameMacroIntroducedTopLevelBinders(form)
		keys := symbolKeys(got)
		qt.Assert(t, countKey(keys, "tmp"), qt.Equals, 1,
			qt.Commentf("the literal tmp under quote-in-quasiquote stays: %v", keys))
		var fresh string
		for _, k := range keys {
			if strings.HasPrefix(k, "tmp.") {
				fresh = k
				break
			}
		}
		qt.Assert(t, countKey(keys, fresh), qt.Equals, 2,
			qt.Commentf("binder + unquoted reference renamed: %v", keys))
	})

	t.Run("nested quasiquote: only a fully-unquoted reference is renamed", func(t *testing.T) {
		s1 := syntax.NewScopeWithLabel("intro")
		// (begin (define tmp 1) (define z `(a `(b ,tmp ,,tmp))))
		// ,tmp  is at quasi depth 1 → literal, stays.
		// ,,tmp is at quasi depth 0 → renamed.
		inner := lst(sym("quasiquote"),
			lst(sym("b"),
				lst(sym("unquote"), sym("tmp", s1)),
				lst(sym("unquote"), lst(sym("unquote"), sym("tmp", s1)))))
		qq := lst(sym("quasiquote"), lst(sym("a"), inner))
		form := lst(
			sym("begin"),
			lst(sym("define"), sym("tmp", s1), one),
			lst(sym("define"), sym("z"), qq),
		)
		got := renameMacroIntroducedTopLevelBinders(form)
		keys := symbolKeys(got)
		qt.Assert(t, countKey(keys, "tmp"), qt.Equals, 1,
			qt.Commentf("the singly-unquoted tmp at depth 1 stays literal: %v", keys))
		var fresh string
		for _, k := range keys {
			if strings.HasPrefix(k, "tmp.") {
				fresh = k
				break
			}
		}
		qt.Assert(t, countKey(keys, fresh), qt.Equals, 2,
			qt.Commentf("binder + doubly-unquoted tmp renamed: %v", keys))
	})
}
