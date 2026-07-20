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

package match

import (
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

// --- filterRebindingScopes tests ---

func TestFilterRebindingScopes(t *testing.T) {
	c := qt.New(t)

	c.Run("nil scopes returns nil", func(c *qt.C) {
		result := filterRebindingScopes(nil)
		c.Assert(result, qt.IsNil)
	})

	c.Run("no rebinding scopes returns nil", func(c *qt.C) {
		scopes := []*syntax.Scope{
			syntax.NewScope(),
			syntax.NewScope(),
		}
		result := filterRebindingScopes(scopes)
		c.Assert(result, qt.IsNil)
	})

	c.Run("filters only rebinding scopes", func(c *qt.C) {
		rebind := syntax.NewRebindingScope()
		normal := syntax.NewScope()
		scopes := []*syntax.Scope{normal, rebind}
		result := filterRebindingScopes(scopes)
		c.Assert(len(result), qt.Equals, 1)
		c.Assert(result[0], qt.Equals, rebind)
	})

	c.Run("nil scope in list skipped", func(c *qt.C) {
		rebind := syntax.NewRebindingScope()
		scopes := []*syntax.Scope{nil, rebind}
		result := filterRebindingScopes(scopes)
		c.Assert(len(result), qt.Equals, 1)
		c.Assert(result[0], qt.Equals, rebind)
	})
}

// --- ByteCode String() tests ---

func TestByteCodeString(t *testing.T) {
	c := qt.New(t)

	c.Run("CaptureCdr", func(c *qt.C) {
		bc := ByteCodeCaptureCdr{Binding: "rest"}
		c.Assert(bc.String(), qt.Equals, "CaptureCdr(rest)")
	})

	c.Run("CompareCdr", func(c *qt.C) {
		bc := ByteCodeCompareCdr{Value: syntax.NewSyntaxObject(values.NewInteger(42), nil)}
		c.Assert(bc.String(), qt.Matches, `CompareCdr\(.*\)`)
	})

	c.Run("RequireCarEmpty", func(c *qt.C) {
		bc := ByteCodeRequireCarEmpty{}
		c.Assert(bc.String(), qt.Equals, "RequireCarEmpty")
	})

	c.Run("SkipIfTailCount", func(c *qt.C) {
		bc := ByteCodeSkipIfTailCount{Offset: 5, Count: 2}
		c.Assert(bc.String(), qt.Equals, "SkipIfTailCount(5, count=2)")
	})
}

// --- SyntaxMatcher.GetBindings tests ---

func TestSyntaxMatcher_GetBindings(t *testing.T) {
	c := qt.New(t)

	codes := []SyntaxCommand{
		ByteCodeCaptureCar{Binding: "x"},
		ByteCodeDone{},
	}
	vars := map[string]struct{}{"x": {}}
	m := NewSyntaxMatcher(vars, codes, nil)

	target := testSyntaxList(testSyntaxInt(99))
	err := m.Match(context.Background(), target)
	c.Assert(err, qt.IsNil)

	bindings := m.GetBindings()
	c.Assert(bindings, qt.IsNotNil)
	c.Assert(bindings["x"], qt.IsNotNil)
}

// --- capturedValueToSyntax fallback paths ---

func TestCapturedValueToSyntax_FallbackPaths(t *testing.T) {
	c := qt.New(t)

	sm := &SyntaxMatcher{matcher: &Matcher{}}

	c.Run("values.Symbol wraps to SyntaxSymbol", func(c *qt.C) {
		sym := values.NewSymbol("hello")
		result, err := sm.capturedValueToSyntax(sym, &ExpandOptions{})
		c.Assert(err, qt.IsNil)
		ss, ok := result.(*syntax.SyntaxSymbol)
		c.Assert(ok, qt.IsTrue)
		c.Assert(ss.Key(), qt.Equals, "hello")
	})

	c.Run("values.Pair wraps recursively", func(c *qt.C) {
		pr := values.NewCons(values.NewInteger(1), values.EmptyList)
		result, err := sm.capturedValueToSyntax(pr, &ExpandOptions{})
		c.Assert(err, qt.IsNil)
		sp, ok := result.(*syntax.SyntaxPair)
		c.Assert(ok, qt.IsTrue)
		c.Assert(sp.Length(), qt.Equals, 1)
	})

	c.Run("values.EmptyList wraps to SyntaxEmptyList", func(c *qt.C) {
		result, err := sm.capturedValueToSyntax(values.EmptyList, &ExpandOptions{})
		c.Assert(err, qt.IsNil)
		c.Assert(syntax.IsSyntaxEmptyList(result), qt.IsTrue)
	})

	c.Run("other value wraps to SyntaxObject", func(c *qt.C) {
		str := values.NewString("test")
		result, err := sm.capturedValueToSyntax(str, &ExpandOptions{})
		c.Assert(err, qt.IsNil)
		so, ok := result.(*syntax.SyntaxObject)
		c.Assert(ok, qt.IsTrue)
		c.Assert(so.Unwrap().SchemeString(), qt.Equals, `"test"`)
	})

	c.Run("with origin and useSiteCtx", func(c *qt.C) {
		origin := &syntax.OriginInfo{Identifier: "test-macro"}
		useSite := &syntax.SourceContext{Text: "use"}
		str := values.NewString("val")
		result, err := sm.capturedValueToSyntax(str, &ExpandOptions{UseSiteCtx: useSite, Origin: origin})
		c.Assert(err, qt.IsNil)
		c.Assert(result, qt.IsNotNil)
		c.Assert(result.SourceContext().Origin, qt.Equals, origin)
	})

	c.Run("with origin but no useSiteCtx", func(c *qt.C) {
		origin := &syntax.OriginInfo{Identifier: "test-macro"}
		str := values.NewString("val")
		result, err := sm.capturedValueToSyntax(str, &ExpandOptions{Origin: origin})
		c.Assert(err, qt.IsNil)
		c.Assert(result, qt.IsNotNil)
		c.Assert(result.SourceContext().Origin, qt.Equals, origin)
	})
}

// --- applyHygieneToSymbol tests ---

// mockFreeIdResolver implements FreeIdResolver for testing.
// Set only the fields relevant to each test case; zero values mean "absent."
type mockFreeIdResolver struct {
	localScopes     []*syntax.Scope
	global          *environment.GlobalIndex
	hasLocalBinding bool
	libScope        *syntax.Scope
}

func (p mockFreeIdResolver) GetLocalScopes() []*syntax.Scope {
	return p.localScopes
}

func (p mockFreeIdResolver) GetGlobal() *environment.GlobalIndex {
	return p.global
}

func (p mockFreeIdResolver) GetHasLocalBinding() bool {
	return p.hasLocalBinding
}

func (p mockFreeIdResolver) GetLibraryScope() *syntax.Scope {
	return p.libScope
}

func TestApplyHygieneToSymbol(t *testing.T) {
	c := qt.New(t)

	sm := &SyntaxMatcher{matcher: &Matcher{}}
	introScope := syntax.NewScope()

	c.Run("non-free identifier gets intro scope", func(c *qt.C) {
		sym := syntax.NewSyntaxSymbol("foo", nil)
		result := sm.applyHygieneToSymbol(sym, &ExpandOptions{IntroScope: introScope})
		ss := result.(*syntax.SyntaxSymbol)
		c.Assert(ss.Key(), qt.Equals, "foo")
		scopes := ss.Scopes()
		c.Assert(len(scopes), qt.Equals, 1)
		c.Assert(scopes[0], qt.Equals, introScope)
	})

	c.Run("free identifier with local scopes", func(c *qt.C) {
		defScope := syntax.NewScope()
		sym := syntax.NewSyntaxSymbol("bar", &syntax.SourceContext{Text: "bar"})
		freeIds := map[string]FreeIdResolver{
			FreeIdKey("bar", sym.Scopes()): mockFreeIdResolver{localScopes: []*syntax.Scope{defScope}},
		}
		result := sm.applyHygieneToSymbol(sym, &ExpandOptions{IntroScope: introScope, FreeIds: freeIds})
		ss := result.(*syntax.SyntaxSymbol)
		c.Assert(ss.Key(), qt.Equals, "bar")
		// Should have definition-site scopes, not intro scope
		scopes := ss.Scopes()
		c.Assert(len(scopes), qt.Equals, 1)
		c.Assert(scopes[0], qt.Equals, defScope)
	})

	c.Run("free identifier with global binding", func(c *qt.C) {
		globalIdx := environment.NewGlobalIndex(values.NewSymbol("baz"))
		sym := syntax.NewSyntaxSymbol("baz", &syntax.SourceContext{Text: "baz"})
		freeIds := map[string]FreeIdResolver{
			FreeIdKey("baz", sym.Scopes()): mockFreeIdResolver{global: globalIdx},
		}
		result := sm.applyHygieneToSymbol(sym, &ExpandOptions{IntroScope: introScope, FreeIds: freeIds})
		ss := result.(*syntax.SyntaxSymbol)
		c.Assert(ss.Key(), qt.Equals, "baz")
		c.Assert(ss.ResolvedBinding, qt.Equals, globalIdx)
	})

	c.Run("free identifier with hasLocalBinding true", func(c *qt.C) {
		sym := syntax.NewSyntaxSymbol("qux", nil)
		freeIds := map[string]FreeIdResolver{
			FreeIdKey("qux", sym.Scopes()): mockFreeIdResolver{hasLocalBinding: true},
		}
		result := sm.applyHygieneToSymbol(sym, &ExpandOptions{IntroScope: introScope, FreeIds: freeIds})
		ss := result.(*syntax.SyntaxSymbol)
		c.Assert(ss.Key(), qt.Equals, "qux")
		// Should NOT have intro scope (has local binding)
		c.Assert(len(ss.Scopes()), qt.Equals, 0)
	})

	c.Run("with useSiteCtx overrides source context", func(c *qt.C) {
		useSite := &syntax.SourceContext{Text: "use-site"}
		sym := syntax.NewSyntaxSymbol("foo", &syntax.SourceContext{Text: "template"})
		result := sm.applyHygieneToSymbol(sym, &ExpandOptions{IntroScope: introScope, UseSiteCtx: useSite})
		c.Assert(result, qt.IsNotNil)
	})

	c.Run("with origin info", func(c *qt.C) {
		origin := &syntax.OriginInfo{Identifier: "test"}
		srcCtx := &syntax.SourceContext{Text: "src"}
		sym := syntax.NewSyntaxSymbol("foo", srcCtx)
		result := sm.applyHygieneToSymbol(sym, &ExpandOptions{IntroScope: introScope, Origin: origin})
		ss := result.(*syntax.SyntaxSymbol)
		c.Assert(ss.SourceContext().Origin, qt.Equals, origin)
	})

	c.Run("global binding keeps definition-site scopes and adds intro scope", func(c *qt.C) {
		globalIdx := environment.NewGlobalIndex(values.NewSymbol("baz"))
		existingScope := syntax.NewScope()
		srcCtx := &syntax.SourceContext{
			Text:   "baz",
			Scopes: []*syntax.Scope{existingScope},
		}
		sym := syntax.NewSyntaxSymbol("baz", srcCtx)
		freeIds := map[string]FreeIdResolver{
			FreeIdKey("baz", sym.Scopes()): mockFreeIdResolver{global: globalIdx},
		}
		result := sm.applyHygieneToSymbol(sym, &ExpandOptions{IntroScope: introScope, FreeIds: freeIds})
		ss := result.(*syntax.SyntaxSymbol)
		// The recorded global is kept as a fallback (resolved after scope-set
		// local resolution in CompileSymbol).
		c.Assert(ss.ResolvedBinding, qt.Equals, globalIdx)
		// The definition-site scope is retained — R7RS §4.3 referential
		// transparency: a template identifier names what was visible where the
		// macro was DEFINED, and a binder in that context is keyed on that scope.
		// The intro scope is carried on top, so a binding co-introduced by the
		// same template can shadow the global (R1 fix — see
		// plans/2026-06-15-macro-hygiene-global-shadow-fix), and so a same-named
		// identifier at the use site stays distinct.
		scopes := ss.Scopes()
		c.Assert(len(scopes), qt.Equals, 2)
		c.Assert(scopes, qt.Contains, existingScope)
		c.Assert(scopes, qt.Contains, introScope)
	})

	c.Run("non-free keeps definition-site scopes and adds intro scope", func(c *qt.C) {
		existingScope := syntax.NewScope()
		srcCtx := &syntax.SourceContext{
			Text:   "foo",
			Scopes: []*syntax.Scope{existingScope},
		}
		sym := syntax.NewSyntaxSymbol("foo", srcCtx)
		result := sm.applyHygieneToSymbol(sym, &ExpandOptions{IntroScope: introScope})
		ss := result.(*syntax.SyntaxSymbol)
		// Definition-site scope retained, intro scope added on top.
		scopes := ss.Scopes()
		c.Assert(len(scopes), qt.Equals, 2)
		c.Assert(scopes, qt.Contains, existingScope)
		c.Assert(scopes, qt.Contains, introScope)
	})
}

// --- expandEscapedSyntaxTemplate tests ---

func TestExpandEscapedSyntaxTemplate(t *testing.T) {
	c := qt.New(t)

	c.Run("nil template returns nil", func(c *qt.C) {
		sm := &SyntaxMatcher{matcher: &Matcher{}}
		ctx := &captureContext{bindings: map[string]syntax.SyntaxValue{}}
		result, err := sm.expandEscapedSyntaxTemplate(nil, ctx, nil, &ExpandOptions{})
		c.Assert(err, qt.IsNil)
		c.Assert(result, qt.IsNil)
	})

	c.Run("pattern variable substituted in escaped context", func(c *qt.C) {
		sm := &SyntaxMatcher{matcher: &Matcher{}}
		captured := syntax.NewSyntaxObject(values.NewInteger(42), nil)
		ctx := &captureContext{
			bindings: map[string]syntax.SyntaxValue{"x": captured},
		}
		template := syntax.NewSyntaxSymbol("x", nil)
		result, err := sm.expandEscapedSyntaxTemplate(template, ctx, nil, &ExpandOptions{})
		c.Assert(err, qt.IsNil)
		c.Assert(result, qt.IsNotNil)
	})

	c.Run("non-variable symbol gets hygiene", func(c *qt.C) {
		sm := &SyntaxMatcher{matcher: &Matcher{}}
		ctx := &captureContext{bindings: map[string]syntax.SyntaxValue{}}
		is := syntax.NewScope()
		template := syntax.NewSyntaxSymbol("foo", nil)
		result, err := sm.expandEscapedSyntaxTemplate(template, ctx, nil, &ExpandOptions{IntroScope: is})
		c.Assert(err, qt.IsNil)
		ss := result.(*syntax.SyntaxSymbol)
		scopes := ss.Scopes()
		c.Assert(len(scopes), qt.Equals, 1)
		c.Assert(scopes[0], qt.Equals, is)
	})

	c.Run("pair template recursed in escaped context", func(c *qt.C) {
		sm := &SyntaxMatcher{matcher: &Matcher{}}
		captured := syntax.NewSyntaxObject(values.NewInteger(10), nil)
		ctx := &captureContext{
			bindings: map[string]syntax.SyntaxValue{"a": captured},
		}
		template := testSyntaxList(syntax.NewSyntaxSymbol("a", nil))
		result, err := sm.expandEscapedSyntaxTemplate(template, ctx, nil, &ExpandOptions{})
		c.Assert(err, qt.IsNil)
		sp, ok := result.(*syntax.SyntaxPair)
		c.Assert(ok, qt.IsTrue)
		c.Assert(sp, qt.IsNotNil)
	})

	c.Run("empty list passthrough", func(c *qt.C) {
		sm := &SyntaxMatcher{matcher: &Matcher{}}
		ctx := &captureContext{bindings: map[string]syntax.SyntaxValue{}}
		emptyList := syntax.SyntaxEmptyList
		template := syntax.NewSyntaxCons(emptyList, syntax.SyntaxEmptyList, nil)
		result, err := sm.expandEscapedSyntaxTemplate(template, ctx, nil, &ExpandOptions{})
		c.Assert(err, qt.IsNil)
		c.Assert(result, qt.IsNotNil)
	})

	c.Run("non-syntax value passthrough", func(c *qt.C) {
		sm := &SyntaxMatcher{matcher: &Matcher{}}
		ctx := &captureContext{bindings: map[string]syntax.SyntaxValue{}}
		literal := syntax.NewSyntaxObject(values.NewInteger(99), nil)
		result, err := sm.expandEscapedSyntaxTemplate(literal, ctx, nil, &ExpandOptions{})
		c.Assert(err, qt.IsNil)
		c.Assert(result, qt.Equals, literal)
	})

	c.Run("scope incompatible substitution blocked", func(c *qt.C) {
		sm := &SyntaxMatcher{matcher: &Matcher{}}
		captured := syntax.NewSyntaxObject(values.NewInteger(42), nil)
		ctx := &captureContext{
			bindings: map[string]syntax.SyntaxValue{"x": captured},
		}
		// Template symbol has extra scope that pattern doesn't
		extraScope := syntax.NewScope()
		templateCtx := &syntax.SourceContext{Scopes: []*syntax.Scope{extraScope}}
		template := syntax.NewSyntaxSymbol("x", templateCtx)
		patternVarSyntax := map[string]*syntax.SyntaxSymbol{
			"x": syntax.NewSyntaxSymbol("x", nil), // no scopes
		}
		is := syntax.NewScope()
		result, err := sm.expandEscapedSyntaxTemplate(template, ctx, nil, &ExpandOptions{IntroScope: is, PatternVarSyntax: patternVarSyntax})
		c.Assert(err, qt.IsNil)
		// Should NOT substitute — should apply hygiene instead, which keeps the
		// template identifier's definition-site scope and adds the intro scope.
		ss := result.(*syntax.SyntaxSymbol)
		c.Assert(ss.Key(), qt.Equals, "x")
		scopes := ss.Scopes()
		c.Assert(len(scopes), qt.Equals, 2)
		c.Assert(scopes, qt.Contains, extraScope)
		c.Assert(scopes, qt.Contains, is)
	})
}

// --- MatchSyntax RequireCarEmpty coverage ---

func TestMatchSyntax_RequireCarEmpty(t *testing.T) {
	c := qt.New(t)

	// Bytecode for: (() x) — RequireCarEmpty checks car is () and advances cdr
	// RequireCarEmpty already advances to cdr, so no VisitCdr needed after it
	codes := []SyntaxCommand{
		ByteCodeRequireCarEmpty{},
		ByteCodeCaptureCar{Binding: "x"},
		ByteCodeDone{},
	}
	vars := map[string]struct{}{"x": {}}

	c.Run("empty list at car matches", func(c *qt.C) {
		m := NewMatcher(vars, codes)
		target := testSyntaxList(
			syntax.SyntaxEmptyList,
			testSyntaxInt(42),
		)
		err := m.MatchSyntax(context.Background(), target)
		c.Assert(err, qt.IsNil)
		bindings := m.GetBindings()
		c.Assert(bindings["x"], qt.IsNotNil)
	})

	c.Run("non-empty at car fails", func(c *qt.C) {
		m := NewMatcher(vars, codes)
		target := testSyntaxList(
			testSyntaxList(testSyntaxInt(1)),
			testSyntaxInt(42),
		)
		err := m.MatchSyntax(context.Background(), target)
		c.Assert(err, qt.IsNotNil)
	})
}

// --- MatchSyntax CaptureCdr path ---

func TestMatchSyntax_CaptureCdr(t *testing.T) {
	c := qt.New(t)

	// Bytecode for pattern ((a . rest)) — nested improper list
	// CaptureCdr modifies the stack so Done sees empty cdr
	codes := []SyntaxCommand{
		ByteCodeVisitCar{},
		ByteCodeCaptureCar{Binding: "a"},
		ByteCodeCaptureCdr{Binding: "rest"},
		ByteCodeDone{},
		ByteCodeDone{},
	}
	vars := map[string]struct{}{"a": {}, "rest": {}}

	c.Run("captures car and cdr", func(c *qt.C) {
		m := NewMatcher(vars, codes)
		// Build target: ((10 . 20))
		inner := syntax.NewSyntaxCons(
			testSyntaxInt(10),
			testSyntaxInt(20),
			nil,
		)
		target := testSyntaxList(inner)
		err := m.MatchSyntax(context.Background(), target)
		c.Assert(err, qt.IsNil)
		bindings := m.GetBindings()
		c.Assert(bindings["a"], qt.IsNotNil)
		c.Assert(bindings["rest"], qt.IsNotNil)
	})
}
