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

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

// --- literalScopesMatch tests ---

func TestLiteralScopesMatch(t *testing.T) {
	c := qt.New(t)

	c.Run("both nil returns false", func(c *qt.C) {
		c.Assert(literalScopesMatch(nil, nil), qt.IsFalse)
	})

	c.Run("input nil returns false", func(c *qt.C) {
		pattern := syntax.NewSyntaxSymbol("x", nil)
		c.Assert(literalScopesMatch(nil, pattern), qt.IsFalse)
	})

	c.Run("pattern nil returns false", func(c *qt.C) {
		input := syntax.NewSyntaxSymbol("x", nil)
		c.Assert(literalScopesMatch(input, nil), qt.IsFalse)
	})

	c.Run("both no scopes matches", func(c *qt.C) {
		input := syntax.NewSyntaxSymbol("x", nil)
		pattern := syntax.NewSyntaxSymbol("x", nil)
		c.Assert(literalScopesMatch(input, pattern), qt.IsTrue)
	})

	c.Run("input has rebinding scope pattern does not", func(c *qt.C) {
		rebindScope := syntax.NewRebindingScope()
		inputCtx := &syntax.SourceContext{Scopes: []*syntax.Scope{rebindScope}}
		input := syntax.NewSyntaxSymbol("=>", inputCtx)
		pattern := syntax.NewSyntaxSymbol("=>", nil)
		c.Assert(literalScopesMatch(input, pattern), qt.IsFalse)
	})

	c.Run("both have same rebinding scope matches", func(c *qt.C) {
		rebindScope := syntax.NewRebindingScope()
		inputCtx := &syntax.SourceContext{Scopes: []*syntax.Scope{rebindScope}}
		patternCtx := &syntax.SourceContext{Scopes: []*syntax.Scope{rebindScope}}
		input := syntax.NewSyntaxSymbol("=>", inputCtx)
		pattern := syntax.NewSyntaxSymbol("=>", patternCtx)
		c.Assert(literalScopesMatch(input, pattern), qt.IsTrue)
	})

	c.Run("non-rebinding scopes ignored", func(c *qt.C) {
		normalScope := syntax.NewScope()
		inputCtx := &syntax.SourceContext{Scopes: []*syntax.Scope{normalScope}}
		input := syntax.NewSyntaxSymbol("x", inputCtx)
		pattern := syntax.NewSyntaxSymbol("x", nil)
		c.Assert(literalScopesMatch(input, pattern), qt.IsTrue)
	})
}

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

// --- ExpandPreservingSyntax tests ---

func TestExpandPreservingSyntax(t *testing.T) {
	c := qt.New(t)

	c.Run("simple variable substitution preserves syntax", func(c *qt.C) {
		// Hand-build bytecode: CaptureCar(a), Done
		codes := []SyntaxCommand{
			ByteCodeCaptureCar{Binding: "a"},
			ByteCodeDone{},
		}
		vars := map[string]struct{}{"a": {}}
		m := NewMatcher(vars, codes)
		target := testSyntaxList(testSyntaxInt(42))
		err := m.MatchSyntax(context.Background(), target)
		c.Assert(err, qt.IsNil)

		template := values.NewCons(values.NewSymbol("a"), values.EmptyList)
		result, err := m.ExpandPreservingSyntax(template)
		c.Assert(err, qt.IsNil)
		c.Assert(result, qt.IsNotNil)
		pr, ok := result.(*values.Pair)
		c.Assert(ok, qt.IsTrue)
		// Car should be syntax-wrapped (SyntaxObject with Integer inside)
		_, isSyntax := pr[0].(syntax.SyntaxValue)
		c.Assert(isSyntax, qt.IsTrue)
	})

	c.Run("no capture context returns error", func(c *qt.C) {
		codes := []SyntaxCommand{
			ByteCodeCaptureCar{Binding: "a"},
			ByteCodeDone{},
		}
		vars := map[string]struct{}{"a": {}}
		m := NewMatcher(vars, codes)
		// Don't call Match — no capture context
		_, err := m.ExpandPreservingSyntax(values.NewSymbol("a"))
		c.Assert(err, qt.IsNotNil)
		c.Assert(err.Error(), qt.Matches, ".*capture context.*")
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
	m := NewSyntaxMatcher(vars, codes)

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
		result, err := sm.capturedValueToSyntax(sym, nil, nil, nil)
		c.Assert(err, qt.IsNil)
		ss, ok := result.(*syntax.SyntaxSymbol)
		c.Assert(ok, qt.IsTrue)
		c.Assert(ss.Sym.Key, qt.Equals, "hello")
	})

	c.Run("values.Pair wraps recursively", func(c *qt.C) {
		pr := values.NewCons(values.NewInteger(1), values.EmptyList)
		result, err := sm.capturedValueToSyntax(pr, nil, nil, nil)
		c.Assert(err, qt.IsNil)
		sp, ok := result.(*syntax.SyntaxPair)
		c.Assert(ok, qt.IsTrue)
		c.Assert(sp.Length(), qt.Equals, 1)
	})

	c.Run("values.EmptyList wraps to SyntaxEmptyList", func(c *qt.C) {
		result, err := sm.capturedValueToSyntax(values.EmptyList, nil, nil, nil)
		c.Assert(err, qt.IsNil)
		c.Assert(syntax.IsSyntaxEmptyList(result), qt.IsTrue)
	})

	c.Run("other value wraps to SyntaxObject", func(c *qt.C) {
		str := values.NewString("test")
		result, err := sm.capturedValueToSyntax(str, nil, nil, nil)
		c.Assert(err, qt.IsNil)
		so, ok := result.(*syntax.SyntaxObject)
		c.Assert(ok, qt.IsTrue)
		c.Assert(so.Unwrap().SchemeString(), qt.Equals, `"test"`)
	})

	c.Run("with origin and useSiteCtx", func(c *qt.C) {
		origin := &syntax.OriginInfo{Identifier: "test-macro"}
		useSite := &syntax.SourceContext{Text: "use"}
		str := values.NewString("val")
		result, err := sm.capturedValueToSyntax(str, nil, useSite, origin)
		c.Assert(err, qt.IsNil)
		c.Assert(result, qt.IsNotNil)
		c.Assert(result.SourceContext().Origin, qt.Equals, origin)
	})

	c.Run("with origin but no useSiteCtx", func(c *qt.C) {
		origin := &syntax.OriginInfo{Identifier: "test-macro"}
		str := values.NewString("val")
		result, err := sm.capturedValueToSyntax(str, nil, nil, origin)
		c.Assert(err, qt.IsNil)
		c.Assert(result, qt.IsNotNil)
		c.Assert(result.SourceContext().Origin, qt.Equals, origin)
	})
}

// --- applyHygieneToSymbol tests ---

// mockLocalScopes implements localScopesProvider for testing.
type mockLocalScopes struct {
	scopes []*syntax.Scope
}

func (p mockLocalScopes) GetLocalScopes() []*syntax.Scope { return p.scopes }

// mockGlobalBinding implements globalBindingProvider for testing.
type mockGlobalBinding struct {
	binding *environment.GlobalIndex
}

func (p mockGlobalBinding) GetGlobal() *environment.GlobalIndex { return p.binding }

// mockHasLocalBinding implements hasLocalBindingProvider for testing.
type mockHasLocalBinding struct {
	has bool
}

func (p mockHasLocalBinding) GetHasLocalBinding() bool { return p.has }

func TestApplyHygieneToSymbol(t *testing.T) {
	c := qt.New(t)

	sm := &SyntaxMatcher{matcher: &Matcher{}}
	introScope := syntax.NewScope()

	c.Run("non-free identifier gets intro scope", func(c *qt.C) {
		sym := syntax.NewSyntaxSymbol("foo", nil)
		result := sm.applyHygieneToSymbol(sym, introScope, nil, nil, nil)
		ss := result.(*syntax.SyntaxSymbol)
		c.Assert(ss.Sym.Key, qt.Equals, "foo")
		scopes := ss.Scopes()
		c.Assert(len(scopes), qt.Equals, 1)
		c.Assert(scopes[0], qt.Equals, introScope)
	})

	c.Run("free identifier with local scopes", func(c *qt.C) {
		defScope := syntax.NewScope()
		freeIds := map[string]any{
			"bar": mockLocalScopes{scopes: []*syntax.Scope{defScope}},
		}
		sym := syntax.NewSyntaxSymbol("bar", &syntax.SourceContext{Text: "bar"})
		result := sm.applyHygieneToSymbol(sym, introScope, freeIds, nil, nil)
		ss := result.(*syntax.SyntaxSymbol)
		c.Assert(ss.Sym.Key, qt.Equals, "bar")
		// Should have definition-site scopes, not intro scope
		scopes := ss.Scopes()
		c.Assert(len(scopes), qt.Equals, 1)
		c.Assert(scopes[0], qt.Equals, defScope)
	})

	c.Run("free identifier with global binding", func(c *qt.C) {
		globalIdx := environment.NewGlobalIndex(values.NewSymbol("baz"))
		freeIds := map[string]any{
			"baz": mockGlobalBinding{binding: globalIdx},
		}
		sym := syntax.NewSyntaxSymbol("baz", &syntax.SourceContext{Text: "baz"})
		result := sm.applyHygieneToSymbol(sym, introScope, freeIds, nil, nil)
		ss := result.(*syntax.SyntaxSymbol)
		c.Assert(ss.Sym.Key, qt.Equals, "baz")
		c.Assert(ss.ResolvedBinding, qt.Equals, globalIdx)
	})

	c.Run("free identifier with hasLocalBinding true", func(c *qt.C) {
		freeIds := map[string]any{
			"qux": mockHasLocalBinding{has: true},
		}
		sym := syntax.NewSyntaxSymbol("qux", nil)
		result := sm.applyHygieneToSymbol(sym, introScope, freeIds, nil, nil)
		ss := result.(*syntax.SyntaxSymbol)
		c.Assert(ss.Sym.Key, qt.Equals, "qux")
		// Should NOT have intro scope (has local binding)
		c.Assert(len(ss.Scopes()), qt.Equals, 0)
	})

	c.Run("with useSiteCtx overrides source context", func(c *qt.C) {
		useSite := &syntax.SourceContext{Text: "use-site"}
		sym := syntax.NewSyntaxSymbol("foo", &syntax.SourceContext{Text: "template"})
		result := sm.applyHygieneToSymbol(sym, introScope, nil, useSite, nil)
		c.Assert(result, qt.IsNotNil)
	})

	c.Run("with origin info", func(c *qt.C) {
		origin := &syntax.OriginInfo{Identifier: "test"}
		srcCtx := &syntax.SourceContext{Text: "src"}
		sym := syntax.NewSyntaxSymbol("foo", srcCtx)
		result := sm.applyHygieneToSymbol(sym, introScope, nil, nil, origin)
		ss := result.(*syntax.SyntaxSymbol)
		c.Assert(ss.SourceContext().Origin, qt.Equals, origin)
	})

	c.Run("global binding with existing scopes clears them", func(c *qt.C) {
		globalIdx := environment.NewGlobalIndex(values.NewSymbol("baz"))
		freeIds := map[string]any{
			"baz": mockGlobalBinding{binding: globalIdx},
		}
		existingScope := syntax.NewScope()
		srcCtx := &syntax.SourceContext{
			Text:   "baz",
			Scopes: []*syntax.Scope{existingScope},
		}
		sym := syntax.NewSyntaxSymbol("baz", srcCtx)
		result := sm.applyHygieneToSymbol(sym, introScope, freeIds, nil, nil)
		ss := result.(*syntax.SyntaxSymbol)
		c.Assert(ss.ResolvedBinding, qt.Equals, globalIdx)
		// Scopes should be cleared when applying global binding
		c.Assert(len(ss.Scopes()), qt.Equals, 0)
	})

	c.Run("non-free with existing scopes clears them", func(c *qt.C) {
		existingScope := syntax.NewScope()
		srcCtx := &syntax.SourceContext{
			Text:   "foo",
			Scopes: []*syntax.Scope{existingScope},
		}
		sym := syntax.NewSyntaxSymbol("foo", srcCtx)
		result := sm.applyHygieneToSymbol(sym, introScope, nil, nil, nil)
		ss := result.(*syntax.SyntaxSymbol)
		// Should have only intro scope, not the existing scope
		scopes := ss.Scopes()
		c.Assert(len(scopes), qt.Equals, 1)
		c.Assert(scopes[0], qt.Equals, introScope)
	})
}

// --- expandEscapedSyntaxTemplate tests ---

func TestExpandEscapedSyntaxTemplate(t *testing.T) {
	c := qt.New(t)

	c.Run("nil template returns nil", func(c *qt.C) {
		sm := &SyntaxMatcher{matcher: &Matcher{}}
		ctx := &captureContext{bindings: map[string]syntax.SyntaxValue{}}
		result, err := sm.expandEscapedSyntaxTemplate(nil, ctx, nil, nil, nil, nil, nil, nil)
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
		result, err := sm.expandEscapedSyntaxTemplate(template, ctx, nil, nil, nil, nil, nil, nil)
		c.Assert(err, qt.IsNil)
		c.Assert(result, qt.IsNotNil)
	})

	c.Run("non-variable symbol gets hygiene", func(c *qt.C) {
		sm := &SyntaxMatcher{matcher: &Matcher{}}
		ctx := &captureContext{bindings: map[string]syntax.SyntaxValue{}}
		is := syntax.NewScope()
		template := syntax.NewSyntaxSymbol("foo", nil)
		result, err := sm.expandEscapedSyntaxTemplate(template, ctx, nil, is, nil, nil, nil, nil)
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
		result, err := sm.expandEscapedSyntaxTemplate(template, ctx, nil, nil, nil, nil, nil, nil)
		c.Assert(err, qt.IsNil)
		sp, ok := result.(*syntax.SyntaxPair)
		c.Assert(ok, qt.IsTrue)
		c.Assert(sp, qt.IsNotNil)
	})

	c.Run("empty list passthrough", func(c *qt.C) {
		sm := &SyntaxMatcher{matcher: &Matcher{}}
		ctx := &captureContext{bindings: map[string]syntax.SyntaxValue{}}
		emptyList := syntax.NewSyntaxEmptyList(nil)
		template := syntax.NewSyntaxCons(emptyList, syntax.NewSyntaxEmptyList(nil), nil)
		result, err := sm.expandEscapedSyntaxTemplate(template, ctx, nil, nil, nil, nil, nil, nil)
		c.Assert(err, qt.IsNil)
		c.Assert(result, qt.IsNotNil)
	})

	c.Run("non-syntax value passthrough", func(c *qt.C) {
		sm := &SyntaxMatcher{matcher: &Matcher{}}
		ctx := &captureContext{bindings: map[string]syntax.SyntaxValue{}}
		literal := syntax.NewSyntaxObject(values.NewInteger(99), nil)
		result, err := sm.expandEscapedSyntaxTemplate(literal, ctx, nil, nil, nil, nil, nil, nil)
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
		result, err := sm.expandEscapedSyntaxTemplate(template, ctx, nil, is, nil, nil, nil, patternVarSyntax)
		c.Assert(err, qt.IsNil)
		// Should NOT substitute — should apply hygiene instead
		ss := result.(*syntax.SyntaxSymbol)
		c.Assert(ss.Sym.Key, qt.Equals, "x")
		scopes := ss.Scopes()
		c.Assert(len(scopes), qt.Equals, 1)
		c.Assert(scopes[0], qt.Equals, is)
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
			syntax.NewSyntaxEmptyList(nil),
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
