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

package wile_test

// Hygiene of macro-introduced TOP-LEVEL binders.
//
// These pin behavior across the migration from syntactic renaming
// (compilation/toplevel_binder_hygiene.go, which gave each macro-introduced
// top-level define a fresh unguessable name and rewrote its references) to
// scope-keyed global bindings (the binder and its references carry the macro's
// intro scope, and the global frame keys on it).
//
// They MUST run through the Engine: the rename pass is invoked from
// ExpandTopLevelExpression, so a harness calling ExpandExpression directly does
// not exercise the production path at all.

import (
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

func evalTopLevel(t *testing.T, src string) (string, error) {
	t.Helper()
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	if err != nil {
		return "", err
	}
	defer eng.Close()
	v, err := eng.EvalMultiple(ctx, src)
	if err != nil {
		return "", err
	}
	return v.SchemeString(), nil
}

// A macro-introduced top-level define must not capture a user's same-named
// top-level variable.
func TestTopLevelMacroBinder_DoesNotLeakToUser(t *testing.T) {
	c := qt.New(t)
	got, err := evalTopLevel(t, `
		(define x 1)
		(define-syntax m (syntax-rules () ((_) (define x 0))))
		(m)
		x`)
	c.Assert(err, qt.IsNil)
	c.Assert(got, qt.Equals, "1")
}

// Two expansions of the same binder-introducing macro must not alias one
// another through their shared introduced temporary.
func TestTopLevelMacroBinder_ExpansionsDoNotCollide(t *testing.T) {
	c := qt.New(t)
	got, err := evalTopLevel(t, `
		(define-syntax def-and-read
			(syntax-rules ()
				((_ out v) (begin (define tmp v) (define out tmp)))))
		(def-and-read a 1)
		(def-and-read b 2)
		(+ a b)`)
	c.Assert(err, qt.IsNil)
	c.Assert(got, qt.Equals, "3")
}

// Nested expansions of a binder-introducing macro produce same-named binders
// whose scope sets are nested ({s1} and {s1,s2}); a reference carrying the
// larger set must resolve to the more specific binder, per Flatt's maximal
// rule. The retired rename pass delivered this by giving each binder a distinct
// fresh name; scope-keyed globals must deliver it from the scope sets alone.
func TestTopLevelMacroBinder_NestedBindersResolveMaximally(t *testing.T) {
	c := qt.New(t)
	got, err := evalTopLevel(t, `
		(define-syntax inner
			(syntax-rules ()
				((_ out) (begin (define tmp 'inner) (define out tmp)))))
		(define-syntax outer
			(syntax-rules ()
				((_ out) (begin (define tmp 'outer) (inner out)))))
		(outer got)
		got`)
	c.Assert(err, qt.IsNil)
	// `out` is bound by inner's expansion, so it must see inner's tmp, not the
	// outer expansion's same-named binder one scope up.
	c.Assert(got, qt.Equals, "inner")
}

// A quoted symbol is data (R7RS §4.1.2), never a reference, so a
// macro-introduced binder of the same name must not change what it evaluates
// to. The rename pass had to special-case quote/quasiquote traversal to avoid
// rewriting data; scope keying rewrites nothing, so this must hold trivially.
func TestTopLevelMacroBinder_QuotedDatumIsNotCaptured(t *testing.T) {
	c := qt.New(t)
	got, err := evalTopLevel(t, `
		(define-syntax m (syntax-rules () ((_) (define tmp 'introduced))))
		(m)
		(quote tmp)`)
	c.Assert(err, qt.IsNil)
	c.Assert(got, qt.Equals, "tmp")
}

// A macro-introduced binder must stay invisible to user code that names it.
func TestTopLevelMacroBinder_IntroducedNameIsNotReachable(t *testing.T) {
	c := qt.New(t)
	_, err := evalTopLevel(t, `
		(define-syntax m (syntax-rules () ((_) (define hidden 7))))
		(m)
		hidden`)
	c.Assert(err, qt.IsNotNil)
}

// A macro-generating macro expanded TWICE must give each generated inner macro
// its own copy of the phase-0 define the outer template introduced.
//
// This is the jabberwocky/march-hare case that GetGlobalIndexAcrossPhases's doc
// comment cites as the reason the phase-0 runtime search must stay reachable
// (environment_frame.go). One expansion always worked. Two did not: each
// expansion's `march-hare` carries that expansion's intro scope, so the name owns
// two slots, and the free-identifier resolution behind the generated macro's
// template resolved it by BARE NAME — taking whichever slot was created first, for
// both hatters. Definition order decided the answer for everyone.
func TestMacroGeneratingMacro_TwoExpansionsDoNotShareBinder(t *testing.T) {
	c := qt.New(t)
	got, err := evalTopLevel(t, `
		(define-syntax jabberwocky
		  (syntax-rules ()
		    ((_ hatter val)
		     (begin (define march-hare val)
		            (define-syntax hatter (syntax-rules () ((_) march-hare)))))))
		(jabberwocky mad-hatter 1)
		(jabberwocky dormouse 2)
		(list (mad-hatter) (dormouse))`)
	c.Assert(err, qt.IsNil)
	c.Assert(got, qt.Equals, "(1 2)")
}

// A macro-introduced top-level define-syntax keyword must not be visible to a
// bare user reference — the define-syntax analogue of
// TestTopLevelMacroBinder_DoesNotLeakToUser above.
//
// RED and t.Skip-guarded. Stage C task C2; see the plan's C2 section.
//
// Stage B closed this for VALUE binders by passing the binder's scope set to
// binding creation. Syntax binders keep the pre-Stage-B shape at BOTH creation
// sites — compile_define_syntax.go:83 (top level) and expander_body.go:166
// (internal) — create under nil, then stamp m.Scopes afterwards. Creation dedupes
// with scopeSetsEqual against the STAMPED set (global_environment_frame.go:327),
// so passing nil compares against the empty set: a macro-introduced {intro}
// keyword reuses a pre-existing empty-scoped binding and re-stamps it, and the
// later define wins. Lookup would not discriminate anyway — lookupMacroBinding
// (expander_time_continuation.go:330) passes nil scopes, which GetBinding
// documents as MATCH ANY, on all three arms.
//
// The obvious fix — pass keywordSym.Scopes() at creation and resolve
// lookupMacroBinding under the reference's scopes — makes this test pass and
// breaks 9 tests in 4 packages (letrec-syntax self- and mutual recursion;
// guard/guard-aux).
//
// The cause is NOT that a binder's scope set drifts between passes. Expansion is
// one pass (ExpandAndCompile, expand_and_compile.go:37, expands to completion then
// compiles; the compiler never re-enters the expander on expander output).
// Template identifiers are scope-STRIPPED unconditionally — applyHygieneToSymbol
// calls srcCtx.WithoutScopes() (match/syntax_expand.go:285) and then adds only the
// intro scope. So a binder's ambient scopes can never appear on a template
// reference to it, by design, and keying binders on the full ambient set can never
// resolve macro-to-macro reference. Measured: binder [scope:97(let)], template ref
// [scope:98(intro)]; a NON-template reference to the same sibling keeps its let
// scope intact. In the guard case the [scope:75(let)] binder and the
// [scope:102(let) …] reference are two DIFFERENT lets (my-guard's handler let and
// its body let), not one let re-minted.
//
// The FreeIds pin (compile_syntax_rules.go:310) is what re-establishes the
// definition-site link out of band, and it covers macro keywords, but it resolves
// only what is bound at macro-DEFINITION time. my-guard's template names
// my-guard-aux before that binder exists, so the pin is nil and the reference
// falls to the strip-plus-intro branch. A non-nil pin would not help either:
// tryResolvedBinding (compile_time_continuation.go:322) is consumed only by the
// value-level symbol compiler, never by the macro lookup.
//
// The value-binder path avoids all of this because a define and its references are
// scoped in the same pass, with no stripping between them.
//
// Path forward: two defects sit under C2 and are separately observable. (1) The nil
// creation key above. (2) MaybeCreateOwnGlobalBinding returns a name-only
// GlobalIndex (matchAny() true), so the following GetGlobalBinding re-resolves to
// the first live slot of that name and can stamp a different binding than the one
// just created. Fix both, then re-measure whether keying on the macro-introduction
// scopes alone is still needed — treating both creation sites that way already
// takes the fallout from 9 failures to 1.
func TestTopLevelMacroSyntaxBinder_DoesNotLeakToUser(t *testing.T) {
	t.Skip("RED: C2 — define-syntax binders are created under a nil scope key and dedupe onto one slot; see plan C2")

	c := qt.New(t)
	_, err := evalTopLevel(t, `
		(define-syntax mk
		  (syntax-rules () ((_ u) (define-syntax helper (syntax-rules () ((_) 'introduced))))))
		(mk ignored)
		(helper)`)
	c.Assert(err, qt.IsNotNil)
}

// A macro-introduced define-syntax keyword must not clobber a user's same-named
// keyword. Both orders must give the user's transformer for a bare reference;
// today the two share one slot, so whichever is defined LAST wins.
//
// RED and t.Skip-guarded — same C2 cause as the test above. Measured on HEAD:
// user-then-macro yields 'introduced (wrong), macro-then-user yields 'user (right
// answer, wrong reason). Order deciding the answer is the shared-slot signature;
// two hygiene-distinct slots would make both orders yield 'user.
func TestTopLevelMacroSyntaxBinder_DoesNotClobberUser(t *testing.T) {
	t.Skip("RED: C2 — one shared slot, so last define-syntax wins; see plan C2")

	c := qt.New(t)

	// User's keyword defined FIRST, macro-introduced one after.
	got, err := evalTopLevel(t, `
		(define-syntax helper (syntax-rules () ((_) 'user)))
		(define-syntax mk
		  (syntax-rules () ((_ u) (define-syntax helper (syntax-rules () ((_) 'introduced))))))
		(mk ignored)
		(helper)`)
	c.Assert(err, qt.IsNil)
	c.Assert(got, qt.Equals, "user")

	// Reverse order. This one already answered 'user, but for the wrong reason —
	// last-definition-wins rather than hygiene.
	got, err = evalTopLevel(t, `
		(define-syntax mk
		  (syntax-rules () ((_ u) (define-syntax helper (syntax-rules () ((_) 'introduced))))))
		(mk ignored)
		(define-syntax helper (syntax-rules () ((_) 'user)))
		(helper)`)
	c.Assert(err, qt.IsNil)
	c.Assert(got, qt.Equals, "user")
}
