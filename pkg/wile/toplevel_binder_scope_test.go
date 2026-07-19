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
// RED and t.Skip-guarded. This is Stage C task C2, ATTEMPTED AND STOPPED on
// 2026-07-19; see the plan's C2 blocker section for the measurement.
//
// Stage B closed this for VALUE binders by passing the binder's scope set to
// binding creation. The syntax binder keeps the pre-Stage-B shape: create under
// nil (compile_define_syntax.go), then stamp m.Scopes afterwards. The set is the
// slot's KEY, so stamping it after the fact keys the slot under the empty set and
// then makes it report a set it was never keyed under — the same defect the plan
// diagnosed and fixed in predeclareBinding (letrec_semantics.go). Both keywords
// therefore key under {} and dedupe onto ONE slot, so the later define wins.
//
// The obvious fix — pass keywordSym.Scopes() at creation and resolve
// lookupMacroBinding under the reference's scopes — DOES make this test and its
// sibling pass, and breaks 9 tests in 4 packages (letrec-syntax self- and mutual
// recursion; guard/guard-aux). Measured cause: a syntax binder's scope set is not
// stable across expansion passes, so it is not a usable key. For the guard case,
// the my-guard-aux BINDER carries [scope:75(let)] while the reference to it from
// inside my-guard's template carries [scope:102(let) scope:101(lambda)
// scope:100(lambda) scope:99(lambda) scope:98(lambda) scope:97(intro)] — a
// DIFFERENT let scope. The binder is created when its define-syntax is compiled;
// a sibling macro's template reference arrives in a later expansion carrying
// freshly minted scopes, so bindingScopes ⊄ useScopes and an ordinary
// macro-to-macro reference stops resolving.
//
// That is why syntax binders were left unscoped, and it falsifies C2's stated
// premise that "scope-keying does not have that side effect". The value-binder
// path does not hit it because a define and its references are scoped in the same
// pass. Any real fix needs a binder key that survives re-expansion — the
// macro-introduction scopes alone, or a stable binder identity — which is
// successor work, not a call-site change.
func TestTopLevelMacroSyntaxBinder_DoesNotLeakToUser(t *testing.T) {
	t.Skip("RED: C2 blocker — define-syntax keywords key under {} and dedupe onto one slot; see plan C2 blocker")

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
// RED and t.Skip-guarded — same C2 blocker as the test above. Measured on HEAD:
// user-then-macro yields 'introduced (wrong), macro-then-user yields 'user (right
// answer, wrong reason). Order deciding the answer is the shared-slot signature;
// two hygiene-distinct slots would make both orders yield 'user.
func TestTopLevelMacroSyntaxBinder_DoesNotClobberUser(t *testing.T) {
	t.Skip("RED: C2 blocker — one shared slot, so last define-syntax wins; see plan C2 blocker")

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
