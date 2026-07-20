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

// The LOCAL analogue of the case above. Two expansions of a macro-generating
// macro inside a body bind `march-hare` as an internal define, so the binders
// land in a LocalEnvironmentFrame rather than the global one — and the free
// identifier behind each generated macro's template is resolved by the local
// arm of collectFreeIdentifiersWithEllipsis, which passed nil scopes.
//
// nil is MATCH ANY (resolveLocal, environment_frame.go: matchAny := scopes ==
// nil), so the walk took the first live slot and both hatters saw expansion
// one's binder. The local arm also returns EARLY on a hit, so it shadowed the
// scope-verified global arm eleven lines below it — the two arms of one
// function disagreed about whether hygiene is checked.
//
// Racket gives (1 2).
func TestMacroGeneratingMacro_TwoLocalExpansionsDoNotShareBinder(t *testing.T) {
	c := qt.New(t)
	got, err := evalTopLevel(t, `
		(define-syntax jabberwocky
		  (syntax-rules ()
		    ((_ hatter val)
		     (begin (define march-hare val)
		            (define-syntax hatter (syntax-rules () ((_) march-hare)))))))
		(let ()
		  (jabberwocky mad-hatter 1)
		  (jabberwocky dormouse 2)
		  (list (mad-hatter) (dormouse)))`)
	c.Assert(err, qt.IsNil)
	c.Assert(got, qt.Equals, "(1 2)")
}

// Two same-named free identifiers in ONE generated template, carrying DIFFERENT
// scope sets, must each keep their own definition-time resolution.
//
// jab introduces its own `mh` (bound to v) and generates a macro whose template
// is `(list mh mhref)`. After jab expands, `mh` is jab's introduced binder
// (jab's intro scope) and `mhref` has been substituted with the user's `mh`
// (top-level scope) — so the generated macro's template holds two free
// identifiers both named "mh" but scoped differently. collectFreeIdentifiers
// resolves each correctly (the local/global arms key resolution on t.Scopes()),
// then stored both under the bare name "mh" in a map[string]*FreeIdResolution:
// the second write clobbered the first, and at expansion BOTH template `mh`s
// received the surviving resolution (the user's), yielding (99 99).
//
// Racket gives (1 99). Unlike the sibling cases above this reproduces from
// surface syntax at top level — the generated macro is a top-level binding, so
// it dodges the internal-define-syntax visibility limitation.
func TestMacroGeneratingMacro_SameNameFreeIdsDoNotCollapse(t *testing.T) {
	c := qt.New(t)
	got, err := evalTopLevel(t, `
		(define mh 99)
		(define-syntax jab
		  (syntax-rules ()
		    ((_ k mhref v)
		     (begin
		       (define mh v)
		       (define-syntax k
		         (syntax-rules () ((_) (list mh mhref))))))))
		(jab h1 mh 1)
		(h1)`)
	c.Assert(err, qt.IsNil)
	c.Assert(got, qt.Equals, "(1 99)")

	// Control: rename jab's introduced binder so the two free identifiers do NOT
	// share a name. No collapse is possible, so this yields (1 99) on the broken
	// code too — it proves the expectation is right and isolates the collapse
	// (same name) as the sole difference that produced (99 99).
	gotCtl, errCtl := evalTopLevel(t, `
		(define mh 99)
		(define-syntax jab
		  (syntax-rules ()
		    ((_ k mhref v)
		     (begin
		       (define zz v)
		       (define-syntax k
		         (syntax-rules () ((_) (list zz mhref))))))))
		(jab h1 mh 1)
		(h1)`)
	c.Assert(errCtl, qt.IsNil)
	c.Assert(gotCtl, qt.Equals, "(1 99)")
}

// A template that introduces two sibling defines, where the first forward-
// references the second, must resolve that reference to its co-introduced
// sibling. Both binders carry the same expansion's intro scope, so ordinary
// scoped resolution covers it — but only because the reference and the binder
// agree on that scope set.
//
// This shape had no Go unit test. It was guarded solely by
// integration/testdata/r7rs-tests.scm:557-567, which runs against the built
// dist/ binary and therefore passes silently when dist/ is stale (see the
// make-build-first note in the plan's gates). march-hare, the sibling shape,
// has two unit tests; this one had none.
func TestMacroIntroducedSiblingDefines_ForwardReferenceResolves(t *testing.T) {
	c := qt.New(t)
	got, err := evalTopLevel(t, `
		(define-syntax ffoo
		  (syntax-rules ()
		    ((ffoo ff)
		     (begin
		       (define (ff x) (gg x))
		       (define (gg x) (* x x))))))
		(ffoo ff)
		(ff 10)`)
	c.Assert(err, qt.IsNil)
	c.Assert(got, qt.Equals, "100")
}

// A macro-introduced top-level define-syntax keyword must not be visible to a
// bare user reference — the define-syntax analogue of
// TestTopLevelMacroBinder_DoesNotLeakToUser above.
//
// Closed by C2 (Stage C). Four changes land together because every proper subset
// is either inert or merely relocates the asymmetry: both creation sites key the
// binder on keywordSym.Scopes() instead of creating under nil and stamping the set
// afterwards, each pairs the create with a scope-resolved index rather than the
// name-only one creation returns, and lookupMacroBinding resolves under the
// reference's own scope set instead of MATCH ANY.
//
// The reference side had to move first, and it was the whole blocker. Template
// identifiers were scope-STRIPPED unconditionally (applyHygieneToSymbol), so a
// binder's ambient scopes could never reach a template reference to it and
// scope-keyed binders could not resolve macro-to-macro reference at all: 9 tests
// in 4 packages, every one the same "no such binding with compatible scopes" on
// letrec-syntax recursion or guard/guard-aux. The strip was defending against
// UseSiteCtx, whose scope set belongs to the invoking code; clearing the set
// outright took the definition-site scopes with it. Substituting the
// definition-site set defends against the use site and keeps R7RS §4.3
// referential transparency.
//
// The FreeIds pin cannot substitute for that. It resolves only what is bound at
// macro-DEFINITION time, so a forward reference (my-guard names my-guard-aux
// before its binder exists) pins nil — and tryResolvedBinding is consumed by the
// value-level symbol compiler, never by the macro lookup.
func TestTopLevelMacroSyntaxBinder_DoesNotLeakToUser(t *testing.T) {
	c := qt.New(t)
	_, err := evalTopLevel(t, `
		(define-syntax mk
		  (syntax-rules () ((_ u) (define-syntax helper (syntax-rules () ((_) 'introduced))))))
		(mk ignored)
		(helper)`)
	c.Assert(err, qt.IsNotNil)
}

// A macro-introduced define-syntax keyword must not clobber a user's same-named
// keyword. Both orders must give the user's transformer for a bare reference.
//
// Closed by C2 — same cause as the test above. Before it the two shared one slot,
// so whichever was defined LAST won: user-then-macro yielded 'introduced, and
// macro-then-user yielded 'user for the wrong reason. Order deciding the answer is
// the shared-slot signature, which is why both orders are exercised here.
func TestTopLevelMacroSyntaxBinder_DoesNotClobberUser(t *testing.T) {
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

	// Reverse order. This one answered 'user even before the fix, but by
	// last-definition-wins rather than by hygiene.
	got, err = evalTopLevel(t, `
		(define-syntax mk
		  (syntax-rules () ((_ u) (define-syntax helper (syntax-rules () ((_) 'introduced))))))
		(mk ignored)
		(define-syntax helper (syntax-rules () ((_) 'user)))
		(helper)`)
	c.Assert(err, qt.IsNil)
	c.Assert(got, qt.Equals, "user")
}

// The internal site has its own copy of both defects the two tests above pin at
// the top level: expander_body.go's define-syntax handler created the binder
// under a nil scope key and addressed the write with a name-only index. The
// top-level shape hides half of it — a top-level user binder carries the empty
// scope set, so it is distinguishable from a macro-introduced {intro} binder by
// the creation key alone. Inside a let body the user's binder carries {let}, so
// both the creation key and the scope-resolved write have to be right.
func TestInternalMacroSyntaxBinder_DoesNotClobberUser(t *testing.T) {
	c := qt.New(t)

	got, err := evalTopLevel(t, `
		(define-syntax mk
		  (syntax-rules () ((_ u) (define-syntax helper (syntax-rules () ((_) 'introduced))))))
		(let ()
		  (define-syntax helper (syntax-rules () ((_) 'user)))
		  (mk ignored)
		  (helper))`)
	c.Assert(err, qt.IsNil)
	c.Assert(got, qt.Equals, "user")
}

// The leak analogue of the test above: with no user binder in the let body at
// all, the keyword a macro introduced into that body must not answer a bare
// reference written by the user.
func TestInternalMacroSyntaxBinder_DoesNotLeakToUser(t *testing.T) {
	c := qt.New(t)

	_, err := evalTopLevel(t, `
		(define-syntax mk
		  (syntax-rules () ((_ u) (define-syntax helper (syntax-rules () ((_) 'introduced))))))
		(let ()
		  (mk ignored)
		  (helper))`)
	c.Assert(err, qt.IsNotNil)
}
