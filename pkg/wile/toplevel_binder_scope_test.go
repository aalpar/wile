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
