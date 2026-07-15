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

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/wile"
)

func newHygieneEngine(ctx context.Context, t *testing.T) *wile.Engine {
	t.Helper()
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithSourceOS(),
		wile.WithLibraryPaths(),
	) // default: immutable top level
	qt.Assert(t, err, qt.IsNil)
	return eng
}

// TestMacroIntroducedTopLevelTemp_UniquePerExpansion pins the general hygiene
// property: a macro-introduced top-level binder is uniquely renamed per
// expansion, so two invocations in separate compilation units do not collide on
// the temp name under the default immutable top level. This is the general gap
// that define-values sat on; a plain syntax-rules macro exercises it directly.
//
// Engine.EvalMultiple compiles each top-level form as its own unit, so the two
// (def2 ...) calls are two units. Before the fix the second (define tmp ...) died
// with "cannot redefine immutable top-level binding tmp".
func TestMacroIntroducedTopLevelTemp_UniquePerExpansion(t *testing.T) {
	ctx := context.Background()
	eng := newHygieneEngine(ctx, t)
	defer func() {
		_ = eng.Close()
	}()

	q, evalErr := eng.EvalMultiple(ctx, `
		(define-syntax def2
		  (syntax-rules ()
		    ((def2 a b e)
		     (begin
		       (define tmp e)
		       (define a (car tmp))
		       (define b (cadr tmp))))))
		(def2 x y (list 1 2))
		(def2 p q (list 3 4))
		(list x y p q)
	`)
	qt.Assert(t, evalErr, qt.IsNil,
		qt.Commentf("macro-introduced top-level tmp must be unique per expansion"))
	qt.Assert(t, q.SchemeString(), qt.Equals, "(1 2 3 4)")
}

// TestMacroIntroducedTopLevelBinding_HiddenFromUseSite pins the other half of
// hygiene (R7RS §4.3): a macro-introduced top-level binding is NOT visible at the
// use site by its bare template name. Before the fix, name-keyed globals leaked
// the binding — (define-counter) then counter wrongly resolved to the macro's
// counter. Now the use-site counter is unbound.
func TestMacroIntroducedTopLevelBinding_HiddenFromUseSite(t *testing.T) {
	ctx := context.Background()
	eng := newHygieneEngine(ctx, t)
	defer func() {
		_ = eng.Close()
	}()

	_, defErr := eng.EvalMultiple(ctx, `
		(define-syntax define-counter
		  (syntax-rules () ((_) (define counter 0))))
		(define-counter)
	`)
	qt.Assert(t, defErr, qt.IsNil)

	_, refErr := eng.EvalMultiple(ctx, `counter`)
	qt.Assert(t, refErr, qt.IsNotNil,
		qt.Commentf("a macro-introduced top-level binding must be hidden from the use site"))
}

// TestMacroIntroducedTopLevelTemp_ReferencedInsideLambda covers a reference that
// carries EXTRA binding-form scopes relative to the binder: the temp is used
// inside a lambda body, so its scope set is a superset of the binder's. Maximal
// subset resolution must still rename it to the same fresh name.
func TestMacroIntroducedTopLevelTemp_ReferencedInsideLambda(t *testing.T) {
	ctx := context.Background()
	eng := newHygieneEngine(ctx, t)
	defer func() {
		_ = eng.Close()
	}()

	q, evalErr := eng.EvalMultiple(ctx, `
		(define-syntax defn
		  (syntax-rules ()
		    ((defn name e)
		     (begin
		       (define tmp e)
		       (define name (lambda () tmp))))))
		(defn f 10)
		(defn g 20)
		(list (f) (g))
	`)
	qt.Assert(t, evalErr, qt.IsNil)
	qt.Assert(t, q.SchemeString(), qt.Equals, "(10 20)")
}

// TestMacroIntroducedTopLevelTemp_DoesNotCorruptQuotedData pins that the rename
// touches identifiers only, never quoted/quasiquoted DATUM. A macro that both
// introduces a top-level temp `tmp` and mentions `tmp` in quote / quasiquote
// position must leave the datum's symbol alone (R7RS §4.1.2) — otherwise the
// program-observable value is silently corrupted (e.g. `(quote tmp)` becoming an
// internal name).
func TestMacroIntroducedTopLevelTemp_DoesNotCorruptQuotedData(t *testing.T) {
	ctx := context.Background()

	tcs := []struct {
		name string
		src  string
		want string
	}{
		{
			"quote",
			`(define-syntax mk
			   (syntax-rules ()
			     ((mk out) (begin (define tmp 5) (define out (quote tmp))))))
			 (mk result)
			 result`,
			"tmp",
		},
		{
			"quasiquote literal + unquote",
			`(define-syntax mk
			   (syntax-rules ()
			     ((mk out v) (begin (define tmp v) (define out ` + "`" + `(tmp ,tmp))))))
			 (mk result 7)
			 result`,
			"(tmp 7)",
		},
		{
			// quote NESTED IN quasiquote: quote is ordinary structure there, so
			// the unquoted tmp is a live reference and must be renamed, while the
			// literal quote/list structure is preserved. Regression for the
			// quote-is-a-barrier-at-all-depths bug.
			"quote inside quasiquote",
			`(define-syntax mk
			   (syntax-rules ()
			     ((mk out v) (begin (define tmp v) (define out ` + "`" + `(quote ,tmp))))))
			 (mk result 42)
			 result`,
			"(quote 42)",
		},
		{
			"unquote-splicing",
			`(define-syntax mk
			   (syntax-rules ()
			     ((mk out v) (begin (define tmp v) (define out ` + "`" + `(a ,@tmp z))))))
			 (mk result (list 1 2))
			 result`,
			"(a 1 2 z)",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			eng := newHygieneEngine(ctx, t)
			defer func() {
				_ = eng.Close()
			}()
			q, evalErr := eng.EvalMultiple(ctx, tc.src)
			qt.Assert(t, evalErr, qt.IsNil,
				qt.Commentf("quoted data must survive the rename"))
			qt.Assert(t, q.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestMacroIntroducedTopLevelTemp_FreshNameUnforgeable guards that the generated
// name is not a reader-producible identifier: a program cannot reach or clobber
// the hidden temp by spelling its name. Before the fix the name was "tmp.1", a
// legal identifier, so `(define tmp.1 …)` collided with the macro's temp.
func TestMacroIntroducedTopLevelTemp_FreshNameUnforgeable(t *testing.T) {
	ctx := context.Background()
	eng := newHygieneEngine(ctx, t)
	defer func() {
		_ = eng.Close()
	}()

	// A user binding named "tmp.1" must coexist with a macro that introduces a
	// top-level temp — the generated name cannot be "tmp.1".
	_, err := eng.EvalMultiple(ctx, `
		(define tmp.1 111)
		(define-syntax mk
		  (syntax-rules ()
		    ((mk out e) (begin (define tmp e) (define out (car tmp))))))
		(mk a (list 42))`)
	qt.Assert(t, err, qt.IsNil,
		qt.Commentf("the generated temp name must not collide with a user 'tmp.1'"))

	q, evalErr := eng.EvalMultiple(ctx, `(list tmp.1 a)`)
	qt.Assert(t, evalErr, qt.IsNil)
	qt.Assert(t, q.SchemeString(), qt.Equals, "(111 42)",
		qt.Commentf("user tmp.1 unclobbered; macro temp unreachable by name"))
}

// TestMacroIntroducedTopLevelFunctionBinder covers a macro that introduces a
// top-level FUNCTION-form helper (define (helper …) …) — the function-form
// binder shape — used across two compilation units. It must be renamed uniquely
// per expansion just like a value binder.
func TestMacroIntroducedTopLevelFunctionBinder(t *testing.T) {
	ctx := context.Background()
	eng := newHygieneEngine(ctx, t)
	defer func() {
		_ = eng.Close()
	}()

	q, evalErr := eng.EvalMultiple(ctx, `
		(define-syntax defn
		  (syntax-rules ()
		    ((defn name k)
		     (begin
		       (define (helper x) (* x k))
		       (define name (helper 3))))))
		(defn a 10)
		(defn b 100)
		(list a b)`)
	qt.Assert(t, evalErr, qt.IsNil,
		qt.Commentf("a macro-introduced function-form top-level helper must be unique per expansion"))
	qt.Assert(t, q.SchemeString(), qt.Equals, "(30 300)")
}

// TestMacroIntroducedTopLevelDefineSyntaxSurvives guards the pass's scope
// boundary: a macro-introduced top-level (define-syntax …) is intentionally NOT
// renamed (it is resolved across phases by the macro-generating-macro path), so
// it must stay usable within the expansion.
func TestMacroIntroducedTopLevelDefineSyntaxSurvives(t *testing.T) {
	ctx := context.Background()
	eng := newHygieneEngine(ctx, t)
	defer func() {
		_ = eng.Close()
	}()

	q, evalErr := eng.EvalMultiple(ctx, `
		(define-syntax gen
		  (syntax-rules ()
		    ((gen out)
		     (begin
		       (define-syntax inner (syntax-rules () ((inner) 99)))
		       (define out (inner))))))
		(gen result)
		result`)
	qt.Assert(t, evalErr, qt.IsNil,
		qt.Commentf("a macro-introduced top-level define-syntax must survive the pass"))
	qt.Assert(t, q.SchemeString(), qt.Equals, "99")
}

// TestMacroIntroducedTopLevelTemp_WholeProgram covers the EvalProgram path, where
// every top-level form is spliced into ONE (begin …) compilation unit. Two
// temp-introducing macro invocations plus a define-values therefore share a unit,
// stressing within-unit uniqueness of the generated names.
func TestMacroIntroducedTopLevelTemp_WholeProgram(t *testing.T) {
	ctx := context.Background()
	eng := newHygieneEngine(ctx, t)
	defer func() {
		_ = eng.Close()
	}()

	q, evalErr := eng.EvalProgram(ctx, `
		(define-syntax defn
		  (syntax-rules ()
		    ((defn name e) (begin (define tmp e) (define name (car tmp))))))
		(defn a (list 1))
		(defn b (list 2))
		(define-values (c d) (values 3 4))
		(list a b c d)
	`, "")
	qt.Assert(t, evalErr, qt.IsNil,
		qt.Commentf("whole-program: macro-introduced temps must stay unique within one unit"))
	qt.Assert(t, q.SchemeString(), qt.Equals, "(1 2 3 4)")
}

// TestMacroIntroducedTopLevelBinding_HiddenUnderMutableTopLevel pins that the
// use-site hiding holds regardless of top-level mutability. Under
// WithMutableTopLevel the OLD leak silently "worked" by aliasing (no redefine
// error to surface it), so this is where a regression would hide.
func TestMacroIntroducedTopLevelBinding_HiddenUnderMutableTopLevel(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithSourceOS(),
		wile.WithLibraryPaths(),
		wile.WithMutableTopLevel(),
	)
	qt.Assert(t, err, qt.IsNil)
	defer func() {
		_ = eng.Close()
	}()

	_, defErr := eng.EvalMultiple(ctx, `
		(define-syntax define-counter
		  (syntax-rules () ((_) (define counter 0))))
		(define-counter)`)
	qt.Assert(t, defErr, qt.IsNil)

	_, refErr := eng.EvalMultiple(ctx, `counter`)
	qt.Assert(t, refErr, qt.IsNotNil,
		qt.Commentf("the macro's counter must stay hidden even under a mutable top level"))
}

// TestMacroIntroducedTopLevelTemp_NoAliasingUnderMutable catches temp ALIASING
// under a mutable top level. There a name collision does not raise a redefine
// error (that guard is immutable-only), so two expansions sharing a temp name
// would silently alias — each closure would read the last-written value. Deferred
// reads through captured lambdas expose that as a wrong value: distinct names
// give (10 20), an aliased single temp would give (20 20).
func TestMacroIntroducedTopLevelTemp_NoAliasingUnderMutable(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithSourceOS(),
		wile.WithLibraryPaths(),
		wile.WithMutableTopLevel(),
	)
	qt.Assert(t, err, qt.IsNil)
	defer func() {
		_ = eng.Close()
	}()

	q, evalErr := eng.EvalMultiple(ctx, `
		(define-syntax defn
		  (syntax-rules ()
		    ((defn name e) (begin (define tmp e) (define name (lambda () tmp))))))
		(defn f 10)
		(defn g 20)
		(list (f) (g))`)
	qt.Assert(t, evalErr, qt.IsNil)
	qt.Assert(t, q.SchemeString(), qt.Equals, "(10 20)",
		qt.Commentf("each expansion's temp must be distinct (no silent aliasing under mutable)"))
}
