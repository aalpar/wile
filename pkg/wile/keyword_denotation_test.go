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
	"testing/fstest"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// A special form is recognized by what its head is BOUND to, not by its
// spelling (R7RS §5.6.1: rename and prefix apply to keywords). Petite Chez
// 10.4.1 and Racket 9.2 accept every row below.

type keywordRow struct {
	name string
	src  string
	want string
}

func keywordDenotationEngine(t *testing.T) *wile.Engine {
	t.Helper()
	eng, err := wile.NewEngine(context.Background(),
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."),
	)
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	return eng
}

func runKeywordRows(t *testing.T, rows []keywordRow) {
	t.Helper()
	for _, row := range rows {
		t.Run(row.name, func(t *testing.T) {
			eng := keywordDenotationEngine(t)
			v, err := eng.EvalMultiple(context.Background(), row.src)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, v.SchemeString(), qt.Equals, row.want)
		})
	}
}

// Controls: pass before and after. A renamed keyword is still shadowable, and
// the unrenamed forms keep working.
func TestKeywordDenotationControls(t *testing.T) {
	runKeywordRows(t, []keywordRow{
		{"let shadows a renamed keyword",
			`(import (scheme base) (rename (scheme base) (if my-if))) (let ((my-if list)) (my-if 1 2 3))`, "(1 2 3)"},
		{"define shadows if",
			`(import (scheme base)) (define if list) (if 1 2 3)`, "(1 2 3)"},
		{"nested define-syntax in a template",
			`(import (scheme base)) (define-syntax outer (syntax-rules () ((_ v) (let () (define-syntax inner (syntax-rules () ((_) v))) (inner))))) (outer 9)`, "9"},
		{"macro emits define",
			`(import (scheme base)) (define-syntax defx (syntax-rules () ((_ n) (define n 11)))) (defx zz) zz`, "11"},
		{"macro emits begin of defines",
			`(import (scheme base)) (define-syntax defxy (syntax-rules () ((_ a b) (begin (define a 1) (define b 2))))) (defxy p q) (+ p q)`, "3"},
	})
}

// Task 2: the validator dispatches on the denoted form.
func TestRenamedKeywordValidates(t *testing.T) {
	runKeywordRows(t, []keywordRow{
		{"if", `(import (rename (scheme base) (if my-if))) (my-if #t 1 2)`, "1"},
		{"lambda", `(import (rename (scheme base) (lambda my-lambda))) ((my-lambda (x) x) 5)`, "5"},
		{"define at top level", `(import (rename (scheme base) (define my-define))) (my-define z 3) z`, "3"},
		{"define in a body", `(import (scheme base) (rename (scheme base) (define my-define))) (define (f) (my-define z 3) z) (f)`, "3"},
		{"define-syntax at top level", `(import (scheme base) (rename (scheme base) (define-syntax my-ds))) (my-ds three (syntax-rules () ((_) 3))) (three)`, "3"},
		{"begin splicing a define", `(import (scheme base) (rename (scheme base) (begin my-begin))) (define (f) (my-begin (define z 4)) z) (f)`, "4"},
		{"begin at top level", `(import (scheme base) (rename (scheme base) (begin my-begin))) (my-begin (define z 10)) z`, "10"},
		{"quote", `(import (rename (scheme base) (quote my-quote))) (my-quote (a b))`, "(a b)"},
		{"quasiquote", `(import (scheme base) (rename (scheme base) (quasiquote qq))) (qq (1 ,(+ 1 1)))`, "(1 2)"},
		{"set!", `(import (scheme base) (rename (scheme base) (set! my-set!))) (let ((z 1)) (my-set! z 7) z)`, "7"},
		{"prefix", `(import (prefix (scheme base) b:)) (b:if #t 8 9)`, "8"},
		{"if and lambda swapped", `(import (rename (scheme base) (if lambda) (lambda if))) ((if (x) (lambda #f 1 x)) 5)`, "5"},
		{"lambda body with an internal define", `(import (scheme base) (rename (scheme base) (lambda fn))) (define f (fn (x) (define y (* x 2)) y)) (f 4)`, "8"},
		{"inside a macro template", `(import (scheme base) (rename (scheme base) (if my-if))) (define-syntax m (syntax-rules () ((_ c) (my-if c 'y 'n)))) (m #f)`, "n"},
		{"define beside a define-syntax in a body", `(import (scheme base) (rename (scheme base) (define my-define))) (define (f) (define-syntax m (syntax-rules () ((_) 1))) (my-define z (m)) z) (f)`, "1"},
		{"define inside a let-syntax body", `(import (scheme base) (rename (scheme base) (define my-define))) (let-syntax ((m (syntax-rules () ((_) 1)))) (my-define z 8) z)`, "8"},
	})
}

// Task 3: the expander must see a renamed begin as begin, or the define-syntax it
// splices into a body is never compiled before the body uses it.
func TestRenamedKeywordExpands(t *testing.T) {
	runKeywordRows(t, []keywordRow{
		{"begin splicing a define-syntax into a body",
			`(import (scheme base) (rename (scheme base) (begin my-begin))) (define (f) (my-begin (define-syntax m (syntax-rules () ((_) 6)))) (m)) (f)`, "6"},
	})
}

// Task 4: definitions are recognized by denotation in body scans and in use-site
// scope pruning of macro output.
func TestRenamedDefinitionsAreRecognized(t *testing.T) {
	runKeywordRows(t, []keywordRow{
		{"define-syntax in a let body",
			`(import (scheme base) (rename (scheme base) (define-syntax my-ds))) (let () (my-ds inner (syntax-rules () ((_) 3))) (inner))`, "3"},
		{"define-syntax in a lambda body",
			`(import (scheme base) (rename (scheme base) (define-syntax my-ds))) (define (f) (my-ds inner (syntax-rules () ((_) 4))) (inner)) (f)`, "4"},
		{"nested renamed define-syntax in a template",
			`(import (scheme base) (rename (scheme base) (syntax-rules my-sr) (define-syntax my-ds))) (define-syntax outer (syntax-rules () ((_ v) (let () (my-ds inner (syntax-rules () ((_) v))) (inner))))) (outer 9)`, "9"},
		{"macro emits a renamed define",
			`(import (scheme base) (rename (scheme base) (define my-define))) (define-syntax defx (syntax-rules () ((_ n) (my-define n 11)))) (defx zz) zz`, "11"},
		{"macro emits a renamed begin of defines",
			`(import (scheme base) (rename (scheme base) (begin my-begin))) (define-syntax defxy (syntax-rules () ((_ a b) (my-begin (define a 1) (define b 2))))) (defxy p q) (+ p q)`, "3"},
		{"macro emits a renamed define-syntax",
			`(import (scheme base) (rename (scheme base) (define-syntax my-ds))) (define-syntax mk (syntax-rules () ((_ n) (my-ds n (syntax-rules () ((_) 12)))))) (mk k) (k)`, "12"},
	})
}

// TestRenamedKeywordSurvivesCanonicalShadow pins the fix for
// lookupHeadPrimitiveExpander's canonical-name fallback: the fallback looks up
// the synthesized canonical spelling (e.g. "quote" for a head renamed to
// core-quote), and that lookup must not be shadowable by an unrelated user
// binding of the canonical name. Before the fix, redefining the canonical
// spelling ((define-syntax quote ...) after renaming quote to core-quote) made
// LookupPrimitiveExpander refuse to look past the user's shadow, so the renamed
// head fell into the ordinary-call path while the validator still dispatched it
// as the special form -- quoted data got macro-expanded, and a renamed begin
// lost its ability to splice a define-syntax into the enclosing body.
func TestRenamedKeywordSurvivesCanonicalShadow(t *testing.T) {
	runKeywordRows(t, []keywordRow{
		{"renamed quote survives a user shadow of quote",
			`(import (scheme base) (rename (scheme base) (quote core-quote))) (define-syntax quote (syntax-rules () ((_ x) 0))) (core-quote (when #t 2))`,
			"(when #t 2)"},
		{"renamed begin still splices a define-syntax despite a user shadow of begin",
			`(import (scheme base) (rename (scheme base) (begin my-begin))) (define-syntax begin (syntax-rules () ((_ x ...) (list x ...)))) (define (f) (my-begin (define-syntax m (syntax-rules () ((_) 6)))) (m)) (f)`,
			"6"},
	})
}

// TestKeywordImportConflicts pins import conflict detection for keywords: two
// libraries that re-export the SAME keyword under one name are a diamond (both
// rooted at the base's if), and two libraries that export DIFFERENT keywords
// under the SAME local name are refused as a conflict. When keyword bindings
// held values.Void, a value comparison could not tell them apart and the later
// import silently won.
func TestKeywordImportConflicts(t *testing.T) {
	t.Run("diamond: two libraries re-export the same keyword under one name", func(t *testing.T) {
		eng, err := wile.NewEngine(context.Background(),
			wile.WithProfile(wile.KitchenSink),
			wile.WithSourceFS(fstest.MapFS{
				"lib-if-a.scm": &fstest.MapFile{Data: []byte(
					"(define-library (lib-if-a)\n" +
						"  (import (scheme base))\n" +
						"  (export (rename if common-if)))\n")},
				"lib-if-b.scm": &fstest.MapFile{Data: []byte(
					"(define-library (lib-if-b)\n" +
						"  (import (scheme base))\n" +
						"  (export (rename if common-if)))\n")},
			}),
			wile.WithSourceFS(stdlib.FS),
			wile.WithLibraryPaths("."),
		)
		qt.Assert(t, err, qt.IsNil)
		t.Cleanup(func() {
			_ = eng.Close()
		})
		v, err := eng.EvalMultiple(context.Background(),
			"(import (lib-if-a) (lib-if-b)) (common-if #t 1 2)")
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, v.SchemeString(), qt.Equals, "1")
	})

	t.Run("clash: two libraries export different keywords under the same name", func(t *testing.T) {
		eng, err := wile.NewEngine(context.Background(),
			wile.WithProfile(wile.KitchenSink),
			wile.WithSourceFS(fstest.MapFS{
				"lib-if.scm": &fstest.MapFile{Data: []byte(
					"(define-library (lib-if)\n" +
						"  (import (scheme base))\n" +
						"  (export (rename if my-if)))\n")},
				"lib-lambda.scm": &fstest.MapFile{Data: []byte(
					"(define-library (lib-lambda)\n" +
						"  (import (scheme base))\n" +
						"  (export (rename lambda my-if)))\n")},
			}),
			wile.WithSourceFS(stdlib.FS),
			wile.WithLibraryPaths("."),
		)
		qt.Assert(t, err, qt.IsNil)
		t.Cleanup(func() {
			_ = eng.Close()
		})
		_, err = eng.EvalMultiple(context.Background(), "(import (lib-if) (lib-lambda))")
		qt.Assert(t, err, qt.IsNotNil)
		qt.Assert(t, err.Error(), qt.Contains, "my-if")
	})
}
