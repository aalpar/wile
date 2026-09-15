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
