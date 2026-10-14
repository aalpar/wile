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

package integration_test

import (
	"context"
	"errors"
	"testing"
	"testing/fstest"
	"time"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/wile"
)

// coreFormEngine builds a KitchenSink engine that can import the libraries in
// libs, which are written as .sld files at the root of a virtual filesystem.
func coreFormEngine(c *qt.C, libs fstest.MapFS) *wile.Engine {
	engine, err := wile.NewEngine(context.Background(),
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(libs),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."))
	c.Assert(err, qt.IsNil)
	return engine
}

// TestCoreFormDenotation is I132: a core form spelled in a stdlib or library
// template denotes the form it denoted where the template was written. A
// use-site (define-syntax if …) does not capture the if inside cond, and a form
// renamed on the library's import (wif, qt) compiles as that form at a use site
// where the new spelling is unbound.
//
// Expected column: Racket 9.2 and Chez 10.4.1, byte-identical. The direct (if …)
// in the fourth slot is the user's macro and answers mine on all three.
func TestCoreFormDenotation(t *testing.T) {
	const userIf = `(define-syntax if (syntax-rules () ((_ . r) 'mine)))`
	tcs := []struct {
		name     string
		libs     fstest.MapFS
		program  string
		expected string
	}{
		{
			name:     "d1b_user_if_does_not_capture_derived_forms",
			program:  userIf + ` (list (cond (#f 1) (else 2)) (and 1 2) (or #f 7) (if #t 1 2) (case 1 ((1) 'one) (else 'other)))`,
			expected: "(2 2 7 mine one)",
		},
		{
			name:     "d1b_with_when",
			program:  userIf + ` (list (cond (#f 1) (else 2)) (and 1 2) (or #f 7) (if #t 1 2) (when #t 5) (case 1 ((1) 'one) (else 'other)))`,
			expected: "(2 2 7 mine 5 one)",
		},
		{
			name: "pin3_pin4_renamed_if_through_library_template",
			libs: fstest.MapFS{"wlib.sld": &fstest.MapFile{Data: []byte(`(define-library (wlib)
			  (export pick)
			  (import (rename (scheme base) (if wif)))
			  (begin
			    (define-syntax pick
			      (syntax-rules () ((_ c a b) (wif c a b))))))`)}},
			program:  `(import (wlib)) (list (pick #t 5 0) (pick #f 0 6))`,
			expected: "(5 6)",
		},
		{
			name: "qt_renamed_quote_through_library_template",
			libs: fstest.MapFS{"qlib.sld": &fstest.MapFile{Data: []byte(`(define-library (qlib)
			  (export q)
			  (import (rename (scheme base) (quote qt)))
			  (begin
			    (define-syntax q
			      (syntax-rules () ((_ x) (list (qt x) 4))))))`)}},
			program:  `(import (qlib)) (q abc)`,
			expected: "(abc 4)",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			engine := coreFormEngine(c, tc.libs)
			result, err := engine.EvalMultiple(context.Background(), tc.program)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tc.expected)
		})
	}
}

// TestCoreFormDenotationExceptIfTerminates is I133: a user if written with cond,
// with the core if excluded from the import. cond's template still denotes the
// core if, so the expansion stops instead of cycling if → cond → if until the
// deadline. Racket and Chez answer 1.
//
// err is asserted, never printed: a cancelled expansion wraps thousands of links
// deep.
func TestCoreFormDenotationExceptIfTerminates(t *testing.T) {
	c := qt.New(t)
	engine := coreFormEngine(c, nil)
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
	defer cancel()
	result, err := engine.EvalMultiple(ctx, `(import (except (scheme base) if))
	  (define-syntax if (syntax-rules () ((_ c a b) (cond (c a) (else b)))))
	  (if #t 1 2)`)
	c.Assert(errors.Is(err, context.DeadlineExceeded), qt.IsFalse, qt.Commentf("the if → cond → if cycle is back"))
	c.Assert(err == nil, qt.IsTrue)
	c.Assert(result.SchemeString(), qt.Equals, "1")
}

// TestCoreFormDenotationTemplateLocalBinder: a template that binds a core-form
// name itself shadows the core form it was written beside, so the pin does not
// outrank the template's own local binder. Expected column: Racket 9.2 and Chez
// 10.4.1, and master before P4.3.
func TestCoreFormDenotationTemplateLocalBinder(t *testing.T) {
	tcs := []struct {
		name     string
		program  string
		expected string
	}{
		{
			name:     "let_bound_if",
			program:  `(define-syntax m (syntax-rules () ((_ x) (let ((if (lambda a (list 'proc a)))) (if x 1 2))))) (m #t)`,
			expected: "(proc (#t 1 2))",
		},
		{
			name:     "let_bound_quote",
			program:  `(define-syntax m (syntax-rules () ((_ x) (let ((quote (lambda a (list 'proc a)))) (quote x))))) (m 5)`,
			expected: "(proc (5))",
		},
		{
			name:     "lambda_parameter_if",
			program:  `(define-syntax m (syntax-rules () ((_ x) ((lambda (if) (if x)) (lambda (v) (* v 10)))))) (m 5)`,
			expected: "50",
		},
		{
			name:     "let_bound_quote_two_args",
			program:  `(define-syntax m (syntax-rules () ((_ x) (let ((quote list)) (quote x 2))))) (m 5)`,
			expected: "(5 2)",
		},
		{
			name:     "internal_define_if",
			program:  `(define-syntax m (syntax-rules () ((_ x) (let () (define (if a) (* a 3)) (if x))))) (m 5)`,
			expected: "15",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			engine := coreFormEngine(c, nil)
			result, err := engine.EvalMultiple(context.Background(), tc.program)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tc.expected)
		})
	}
}

// TestCoreFormDenotationSyntaxLocalValue: syntax-local-value on a template's if,
// pinned to the core form, is not the user's (define-syntax if …). It reads
// lookupMacroBinding through ExpanderContext.MacroValue, which inherits the
// pin's terminal stop; master answered macro. Not oracle-checked.
func TestCoreFormDenotationSyntaxLocalValue(t *testing.T) {
	c := qt.New(t)
	engine := coreFormEngine(c, fstest.MapFS{"slv.sld": &fstest.MapFile{Data: []byte(`(define-library (slv)
	  (export probe-if)
	  (import (scheme base))
	  (begin
	    (define-syntax probe-if (syntax-rules () ((_ p) (p if))))))`)}})
	result, err := engine.EvalMultiple(context.Background(), `(import (slv))
	  (define-syntax probe
	    (lambda (stx)
	      (syntax-case stx ()
	        ((_ id) (if (syntax-local-value #'id (lambda () #f)) #''macro #''core)))))
	  (define-syntax if (syntax-rules () ((_ . r) 'mine)))
	  (probe-if probe)`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "core")
}
