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

package compilation_test

import (
	"testing"
	"testing/fstest"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// TestErMacroRename tests the ER macro rename closure via Scheme-level tests.
// The rename closure ensures that renamed identifiers resolve to definition-site
// bindings, providing hygiene for ER macros.
//
// Every program declares (for-syntax (scheme base)) and (for-syntax (scheme cxr)).
// An er-macro-transformer body is PROCEDURAL: it runs at phase 1, and since the
// sealed base stopped being ambient nothing reaches phase 1 that the program did
// not import. cadr/caddr/cadddr/cddr are bootstrap Scheme definitions with no
// phase-1 slot of their own, so they are exactly the names that go missing; cxr
// is a second import because caddr and cadddr live in cxr.sld, not base.sld. A
// syntax-rules macro needs no import — its template expands into the use site,
// which is phase 0.
func TestErMacroRename(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "rename provides hygienic if",
			Code: `(begin
			  (import (for-syntax (scheme base)) (for-syntax (scheme cxr)))
			  (define-syntax my-if
			    (er-macro-transformer
			      (lambda (form rename compare)
			        (let ((test (cadr form))
			              (consequent (caddr form))
			              (alternative (cadddr form)))
			          (list (rename 'if) test consequent alternative)))))
			  (my-if #t 'yes 'no))`,
			Expected: values.NewSymbol("yes"),
		},
		{
			Name: "rename provides hygienic if false branch",
			Code: `(begin
			  (import (for-syntax (scheme base)) (for-syntax (scheme cxr)))
			  (define-syntax my-if
			    (er-macro-transformer
			      (lambda (form rename compare)
			        (let ((test (cadr form))
			              (consequent (caddr form))
			              (alternative (cadddr form)))
			          (list (rename 'if) test consequent alternative)))))
			  (my-if #f 'yes 'no))`,
			Expected: values.NewSymbol("no"),
		},
		{
			Name: "rename provides hygienic let and set",
			Code: `(begin
			  (import (for-syntax (scheme base)) (for-syntax (scheme cxr)))
			  (define-syntax my-swap!
			    (er-macro-transformer
			      (lambda (form rename compare)
			        (let ((a (cadr form))
			              (b (caddr form)))
			          (list (rename 'let) (list (list (rename 'tmp) a))
			                (list (rename 'set!) a b)
			                (list (rename 'set!) b (rename 'tmp)))))))
			  (define x 1)
			  (define y 2)
			  (my-swap! x y)
			  (list x y))`,
			Expected: values.List(values.NewInteger(2), values.NewInteger(1)),
		},
		{
			Name: "rename tmp does not capture user tmp",
			Code: `(begin
			  (import (for-syntax (scheme base)) (for-syntax (scheme cxr)))
			  (define-syntax my-swap!
			    (er-macro-transformer
			      (lambda (form rename compare)
			        (let ((a (cadr form))
			              (b (caddr form)))
			          (list (rename 'let) (list (list (rename 'tmp) a))
			                (list (rename 'set!) a b)
			                (list (rename 'set!) b (rename 'tmp)))))))
			  (define tmp 999)
			  (define x 1)
			  (define y 2)
			  (my-swap! x y)
			  tmp)`,
			Expected: values.NewInteger(999),
		},
		{
			Name: "rename provides hygienic list constructor",
			Code: `(begin
			  (import (for-syntax (scheme base)) (for-syntax (scheme cxr)))
			  (define-syntax make-triple
			    (er-macro-transformer
			      (lambda (form rename compare)
			        (let ((a (cadr form))
			              (b (caddr form))
			              (c (cadddr form)))
			          (list (rename 'list) a b c)))))
			  (make-triple 10 20 30))`,
			Expected: values.List(
				values.NewInteger(10),
				values.NewInteger(20),
				values.NewInteger(30),
			),
		},
		{
			Name: "rename or macro falsy path",
			Code: `(begin
			  (import (for-syntax (scheme base)) (for-syntax (scheme cxr)))
			  (define-syntax my-or
			    (er-macro-transformer
			      (lambda (form rename compare)
			        (let ((a (cadr form))
			              (b (caddr form)))
			          (list (rename 'let) (list (list (rename 'tmp) a))
			                (list (rename 'if) (rename 'tmp) (rename 'tmp) b))))))
			  (my-or #f 42))`,
			Expected: values.NewInteger(42),
		},
		{
			Name: "rename or macro truthy path",
			Code: `(begin
			  (import (for-syntax (scheme base)) (for-syntax (scheme cxr)))
			  (define-syntax my-or
			    (er-macro-transformer
			      (lambda (form rename compare)
			        (let ((a (cadr form))
			              (b (caddr form)))
			          (list (rename 'let) (list (list (rename 'tmp) a))
			                (list (rename 'if) (rename 'tmp) (rename 'tmp) b))))))
			  (my-or 7 42))`,
			Expected: values.NewInteger(7),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			// SetupEngineTest, not RunSchemeCode: the programs now (import ...),
			// and bootstrap.NewNamespaceFrame alone configures no library
			// registry ("load-library: no library registry configured"). The
			// empty MapFS contributes no user libraries; SetupEngineTest chains
			// stdlib.FS behind it, which is where (scheme base) and (scheme cxr)
			// come from.
			env := testhelpers.SetupEngineTest(t, fstest.MapFS{})
			result, err := testhelpers.RunSchemeCodeWithEnv(t, env, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// TestErMacroRenameErrors tests error cases for the ER macro rename closure.
func TestErMacroRenameErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{
			Name: "rename with non-symbol argument",
			Code: `(begin
			  (define-syntax bad-macro
			    (er-macro-transformer
			      (lambda (form rename compare)
			        (rename 42))))
			  (bad-macro))`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
