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

// TestErMacroCompare tests the ER macro compare closure via Scheme-level tests.
// The compare closure checks whether two identifiers resolve to the same binding,
// enabling literal matching in ER macros.
//
// Every program declares (for-syntax (scheme base)) and (for-syntax (scheme cxr)).
// An er-macro-transformer body is PROCEDURAL: it runs at phase 1, and since the
// sealed base stopped being ambient nothing reaches phase 1 that the program did
// not import. cadr/caddr/cadddr/cddr are bootstrap Scheme definitions with no
// phase-1 slot of their own, so they are exactly the names that go missing; cxr
// is a second import because caddr and cadddr live in cxr.sld, not base.sld. A
// syntax-rules macro needs no import — its template expands into the use site,
// which is phase 0.
func TestErMacroCompare(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "compare renamed with same symbol returns true",
			Code: `(begin
			  (import (for-syntax (scheme base)) (for-syntax (scheme cxr)))
			  (define-syntax literal-check
			    (er-macro-transformer
			      (lambda (form rename compare)
			        (if (compare (cadr form) (rename 'magic))
			            (list (rename 'quote) 'found-magic)
			            (list (rename 'quote) 'not-magic)))))
			  (literal-check magic))`,
			Expected: values.NewSymbol("found-magic"),
		},
		{
			Name: "compare with different symbol returns false",
			Code: `(begin
			  (import (for-syntax (scheme base)) (for-syntax (scheme cxr)))
			  (define-syntax literal-check
			    (er-macro-transformer
			      (lambda (form rename compare)
			        (if (compare (cadr form) (rename 'magic))
			            (list (rename 'quote) 'found-magic)
			            (list (rename 'quote) 'not-magic)))))
			  (literal-check other))`,
			Expected: values.NewSymbol("not-magic"),
		},
		{
			Name: "compare distinguishes different keywords",
			Code: `(begin
			  (import (for-syntax (scheme base)) (for-syntax (scheme cxr)))
			  (define-syntax kw-check
			    (er-macro-transformer
			      (lambda (form rename compare)
			        (let ((kw (cadr form)))
			          (cond
			            ((compare kw (rename 'alpha))
			             (list (rename 'quote) 'found-alpha))
			            ((compare kw (rename 'beta))
			             (list (rename 'quote) 'found-beta))
			            (else
			             (list (rename 'quote) 'unknown)))))))
			  (kw-check beta))`,
			Expected: values.NewSymbol("found-beta"),
		},
		{
			Name: "compare unknown keyword falls through",
			Code: `(begin
			  (import (for-syntax (scheme base)) (for-syntax (scheme cxr)))
			  (define-syntax kw-check
			    (er-macro-transformer
			      (lambda (form rename compare)
			        (let ((kw (cadr form)))
			          (cond
			            ((compare kw (rename 'alpha))
			             (list (rename 'quote) 'found-alpha))
			            ((compare kw (rename 'beta))
			             (list (rename 'quote) 'found-beta))
			            (else
			             (list (rename 'quote) 'unknown)))))))
			  (kw-check gamma))`,
			Expected: values.NewSymbol("unknown"),
		},
		{
			Name: "compare in when macro true test",
			Code: `(begin
			  (import (for-syntax (scheme base)) (for-syntax (scheme cxr)))
			  (define-syntax my-when
			    (er-macro-transformer
			      (lambda (form rename compare)
			        (let ((test (cadr form))
			              (body (cddr form)))
			          (list (rename 'if) test
			                (cons (rename 'begin) body)
			                #f)))))
			  (my-when #t 42))`,
			Expected: values.NewInteger(42),
		},
		{
			Name: "compare in when macro false test",
			Code: `(begin
			  (import (for-syntax (scheme base)) (for-syntax (scheme cxr)))
			  (define-syntax my-when
			    (er-macro-transformer
			      (lambda (form rename compare)
			        (let ((test (cadr form))
			              (body (cddr form)))
			          (list (rename 'if) test
			                (cons (rename 'begin) body)
			                #f)))))
			  (my-when #f 42))`,
			Expected: values.FalseValue,
		},
		{
			// Over-match fix (option B): two DISTINCT top-level defines holding
			// the same value are NOT the same identifier. The former same-value
			// fallback matched them; origin-keyed SameBinding gives each its own
			// (nil) root — distinct objects, so compare correctly reports diff.
			Name: "compare distinct defines of one value returns false",
			Code: `(begin
			  (import (for-syntax (scheme base)) (for-syntax (scheme cxr)))
			  (define a car)
			  (define b car)
			  (define-syntax check-ab
			    (er-macro-transformer
			      (lambda (form rename compare)
			        (if (compare (cadr form) (caddr form))
			            (list (rename 'quote) 'same)
			            (list (rename 'quote) 'diff)))))
			  (check-ab a b))`,
			Expected: values.NewSymbol("diff"),
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

// TestErMacroCompareErrors tests error cases for the ER macro compare closure.
func TestErMacroCompareErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{
			Name: "compare with non-symbol argument",
			Code: `(begin
			  (define-syntax bad-compare
			    (er-macro-transformer
			      (lambda (form rename compare)
			        (compare 42 (rename 'x)))))
			  (bad-compare))`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
