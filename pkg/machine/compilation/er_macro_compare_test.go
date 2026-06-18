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

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// TestErMacroCompare tests the ER macro compare closure via Scheme-level tests.
// The compare closure checks whether two identifiers resolve to the same binding,
// enabling literal matching in ER macros.
func TestErMacroCompare(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "compare renamed with same symbol returns true",
			Code: `(begin
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
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
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
