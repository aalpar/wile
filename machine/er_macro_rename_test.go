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

package machine_test

import (
	"testing"

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// TestErMacroRename tests the ER macro rename closure via Scheme-level tests.
// The rename closure ensures that renamed identifiers resolve to definition-site
// bindings, providing hygiene for ER macros.
func TestErMacroRename(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "rename provides hygienic if",
			Code: `(begin
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
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
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
