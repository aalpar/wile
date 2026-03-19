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

package validate_test

import (
	"testing"

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// ============================================================================
// define-syntax
// ============================================================================

func TestDefineSyntax(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "simple macro identity",
			Code: `(begin
				(define-syntax my-id
					(syntax-rules ()
						((_ x) x)))
				(my-id 42))`,
			Expected: values.NewInteger(42),
		},
		{
			Name: "macro with literal keyword",
			Code: `(begin
				(define-syntax my-if
					(syntax-rules (then else)
						((_ test then conseq else alt) (if test conseq alt))))
				(my-if #t then 1 else 2))`,
			Expected: values.NewInteger(1),
		},
		{
			Name: "macro with multiple clauses",
			Code: `(begin
				(define-syntax my-or
					(syntax-rules ()
						((_) #f)
						((_ e) e)
						((_ e1 e2 ...) (let ((t e1)) (if t t (my-or e2 ...))))))
				(my-or #f #f 99))`,
			Expected: values.NewInteger(99),
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

func TestDefineSyntax_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{
			Name: "missing transformer",
			Code: `(define-syntax my-macro)`,
		},
		{
			Name: "keyword not a symbol",
			Code: `(define-syntax 42 (syntax-rules () ((_ x) x)))`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.Code)
		})
	}
}

// ============================================================================
// syntax-rules
// ============================================================================

func TestSyntaxRules(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "empty literals list",
			Code: `(begin
				(define-syntax const
					(syntax-rules ()
						((_ v) v)))
				(const 10))`,
			Expected: values.NewInteger(10),
		},
		{
			Name: "single clause",
			Code: `(begin
				(define-syntax double
					(syntax-rules ()
						((_ x) (+ x x))))
				(double 5))`,
			Expected: values.NewInteger(10),
		},
		{
			Name: "syntax-rules with named literals",
			Code: `(begin
				(define-syntax my-cond
					(syntax-rules (else)
						((_ (else e)) e)
						((_ (t e)) (if t e #f))))
				(my-cond (else 77)))`,
			Expected: values.NewInteger(77),
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

func TestSyntaxRules_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{
			Name: "missing literals list",
			Code: `(define-syntax bad (syntax-rules))`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.Code)
		})
	}
}

// ============================================================================
// import
// ============================================================================

func TestImport_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{
			Name: "import with no import sets",
			Code: `(import)`,
		},
		{
			Name: "import nonexistent library",
			Code: `(import (scheme nonexistent-library-xyz))`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.Code)
		})
	}
}

// ============================================================================
// cond-expand
// ============================================================================

func TestCondExpand(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "cond-expand with else clause",
			Code:     `(cond-expand (else 42))`,
			Expected: values.NewInteger(42),
		},
		{
			Name:     "cond-expand r7rs feature",
			Code:     `(cond-expand (r7rs 100) (else 0))`,
			Expected: values.NewInteger(100),
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
