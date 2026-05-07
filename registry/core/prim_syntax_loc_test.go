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

package core_test

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"
)

func TestSyntaxLocationAccessors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Syntax values with no source context (datum->syntax #f ...) return #f
		{Name: "source/no-context", Code: `(syntax-source (datum->syntax #f 'x))`, Expected: values.FalseValue},
		{Name: "line/no-context", Code: `(syntax-line (datum->syntax #f 'x))`, Expected: values.FalseValue},
		{Name: "column/no-context", Code: `(syntax-column (datum->syntax #f 'x))`, Expected: values.FalseValue},
		{Name: "position/no-context", Code: `(syntax-position (datum->syntax #f 'x))`, Expected: values.FalseValue},
		{Name: "span/no-context", Code: `(syntax-span (datum->syntax #f 'x))`, Expected: values.FalseValue},

		// The empty list is a syntax value with no source context (Chez
		// conformance — `(equal? (syntax ()) '()) → #t`). Each accessor
		// returns #f rather than erroring.
		{Name: "source/empty-list", Code: `(syntax-source '())`, Expected: values.FalseValue},
		{Name: "line/empty-list", Code: `(syntax-line '())`, Expected: values.FalseValue},
		{Name: "column/empty-list", Code: `(syntax-column '())`, Expected: values.FalseValue},
		{Name: "position/empty-list", Code: `(syntax-position '())`, Expected: values.FalseValue},
		{Name: "span/empty-list", Code: `(syntax-span '())`, Expected: values.FalseValue},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestSyntaxLocationAccessorsErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		// Non-syntax values raise errors. Note: '() is *not* a non-syntax
		// value — after the empty-list duality merge it satisfies
		// SyntaxValue (matching Chez's `(equal? (syntax ()) '()) → #t`).
		// Tests that depend on the Chez-conformant behavior — that
		// (syntax-position '()) returns #f rather than erroring — live in
		// TestSyntaxLocationAccessors / "*/no-context" cases.
		{Name: "source/non-syntax", Code: `(syntax-source 42)`},
		{Name: "line/non-syntax", Code: `(syntax-line "hello")`},
		{Name: "column/non-syntax", Code: `(syntax-column #t)`},
		{Name: "position/non-syntax", Code: `(syntax-position 99.5)`},
		{Name: "span/non-syntax", Code: `(syntax-span 'foo)`},
		{Name: "->list/non-syntax", Code: `(syntax->list 42)`},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// TestEmptyListSyntaxValueChezConformance pins the Chez-conformant
// behavior introduced by the empty-list duality merge:
// (equal? (syntax ()) '()) returns #t. The previous strict pointer-type
// EqualTo on the now-deleted *syntaxEmptyListType returned #f, contrary
// to Chez Scheme 10.3.0 (verified in the REPL).
func TestEmptyListSyntaxValueChezConformance(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "equal/syntax-empty-vs-empty",
			Code:     `(equal? (syntax ()) '())`,
			Expected: values.TrueValue,
		},
		{
			Name:     "equal/empty-vs-syntax-empty (reverse)",
			Code:     `(equal? '() (syntax ()))`,
			Expected: values.TrueValue,
		},
		{
			Name:     "null?/syntax-empty",
			Code:     `(null? (syntax ()))`,
			Expected: values.TrueValue,
		},
		{
			Name:     "syntax->datum/empty",
			Code:     `(equal? (syntax->datum (syntax ())) '())`,
			Expected: values.TrueValue,
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

func TestSyntaxToList(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// datum->syntax on a list creates a SyntaxPair chain
		{Name: "proper-list/length", Code: `
			(length (syntax->list (datum->syntax #f '(a b c))))`,
			Expected: values.NewInteger(3)},

		// Empty syntax list
		{Name: "empty-list", Code: `
			(null? (syntax->list (datum->syntax #f '())))`,
			Expected: values.TrueValue},

		// Elements are syntax objects (identifiers for symbols)
		{Name: "elements-are-identifiers", Code: `
			(identifier? (car (syntax->list (datum->syntax #f '(a b c)))))`,
			Expected: values.TrueValue},

		// Non-list syntax returns #f (syntax object but not a list)
		{Name: "non-list-syntax", Code: `
			(syntax->list (datum->syntax #f 42))`,
			Expected: values.FalseValue},

		// Improper list returns #f
		{Name: "improper-list", Code: `
			(syntax->list (datum->syntax #f '(a . b)))`,
			Expected: values.FalseValue},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}
