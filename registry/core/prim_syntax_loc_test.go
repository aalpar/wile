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
		// Non-syntax values return #f
		{Name: "source/non-syntax", Code: `(syntax-source 42)`, Expected: values.FalseValue},
		{Name: "line/non-syntax", Code: `(syntax-line "hello")`, Expected: values.FalseValue},
		{Name: "column/non-syntax", Code: `(syntax-column #t)`, Expected: values.FalseValue},
		{Name: "position/non-syntax", Code: `(syntax-position '())`, Expected: values.FalseValue},
		{Name: "span/non-syntax", Code: `(syntax-span 'foo)`, Expected: values.FalseValue},

		// Syntax values with no source context (datum->syntax #f ...) return #f for source
		{Name: "source/no-context", Code: `(syntax-source (datum->syntax #f 'x))`, Expected: values.FalseValue},

		// Line/column/position return integers for syntax with no source context
		// (SourceContext is nil => #f)
		{Name: "line/no-context", Code: `(syntax-line (datum->syntax #f 'x))`, Expected: values.FalseValue},
		{Name: "column/no-context", Code: `(syntax-column (datum->syntax #f 'x))`, Expected: values.FalseValue},
		{Name: "position/no-context", Code: `(syntax-position (datum->syntax #f 'x))`, Expected: values.FalseValue},
		{Name: "span/no-context", Code: `(syntax-span (datum->syntax #f 'x))`, Expected: values.FalseValue},
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
		// Non-syntax returns #f
		{Name: "non-syntax", Code: `(syntax->list 42)`, Expected: values.FalseValue},

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

		// Non-list syntax returns #f
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
