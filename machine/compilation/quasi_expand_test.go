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

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

func TestQuasiExpand(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Simple quasiquote (no unquotes)
		{
			Name: "simple quasiquote list",
			Code: "`(a b c)",
			Expected: values.List(
				values.NewSymbol("a"),
				values.NewSymbol("b"),
				values.NewSymbol("c"),
			),
		},
		// Unquote
		{
			Name: "unquote in list",
			Code: "`(a ,(+ 1 2) c)",
			Expected: values.List(
				values.NewSymbol("a"),
				values.NewInteger(3),
				values.NewSymbol("c"),
			),
		},
		// Unquote-splicing
		{
			Name: "unquote-splicing in list",
			Code: "`(a ,@(list 1 2) c)",
			Expected: values.List(
				values.NewSymbol("a"),
				values.NewInteger(1),
				values.NewInteger(2),
				values.NewSymbol("c"),
			),
		},
		// Vector quasiquote
		{
			Name:     "vector quasiquote with unquote",
			Code:     "`#(1 ,(+ 1 1) 3)",
			Expected: values.NewVector(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},
		// Deep unquote (nested in sublist)
		{
			Name: "deep unquote in sublist",
			Code: "`(a (b ,(+ 1 2)) c)",
			Expected: values.List(
				values.NewSymbol("a"),
				values.List(values.NewSymbol("b"), values.NewInteger(3)),
				values.NewSymbol("c"),
			),
		},
		// Multiple unquotes
		{
			Name: "multiple unquotes",
			Code: "`(,(+ 1 0) ,(+ 2 0) ,(+ 3 0))",
			Expected: values.List(
				values.NewInteger(1),
				values.NewInteger(2),
				values.NewInteger(3),
			),
		},
		// Quasiquote with no unquotes is like quote
		{
			Name:     "quasiquote number",
			Code:     "`42",
			Expected: values.NewInteger(42),
		},
		{
			Name:     "quasiquote symbol",
			Code:     "`hello",
			Expected: values.NewSymbol("hello"),
		},
		// Unquote-splicing at beginning
		{
			Name: "unquote-splicing at start",
			Code: "`(,@(list 1 2) 3)",
			Expected: values.List(
				values.NewInteger(1),
				values.NewInteger(2),
				values.NewInteger(3),
			),
		},
		// Unquote-splicing at end
		{
			Name: "unquote-splicing at end",
			Code: "`(1 ,@(list 2 3))",
			Expected: values.List(
				values.NewInteger(1),
				values.NewInteger(2),
				values.NewInteger(3),
			),
		},
		// Empty quasiquote list
		{
			Name:     "quasiquote empty list",
			Code:     "`()",
			Expected: values.EmptyList,
		},
		// Unquote with variable
		{
			Name: "unquote with let binding",
			Code: `(let ((x 10)) ` + "`(a ,x c))",
			Expected: values.List(
				values.NewSymbol("a"),
				values.NewInteger(10),
				values.NewSymbol("c"),
			),
		},
		// Vector with splicing
		{
			Name: "vector quasiquote with splicing",
			Code: "`#(1 ,@(list 2 3) 4)",
			Expected: values.NewVector(
				values.NewInteger(1),
				values.NewInteger(2),
				values.NewInteger(3),
				values.NewInteger(4),
			),
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
