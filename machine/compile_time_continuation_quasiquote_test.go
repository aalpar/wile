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

// TestCompileQuasiquote tests quasiquote compilation: basic quoting, unquote,
// unquote-splicing, nested quasiquote, and vector quasiquote.
//
// Source: compile_time_continuation_quasiquote.go (compileQuasiquoteDatum,
// expandQuasiquote, expandQuasiquoteVector, quasiquoteNeedsRuntime,
// CompileUnquote, CompileUnquoteSplicing).
func TestCompileQuasiquote(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Basic quasiquote (no unquotes, compiles as literal)
		{
			Name: "all-literal quasiquote",
			Code: "`(a b c)",
			Expected: values.List(
				values.NewSymbol("a"),
				values.NewSymbol("b"),
				values.NewSymbol("c"),
			),
		},

		// Unquote
		{
			Name: "unquote expression",
			Code: "`(a ,(+ 1 2) c)",
			Expected: values.List(
				values.NewSymbol("a"),
				values.NewInteger(3),
				values.NewSymbol("c"),
			),
		},
		{
			Name: "unquote variable",
			Code: "(let ((x 42)) `(result ,x))",
			Expected: values.List(
				values.NewSymbol("result"),
				values.NewInteger(42),
			),
		},

		// Unquote-splicing
		{
			Name: "unquote-splicing list",
			Code: "`(a ,@(list 1 2) c)",
			Expected: values.List(
				values.NewSymbol("a"),
				values.NewInteger(1),
				values.NewInteger(2),
				values.NewSymbol("c"),
			),
		},
		{
			Name: "unquote-splicing empty list",
			Code: "`(a ,@'() b)",
			Expected: values.List(
				values.NewSymbol("a"),
				values.NewSymbol("b"),
			),
		},

		// Nested quasiquote preserves inner quasiquote
		{
			Name: "nested quasiquote preserves inner",
			Code: "`(a `(b ,(+ 1 2)))",
			Expected: values.List(
				values.NewSymbol("a"),
				values.List(
					values.NewSymbol("quasiquote"),
					values.List(
						values.NewSymbol("b"),
						values.List(
							values.NewSymbol("unquote"),
							values.List(
								values.NewSymbol("+"),
								values.NewInteger(1),
								values.NewInteger(2),
							),
						),
					),
				),
			),
		},

		// Vector quasiquote
		{
			Name:     "vector quasiquote with unquote",
			Code:     "`#(1 ,(+ 1 1) 3)",
			Expected: values.NewVector(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},
		{
			Name:     "vector quasiquote all literal",
			Code:     "`#(1 2 3)",
			Expected: values.NewVector(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},

		// Single element
		{
			Name:     "quasiquote single integer",
			Code:     "`42",
			Expected: values.NewInteger(42),
		},
		{
			Name:     "quasiquote single symbol",
			Code:     "`foo",
			Expected: values.NewSymbol("foo"),
		},

		// Dotted pair quasiquote
		{
			Name: "quasiquote dotted pair with unquote",
			Code: "(let ((x 2)) `(1 . ,x))",
			Expected: values.NewCons(
				values.NewInteger(1),
				values.NewInteger(2),
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

// TestCompileQuasiquoteErrors tests error conditions for unquote outside quasiquote.
func TestCompileQuasiquoteErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "unquote outside quasiquote", Code: `,42`},
		{Name: "unquote-splicing outside quasiquote", Code: `,@(list 1 2)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
