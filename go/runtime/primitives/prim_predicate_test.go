// Copyright 2025 Aaron Alpar
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

package primitives_test

import (
	"testing"

	"wile/values"

	qt "github.com/frankban/quicktest"
)

// ----------------------------------------------------------------------------
// Type Predicate Tests
// ----------------------------------------------------------------------------

func TestTypePredicates(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		// null?
		{
			name: "null? on empty list",
			prog: values.List(values.NewSymbol("null?"),
				values.List(values.NewSymbol("quote"), values.EmptyList)),
			out: values.TrueValue,
		},
		{
			name: "null? on non-empty list",
			prog: values.List(values.NewSymbol("null?"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewInteger(1)))),
			out: values.FalseValue,
		},
		// pair?
		{
			name: "pair? on pair",
			prog: values.List(values.NewSymbol("pair?"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewInteger(1), values.NewInteger(2)))),
			out: values.TrueValue,
		},
		// Note: R7RS says (pair? '()) should be #f, but current implementation returns #t
		// because EmptyList is implemented as a *values.Pair. This test documents actual behavior.
		{
			name: "pair? on empty list",
			prog: values.List(values.NewSymbol("pair?"),
				values.List(values.NewSymbol("quote"), values.EmptyList)),
			out: values.FalseValue,
		},
		// boolean?
		{
			name: "boolean? on true",
			prog: values.List(values.NewSymbol("boolean?"), values.TrueValue),
			out:  values.TrueValue,
		},
		{
			name: "boolean? on false",
			prog: values.List(values.NewSymbol("boolean?"), values.FalseValue),
			out:  values.TrueValue,
		},
		{
			name: "boolean? on number",
			prog: values.List(values.NewSymbol("boolean?"), values.NewInteger(1)),
			out:  values.FalseValue,
		},
		// number?
		{
			name: "number? on integer",
			prog: values.List(values.NewSymbol("number?"), values.NewInteger(42)),
			out:  values.TrueValue,
		},
		{
			name: "number? on float",
			prog: values.List(values.NewSymbol("number?"), values.NewFloat(3.14)),
			out:  values.TrueValue,
		},
		{
			name: "number? on symbol",
			prog: values.List(values.NewSymbol("number?"),
				values.List(values.NewSymbol("quote"), values.NewSymbol("foo"))),
			out: values.FalseValue,
		},
		// integer?
		{
			name: "integer? on integer",
			prog: values.List(values.NewSymbol("integer?"), values.NewInteger(42)),
			out:  values.TrueValue,
		},
		{
			name: "integer? on float",
			prog: values.List(values.NewSymbol("integer?"), values.NewFloat(3.14)),
			out:  values.FalseValue,
		},
		// symbol?
		{
			name: "symbol? on symbol",
			prog: values.List(values.NewSymbol("symbol?"),
				values.List(values.NewSymbol("quote"), values.NewSymbol("foo"))),
			out: values.TrueValue,
		},
		{
			name: "symbol? on string",
			prog: values.List(values.NewSymbol("symbol?"), values.NewString("foo")),
			out:  values.FalseValue,
		},
		// string?
		{
			name: "string? on string",
			prog: values.List(values.NewSymbol("string?"), values.NewString("hello")),
			out:  values.TrueValue,
		},
		{
			name: "string? on symbol",
			prog: values.List(values.NewSymbol("string?"),
				values.List(values.NewSymbol("quote"), values.NewSymbol("foo"))),
			out: values.FalseValue,
		},
		// list?
		{
			name: "list? on proper list",
			prog: values.List(values.NewSymbol("list?"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewInteger(1), values.NewInteger(2)))),
			out: values.TrueValue,
		},
		{
			name: "list? on empty list",
			prog: values.List(values.NewSymbol("list?"),
				values.List(values.NewSymbol("quote"), values.EmptyList)),
			out: values.TrueValue,
		},
		// procedure?
		{
			name: "procedure? on lambda",
			prog: values.List(values.NewSymbol("procedure?"),
				values.List(values.NewSymbol("lambda"),
					values.List(values.NewSymbol("x")),
					values.NewSymbol("x"))),
			out: values.TrueValue,
		},
		{
			name: "procedure? on number",
			prog: values.List(values.NewSymbol("procedure?"), values.NewInteger(5)),
			out:  values.FalseValue,
		},
		// vector?
		{
			name: "vector? on vector",
			prog: values.List(values.NewSymbol("vector?"),
				values.List(values.NewSymbol("vector"), values.NewInteger(1), values.NewInteger(2))),
			out: values.TrueValue,
		},
		{
			name: "vector? on list",
			prog: values.List(values.NewSymbol("vector?"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewInteger(1), values.NewInteger(2)))),
			out: values.FalseValue,
		},
		// not
		{
			name: "not on false",
			prog: values.List(values.NewSymbol("not"), values.FalseValue),
			out:  values.TrueValue,
		},
		{
			name: "not on true",
			prog: values.List(values.NewSymbol("not"), values.TrueValue),
			out:  values.FalseValue,
		},
		{
			name: "not on non-false value",
			prog: values.List(values.NewSymbol("not"), values.NewInteger(1)),
			out:  values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

// ----------------------------------------------------------------------------
// Numeric Predicate Tests
// ----------------------------------------------------------------------------

func TestNumericPredicates(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		// zero?
		{
			name: "zero? on zero",
			prog: values.List(values.NewSymbol("zero?"), values.NewInteger(0)),
			out:  values.TrueValue,
		},
		{
			name: "zero? on non-zero",
			prog: values.List(values.NewSymbol("zero?"), values.NewInteger(5)),
			out:  values.FalseValue,
		},
		// positive?
		{
			name: "positive? on positive",
			prog: values.List(values.NewSymbol("positive?"), values.NewInteger(5)),
			out:  values.TrueValue,
		},
		{
			name: "positive? on negative",
			prog: values.List(values.NewSymbol("positive?"), values.NewInteger(-5)),
			out:  values.FalseValue,
		},
		{
			name: "positive? on zero",
			prog: values.List(values.NewSymbol("positive?"), values.NewInteger(0)),
			out:  values.FalseValue,
		},
		// negative?
		{
			name: "negative? on negative",
			prog: values.List(values.NewSymbol("negative?"), values.NewInteger(-5)),
			out:  values.TrueValue,
		},
		{
			name: "negative? on positive",
			prog: values.List(values.NewSymbol("negative?"), values.NewInteger(5)),
			out:  values.FalseValue,
		},
		// odd?
		{
			name: "odd? on odd",
			prog: values.List(values.NewSymbol("odd?"), values.NewInteger(5)),
			out:  values.TrueValue,
		},
		{
			name: "odd? on even",
			prog: values.List(values.NewSymbol("odd?"), values.NewInteger(4)),
			out:  values.FalseValue,
		},
		// even?
		{
			name: "even? on even",
			prog: values.List(values.NewSymbol("even?"), values.NewInteger(4)),
			out:  values.TrueValue,
		},
		{
			name: "even? on odd",
			prog: values.List(values.NewSymbol("even?"), values.NewInteger(5)),
			out:  values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}
