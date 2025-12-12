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
// Numeric Type Predicate Tests
// ----------------------------------------------------------------------------

func TestComplexPredicate(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "complex? on integer",
			prog: values.List(values.NewSymbol("complex?"), values.NewInteger(42)),
			out:  values.TrueValue,
		},
		{
			name: "complex? on float",
			prog: values.List(values.NewSymbol("complex?"), values.NewFloat(3.14)),
			out:  values.TrueValue,
		},
		{
			name: "complex? on rational",
			prog: values.List(values.NewSymbol("complex?"), values.NewRational(3, 4)),
			out:  values.TrueValue,
		},
		{
			name: "complex? on complex number",
			prog: values.List(values.NewSymbol("complex?"), values.NewComplexFromParts(2.0, 3.0)),
			out:  values.TrueValue,
		},
		{
			name: "complex? on string",
			prog: values.List(values.NewSymbol("complex?"), values.NewString("hello")),
			out:  values.FalseValue,
		},
		{
			name: "complex? on symbol",
			prog: values.List(values.NewSymbol("complex?"),
				values.List(values.NewSymbol("quote"), values.NewSymbol("foo"))),
			out: values.FalseValue,
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

func TestRealPredicate(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "real? on integer",
			prog: values.List(values.NewSymbol("real?"), values.NewInteger(42)),
			out:  values.TrueValue,
		},
		{
			name: "real? on float",
			prog: values.List(values.NewSymbol("real?"), values.NewFloat(3.14)),
			out:  values.TrueValue,
		},
		{
			name: "real? on rational",
			prog: values.List(values.NewSymbol("real?"), values.NewRational(3, 4)),
			out:  values.TrueValue,
		},
		{
			name: "real? on complex number with imaginary part",
			prog: values.List(values.NewSymbol("real?"), values.NewComplexFromParts(2.0, 3.0)),
			out:  values.FalseValue,
		},
		{
			name: "real? on complex number with zero imaginary part",
			prog: values.List(values.NewSymbol("real?"), values.NewComplexFromParts(2.0, 0.0)),
			out:  values.TrueValue,
		},
		{
			name: "real? on string",
			prog: values.List(values.NewSymbol("real?"), values.NewString("hello")),
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

func TestRationalPredicate(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "rational? on integer",
			prog: values.List(values.NewSymbol("rational?"), values.NewInteger(42)),
			out:  values.TrueValue,
		},
		{
			name: "rational? on rational",
			prog: values.List(values.NewSymbol("rational?"), values.NewRational(3, 4)),
			out:  values.TrueValue,
		},
		{
			name: "rational? on finite float",
			prog: values.List(values.NewSymbol("rational?"), values.NewFloat(3.14)),
			out:  values.TrueValue,
		},
		{
			name: "rational? on complex number",
			prog: values.List(values.NewSymbol("rational?"), values.NewComplexFromParts(2.0, 3.0)),
			out:  values.FalseValue,
		},
		{
			name: "rational? on string",
			prog: values.List(values.NewSymbol("rational?"), values.NewString("hello")),
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
// Features and Time Primitives Tests
// ----------------------------------------------------------------------------

func TestFeatures(t *testing.T) {
	result, err := runSchemeCode(t, "(features)")
	qt.Assert(t, err, qt.IsNil)

	// features should return a list
	_, isPair := result.(*values.Pair)
	isEmpty := values.IsEmptyList(result)
	qt.Assert(t, isPair || isEmpty, qt.IsTrue, qt.Commentf("features should return a list"))
}

func TestJiffiesPerSecond(t *testing.T) {
	result, err := runSchemeCode(t, "(jiffies-per-second)")
	qt.Assert(t, err, qt.IsNil)

	// Should return a positive integer
	intVal, ok := result.(*values.Integer)
	qt.Assert(t, ok, qt.IsTrue, qt.Commentf("jiffies-per-second should return an integer"))
	if ok {
		qt.Assert(t, intVal.Value > 0, qt.IsTrue, qt.Commentf("jiffies-per-second should be positive"))
	}
}

func TestCurrentJiffy(t *testing.T) {
	result, err := runSchemeCode(t, "(current-jiffy)")
	qt.Assert(t, err, qt.IsNil)

	// Should return a number (integer or float)
	switch result.(type) {
	case *values.Integer, *values.Float:
		// success
	default:
		t.Fatalf("current-jiffy should return a number, got %T", result)
	}
}

func TestCurrentSecond(t *testing.T) {
	result, err := runSchemeCode(t, "(current-second)")
	qt.Assert(t, err, qt.IsNil)

	// Should return a number (typically a float representing Unix timestamp)
	switch val := result.(type) {
	case *values.Integer:
		// Integer Unix timestamp is valid
		qt.Assert(t, val.Value > 0, qt.IsTrue, qt.Commentf("current-second should be positive"))
	case *values.Float:
		// Float Unix timestamp is valid
		qt.Assert(t, val.Value > 0, qt.IsTrue, qt.Commentf("current-second should be positive"))
	default:
		t.Fatalf("current-second should return a number, got %T", result)
	}
}

func TestCurrentJiffyMonotonic(t *testing.T) {
	// Test that current-jiffy is monotonically increasing
	code := `
		(begin
			(define j1 (current-jiffy))
			(define j2 (current-jiffy))
			(>= j2 j1))
	`
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestCurrentSecondReasonable(t *testing.T) {
	// Test that current-second returns a reasonable Unix timestamp
	// (greater than Jan 1, 2020 timestamp: 1577836800)
	code := "(> (current-second) 1577836800)"
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

// ----------------------------------------------------------------------------
// Additional Predicate Tests (for completeness)
// ----------------------------------------------------------------------------

func TestNotPredicate(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "not on false",
			code: "(not #f)",
			out:  values.TrueValue,
		},
		{
			name: "not on true",
			code: "(not #t)",
			out:  values.FalseValue,
		},
		{
			name: "not on number",
			code: "(not 1)",
			out:  values.FalseValue,
		},
		{
			name: "not on zero",
			code: "(not 0)",
			out:  values.FalseValue,
		},
		{
			name: "not on empty list",
			code: "(not '())",
			out:  values.FalseValue,
		},
		{
			name: "not on string",
			code: `(not "")`,
			out:  values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestListPredicateWithImproperList(t *testing.T) {
	// Test that list? correctly identifies improper lists
	code := "(list? '(1 . 2))"
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.FalseValue)
}

func TestListPredicateWithProperList(t *testing.T) {
	code := "(list? '(1 2 3))"
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestListPredicateWithEmptyList(t *testing.T) {
	code := "(list? '())"
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestProcedurePredicateOnBuiltin(t *testing.T) {
	// Test procedure? on built-in primitive
	code := "(procedure? +)"
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestProcedurePredicateOnLambda(t *testing.T) {
	code := "(procedure? (lambda (x) x))"
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestProcedurePredicateOnNonProcedure(t *testing.T) {
	code := "(procedure? 5)"
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.FalseValue)
}
