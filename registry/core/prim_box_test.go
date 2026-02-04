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

	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

// =============================================================================
// box Tests
// =============================================================================

func TestBox(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "box creates a box",
			code: "(box? (box 42))",
			out:  values.TrueValue,
		},
		{
			name: "box with string",
			code: "(unbox (box \"hello\"))",
			out:  values.NewString("hello"),
		},
		{
			name: "box with boolean",
			code: "(unbox (box #t))",
			out:  values.TrueValue,
		},
		{
			name: "box with empty list",
			code: "(unbox (box '()))",
			out:  values.EmptyList,
		},
		{
			name: "nested boxes",
			code: "(unbox (unbox (box (box 1))))",
			out:  values.NewInteger(1),
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

// =============================================================================
// box? Tests
// =============================================================================

func TestBoxQ(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "box? returns true for box",
			code: "(box? (box 42))",
			out:  values.TrueValue,
		},
		{
			name: "box? returns false for integer",
			code: "(box? 42)",
			out:  values.FalseValue,
		},
		{
			name: "box? returns false for string",
			code: "(box? \"hello\")",
			out:  values.FalseValue,
		},
		{
			name: "box? returns false for pair",
			code: "(box? '(1 2))",
			out:  values.FalseValue,
		},
		{
			name: "box? returns false for boolean",
			code: "(box? #t)",
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

// =============================================================================
// unbox Tests
// =============================================================================

func TestUnbox(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "unbox extracts integer",
			code: "(unbox (box 42))",
			out:  values.NewInteger(42),
		},
		{
			name: "unbox extracts symbol",
			code: "(unbox (box 'foo))",
			out:  values.NewSymbol("foo"),
		},
		{
			name: "unbox extracts list",
			code: "(unbox (box '(1 2 3)))",
			out:  values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
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

func TestUnboxErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{
			name: "unbox non-box integer",
			code: "(unbox 42)",
		},
		{
			name: "unbox non-box string",
			code: "(unbox \"hello\")",
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// =============================================================================
// set-box! Tests
// =============================================================================

func TestSetBox(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "set-box! mutates box",
			code: "(let ((b (box 1))) (set-box! b 2) (unbox b))",
			out:  values.NewInteger(2),
		},
		{
			name: "set-box! returns void",
			code: "(void? (set-box! (box 1) 2))",
			out:  values.TrueValue,
		},
		{
			name: "set-box! with different type",
			code: "(let ((b (box 42))) (set-box! b \"hello\") (unbox b))",
			out:  values.NewString("hello"),
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

func TestSetBoxErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{
			name: "set-box! non-box integer",
			code: "(set-box! 42 1)",
		},
		{
			name: "set-box! non-box string",
			code: "(set-box! \"hello\" 1)",
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// =============================================================================
// equal? on boxes
// =============================================================================

func TestBoxEqual(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "equal? on boxes with equal contents",
			code: "(equal? (box 42) (box 42))",
			out:  values.TrueValue,
		},
		{
			name: "equal? on boxes with different contents",
			code: "(equal? (box 1) (box 2))",
			out:  values.FalseValue,
		},
		{
			name: "equal? on nested boxes",
			code: "(equal? (box (box 1)) (box (box 1)))",
			out:  values.TrueValue,
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
