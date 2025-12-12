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
	"fmt"
	"os"
	"testing"

	qt "github.com/frankban/quicktest"
	"wile/values"
)

func TestMinMaxWithRationals(t *testing.T) {
	tests := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{
			name:     "min with rational",
			code:     `(min 1/2 3/4 1/4)`,
			expected: values.NewRational(1, 4),
		},
		{
			name:     "max with rational",
			code:     `(max 1/2 3/4 1/4)`,
			expected: values.NewRational(3, 4),
		},
		{
			name:     "min with mixed int and rational",
			code:     `(min 1 1/2)`,
			expected: values.NewRational(1, 2),
		},
		{
			name:     "max with mixed int and rational",
			code:     `(max 1 1/2)`,
			expected: values.NewInteger(1),
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestRationalQExtended(t *testing.T) {
	tests := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{
			name:     "rational? on positive integer",
			code:     `(rational? 42)`,
			expected: values.TrueValue,
		},
		{
			name:     "rational? on negative integer",
			code:     `(rational? -42)`,
			expected: values.TrueValue,
		},
		{
			name:     "rational? on positive float",
			code:     `(rational? 3.14)`,
			expected: values.TrueValue,
		},
		{
			name:     "rational? on negative float",
			code:     `(rational? -3.14)`,
			expected: values.TrueValue,
		},
		{
			name:     "rational? on zero",
			code:     `(rational? 0)`,
			expected: values.TrueValue,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.Equals, tc.expected)
		})
	}
}

func TestStringAppendExtended(t *testing.T) {
	tests := []struct {
		name     string
		code     string
		expected string
	}{
		{
			name:     "string-append five strings",
			code:     `(string-append "a" "b" "c" "d" "e")`,
			expected: "abcde",
		},
		{
			name:     "string-append two strings",
			code:     `(string-append "hello" "world")`,
			expected: "helloworld",
		},
		{
			name:     "string-append with space",
			code:     `(string-append "hello" " " "world")`,
			expected: "hello world",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			s, ok := result.(*values.String)
			qt.Assert(t, ok, qt.IsTrue)
			qt.Assert(t, s.Value, qt.Equals, tc.expected)
		})
	}
}

func TestReadSyntaxFromStringPort(t *testing.T) {
	tests := []struct {
		name string
		code string
	}{
		{
			name: "read-syntax from string port",
			code: `(let ((p (open-input-string "(+ 1 2)")))
				(read-syntax p))`,
		},
		{
			name: "read-syntax simple symbol",
			code: `(let ((p (open-input-string "hello")))
				(read-syntax p))`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.IsNotNil)
		})
	}
}

func TestReadTokenFromStringPort(t *testing.T) {
	tests := []struct {
		name string
		code string
	}{
		{
			name: "read-token identifier",
			code: `(let ((p (open-input-string "hello")))
				(read-token p))`,
		},
		{
			name: "read-token number",
			code: `(let ((p (open-input-string "42")))
				(read-token p))`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.IsNotNil)
		})
	}
}

func TestClosePortExtended(t *testing.T) {
	tests := []struct {
		name string
		code string
	}{
		{
			name: "close-port on string input port",
			code: `(let ((p (open-input-string "hello")))
				(close-port p)
				#t)`,
		},
		{
			name: "close-port on string output port",
			code: `(let ((p (open-output-string)))
				(close-port p)
				#t)`,
		},
		{
			name: "close-port on bytevector input port",
			code: `(let ((p (open-input-bytevector (bytevector 1 2 3))))
				(close-port p)
				#t)`,
		},
		{
			name: "close-port on bytevector output port",
			code: `(let ((p (open-output-bytevector)))
				(close-port p)
				#t)`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.Equals, values.TrueValue)
		})
	}
}

func TestEvalExtended(t *testing.T) {
	tests := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{
			name:     "eval subtraction",
			code:     `(eval '(- 10 3) (interaction-environment))`,
			expected: values.NewInteger(7),
		},
		{
			name:     "eval nested expression",
			code:     `(eval '(+ (* 2 3) 4) (interaction-environment))`,
			expected: values.NewInteger(10),
		},
		{
			name:     "eval if expression",
			code:     `(eval '(if (> 5 3) 1 2) (interaction-environment))`,
			expected: values.NewInteger(1),
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestAddWithComplexAndRationals(t *testing.T) {
	tests := []struct {
		name string
		code string
	}{
		{
			name: "add with complex",
			code: `(+ 1+2i 3+4i)`,
		},
		{
			name: "add with rational",
			code: `(+ 1/2 1/3)`,
		},
		{
			name: "add mixed types",
			code: `(+ 1 2.0 3/4)`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.IsNotNil)
		})
	}
}

func TestListOperationsExtended(t *testing.T) {
	tests := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{
			name:     "list-ref first",
			code:     `(list-ref '(a b c d) 0)`,
			expected: nil, // just check no error
		},
		{
			name:     "list-ref middle",
			code:     `(list-ref '(a b c d) 2)`,
			expected: nil,
		},
		{
			name:     "list-tail from start",
			code:     `(list-tail '(a b c d) 0)`,
			expected: nil,
		},
		{
			name:     "list-tail from middle",
			code:     `(list-tail '(a b c d) 2)`,
			expected: nil,
		},
		{
			name:     "list->string",
			code:     `(list->string '(#\a #\b #\c))`,
			expected: nil,
		},
		{
			name:     "list->vector",
			code:     `(list->vector '(1 2 3))`,
			expected: nil,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.IsNotNil)
		})
	}
}

func TestDynamicWindBasic(t *testing.T) {
	result, err := runSchemeCode(t, `
		(let ((result '()))
			(dynamic-wind
				(lambda () (set! result (cons 'before result)))
				(lambda () (set! result (cons 'during result)) 42)
				(lambda () (set! result (cons 'after result))))
			result)
	`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, qt.IsNotNil)
}

func TestCallWithValuesExtended(t *testing.T) {
	tests := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{
			name:     "call-with-values single value",
			code:     `(call-with-values (lambda () 42) (lambda (x) x))`,
			expected: values.NewInteger(42),
		},
		{
			name:     "call-with-values two values",
			code:     `(call-with-values (lambda () (values 1 2)) (lambda (x y) (+ x y)))`,
			expected: values.NewInteger(3),
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestExactInexactExtended(t *testing.T) {
	tests := []struct {
		name string
		code string
	}{
		{
			name: "exact on rational",
			code: `(exact 1/2)`,
		},
		{
			name: "exact on integer",
			code: `(exact 42)`,
		},
		{
			name: "inexact on integer",
			code: `(inexact 42)`,
		},
		{
			name: "inexact on rational",
			code: `(inexact 1/2)`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.IsNotNil)
		})
	}
}

func TestMakeRectangularExtended(t *testing.T) {
	tests := []struct {
		name string
		code string
	}{
		{
			name: "make-rectangular with integers",
			code: `(make-rectangular 3 4)`,
		},
		{
			name: "make-rectangular with floats",
			code: `(make-rectangular 3.0 4.0)`,
		},
		{
			name: "make-rectangular with rationals",
			code: `(make-rectangular 1/2 3/4)`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.IsNotNil)
		})
	}
}

func TestStringAppendWithNonString(t *testing.T) {
	// Test error path when non-string is passed
	_, err := runSchemeCode(t, `(string-append "hello" 42)`)
	qt.Assert(t, err, qt.IsNotNil)
}

func TestMinMaxWithFloats(t *testing.T) {
	// Test min/max with floats for coverage
	result, err := runSchemeCode(t, `(min 1.0 2.0 0.5)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, qt.IsNotNil)

	result2, err2 := runSchemeCode(t, `(max 1.0 2.0 0.5)`)
	qt.Assert(t, err2, qt.IsNil)
	qt.Assert(t, result2, qt.IsNotNil)
}

func TestRationalQWithMoreTypes(t *testing.T) {
	tests := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{
			name:     "rational? on symbol",
			code:     `(rational? 'hello)`,
			expected: values.FalseValue,
		},
		{
			name:     "rational? on string",
			code:     `(rational? "hello")`,
			expected: values.FalseValue,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.Equals, tc.expected)
		})
	}
}

func TestBytevectorU8RefSet(t *testing.T) {
	tests := []struct {
		name string
		code string
	}{
		{
			name: "bytevector-u8-ref",
			code: `(let ((bv (bytevector 1 2 3))) (bytevector-u8-ref bv 1))`,
		},
		{
			name: "bytevector-u8-set!",
			code: `(let ((bv (bytevector 1 2 3))) (bytevector-u8-set! bv 1 99) (bytevector-u8-ref bv 1))`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.IsNotNil)
		})
	}
}

func TestDenominatorNumeratorExtended(t *testing.T) {
	tests := []struct {
		name string
		code string
	}{
		{
			name: "denominator of float",
			code: `(denominator 2.5)`,
		},
		{
			name: "numerator of float",
			code: `(numerator 2.5)`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.IsNotNil)
		})
	}
}

func TestDivisionWithRationals(t *testing.T) {
	tests := []struct {
		name string
		code string
	}{
		{
			name: "divide rationals",
			code: `(/ 1/2 1/4)`,
		},
		{
			name: "divide integer by rational",
			code: `(/ 2 1/2)`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.IsNotNil)
		})
	}
}

func TestMagnitudeExtended(t *testing.T) {
	tests := []struct {
		name string
		code string
	}{
		{
			name: "magnitude of rational",
			code: `(magnitude 3/4)`,
		},
		{
			name: "magnitude of negative",
			code: `(magnitude -5)`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.IsNotNil)
		})
	}
}

func TestGcdLcmExtended(t *testing.T) {
	tests := []struct {
		name string
		code string
	}{
		{
			name: "gcd with rationals",
			code: `(gcd 6 9 12)`,
		},
		{
			name: "lcm with rationals",
			code: `(lcm 2 3 4)`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.IsNotNil)
		})
	}
}

func TestExactIntegerSqrtMore(t *testing.T) {
	tests := []struct {
		name string
		code string
	}{
		{
			name: "exact-integer-sqrt perfect square",
			code: `(exact-integer-sqrt 16)`,
		},
		{
			name: "exact-integer-sqrt non-perfect",
			code: `(exact-integer-sqrt 17)`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.IsNotNil)
		})
	}
}

func TestNewlineExtended(t *testing.T) {
	result, err := runSchemeCode(t, `
		(let ((p (open-output-string)))
			(newline p)
			(newline p)
			(get-output-string p))
	`)
	qt.Assert(t, err, qt.IsNil)
	s, ok := result.(*values.String)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, s.Value, qt.Equals, "\n\n")
}

func TestFloorDivQuotientRemainder(t *testing.T) {
	tests := []struct {
		name string
		code string
	}{
		{
			name: "floor/ basic",
			code: `(floor/ 10 3)`,
		},
		{
			name: "floor-quotient",
			code: `(floor-quotient 10 3)`,
		},
		{
			name: "floor-remainder",
			code: `(floor-remainder 10 3)`,
		},
		{
			name: "truncate/",
			code: `(truncate/ 10 3)`,
		},
		{
			name: "truncate-quotient",
			code: `(truncate-quotient 10 3)`,
		},
		{
			name: "truncate-remainder",
			code: `(truncate-remainder 10 3)`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.IsNotNil)
		})
	}
}

func TestApplyExtended(t *testing.T) {
	tests := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{
			name:     "apply with prefix args",
			code:     `(apply + 1 2 '(3 4))`,
			expected: values.NewInteger(10),
		},
		{
			name:     "apply with empty list",
			code:     `(apply + '())`,
			expected: values.NewInteger(0),
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestAppendExtended(t *testing.T) {
	tests := []struct {
		name string
		code string
	}{
		{
			name: "append three lists",
			code: `(append '(a b) '(c d) '(e f))`,
		},
		{
			name: "append with empty lists",
			code: `(append '() '(a b) '())`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.IsNotNil)
		})
	}
}

func TestAssocAssqAssv(t *testing.T) {
	tests := []struct {
		name string
		code string
	}{
		{
			name: "assoc found",
			code: `(assoc 'b '((a 1) (b 2) (c 3)))`,
		},
		{
			name: "assoc not found",
			code: `(assoc 'd '((a 1) (b 2) (c 3)))`,
		},
		{
			name: "assq found",
			code: `(assq 'b '((a 1) (b 2) (c 3)))`,
		},
		{
			name: "assv found",
			code: `(assv 2 '((1 a) (2 b) (3 c)))`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
		})
	}
}

func TestMemberMemqMemv(t *testing.T) {
	tests := []struct {
		name string
		code string
	}{
		{
			name: "member found",
			code: `(member 2 '(1 2 3))`,
		},
		{
			name: "member not found",
			code: `(member 5 '(1 2 3))`,
		},
		{
			name: "memq found",
			code: `(memq 'b '(a b c))`,
		},
		{
			name: "memv found",
			code: `(memv 2 '(1 2 3))`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
		})
	}
}

// TestClosePortMoreCases tests additional close-port scenarios
func TestClosePortMoreCases(t *testing.T) {
	tests := []struct {
		name string
		code string
	}{
		{
			name: "close string input port",
			code: `(close-port (open-input-string "hello"))`,
		},
		{
			name: "close string output port",
			code: `(close-port (open-output-string))`,
		},
		{
			name: "close bytevector input port",
			code: `(close-port (open-input-bytevector #u8(1 2 3)))`,
		},
		{
			name: "close bytevector output port",
			code: `(close-port (open-output-bytevector))`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
		})
	}
}

// TestMinMaxErrorCases tests error cases for min and max
func TestMinMaxErrorCases(t *testing.T) {
	tests := []struct {
		name string
		code string
	}{
		{
			name: "min with single number",
			code: `(min 42)`,
		},
		{
			name: "max with single number",
			code: `(max 42)`,
		},
		{
			name: "min with rationals",
			code: `(min 1/2 3/4 1/4)`,
		},
		{
			name: "max with rationals",
			code: `(max 1/2 3/4 1/4)`,
		},
		{
			name: "min with mixed types",
			code: `(min 1 2.5 3/2)`,
		},
		{
			name: "max with mixed types",
			code: `(max 1 2.5 3/2)`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
		})
	}
}

// TestRationalQMoreCases tests additional rational? edge cases
func TestRationalQMoreCases(t *testing.T) {
	tests := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{
			name:     "rational? on integer",
			code:     `(rational? 42)`,
			expected: values.TrueValue,
		},
		{
			name:     "rational? on rational",
			code:     `(rational? 3/4)`,
			expected: values.TrueValue,
		},
		{
			name:     "rational? on finite float",
			code:     `(rational? 3.14)`,
			expected: values.TrueValue,
		},
		{
			name:     "rational? on +inf.0",
			code:     `(rational? +inf.0)`,
			expected: values.FalseValue,
		},
		{
			name:     "rational? on -inf.0",
			code:     `(rational? -inf.0)`,
			expected: values.FalseValue,
		},
		{
			name:     "rational? on +nan.0",
			code:     `(rational? +nan.0)`,
			expected: values.FalseValue,
		},
		{
			name:     "rational? on complex with zero imaginary",
			code:     `(rational? (make-rectangular 3.0 0.0))`,
			expected: values.TrueValue,
		},
		{
			name:     "rational? on complex with nonzero imaginary",
			code:     `(rational? 3+4i)`,
			expected: values.FalseValue,
		},
		{
			name:     "rational? on complex with infinite real",
			code:     `(rational? (make-rectangular +inf.0 0.0))`,
			expected: values.FalseValue,
		},
		{
			name:     "rational? on complex with NaN real",
			code:     `(rational? (make-rectangular +nan.0 0.0))`,
			expected: values.FalseValue,
		},
		{
			name:     "rational? on string",
			code:     `(rational? "hello")`,
			expected: values.FalseValue,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

// TestNewlineMoreCases tests newline with different port types
func TestNewlineMoreCases(t *testing.T) {
	tests := []struct {
		name string
		code string
	}{
		{
			name: "newline to string output port",
			code: `(let ((p (open-output-string)))
			         (newline p)
			         (get-output-string p))`,
		},
		{
			name: "newline to bytevector output port",
			code: `(let ((p (open-output-bytevector)))
			         (newline p)
			         (get-output-bytevector p))`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
		})
	}
}

// TestEvalMoreCases tests eval with different environment cases
func TestEvalMoreCases(t *testing.T) {
	tests := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{
			name:     "eval simple addition",
			code:     `(eval '(+ 2 3) (interaction-environment))`,
			expected: values.NewInteger(5),
		},
		{
			name:     "eval with quote",
			code:     `(eval ''foo (interaction-environment))`,
			expected: values.NewSymbol("foo"),
		},
		{
			name:     "eval with list",
			code:     `(eval '(list 1 2 3) (interaction-environment))`,
			expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

// TestRationalizeMoreCases tests rationalize with edge cases
func TestRationalizeMoreCases(t *testing.T) {
	tests := []struct {
		name string
		code string
	}{
		{
			name: "rationalize integer",
			code: `(rationalize 5 0)`,
		},
		{
			name: "rationalize float",
			code: `(rationalize 3.14159 0.001)`,
		},
		{
			name: "rationalize with larger tolerance",
			code: `(rationalize 3.14159 0.5)`,
		},
		{
			name: "rationalize negative",
			code: `(rationalize -3.14159 0.001)`,
		},
		{
			name: "rationalize zero",
			code: `(rationalize 0 0.1)`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
		})
	}
}

// TestCallWithValuesMoreCases tests call-with-values edge cases
func TestCallWithValuesMoreCases(t *testing.T) {
	tests := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{
			name:     "call-with-values returning no values",
			code:     `(call-with-values (lambda () (values)) (lambda () 'done))`,
			expected: values.NewSymbol("done"),
		},
		{
			name:     "call-with-values returning one value",
			code:     `(call-with-values (lambda () 42) (lambda (x) x))`,
			expected: values.NewInteger(42),
		},
		{
			name:     "call-with-values returning three values",
			code:     `(call-with-values (lambda () (values 1 2 3)) (lambda (a b c) (+ a b c)))`,
			expected: values.NewInteger(6),
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

// TestTrigWithRationals tests trig functions with rational numbers
func TestTrigWithRationals(t *testing.T) {
	tests := []struct {
		name string
		code string
	}{
		{
			name: "sin with rational",
			code: `(sin 1/2)`,
		},
		{
			name: "cos with rational",
			code: `(cos 1/2)`,
		},
		{
			name: "tan with rational",
			code: `(tan 1/4)`,
		},
		{
			name: "asin with rational",
			code: `(asin 1/2)`,
		},
		{
			name: "acos with rational",
			code: `(acos 1/2)`,
		},
		{
			name: "atan with rational",
			code: `(atan 1/2)`,
		},
		{
			name: "atan2 with rationals",
			code: `(atan 1/2 3/4)`,
		},
		{
			name: "log with rational",
			code: `(log 1/2)`,
		},
		{
			name: "log with rational base",
			code: `(log 8 2)`,
		},
		{
			name: "exp with rational",
			code: `(exp 1/2)`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
		})
	}
}

// TestExactMoreCases tests exact with more numeric types
func TestExactMoreCases(t *testing.T) {
	tests := []struct {
		name string
		code string
	}{
		{
			name: "exact with integer",
			code: `(exact 42)`,
		},
		{
			name: "exact with rational",
			code: `(exact 3/4)`,
		},
		{
			name: "exact with float",
			code: `(exact 3.5)`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
		})
	}
}

// TestListToStringExtended tests list->string with more cases
func TestListToStringExtended(t *testing.T) {
	tests := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{
			name:     "list->string empty",
			code:     `(list->string '())`,
			expected: values.NewString(""),
		},
		{
			name:     "list->string single char",
			code:     `(list->string '(#\a))`,
			expected: values.NewString("a"),
		},
		{
			name:     "list->string multiple chars",
			code:     `(list->string '(#\h #\e #\l #\l #\o))`,
			expected: values.NewString("hello"),
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

// TestDynamicWindCases tests dynamic-wind behavior
func TestDynamicWindCases(t *testing.T) {
	tests := []struct {
		name string
		code string
	}{
		{
			name: "dynamic-wind simple",
			code: `(let ((x 0))
			         (dynamic-wind
			           (lambda () (set! x (+ x 1)))
			           (lambda () x)
			           (lambda () (set! x (+ x 1)))))`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
		})
	}
}

// TestCloseInputOutputFile tests close-input-port and close-output-port
func TestCloseInputOutputFile(t *testing.T) {
	// Test close-input-port with open-input-file
	tempIn, err := os.CreateTemp("", "test-input-*.txt")
	qt.Assert(t, err, qt.IsNil)
	tempIn.WriteString("test content") //nolint:errcheck
	tempIn.Close()                     //nolint:errcheck
	defer os.Remove(tempIn.Name())     //nolint:errcheck

	code := fmt.Sprintf(`(let ((p (open-input-file "%s")))
	                       (close-input-port p))`, tempIn.Name())
	_, err = runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)

	// Test close-output-port with open-output-file
	tempOut, err := os.CreateTemp("", "test-output-*.txt")
	qt.Assert(t, err, qt.IsNil)
	tempOut.Close()                //nolint:errcheck
	defer os.Remove(tempOut.Name()) //nolint:errcheck

	code = fmt.Sprintf(`(let ((p (open-output-file "%s")))
	                       (close-output-port p))`, tempOut.Name())
	_, err = runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
}
