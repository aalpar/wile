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
	"errors"
	"testing"

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

func TestBytevectorQ(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "bytevector? with bytevector",
			prog: values.List(values.NewSymbol("bytevector?"),
				values.List(values.NewSymbol("bytevector"), values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))),
			out: values.TrueValue,
		},
		{
			name: "bytevector? with non-bytevector",
			prog: values.List(values.NewSymbol("bytevector?"),
				values.NewInteger(42)),
			out: values.FalseValue,
		},
		{
			name: "bytevector? with string",
			prog: values.List(values.NewSymbol("bytevector?"),
				values.NewString("hello")),
			out: values.FalseValue,
		},
		{
			name: "bytevector? with empty bytevector",
			prog: values.List(values.NewSymbol("bytevector?"),
				values.List(values.NewSymbol("bytevector"))),
			out: values.TrueValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestMakeBytevector(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "make-bytevector without fill",
			prog: values.List(values.NewSymbol("bytevector-length"),
				values.List(values.NewSymbol("make-bytevector"), values.NewInteger(5))),
			out: values.NewInteger(5),
		},
		{
			name: "make-bytevector with fill - verify length",
			prog: values.List(values.NewSymbol("bytevector-length"),
				values.List(values.NewSymbol("make-bytevector"), values.NewInteger(3), values.NewInteger(42))),
			out: values.NewInteger(3),
		},
		{
			name: "make-bytevector with fill - verify content",
			prog: values.List(values.NewSymbol("bytevector-u8-ref"),
				values.List(values.NewSymbol("make-bytevector"), values.NewInteger(3), values.NewInteger(42)),
				values.NewInteger(0)),
			out: values.NewInteger(42),
		},
		{
			name: "make-bytevector zero length",
			prog: values.List(values.NewSymbol("bytevector-length"),
				values.List(values.NewSymbol("make-bytevector"), values.NewInteger(0))),
			out: values.NewInteger(0),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestBytevector(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "bytevector empty",
			prog: values.List(values.NewSymbol("bytevector-length"),
				values.List(values.NewSymbol("bytevector"))),
			out: values.NewInteger(0),
		},
		{
			name: "bytevector with values",
			prog: values.List(values.NewSymbol("bytevector-length"),
				values.List(values.NewSymbol("bytevector"), values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))),
			out: values.NewInteger(3),
		},
		{
			name: "bytevector access element",
			prog: values.List(values.NewSymbol("bytevector-u8-ref"),
				values.List(values.NewSymbol("bytevector"), values.NewInteger(10), values.NewInteger(20), values.NewInteger(30)),
				values.NewInteger(1)),
			out: values.NewInteger(20),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestBytevectorLength(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "bytevector-length empty",
			prog: values.List(values.NewSymbol("bytevector-length"),
				values.List(values.NewSymbol("bytevector"))),
			out: values.NewInteger(0),
		},
		{
			name: "bytevector-length non-empty",
			prog: values.List(values.NewSymbol("bytevector-length"),
				values.List(values.NewSymbol("bytevector"), values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))),
			out: values.NewInteger(3),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestBytevectorU8Ref(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "bytevector-u8-ref first",
			prog: values.List(values.NewSymbol("bytevector-u8-ref"),
				values.List(values.NewSymbol("bytevector"), values.NewInteger(10), values.NewInteger(20), values.NewInteger(30)),
				values.NewInteger(0)),
			out: values.NewInteger(10),
		},
		{
			name: "bytevector-u8-ref middle",
			prog: values.List(values.NewSymbol("bytevector-u8-ref"),
				values.List(values.NewSymbol("bytevector"), values.NewInteger(10), values.NewInteger(20), values.NewInteger(30)),
				values.NewInteger(1)),
			out: values.NewInteger(20),
		},
		{
			name: "bytevector-u8-ref last",
			prog: values.List(values.NewSymbol("bytevector-u8-ref"),
				values.List(values.NewSymbol("bytevector"), values.NewInteger(10), values.NewInteger(20), values.NewInteger(30)),
				values.NewInteger(2)),
			out: values.NewInteger(30),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestBytevectorU8Set(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "bytevector-u8-set! and verify",
			code: `(let ((bv (bytevector 1 2 3)))
				     (bytevector-u8-set! bv 1 99)
				     (bytevector-u8-ref bv 1))`,
			out: values.NewInteger(99),
		},
		{
			name: "bytevector-u8-set! first element",
			code: `(let ((bv (bytevector 1 2 3)))
				     (bytevector-u8-set! bv 0 100)
				     (bytevector-u8-ref bv 0))`,
			out: values.NewInteger(100),
		},
		{
			name: "bytevector-u8-set! last element",
			code: `(let ((bv (bytevector 1 2 3)))
				     (bytevector-u8-set! bv 2 200)
				     (bytevector-u8-ref bv 2))`,
			out: values.NewInteger(200),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestBytevectorCopy(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "bytevector-copy creates independent copy",
			code: `(let* ((bv1 (bytevector 1 2 3))
				          (bv2 (bytevector-copy bv1)))
				      (bytevector-u8-set! bv2 1 99)
				      (bytevector-u8-ref bv1 1))`,
			out: values.NewInteger(2),
		},
		{
			name: "bytevector-copy length",
			code: `(let ((bv (bytevector 1 2 3 4 5)))
				     (bytevector-length (bytevector-copy bv)))`,
			out: values.NewInteger(5),
		},
		{
			name: "bytevector-copy preserves content",
			code: `(let ((bv (bytevector 10 20 30)))
				     (bytevector-u8-ref (bytevector-copy bv) 1))`,
			out: values.NewInteger(20),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestBytevectorAppend(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "bytevector-append two bytevectors",
			code: `(bytevector-length (bytevector-append (bytevector 1 2) (bytevector 3 4)))`,
			out:  values.NewInteger(4),
		},
		{
			name: "bytevector-append and access",
			code: `(bytevector-u8-ref (bytevector-append (bytevector 1 2) (bytevector 3 4)) 2)`,
			out:  values.NewInteger(3),
		},
		{
			name: "bytevector-append empty",
			code: `(bytevector-length (bytevector-append (bytevector) (bytevector)))`,
			out:  values.NewInteger(0),
		},
		{
			name: "bytevector-append three bytevectors",
			code: `(bytevector-length (bytevector-append (bytevector 1) (bytevector 2 3) (bytevector 4 5 6)))`,
			out:  values.NewInteger(6),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestUtf8ToString(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "utf8->string simple ASCII",
			code: `(utf8->string (bytevector 72 101 108 108 111))`,
			out:  values.NewString("Hello"),
		},
		{
			name: "utf8->string empty",
			code: `(utf8->string (bytevector))`,
			out:  values.NewString(""),
		},
		{
			name: "utf8->string single char",
			code: `(utf8->string (bytevector 65))`,
			out:  values.NewString("A"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestStringToUtf8(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "string->utf8 simple ASCII",
			code: `(bytevector-length (string->utf8 "Hello"))`,
			out:  values.NewInteger(5),
		},
		{
			name: "string->utf8 and back",
			code: `(utf8->string (string->utf8 "test"))`,
			out:  values.NewString("test"),
		},
		{
			name: "string->utf8 empty string",
			code: `(bytevector-length (string->utf8 ""))`,
			out:  values.NewInteger(0),
		},
		{
			name: "string->utf8 verify byte value",
			code: `(bytevector-u8-ref (string->utf8 "A") 0)`,
			out:  values.NewInteger(65),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestBytevectorRoundTrip(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "create, modify, copy, verify independence",
			code: `(let* ((bv1 (bytevector 1 2 3))
				          (bv2 (bytevector-copy bv1)))
				      (bytevector-u8-set! bv1 0 100)
				      (list (bytevector-u8-ref bv1 0) (bytevector-u8-ref bv2 0)))`,
			out: values.List(values.NewInteger(100), values.NewInteger(1)),
		},
		{
			name: "append and verify all elements",
			code: `(let ((bv (bytevector-append (bytevector 1 2) (bytevector 3 4 5))))
				     (list (bytevector-u8-ref bv 0)
				           (bytevector-u8-ref bv 2)
				           (bytevector-u8-ref bv 4)))`,
			out: values.List(values.NewInteger(1), values.NewInteger(3), values.NewInteger(5)),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestBytevectorU8RefSet(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{
			name: "bytevector-u8-ref",
			code: `(let ((bv (bytevector 1 2 3))) (bytevector-u8-ref bv 1))`,
		},
		{
			name: "bytevector-u8-set!",
			code: `(let ((bv (bytevector 1 2 3))) (bytevector-u8-set! bv 1 99) (bytevector-u8-ref bv 1))`,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.IsNotNil)
		})
	}
}

// --- Error tests ---

func TestMakeBytevector_Errors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "non-integer size", code: `(make-bytevector "a")`},
		{name: "negative size", code: `(make-bytevector -1)`},
		{name: "non-integer fill", code: `(make-bytevector 3 "x")`},
		{name: "fill > 255", code: `(make-bytevector 3 256)`},
		{name: "fill < 0", code: `(make-bytevector 3 -1)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.Not(qt.IsNil))
		})
	}
}

func TestMakeBytevector_EdgeCases(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{name: "fill = 0 boundary", code: `(bytevector-u8-ref (make-bytevector 1 0) 0)`, expected: values.NewInteger(0)},
		{name: "fill = 255 boundary", code: `(bytevector-u8-ref (make-bytevector 1 255) 0)`, expected: values.NewInteger(255)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.expected)
		})
	}
}

func TestBytevector_Errors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "non-integer element", code: `(bytevector 1 "a" 3)`},
		{name: "element > 255", code: `(bytevector 1 256 3)`},
		{name: "element < 0", code: `(bytevector 1 -1 3)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.Not(qt.IsNil))
		})
	}
}

func TestBytevector_EdgeCases(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{name: "boundary 0", code: `(bytevector-u8-ref (bytevector 0) 0)`, expected: values.NewInteger(0)},
		{name: "boundary 255", code: `(bytevector-u8-ref (bytevector 255) 0)`, expected: values.NewInteger(255)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.expected)
		})
	}
}

func TestBytevectorLength_Errors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "non-bytevector string", code: `(bytevector-length "hello")`},
		{name: "non-bytevector integer", code: `(bytevector-length 42)`},
		{name: "non-bytevector list", code: `(bytevector-length '(1 2 3))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.Not(qt.IsNil))
		})
	}
}

func TestBytevectorU8Ref_Errors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "non-bytevector", code: `(bytevector-u8-ref "hello" 0)`},
		{name: "non-integer index", code: `(bytevector-u8-ref (bytevector 1 2 3) "a")`},
		{name: "negative index", code: `(bytevector-u8-ref (bytevector 1 2 3) -1)`},
		{name: "index = length", code: `(bytevector-u8-ref (bytevector 1 2 3) 3)`},
		{name: "index > length", code: `(bytevector-u8-ref (bytevector 1 2 3) 10)`},
		{name: "empty bytevector", code: `(bytevector-u8-ref (bytevector) 0)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.Not(qt.IsNil))
		})
	}
}

func TestBytevectorU8Set_Errors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "non-bytevector", code: `(bytevector-u8-set! "hello" 0 1)`},
		{name: "non-integer index", code: `(bytevector-u8-set! (bytevector 1 2 3) "a" 1)`},
		{name: "negative index", code: `(bytevector-u8-set! (bytevector 1 2 3) -1 1)`},
		{name: "index = length", code: `(bytevector-u8-set! (bytevector 1 2 3) 3 1)`},
		{name: "non-integer value", code: `(bytevector-u8-set! (bytevector 1 2 3) 0 "a")`},
		{name: "value > 255", code: `(bytevector-u8-set! (bytevector 1 2 3) 0 256)`},
		{name: "value < 0", code: `(bytevector-u8-set! (bytevector 1 2 3) 0 -1)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.Not(qt.IsNil))
		})
	}
}

func TestBytevectorU8Set_EdgeCases(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{
			name:     "value = 0 boundary",
			code:     `(let ((bv (bytevector 1 2 3))) (bytevector-u8-set! bv 0 0) (bytevector-u8-ref bv 0))`,
			expected: values.NewInteger(0),
		},
		{
			name:     "value = 255 boundary",
			code:     `(let ((bv (bytevector 1 2 3))) (bytevector-u8-set! bv 0 255) (bytevector-u8-ref bv 0))`,
			expected: values.NewInteger(255),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.expected)
		})
	}
}

func TestByteRangeValidation_Sentinel(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"make-bytevector fill > 255", `(make-bytevector 3 256)`},
		{"make-bytevector fill < 0", `(make-bytevector 3 -1)`},
		{"bytevector element > 255", `(bytevector 1 256 3)`},
		{"bytevector element < 0", `(bytevector 1 -1 3)`},
		{"bytevector-u8-set! value > 255", `(bytevector-u8-set! (bytevector 1 2 3) 0 256)`},
		{"bytevector-u8-set! value < 0", `(bytevector-u8-set! (bytevector 1 2 3) 0 -1)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
			qt.Assert(t, errors.Is(err, values.ErrNotAByte), qt.IsTrue)
		})
	}
}

func TestBytevectorCopy_Errors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "non-bytevector", code: `(bytevector-copy "hello")`},
		{name: "non-integer start", code: `(bytevector-copy (bytevector 1 2 3) "a")`},
		{name: "negative start", code: `(bytevector-copy (bytevector 1 2 3) -1)`},
		{name: "start > length", code: `(bytevector-copy (bytevector 1 2 3) 4)`},
		{name: "non-integer end", code: `(bytevector-copy (bytevector 1 2 3) 0 "a")`},
		{name: "end < start", code: `(bytevector-copy (bytevector 1 2 3) 2 1)`},
		{name: "end > length", code: `(bytevector-copy (bytevector 1 2 3) 0 5)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.Not(qt.IsNil))
		})
	}
}

func TestBytevectorCopy_EdgeCases(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{name: "start = end empty", code: `(bytevector-length (bytevector-copy (bytevector 1 2 3) 1 1))`, expected: values.NewInteger(0)},
		{name: "partial copy with start", code: `(bytevector-length (bytevector-copy (bytevector 1 2 3) 1))`, expected: values.NewInteger(2)},
		{name: "partial copy start+end", code: `(bytevector-length (bytevector-copy (bytevector 1 2 3 4 5) 1 3))`, expected: values.NewInteger(2)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.expected)
		})
	}
}

func TestBytevectorAppend_Errors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "non-bytevector element", code: `(bytevector-append (bytevector 1 2) "hello")`},
		{name: "non-bytevector integer", code: `(bytevector-append 42)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.Not(qt.IsNil))
		})
	}
}

func TestBytevectorAppend_EdgeCases(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{name: "no arguments", code: `(bytevector-length (bytevector-append))`, expected: values.NewInteger(0)},
		{name: "single argument", code: `(bytevector-length (bytevector-append (bytevector 1 2 3)))`, expected: values.NewInteger(3)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.expected)
		})
	}
}

func TestUtf8ToString_Errors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "non-bytevector", code: `(utf8->string "hello")`},
		{name: "non-integer start", code: `(utf8->string (bytevector 65) "a")`},
		{name: "negative start", code: `(utf8->string (bytevector 65) -1)`},
		{name: "start > length", code: `(utf8->string (bytevector 65) 2)`},
		{name: "end < start", code: `(utf8->string (bytevector 65 66 67) 2 1)`},
		{name: "end > length", code: `(utf8->string (bytevector 65) 0 5)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.Not(qt.IsNil))
		})
	}
}

func TestUtf8ToString_EdgeCases(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{name: "with start", code: `(utf8->string (bytevector 65 66 67) 1)`, expected: values.NewString("BC")},
		{name: "with start and end", code: `(utf8->string (bytevector 65 66 67) 1 2)`, expected: values.NewString("B")},
		{name: "multi-byte UTF-8 lambda", code: `(utf8->string (bytevector 206 187))`, expected: values.NewString("λ")},
		{name: "multi-byte round-trip", code: `(utf8->string (string->utf8 "λ"))`, expected: values.NewString("λ")},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.expected)
		})
	}
}

func TestStringToUtf8_Errors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "non-string", code: `(string->utf8 42)`},
		{name: "non-integer start", code: `(string->utf8 "hello" "a")`},
		{name: "negative start", code: `(string->utf8 "hello" -1)`},
		{name: "start > length", code: `(string->utf8 "hello" 10)`},
		{name: "end < start", code: `(string->utf8 "hello" 3 1)`},
		{name: "end > length", code: `(string->utf8 "hello" 0 10)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.Not(qt.IsNil))
		})
	}
}

func TestStringToUtf8_EdgeCases(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{name: "with start", code: `(bytevector-length (string->utf8 "hello" 2))`, expected: values.NewInteger(3)},
		{name: "with start and end", code: `(bytevector-length (string->utf8 "hello" 1 3))`, expected: values.NewInteger(2)},
		{name: "multi-byte UTF-8 lambda", code: `(bytevector-length (string->utf8 "λ"))`, expected: values.NewInteger(2)},
		{name: "multi-byte verify bytes", code: `(bytevector-u8-ref (string->utf8 "λ") 0)`, expected: values.NewInteger(206)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.expected)
		})
	}
}
