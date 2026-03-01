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

	"github.com/aalpar/wile/registry/testhelpers"
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
			result, err := testhelpers.RunProgramAST(t, tc.prog)
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
			result, err := testhelpers.RunProgramAST(t, tc.prog)
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
			result, err := testhelpers.RunProgramAST(t, tc.prog)
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
			result, err := testhelpers.RunProgramAST(t, tc.prog)
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
			result, err := testhelpers.RunProgramAST(t, tc.prog)
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
			result, err := testhelpers.RunSchemeCode(t, tc.code)
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
			result, err := testhelpers.RunSchemeCode(t, tc.code)
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
			result, err := testhelpers.RunSchemeCode(t, tc.code)
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
			result, err := testhelpers.RunSchemeCode(t, tc.code)
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
			result, err := testhelpers.RunSchemeCode(t, tc.code)
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
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestBytevectorU8RefSet(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{
			Name: "bytevector-u8-ref",
			Code: `(let ((bv (bytevector 1 2 3))) (bytevector-u8-ref bv 1))`,
		},
		{
			Name: "bytevector-u8-set!",
			Code: `(let ((bv (bytevector 1 2 3))) (bytevector-u8-set! bv 1 99) (bytevector-u8-ref bv 1))`,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, qt.IsNotNil)
		})
	}
}

// --- Error tests ---

func TestMakeBytevector_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "non-integer size", Code: `(make-bytevector "a")`},
		{Name: "negative size", Code: `(make-bytevector -1)`},
		{Name: "non-integer fill", Code: `(make-bytevector 3 "x")`},
		{Name: "fill > 255", Code: `(make-bytevector 3 256)`},
		{Name: "fill < 0", Code: `(make-bytevector 3 -1)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.Not(qt.IsNil))
		})
	}
}

func TestMakeBytevector_EdgeCases(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "fill = 0 boundary", Code: `(bytevector-u8-ref (make-bytevector 1 0) 0)`, Expected: values.NewInteger(0)},
		{Name: "fill = 255 boundary", Code: `(bytevector-u8-ref (make-bytevector 1 255) 0)`, Expected: values.NewInteger(255)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestBytevector_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "non-integer element", Code: `(bytevector 1 "a" 3)`},
		{Name: "element > 255", Code: `(bytevector 1 256 3)`},
		{Name: "element < 0", Code: `(bytevector 1 -1 3)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.Not(qt.IsNil))
		})
	}
}

func TestBytevector_EdgeCases(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "boundary 0", Code: `(bytevector-u8-ref (bytevector 0) 0)`, Expected: values.NewInteger(0)},
		{Name: "boundary 255", Code: `(bytevector-u8-ref (bytevector 255) 0)`, Expected: values.NewInteger(255)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestBytevectorLength_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "non-bytevector string", Code: `(bytevector-length "hello")`},
		{Name: "non-bytevector integer", Code: `(bytevector-length 42)`},
		{Name: "non-bytevector list", Code: `(bytevector-length '(1 2 3))`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.Not(qt.IsNil))
		})
	}
}

func TestBytevectorU8Ref_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "non-bytevector", Code: `(bytevector-u8-ref "hello" 0)`},
		{Name: "non-integer index", Code: `(bytevector-u8-ref (bytevector 1 2 3) "a")`},
		{Name: "negative index", Code: `(bytevector-u8-ref (bytevector 1 2 3) -1)`},
		{Name: "index = length", Code: `(bytevector-u8-ref (bytevector 1 2 3) 3)`},
		{Name: "index > length", Code: `(bytevector-u8-ref (bytevector 1 2 3) 10)`},
		{Name: "empty bytevector", Code: `(bytevector-u8-ref (bytevector) 0)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.Not(qt.IsNil))
		})
	}
}

func TestBytevectorU8Set_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "non-bytevector", Code: `(bytevector-u8-set! "hello" 0 1)`},
		{Name: "non-integer index", Code: `(bytevector-u8-set! (bytevector 1 2 3) "a" 1)`},
		{Name: "negative index", Code: `(bytevector-u8-set! (bytevector 1 2 3) -1 1)`},
		{Name: "index = length", Code: `(bytevector-u8-set! (bytevector 1 2 3) 3 1)`},
		{Name: "non-integer value", Code: `(bytevector-u8-set! (bytevector 1 2 3) 0 "a")`},
		{Name: "value > 255", Code: `(bytevector-u8-set! (bytevector 1 2 3) 0 256)`},
		{Name: "value < 0", Code: `(bytevector-u8-set! (bytevector 1 2 3) 0 -1)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.Not(qt.IsNil))
		})
	}
}

func TestBytevectorU8Set_EdgeCases(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "value = 0 boundary",
			Code:     `(let ((bv (bytevector 1 2 3))) (bytevector-u8-set! bv 0 0) (bytevector-u8-ref bv 0))`,
			Expected: values.NewInteger(0),
		},
		{
			Name:     "value = 255 boundary",
			Code:     `(let ((bv (bytevector 1 2 3))) (bytevector-u8-set! bv 0 255) (bytevector-u8-ref bv 0))`,
			Expected: values.NewInteger(255),
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
			_, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
			qt.Assert(t, errors.Is(err, values.ErrNotAByte), qt.IsTrue)
		})
	}
}

func TestBytevectorCopy_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "non-bytevector", Code: `(bytevector-copy "hello")`},
		{Name: "non-integer start", Code: `(bytevector-copy (bytevector 1 2 3) "a")`},
		{Name: "negative start", Code: `(bytevector-copy (bytevector 1 2 3) -1)`},
		{Name: "start > length", Code: `(bytevector-copy (bytevector 1 2 3) 4)`},
		{Name: "non-integer end", Code: `(bytevector-copy (bytevector 1 2 3) 0 "a")`},
		{Name: "end < start", Code: `(bytevector-copy (bytevector 1 2 3) 2 1)`},
		{Name: "end > length", Code: `(bytevector-copy (bytevector 1 2 3) 0 5)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.Not(qt.IsNil))
		})
	}
}

func TestBytevectorCopy_EdgeCases(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "start = end empty", Code: `(bytevector-length (bytevector-copy (bytevector 1 2 3) 1 1))`, Expected: values.NewInteger(0)},
		{Name: "partial copy with start", Code: `(bytevector-length (bytevector-copy (bytevector 1 2 3) 1))`, Expected: values.NewInteger(2)},
		{Name: "partial copy start+end", Code: `(bytevector-length (bytevector-copy (bytevector 1 2 3 4 5) 1 3))`, Expected: values.NewInteger(2)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestBytevectorAppend_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "non-bytevector element", Code: `(bytevector-append (bytevector 1 2) "hello")`},
		{Name: "non-bytevector integer", Code: `(bytevector-append 42)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.Not(qt.IsNil))
		})
	}
}

func TestBytevectorAppend_EdgeCases(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "no arguments", Code: `(bytevector-length (bytevector-append))`, Expected: values.NewInteger(0)},
		{Name: "single argument", Code: `(bytevector-length (bytevector-append (bytevector 1 2 3)))`, Expected: values.NewInteger(3)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestUtf8ToString_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "non-bytevector", Code: `(utf8->string "hello")`},
		{Name: "non-integer start", Code: `(utf8->string (bytevector 65) "a")`},
		{Name: "negative start", Code: `(utf8->string (bytevector 65) -1)`},
		{Name: "start > length", Code: `(utf8->string (bytevector 65) 2)`},
		{Name: "end < start", Code: `(utf8->string (bytevector 65 66 67) 2 1)`},
		{Name: "end > length", Code: `(utf8->string (bytevector 65) 0 5)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.Not(qt.IsNil))
		})
	}
}

func TestUtf8ToString_EdgeCases(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "with start", Code: `(utf8->string (bytevector 65 66 67) 1)`, Expected: values.NewString("BC")},
		{Name: "with start and end", Code: `(utf8->string (bytevector 65 66 67) 1 2)`, Expected: values.NewString("B")},
		{Name: "multi-byte UTF-8 lambda", Code: `(utf8->string (bytevector 206 187))`, Expected: values.NewString("λ")},
		{Name: "multi-byte round-trip", Code: `(utf8->string (string->utf8 "λ"))`, Expected: values.NewString("λ")},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestStringToUtf8_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "non-string", Code: `(string->utf8 42)`},
		{Name: "non-integer start", Code: `(string->utf8 "hello" "a")`},
		{Name: "negative start", Code: `(string->utf8 "hello" -1)`},
		{Name: "start > length", Code: `(string->utf8 "hello" 10)`},
		{Name: "end < start", Code: `(string->utf8 "hello" 3 1)`},
		{Name: "end > length", Code: `(string->utf8 "hello" 0 10)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.Not(qt.IsNil))
		})
	}
}

func TestStringToUtf8_EdgeCases(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "with start", Code: `(bytevector-length (string->utf8 "hello" 2))`, Expected: values.NewInteger(3)},
		{Name: "with start and end", Code: `(bytevector-length (string->utf8 "hello" 1 3))`, Expected: values.NewInteger(2)},
		{Name: "multi-byte UTF-8 lambda", Code: `(bytevector-length (string->utf8 "λ"))`, Expected: values.NewInteger(2)},
		{Name: "multi-byte verify bytes", Code: `(bytevector-u8-ref (string->utf8 "λ") 0)`, Expected: values.NewInteger(206)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}
