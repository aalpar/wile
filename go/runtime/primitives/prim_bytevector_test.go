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

	qt "github.com/frankban/quicktest"

	"wile/values"
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
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
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
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
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
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
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
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
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
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
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
			qt.Assert(t, result, values.SchemeEquals, tc.out)
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
			qt.Assert(t, result, values.SchemeEquals, tc.out)
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
			qt.Assert(t, result, values.SchemeEquals, tc.out)
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
			qt.Assert(t, result, values.SchemeEquals, tc.out)
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
			qt.Assert(t, result, values.SchemeEquals, tc.out)
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
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}
