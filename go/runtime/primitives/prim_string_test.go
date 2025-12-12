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
	"bytes"
	"testing"

	"wile/runtime/primitives"
	"wile/values"

	qt "github.com/frankban/quicktest"
)

func TestStringValue(t *testing.T) {
	tcs := []struct {
		name   string
		input  values.Value
		expect string
	}{
		{
			name:   "integer",
			input:  values.NewInteger(42),
			expect: "42",
		},
		{
			name:   "string",
			input:  values.NewString("hello"),
			expect: "hello",
		},
		{
			name:   "symbol",
			input:  values.NewSymbol("foo"),
			expect: "foo",
		},
		{
			name:   "boolean true",
			input:  values.TrueValue,
			expect: "#t",
		},
		{
			name:   "boolean false",
			input:  values.FalseValue,
			expect: "#f",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := primitives.StringValue(tc.input)
			qt.Assert(t, result, qt.Equals, tc.expect)
		})
	}
}

func TestStringValueWithPair(t *testing.T) {
	// Test StringValue with a Pair which has both String() and SchemeString()
	pair := values.NewCons(values.NewInteger(1), values.NewInteger(2))
	result := primitives.StringValue(pair)
	qt.Assert(t, result, qt.Equals, "(1 . 2)")
}

func TestStringValueWithoutStringer(t *testing.T) {
	// Test StringValue with a type that doesn't implement fmt.Stringer
	// CharacterOutputPort implements values.Value but not fmt.Stringer
	buf := &bytes.Buffer{}
	port := values.NewCharacterOutputPort(buf)
	result := primitives.StringValue(port)
	// Should use SchemeString() instead
	qt.Assert(t, result, qt.Equals, port.SchemeString())
}

func TestStringLength(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "string-length of hello",
			prog: values.List(values.NewSymbol("string-length"), values.NewString("hello")),
			out:  values.NewInteger(5),
		},
		{
			name: "string-length of empty string",
			prog: values.List(values.NewSymbol("string-length"), values.NewString("")),
			out:  values.NewInteger(0),
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

func TestStringRef(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "string-ref first char",
			prog: values.List(values.NewSymbol("string-ref"), values.NewString("hello"), values.NewInteger(0)),
			out:  values.NewCharacter('h'),
		},
		{
			name: "string-ref middle char",
			prog: values.List(values.NewSymbol("string-ref"), values.NewString("hello"), values.NewInteger(2)),
			out:  values.NewCharacter('l'),
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

func TestSubstring(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "substring from middle",
			prog: values.List(values.NewSymbol("substring"), values.NewString("hello"), values.NewInteger(1), values.NewInteger(4)),
			out:  values.NewString("ell"),
		},
		{
			name: "substring from start",
			prog: values.List(values.NewSymbol("substring"), values.NewString("hello"), values.NewInteger(0), values.NewInteger(2)),
			out:  values.NewString("he"),
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

func TestStringAppend(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "string-append two strings",
			prog: values.List(values.NewSymbol("string-append"), values.NewString("hello"), values.NewString(" world")),
			out:  values.NewString("hello world"),
		},
		{
			name: "string-append three strings",
			prog: values.List(values.NewSymbol("string-append"), values.NewString("a"), values.NewString("b"), values.NewString("c")),
			out:  values.NewString("abc"),
		},
		{
			name: "string-append no strings",
			prog: values.List(values.NewSymbol("string-append")),
			out:  values.NewString(""),
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

func TestStringToList(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "string->list",
			prog: values.List(values.NewSymbol("string->list"), values.NewString("abc")),
			out:  values.List(values.NewCharacter('a'), values.NewCharacter('b'), values.NewCharacter('c')),
		},
		{
			name: "string->list empty",
			prog: values.List(values.NewSymbol("string->list"), values.NewString("")),
			out:  values.EmptyList,
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

func TestListToString(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "list->string",
			prog: values.List(values.NewSymbol("list->string"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewCharacter('a'), values.NewCharacter('b'), values.NewCharacter('c')))),
			out: values.NewString("abc"),
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

func TestStringToSymbol(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "string->symbol",
			prog: values.List(values.NewSymbol("string->symbol"), values.NewString("foo")),
			out:  values.NewSymbol("foo"),
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

func TestSymbolToString(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "symbol->string",
			prog: values.List(values.NewSymbol("symbol->string"),
				values.List(values.NewSymbol("quote"), values.NewSymbol("foo"))),
			out: values.NewString("foo"),
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

func TestNumberToString(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "number->string integer",
			prog: values.List(values.NewSymbol("number->string"), values.NewInteger(42)),
			out:  values.NewString("42"),
		},
		{
			name: "number->string negative",
			prog: values.List(values.NewSymbol("number->string"), values.NewInteger(-123)),
			out:  values.NewString("-123"),
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

func TestStringToNumber(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "string->number integer",
			prog: values.List(values.NewSymbol("string->number"), values.NewString("42")),
			out:  values.NewInteger(42),
		},
		{
			name: "string->number negative",
			prog: values.List(values.NewSymbol("string->number"), values.NewString("-123")),
			out:  values.NewInteger(-123),
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
