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
	"bytes"
	"testing"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// stringValue returns the display representation of a value.
// Uses String() if available (for human-readable output), otherwise SchemeString().
func stringValue(o values.Value) string {
	stringer, ok := o.(interface{ String() string })
	if ok {
		return stringer.String()
	}
	return o.SchemeString()
}

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
			result := stringValue(tc.input)
			qt.Assert(t, result, qt.Equals, tc.expect)
		})
	}
}

func TestStringValueWithPair(t *testing.T) {
	// Test StringValue with a Pair which has both String() and SchemeString()
	pair := values.NewCons(values.NewInteger(1), values.NewInteger(2))
	result := stringValue(pair)
	qt.Assert(t, result, qt.Equals, "(1 . 2)")
}

func TestStringValueWithoutStringer(t *testing.T) {
	// Test StringValue with a type that doesn't implement fmt.Stringer
	// CharacterOutputPort implements values.wrt but not fmt.Stringer
	buf := &bytes.Buffer{}
	port := values.NewCharacterOutputPortFromWriter(buf)
	result := stringValue(port)
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
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
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
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
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
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
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
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
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
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
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
		{
			name: "list->string empty list",
			prog: values.List(values.NewSymbol("list->string"),
				values.List(values.NewSymbol("quote"), values.EmptyList)),
			out: values.NewString(""),
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

func TestListToStringUnicode(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "list->string unicode chars",
			Code:     `(list->string (list #\α #\β #\γ))`,
			Expected: values.NewString("αβγ"),
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
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestStringToSymbolEdgeCases(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "string->symbol empty string round-trip",
			Code:     `(symbol->string (string->symbol ""))`,
			Expected: values.NewString(""),
		},
		{
			Name:     "string->symbol unicode round-trip",
			Code:     `(symbol->string (string->symbol "你好"))`,
			Expected: values.NewString("你好"),
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

func TestStringSymbolRoundTrip(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "ASCII round-trip",
			Code:     `(symbol->string (string->symbol "hello"))`,
			Expected: values.NewString("hello"),
		},
		{
			Name:     "empty round-trip",
			Code:     `(symbol->string (string->symbol ""))`,
			Expected: values.NewString(""),
		},
		{
			Name:     "unicode round-trip",
			Code:     `(symbol->string (string->symbol "café"))`,
			Expected: values.NewString("café"),
		},
		{
			Name:     "Chinese round-trip",
			Code:     `(symbol->string (string->symbol "你好"))`,
			Expected: values.NewString("你好"),
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
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestSymbolToStringEdgeCases(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "symbol->string empty symbol",
			Code:     `(symbol->string (string->symbol ""))`,
			Expected: values.NewString(""),
		},
		{
			Name:     "symbol->string unicode symbol",
			Code:     `(symbol->string (string->symbol "αβγ"))`,
			Expected: values.NewString("αβγ"),
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
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
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
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestStringAppendExtended(t *testing.T) {
	tcs := []struct {
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

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			s, ok := result.(*values.String)
			qt.Assert(t, ok, qt.IsTrue)
			qt.Assert(t, s.Value, qt.Equals, tc.expected)
		})
	}
}

func TestStringAppendWithNonString(t *testing.T) {
	_, err := testhelpers.RunSchemeCode(t, `(string-append "hello" 42)`)
	qt.Assert(t, err, qt.IsNotNil)
}

func TestStringLengthExtended(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "string-length unicode",
			Code:     `(string-length "héllo")`,
			Expected: values.NewInteger(5),
		},
		{
			Name:     "string-length single char",
			Code:     `(string-length "x")`,
			Expected: values.NewInteger(1),
		},
		{
			Name:     "string-length with spaces",
			Code:     `(string-length "hello world")`,
			Expected: values.NewInteger(11),
		},
		{
			Name:     "string-length with newline",
			Code:     `(string-length "line1\nline2")`,
			Expected: values.NewInteger(11),
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

func TestStringRefExtended(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "string-ref last char",
			Code:     `(string-ref "hello" 4)`,
			Expected: values.NewCharacter('o'),
		},
		{
			Name:     "string-ref space",
			Code:     `(string-ref "a b" 1)`,
			Expected: values.NewCharacter(' '),
		},
		{
			Name:     "string-ref single char string",
			Code:     `(string-ref "x" 0)`,
			Expected: values.NewCharacter('x'),
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

func TestStringRefOutOfBounds(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{
			Name: "string-ref negative index",
			Code: `(string-ref "hello" -1)`,
		},
		{
			Name: "string-ref index too large",
			Code: `(string-ref "hello" 5)`,
		},
		{
			Name: "string-ref index on empty string",
			Code: `(string-ref "" 0)`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

func TestSubstringExtended(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "substring full string",
			Code:     `(substring "hello" 0 5)`,
			Expected: values.NewString("hello"),
		},
		{
			Name:     "substring empty result",
			Code:     `(substring "hello" 2 2)`,
			Expected: values.NewString(""),
		},
		{
			Name:     "substring single char",
			Code:     `(substring "hello" 1 2)`,
			Expected: values.NewString("e"),
		},
		{
			Name:     "substring to end",
			Code:     `(substring "hello" 3 5)`,
			Expected: values.NewString("lo"),
		},
		{
			Name:     "substring from start",
			Code:     `(substring "hello" 0 3)`,
			Expected: values.NewString("hel"),
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

func TestSubstringErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{
			Name: "substring start > end",
			Code: `(substring "hello" 3 2)`,
		},
		{
			Name: "substring end too large",
			Code: `(substring "hello" 0 6)`,
		},
		{
			Name: "substring negative start",
			Code: `(substring "hello" -1 3)`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

func TestMakeString(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "make-string with length only",
			Code:     `(string-length (make-string 5))`,
			Expected: values.NewInteger(5),
		},
		{
			Name:     "make-string with fill char",
			Code:     `(make-string 3 #\a)`,
			Expected: values.NewString("aaa"),
		},
		{
			Name:     "make-string zero length",
			Code:     `(make-string 0)`,
			Expected: values.NewString(""),
		},
		{
			Name:     "make-string with space fill",
			Code:     `(make-string 4 #\space)`,
			Expected: values.NewString("    "),
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

func TestStringCopy(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "string-copy full",
			Code:     `(string-copy "hello")`,
			Expected: values.NewString("hello"),
		},
		{
			Name:     "string-copy empty",
			Code:     `(string-copy "")`,
			Expected: values.NewString(""),
		},
		// R7RS extended: (string-copy string start)
		{
			Name:     "string-copy with start",
			Code:     `(string-copy "hello" 2)`,
			Expected: values.NewString("llo"),
		},
		{
			Name:     "string-copy with start at 0",
			Code:     `(string-copy "hello" 0)`,
			Expected: values.NewString("hello"),
		},
		{
			Name:     "string-copy with start at end",
			Code:     `(string-copy "hello" 5)`,
			Expected: values.NewString(""),
		},
		// R7RS extended: (string-copy string start end)
		{
			Name:     "string-copy with start and end",
			Code:     `(string-copy "hello" 1 4)`,
			Expected: values.NewString("ell"),
		},
		{
			Name:     "string-copy with start and end full",
			Code:     `(string-copy "hello" 0 5)`,
			Expected: values.NewString("hello"),
		},
		{
			Name:     "string-copy with start equals end",
			Code:     `(string-copy "hello" 2 2)`,
			Expected: values.NewString(""),
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

func TestStringCopyErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "string-copy with non-string", Code: `(string-copy 42)`},
		{Name: "string-copy with non-integer start", Code: `(string-copy "hello" "0")`},
		{Name: "string-copy with non-integer end", Code: `(string-copy "hello" 0 "5")`},
		{Name: "string-copy with negative start", Code: `(string-copy "hello" -1)`},
		{Name: "string-copy with end out of bounds", Code: `(string-copy "hello" 0 6)`},
		{Name: "string-copy with start > end", Code: `(string-copy "hello" 3 2)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

func TestStringCase(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// string-upcase
		{
			Name:     "string-upcase lowercase",
			Code:     `(string-upcase "hello")`,
			Expected: values.NewString("HELLO"),
		},
		{
			Name:     "string-upcase mixed",
			Code:     `(string-upcase "HeLLo")`,
			Expected: values.NewString("HELLO"),
		},
		{
			Name:     "string-upcase empty",
			Code:     `(string-upcase "")`,
			Expected: values.NewString(""),
		},
		{
			Name:     "string-upcase with numbers",
			Code:     `(string-upcase "abc123")`,
			Expected: values.NewString("ABC123"),
		},
		// string-downcase
		{
			Name:     "string-downcase uppercase",
			Code:     `(string-downcase "HELLO")`,
			Expected: values.NewString("hello"),
		},
		{
			Name:     "string-downcase mixed",
			Code:     `(string-downcase "HeLLo")`,
			Expected: values.NewString("hello"),
		},
		{
			Name:     "string-downcase empty",
			Code:     `(string-downcase "")`,
			Expected: values.NewString(""),
		},
		{
			Name:     "string-downcase with numbers",
			Code:     `(string-downcase "ABC123")`,
			Expected: values.NewString("abc123"),
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

func TestStringPredicate(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "string? with string",
			Code:     `(string? "hello")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "string? with empty string",
			Code:     `(string? "")`,
			Expected: values.TrueValue,
		},
		{
			Name:     "string? with integer",
			Code:     `(string? 42)`,
			Expected: values.FalseValue,
		},
		{
			Name:     "string? with character",
			Code:     `(string? #\a)`,
			Expected: values.FalseValue,
		},
		{
			Name:     "string? with symbol",
			Code:     `(string? 'hello)`,
			Expected: values.FalseValue,
		},
		{
			Name:     "string? with list",
			Code:     `(string? '(1 2 3))`,
			Expected: values.FalseValue,
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

func TestStringFoldcase(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "string-foldcase uppercase",
			Code:     `(string-foldcase "HELLO")`,
			Expected: values.NewString("hello"),
		},
		{
			Name:     "string-foldcase mixed",
			Code:     `(string-foldcase "HeLLo")`,
			Expected: values.NewString("hello"),
		},
		{
			Name:     "string-foldcase empty",
			Code:     `(string-foldcase "")`,
			Expected: values.NewString(""),
		},
		{
			Name:     "string-foldcase with numbers",
			Code:     `(string-foldcase "ABC123")`,
			Expected: values.NewString("abc123"),
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

// TestStringFoldcaseUnicode tests R7RS Unicode full case folding for string-foldcase.
// Per R7RS §6.7, string-foldcase uses Unicode full case folding which can expand
// characters (e.g., ß → ss, ẞ → ss).
func TestStringFoldcaseUnicode(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// German sharp S - full case folding expands to "ss"
		{
			Name:     "sharp s folds to ss",
			Code:     `(string-foldcase "ß")`,
			Expected: values.NewString("ss"),
		},
		{
			Name:     "capital sharp S folds to ss",
			Code:     `(string-foldcase "ẞ")`,
			Expected: values.NewString("ss"),
		},
		{
			Name:     "Straße becomes strasse",
			Code:     `(string-foldcase "Straße")`,
			Expected: values.NewString("strasse"),
		},
		{
			Name:     "STRASSE stays strasse",
			Code:     `(string-foldcase "STRASSE")`,
			Expected: values.NewString("strasse"),
		},
		// Greek letters
		{
			Name:     "Greek sigma",
			Code:     `(string-foldcase "ΣΕΛΛΑΣ")`,
			Expected: values.NewString("σελλασ"),
		},
		// Mixed Unicode and ASCII
		{
			Name:     "mixed Unicode",
			Code:     `(string-foldcase "Große Stadt")`,
			Expected: values.NewString("grosse stadt"),
		},
		// Already lowercase stays same (except ß)
		{
			Name:     "lowercase stays lowercase",
			Code:     `(string-foldcase "hello")`,
			Expected: values.NewString("hello"),
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

func TestStringConstructor(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "string from chars",
			Code:     `(string #\h #\e #\l #\l #\o)`,
			Expected: values.NewString("hello"),
		},
		{
			Name:     "string single char",
			Code:     `(string #\a)`,
			Expected: values.NewString("a"),
		},
		{
			Name:     "string no args",
			Code:     `(string)`,
			Expected: values.NewString(""),
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

// Unicode string tests

func TestStringUnicode(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// string-length with Unicode
		{
			Name:     "string-length with emoji",
			Code:     `(string-length "hello😀world")`, // emoji directly in string
			Expected: values.NewInteger(11),
		},
		{
			Name:     "string-length with Chinese",
			Code:     `(string-length "你好世界")`,
			Expected: values.NewInteger(4),
		},
		{
			Name:     "string-length with accented chars",
			Code:     `(string-length "café")`,
			Expected: values.NewInteger(4),
		},
		// string-ref with Unicode
		{
			Name:     "string-ref Chinese char",
			Code:     `(char->integer (string-ref "你好" 0))`,
			Expected: values.NewInteger(20320), // 你 = U+4F60
		},
		{
			Name:     "string-ref second Chinese char",
			Code:     `(char->integer (string-ref "你好" 1))`,
			Expected: values.NewInteger(22909), // 好 = U+597D
		},
		// substring with Unicode
		{
			Name:     "substring with Chinese",
			Code:     `(substring "你好世界" 1 3)`,
			Expected: values.NewString("好世"),
		},
		// string-upcase with Unicode
		{
			Name:     "string-upcase with accented",
			Code:     `(string-upcase "café")`,
			Expected: values.NewString("CAFÉ"),
		},
		{
			Name:     "string-upcase Greek",
			Code:     `(string-upcase "αβγ")`,
			Expected: values.NewString("ΑΒΓ"),
		},
		// string-downcase with Unicode
		{
			Name:     "string-downcase with accented",
			Code:     `(string-downcase "CAFÉ")`,
			Expected: values.NewString("café"),
		},
		{
			Name:     "string-downcase Greek",
			Code:     `(string-downcase "ΑΒΓ")`,
			Expected: values.NewString("αβγ"),
		},
		// string-append with Unicode
		{
			Name:     "string-append with Chinese",
			Code:     `(string-append "你好" "世界")`,
			Expected: values.NewString("你好世界"),
		},
		// string->list with Unicode
		{
			Name:     "string->list with Chinese length",
			Code:     `(length (string->list "你好"))`,
			Expected: values.NewInteger(2),
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

// Error condition tests for string operations

func TestStringErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		// string-length errors
		{
			Name: "string-length with non-string",
			Code: `(string-length 42)`,
		},
		{
			Name: "string-length with symbol",
			Code: `(string-length 'foo)`,
		},
		// string-ref errors
		{
			Name: "string-ref with non-string",
			Code: `(string-ref 42 0)`,
		},
		{
			Name: "string-ref with non-integer index",
			Code: `(string-ref "hello" "0")`,
		},
		// substring errors
		{
			Name: "substring with non-string",
			Code: `(substring 42 0 1)`,
		},
		{
			Name: "substring with non-integer start",
			Code: `(substring "hello" "0" 3)`,
		},
		{
			Name: "substring with non-integer end",
			Code: `(substring "hello" 0 "3")`,
		},
		// make-string errors
		{
			Name: "make-string with non-integer length",
			Code: `(make-string "5")`,
		},
		{
			Name: "make-string with negative length",
			Code: `(make-string -1)`,
		},
		{
			Name: "make-string with non-char fill",
			Code: `(make-string 3 "a")`,
		},
		// string-copy errors
		{
			Name: "string-copy with non-string",
			Code: `(string-copy 42)`,
		},
		// string constructor errors
		{
			Name: "string with non-char arg",
			Code: `(string #\a "b" #\c)`,
		},
		// string->list errors
		{
			Name: "string->list with non-string",
			Code: `(string->list 42)`,
		},
		// list->string errors
		{
			Name: "list->string with non-list",
			Code: `(list->string "abc")`,
		},
		{
			Name: "list->string with non-char element",
			Code: `(list->string '(#\a "b" #\c))`,
		},
		{
			Name: "list->string with improper list",
			Code: `(list->string '(#\a #\b . #\c))`,
		},
		// string-upcase errors
		{
			Name: "string-upcase with non-string",
			Code: `(string-upcase 42)`,
		},
		// string-downcase errors
		{
			Name: "string-downcase with non-string",
			Code: `(string-downcase 42)`,
		},
		// string-foldcase errors
		{
			Name: "string-foldcase with non-string",
			Code: `(string-foldcase 42)`,
		},
		{
			Name: "string-foldcase with symbol",
			Code: `(string-foldcase 'foo)`,
		},
		// string comparison errors
		{
			Name: "string=? with non-string first arg",
			Code: `(string=? 42 "hello")`,
		},
		{
			Name: "string=? with non-string second arg",
			Code: `(string=? "hello" 42)`,
		},
		{
			Name: "string<? with non-string first arg",
			Code: `(string<? 42 "hello")`,
		},
		{
			Name: "string<? with non-string second arg",
			Code: `(string<? "hello" 42)`,
		},
		{
			Name: "string>? with non-string first arg",
			Code: `(string>? 42 "hello")`,
		},
		{
			Name: "string>? with non-string second arg",
			Code: `(string>? "hello" 42)`,
		},
		{
			Name: "string<=? with non-string first arg",
			Code: `(string<=? 42 "hello")`,
		},
		{
			Name: "string<=? with non-string second arg",
			Code: `(string<=? "hello" 42)`,
		},
		{
			Name: "string>=? with non-string first arg",
			Code: `(string>=? 42 "hello")`,
		},
		{
			Name: "string>=? with non-string second arg",
			Code: `(string>=? "hello" 42)`,
		},
		// string-ci comparison errors
		{
			Name: "string-ci=? with non-string first arg",
			Code: `(string-ci=? 42 "hello")`,
		},
		{
			Name: "string-ci=? with non-string second arg",
			Code: `(string-ci=? "hello" 42)`,
		},
		{
			Name: "string-ci<? with non-string first arg",
			Code: `(string-ci<? 42 "hello")`,
		},
		{
			Name: "string-ci<? with non-string second arg",
			Code: `(string-ci<? "hello" 42)`,
		},
		{
			Name: "string-ci>? with non-string first arg",
			Code: `(string-ci>? 42 "hello")`,
		},
		{
			Name: "string-ci>? with non-string second arg",
			Code: `(string-ci>? "hello" 42)`,
		},
		{
			Name: "string-ci<=? with non-string first arg",
			Code: `(string-ci<=? 42 "hello")`,
		},
		{
			Name: "string-ci<=? with non-string second arg",
			Code: `(string-ci<=? "hello" 42)`,
		},
		{
			Name: "string-ci>=? with non-string first arg",
			Code: `(string-ci>=? 42 "hello")`,
		},
		{
			Name: "string-ci>=? with non-string second arg",
			Code: `(string-ci>=? "hello" 42)`,
		},
		// string->symbol errors
		{
			Name: "string->symbol with non-string",
			Code: `(string->symbol 42)`,
		},
		{
			Name: "string->symbol with symbol",
			Code: `(string->symbol 'foo)`,
		},
		// symbol->string errors
		{
			Name: "symbol->string with non-symbol",
			Code: `(symbol->string "foo")`,
		},
		{
			Name: "symbol->string with integer",
			Code: `(symbol->string 42)`,
		},
		// number->string errors
		{
			Name: "number->string with non-number",
			Code: `(number->string "42")`,
		},
		{
			Name: "number->string with symbol",
			Code: `(number->string 'foo)`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// Unicode character tests

func TestCharUnicode(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// char->integer with Unicode
		{
			Name:     "char->integer with Greek alpha",
			Code:     `(char->integer #\α)`,
			Expected: values.NewInteger(945), // α = U+03B1
		},
		// integer->char with Unicode
		{
			Name:     "integer->char to Greek alpha",
			Code:     `(integer->char 945)`,
			Expected: values.NewCharacter('α'),
		},
		// char-upcase with Unicode
		{
			Name:     "char-upcase Greek",
			Code:     `(char-upcase #\α)`,
			Expected: values.NewCharacter('Α'),
		},
		// char-downcase with Unicode
		{
			Name:     "char-downcase Greek",
			Code:     `(char-downcase #\Α)`,
			Expected: values.NewCharacter('α'),
		},
		// char-alphabetic? with Unicode
		{
			Name:     "char-alphabetic? with Chinese",
			Code:     `(char-alphabetic? #\中)`,
			Expected: values.TrueValue,
		},
		{
			Name:     "char-alphabetic? with Greek",
			Code:     `(char-alphabetic? #\α)`,
			Expected: values.TrueValue,
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

// R7RS string mutation tests

func TestStringSet(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "string-set! first char",
			Code:     `(let ((s (string-copy "hello"))) (string-set! s 0 #\H) s)`,
			Expected: values.NewString("Hello"),
		},
		{
			Name:     "string-set! last char",
			Code:     `(let ((s (string-copy "hello"))) (string-set! s 4 #\O) s)`,
			Expected: values.NewString("hellO"),
		},
		{
			Name:     "string-set! middle char",
			Code:     `(let ((s (string-copy "hello"))) (string-set! s 2 #\L) s)`,
			Expected: values.NewString("heLlo"),
		},
		{
			Name:     "string-set! returns void",
			Code:     `(let ((s (string-copy "hello"))) (string-set! s 0 #\H))`,
			Expected: values.Void,
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

func TestStringSetErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "string-set! with non-string", Code: `(string-set! 42 0 #\a)`},
		{Name: "string-set! with non-integer index", Code: `(string-set! "hello" "0" #\a)`},
		{Name: "string-set! with non-character", Code: `(string-set! "hello" 0 "a")`},
		{Name: "string-set! index out of bounds", Code: `(string-set! "hello" 5 #\a)`},
		{Name: "string-set! negative index", Code: `(string-set! "hello" -1 #\a)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

func TestStringFill(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "string-fill! entire string",
			Code:     `(let ((s (string-copy "hello"))) (string-fill! s #\z) s)`,
			Expected: values.NewString("zzzzz"),
		},
		{
			Name:     "string-fill! with start",
			Code:     `(let ((s (string-copy "hello"))) (string-fill! s #\z 2) s)`,
			Expected: values.NewString("hezzz"),
		},
		{
			Name:     "string-fill! with start and end",
			Code:     `(let ((s (string-copy "hello"))) (string-fill! s #\z 1 4) s)`,
			Expected: values.NewString("hzzzo"),
		},
		{
			Name:     "string-fill! returns void",
			Code:     `(let ((s (string-copy "hello"))) (string-fill! s #\z))`,
			Expected: values.Void,
		},
		{
			Name:     "string-fill! empty range",
			Code:     `(let ((s (string-copy "hello"))) (string-fill! s #\z 2 2) s)`,
			Expected: values.NewString("hello"),
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

func TestStringFillErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "string-fill! with non-string", Code: `(string-fill! 42 #\z)`},
		{Name: "string-fill! with non-character", Code: `(string-fill! "hello" "x")`},
		{Name: "string-fill! start out of bounds", Code: `(string-fill! "hello" #\z 6)`},
		{Name: "string-fill! end out of bounds", Code: `(string-fill! "hello" #\z 0 6)`},
		{Name: "string-fill! start > end", Code: `(string-fill! "hello" #\z 3 2)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

func TestStringCopyTo(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "string-copy! basic",
			Code:     `(let ((to (string-copy "12345"))) (string-copy! to 0 "abc") to)`,
			Expected: values.NewString("abc45"),
		},
		{
			Name:     "string-copy! with offset",
			Code:     `(let ((to (string-copy "12345"))) (string-copy! to 2 "abc") to)`,
			Expected: values.NewString("12abc"),
		},
		{
			Name:     "string-copy! with start",
			Code:     `(let ((to (string-copy "12345"))) (string-copy! to 0 "abcde" 2) to)`,
			Expected: values.NewString("cde45"),
		},
		{
			Name:     "string-copy! with start and end",
			Code:     `(let ((to (string-copy "12345"))) (string-copy! to 1 "abcde" 1 3) to)`,
			Expected: values.NewString("1bc45"),
		},
		{
			Name:     "string-copy! returns void",
			Code:     `(let ((to (string-copy "12345"))) (string-copy! to 0 "abc"))`,
			Expected: values.Void,
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

func TestStringCopyToErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "string-copy! to non-string", Code: `(string-copy! 42 0 "abc")`},
		{Name: "string-copy! at non-integer", Code: `(string-copy! "12345" "0" "abc")`},
		{Name: "string-copy! from non-string", Code: `(string-copy! "12345" 0 42)`},
		{Name: "string-copy! destination overflow", Code: `(string-copy! "123" 2 "abcde")`},
		{Name: "string-copy! invalid source range", Code: `(string-copy! "12345" 0 "abc" 2 1)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

func TestStringMap(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "string-map single string",
			Code:     `(string-map char-upcase "hello")`,
			Expected: values.NewString("HELLO"),
		},
		{
			Name:     "string-map with lambda",
			Code:     `(string-map (lambda (c) (integer->char (+ 1 (char->integer c)))) "abc")`,
			Expected: values.NewString("bcd"),
		},
		{
			Name:     "string-map empty string",
			Code:     `(string-map char-upcase "")`,
			Expected: values.NewString(""),
		},
		{
			Name:     "string-map two strings",
			Code:     `(string-map (lambda (a b) (if (char<? a b) b a)) "abc" "bac")`,
			Expected: values.NewString("bbc"),
		},
		// Single char
		{
			Name:     "string-map single char",
			Code:     `(string-map char-upcase "a")`,
			Expected: values.NewString("A"),
		},
		// Unequal lengths - stops at shortest
		{
			Name:     "string-map unequal lengths",
			Code:     `(string-map (lambda (a b) a) "abcde" "xy")`,
			Expected: values.NewString("ab"),
		},
		// Three strings
		{
			Name:     "string-map three strings",
			Code:     `(string-map (lambda (a b c) a) "abc" "def" "ghi")`,
			Expected: values.NewString("abc"),
		},
		// Unicode identity
		{
			Name:     "string-map Unicode identity",
			Code:     "(string-map (lambda (c) c) \"\u03b1\u03b2\u03b3\")",
			Expected: values.NewString("\u03b1\u03b2\u03b3"),
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

func TestStringMapErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "string-map non-procedure", Code: `(string-map 42 "hello")`},
		{Name: "string-map non-string", Code: `(string-map char-upcase 42)`},
		{Name: "string-map proc returns non-char", Code: `(string-map (lambda (c) 42) "hello")`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

func TestStringForEach(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "string-for-each returns void",
			Code:     `(string-for-each (lambda (c) c) "hello")`,
			Expected: values.Void,
		},
		{
			Name:     "string-for-each with side effect",
			Code:     `(let ((count 0)) (string-for-each (lambda (c) (set! count (+ count 1))) "hello") count)`,
			Expected: values.NewInteger(5),
		},
		{
			Name:     "string-for-each empty string",
			Code:     `(let ((count 0)) (string-for-each (lambda (c) (set! count (+ count 1))) "") count)`,
			Expected: values.NewInteger(0),
		},
		// Order verification
		{
			Name:     "string-for-each order verification",
			Code:     `(let ((result '())) (string-for-each (lambda (c) (set! result (cons c result))) "abc") result)`,
			Expected: values.List(values.NewCharacter('c'), values.NewCharacter('b'), values.NewCharacter('a')),
		},
		// Two strings with side effects
		{
			Name: "string-for-each two strings",
			Code: `(let ((result '())) (string-for-each (lambda (a b) (set! result (cons (list a b) result))) "ab" "xy") result)`,
			Expected: values.List(
				values.List(values.NewCharacter('b'), values.NewCharacter('y')),
				values.List(values.NewCharacter('a'), values.NewCharacter('x'))),
		},
		// Unequal lengths - stops at shortest
		{
			Name:     "string-for-each unequal lengths",
			Code:     `(let ((count 0)) (string-for-each (lambda (a b) (set! count (+ count 1))) "abcde" "xy") count)`,
			Expected: values.NewInteger(2),
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

func TestStringForEachErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "string-for-each non-procedure", Code: `(string-for-each 42 "hello")`},
		{Name: "string-for-each non-string", Code: `(string-for-each (lambda (c) c) 42)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

func TestStringToListOptional(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Basic (no optional args)
		{Name: "string->list full", Code: `(string->list "hello")`,
			Expected: values.List(values.NewCharacter('h'), values.NewCharacter('e'), values.NewCharacter('l'), values.NewCharacter('l'), values.NewCharacter('o'))},
		{Name: "string->list empty", Code: `(string->list "")`, Expected: values.EmptyList},
		// With start argument
		{Name: "string->list with start", Code: `(string->list "hello" 2)`,
			Expected: values.List(values.NewCharacter('l'), values.NewCharacter('l'), values.NewCharacter('o'))},
		{Name: "string->list with start at 0", Code: `(string->list "hello" 0)`,
			Expected: values.List(values.NewCharacter('h'), values.NewCharacter('e'), values.NewCharacter('l'), values.NewCharacter('l'), values.NewCharacter('o'))},
		{Name: "string->list with start at end", Code: `(string->list "hello" 5)`, Expected: values.EmptyList},
		// With start and end arguments
		{Name: "string->list with start and end", Code: `(string->list "hello" 1 4)`,
			Expected: values.List(values.NewCharacter('e'), values.NewCharacter('l'), values.NewCharacter('l'))},
		{Name: "string->list with start and end full", Code: `(string->list "hello" 0 5)`,
			Expected: values.List(values.NewCharacter('h'), values.NewCharacter('e'), values.NewCharacter('l'), values.NewCharacter('l'), values.NewCharacter('o'))},
		{Name: "string->list with start equals end", Code: `(string->list "hello" 2 2)`, Expected: values.EmptyList},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestStringToListErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "string->list with non-string", Code: `(string->list 42)`},
		{Name: "string->list with non-integer start", Code: `(string->list "hello" "x")`},
		{Name: "string->list with non-integer end", Code: `(string->list "hello" 0 "x")`},
		{Name: "string->list with negative start", Code: `(string->list "hello" -1)`},
		{Name: "string->list with end out of bounds", Code: `(string->list "hello" 0 10)`},
		{Name: "string->list with start > end", Code: `(string->list "hello" 3 1)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

func TestStringSetImmutable(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{
			Name: "string-set! on literal",
			Code: `(string-set! "hello" 0 #\H)`,
		},
		{
			Name: "string-set! on symbol->string",
			Code: `(string-set! (symbol->string 'test) 0 #\x)`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

func TestStringSetMutable(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "string-set! on string-copy",
			Code:     `(let ((s (string-copy "hello"))) (string-set! s 0 #\H) s)`,
			Expected: values.NewString("Hello"),
		},
		{
			Name:     "string-set! on make-string",
			Code:     `(let ((s (make-string 5 #\a))) (string-set! s 2 #\x) s)`,
			Expected: values.NewString("aaxaa"),
		},
		{
			Name:     "string-set! on list->string",
			Code:     `(let ((s (list->string '(#\h #\i)))) (string-set! s 0 #\H) s)`,
			Expected: values.NewString("Hi"),
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
