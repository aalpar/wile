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

	"github.com/aalpar/wile/values"

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
			result, err := runProgramAST(t, tc.prog)
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
			result, err := runProgramAST(t, tc.prog)
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
			result, err := runProgramAST(t, tc.prog)
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
			result, err := runProgramAST(t, tc.prog)
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
			result, err := runProgramAST(t, tc.prog)
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
		{
			name: "list->string empty list",
			prog: values.List(values.NewSymbol("list->string"),
				values.List(values.NewSymbol("quote"), values.EmptyList)),
			out: values.NewString(""),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestListToStringUnicode(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{
			name:     "list->string unicode chars",
			code:     `(list->string (list #\α #\β #\γ))`,
			expected: values.NewString("αβγ"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
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
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestStringToSymbolEdgeCases(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{
			name:     "string->symbol empty string round-trip",
			code:     `(symbol->string (string->symbol ""))`,
			expected: values.NewString(""),
		},
		{
			name:     "string->symbol unicode round-trip",
			code:     `(symbol->string (string->symbol "你好"))`,
			expected: values.NewString("你好"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestStringSymbolRoundTrip(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{
			name:     "ASCII round-trip",
			code:     `(symbol->string (string->symbol "hello"))`,
			expected: values.NewString("hello"),
		},
		{
			name:     "empty round-trip",
			code:     `(symbol->string (string->symbol ""))`,
			expected: values.NewString(""),
		},
		{
			name:     "unicode round-trip",
			code:     `(symbol->string (string->symbol "café"))`,
			expected: values.NewString("café"),
		},
		{
			name:     "Chinese round-trip",
			code:     `(symbol->string (string->symbol "你好"))`,
			expected: values.NewString("你好"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
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
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestSymbolToStringEdgeCases(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{
			name:     "symbol->string empty symbol",
			code:     `(symbol->string (string->symbol ""))`,
			expected: values.NewString(""),
		},
		{
			name:     "symbol->string unicode symbol",
			code:     `(symbol->string (string->symbol "αβγ"))`,
			expected: values.NewString("αβγ"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
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
			result, err := runProgramAST(t, tc.prog)
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
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
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
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			s, ok := result.(*values.String)
			qt.Assert(t, ok, qt.IsTrue)
			qt.Assert(t, s.Value, qt.Equals, tc.expected)
		})
	}
}

func TestStringAppendWithNonString(t *testing.T) {
	_, err := runSchemeCode(t, `(string-append "hello" 42)`)
	qt.Assert(t, err, qt.IsNotNil)
}

func TestStringLengthExtended(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{
			name:     "string-length unicode",
			code:     `(string-length "héllo")`,
			expected: values.NewInteger(5),
		},
		{
			name:     "string-length single char",
			code:     `(string-length "x")`,
			expected: values.NewInteger(1),
		},
		{
			name:     "string-length with spaces",
			code:     `(string-length "hello world")`,
			expected: values.NewInteger(11),
		},
		{
			name:     "string-length with newline",
			code:     `(string-length "line1\nline2")`,
			expected: values.NewInteger(11),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestStringRefExtended(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{
			name:     "string-ref last char",
			code:     `(string-ref "hello" 4)`,
			expected: values.NewCharacter('o'),
		},
		{
			name:     "string-ref space",
			code:     `(string-ref "a b" 1)`,
			expected: values.NewCharacter(' '),
		},
		{
			name:     "string-ref single char string",
			code:     `(string-ref "x" 0)`,
			expected: values.NewCharacter('x'),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestStringRefOutOfBounds(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{
			name: "string-ref negative index",
			code: `(string-ref "hello" -1)`,
		},
		{
			name: "string-ref index too large",
			code: `(string-ref "hello" 5)`,
		},
		{
			name: "string-ref index on empty string",
			code: `(string-ref "" 0)`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

func TestSubstringExtended(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{
			name:     "substring full string",
			code:     `(substring "hello" 0 5)`,
			expected: values.NewString("hello"),
		},
		{
			name:     "substring empty result",
			code:     `(substring "hello" 2 2)`,
			expected: values.NewString(""),
		},
		{
			name:     "substring single char",
			code:     `(substring "hello" 1 2)`,
			expected: values.NewString("e"),
		},
		{
			name:     "substring to end",
			code:     `(substring "hello" 3 5)`,
			expected: values.NewString("lo"),
		},
		{
			name:     "substring from start",
			code:     `(substring "hello" 0 3)`,
			expected: values.NewString("hel"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestSubstringErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{
			name: "substring start > end",
			code: `(substring "hello" 3 2)`,
		},
		{
			name: "substring end too large",
			code: `(substring "hello" 0 6)`,
		},
		{
			name: "substring negative start",
			code: `(substring "hello" -1 3)`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

func TestMakeString(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{
			name:     "make-string with length only",
			code:     `(string-length (make-string 5))`,
			expected: values.NewInteger(5),
		},
		{
			name:     "make-string with fill char",
			code:     `(make-string 3 #\a)`,
			expected: values.NewString("aaa"),
		},
		{
			name:     "make-string zero length",
			code:     `(make-string 0)`,
			expected: values.NewString(""),
		},
		{
			name:     "make-string with space fill",
			code:     `(make-string 4 #\space)`,
			expected: values.NewString("    "),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestStringCopy(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{
			name:     "string-copy full",
			code:     `(string-copy "hello")`,
			expected: values.NewString("hello"),
		},
		{
			name:     "string-copy empty",
			code:     `(string-copy "")`,
			expected: values.NewString(""),
		},
		// R7RS extended: (string-copy string start)
		{
			name:     "string-copy with start",
			code:     `(string-copy "hello" 2)`,
			expected: values.NewString("llo"),
		},
		{
			name:     "string-copy with start at 0",
			code:     `(string-copy "hello" 0)`,
			expected: values.NewString("hello"),
		},
		{
			name:     "string-copy with start at end",
			code:     `(string-copy "hello" 5)`,
			expected: values.NewString(""),
		},
		// R7RS extended: (string-copy string start end)
		{
			name:     "string-copy with start and end",
			code:     `(string-copy "hello" 1 4)`,
			expected: values.NewString("ell"),
		},
		{
			name:     "string-copy with start and end full",
			code:     `(string-copy "hello" 0 5)`,
			expected: values.NewString("hello"),
		},
		{
			name:     "string-copy with start equals end",
			code:     `(string-copy "hello" 2 2)`,
			expected: values.NewString(""),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestStringCopyErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "string-copy with non-string", code: `(string-copy 42)`},
		{name: "string-copy with non-integer start", code: `(string-copy "hello" "0")`},
		{name: "string-copy with non-integer end", code: `(string-copy "hello" 0 "5")`},
		{name: "string-copy with negative start", code: `(string-copy "hello" -1)`},
		{name: "string-copy with end out of bounds", code: `(string-copy "hello" 0 6)`},
		{name: "string-copy with start > end", code: `(string-copy "hello" 3 2)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

func TestStringCase(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// string-upcase
		{
			name:     "string-upcase lowercase",
			code:     `(string-upcase "hello")`,
			expected: values.NewString("HELLO"),
		},
		{
			name:     "string-upcase mixed",
			code:     `(string-upcase "HeLLo")`,
			expected: values.NewString("HELLO"),
		},
		{
			name:     "string-upcase empty",
			code:     `(string-upcase "")`,
			expected: values.NewString(""),
		},
		{
			name:     "string-upcase with numbers",
			code:     `(string-upcase "abc123")`,
			expected: values.NewString("ABC123"),
		},
		// string-downcase
		{
			name:     "string-downcase uppercase",
			code:     `(string-downcase "HELLO")`,
			expected: values.NewString("hello"),
		},
		{
			name:     "string-downcase mixed",
			code:     `(string-downcase "HeLLo")`,
			expected: values.NewString("hello"),
		},
		{
			name:     "string-downcase empty",
			code:     `(string-downcase "")`,
			expected: values.NewString(""),
		},
		{
			name:     "string-downcase with numbers",
			code:     `(string-downcase "ABC123")`,
			expected: values.NewString("abc123"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestStringPredicate(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{
			name:     "string? with string",
			code:     `(string? "hello")`,
			expected: values.TrueValue,
		},
		{
			name:     "string? with empty string",
			code:     `(string? "")`,
			expected: values.TrueValue,
		},
		{
			name:     "string? with integer",
			code:     `(string? 42)`,
			expected: values.FalseValue,
		},
		{
			name:     "string? with character",
			code:     `(string? #\a)`,
			expected: values.FalseValue,
		},
		{
			name:     "string? with symbol",
			code:     `(string? 'hello)`,
			expected: values.FalseValue,
		},
		{
			name:     "string? with list",
			code:     `(string? '(1 2 3))`,
			expected: values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestStringFoldcase(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{
			name:     "string-foldcase uppercase",
			code:     `(string-foldcase "HELLO")`,
			expected: values.NewString("hello"),
		},
		{
			name:     "string-foldcase mixed",
			code:     `(string-foldcase "HeLLo")`,
			expected: values.NewString("hello"),
		},
		{
			name:     "string-foldcase empty",
			code:     `(string-foldcase "")`,
			expected: values.NewString(""),
		},
		{
			name:     "string-foldcase with numbers",
			code:     `(string-foldcase "ABC123")`,
			expected: values.NewString("abc123"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

// TestStringFoldcaseUnicode tests R7RS Unicode full case folding for string-foldcase.
// Per R7RS §6.7, string-foldcase uses Unicode full case folding which can expand
// characters (e.g., ß → ss, ẞ → ss).
func TestStringFoldcaseUnicode(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// German sharp S - full case folding expands to "ss"
		{
			name:     "sharp s folds to ss",
			code:     `(string-foldcase "ß")`,
			expected: values.NewString("ss"),
		},
		{
			name:     "capital sharp S folds to ss",
			code:     `(string-foldcase "ẞ")`,
			expected: values.NewString("ss"),
		},
		{
			name:     "Straße becomes strasse",
			code:     `(string-foldcase "Straße")`,
			expected: values.NewString("strasse"),
		},
		{
			name:     "STRASSE stays strasse",
			code:     `(string-foldcase "STRASSE")`,
			expected: values.NewString("strasse"),
		},
		// Greek letters
		{
			name:     "Greek sigma",
			code:     `(string-foldcase "ΣΕΛΛΑΣ")`,
			expected: values.NewString("σελλασ"),
		},
		// Mixed Unicode and ASCII
		{
			name:     "mixed Unicode",
			code:     `(string-foldcase "Große Stadt")`,
			expected: values.NewString("grosse stadt"),
		},
		// Already lowercase stays same (except ß)
		{
			name:     "lowercase stays lowercase",
			code:     `(string-foldcase "hello")`,
			expected: values.NewString("hello"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestStringConstructor(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{
			name:     "string from chars",
			code:     `(string #\h #\e #\l #\l #\o)`,
			expected: values.NewString("hello"),
		},
		{
			name:     "string single char",
			code:     `(string #\a)`,
			expected: values.NewString("a"),
		},
		{
			name:     "string no args",
			code:     `(string)`,
			expected: values.NewString(""),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

// Unicode string tests

func TestStringUnicode(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// string-length with Unicode
		{
			name:     "string-length with emoji",
			code:     `(string-length "hello😀world")`, // emoji directly in string
			expected: values.NewInteger(11),
		},
		{
			name:     "string-length with Chinese",
			code:     `(string-length "你好世界")`,
			expected: values.NewInteger(4),
		},
		{
			name:     "string-length with accented chars",
			code:     `(string-length "café")`,
			expected: values.NewInteger(4),
		},
		// string-ref with Unicode
		{
			name:     "string-ref Chinese char",
			code:     `(char->integer (string-ref "你好" 0))`,
			expected: values.NewInteger(20320), // 你 = U+4F60
		},
		{
			name:     "string-ref second Chinese char",
			code:     `(char->integer (string-ref "你好" 1))`,
			expected: values.NewInteger(22909), // 好 = U+597D
		},
		// substring with Unicode
		{
			name:     "substring with Chinese",
			code:     `(substring "你好世界" 1 3)`,
			expected: values.NewString("好世"),
		},
		// string-upcase with Unicode
		{
			name:     "string-upcase with accented",
			code:     `(string-upcase "café")`,
			expected: values.NewString("CAFÉ"),
		},
		{
			name:     "string-upcase Greek",
			code:     `(string-upcase "αβγ")`,
			expected: values.NewString("ΑΒΓ"),
		},
		// string-downcase with Unicode
		{
			name:     "string-downcase with accented",
			code:     `(string-downcase "CAFÉ")`,
			expected: values.NewString("café"),
		},
		{
			name:     "string-downcase Greek",
			code:     `(string-downcase "ΑΒΓ")`,
			expected: values.NewString("αβγ"),
		},
		// string-append with Unicode
		{
			name:     "string-append with Chinese",
			code:     `(string-append "你好" "世界")`,
			expected: values.NewString("你好世界"),
		},
		// string->list with Unicode
		{
			name:     "string->list with Chinese length",
			code:     `(length (string->list "你好"))`,
			expected: values.NewInteger(2),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

// Error condition tests for string operations

func TestStringErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		// string-length errors
		{
			name: "string-length with non-string",
			code: `(string-length 42)`,
		},
		{
			name: "string-length with symbol",
			code: `(string-length 'foo)`,
		},
		// string-ref errors
		{
			name: "string-ref with non-string",
			code: `(string-ref 42 0)`,
		},
		{
			name: "string-ref with non-integer index",
			code: `(string-ref "hello" "0")`,
		},
		// substring errors
		{
			name: "substring with non-string",
			code: `(substring 42 0 1)`,
		},
		{
			name: "substring with non-integer start",
			code: `(substring "hello" "0" 3)`,
		},
		{
			name: "substring with non-integer end",
			code: `(substring "hello" 0 "3")`,
		},
		// make-string errors
		{
			name: "make-string with non-integer length",
			code: `(make-string "5")`,
		},
		{
			name: "make-string with negative length",
			code: `(make-string -1)`,
		},
		{
			name: "make-string with non-char fill",
			code: `(make-string 3 "a")`,
		},
		// string-copy errors
		{
			name: "string-copy with non-string",
			code: `(string-copy 42)`,
		},
		// string constructor errors
		{
			name: "string with non-char arg",
			code: `(string #\a "b" #\c)`,
		},
		// string->list errors
		{
			name: "string->list with non-string",
			code: `(string->list 42)`,
		},
		// list->string errors
		{
			name: "list->string with non-list",
			code: `(list->string "abc")`,
		},
		{
			name: "list->string with non-char element",
			code: `(list->string '(#\a "b" #\c))`,
		},
		{
			name: "list->string with improper list",
			code: `(list->string '(#\a #\b . #\c))`,
		},
		// string-upcase errors
		{
			name: "string-upcase with non-string",
			code: `(string-upcase 42)`,
		},
		// string-downcase errors
		{
			name: "string-downcase with non-string",
			code: `(string-downcase 42)`,
		},
		// string-foldcase errors
		{
			name: "string-foldcase with non-string",
			code: `(string-foldcase 42)`,
		},
		{
			name: "string-foldcase with symbol",
			code: `(string-foldcase 'foo)`,
		},
		// string comparison errors
		{
			name: "string=? with non-string first arg",
			code: `(string=? 42 "hello")`,
		},
		{
			name: "string=? with non-string second arg",
			code: `(string=? "hello" 42)`,
		},
		{
			name: "string<? with non-string first arg",
			code: `(string<? 42 "hello")`,
		},
		{
			name: "string<? with non-string second arg",
			code: `(string<? "hello" 42)`,
		},
		{
			name: "string>? with non-string first arg",
			code: `(string>? 42 "hello")`,
		},
		{
			name: "string>? with non-string second arg",
			code: `(string>? "hello" 42)`,
		},
		{
			name: "string<=? with non-string first arg",
			code: `(string<=? 42 "hello")`,
		},
		{
			name: "string<=? with non-string second arg",
			code: `(string<=? "hello" 42)`,
		},
		{
			name: "string>=? with non-string first arg",
			code: `(string>=? 42 "hello")`,
		},
		{
			name: "string>=? with non-string second arg",
			code: `(string>=? "hello" 42)`,
		},
		// string-ci comparison errors
		{
			name: "string-ci=? with non-string first arg",
			code: `(string-ci=? 42 "hello")`,
		},
		{
			name: "string-ci=? with non-string second arg",
			code: `(string-ci=? "hello" 42)`,
		},
		{
			name: "string-ci<? with non-string first arg",
			code: `(string-ci<? 42 "hello")`,
		},
		{
			name: "string-ci<? with non-string second arg",
			code: `(string-ci<? "hello" 42)`,
		},
		{
			name: "string-ci>? with non-string first arg",
			code: `(string-ci>? 42 "hello")`,
		},
		{
			name: "string-ci>? with non-string second arg",
			code: `(string-ci>? "hello" 42)`,
		},
		{
			name: "string-ci<=? with non-string first arg",
			code: `(string-ci<=? 42 "hello")`,
		},
		{
			name: "string-ci<=? with non-string second arg",
			code: `(string-ci<=? "hello" 42)`,
		},
		{
			name: "string-ci>=? with non-string first arg",
			code: `(string-ci>=? 42 "hello")`,
		},
		{
			name: "string-ci>=? with non-string second arg",
			code: `(string-ci>=? "hello" 42)`,
		},
		// string->symbol errors
		{
			name: "string->symbol with non-string",
			code: `(string->symbol 42)`,
		},
		{
			name: "string->symbol with symbol",
			code: `(string->symbol 'foo)`,
		},
		// symbol->string errors
		{
			name: "symbol->string with non-symbol",
			code: `(symbol->string "foo")`,
		},
		{
			name: "symbol->string with integer",
			code: `(symbol->string 42)`,
		},
		// number->string errors
		{
			name: "number->string with non-number",
			code: `(number->string "42")`,
		},
		{
			name: "number->string with symbol",
			code: `(number->string 'foo)`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// Unicode character tests

func TestCharUnicode(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// char->integer with Unicode
		{
			name:     "char->integer with Greek alpha",
			code:     `(char->integer #\α)`,
			expected: values.NewInteger(945), // α = U+03B1
		},
		// integer->char with Unicode
		{
			name:     "integer->char to Greek alpha",
			code:     `(integer->char 945)`,
			expected: values.NewCharacter('α'),
		},
		// char-upcase with Unicode
		{
			name:     "char-upcase Greek",
			code:     `(char-upcase #\α)`,
			expected: values.NewCharacter('Α'),
		},
		// char-downcase with Unicode
		{
			name:     "char-downcase Greek",
			code:     `(char-downcase #\Α)`,
			expected: values.NewCharacter('α'),
		},
		// char-alphabetic? with Unicode
		{
			name:     "char-alphabetic? with Chinese",
			code:     `(char-alphabetic? #\中)`,
			expected: values.TrueValue,
		},
		{
			name:     "char-alphabetic? with Greek",
			code:     `(char-alphabetic? #\α)`,
			expected: values.TrueValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

// R7RS string mutation tests

func TestStringSet(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{
			name:     "string-set! first char",
			code:     `(let ((s (string-copy "hello"))) (string-set! s 0 #\H) s)`,
			expected: values.NewString("Hello"),
		},
		{
			name:     "string-set! last char",
			code:     `(let ((s (string-copy "hello"))) (string-set! s 4 #\O) s)`,
			expected: values.NewString("hellO"),
		},
		{
			name:     "string-set! middle char",
			code:     `(let ((s (string-copy "hello"))) (string-set! s 2 #\L) s)`,
			expected: values.NewString("heLlo"),
		},
		{
			name:     "string-set! returns void",
			code:     `(let ((s (string-copy "hello"))) (string-set! s 0 #\H))`,
			expected: values.Void,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestStringSetErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "string-set! with non-string", code: `(string-set! 42 0 #\a)`},
		{name: "string-set! with non-integer index", code: `(string-set! "hello" "0" #\a)`},
		{name: "string-set! with non-character", code: `(string-set! "hello" 0 "a")`},
		{name: "string-set! index out of bounds", code: `(string-set! "hello" 5 #\a)`},
		{name: "string-set! negative index", code: `(string-set! "hello" -1 #\a)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

func TestStringFill(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{
			name:     "string-fill! entire string",
			code:     `(let ((s (string-copy "hello"))) (string-fill! s #\z) s)`,
			expected: values.NewString("zzzzz"),
		},
		{
			name:     "string-fill! with start",
			code:     `(let ((s (string-copy "hello"))) (string-fill! s #\z 2) s)`,
			expected: values.NewString("hezzz"),
		},
		{
			name:     "string-fill! with start and end",
			code:     `(let ((s (string-copy "hello"))) (string-fill! s #\z 1 4) s)`,
			expected: values.NewString("hzzzo"),
		},
		{
			name:     "string-fill! returns void",
			code:     `(let ((s (string-copy "hello"))) (string-fill! s #\z))`,
			expected: values.Void,
		},
		{
			name:     "string-fill! empty range",
			code:     `(let ((s (string-copy "hello"))) (string-fill! s #\z 2 2) s)`,
			expected: values.NewString("hello"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestStringFillErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "string-fill! with non-string", code: `(string-fill! 42 #\z)`},
		{name: "string-fill! with non-character", code: `(string-fill! "hello" "x")`},
		{name: "string-fill! start out of bounds", code: `(string-fill! "hello" #\z 6)`},
		{name: "string-fill! end out of bounds", code: `(string-fill! "hello" #\z 0 6)`},
		{name: "string-fill! start > end", code: `(string-fill! "hello" #\z 3 2)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

func TestStringCopyTo(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{
			name:     "string-copy! basic",
			code:     `(let ((to (string-copy "12345"))) (string-copy! to 0 "abc") to)`,
			expected: values.NewString("abc45"),
		},
		{
			name:     "string-copy! with offset",
			code:     `(let ((to (string-copy "12345"))) (string-copy! to 2 "abc") to)`,
			expected: values.NewString("12abc"),
		},
		{
			name:     "string-copy! with start",
			code:     `(let ((to (string-copy "12345"))) (string-copy! to 0 "abcde" 2) to)`,
			expected: values.NewString("cde45"),
		},
		{
			name:     "string-copy! with start and end",
			code:     `(let ((to (string-copy "12345"))) (string-copy! to 1 "abcde" 1 3) to)`,
			expected: values.NewString("1bc45"),
		},
		{
			name:     "string-copy! returns void",
			code:     `(let ((to (string-copy "12345"))) (string-copy! to 0 "abc"))`,
			expected: values.Void,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestStringCopyToErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "string-copy! to non-string", code: `(string-copy! 42 0 "abc")`},
		{name: "string-copy! at non-integer", code: `(string-copy! "12345" "0" "abc")`},
		{name: "string-copy! from non-string", code: `(string-copy! "12345" 0 42)`},
		{name: "string-copy! destination overflow", code: `(string-copy! "123" 2 "abcde")`},
		{name: "string-copy! invalid source range", code: `(string-copy! "12345" 0 "abc" 2 1)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

func TestStringMap(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{
			name:     "string-map single string",
			code:     `(string-map char-upcase "hello")`,
			expected: values.NewString("HELLO"),
		},
		{
			name:     "string-map with lambda",
			code:     `(string-map (lambda (c) (integer->char (+ 1 (char->integer c)))) "abc")`,
			expected: values.NewString("bcd"),
		},
		{
			name:     "string-map empty string",
			code:     `(string-map char-upcase "")`,
			expected: values.NewString(""),
		},
		{
			name:     "string-map two strings",
			code:     `(string-map (lambda (a b) (if (char<? a b) b a)) "abc" "bac")`,
			expected: values.NewString("bbc"),
		},
		// Single char
		{
			name:     "string-map single char",
			code:     `(string-map char-upcase "a")`,
			expected: values.NewString("A"),
		},
		// Unequal lengths - stops at shortest
		{
			name:     "string-map unequal lengths",
			code:     `(string-map (lambda (a b) a) "abcde" "xy")`,
			expected: values.NewString("ab"),
		},
		// Three strings
		{
			name:     "string-map three strings",
			code:     `(string-map (lambda (a b c) a) "abc" "def" "ghi")`,
			expected: values.NewString("abc"),
		},
		// Unicode identity
		{
			name:     "string-map Unicode identity",
			code:     "(string-map (lambda (c) c) \"\u03b1\u03b2\u03b3\")",
			expected: values.NewString("\u03b1\u03b2\u03b3"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestStringMapErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "string-map non-procedure", code: `(string-map 42 "hello")`},
		{name: "string-map non-string", code: `(string-map char-upcase 42)`},
		{name: "string-map proc returns non-char", code: `(string-map (lambda (c) 42) "hello")`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

func TestStringForEach(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{
			name:     "string-for-each returns void",
			code:     `(string-for-each (lambda (c) c) "hello")`,
			expected: values.Void,
		},
		{
			name:     "string-for-each with side effect",
			code:     `(let ((count 0)) (string-for-each (lambda (c) (set! count (+ count 1))) "hello") count)`,
			expected: values.NewInteger(5),
		},
		{
			name:     "string-for-each empty string",
			code:     `(let ((count 0)) (string-for-each (lambda (c) (set! count (+ count 1))) "") count)`,
			expected: values.NewInteger(0),
		},
		// Order verification
		{
			name:     "string-for-each order verification",
			code:     `(let ((result '())) (string-for-each (lambda (c) (set! result (cons c result))) "abc") result)`,
			expected: values.List(values.NewCharacter('c'), values.NewCharacter('b'), values.NewCharacter('a')),
		},
		// Two strings with side effects
		{
			name: "string-for-each two strings",
			code: `(let ((result '())) (string-for-each (lambda (a b) (set! result (cons (list a b) result))) "ab" "xy") result)`,
			expected: values.List(
				values.List(values.NewCharacter('b'), values.NewCharacter('y')),
				values.List(values.NewCharacter('a'), values.NewCharacter('x'))),
		},
		// Unequal lengths - stops at shortest
		{
			name:     "string-for-each unequal lengths",
			code:     `(let ((count 0)) (string-for-each (lambda (a b) (set! count (+ count 1))) "abcde" "xy") count)`,
			expected: values.NewInteger(2),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestStringForEachErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "string-for-each non-procedure", code: `(string-for-each 42 "hello")`},
		{name: "string-for-each non-string", code: `(string-for-each (lambda (c) c) 42)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

func TestStringToListOptional(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Basic (no optional args)
		{name: "string->list full", code: `(string->list "hello")`,
			expected: values.List(values.NewCharacter('h'), values.NewCharacter('e'), values.NewCharacter('l'), values.NewCharacter('l'), values.NewCharacter('o'))},
		{name: "string->list empty", code: `(string->list "")`, expected: values.EmptyList},
		// With start argument
		{name: "string->list with start", code: `(string->list "hello" 2)`,
			expected: values.List(values.NewCharacter('l'), values.NewCharacter('l'), values.NewCharacter('o'))},
		{name: "string->list with start at 0", code: `(string->list "hello" 0)`,
			expected: values.List(values.NewCharacter('h'), values.NewCharacter('e'), values.NewCharacter('l'), values.NewCharacter('l'), values.NewCharacter('o'))},
		{name: "string->list with start at end", code: `(string->list "hello" 5)`, expected: values.EmptyList},
		// With start and end arguments
		{name: "string->list with start and end", code: `(string->list "hello" 1 4)`,
			expected: values.List(values.NewCharacter('e'), values.NewCharacter('l'), values.NewCharacter('l'))},
		{name: "string->list with start and end full", code: `(string->list "hello" 0 5)`,
			expected: values.List(values.NewCharacter('h'), values.NewCharacter('e'), values.NewCharacter('l'), values.NewCharacter('l'), values.NewCharacter('o'))},
		{name: "string->list with start equals end", code: `(string->list "hello" 2 2)`, expected: values.EmptyList},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestStringToListErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "string->list with non-string", code: `(string->list 42)`},
		{name: "string->list with non-integer start", code: `(string->list "hello" "x")`},
		{name: "string->list with non-integer end", code: `(string->list "hello" 0 "x")`},
		{name: "string->list with negative start", code: `(string->list "hello" -1)`},
		{name: "string->list with end out of bounds", code: `(string->list "hello" 0 10)`},
		{name: "string->list with start > end", code: `(string->list "hello" 3 1)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
