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
	port := values.NewCharacterOutputPortFromWriter(buf)
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
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgramAST(t, tc.prog)
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
			result, err := runProgramAST(t, tc.prog)
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
			result, err := runProgramAST(t, tc.prog)
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
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
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
		// string comparison errors
		{
			name: "string=? with non-strings",
			code: `(string=? "hello" 42)`,
		},
		{
			name: "string<? with non-strings",
			code: `(string<? 42 "hello")`,
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
