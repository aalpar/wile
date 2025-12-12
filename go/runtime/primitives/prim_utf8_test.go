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

func TestStringToUtf8Basic(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "string->utf8 hello",
			code: `(equal? (string->utf8 "hello") #u8(104 101 108 108 111))`,
			out:  values.TrueValue,
		},
		{
			name: "string->utf8 empty string",
			code: `(bytevector-length (string->utf8 ""))`,
			out:  values.NewInteger(0),
		},
		{
			name: "string->utf8 single char",
			code: `(equal? (string->utf8 "A") #u8(65))`,
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

func TestStringToUtf8WithIndices(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "string->utf8 with start index",
			code: `(equal? (string->utf8 "hello" 1) #u8(101 108 108 111))`,
			out:  values.TrueValue,
		},
		{
			name: "string->utf8 with start and end",
			code: `(equal? (string->utf8 "hello" 1 3) #u8(101 108))`,
			out:  values.TrueValue,
		},
		{
			name: "string->utf8 start at 0",
			code: `(equal? (string->utf8 "hello" 0) #u8(104 101 108 108 111))`,
			out:  values.TrueValue,
		},
		{
			name: "string->utf8 start equals end",
			code: `(bytevector-length (string->utf8 "hello" 2 2))`,
			out:  values.NewInteger(0),
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

func TestStringToUtf8VerifyBytes(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "verify first byte of hello",
			code: `(bytevector-u8-ref (string->utf8 "hello") 0)`,
			out:  values.NewInteger(104),
		},
		{
			name: "verify last byte of hello",
			code: `(bytevector-u8-ref (string->utf8 "hello") 4)`,
			out:  values.NewInteger(111),
		},
		{
			name: "verify byte of A",
			code: `(bytevector-u8-ref (string->utf8 "A") 0)`,
			out:  values.NewInteger(65),
		},
		{
			name: "verify length of hello",
			code: `(bytevector-length (string->utf8 "hello"))`,
			out:  values.NewInteger(5),
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

func TestUtf8ToStringBasic(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "utf8->string hello",
			code: `(utf8->string #u8(104 101 108 108 111))`,
			out:  values.NewString("hello"),
		},
		{
			name: "utf8->string empty bytevector",
			code: `(utf8->string (bytevector))`,
			out:  values.NewString(""),
		},
		{
			name: "utf8->string single byte",
			code: `(utf8->string #u8(65))`,
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

func TestUtf8ToStringWithIndices(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "utf8->string with start index",
			code: `(utf8->string #u8(104 101 108 108 111) 1)`,
			out:  values.NewString("ello"),
		},
		{
			name: "utf8->string with start and end",
			code: `(utf8->string #u8(104 101 108 108 111) 1 3)`,
			out:  values.NewString("el"),
		},
		{
			name: "utf8->string start at 0",
			code: `(utf8->string #u8(104 101 108 108 111) 0)`,
			out:  values.NewString("hello"),
		},
		{
			name: "utf8->string start equals end",
			code: `(utf8->string #u8(104 101 108 108 111) 2 2)`,
			out:  values.NewString(""),
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

func TestUtf8ToStringVerifyResult(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "verify string length",
			code: `(string-length (utf8->string #u8(104 101 108 108 111)))`,
			out:  values.NewInteger(5),
		},
		{
			name: "verify first character",
			code: `(string-ref (utf8->string #u8(104 101 108 108 111)) 0)`,
			out:  values.NewCharacter('h'),
		},
		{
			name: "verify single char result",
			code: `(string-ref (utf8->string #u8(65)) 0)`,
			out:  values.NewCharacter('A'),
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

func TestUtf8RoundTrip(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "string->utf8->string hello",
			code: `(utf8->string (string->utf8 "hello"))`,
			out:  values.NewString("hello"),
		},
		{
			name: "string->utf8->string empty",
			code: `(utf8->string (string->utf8 ""))`,
			out:  values.NewString(""),
		},
		{
			name: "string->utf8->string single char",
			code: `(utf8->string (string->utf8 "A"))`,
			out:  values.NewString("A"),
		},
		{
			name: "string->utf8->string with indices",
			code: `(utf8->string (string->utf8 "hello" 1 4))`,
			out:  values.NewString("ell"),
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

func TestUtf8WithMultiByteCharacters(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "utf8->string with multi-byte char (lambda λ)",
			code: `(utf8->string #u8(206 187))`,
			out:  values.NewString("λ"),
		},
		{
			name: "string->utf8 with multi-byte char (lambda λ)",
			code: `(equal? (string->utf8 "λ") #u8(206 187))`,
			out:  values.TrueValue,
		},
		{
			name: "utf8 round trip with emoji",
			code: `(utf8->string (string->utf8 "👍"))`,
			out:  values.NewString("👍"),
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

func TestUtf8EdgeCases(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "string->utf8 with substring range covers full string",
			code: `(equal? (string->utf8 "hello" 0 5) #u8(104 101 108 108 111))`,
			out:  values.TrueValue,
		},
		{
			name: "utf8->string with range covers full bytevector",
			code: `(utf8->string #u8(104 101 108 108 111) 0 5)`,
			out:  values.NewString("hello"),
		},
		{
			name: "verify bytevector equality",
			code: `(equal? (string->utf8 "test") (string->utf8 "test"))`,
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
