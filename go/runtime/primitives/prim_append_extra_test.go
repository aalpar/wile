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

	"wile/values"

	qt "github.com/frankban/quicktest"
)

func TestBytevectorAppendExtra(t *testing.T) {
	tcs := []struct {
		name   string
		code   string
		expect string
	}{
		{
			name:   "bytevector-append two bytevectors",
			code:   `(bytevector-append (bytevector 1 2) (bytevector 3 4))`,
			expect: "#u8( 1 2 3 4 )",
		},
		{
			name:   "bytevector-append three args",
			code:   `(bytevector-append (bytevector 1) (bytevector 2) (bytevector 3))`,
			expect: "#u8( 1 2 3 )",
		},
		{
			name:   "bytevector-append single empty",
			code:   `(bytevector-append (bytevector))`,
			expect: "#u8()",
		},
		{
			name:   "bytevector-append no args",
			code:   `(bytevector-append)`,
			expect: "#u8()",
		},
		{
			name:   "bytevector-append single arg",
			code:   `(bytevector-append (bytevector 1 2 3))`,
			expect: "#u8( 1 2 3 )",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.SchemeString(), qt.Equals, tc.expect)
		})
	}
}

func TestStringAppendExtra(t *testing.T) {
	tcs := []struct {
		name   string
		code   string
		expect string
	}{
		{
			name:   "string-append three strings",
			code:   `(string-append "hello" " " "world")`,
			expect: "hello world",
		},
		{
			name:   "string-append four args",
			code:   `(string-append "a" "b" "c" "d")`,
			expect: "abcd",
		},
		{
			name:   "string-append single empty",
			code:   `(string-append "")`,
			expect: "",
		},
		{
			name:   "string-append no args",
			code:   `(string-append)`,
			expect: "",
		},
		{
			name:   "string-append single arg",
			code:   `(string-append "hello")`,
			expect: "hello",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			str, ok := result.(*values.String)
			qt.Assert(t, ok, qt.IsTrue, qt.Commentf("expected String, got %T", result))
			qt.Assert(t, str.Value, qt.Equals, tc.expect)
		})
	}
}
