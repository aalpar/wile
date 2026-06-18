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
	"fmt"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
)

// TestStringToUtf8CharacterIndices tests that string->utf8 uses character
// (not byte) indices for start/end parameters per R7RS §6.9.
func TestStringToUtf8CharacterIndices(t *testing.T) {
	c := qt.New(t)

	// Test string: "héllo"
	// Characters: h=0, é=1, l=2, l=3, o=4  (5 characters)
	// UTF-8 bytes: 68, c3 a9, 6c, 6c, 6f  (6 bytes: é is 2 bytes)

	tcs := []struct {
		name    string
		code    string
		wantHex string // Expected bytes in hex (space-separated)
	}{
		{
			name:    "full string",
			code:    `(string->utf8 "héllo")`,
			wantHex: "68 c3 a9 6c 6c 6f",
		},
		{
			name:    "chars 0-2 (hé)",
			code:    `(string->utf8 "héllo" 0 2)`,
			wantHex: "68 c3 a9", // h + é (3 bytes)
		},
		{
			name:    "chars 1-3 (él)",
			code:    `(string->utf8 "héllo" 1 3)`,
			wantHex: "c3 a9 6c", // é + l (3 bytes)
		},
		{
			name:    "chars 2-4 (ll)",
			code:    `(string->utf8 "héllo" 2 4)`,
			wantHex: "6c 6c", // l + l (2 bytes)
		},
		{
			name:    "chars 1-2 (é only)",
			code:    `(string->utf8 "héllo" 1 2)`,
			wantHex: "c3 a9", // é (2 bytes)
		},
		{
			name:    "emoji - chars 0-1 (😀)",
			code:    `(string->utf8 "😀test" 0 1)`,
			wantHex: "f0 9f 98 80", // 😀 is 4 bytes
		},
		{
			name:    "emoji - chars 1-2 (t)",
			code:    `(string->utf8 "😀test" 1 2)`,
			wantHex: "74", // 't' is 1 byte
		},
		{
			name:    "emoji - chars 1-5 (test)",
			code:    `(string->utf8 "😀test" 1 5)`,
			wantHex: "74 65 73 74", // "test"
		},
		{
			name:    "chinese - full string (你好)",
			code:    `(string->utf8 "你好")`,
			wantHex: "e4 bd a0 e5 a5 bd", // 你=e4bda0, 好=e5a5bd
		},
		{
			name:    "chinese - chars 0-1 (你)",
			code:    `(string->utf8 "你好" 0 1)`,
			wantHex: "e4 bd a0",
		},
		{
			name:    "chinese - chars 1-2 (好)",
			code:    `(string->utf8 "你好" 1 2)`,
			wantHex: "e5 a5 bd",
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			c.Assert(err, qt.IsNil, qt.Commentf("failed to run: %s", tc.code))

			bv, ok := result.(*values.ByteVector)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected bytevector, got %T", result))

			// Convert to hex string for comparison
			var gotHex strings.Builder
			for i := 0; i < len(*bv); i++ {
				if i > 0 {
					gotHex.WriteString(" ")
				}
				fmt.Fprintf(&gotHex, "%02x", (*bv)[i].Value)
			}

			c.Assert(gotHex.String(), qt.Equals, tc.wantHex,
				qt.Commentf("for code: %s", tc.code))
		})
	}
}

// TestStringToUtf8EdgeCases tests edge cases for string->utf8.
func TestStringToUtf8EdgeCases(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name    string
		code    string
		wantHex string
	}{
		{
			name:    "empty string",
			code:    `(string->utf8 "")`,
			wantHex: "",
		},
		{
			name:    "ASCII only",
			code:    `(string->utf8 "hello")`,
			wantHex: "68 65 6c 6c 6f",
		},
		{
			name:    "start = end (empty range)",
			code:    `(string->utf8 "test" 1 1)`,
			wantHex: "",
		},
		{
			name:    "single ASCII char",
			code:    `(string->utf8 "a")`,
			wantHex: "61",
		},
		{
			name:    "single multi-byte char",
			code:    `(string->utf8 "é")`,
			wantHex: "c3 a9",
		},
		{
			name:    "mixed ASCII and multi-byte",
			code:    `(string->utf8 "café")`,
			wantHex: "63 61 66 c3 a9", // c, a, f, é
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			c.Assert(err, qt.IsNil)

			bv, ok := result.(*values.ByteVector)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected bytevector, got %T", result))

			var gotHex strings.Builder
			for i := 0; i < len(*bv); i++ {
				if i > 0 {
					gotHex.WriteString(" ")
				}
				fmt.Fprintf(&gotHex, "%02x", (*bv)[i].Value)
			}

			c.Assert(gotHex.String(), qt.Equals, tc.wantHex)
		})
	}
}

// TestUtf8ToStringInvalid tests that utf8->string raises an error when
// the bytevector is not well-formed UTF-8 (R7RS §6.9).
func TestUtf8ToStringInvalid(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "lone continuation byte", Code: `(utf8->string #u8(#x80))`},
		{Name: "two lone continuation bytes", Code: `(utf8->string #u8(255 254 253))`},
		{Name: "truncated two-byte sequence", Code: `(utf8->string #u8(#xc3))`},
		{Name: "truncated three-byte sequence", Code: `(utf8->string #u8(#xe4 #xbd))`},
		{Name: "truncated four-byte sequence", Code: `(utf8->string #u8(#xf0 #x9f #x98))`},
		{Name: "overlong encoding", Code: `(utf8->string #u8(#xc0 #x80))`},
		{Name: "surrogate half (U+D800)", Code: `(utf8->string #u8(#xed #xa0 #x80))`},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// TestStringToUtf8RoundTrip verifies round-trip consistency.
func TestStringToUtf8RoundTrip(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name  string
		input string
		start int
		end   int
	}{
		{"ASCII", "hello", 1, 4},
		{"multi-byte", "héllo", 1, 3},
		{"emoji", "😀test😀", 1, 5},
		{"chinese", "你好世界", 1, 3},
		{"mixed", "a😀b你c", 0, 5},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			// Extract substring using character indices
			runes := []rune(tc.input)
			expected := string(runes[tc.start:tc.end])

			// Round trip through string->utf8 and utf8->string
			code := fmt.Sprintf(`(utf8->string (string->utf8 %q %d %d))`,
				tc.input, tc.start, tc.end)

			result, err := testhelpers.RunSchemeCode(t, code)
			c.Assert(err, qt.IsNil)

			str, ok := result.(*values.String)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected string, got %T", result))
			c.Assert(str.Value, qt.Equals, expected)
		})
	}
}
