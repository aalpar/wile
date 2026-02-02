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

package core_test

import (
	"testing"

	"github.com/aalpar/wile/go/values"

	qt "github.com/frankban/quicktest"
)

// write-simple Tests (R7RS §6.13.3)

func TestWriteSimple(t *testing.T) {
	tcs := []struct {
		name     string
		code     string
		expected string
	}{
		{
			name: "write-simple integer",
			code: `(let ((p (open-output-string)))
				(write-simple 42 p)
				(get-output-string p))`,
			expected: "42",
		},
		{
			name: "write-simple string",
			code: `(let ((p (open-output-string)))
				(write-simple "hello" p)
				(get-output-string p))`,
			expected: `"hello"`,
		},
		{
			name: "write-simple symbol",
			code: `(let ((p (open-output-string)))
				(write-simple 'foo p)
				(get-output-string p))`,
			expected: "foo",
		},
		{
			name: "write-simple list",
			code: `(let ((p (open-output-string)))
				(write-simple '(1 2 3) p)
				(get-output-string p))`,
			expected: "(1 2 3)",
		},
		{
			name: "write-simple boolean true",
			code: `(let ((p (open-output-string)))
				(write-simple #t p)
				(get-output-string p))`,
			expected: "#t",
		},
		{
			name: "write-simple boolean false",
			code: `(let ((p (open-output-string)))
				(write-simple #f p)
				(get-output-string p))`,
			expected: "#f",
		},
		{
			name: "write-simple character",
			code: `(let ((p (open-output-string)))
				(write-simple #\A p)
				(get-output-string p))`,
			expected: `#\A`,
		},
		{
			name: "write-simple vector",
			code: `(let ((p (open-output-string)))
				(write-simple #(1 2 3) p)
				(get-output-string p))`,
			expected: "#( 1 2 3 )",
		},
		{
			name: "write-simple empty list",
			code: `(let ((p (open-output-string)))
				(write-simple '() p)
				(get-output-string p))`,
			expected: "()",
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			str, ok := result.(*values.String)
			qt.Assert(t, ok, qt.IsTrue)
			qt.Assert(t, str.Value, qt.Equals, tc.expected)
		})
	}
}

func TestWriteSimpleDefaultPort(t *testing.T) {
	// write-simple with only one argument should use current-output-port
	// We can't easily capture stdout, so we just verify it doesn't error
	result, err := runSchemeCode(t, `(begin (write-simple 42) #t)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}
