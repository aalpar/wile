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
	"testing"

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// write-shared Tests (R7RS §6.13.3)

func TestWriteShared(t *testing.T) {
	tcs := []struct {
		name     string
		code     string
		expected string
	}{
		{
			name: "write-shared integer",
			code: `(let ((p (open-output-string)))
				(write-shared 42 p)
				(get-output-string p))`,
			expected: "42",
		},
		{
			name: "write-shared string",
			code: `(let ((p (open-output-string)))
				(write-shared "hello" p)
				(get-output-string p))`,
			expected: `"hello"`,
		},
		{
			name: "write-shared symbol",
			code: `(let ((p (open-output-string)))
				(write-shared 'bar p)
				(get-output-string p))`,
			expected: "bar",
		},
		{
			name: "write-shared list",
			code: `(let ((p (open-output-string)))
				(write-shared '(a b c) p)
				(get-output-string p))`,
			expected: "(a b c)",
		},
		{
			name: "write-shared vector",
			code: `(let ((p (open-output-string)))
				(write-shared #(1 2 3) p)
				(get-output-string p))`,
			expected: "#(1 2 3)",
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

func TestWriteSharedDefaultPort(t *testing.T) {
	// write-shared with only one argument should use current-output-port
	result, err := runSchemeCode(t, `(begin (write-shared 42) #t)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
}
