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
	"os"
	"path/filepath"
	"testing"

	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

// call-with-output-file Tests (R7RS §6.13.1)

func TestCallWithOutputFile(t *testing.T) {
	tmpfile := filepath.Join(os.TempDir(), "test_cwof.txt")
	defer os.Remove(tmpfile) //nolint:errcheck

	tcs := []struct {
		name     string
		code     string
		expected string // expected file content
	}{
		{
			name: "write to file using call-with-output-file",
			code: fmt.Sprintf(`(call-with-output-file "%s"
				(lambda (p) (display "hello" p)))`, tmpfile),
			expected: "hello",
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)

			content, err := os.ReadFile(tmpfile)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, string(content), qt.Equals, tc.expected)

			// Clean up for next test
			os.Remove(tmpfile) //nolint:errcheck
		})
	}
}

func TestCallWithOutputFileReturnsResult(t *testing.T) {
	tmpfile := filepath.Join(os.TempDir(), "test_cwof_return.txt")
	defer os.Remove(tmpfile) //nolint:errcheck

	code := fmt.Sprintf(`(call-with-output-file "%s"
		(lambda (p)
			(display "test" p)
			'done))`, tmpfile)

	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.NewSymbol("done"))
}

func TestCallWithOutputFileErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{
			name: "wrong type for filename - integer",
			code: `(call-with-output-file 42 (lambda (p) p))`,
		},
		{
			name: "wrong type for procedure - not a procedure",
			code: `(call-with-output-file "/tmp/test.txt" "not-a-proc")`,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
