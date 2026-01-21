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
)

// I/O Error Condition Tests

func TestReadErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{
			name: "read from output port",
			code: `(read (open-output-string))`,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

func TestWriteErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{
			name: "write to input port",
			code: `(write 42 (open-input-string ""))`,
		},
		{
			name: "display to input port",
			code: `(display "hello" (open-input-string ""))`,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

func TestOpenInputFileErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{
			name: "open nonexistent file",
			code: `(open-input-file "/this/path/does/not/exist.txt")`,
		},
		{
			name: "wrong type - symbol",
			code: `(open-input-file 'foo)`,
		},
		{
			name: "wrong type - integer",
			code: `(open-input-file 42)`,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

func TestOpenOutputFileErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{
			name: "wrong type - symbol",
			code: `(open-output-file 'foo)`,
		},
		{
			name: "wrong type - integer",
			code: `(open-output-file 42)`,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
