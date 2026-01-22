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
	"fmt"
	"os"
	"path/filepath"
	"testing"

	"wile/values"

	qt "github.com/frankban/quicktest"
)

// open-binary-output-file Tests (R7RS §6.13.1)

func TestOpenBinaryOutputFile(t *testing.T) {
	tmpfile := filepath.Join(os.TempDir(), "test_obof.bin")
	defer os.Remove(tmpfile) //nolint:errcheck

	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "open-binary-output-file returns output port",
			code: fmt.Sprintf(`(output-port? (open-binary-output-file "%s"))`, tmpfile),
			out:  values.TrueValue,
		},
		// Note: port? does not currently recognize BinaryOutputPort as a port.
		// This is an implementation limitation.
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)

			// Clean up for next test
			os.Remove(tmpfile) //nolint:errcheck
		})
	}
}

func TestOpenBinaryOutputFileAndClose(t *testing.T) {
	tmpfile := filepath.Join(os.TempDir(), "test_obof_close.bin")
	defer os.Remove(tmpfile) //nolint:errcheck

	code := fmt.Sprintf(`(let ((p (open-binary-output-file "%s")))
		(close-port p)
		#t)`, tmpfile)

	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestOpenBinaryOutputFileErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{
			name: "wrong type - integer",
			code: `(open-binary-output-file 42)`,
		},
		{
			name: "wrong type - symbol",
			code: `(open-binary-output-file 'foo)`,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
