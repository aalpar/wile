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
	"testing"

	"wile/values"

	qt "github.com/frankban/quicktest"
)

// open-binary-input-file Tests (R7RS §6.13.1)

func TestOpenBinaryInputFile(t *testing.T) {
	// Create a temp file with binary content
	f, err := os.CreateTemp("", "test_obif_*.bin")
	qt.Assert(t, err, qt.IsNil)
	_, err = f.Write([]byte{0x00, 0x01, 0x02, 0xFF})
	qt.Assert(t, err, qt.IsNil)
	err = f.Close()
	qt.Assert(t, err, qt.IsNil)
	defer os.Remove(f.Name()) //nolint:errcheck

	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "open-binary-input-file returns input port",
			code: fmt.Sprintf(`(input-port? (open-binary-input-file %q))`, f.Name()),
			out:  values.TrueValue,
		},
		// Note: port? does not currently recognize BinaryInputPort as a port.
		// This is an implementation limitation.
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestOpenBinaryInputFileAndClose(t *testing.T) {
	f, err := os.CreateTemp("", "test_obif_close_*.bin")
	qt.Assert(t, err, qt.IsNil)
	_, err = f.Write([]byte{0x00})
	qt.Assert(t, err, qt.IsNil)
	err = f.Close()
	qt.Assert(t, err, qt.IsNil)
	defer os.Remove(f.Name()) //nolint:errcheck

	code := fmt.Sprintf(`(let ((p (open-binary-input-file %q)))
		(close-port p)
		#t)`, f.Name())

	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestOpenBinaryInputFileErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{
			name: "file not found",
			code: `(open-binary-input-file "/nonexistent/file.bin")`,
		},
		{
			name: "wrong type - integer",
			code: `(open-binary-input-file 42)`,
		},
		{
			name: "wrong type - symbol",
			code: `(open-binary-input-file 'foo)`,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
