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

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

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
			code: fmt.Sprintf(`(output-port? (open-binary-output-file %q))`, tmpfile),
			out:  values.TrueValue,
		},
		// Note: port? does not currently recognize BinaryOutputPort as a port.
		// This is an implementation limitation.
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)

			// Clean up for next test
			os.Remove(tmpfile) //nolint:errcheck
		})
	}
}

func TestOpenBinaryOutputFileAndClose(t *testing.T) {
	tmpfile := filepath.Join(os.TempDir(), "test_obof_close.bin")
	defer os.Remove(tmpfile) //nolint:errcheck

	code := fmt.Sprintf(`(let ((p (open-binary-output-file %q)))
		(close-port p)
		#t)`, tmpfile)

	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
}

func TestOpenBinaryOutputFileErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{
			Name: "wrong type - integer",
			Code: `(open-binary-output-file 42)`,
		},
		{
			Name: "wrong type - symbol",
			Code: `(open-binary-output-file 'foo)`,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
