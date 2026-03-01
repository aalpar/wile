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
	"testing"

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// with-input-from-file Tests (R7RS §6.13.1)

func TestWithInputFromFile(t *testing.T) {
	// Create a temp file with content
	f, err := os.CreateTemp("", "test_wiff_*.txt")
	qt.Assert(t, err, qt.IsNil)
	_, err = f.WriteString("42")
	qt.Assert(t, err, qt.IsNil)
	err = f.Close()
	qt.Assert(t, err, qt.IsNil)
	defer os.Remove(f.Name()) //nolint:errcheck

	// with-input-from-file should change current-input-port for the thunk
	code := fmt.Sprintf(`(with-input-from-file "%s"
		(lambda () (read)))`, f.Name())

	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewInteger(42))
}

func TestWithInputFromFileReturnsResult(t *testing.T) {
	f, err := os.CreateTemp("", "test_wiff_return_*.txt")
	qt.Assert(t, err, qt.IsNil)
	_, err = f.WriteString("ignored")
	qt.Assert(t, err, qt.IsNil)
	err = f.Close()
	qt.Assert(t, err, qt.IsNil)
	defer os.Remove(f.Name()) //nolint:errcheck

	code := fmt.Sprintf(`(with-input-from-file "%s"
		(lambda () 'done))`, f.Name())

	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewSymbol("done"))
}

func TestWithInputFromFileErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{
			Name: "file not found",
			Code: `(with-input-from-file "/nonexistent/path.txt" (lambda () #t))`,
		},
		{
			Name: "wrong type for filename",
			Code: `(with-input-from-file 42 (lambda () #t))`,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
