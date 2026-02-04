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

	"github.com/aalpar/wile/values"

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

	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.NewInteger(42))
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

	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.NewSymbol("done"))
}

func TestWithInputFromFileErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{
			name: "file not found",
			code: `(with-input-from-file "/nonexistent/path.txt" (lambda () #t))`,
		},
		{
			name: "wrong type for filename",
			code: `(with-input-from-file 42 (lambda () #t))`,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
