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
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// with-output-to-file Tests (R7RS §6.13.1)

func TestWithOutputToFile(t *testing.T) {
	tmpfile := filepath.Join(os.TempDir(), "test_wotf.txt")
	defer os.Remove(tmpfile) //nolint:errcheck

	// with-output-to-file should change current-output-port for the thunk
	code := fmt.Sprintf(`(with-output-to-file "%s"
		(lambda () (display "hello")))`, tmpfile)

	_, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)

	content, err := os.ReadFile(tmpfile)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, string(content), qt.Equals, "hello")
}

func TestWithOutputToFileReturnsResult(t *testing.T) {
	tmpfile := filepath.Join(os.TempDir(), "test_wotf_return.txt")
	defer os.Remove(tmpfile) //nolint:errcheck

	code := fmt.Sprintf(`(with-output-to-file "%s"
		(lambda ()
			(display "test")
			'result))`, tmpfile)

	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewSymbol("result"))
}

func TestWithOutputToFileErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{
			name: "wrong type for filename",
			code: `(with-output-to-file 42 (lambda () #t))`,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
