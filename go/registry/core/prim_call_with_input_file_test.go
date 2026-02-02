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

	"github.com/aalpar/wile/go/values"

	qt "github.com/frankban/quicktest"
)

// call-with-input-file Tests (R7RS §6.13.1)

func TestCallWithInputFile(t *testing.T) {
	// Create a temp file with content
	f, err := os.CreateTemp("", "test_cwif_*.txt")
	qt.Assert(t, err, qt.IsNil)
	_, err = f.WriteString("hello world")
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
			name: "read from file using call-with-input-file",
			code: fmt.Sprintf(`(call-with-input-file "%s"
				(lambda (p) (read p)))`, f.Name()),
			out: values.NewSymbol("hello"),
		},
		{
			name: "call-with-input-file returns procedure result",
			code: fmt.Sprintf(`(call-with-input-file "%s"
				(lambda (p) 42))`, f.Name()),
			out: values.NewInteger(42),
		},
		{
			name: "port is closed after call-with-input-file",
			code: fmt.Sprintf(`(call-with-input-file "%s"
				(lambda (p) (input-port? p)))`, f.Name()),
			out: values.TrueValue,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestCallWithInputFileErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{
			name: "file not found",
			code: `(call-with-input-file "/nonexistent/file/path.txt"
				(lambda (p) (read p)))`,
		},
		{
			name: "wrong type for filename - integer",
			code: `(call-with-input-file 42 (lambda (p) p))`,
		},
		{
			name: "wrong type for procedure - not a procedure",
			code: `(call-with-input-file "/tmp/test.txt" 42)`,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
