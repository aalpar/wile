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
	"fmt"
	"os"
	"path/filepath"
	"testing"

	qt "github.com/frankban/quicktest"

	"wile/values"
)

func TestOpenInputFile(t *testing.T) {
	f, err := os.CreateTemp("", "test*.txt")
	qt.Assert(t, err, qt.IsNil)
	_, err = f.WriteString("hello world")
	qt.Assert(t, err, qt.IsNil)
	err = f.Close()
	qt.Assert(t, err, qt.IsNil)
	defer os.Remove(f.Name()) //nolint:errcheck

	code := fmt.Sprintf(`(let ((p (open-input-file "%s")))
		(close-port p)
		#t)`, f.Name())
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestOpenInputFileAndRead(t *testing.T) {
	f, err := os.CreateTemp("", "test*.txt")
	qt.Assert(t, err, qt.IsNil)
	_, err = f.WriteString("(+ 1 2)")
	qt.Assert(t, err, qt.IsNil)
	err = f.Close()
	qt.Assert(t, err, qt.IsNil)
	defer os.Remove(f.Name()) //nolint:errcheck

	code := fmt.Sprintf(`(let ((p (open-input-file "%s")))
		(let ((data (read p)))
			(close-port p)
			data))`, f.Name())
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	expected := values.List(values.NewSymbol("+"), values.NewInteger(1), values.NewInteger(2))
	qt.Assert(t, result, values.SchemeEquals, expected)
}

func TestOpenOutputFile(t *testing.T) {
	tmpfile := filepath.Join(os.TempDir(), "scheme_test_output.txt")
	defer os.Remove(tmpfile) //nolint:errcheck

	code := fmt.Sprintf(`(let ((p (open-output-file "%s")))
		(display "hello" p)
		(close-port p)
		#t)`, tmpfile)
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)

	content, err := os.ReadFile(tmpfile)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, string(content), qt.Equals, "hello")
}

func TestOpenOutputFileWithMultipleWrites(t *testing.T) {
	tmpfile := filepath.Join(os.TempDir(), "scheme_test_output2.txt")
	defer os.Remove(tmpfile) //nolint:errcheck

	code := fmt.Sprintf(`(let ((p (open-output-file "%s")))
		(display "hello" p)
		(display " " p)
		(display "world" p)
		(close-port p)
		#t)`, tmpfile)
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)

	content, err := os.ReadFile(tmpfile)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, string(content), qt.Equals, "hello world")
}

func TestClosePortWithFileInputPort(t *testing.T) {
	f, err := os.CreateTemp("", "test*.txt")
	qt.Assert(t, err, qt.IsNil)
	_, err = f.WriteString("test data")
	qt.Assert(t, err, qt.IsNil)
	err = f.Close()
	qt.Assert(t, err, qt.IsNil)
	defer os.Remove(f.Name()) //nolint:errcheck

	code := fmt.Sprintf(`(let ((p (open-input-file "%s")))
		(close-port p)
		#t)`, f.Name())
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestClosePortWithFileOutputPort(t *testing.T) {
	tmpfile := filepath.Join(os.TempDir(), "scheme_test_close_output.txt")
	defer os.Remove(tmpfile) //nolint:errcheck

	code := fmt.Sprintf(`(let ((p (open-output-file "%s")))
		(display "test" p)
		(close-port p)
		#t)`, tmpfile)
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)

	content, err := os.ReadFile(tmpfile)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, string(content), qt.Equals, "test")
}
