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
	"testing"

	qt "github.com/frankban/quicktest"

	"wile/values"
)

func TestDeleteFile(t *testing.T) {
	f, err := os.CreateTemp("", "test*.txt")
	qt.Assert(t, err, qt.IsNil)
	f.WriteString("test") //nolint:errcheck
	f.Close()             //nolint:errcheck
	path := f.Name()

	code := fmt.Sprintf(`(begin (delete-file "%s") #t)`, path)
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)

	// Verify file was deleted
	_, err = os.Stat(path)
	qt.Assert(t, os.IsNotExist(err), qt.IsTrue)
}

func TestDeleteFileErrorWithNonexistentFile(t *testing.T) {
	code := `(delete-file "/nonexistent/path/file-xyz-12345.txt")`
	_, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.Not(qt.IsNil), qt.Commentf("expected error when deleting nonexistent file"))
}

func TestDeleteFileErrorWithNonString(t *testing.T) {
	code := `(delete-file 42)`
	_, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.Not(qt.IsNil), qt.Commentf("expected error when passing non-string"))
}

func TestLoad(t *testing.T) {
	f, err := os.CreateTemp("", "test*.scm")
	qt.Assert(t, err, qt.IsNil)
	f.WriteString("42")       //nolint:errcheck
	f.Close()                 //nolint:errcheck
	defer os.Remove(f.Name()) //nolint:errcheck

	code := fmt.Sprintf(`(load "%s")`, f.Name())
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.NewInteger(42))
}

func TestLoadWithMultipleExpressions(t *testing.T) {
	f, err := os.CreateTemp("", "test*.scm")
	qt.Assert(t, err, qt.IsNil)
	f.WriteString("(define x 10)\n(define y 20)\n(+ x y)") //nolint:errcheck
	f.Close()                                              //nolint:errcheck
	defer os.Remove(f.Name())                              //nolint:errcheck

	code := fmt.Sprintf(`(load "%s")`, f.Name())
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	// Should return the value of the last expression
	qt.Assert(t, result, values.SchemeEquals, values.NewInteger(30))
}

func TestLoadReturnsLastValue(t *testing.T) {
	f, err := os.CreateTemp("", "test*.scm")
	qt.Assert(t, err, qt.IsNil)
	f.WriteString("1\n2\n3")  //nolint:errcheck
	f.Close()                 //nolint:errcheck
	defer os.Remove(f.Name()) //nolint:errcheck

	code := fmt.Sprintf(`(load "%s")`, f.Name())
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.NewInteger(3))
}

func TestLoadWithEmptyFile(t *testing.T) {
	f, err := os.CreateTemp("", "test*.scm")
	qt.Assert(t, err, qt.IsNil)
	f.Close()                 //nolint:errcheck
	defer os.Remove(f.Name()) //nolint:errcheck

	code := fmt.Sprintf(`(load "%s")`, f.Name())
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	// Empty file should return void
	qt.Assert(t, result.IsVoid(), qt.IsTrue)
}

func TestLoadErrorWithNonexistentFile(t *testing.T) {
	code := `(load "/nonexistent/path/file-xyz-12345.scm")`
	_, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.Not(qt.IsNil), qt.Commentf("expected error when loading nonexistent file"))
}

func TestLoadErrorWithNonString(t *testing.T) {
	code := `(load 42)`
	_, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.Not(qt.IsNil), qt.Commentf("expected error when passing non-string"))
}

func TestLoadDefinesVariableInTopLevel(t *testing.T) {
	f, err := os.CreateTemp("", "test*.scm")
	qt.Assert(t, err, qt.IsNil)
	f.WriteString("(define loaded-var 99)") //nolint:errcheck
	f.Close()                               //nolint:errcheck
	defer os.Remove(f.Name())               //nolint:errcheck

	code := fmt.Sprintf(`(load "%s")`, f.Name())
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	// define returns void
	qt.Assert(t, result.IsVoid(), qt.IsTrue)
}

func TestLoadAccessesTopLevelEnvironment(t *testing.T) {
	// Test that load can access variables defined in the top-level environment
	f, err := os.CreateTemp("", "test*.scm")
	qt.Assert(t, err, qt.IsNil)
	f.WriteString("(+ global-var 10)") //nolint:errcheck
	f.Close()                          //nolint:errcheck
	defer os.Remove(f.Name())          //nolint:errcheck

	code := fmt.Sprintf(`(begin (define global-var 32) (load "%s"))`, f.Name())
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.NewInteger(42))
}
