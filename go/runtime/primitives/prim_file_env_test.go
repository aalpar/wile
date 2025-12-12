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
	"os"
	"testing"

	qt "github.com/frankban/quicktest"

	"wile/values"
)

func TestFileExistsWithExistingFile(t *testing.T) {
	result, err := runSchemeCode(t, `(file-exists? "test_helpers_test.go")`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestFileExistsWithNonexistentFile(t *testing.T) {
	result, err := runSchemeCode(t, `(file-exists? "nonexistent-file-xyz.txt")`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.FalseValue)
}

func TestGetEnvironmentVariableWithPATH(t *testing.T) {
	result, err := runSchemeCode(t, `(get-environment-variable "PATH")`)
	qt.Assert(t, err, qt.IsNil)

	// PATH should exist and be a string
	str, ok := result.(*values.String)
	qt.Assert(t, ok, qt.IsTrue, qt.Commentf("expected string, got %T", result))
	qt.Assert(t, len(str.Value) > 0, qt.IsTrue, qt.Commentf("PATH should not be empty"))
}

func TestGetEnvironmentVariableWithNonexistentVar(t *testing.T) {
	result, err := runSchemeCode(t, `(get-environment-variable "wile_NONEXISTENT_VAR_12345")`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.FalseValue)
}

func TestGetEnvironmentVariableWithTestVar(t *testing.T) {
	os.Setenv("wile_TEST_VAR", "test_value")   //nolint:errcheck
	defer os.Unsetenv("wile_TEST_VAR")         //nolint:errcheck

	result, err := runSchemeCode(t, `(get-environment-variable "wile_TEST_VAR")`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.NewString("test_value"))
}

func TestGetEnvironmentVariablesReturnsAlist(t *testing.T) {
	result, err := runSchemeCode(t, `(get-environment-variables)`)
	qt.Assert(t, err, qt.IsNil)

	// Result should be a list (pair or empty list)
	_, isPair := result.(*values.Pair)
	isEmptyList := result == values.EmptyList
	qt.Assert(t, isPair || isEmptyList, qt.IsTrue, qt.Commentf("expected list, got %T", result))

	// If it's a pair, verify structure is an alist (list of pairs)
	if pair, ok := result.(*values.Pair); ok {
		// First element should be a pair (key . value)
		firstElem := pair.Car()
		firstPair, ok := firstElem.(*values.Pair)
		qt.Assert(t, ok, qt.IsTrue, qt.Commentf("expected first element to be a pair, got %T", firstElem))

		// Both car and cdr should be strings
		_, carIsString := firstPair.Car().(*values.String)
		_, cdrIsString := firstPair.Cdr().(*values.String)
		qt.Assert(t, carIsString, qt.IsTrue, qt.Commentf("expected car to be string, got %T", firstPair.Car()))
		qt.Assert(t, cdrIsString, qt.IsTrue, qt.Commentf("expected cdr to be string, got %T", firstPair.Cdr()))
	}
}

func TestCommandLineReturnsList(t *testing.T) {
	result, err := runSchemeCode(t, `(command-line)`)
	qt.Assert(t, err, qt.IsNil)

	// Result should be a list (pair or empty list)
	_, isPair := result.(*values.Pair)
	isEmptyList := result == values.EmptyList
	qt.Assert(t, isPair || isEmptyList, qt.IsTrue, qt.Commentf("expected list, got %T", result))

	// If it's a pair, verify elements are strings
	if pair, ok := result.(*values.Pair); ok {
		firstElem := pair.Car()
		_, ok := firstElem.(*values.String)
		qt.Assert(t, ok, qt.IsTrue, qt.Commentf("expected first element to be string, got %T", firstElem))
	}
}

func TestGetEnvironmentVariableErrorWithNonString(t *testing.T) {
	_, err := runSchemeCode(t, `(get-environment-variable 42)`)
	qt.Assert(t, err, qt.Not(qt.IsNil), qt.Commentf("expected error when passing non-string"))
}

func TestFileExistsErrorWithNonString(t *testing.T) {
	_, err := runSchemeCode(t, `(file-exists? 42)`)
	qt.Assert(t, err, qt.Not(qt.IsNil), qt.Commentf("expected error when passing non-string"))
}
