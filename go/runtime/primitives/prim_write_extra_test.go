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
	"testing"

	qt "github.com/frankban/quicktest"

	"wile/values"
)

func TestWriteWithStringOutputPort(t *testing.T) {
	result, err := runSchemeCode(t, `
		(let ((p (open-output-string)))
		  (write "hello" p)
		  (get-output-string p))
	`)
	qt.Assert(t, err, qt.IsNil)
	str, ok := result.(*values.String)
	qt.Assert(t, ok, qt.IsTrue, qt.Commentf("Expected *values.String, got %T", result))
	qt.Assert(t, str.Value, qt.Equals, "\"hello\"")
}

func TestWriteWithSymbolOutputPort(t *testing.T) {
	result, err := runSchemeCode(t, `
		(let ((p (open-output-string)))
		  (write 'hello p)
		  (get-output-string p))
	`)
	qt.Assert(t, err, qt.IsNil)
	str, ok := result.(*values.String)
	qt.Assert(t, ok, qt.IsTrue, qt.Commentf("Expected *values.String, got %T", result))
	qt.Assert(t, str.Value, qt.Equals, "hello")
}

func TestWriteWithListOutputPort(t *testing.T) {
	result, err := runSchemeCode(t, `
		(let ((p (open-output-string)))
		  (write '(1 2 3) p)
		  (get-output-string p))
	`)
	qt.Assert(t, err, qt.IsNil)
	str, ok := result.(*values.String)
	qt.Assert(t, ok, qt.IsTrue, qt.Commentf("Expected *values.String, got %T", result))
	qt.Assert(t, str.Value, qt.Equals, "(1 2 3)")
}

func TestWriteCharWithOutputPort(t *testing.T) {
	result, err := runSchemeCode(t, `
		(let ((p (open-output-string)))
		  (write-char #\A p)
		  (get-output-string p))
	`)
	qt.Assert(t, err, qt.IsNil)
	str, ok := result.(*values.String)
	qt.Assert(t, ok, qt.IsTrue, qt.Commentf("Expected *values.String, got %T", result))
	qt.Assert(t, str.Value, qt.Equals, "A")
}

func TestNewlineWithOutputPort(t *testing.T) {
	result, err := runSchemeCode(t, `
		(let ((p (open-output-string)))
		  (newline p)
		  (get-output-string p))
	`)
	qt.Assert(t, err, qt.IsNil)
	str, ok := result.(*values.String)
	qt.Assert(t, ok, qt.IsTrue, qt.Commentf("Expected *values.String, got %T", result))
	qt.Assert(t, str.Value, qt.Equals, "\n")
}

func TestMultipleWriteCharsWithOutputPort(t *testing.T) {
	result, err := runSchemeCode(t, `
		(let ((p (open-output-string)))
		  (write-char #\H p)
		  (write-char #\i p)
		  (get-output-string p))
	`)
	qt.Assert(t, err, qt.IsNil)
	str, ok := result.(*values.String)
	qt.Assert(t, ok, qt.IsTrue, qt.Commentf("Expected *values.String, got %T", result))
	qt.Assert(t, str.Value, qt.Equals, "Hi")
}

func TestWriteWithInteger(t *testing.T) {
	result, err := runSchemeCode(t, `
		(let ((p (open-output-string)))
		  (write 42 p)
		  (get-output-string p))
	`)
	qt.Assert(t, err, qt.IsNil)
	str, ok := result.(*values.String)
	qt.Assert(t, ok, qt.IsTrue, qt.Commentf("Expected *values.String, got %T", result))
	qt.Assert(t, str.Value, qt.Equals, "42")
}

func TestWriteWithBoolean(t *testing.T) {
	result, err := runSchemeCode(t, `
		(let ((p (open-output-string)))
		  (write #t p)
		  (get-output-string p))
	`)
	qt.Assert(t, err, qt.IsNil)
	str, ok := result.(*values.String)
	qt.Assert(t, ok, qt.IsTrue, qt.Commentf("Expected *values.String, got %T", result))
	qt.Assert(t, str.Value, qt.Equals, "#t")
}

func TestWriteWithNestedList(t *testing.T) {
	result, err := runSchemeCode(t, `
		(let ((p (open-output-string)))
		  (write '((a b) (c d)) p)
		  (get-output-string p))
	`)
	qt.Assert(t, err, qt.IsNil)
	str, ok := result.(*values.String)
	qt.Assert(t, ok, qt.IsTrue, qt.Commentf("Expected *values.String, got %T", result))
	qt.Assert(t, str.Value, qt.Equals, "((a b) (c d))")
}

func TestWriteMixedOperationsToPort(t *testing.T) {
	result, err := runSchemeCode(t, `
		(let ((p (open-output-string)))
		  (write "line1" p)
		  (newline p)
		  (write-char #\> p)
		  (write-char #\space p)
		  (write 123 p)
		  (get-output-string p))
	`)
	qt.Assert(t, err, qt.IsNil)
	str, ok := result.(*values.String)
	qt.Assert(t, ok, qt.IsTrue, qt.Commentf("Expected *values.String, got %T", result))
	qt.Assert(t, str.Value, qt.Equals, "\"line1\"\n> 123")
}
