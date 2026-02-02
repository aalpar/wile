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
	"testing"

	"github.com/aalpar/wile/go/values"

	qt "github.com/frankban/quicktest"
)

// Display vs WriteByte Comparison Tests (R7RS §6.13.3)

func TestDisplayVsWriteString(t *testing.T) {
	// display should output without quotes
	displayResult, err := runSchemeCode(t, `
		(let ((p (open-output-string)))
			(display "hello" p)
			(get-output-string p))
	`)
	qt.Assert(t, err, qt.IsNil)
	displayStr, ok := displayResult.(*values.String)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, displayStr.Value, qt.Equals, "hello")

	// write should output with quotes
	writeResult, err := runSchemeCode(t, `
		(let ((p (open-output-string)))
			(write "hello" p)
			(get-output-string p))
	`)
	qt.Assert(t, err, qt.IsNil)
	writeStr, ok := writeResult.(*values.String)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, writeStr.Value, qt.Equals, `"hello"`)
}

func TestDisplayVsWriteCharacter(t *testing.T) {
	// display should output the character directly
	displayResult, err := runSchemeCode(t, `
		(let ((p (open-output-string)))
			(display #\A p)
			(get-output-string p))
	`)
	qt.Assert(t, err, qt.IsNil)
	displayStr, ok := displayResult.(*values.String)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, displayStr.Value, qt.Equals, "A")

	// write should output in character notation
	writeResult, err := runSchemeCode(t, `
		(let ((p (open-output-string)))
			(write #\A p)
			(get-output-string p))
	`)
	qt.Assert(t, err, qt.IsNil)
	writeStr, ok := writeResult.(*values.String)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, writeStr.Value, qt.Equals, `#\A`)
}
