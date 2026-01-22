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

	"wile/values"

	qt "github.com/frankban/quicktest"
)

// Read Variants Tests (R7RS §6.13.2)

func TestReadWithExplicitPort(t *testing.T) {
	result, err := runSchemeCode(t, `
		(let ((p (open-input-string "42")))
			(read p))
	`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.NewInteger(42))
}

func TestReadSyntaxReturnsSyntaxObject(t *testing.T) {
	// read-syntax should return a syntax object
	// We verify by converting back to datum
	result, err := runSchemeCode(t, `
		(let ((p (open-input-string "foo")))
			(let ((stx (read-syntax p)))
				(syntax->datum stx)))
	`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.NewSymbol("foo"))
}

func TestReadTokenReturnsToken(t *testing.T) {
	// read-token should return a token object or value
	result, err := runSchemeCode(t, `
		(let ((p (open-input-string "hello")))
			(let ((tok (read-token p)))
				(not (eof-object? tok))))  ; should not be eof
	`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}
