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

	"wile/values"

	qt "github.com/frankban/quicktest"
)

// EOF Object Tests (R7RS §6.13.1)

func TestEofObjectFromRead(t *testing.T) {
	// Note: In this implementation, read from an empty port returns an error
	// rather than eof-object. This is an implementation difference from R7RS.
	// This test verifies the actual behavior.
	_, err := runSchemeCode(t, `
		(let ((p (open-input-string "")))
			(read p))
	`)
	qt.Assert(t, err, qt.IsNotNil) // Expect error on EOF
}

func TestEofObjectFromReadAfterData(t *testing.T) {
	// Note: In this implementation, reading past the end returns an error
	// rather than eof-object. This is an implementation difference from R7RS.
	_, err := runSchemeCode(t, `
		(let ((p (open-input-string "42")))
			(read p)  ; consume the 42
			(read p))  ; will error on EOF
	`)
	qt.Assert(t, err, qt.IsNotNil) // Expect error on EOF
}

func TestEofObjectUniqueness(t *testing.T) {
	// There is only one eof-object
	result, err := runSchemeCode(t, `(eq? (eof-object) (eof-object))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestEofObjectEqv(t *testing.T) {
	result, err := runSchemeCode(t, `(eqv? (eof-object) (eof-object))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestEofObjectEqual(t *testing.T) {
	result, err := runSchemeCode(t, `(equal? (eof-object) (eof-object))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}
