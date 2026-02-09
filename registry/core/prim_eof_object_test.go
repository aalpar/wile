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
	"testing"

	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

// EOF Object Tests (R7RS §6.13.2)

func TestEofObjectFromRead(t *testing.T) {
	// R7RS §6.13.2: "If an end of file is encountered in the input before any
	// characters are found that can begin an object, then an end-of-file object
	// is returned."
	result, err := runSchemeCode(t, `
		(let ((p (open-input-string "")))
			(eof-object? (read p)))
	`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestEofObjectFromReadAfterData(t *testing.T) {
	// Reading past the end of data returns eof-object
	result, err := runSchemeCode(t, `
		(let ((p (open-input-string "42")))
			(read p)
			(eof-object? (read p)))
	`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestEofObjectFromReadCharAfterRead(t *testing.T) {
	// After read consumes data, read-char also returns eof-object
	result, err := runSchemeCode(t, `
		(let ((p (open-input-string "42")))
			(read p)
			(eof-object? (read-char p)))
	`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestEofObjectFromReadMultipleEof(t *testing.T) {
	// Multiple reads past EOF all return eof-object
	result, err := runSchemeCode(t, `
		(let ((p (open-input-string "")))
			(let ((a (read p))
			      (b (read p))
			      (c (read p)))
				(and (eof-object? a)
				     (eof-object? b)
				     (eof-object? c))))
	`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
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
