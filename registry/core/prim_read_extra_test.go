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
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// Read Variants Tests (R7RS §6.13.2)

func TestReadWithExplicitPort(t *testing.T) {
	result, err := runSchemeCode(t, `
		(let ((p (open-input-string "42")))
			(read p))
	`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewInteger(42))
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
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewSymbol("foo"))
}

func TestReadEOFOnExhaustedPort(t *testing.T) {
	// After reading all data, subsequent reads return eof-object
	result, err := runSchemeCode(t, `
		(let ((p (open-input-string "42")))
			(read p)       ; consume the datum
			(eof-object? (read p)))
	`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
}

func TestReadRepeatedEOF(t *testing.T) {
	// Multiple reads past EOF all return eof-object
	result, err := runSchemeCode(t, `
		(let ((p (open-input-string "")))
			(let ((a (read p))
			      (b (read p)))
				(and (eof-object? a) (eof-object? b))))
	`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
}

func TestReadSyntaxEOFOnExhaustedPort(t *testing.T) {
	result, err := runSchemeCode(t, `
		(let ((p (open-input-string "foo")))
			(read-syntax p)
			(eof-object? (read-syntax p)))
	`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
}

func TestReadTokenEOFOnExhaustedPort(t *testing.T) {
	result, err := runSchemeCode(t, `
		(let ((p (open-input-string "x")))
			(read-token p)
			(eof-object? (read-token p)))
	`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
}

func TestClosePortThenRead(t *testing.T) {
	// After close-port, reading returns eof-object (the cached parser
	// was evicted by close-port, and a fresh read on the closed port
	// yields EOF immediately).
	result, err := runSchemeCode(t, `
		(let ((p (open-input-string "42")))
			(close-port p)
			(eof-object? (read p)))
	`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
}

func TestCallWithPortClosesOnReturn(t *testing.T) {
	// call-with-port closes the port after proc returns
	result, err := runSchemeCode(t, `
		(let ((p (open-input-string "hello")))
			(call-with-port p
				(lambda (port) (read port)))
			(not (input-port-open? p)))
	`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
}

func TestReadTokenReturnsToken(t *testing.T) {
	// read-token should return a token object or value
	result, err := runSchemeCode(t, `
		(let ((p (open-input-string "hello")))
			(let ((tok (read-token p)))
				(not (eof-object? tok))))  ; should not be eof
	`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
}
