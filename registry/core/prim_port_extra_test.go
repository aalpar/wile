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

	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

func TestPortPredicateWithInputPort(t *testing.T) {
	result, err := runSchemeCode(t, `(port? (open-input-string "test"))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestPortPredicateWithOutputPort(t *testing.T) {
	result, err := runSchemeCode(t, `(port? (open-output-string))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestPortPredicateWithNonPort(t *testing.T) {
	result, err := runSchemeCode(t, `(port? 42)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.FalseValue)
}

func TestPortPredicateWithString(t *testing.T) {
	result, err := runSchemeCode(t, `(port? "not a port")`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.FalseValue)
}

func TestInputPortPredicateWithInputPort(t *testing.T) {
	result, err := runSchemeCode(t, `(input-port? (open-input-string "test"))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestInputPortPredicateWithOutputPort(t *testing.T) {
	result, err := runSchemeCode(t, `(input-port? (open-output-string))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.FalseValue)
}

func TestInputPortPredicateWithNonPort(t *testing.T) {
	result, err := runSchemeCode(t, `(input-port? 42)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.FalseValue)
}

func TestOutputPortPredicateWithOutputPort(t *testing.T) {
	result, err := runSchemeCode(t, `(output-port? (open-output-string))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestOutputPortPredicateWithInputPort(t *testing.T) {
	result, err := runSchemeCode(t, `(output-port? (open-input-string "test"))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.FalseValue)
}

func TestOutputPortPredicateWithNonPort(t *testing.T) {
	result, err := runSchemeCode(t, `(output-port? "not a port")`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.FalseValue)
}

func TestInputPortOpenPredicate(t *testing.T) {
	result, err := runSchemeCode(t, `(input-port-open? (current-input-port))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestOutputPortOpenPredicate(t *testing.T) {
	result, err := runSchemeCode(t, `(output-port-open? (current-output-port))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestEofObject(t *testing.T) {
	result, err := runSchemeCode(t, `(eof-object)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.EOFObject)
}

func TestEofObjectPredicateWithEof(t *testing.T) {
	result, err := runSchemeCode(t, `(eof-object? (eof-object))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestEofObjectPredicateWithNonEof(t *testing.T) {
	result, err := runSchemeCode(t, `(eof-object? 42)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.FalseValue)
}

func TestEofObjectPredicateWithString(t *testing.T) {
	result, err := runSchemeCode(t, `(eof-object? "not eof")`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.FalseValue)
}

func TestEofObjectPredicateWithBoolean(t *testing.T) {
	result, err := runSchemeCode(t, `(eof-object? #f)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.FalseValue)
}

func TestPortPredicatesOnBytevectorPorts(t *testing.T) {
	result, err := runSchemeCode(t, `(port? (open-input-bytevector #u8(1 2 3)))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)

	result, err = runSchemeCode(t, `(port? (open-output-bytevector))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)

	result, err = runSchemeCode(t, `(input-port? (open-input-bytevector #u8(1 2 3)))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)

	result, err = runSchemeCode(t, `(output-port? (open-output-bytevector))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestPortOpenPredicatesOnBytevectorPorts(t *testing.T) {
	result, err := runSchemeCode(t, `(input-port-open? (open-input-bytevector #u8(1 2 3)))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)

	result, err = runSchemeCode(t, `(output-port-open? (open-output-bytevector))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestPortOpenPredicatesOnStringPorts(t *testing.T) {
	result, err := runSchemeCode(t, `(input-port-open? (open-input-string "test"))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)

	result, err = runSchemeCode(t, `(output-port-open? (open-output-string))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)

	result, err = runSchemeCode(t, `(let ((p (open-input-string "test"))) (close-input-port p) (input-port-open? p))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.FalseValue)

	result, err = runSchemeCode(t, `(let ((p (open-output-string))) (close-output-port p) (output-port-open? p))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.FalseValue)
}

func TestPortPredicatesWithCurrentPorts(t *testing.T) {
	result, err := runSchemeCode(t, `(port? (current-input-port))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)

	result, err = runSchemeCode(t, `(port? (current-output-port))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)

	result, err = runSchemeCode(t, `(input-port? (current-input-port))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)

	result, err = runSchemeCode(t, `(output-port? (current-output-port))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestInputPortOpenWithCurrentPort(t *testing.T) {
	result, err := runSchemeCode(t, `(input-port-open? (current-input-port))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestOutputPortOpenWithCurrentPort(t *testing.T) {
	result, err := runSchemeCode(t, `(output-port-open? (current-output-port))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}
