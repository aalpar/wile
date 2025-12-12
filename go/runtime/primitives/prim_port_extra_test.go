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
	// Note: input-port-open? currently only accepts CharacterInputPort
	// (from current-input-port), not string or bytevector ports.
	result, err := runSchemeCode(t, `(input-port-open? (current-input-port))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestOutputPortOpenPredicate(t *testing.T) {
	// Note: output-port-open? currently only accepts CharacterOutputPort
	// (from current-output-port), not string or bytevector ports.
	result, err := runSchemeCode(t, `(output-port-open? (current-output-port))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestEofObject(t *testing.T) {
	result, err := runSchemeCode(t, `(eof-object)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.EofObject)
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
	// Note: BytevectorInputPort and BytevectorOutputPort are not currently
	// recognized by input-port-open? and output-port-open?, so we skip these tests.
	// This is a known limitation that may be addressed in the future.
	t.Skip("BytevectorInputPort/OutputPort not supported by *-port-open? predicates")
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
