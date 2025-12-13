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

package primitives

import (
	"os"
	"time"
	"weak"

	"wile/parser"
	"wile/tokenizer"
	"wile/values"
)

// Package-level state for I/O ports.
// These are lazily initialized on first access.
var (
	// currentInputPortParam is the parameter holding the current input port.
	currentInputPortParam *values.Parameter
	// currentOutputPortParam is the parameter holding the current output port.
	currentOutputPortParam *values.Parameter
	// currentErrorPortParam is the parameter holding the current error port.
	currentErrorPortParam *values.Parameter
	// Tokenizers caches tokenizers per input port using weak references.
	Tokenizers map[values.Value]weak.Pointer[tokenizer.Tokenizer]
	// Parsers caches parsers per input port using weak references.
	Parsers map[values.Value]weak.Pointer[parser.Parser]
	// ProgramStartTime is used for current-jiffy to measure elapsed time.
	ProgramStartTime = time.Now()
)

// stateInitialized tracks whether InitState has been called.
var stateInitialized bool

// InitState initializes the primitives state. Must be called before using I/O primitives.
// Safe to call multiple times - subsequent calls are no-ops.
func InitState() {
	if stateInitialized {
		return
	}
	stateInitialized = true

	Tokenizers = map[values.Value]weak.Pointer[tokenizer.Tokenizer]{}
	Parsers = map[values.Value]weak.Pointer[parser.Parser]{}

	// Initialize port parameters with default values
	currentInputPortParam = values.NewParameter(
		values.NewCharacterInputPortFromReader(os.Stdin),
		nil,
	)
	currentOutputPortParam = values.NewParameter(
		values.NewCharacterOutputPort(os.Stdout),
		nil,
	)
	currentErrorPortParam = values.NewParameter(
		values.NewCharacterOutputPort(os.Stderr),
		nil,
	)
}

// ResetState resets the primitives state. Used for testing.
func ResetState() {
	stateInitialized = false
	currentInputPortParam = nil
	currentOutputPortParam = nil
	currentErrorPortParam = nil
	Tokenizers = nil
	Parsers = nil
}

// GetCurrentInputPort returns the current input port from the parameter.
func GetCurrentInputPort() *values.CharacterInputPort {
	if currentInputPortParam == nil {
		// Fallback for tests that don't call InitState
		return values.NewCharacterInputPortFromReader(os.Stdin)
	}
	return currentInputPortParam.Value().(*values.CharacterInputPort)
}

// GetCurrentInputPortParam returns the current-input-port parameter object.
func GetCurrentInputPortParam() *values.Parameter {
	return currentInputPortParam
}

// SetCurrentInputPort sets the current input port value. Used for testing.
func SetCurrentInputPort(port *values.CharacterInputPort) {
	InitState() // Ensure state is initialized
	currentInputPortParam.SetValue(port)
}

// ResetCurrentInputPort resets the current input port to stdin. Used for testing.
func ResetCurrentInputPort() {
	if currentInputPortParam != nil {
		currentInputPortParam.SetValue(values.NewCharacterInputPortFromReader(os.Stdin))
	}
}

// GetCurrentOutputPort returns the current output port from the parameter.
func GetCurrentOutputPort() *values.CharacterOutputPort {
	if currentOutputPortParam == nil {
		// Fallback for tests that don't call InitState
		return values.NewCharacterOutputPort(os.Stdout)
	}
	return currentOutputPortParam.Value().(*values.CharacterOutputPort)
}

// GetCurrentOutputPortParam returns the current-output-port parameter object.
func GetCurrentOutputPortParam() *values.Parameter {
	return currentOutputPortParam
}

// SetCurrentOutputPort sets the current output port value. Used for testing and parameterize.
func SetCurrentOutputPort(port *values.CharacterOutputPort) {
	InitState() // Ensure state is initialized
	currentOutputPortParam.SetValue(port)
}

// ResetCurrentOutputPort resets the current output port to stdout. Used for testing.
func ResetCurrentOutputPort() {
	if currentOutputPortParam != nil {
		currentOutputPortParam.SetValue(values.NewCharacterOutputPort(os.Stdout))
	}
}

// GetCurrentErrorPort returns the current error port from the parameter.
func GetCurrentErrorPort() *values.CharacterOutputPort {
	if currentErrorPortParam == nil {
		// Fallback for tests that don't call InitState
		return values.NewCharacterOutputPort(os.Stderr)
	}
	return currentErrorPortParam.Value().(*values.CharacterOutputPort)
}

// GetCurrentErrorPortParam returns the current-error-port parameter object.
func GetCurrentErrorPortParam() *values.Parameter {
	return currentErrorPortParam
}

// StringValue returns the display representation of a value.
// Uses String() if available (for human-readable output), otherwise SchemeString().
func StringValue(o values.Value) string {
	if stringer, ok := o.(interface{ String() string }); ok {
		return stringer.String()
	}
	return o.SchemeString()
}
