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

// Package primitives provides backward-compatible wrappers for the io extension state.
// This package is deprecated - use extensions/io directly for new code.
package primitives

import (
	"time"
	"weak"

	ioext "wile/extensions/io"
	"wile/machine"
	"wile/parser"
	"wile/tokenizer"
	"wile/values"
)

// Package-level state - delegates to io extension.
var (
	// Tokenizers caches tokenizers per input port using weak references.
	// Delegates to io extension.
	Tokenizers map[values.Value]weak.Pointer[tokenizer.Tokenizer]
	// Parsers caches parsers per input port using weak references.
	// Delegates to io extension.
	Parsers map[values.Value]weak.Pointer[parser.Parser]
	// ProgramStartTime is used for current-jiffy to measure elapsed time.
	ProgramStartTime = time.Now()
)

// InitState initializes the primitives state by delegating to io extension.
// Safe to call multiple times - subsequent calls are no-ops.
// Deprecated: Use extensions/io.InitState() directly.
func InitState() {
	ioext.InitState()
	// Keep local cache references pointing to io extension caches
	Tokenizers = ioext.Tokenizers
	Parsers = ioext.Parsers
}

// ResetState resets the primitives state. Used for testing.
// Deprecated: Use extensions/io directly.
func ResetState() {
	ioext.ResetState()
	Tokenizers = nil
	Parsers = nil
}

// GetCurrentInputPort returns the current input port from the parameter.
// Deprecated: Use extensions/io.GetCurrentInputPort() directly.
func GetCurrentInputPort() *values.CharacterInputPort {
	return ioext.GetCurrentInputPort()
}

// GetCurrentInputPortParam returns the current-input-port parameter object.
// Deprecated: Use extensions/io.GetCurrentInputPortParam() directly.
func GetCurrentInputPortParam() *machine.Parameter {
	return ioext.GetCurrentInputPortParam()
}

// SetCurrentInputPort sets the current input port value. Used for testing.
// Deprecated: Use extensions/io.SetCurrentInputPort() directly.
func SetCurrentInputPort(port *values.CharacterInputPort) {
	ioext.SetCurrentInputPort(port)
}

// ResetCurrentInputPort resets the current input port to stdin. Used for testing.
// Deprecated: Use extensions/io.ResetCurrentInputPort() directly.
func ResetCurrentInputPort() {
	ioext.ResetCurrentInputPort()
}

// GetCurrentOutputPort returns the current output port from the parameter.
// Deprecated: Use extensions/io.GetCurrentOutputPort() directly.
func GetCurrentOutputPort() *values.CharacterOutputPort {
	return ioext.GetCurrentOutputPort()
}

// GetCurrentOutputPortParam returns the current-output-port parameter object.
// Deprecated: Use extensions/io.GetCurrentOutputPortParam() directly.
func GetCurrentOutputPortParam() *machine.Parameter {
	return ioext.GetCurrentOutputPortParam()
}

// SetCurrentOutputPort sets the current output port value. Used for testing and parameterize.
// Deprecated: Use extensions/io.SetCurrentOutputPort() directly.
func SetCurrentOutputPort(port *values.CharacterOutputPort) {
	ioext.SetCurrentOutputPort(port)
}

// ResetCurrentOutputPort resets the current output port to stdout. Used for testing.
// Deprecated: Use extensions/io.ResetCurrentOutputPort() directly.
func ResetCurrentOutputPort() {
	ioext.ResetCurrentOutputPort()
}

// GetCurrentErrorPort returns the current error port from the parameter.
// Deprecated: Use extensions/io.GetCurrentErrorPort() directly.
func GetCurrentErrorPort() *values.CharacterOutputPort {
	return ioext.GetCurrentErrorPort()
}

// GetCurrentErrorPortParam returns the current-error-port parameter object.
// Deprecated: Use extensions/io.GetCurrentErrorPortParam() directly.
func GetCurrentErrorPortParam() *machine.Parameter {
	return ioext.GetCurrentErrorPortParam()
}

// StringValue returns the display representation of a value.
// Uses String() if available (for human-readable output), otherwise SchemeString().
func StringValue(o values.Value) string {
	if stringer, ok := o.(interface{ String() string }); ok {
		return stringer.String()
	}
	return o.SchemeString()
}
