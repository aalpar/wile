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

package io

import (
	"os"
	"sync"
	"time"

	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/internal/tokenizer"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// Package-level state for I/O ports.
// These are lazily initialized on first access.
var (
	// currentInputPortParam is the parameter holding the current input port.
	currentInputPortParam *machine.Parameter
	// currentOutputPortParam is the parameter holding the current output port.
	currentOutputPortParam *machine.Parameter
	// currentErrorPortParam is the parameter holding the current error port.
	currentErrorPortParam *machine.Parameter
	// cacheMu protects concurrent access to Tokenizers and Parsers maps.
	// Use RLock for reads, Lock for writes and deletes.
	// Fixes T1 from architectural review (thread safety).
	cacheMu sync.RWMutex
	// Tokenizers caches tokenizers per input port.
	//
	// Entries are evicted via evictPortCache() on port close or EOF.
	// Ports that are abandoned without close or EOF will retain their
	// cache entries until the process exits. If that becomes a problem
	// in long-running programs, switch to weak.Pointer[T] (Go 1.24+).
	//
	// Thread safety: All access must be protected by cacheMu.
	Tokenizers map[values.Value]*tokenizer.Tokenizer
	// Parsers caches parsers per input port.
	// Same retention caveat as Tokenizers above.
	//
	// Thread safety: All access must be protected by cacheMu.
	Parsers map[values.Value]*parser.Parser
	// ProgramStartTime is used for current-jiffy to measure elapsed time.
	ProgramStartTime = time.Now()

	// stateMu protects concurrent access to stateInitialized flag and initialization.
	stateMu sync.Mutex
	// stateInitialized tracks whether InitState has been called.
	stateInitialized bool
)

// InitState initializes the primitives state. Must be called before using I/O primitives.
// Safe to call multiple times - subsequent calls are no-ops.
// Thread-safe: protected by stateMu.
func InitState() {
	stateMu.Lock()
	defer stateMu.Unlock()

	if stateInitialized {
		return
	}
	stateInitialized = true

	Tokenizers = map[values.Value]*tokenizer.Tokenizer{}
	Parsers = map[values.Value]*parser.Parser{}

	// Initialize port parameters with default values
	currentInputPortParam = machine.NewParameter(
		values.NewCharacterInputPortFromReader(os.Stdin),
		nil,
	)
	currentOutputPortParam = machine.NewParameter(
		values.NewCharacterOutputPortFromWriter(os.Stdout),
		nil,
	)
	currentErrorPortParam = machine.NewParameter(
		values.NewCharacterOutputPortFromWriter(os.Stderr),
		nil,
	)
}

// ResetState resets the primitives state. Used for testing.
// Thread-safe: protected by stateMu and cacheMu.
func ResetState() {
	stateMu.Lock()
	stateInitialized = false
	currentInputPortParam = nil
	currentOutputPortParam = nil
	currentErrorPortParam = nil
	stateMu.Unlock()

	cacheMu.Lock()
	Tokenizers = nil
	Parsers = nil
	cacheMu.Unlock()
}

// GetCurrentInputPort returns the base input port from the parameter.
// This is a test convenience for save/restore; production code should use
// resolveCurrentInputPort which checks continuation marks from parameterize.
func GetCurrentInputPort() values.TextualReader {
	InitState()
	port, ok := currentInputPortParam.Value().(values.TextualReader)
	if !ok {
		panic(werr.WrapForeignErrorf(
			werr.ErrNotAnInputPort,
			"current-input-port: value is %T, not an input port",
			currentInputPortParam.Value()))
	}
	return port
}

// GetCurrentInputPortParam returns the current-input-port parameter object.
func GetCurrentInputPortParam() *machine.Parameter {
	return currentInputPortParam
}

// SetCurrentInputPort sets the current input port value. Used for testing.
func SetCurrentInputPort(port values.TextualReader) {
	InitState() // Ensure state is initialized
	currentInputPortParam.SetValue(port)
}

// ResetCurrentInputPort resets the current input port to stdin. Used for testing.
func ResetCurrentInputPort() {
	if currentInputPortParam != nil {
		currentInputPortParam.SetValue(values.NewCharacterInputPortFromReader(os.Stdin))
	}
}

// GetCurrentOutputPort returns the base output port from the parameter.
// This is a test convenience for save/restore; production code should use
// resolveCurrentOutputPort which checks continuation marks from parameterize.
func GetCurrentOutputPort() values.OutputPort {
	InitState()
	port, ok := currentOutputPortParam.Value().(values.OutputPort)
	if !ok {
		panic(werr.WrapForeignErrorf(
			werr.ErrNotAnOutputPort,
			"current-output-port: value is %T, not an output port",
			currentOutputPortParam.Value()))
	}
	return port
}

// GetCurrentOutputPortParam returns the current-output-port parameter object.
func GetCurrentOutputPortParam() *machine.Parameter {
	return currentOutputPortParam
}

// SetCurrentOutputPort sets the current output port value. Used for testing and parameterize.
func SetCurrentOutputPort(port values.OutputPort) {
	InitState() // Ensure state is initialized
	currentOutputPortParam.SetValue(port)
}

// ResetCurrentOutputPort resets the current output port to stdout. Used for testing.
func ResetCurrentOutputPort() {
	if currentOutputPortParam != nil {
		currentOutputPortParam.SetValue(values.NewCharacterOutputPortFromWriter(os.Stdout))
	}
}

// GetCurrentErrorPortParam returns the current-error-port parameter object.
func GetCurrentErrorPortParam() *machine.Parameter {
	return currentErrorPortParam
}

// resolveCurrentOutputPort returns the effective current output port, checking
// continuation marks (from parameterize) before falling back to the base value.
// Panics with a wrapped error if the resolved value is not an OutputPort —
// the panic is caught by OperationForeignFunctionCall's recover and converted
// to a Scheme exception.
func resolveCurrentOutputPort(cc machine.CallContext) values.OutputPort {
	mc := cc.(*machine.MachineContext)
	InitState()
	v := mc.ResolveParameterValue(currentOutputPortParam)
	port, ok := v.(values.OutputPort)
	if !ok {
		panic(werr.WrapForeignErrorf(
			werr.ErrNotAnOutputPort,
			"current-output-port: parameterize value is %T, not an output port", v))
	}
	return port
}

// resolveCurrentInputPort returns the effective current input port, checking
// continuation marks (from parameterize) before falling back to the base value.
// Panics with a wrapped error if the resolved value is not a TextualReader —
// the panic is caught by OperationForeignFunctionCall's recover and converted
// to a Scheme exception.
func resolveCurrentInputPort(cc machine.CallContext) values.TextualReader {
	mc := cc.(*machine.MachineContext)
	InitState()
	v := mc.ResolveParameterValue(currentInputPortParam)
	port, ok := v.(values.TextualReader)
	if !ok {
		panic(werr.WrapForeignErrorf(
			werr.ErrNotAnInputPort,
			"current-input-port: parameterize value is %T, not an input port", v))
	}
	return port
}

// StringValue returns the display representation of a value.
// Uses String() if available (for human-readable output), otherwise SchemeString().
func StringValue(o values.Value) string {
	stringer, ok := o.(interface{ String() string })
	if ok {
		return stringer.String()
	}
	return o.SchemeString()
}
