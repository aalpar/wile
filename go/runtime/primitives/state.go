package primitives

import (
	"os"
	"time"
	"weak"

	"skeme/parser"
	"skeme/tokenizer"
	"skeme/values"
)

// Package-level state for I/O ports.
// These are lazily initialized on first access.
var (
	// currentInputPort is the default input port (stdin).
	currentInputPort *values.CharacterInputPort
	// currentOutputPort is the default output port (stdout).
	currentOutputPort *values.CharacterOutputPort
	// Tokenizers caches tokenizers per input port using weak references.
	Tokenizers map[*values.CharacterInputPort]weak.Pointer[tokenizer.Tokenizer]
	// Parsers caches parsers per input port using weak references.
	Parsers map[*values.CharacterInputPort]weak.Pointer[parser.Parser]
	// ProgramStartTime is used for current-jiffy to measure elapsed time.
	ProgramStartTime = time.Now()
)

// InitState initializes the primitives state. Must be called before using I/O primitives.
func InitState() {
	Tokenizers = map[*values.CharacterInputPort]weak.Pointer[tokenizer.Tokenizer]{}
	Parsers = map[*values.CharacterInputPort]weak.Pointer[parser.Parser]{}
}

// GetCurrentInputPort returns the current input port, lazily initializing it to stdin.
func GetCurrentInputPort() *values.CharacterInputPort {
	if currentInputPort == nil {
		currentInputPort = values.NewCharacterInputPortFromReader(os.Stdin)
	}
	return currentInputPort
}

// SetCurrentInputPort sets the current input port. Used for testing.
func SetCurrentInputPort(port *values.CharacterInputPort) {
	currentInputPort = port
}

// ResetCurrentInputPort sets the current input port to nil. Used for testing lazy initialization.
func ResetCurrentInputPort() {
	currentInputPort = nil
}

// GetCurrentOutputPort returns the current output port, lazily initializing it to stdout.
func GetCurrentOutputPort() *values.CharacterOutputPort {
	if currentOutputPort == nil {
		currentOutputPort = values.NewCharacterOutputPort(os.Stdout)
	}
	return currentOutputPort
}

// SetCurrentOutputPort sets the current output port. Used for testing and parameterize.
func SetCurrentOutputPort(port *values.CharacterOutputPort) {
	currentOutputPort = port
}

// ResetCurrentOutputPort sets the current output port to nil. Used for testing lazy initialization.
func ResetCurrentOutputPort() {
	currentOutputPort = nil
}

// StringValue returns the display representation of a value.
// Uses String() if available (for human-readable output), otherwise SchemeString().
func StringValue(o values.Value) string {
	if stringer, ok := o.(interface{ String() string }); ok {
		return stringer.String()
	}
	return o.SchemeString()
}
