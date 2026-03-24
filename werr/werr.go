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

package werr

import (
	"errors"
	"fmt"
)

// Standard error values for type checking and runtime errors.
var (
	ErrNotABoolean               = NewStaticError("not a boolean")
	ErrNotAnInputPort            = NewStaticError("not an input port")
	ErrNotAnOutputPort           = NewStaticError("not an output port")
	ErrNotABox                   = NewStaticError("not a box")
	ErrNotAnOpaqueValue          = NewStaticError("not an opaque value")
	ErrNotAByte                  = NewStaticError("not a byte")
	ErrNotAByteInputPort         = NewStaticError("not a byte input port")
	ErrNotAByteOutputPort        = NewStaticError("not a byte output port")
	ErrNotATextualPort           = NewStaticError("not a textual port")
	ErrStopIteration             = NewStaticError("stop iteration")
	ErrNotAPrimitive             = NewStaticError("not a primitive")
	ErrNoSuchBinding             = NewStaticError("no such binding")
	ErrNotANumber                = NewStaticError("not a number")
	ErrCannotCompare             = NewStaticError("cannot compare values")
	ErrNotAReal                  = NewStaticError("not a real number")
	ErrDivisionByZero            = NewStaticError("division by zero")
	ErrNotAList                  = NewStaticError("not a list")
	ErrNotACloseParen            = NewStaticError("not a close parenthesis")
	ErrWrongNumberOfArguments    = NewStaticError("wrong number of arguments")
	ErrNotAPair                  = NewStaticError("not a pair")
	ErrNotACons                  = NewStaticError("not a cons")
	ErrNotACharacter             = NewStaticError("not a character")
	ErrStackUnderflow            = NewStaticError("stack underflow")
	ErrNotASyntaxValue           = NewStaticError("not a syntax value")
	ErrNotASyntaxPair            = NewStaticError("not a syntax pair")
	ErrNotASyntaxSymbol          = NewStaticError("not a syntax symbol")
	ErrNotASyntaxList            = NewStaticError("not a syntax list")
	ErrNotASyntaxObject          = NewStaticError("not a syntax object")
	ErrNotASymbol                = NewStaticError("not a symbol")
	ErrInvalidSyntax             = NewStaticError("invalid syntax")
	ErrInvalidArgument           = NewStaticError("invalid argument")
	ErrDuplicateBinding          = NewStaticError("duplicate binding")
	ErrNotAClosure               = NewStaticError("not a closure")
	ErrUnknownCharacterMnemonic  = NewStaticError("unknown character mnemonic")
	ErrNotAnInteger              = NewStaticError("not an integer")
	ErrNotALocalEnvironmentFrame = NewStaticError("not a local environment frame")
	ErrNotAMachineTemplate       = NewStaticError("not a machine template")
	ErrUnexpectedNil             = NewStaticError("unexpected nil value")
	ErrUnexpectedTransformer     = NewStaticError("unexpected transformer")
	ErrNotAString                = NewStaticError("not a string")
	ErrNotAVector                = NewStaticError("not a vector")
	ErrNotAByteVector            = NewStaticError("not a bytevector")
	ErrNotAProcedure             = NewStaticError("not a procedure")
	ErrNotAParameter             = NewStaticError("not a parameter")
	ErrNotAStringOutputPort      = NewStaticError("not a string output port")
	ErrNotABytevectorOutputPort  = NewStaticError("not a bytevector output port")
	ErrNotANativeError           = NewStaticError("not an error object")
	ErrNotARecord                = NewStaticError("not a record")
	ErrNotARecordType            = NewStaticError("not a record type")
	ErrFileNotFound              = NewStaticError("file not found")
	ErrLibraryNotFound           = NewStaticError("library not found")
	ErrCircularDependency        = NewStaticError("circular library dependency")
	ErrUnexportedIdentifier      = NewStaticError("identifier not exported")

	// Threading errors
	ErrNotAThread              = NewStaticError("not a thread")
	ErrNotAMutex               = NewStaticError("not a mutex")
	ErrNotAConditionVariable   = NewStaticError("not a condition variable")
	ErrNotATime                = NewStaticError("not a time")
	ErrNotAChannel             = NewStaticError("not a channel")
	ErrNotAWaitGroup           = NewStaticError("not a wait-group")
	ErrNotARWMutex             = NewStaticError("not a rw-mutex")
	ErrNotAOnce                = NewStaticError("not a once")
	ErrNotAnAtomic             = NewStaticError("not an atomic")
	ErrPortClosed              = NewStaticError("port is closed")
	ErrNotAHashtable           = NewStaticError("not a hashtable")
	ErrNoCaptureContext        = NewStaticError("no capture context for expansion")
	ErrExactnessConversion     = NewStaticError("exactness conversion failed")
	ErrInvalidFormat           = NewStaticError("invalid number format")
	ErrUnknownOpCode           = NewStaticError("unknown op code")
	ErrNotAMatch               = NewStaticError("not a match")
	ErrNotAPromptTag           = NewStaticError("not a prompt tag")
	ErrNotAContinuationMarkSet = NewStaticError("not a continuation mark set")
	ErrNotAContinuation        = NewStaticError("not a continuation")
	ErrTypeConversion          = NewStaticError("type conversion failed")
	ErrIndexOutOfRange         = NewStaticError("index out of range")
	ErrImmutableString         = NewStaticError("cannot mutate immutable string")

	// FFI errors
	ErrFFIRegistration          = NewStaticError("FFI registration error")
	ErrFFICallbackError         = NewStaticError("FFI callback error")
	ErrCallbackResultConversion = NewStaticError("callback result conversion failed")
	ErrHashtableInsertionFailed = NewStaticError("hashtable insertion failed")

	// Engine initialization errors
	ErrEngineInit = NewStaticError("engine initialization error")

	// Environment errors (keep as panics but use sentinels)
	ErrMissingNamespace     = NewStaticError("missing Namespace")
	ErrMissingPhaseRegistry = NewStaticError("missing PhaseRegistry")
	ErrNilParentEnvironment = NewStaticError("nil parent environment")

	// Panic recovery errors
	ErrThreadPanic   = NewStaticError("thread panic")
	ErrPanicRecovery = NewStaticError("panic recovery")

	// Syntax errors
	ErrCannotDoubleSyntaxWrap  = NewStaticError("cannot wrap syntax value in SyntaxObject")
	ErrNoMatchingClause        = NewStaticError("no matching clause")
	ErrUnsupportedTransformer  = NewStaticError("unsupported transformer")
	ErrLibraryConfiguration    = NewStaticError("library configuration error")
	ErrLibraryFormMalformed    = NewStaticError("malformed library form")
	ErrLibraryNameMismatch     = NewStaticError("library name mismatch")
	ErrHashtableKeyNotFound    = NewStaticError("hashtable key not found")
	ErrAllocationLimitExceeded = NewStaticError("allocation limit exceeded")
	ErrNonContinuableException = NewStaticError("non-continuable exception")

	// Recursion depth
	ErrCallDepthExceeded     = NewStaticError("call depth exceeded")
	ErrContinuationUnderflow = NewStaticError("continuation underflow")

	// Escape continuations
	ErrExpiredEscape       = NewStaticError("expired escape procedure")
	ErrContinuationBarrier = NewStaticError("continuation barrier violation")

	// Utility errors (keep as panic)
	ErrRandomGenerationFailed = NewStaticError("random generation failed")
	ErrInvalidLoadPath        = NewStaticError("invalid load path")

	// Channel errors
	ErrChannelClosed = NewStaticError("channel is closed")

	// Process errors
	ErrNotAProcess = NewStaticError("not a process")

	// Thread errors
	ErrJoinTimeout             = NewStaticError("thread-join!: timeout")
	ErrThreadAlreadyStarted    = NewStaticError("thread-start!: thread already started")
	ErrCrossThreadContinuation = NewStaticError("cannot invoke continuation from different thread")
)

// StaticError is a sentinel error type for programmatic matching via errors.Is.
// Each sentinel carries a fixed human-readable message and serves as a stable
// identity that callers can match across error wrapping layers.
type StaticError struct {
	message string
}

// NewStaticError creates a new static error with the given message.
func NewStaticError(msg string) *StaticError {
	q := &StaticError{
		message: msg,
	}
	return q
}

func (p *StaticError) Error() string {
	return p.message
}

// ForeignError is an error type for Go primitive implementations (functions
// foreign to Scheme). It wraps an optional underlying error with a message.
type ForeignError struct {
	err     error // sentinel for errors.Is matching
	cause   error // root cause from underlying operation
	message string
}

// newForeignError creates a new foreign error with the given message.
func newForeignError(msg string) *ForeignError {
	return &ForeignError{
		message: msg,
	}
}

// NewForeignErrorf creates a new foreign error with a formatted message.
func NewForeignErrorf(msg string, vs ...any) *ForeignError {
	if len(vs) == 0 {
		return newForeignError(msg)
	}
	return &ForeignError{
		message: fmt.Sprintf(msg, vs...),
	}
}

// WrapForeignErrorf wraps an existing error with a formatted message.
func WrapForeignErrorf(err error, msg string, vs ...any) *ForeignError {
	if err == nil {
		return NewForeignErrorf(msg, vs...)
	}
	return &ForeignError{
		err:     err,
		message: fmt.Sprintf(msg, vs...),
	}
}

// WrapForeignErrorWithCause wraps a sentinel and a root cause into a single
// ForeignError. The sentinel is matched by errors.Is for programmatic
// dispatch; the cause preserves the underlying failure for diagnostics.
func WrapForeignErrorWithCause(sentinel, cause error, msg string, vs ...any) *ForeignError {
	return &ForeignError{
		err:     sentinel,
		cause:   cause,
		message: fmt.Sprintf(msg, vs...),
	}
}

// Is reports whether target matches the sentinel or the cause.
// This replaces Unwrap and gives ForeignError precise two-chain semantics:
// the sentinel identifies the error category, the cause preserves the
// root failure from the underlying operation.
func (p *ForeignError) Is(target error) bool {
	if p.err != nil && errors.Is(p.err, target) {
		return true
	}
	if p.cause != nil && errors.Is(p.cause, target) {
		return true
	}
	return false
}

// As checks whether the sentinel or cause can be assigned to target.
func (p *ForeignError) As(target any) bool {
	if p.err != nil && errors.As(p.err, target) {
		return true
	}
	if p.cause != nil && errors.As(p.cause, target) {
		return true
	}
	return false
}

// Cause returns the root cause error, if any. Useful for debugging/logging
// when you need the underlying failure directly.
func (p *ForeignError) Cause() error {
	if p == nil {
		return nil
	}
	return p.cause
}

func (p *ForeignError) Error() string {
	switch {
	case p.err != nil && p.cause != nil:
		return fmt.Sprintf("%s: %s: %s", p.message, p.err.Error(), p.cause.Error())
	case p.err != nil:
		return fmt.Sprintf("%s: %s", p.message, p.err.Error())
	case p.cause != nil:
		return fmt.Sprintf("%s: %s", p.message, p.cause.Error())
	default:
		return p.message
	}
}

// ForeignFileError represents an error from a file system operation.
// R7RS §6.11: detected by file-error? predicate.
type ForeignFileError struct {
	*ForeignError
	Filename string // the file path that caused the error
	Op       string // the operation (e.g., "open-input-file", "delete-file")
}

// WrapForeignFileError wraps an OS error with file context.
func WrapForeignFileError(err error, op string, filename string) *ForeignFileError {
	q := &ForeignFileError{
		ForeignError: WrapForeignErrorf(err, "%s: %s", op, filename),
		Filename:     filename,
		Op:           op,
	}
	return q
}

// ForeignProcessError represents an error from a process operation.
// Parallel to ForeignFileError for programmatic inspection of failed
// process operations.
type ForeignProcessError struct {
	*ForeignError
	Command string // the command that was run
	Op      string // the operation (e.g., "process-spawn", "system")
}

// WrapForeignProcessError wraps an OS error with process context.
func WrapForeignProcessError(err error, op string, command string) *ForeignProcessError {
	q := &ForeignProcessError{
		ForeignError: WrapForeignErrorf(err, "%s: %s", op, command),
		Command:      command,
		Op:           op,
	}
	return q
}

// ForeignReadError represents an error from a read or parse operation.
// R7RS §6.11: detected by read-error? predicate.
type ForeignReadError struct {
	*ForeignError
}

// WrapForeignReadErrorf wraps an error as a read error.
func WrapForeignReadErrorf(err error, msg string, vs ...any) *ForeignReadError {
	q := &ForeignReadError{
		ForeignError: WrapForeignErrorf(err, msg, vs...),
	}
	return q
}

// NewForeignReadErrorf creates a new read error with a formatted message.
func NewForeignReadErrorf(msg string, vs ...any) *ForeignReadError {
	q := &ForeignReadError{
		ForeignError: NewForeignErrorf(msg, vs...),
	}
	return q
}
