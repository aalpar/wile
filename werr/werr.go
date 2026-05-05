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
	"strings"
)

// Standard error values for type checking and runtime errors.
var (
	ErrNotABoolean               = NewTypeSentinel("boolean")
	ErrNotAnInputPort            = NewTypeSentinel("input port")
	ErrNotAnOutputPort           = NewTypeSentinel("output port")
	ErrNotABox                   = NewTypeSentinel("box")
	ErrNotAnOpaqueValue          = NewTypeSentinel("opaque value")
	ErrNotAByte                  = NewTypeSentinel("byte")
	ErrNotAByteInputPort         = NewTypeSentinel("byte input port")
	ErrNotAByteOutputPort        = NewTypeSentinel("byte output port")
	ErrNotATextualPort           = NewTypeSentinel("textual port")
	ErrStopIteration             = NewStaticError("stop iteration")
	ErrNotAPrimitive             = NewTypeSentinel("primitive")
	ErrNoSuchBinding             = NewStaticError("no such binding")
	ErrNotANumber                = NewTypeSentinel("number")
	ErrCannotCompare             = NewStaticError("cannot compare values")
	ErrNotAReal                  = NewTypeSentinel("real number")
	ErrDivisionByZero            = NewStaticError("division by zero")
	ErrNotAList                  = NewTypeSentinel("list")
	ErrNotACloseParen            = NewStaticError("not a close parenthesis")
	ErrWrongNumberOfArguments    = NewStaticError("wrong number of arguments")
	ErrNotAMachineContext        = NewTypeSentinel("machine context")
	ErrNotAPair                  = NewTypeSentinel("pair")
	ErrNotACons                  = NewTypeSentinel("cons")
	ErrNotACharacter             = NewTypeSentinel("character")
	ErrNotACharSet               = NewTypeSentinel("char-set")
	ErrStackUnderflow            = NewStaticError("stack underflow")
	ErrStackOverflow             = NewStaticError("stack overflow")
	ErrNotASyntaxValue           = NewTypeSentinel("syntax value")
	ErrNotASyntaxPair            = NewTypeSentinel("syntax pair")
	ErrNotASyntaxSymbol          = NewTypeSentinel("syntax symbol")
	ErrNotASyntaxList            = NewTypeSentinel("syntax list")
	ErrNotASyntaxObject          = NewTypeSentinel("syntax object")
	ErrNotASymbol                = NewTypeSentinel("symbol")
	ErrInvalidSyntax             = NewStaticError("invalid syntax")
	ErrInvalidArgument           = NewStaticError("invalid argument")
	ErrInternal                  = NewStaticError("internal error")
	ErrDuplicateBinding          = NewStaticError("duplicate binding")
	ErrNotAClosure               = NewTypeSentinel("closure")
	ErrUnknownCharacterMnemonic  = NewStaticError("unknown character mnemonic")
	ErrNotAnInteger              = NewTypeSentinel("integer")
	ErrNotALocalEnvironmentFrame = NewTypeSentinel("local environment frame")
	ErrNotAMachineTemplate       = NewTypeSentinel("machine template")
	ErrUnexpectedNil             = NewStaticError("unexpected nil value")
	ErrUnexpectedTransformer     = NewStaticError("unexpected transformer")
	ErrNotAString                = NewTypeSentinel("string")
	ErrNotANamespace             = NewTypeSentinel("namespace")
	ErrNotAVector                = NewTypeSentinel("vector")
	ErrNotAByteVector            = NewTypeSentinel("bytevector")
	ErrNotAProcedure             = NewTypeSentinel("procedure")
	ErrNotAParameter             = NewTypeSentinel("parameter")
	ErrNotAStringOutputPort      = NewTypeSentinel("string output port")
	ErrNotABytevectorOutputPort  = NewTypeSentinel("bytevector output port")
	ErrNotANativeError           = NewTypeSentinel("error object")
	ErrNotARecord                = NewTypeSentinel("record")
	ErrNotARecordType            = NewTypeSentinel("record type")
	ErrOpaqueRecord              = NewStaticError("record type is opaque")
	ErrFileNotFound              = NewStaticError("file not found")
	ErrFileOpen                  = NewStaticError("file open failed")
	ErrLibraryNotFound           = NewStaticError("library not found")
	ErrCircularDependency        = NewStaticError("circular library dependency")
	ErrUnexportedIdentifier      = NewStaticError("identifier not exported")

	// Threading errors
	ErrNotAThread            = NewTypeSentinel("thread")
	ErrNotAMutex             = NewTypeSentinel("mutex")
	ErrNotAConditionVariable = NewTypeSentinel("condition variable")
	ErrNotATime              = NewTypeSentinel("time")
	ErrNotAChannel           = NewTypeSentinel("channel")
	ErrNotAWaitGroup         = NewTypeSentinel("wait-group")
	ErrNotARWMutex           = NewTypeSentinel("rw-mutex")
	// "once" is pronounced /wuns/ — letter rule would give "an once", but
	// the consonant /w/ sound calls for "a". Pass-through preserves the
	// correct article.
	ErrNotAOnce                = NewTypeSentinel("a once")
	ErrNotAnAtomic             = NewTypeSentinel("atomic")
	ErrPortClosed              = NewStaticError("port is closed")
	ErrNotAHashtable           = NewTypeSentinel("hashtable")
	ErrNoCaptureContext        = NewStaticError("no capture context for expansion")
	ErrExactnessConversion     = NewStaticError("exactness conversion failed")
	ErrInvalidFormat           = NewStaticError("invalid number format")
	ErrUnknownOpCode           = NewStaticError("unknown op code")
	ErrNotAMatch               = NewTypeSentinel("match")
	ErrNotAPromptTag           = NewTypeSentinel("prompt tag")
	ErrNotAContinuationMarkSet = NewTypeSentinel("continuation mark set")
	ErrNotAContinuation        = NewTypeSentinel("continuation")
	ErrNotAnErrorContext       = NewTypeSentinel("error context")
	ErrTypeConversion          = NewStaticError("type conversion failed")
	ErrCodepointOutOfRange     = NewStaticError("codepoint out of range")
	ErrIndexOutOfRange         = NewStaticError("index out of range")
	ErrImmutableString         = NewStaticError("cannot mutate immutable string")
	ErrImmutableBinding        = NewStaticError("cannot mutate immutable binding")

	// FFI errors
	ErrFFIRegistration          = NewStaticError("FFI registration error")
	ErrFFICallbackError         = NewStaticError("FFI callback error")
	ErrCallbackResultConversion = NewStaticError("callback result conversion failed")
	ErrHashtableInsertionFailed = NewStaticError("hashtable insertion failed")

	// Engine initialization errors
	ErrEngineInit = NewStaticError("engine initialization error")

	// Pipeline errors (expansion, compilation, runtime phases)
	ErrExpansion   = NewStaticError("expansion error")
	ErrCompilation = NewStaticError("compilation error")

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
	ErrNotAProcess = NewTypeSentinel("process")

	// Thread errors
	ErrJoinTimeout             = NewStaticError("thread-join!: timeout")
	ErrThreadAlreadyStarted    = NewStaticError("thread-start!: thread already started")
	ErrCrossThreadContinuation = NewStaticError("cannot invoke continuation from different thread")
)

// StaticError is a sentinel error type for programmatic matching via errors.Is.
// Each sentinel carries a fixed human-readable message and serves as a stable
// identity that callers can match across error wrapping layers.
//
// Type-mismatch sentinels (declared via NewTypeSentinel) additionally carry
// an expectedType phrase (e.g., "a string") used by argument-extraction
// helpers in registry/helpers to format error messages without each call
// site repeating the phrase.
type StaticError struct {
	message      string
	expectedType string
}

// NewStaticError creates a new static error with the given message.
// The returned sentinel has no expectedType; callers using it with
// type-checking helpers will get a degraded ("expected  but got ...")
// error message. Use NewTypeSentinel for type-mismatch sentinels.
func NewStaticError(msg string) *StaticError {
	q := &StaticError{
		message: msg,
	}
	return q
}

// NewTypeSentinel creates a sentinel for a type-mismatch error. The
// noun argument is the bare type-name ("string", "integer", "char-set",
// …); NewTypeSentinel auto-prefixes "a"/"an" using articleFor. The
// sentinel's Error() value is "not " + article + " " + noun, e.g.
// "not a string" or "not an integer".
//
// For irregular cases where letter-based article selection produces the
// wrong result (e.g., "once" pronounced /wuns/ wants "a", not "an"),
// pass the already-articled phrase: NewTypeSentinel("a once"). Inputs
// starting with "a " or "an " are used verbatim.
func NewTypeSentinel(noun string) *StaticError {
	var typeName string
	switch {
	case strings.HasPrefix(noun, "a "), strings.HasPrefix(noun, "an "):
		typeName = noun
	default:
		typeName = articleFor(noun) + " " + noun
	}
	q := &StaticError{
		message:      "not " + typeName,
		expectedType: typeName,
	}
	return q
}

// articleFor returns "a" or "an" by letter rule: "an" before a/e/i/o/u
// (any case), "a" before everything else. This is the conventional
// orthographic rule, not the phonetic one — words like "once" (/wuns/)
// or "honor" (/onor/) violate the letter rule and require an explicit
// article in the NewTypeSentinel call.
func articleFor(noun string) string {
	if len(noun) == 0 {
		return "a"
	}
	switch noun[0] {
	case 'a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U':
		return "an"
	}
	return "a"
}

func (p *StaticError) Error() string {
	return p.message
}

// TypeName returns the expected-type phrase for type-mismatch sentinels
// (those constructed via NewTypeSentinel). Returns "" for sentinels
// constructed via NewStaticError.
func (p *StaticError) TypeName() string {
	return p.expectedType
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
