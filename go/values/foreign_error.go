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

package values

import (
	"fmt"
	"runtime"
)

var (
	ErrNotABoolean                 = NewStaticError("not a boolean")
	ErrNotAnInputPort              = NewStaticError("not an input port")
	ErrNotAnOutputPort             = NewStaticError("not an output port")
	ErrNotAByte                    = NewStaticError("not a byte")
	ErrNotAByteInputPort           = NewStaticError("not a byte input port")
	ErrNotAByteOutputPort          = NewStaticError("not a byte output port")
	ErrNotAnEnvironmentFrame       = NewStaticError("not an environment frame")
	ErrNotAClosureEnvironmentFrame = NewStaticError("not a closure environment frame")
	ErrNotAContinuation            = NewStaticError("not a continuation")
	ErrNotAFunctionTemplate        = NewStaticError("not a function template")
	ErrNotAModule                  = NewStaticError("not a module")
	ErrStopIteration               = NewStaticError("stop iteration")
	ErrNotAPrimitive               = NewStaticError("not a primitive")
	ErrNoSuchBinding               = NewStaticError("no such binding")
	ErrNotAGlobalIndex             = NewStaticError("not a global index")
	ErrNotANumber                  = NewStaticError("not a number")
	ErrCannotCompare               = NewStaticError("cannot compare values")
	ErrDivisionByZero              = NewStaticError("division by zero")
	ErrNotAList                    = NewStaticError("not a list")
	ErrNotACloseParen              = NewStaticError("not a close parenthesis")
	ErrWrongNumberOfArguments      = NewStaticError("wrong number of arguments")
	ErrNotAMachineContinuation     = NewStaticError("not a machine continuation")
	ErrNotAPair                    = NewStaticError("not a pair")
	ErrNotACons                    = NewStaticError("not a cons")
	ErrNotANativeTemplate          = NewStaticError("not a native template")
	ErrNotACharacter               = NewStaticError("not a character")
	ErrNotACharacterInputPort      = NewStaticError("not a character input port")
	ErrNotACharacterOutputPort     = NewStaticError("not a character output port")
	ErrStackUnderflow              = NewStaticError("stack underflow")
	ErrNotASyntaxValue             = NewStaticError("not a syntax value")
	ErrNotASyntaxPair              = NewStaticError("not a syntax pair")
	ErrNotASyntaxSymbol            = NewStaticError("not a syntax symbol")
	ErrNotASyntaxList              = NewStaticError("not a syntax list")
	ErrNotASyntaxObject            = NewStaticError("not a syntax object")
	ErrNotASymbol                  = NewStaticError("not a symbol")
	ErrInvalidSyntax               = NewStaticError("invalid syntax")
	ErrDuplicateBinding            = NewStaticError("duplicate binding")
	ErrNotAClosure                 = NewStaticError("not a closure")
	ErrUnknownCharacterMnemonic    = NewStaticError("unknown character mnemonic")
	ErrNotAnInteger                = NewStaticError("not an integer")
	ErrNotALocalEnvironmentFrame   = NewStaticError("not a local environment frame")
	ErrNotAMachineTemplate         = NewStaticError("not a machine template")
	ErrEndOfFile                   = NewStaticError("end of file")
	ErrUnexpectedNil               = NewStaticError("unexpected nil value")
	ErrUnexpectedTransformer       = NewStaticError("unexpected transformer")
	ErrNotAString                  = NewStaticError("not a string")
	ErrNotAVector                  = NewStaticError("not a vector")
	ErrNotAByteVector              = NewStaticError("not a bytevector")
	ErrNotAProcedure               = NewStaticError("not a procedure")
	ErrNotAStringOutputPort        = NewStaticError("not a string output port")
	ErrNotABytevectorOutputPort    = NewStaticError("not a bytevector output port")
	ErrNotAnErrorObject            = NewStaticError("not an error object")
	ErrNotARecord                  = NewStaticError("not a record")
	ErrNotARecordType              = NewStaticError("not a record type")

	// Threading errors
	ErrNotAThread            = NewStaticError("not a thread")
	ErrNotAMutex             = NewStaticError("not a mutex")
	ErrNotAConditionVariable = NewStaticError("not a condition variable")
	ErrNotATime              = NewStaticError("not a time")
	ErrNotAChannel           = NewStaticError("not a channel")
	ErrNotAWaitGroup         = NewStaticError("not a wait-group")
	ErrNotARWMutex           = NewStaticError("not a rw-mutex")
	ErrNotAOnce              = NewStaticError("not a once")
	ErrNotAnAtomic           = NewStaticError("not an atomic")
)

type StaticError struct {
	message string
}

func NewStaticError(msg string) *StaticError {
	q := &StaticError{
		message: msg,
	}
	return q
}

func (q *StaticError) Error() string {
	return q.message
}

// ForeignError is an error that wraps an error from foreign code, such as C code.
type ForeignError struct {
	err     error
	message string
	stack   []uintptr // stack trace
}

func NewForeignError(msg string) *ForeignError {
	pcs := [50]uintptr{}
	n := runtime.Callers(1, pcs[:])
	q := &ForeignError{
		message: msg,
		stack:   pcs[:n],
	}
	return q
}

func NewForeignErrorf(msg string, vs ...any) *ForeignError {
	if len(vs) == 0 {
		return NewForeignError(msg)
	}
	pcs := [50]uintptr{}
	n := runtime.Callers(1, pcs[:])
	q := &ForeignError{
		message: fmt.Sprintf(msg, vs...),
		stack:   pcs[:n],
	}
	return q
}

func WrapForeignErrorf(err error, msg string, vs ...any) *ForeignError {
	if err == nil {
		return NewForeignErrorf(msg, vs...)
	}
	pcs := [50]uintptr{}
	n := runtime.Callers(1, pcs[:])
	return &ForeignError{
		err:     err,
		message: fmt.Sprintf(msg, vs...),
		stack:   pcs[:n],
	}
}

func (p *ForeignError) Datum() error {
	return p.err
}

func (p *ForeignError) Unwrap() error {
	return p.err
}

func (q *ForeignError) Error() string {
	if q.err != nil {
		return fmt.Sprintf("%s: %s", q.message, q.err.Error())
	}
	return q.message
}
