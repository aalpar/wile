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

import "fmt"

var _ Value = (*NativeError)(nil)

// ErrorKind represents the type of an error object for R7RS error predicates.
type ErrorKind int

const (
	// ErrorKindGeneric is a generic error (default).
	ErrorKindGeneric ErrorKind = iota
	// ErrorKindRead is a read error (from reading data).
	ErrorKindRead
	// ErrorKindFile is a file error (from file operations).
	ErrorKindFile
)

// NativeError represents an R7RS error object created by (error ...).
// It contains a message string and a list of irritant objects that
// provide additional context about the error. It can also wrap a Go error.
type NativeError struct {
	message   *String
	irritants Value     // List of irritant objects (may be EmptyList)
	kind      ErrorKind // Type of error for R7RS predicates
	err       error     // Wrapped Go error (optional)
}

// NewNativeError creates a new native error with the given message.
func NewNativeError(msg string) *NativeError {
	q := &NativeError{
		message:   NewString(msg),
		irritants: EmptyList,
		kind:      ErrorKindGeneric,
	}
	return q
}

// NewErrorObject creates a new error object with the given message and irritants.
func NewErrorObject(message string, irritants ...Value) *NativeError {
	q := &NativeError{
		message:   NewString(message),
		irritants: List(irritants...),
		kind:      ErrorKindGeneric,
	}
	return q
}

// NewReadError creates a new read error object with the given message and irritants.
// R7RS §6.11: read-error? predicate checks for errors during reading.
func NewReadError(message string, irritants ...Value) *NativeError {
	q := &NativeError{
		message:   NewString(message),
		irritants: List(irritants...),
		kind:      ErrorKindRead,
	}
	return q
}

// NewFileError creates a new file error object with the given message and irritants.
// R7RS §6.11: file-error? predicate checks for errors during file operations.
func NewFileError(message string, irritants ...Value) *NativeError {
	q := &NativeError{
		message:   NewString(message),
		irritants: List(irritants...),
	}
	return q
}

// Message returns the error message string.
func (p *NativeError) Message() *String {
	if p == nil {
		return nil
	}
	return p.message
}

// Irritants returns the list of irritant objects.
func (p *NativeError) Irritants() Value {
	if p == nil {
		return EmptyList
	}
	return p.irritants
}

// Kind returns the error kind for R7RS error predicates.
func (p *NativeError) Kind() ErrorKind {
	if p == nil {
		return ErrorKindGeneric
	}
	return p.kind
}

// IsReadError returns true if this is a read error.
func (p *NativeError) IsReadError() bool {
	return p != nil && p.kind == ErrorKindRead
}

// IsFileError returns true if this is a file error.
func (p *NativeError) IsFileError() bool {
	return p != nil && p.kind == ErrorKindFile
}

// Datum returns the underlying Go error, if any.
func (p *NativeError) Datum() error {
	if p == nil {
		return nil
	}
	return p.err
}

// Unwrap returns the underlying Go error for errors.Unwrap compatibility.
func (p *NativeError) Unwrap() error {
	if p == nil {
		return nil
	}
	return p.err
}

// Error implements the error interface.
func (p *NativeError) Error() string {
	if p == nil {
		return ""
	}
	if p.message != nil {
		return p.message.Datum()
	}
	return ""
}

// IsVoid returns true if this error object is nil.
func (p *NativeError) IsVoid() bool {
	return p == nil
}

// SchemeString returns the Scheme string representation of this error object.
func (p *NativeError) SchemeString() string {
	if p == nil {
		return "#<error-object>"
	}
	if p.message != nil {
		return fmt.Sprintf("#<error-object %q>", p.message.Datum())
	}
	return "#<error-object>"
}

// EqualTo returns true if this error object is equal to the given value.
func (p *NativeError) EqualTo(v Value) bool {
	other, ok := v.(*NativeError)
	if !ok {
		return false
	}
	if p == nil || other == nil {
		return p == other
	}
	// Compare messages
	if p.message != nil && other.message != nil {
		if !p.message.EqualTo(other.message) {
			return false
		}
	} else if p.message != other.message {
		return false
	}
	// Compare irritants
	if !EqualTo(p.irritants, other.irritants) {
		return false
	}
	// Compare kind
	if p.kind != other.kind {
		return false
	}
	// Compare wrapped error
	if p.err != other.err {
		return false
	}
	return true
}
