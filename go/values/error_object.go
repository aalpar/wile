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

// ErrorObject represents an R7RS error object created by (error ...).
// It contains a message string and a list of irritant objects that
// provide additional context about the error.
type ErrorObject struct {
	message   *String
	irritants Value // List of irritant objects (may be EmptyList)
}

// NewErrorObject creates a new error object with the given message and irritants.
func NewErrorObject(message string, irritants ...Value) *ErrorObject {
	return &ErrorObject{
		message:   NewString(message),
		irritants: List(irritants...),
	}
}

// Message returns the error message string.
func (p *ErrorObject) Message() *String {
	return p.message
}

// Irritants returns the list of irritant objects.
func (p *ErrorObject) Irritants() Value {
	return p.irritants
}

// IsVoid returns true if this error object is nil.
func (p *ErrorObject) IsVoid() bool {
	return p == nil
}

// SchemeString returns the Scheme string representation of this error object.
func (p *ErrorObject) SchemeString() string {
	if p == nil {
		return "#<error-object>"
	}
	return fmt.Sprintf("#<error-object %q>", p.message.Datum())
}

// EqualTo returns true if this error object is equal to the given value.
func (p *ErrorObject) EqualTo(v Value) bool {
	other, ok := v.(*ErrorObject)
	if !ok {
		return false
	}
	if p == nil || other == nil {
		return p == other
	}
	return p.message.EqualTo(other.message) && EqualTo(p.irritants, other.irritants)
}
