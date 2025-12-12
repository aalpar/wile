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
)

var (
	_ Value = (*Error)(nil)
)

// Error represents an native Scheme error value in the values package.
type Error struct {
	Message *String
}

func NewError(msg string) *Error {
	q := &Error{
		Message: NewString(msg),
	}
	return q
}

func NewErrorf(msg string, vs ...any) *Error {
	if len(vs) == 0 {
		return NewError(msg)
	}
	q := &Error{
		Message: NewString(fmt.Sprintf(msg, vs...)),
	}
	return q
}

func WrapErrorf(err error, msg string, vs ...any) *Error {
	if err == nil {
		return NewErrorf(msg, vs...)
	}
	return &Error{
		Message: NewString(fmt.Sprintf(msg, vs...)),
	}
}

func (p *Error) Datum() string {
	return p.Message.Datum()
}

func (p *Error) Unwrap() *String {
	return p.Message
}

func (q *Error) Error() string {
	return q.Message.Value
}

func (p *Error) IsVoid() bool {
	return p == nil
}

func (p *Error) EqualTo(v Value) bool {
	other, ok := v.(*Error)
	if !ok {
		return false
	}
	return p.Message == other.Message
}

func (p *Error) SchemeString() string {
	return p.Message.SchemeString()
}
