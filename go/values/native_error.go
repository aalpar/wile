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

var _ Value = (*NativeError)(nil)

// NativeError represents a native Go error wrapped as a Scheme value.
type NativeError struct {
	err     error
	message string
}

// NewNativeError creates a new native error with the given message.
func NewNativeError(msg string) *NativeError {
	q := &NativeError{message: msg}
	return q
}

// Datum returns the underlying error.
func (p *NativeError) Datum() error {
	return p.err
}

func (p *NativeError) Unwrap() error {
	return p.err
}

func (p *NativeError) Error() string {
	return p.message
}

// SchemeString returns the Scheme representation of the native error.
func (p *NativeError) SchemeString() string {
	return "#<native-error>"
}

// IsVoid returns true if the error is nil.
func (p *NativeError) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if both errors wrap the same underlying error.
func (p *NativeError) EqualTo(o Value) bool {
	v, ok := o.(*NativeError)
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return v == p
	}
	if p.err != v.err {
		return false
	}
	if p.message != v.message {
		return false
	}
	return true
}
