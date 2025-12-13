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

var (
	_ Value = (*NativeError)(nil)
)

type NativeError struct {
	err     error
	message string
}

func NewNativeError(msg string) *NativeError {
	q := &NativeError{message: msg}
	return q
}

func (p *NativeError) Datum() error {
	return p.err
}

func (p *NativeError) Unwrap() error {
	return p.err
}

func (p *NativeError) Error() string {
	return p.message
}

func (p *NativeError) SchemeString() string {
	return "#<native-error>"
}

func (p *NativeError) IsVoid() bool {
	return p == nil
}

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
