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
	_ Value = (*Promise)(nil)
)

// Promise represents a delayed computation (R7RS lazy evaluation).
// A promise contains either an unevaluated thunk or a cached result.
type Promise struct {
	// Thunk is the procedure to evaluate (nil if already forced)
	Thunk Value
	// Result is the cached result (valid only if Forced is true)
	Result Value
	// Forced indicates whether the promise has been evaluated
	Forced bool
}

// NewPromise creates a new unforced promise with the given thunk.
// The thunk should be a procedure that takes no arguments.
func NewPromise(thunk Value) *Promise {
	return &Promise{
		Thunk:  thunk,
		Result: nil,
		Forced: false,
	}
}

// NewForcedPromise creates an already-forced promise with the given value.
// This is used by make-promise when given a non-promise value.
func NewForcedPromise(value Value) *Promise {
	return &Promise{
		Thunk:  nil,
		Result: value,
		Forced: true,
	}
}

func (p *Promise) IsVoid() bool {
	return p == nil
}

func (p *Promise) EqualTo(v Value) bool {
	other, ok := v.(*Promise)
	if !ok {
		return false
	}
	return p == other // Promises are compared by identity
}

func (p *Promise) SchemeString() string {
	if p.Forced {
		return "#<promise (forced)>"
	}
	return "#<promise>"
}
