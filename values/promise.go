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

package values

var _ Value = (*Promise)(nil)

// Promise represents a delayed computation (R7RS lazy evaluation).
// A promise contains either an unevaluated thunk or a cached result.
//
// R7RS §4.2.5: The first time a promise is forced, its body is evaluated
// and the result is memoized; on subsequent forces, the memoized result
// is returned.
type Promise struct {
	// Thunk is the procedure to evaluate.
	// nil means the promise has been forced; Result is then valid.
	Thunk Callable
	// Result is the cached result (valid only when Thunk is nil)
	Result Value
}

// NewPromise creates a new unforced promise with the given thunk.
// The thunk should be a procedure that takes no arguments.
func NewPromise(thunk Callable) *Promise {
	return &Promise{
		Thunk: thunk,
	}
}

// NewForcedPromise creates an already-forced promise with the given value.
// This is used by make-promise when given a non-promise value.
func NewForcedPromise(value Value) *Promise {
	return &Promise{
		Result: value,
	}
}

// IsVoid returns true if the promise is nil.
func (p *Promise) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if the promises are the same object.
func (p *Promise) EqualTo(v Value) bool {
	other, ok := v.(*Promise)
	if !ok {
		return false
	}
	return p == other // Promises are compared by identity
}

// SchemeString returns the Scheme representation of the promise.
func (p *Promise) SchemeString() string {
	if p.Thunk == nil {
		return "#<promise (forced)>"
	}
	return "#<promise>"
}
