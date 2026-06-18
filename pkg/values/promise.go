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
	// thunk is the procedure to evaluate.
	// nil means the promise has been forced; result is then valid.
	thunk Callable
	// result is the cached result (valid only when thunk is nil)
	result Value
}

// NewPromise creates a new unforced promise with the given thunk.
// The thunk should be a procedure that takes no arguments.
func NewPromise(thunk Callable) *Promise {
	return &Promise{
		thunk: thunk,
	}
}

// NewForcedPromise creates an already-forced promise with the given value.
// This is used by make-promise when given a non-promise value.
func NewForcedPromise(value Value) *Promise {
	return &Promise{
		result: value,
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
	if p.thunk == nil {
		return "#<promise (forced)>"
	}
	return "#<promise>"
}

// IsForced reports whether the promise has been forced.
// A forced promise has a cached result and no thunk.
func (p *Promise) IsForced() bool {
	return p.thunk == nil
}

// CachedResult returns the memoized result of a forced promise.
// Only valid when IsForced returns true.
func (p *Promise) CachedResult() Value {
	return p.result
}

// Thunk returns the unevaluated procedure.
// Returns nil when the promise has been forced.
func (p *Promise) Thunk() Callable {
	return p.thunk
}

// Force transitions the promise from unforced to forced, caching
// the given result and clearing the thunk. Subsequent calls to
// IsForced return true and CachedResult returns the cached value.
func (p *Promise) Force(result Value) {
	p.result = result
	p.thunk = nil
}
