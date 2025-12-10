package values

import "fmt"

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
		return fmt.Sprintf("#<promise (forced)>")
	}
	return fmt.Sprintf("#<promise>")
}
