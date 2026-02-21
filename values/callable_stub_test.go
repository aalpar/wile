package values_test

import "github.com/aalpar/wile/values"

// stubCallable is a minimal Callable implementation for unit tests that need
// to construct Threads, Promises, or ExceptionHandlers without importing the
// machine package. It wraps a Value (for identity/display) and accepts any arity.
type stubCallable struct {
	values.Value
}

func (stubCallable) AcceptsArity(int) bool {
	return true
}

// EqualTo compares via the wrapped value's EqualTo, unwrapping the other
// side if it is also a stubCallable.
func (s stubCallable) EqualTo(other values.Value) bool {
	o, ok := other.(stubCallable)
	if ok {
		return s.Value.EqualTo(o.Value)
	}
	return s.Value.EqualTo(other)
}

// newStubCallable creates a stubCallable wrapping the given value.
func newStubCallable(v values.Value) stubCallable {
	return stubCallable{Value: v}
}
