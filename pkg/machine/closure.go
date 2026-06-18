package machine

import "github.com/aalpar/wile/pkg/values"

// NamedCallable is implemented by callable values that carry a name and
// documentation string. MachineClosure, ForeignClosure, and CaseLambdaClosure
// all satisfy this interface.
type NamedCallable interface {
	Name() string
	Doc() string
}

// Closure is a callable that can be directly applied — either a compiled
// Scheme closure (*MachineClosure) or a Go foreign function (*ForeignClosure).
// Distinguished from other Callable types (CaseLambdaClosure, Parameter,
// ComposableContinuation) which have different application semantics.
type Closure interface {
	values.Callable
	NamedCallable
	closureMarker()
}
