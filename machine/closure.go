package machine

import "github.com/aalpar/wile/values"

// Closure is a callable that can be directly applied — either a compiled
// Scheme closure (*MachineClosure) or a Go foreign function (*ForeignClosure).
// Distinguished from other Callable types (CaseLambdaClosure, Parameter,
// ComposableContinuation) which have different application semantics.
type Closure interface {
	values.Callable
	closureMarker()
}
