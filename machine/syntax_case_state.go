package machine

// SyntaxCaseState is the typed back-channel for syntax-case expansion state.
//
// The expansion state is owned by package machine/compilation/. Because
// machine/ cannot import compilation/ (the dependency must run in only one
// direction, per Parnas), MachineContext historically stored this state in
// an any-typed field — any value of any type satisfied the type, so type
// precision was 1/∞. Every read paid for a runtime type assertion.
//
// This interface narrows the field's type to "values that have explicitly
// declared themselves syntax-case payloads". Any type stored on
// MachineContext.syntaxCase must implement IsSyntaxCaseState; unauthorized
// stores are now rejected at compile time. By convention, exactly one
// implementation exists in the workspace: *compilation.syntaxCaseState.
//
// The marker method is exported because Go's "unexported method seals an
// interface" pattern only works when the implementer lives in the same
// package as the interface — which is not the case here. The marker name
// is specific enough that collisions are zero in practice.
type SyntaxCaseState interface {
	// IsSyntaxCaseState declares that the receiver is syntax-case expansion
	// state suitable for storage on MachineContext. The method has no body
	// and produces no value; its sole purpose is to attest to the type's
	// role as a back-channel payload.
	IsSyntaxCaseState()
}
