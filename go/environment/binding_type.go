package environment

// BindingType represents the type of a binding in the environment.
type BindingType int

const (
	// BindingTypeUnknown indicates a binding with unknown or uninitialized type.
	BindingTypeUnknown = BindingType(iota)
	// BindingTypeVariable indicates a regular variable binding (from define, let, lambda parameters).
	BindingTypeVariable
	// BindingTypeSyntax indicates a syntax transformer binding (from define-syntax).
	// These bindings live in the expand phase environment.
	BindingTypeSyntax
	// BindingTypePrimitive indicates a built-in primitive procedure.
	BindingTypePrimitive
)
