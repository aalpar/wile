package environment

import (
	"fmt"
	"skeme/syntax"
	"skeme/values"
	"slices"
)

// Binding represents a variable binding in the environment.
// It stores the bound value, the binding type (variable, syntax, or primitive),
// and optional scopes for hygienic macro expansion.
type Binding struct {
	value       values.Value
	bindingType BindingType
	scopes      []*syntax.Scope // Scopes associated with this binding
}

// NewBinding creates a new binding with the given value and type.
// The binding has no scopes (for backward compatibility with non-hygienic code).
func NewBinding(value values.Value, bindingType BindingType) *Binding {
	q := &Binding{
		value:       value,
		bindingType: bindingType,
		scopes:      nil, // No scopes by default for backward compatibility
	}
	return q
}

// NewBindingWithScopes creates a binding with associated scopes (for hygiene)
func NewBindingWithScopes(value values.Value, bindingType BindingType, scopes []*syntax.Scope) *Binding {
	q := &Binding{
		value:       value,
		bindingType: bindingType,
		scopes:      scopes,
	}
	return q
}

// Value returns the value stored in this binding.
func (p *Binding) Value() values.Value {
	return p.value
}

// BindingType returns the type of this binding (variable, syntax, or primitive).
func (p *Binding) BindingType() BindingType {
	return p.bindingType
}

// SetValue updates the value stored in this binding.
func (p *Binding) SetValue(value values.Value) {
	p.value = value
}

// SetBindingType updates the type of this binding.
func (p *Binding) SetBindingType(value BindingType) {
	p.bindingType = value
}

// Scopes returns the hygiene scopes associated with this binding.
// Returns nil for bindings without hygiene information.
func (p *Binding) Scopes() []*syntax.Scope {
	return p.scopes
}

// SetScopes updates the hygiene scopes associated with this binding.
func (p *Binding) SetScopes(scopes []*syntax.Scope) {
	p.scopes = scopes
}

// SchemeString returns a string representation of this binding.
func (p *Binding) SchemeString() string {
	return fmt.Sprintf("#<binding>")
}

// IsVoid returns true if this binding is nil.
func (p *Binding) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if this binding is equal to the given value.
// Two bindings are equal if they have the same value and binding type.
func (p *Binding) EqualTo(o values.Value) bool {
	if p == nil || o == nil {
		return p == o
	}
	v, ok := o.(*Binding)
	if !ok {
		return false
	}
	if p.value == nil || v.value == nil {
		return p.value == v.value
	}
	return p.value.EqualTo(v.value) && p.bindingType == v.bindingType
}

// Copy creates a deep copy of this binding, including the scopes slice.
func (p *Binding) Copy() values.Value {
	// Copy the scopes slice to avoid shared references
	var scopesCopy []*syntax.Scope
	if p.scopes != nil {
		scopesCopy = slices.Clone(p.scopes)
	}

	q := &Binding{
		value:       p.value,
		bindingType: p.bindingType,
		scopes:      scopesCopy,
	}
	return q
}
