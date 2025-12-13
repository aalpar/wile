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

package environment

import (
	"wile/syntax"
	"wile/values"
	"slices"
)

// Binding represents a variable binding in the environment.
// It stores the bound value, the binding type (variable, syntax, or primitive),
// optional scopes for hygienic macro expansion, and optional source location.
type Binding struct {
	value       values.Value
	bindingType BindingType
	scopes      []*syntax.Scope       // Scopes associated with this binding
	source      *syntax.SourceContext // Where this binding was defined (optional)
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

// NewBindingWithSource creates a binding with source location information.
func NewBindingWithSource(value values.Value, bindingType BindingType, scopes []*syntax.Scope, source *syntax.SourceContext) *Binding {
	return &Binding{
		value:       value,
		bindingType: bindingType,
		scopes:      scopes,
		source:      source,
	}
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

// Source returns the source location where this binding was defined.
// Returns nil for bindings without source information.
func (p *Binding) Source() *syntax.SourceContext {
	return p.source
}

// SetSource updates the source location for this binding.
func (p *Binding) SetSource(source *syntax.SourceContext) {
	p.source = source
}

// SchemeString returns a string representation of this binding.
func (p *Binding) SchemeString() string {
	return "#<binding>"
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
		source:      p.source, // Source context is immutable, no need to copy
	}
	return q
}
