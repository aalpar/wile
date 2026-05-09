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

package environment

import (
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
)

// BindingMeta holds compile-time metadata (scopes and source location) that
// is never read during VM execution. Stored behind a pointer so that runtime
// Binding copies (the hot path) move 32 bytes instead of 56.
type BindingMeta struct {
	Scopes   []*syntax.Scope
	Source   *syntax.SourceContext
	Doc      string
	Imported bool
	Constant bool
}

// Binding represents a variable binding in the environment.
// It stores the bound value, the binding type (variable, syntax, or primitive),
// and an optional pointer to compile-time metadata (scopes, source location).
type Binding struct {
	value       values.Value
	bindingType BindingType
	meta        *BindingMeta
}

// NewBinding creates a new binding with the given value and type.
// The binding has no scopes (for backward compatibility with non-hygienic code).
func NewBinding(value values.Value, bindingType BindingType) *Binding {
	return &Binding{
		value:       value,
		bindingType: bindingType,
	}
}

// NewBindingWithScopes creates a binding with associated scopes (for hygiene)
func NewBindingWithScopes(value values.Value, bindingType BindingType, scopes []*syntax.Scope) *Binding {
	return &Binding{
		value:       value,
		bindingType: bindingType,
		meta: &BindingMeta{
			Scopes: scopes,
		},
	}
}

// NewBindingWithSource creates a binding with source location information.
func NewBindingWithSource(value values.Value, bindingType BindingType, scopes []*syntax.Scope, source *syntax.SourceContext) *Binding {
	return &Binding{
		value:       value,
		bindingType: bindingType,
		meta: &BindingMeta{
			Scopes: scopes,
			Source: source,
		},
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

// Scopes returns the hygiene scopes associated with this binding.
// Returns nil for bindings without hygiene information.
func (p *Binding) Scopes() []*syntax.Scope {
	if p.meta == nil {
		return nil
	}
	return p.meta.Scopes
}

// SetScopes updates the hygiene scopes associated with this binding.
func (p *Binding) SetScopes(scopes []*syntax.Scope) {
	if p.meta == nil {
		p.meta = &BindingMeta{}
	}
	p.meta.Scopes = scopes
}

// Source returns the source location where this binding was defined.
// Returns nil for bindings without source information.
func (p *Binding) Source() *syntax.SourceContext {
	if p.meta == nil {
		return nil
	}
	return p.meta.Source
}

// SetSource updates the source location for this binding.
func (p *Binding) SetSource(source *syntax.SourceContext) {
	if p.meta == nil {
		p.meta = &BindingMeta{}
	}
	p.meta.Source = source
}

// Doc returns the documentation string for this binding.
// Returns empty string for bindings without documentation.
func (p *Binding) Doc() string {
	if p.meta == nil {
		return ""
	}
	return p.meta.Doc
}

// SetDoc updates the documentation string for this binding.
func (p *Binding) SetDoc(doc string) {
	if p.meta == nil {
		p.meta = &BindingMeta{}
	}
	p.meta.Doc = doc
}

// IsImported returns whether this binding was imported from a library.
func (p *Binding) IsImported() bool {
	if p.meta == nil {
		return false
	}
	return p.meta.Imported
}

// SetImported marks this binding as imported from a library.
func (p *Binding) SetImported(v bool) {
	if p.meta == nil {
		p.meta = &BindingMeta{}
	}
	p.meta.Imported = v
}

// IsConstant returns whether this binding's value is known at compile time.
func (p *Binding) IsConstant() bool {
	if p.meta == nil {
		return false
	}
	return p.meta.Constant
}

// SetConstant marks this binding's value as known at compile time.
func (p *Binding) SetConstant(v bool) {
	if p.meta == nil {
		p.meta = &BindingMeta{}
	}
	p.meta.Constant = v
}

// Copy creates a deep copy of this binding. The meta struct is copied so
// that SetScopes on the original does not affect the copy. This method is
// only used during compilation/expansion, never on the runtime hot path.
func (p *Binding) Copy() *Binding {
	b := &Binding{
		value:       p.value,
		bindingType: p.bindingType,
	}
	if p.meta != nil {
		b.meta = &BindingMeta{
			Scopes:   p.meta.Scopes,
			Source:   p.meta.Source,
			Doc:      p.meta.Doc,
			Imported: p.meta.Imported,
			Constant: p.meta.Constant,
		}
	}
	return b
}
