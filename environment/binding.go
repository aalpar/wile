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
	// Stable is the conclusion of a rebind-stability proof: the binding will
	// not be rebound. It is set ONLY by a completed proof, never as a synonym
	// for evidence. Imported (above) is *evidence* sufficient for that
	// conclusion — R7RS forbids set! on imports — so IsStable() treats Imported
	// as standing evidence and this flag carries the conclusion when a proof
	// discharges it by other means (defined-once ∧ ¬set! ∧ unit-closed for a
	// top-level define). The opt-in WithImmutableTopLevel engine option
	// discharges it for top-level defines: the compiler sets this from the
	// validator's in-unit evidence (StableInUnit) and the language then forbids
	// the cross-unit set!/redefine that evidence alone could not rule out (set!
	// gate + redefine guard in compile_validated.go), making unit-closure hold
	// by enforcement rather than inference. When the option is off (default),
	// this flag stays false for non-imported bindings — asserting it from
	// partial evidence would be a false conclusion. Read by the frame
	// optimizer's MayCapture. Distinct from set!-permission (Imported alone,
	// unless the option is on) and from the retired "Constant" flag (which
	// conflated provenance, stability, and compile-time-value-known).
	Stable bool
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

// Meta returns the BindingMeta pointer, or nil if no metadata has been
// attached. Callers that read metadata fields should nil-check the
// returned pointer; the convenience getters (Scopes, Source, Doc,
// IsImported, IsStable) wrap this pattern.
func (p *Binding) Meta() *BindingMeta {
	return p.meta
}

// EnsureMeta returns the BindingMeta pointer, lazily allocating an empty
// BindingMeta on first call. This is the only mutator API for metadata
// fields: callers assign directly, e.g.
//
//	b.EnsureMeta().Imported = true
//
// Adding a new metadata field thus requires editing only the BindingMeta
// struct itself; no parallel getter/setter accessor pair is needed.
func (p *Binding) EnsureMeta() *BindingMeta {
	if p.meta == nil {
		p.meta = &BindingMeta{}
	}
	return p.meta
}

// Scopes returns the hygiene scopes associated with this binding.
// Returns nil for bindings without hygiene information.
func (p *Binding) Scopes() []*syntax.Scope {
	if p.meta == nil {
		return nil
	}
	return p.meta.Scopes
}

// Source returns the source location where this binding was defined.
// Returns nil for bindings without source information.
func (p *Binding) Source() *syntax.SourceContext {
	if p.meta == nil {
		return nil
	}
	return p.meta.Source
}

// Doc returns the documentation string for this binding.
// Returns empty string for bindings without documentation.
func (p *Binding) Doc() string {
	if p.meta == nil {
		return ""
	}
	return p.meta.Doc
}

// IsImported returns whether this binding was imported from a library.
func (p *Binding) IsImported() bool {
	if p.meta == nil {
		return false
	}
	return p.meta.Imported
}

// IsStable reports the rebind-stability conclusion: the binding will not be
// rebound. Imported is standing evidence for that conclusion (R7RS forbids set!
// on imports); Stable carries it when a proof discharges it by other means.
// This is NOT a set!-permission — that is IsImported alone (R7RS §5.2). Read by
// the frame optimizer's MayCapture (sibling escape-gated plan). Renamed from the
// retired IsConstant, which falsely asserted "value known at compile time".
func (p *Binding) IsStable() bool {
	if p.meta == nil {
		return false
	}
	return p.meta.Imported || p.meta.Stable
}

// Copy creates a deep copy of this binding. The meta struct is copied so
// that mutations through EnsureMeta on the original do not affect the
// copy. This method is only used during compilation/expansion, never on
// the runtime hot path.
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
			Stable:   p.meta.Stable,
		}
	}
	return b
}
