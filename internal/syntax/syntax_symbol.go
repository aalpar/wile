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

package syntax

import (
	"fmt"

	"github.com/aalpar/wile/values"
)

var (
	_ values.Value = (*SyntaxSymbol)(nil)
	_ SyntaxValue  = (*SyntaxSymbol)(nil)
)

// SymbolInterner is an interface for interning symbols.
// This allows SyntaxSymbol to cache interned symbols without importing
// the environment package (which would create a circular dependency).
type SymbolInterner interface {
	InternSymbol(*values.Symbol) *values.Symbol
}

// SyntaxSymbol wraps a Scheme symbol with source context and hygiene scopes.
type SyntaxSymbol struct {
	Sym *values.Symbol
	syntaxBase
	// ResolvedBinding holds a pre-resolved binding for free identifiers in macro templates.
	// This is set during macro expansion for identifiers that should resolve to bindings
	// in the macro's definition environment rather than the use-site environment.
	// Type: *environment.GlobalIndex (stored as any to avoid circular import).
	// nil for normal symbols; only set for free identifiers from macros.
	ResolvedBinding any
}

// NewSyntaxSymbol creates a new syntax symbol from a key string.
func NewSyntaxSymbol(key string, sctx *SourceContext) *SyntaxSymbol {
	q := NewSyntaxSymbolForSymbol(values.NewSymbol(key), sctx)
	return q
}

// NewSyntaxSymbolForSymbol creates a new syntax symbol from an existing symbol.
func NewSyntaxSymbolForSymbol(sym *values.Symbol, sctx *SourceContext) *SyntaxSymbol {
	q := &SyntaxSymbol{
		Sym: sym,
		syntaxBase: syntaxBase{
			sourceContext: sctx,
		},
	}
	return q
}

// NewSyntaxSymbolForSyntaxSymbol creates a new syntax symbol with a different source context.
func NewSyntaxSymbolForSyntaxSymbol(sym *SyntaxSymbol, sctx *SourceContext) *SyntaxSymbol {
	q := &SyntaxSymbol{
		Sym: sym.Sym,
		syntaxBase: syntaxBase{
			sourceContext: sctx,
		},
	}
	return q
}

// AddScope returns a new SyntaxSymbol with an additional scope.
// This is the core operation for implementing hygiene in Flatt's "sets of scopes"
// model. When a macro expands, an "intro scope" is added to all identifiers in
// the expansion. This scope distinguishes macro-introduced identifiers from
// user-provided ones.
//
// The method returns a NEW SyntaxSymbol (syntax objects are immutable) with the
// scope added to its SourceContext. The SyntaxValue return type supports
// recursive scope propagation through nested syntax structures.
//
// Example: When swap! macro introduces "tmp", that "tmp" gets the macro's intro
// scope. A user's "tmp" at the call site doesn't have this scope, so they're
// distinguished during variable resolution (see ScopesMatch in scope_utils.go).
func (p *SyntaxSymbol) AddScope(scope *Scope) SyntaxValue {
	return &SyntaxSymbol{
		Sym: p.Sym,
		syntaxBase: syntaxBase{
			sourceContext: p.sourceContext.WithScope(scope),
		},
		ResolvedBinding: p.ResolvedBinding, // Preserve resolved binding
	}
}

// WithResolvedBinding returns a new SyntaxSymbol with the given pre-resolved binding.
// This is used during macro expansion to tag free identifiers with their
// definition-site bindings, enabling proper resolution across library boundaries.
func (p *SyntaxSymbol) WithResolvedBinding(binding any) *SyntaxSymbol {
	return &SyntaxSymbol{
		Sym: p.Sym,
		syntaxBase: syntaxBase{
			sourceContext: p.sourceContext,
		},
		ResolvedBinding: binding,
	}
}

// Scopes returns the scopes of this syntax symbol
func (p *SyntaxSymbol) Scopes() []*Scope {
	if p.sourceContext == nil {
		return nil
	}
	return p.sourceContext.Scopes
}

// Datum returns the underlying symbol.
func (p *SyntaxSymbol) Datum() *values.Symbol {
	if p.IsVoid() {
		return nil
	}
	return p.Sym
}

// UnwrapAll returns the underlying symbol value.
func (p *SyntaxSymbol) UnwrapAll() values.Value {
	return UnwrapAllShared(p, make(map[SyntaxValue]values.Value))
}

func (p *SyntaxSymbol) Unwrap() values.Value {
	if p.IsVoid() {
		return values.Void
	}
	return p.Sym
}

// IsVoid returns true if the syntax symbol is nil.
func (p *SyntaxSymbol) IsVoid() bool {
	return p == nil
}

// SchemeString returns a string representation of the syntax symbol.
func (p *SyntaxSymbol) SchemeString() string {
	if p.IsVoid() {
		return "#'<void>"
	}
	return fmt.Sprintf("#'%s", p.Sym.Key)
}

// EqualTo performs pointer comparison only, matching Chez Scheme/Racket behavior.
// Two syntax objects are equal? only if they are the same object.
// For value comparison of syntax objects, use bound-identifier=? or free-identifier=?.
func (p *SyntaxSymbol) EqualTo(o values.Value) bool {
	v, ok := o.(*SyntaxSymbol)
	if !ok {
		return false
	}
	return p == v
}
