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

package compilation

// Bridge types that cross the compile-time / run-time seam inside this
// package. The compiler creates instances and stores them in NativeTemplate
// literals; the VM operations, which also live here, type-assert and consume
// them.

import (
	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/internal/match"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// FreeIdResolution records how a free identifier in a syntax-rules template
// was bound at macro definition time. The match package uses this during
// template expansion to preserve hygiene.
//
// Implements localScopesProvider, globalBindingProvider,
// hasLocalBindingProvider, and libraryScopeProvider from the match package.
type FreeIdResolution struct {
	Global          *environment.GlobalIndex
	LocalScopes     []*syntax.Scope
	HasLocalBinding bool
	LibScope        *syntax.Scope
}

// GetLocalScopes implements the localScopesProvider interface.
func (p *FreeIdResolution) GetLocalScopes() []*syntax.Scope {
	if p == nil {
		return nil
	}
	return p.LocalScopes
}

// GetGlobal implements the globalBindingProvider interface.
func (p *FreeIdResolution) GetGlobal() *environment.GlobalIndex {
	if p == nil {
		return nil
	}
	return p.Global
}

// GetLibraryScope implements the libraryScopeProvider interface.
func (p *FreeIdResolution) GetLibraryScope() *syntax.Scope {
	if p == nil {
		return nil
	}
	return p.LibScope
}

// GetHasLocalBinding implements the hasLocalBindingProvider interface.
func (p *FreeIdResolution) GetHasLocalBinding() bool {
	if p == nil {
		return false
	}
	return p.HasLocalBinding
}

// SyntaxRulesClause represents a single compiled pattern-template pair
// in a syntax-rules form. Created by the compiler, consumed by
// OperationSyntaxRulesTransform at runtime.
type SyntaxRulesClause struct {
	Template         syntax.SyntaxValue
	Bytecode         []match.SyntaxCommand
	Matcher          *match.SyntaxMatcher
	PatternVars      map[string]struct{}
	PatternVarSyntax map[string]*syntax.SyntaxSymbol
	EllipsisVars     map[int]map[string]struct{}
	FreeIds          map[string]*FreeIdResolution
	Ellipsis         string
	LiteralSyntax    map[string]*syntax.SyntaxSymbol
}

// ClausesWrapper wraps a slice of SyntaxRulesClause as a values.Value
// for storage in NativeTemplate literals.
type ClausesWrapper struct {
	Clauses []*SyntaxRulesClause
}

func (p *ClausesWrapper) EqualTo(other values.Value) bool {
	return false
}

func (p *ClausesWrapper) IsVoid() bool {
	return false
}

func (p *ClausesWrapper) SchemeString() string {
	return "#<syntax-rules-clauses>"
}

// SyntaxCaseClause wraps compiled pattern info for a syntax-case clause.
// Created by the compiler, consumed by OperationSyntaxCaseMatch at runtime.
type SyntaxCaseClause struct {
	Bytecode    []match.SyntaxCommand
	PatternVars map[string]struct{}
	// LiteralSyntax carries each pattern literal with the scopes it had at the
	// macro definition site, the same data SyntaxRulesClause.LiteralSyntax
	// carries. Without it match.go's hygiene block is gated off on this path
	// (it needs a non-nil literal map AND a non-nil matcher), so a syntax-case
	// pattern literal matched a use-site identifier that shadows it — R7RS
	// §4.3.2 requires the two to share a binding.
	LiteralSyntax  map[string]*syntax.SyntaxSymbol
	EllipsisVars   map[int]map[string]struct{}
	EllipsisDepths map[int]int
}

func (p *SyntaxCaseClause) EqualTo(other values.Value) bool {
	return false
}

func (p *SyntaxCaseClause) IsVoid() bool {
	return false
}

func (p *SyntaxCaseClause) SchemeString() string {
	return "#<syntax-case-clause>"
}
