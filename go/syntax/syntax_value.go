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

package syntax

import (
	"fmt"
	"wile/values"
)

var (
	_ values.Value = (*SyntaxObject)(nil)
	_ SyntaxValue  = (*SyntaxObject)(nil)
)

type ScopeID int64

// Scope represents a single scope in the syntax object.
// It maps symbols to their corresponding ScopeID and maintains a reference to its parent scope.
// This structure allows for nested scopes, enabling proper variable resolution in a scoped environment.
// Identifiers match only if their symbol name and scope sets match the same binding.
// This means two identifiers with the same textual name are distinct if their scopes differ.
type Scope struct {
	keys   map[values.Symbol]ScopeID
	parent *Scope
}

// NewScope creates a new scope with an optional parent
func NewScope(parent *Scope) *Scope {
	return &Scope{
		keys:   make(map[values.Symbol]ScopeID),
		parent: parent,
	}
}

// nextScopeID is a simple counter for generating unique scope IDs
var nextScopeID uint64

// NewScopeID generates a unique scope ID
func NewScopeID() ScopeID {
	nextScopeID++
	return ScopeID(nextScopeID)
}

// NewSyntaxNil creates a syntax empty list.
// Deprecated: Use NewSyntaxEmptyList instead. This function exists for backward
// compatibility but delegates to NewSyntaxEmptyList.
func NewSyntaxNil(sctx *SourceContext) *SyntaxPair {
	return NewSyntaxEmptyList(sctx)
}

type SyntaxObject struct {
	Datum         values.Value
	sourceContext *SourceContext
}

// NewSyntaxObject creates a new SyntaxObject wrapping the given value and source context.
// It panics if the value is already a syntax value to prevent double-wrapping.
func NewSyntaxObject(v values.Value, sctx *SourceContext) *SyntaxObject {
	switch v.(type) {
	case *SyntaxObject, *SyntaxVector, *SyntaxPair, *SyntaxSymbol:
		panic(fmt.Sprintf("cannot wrap a %T in another SyntaxObject", v))
	case *values.Vector, *values.Pair, *values.Symbol:
		panic(fmt.Sprintf("cannot wrap a %T in another SyntaxObject", v))
	}
	q := &SyntaxObject{
		Datum:         v,
		sourceContext: sctx,
	}
	return q
}

// AddScope returns a new SyntaxObject with an additional scope.
// It recursively adds the scope to the datum if it's a syntax value.
// Returns SyntaxValue interface to support recursive scope propagation.
func (p *SyntaxObject) AddScope(scope *Scope) SyntaxValue {
	// If the datum is a syntax value, recursively add scope to it
	var newDatum = p.Datum
	if stx, ok := p.Datum.(SyntaxValue); ok {
		if adder, ok := stx.(interface{ AddScope(*Scope) SyntaxValue }); ok {
			newDatum = adder.AddScope(scope)
		}
	}
	return &SyntaxObject{
		Datum:         newDatum,
		sourceContext: p.sourceContext.WithScope(scope),
	}
}

// Scopes returns the scopes of this syntax object
func (p *SyntaxObject) Scopes() []*Scope {
	if p.sourceContext == nil {
		return nil
	}
	return p.sourceContext.Scopes
}

func (p *SyntaxObject) UnwrapAll() values.Value {
	switch v := p.Datum.(type) {
	case SyntaxValue:
		return v.UnwrapAll()
	}
	return p.Unwrap()
}

func (p *SyntaxObject) Unwrap() values.Value {
	return p.Datum
}

func (p *SyntaxObject) IsPair() bool {
	_, ok := p.Datum.(*values.Pair)
	return ok
}

func (p *SyntaxObject) IsEmptyList() bool {
	q, ok := p.Datum.(*values.Pair)
	if !ok {
		return false
	}
	if !q.IsEmptyList() {
		return false
	}
	return true
}

func (p *SyntaxObject) SourceContext() *SourceContext {
	return p.sourceContext
}

func (p *SyntaxObject) IsVoid() bool {
	return p == nil
}

func (p *SyntaxObject) SchemeString() string {
	return fmt.Sprintf("#'%s", p.Datum.SchemeString())
}

// EqualTo performs pointer comparison only, matching Chez Scheme/Racket behavior.
// Two syntax objects are equal? only if they are the same object.
// For value comparison of syntax objects, use bound-identifier=? or free-identifier=?.
func (p *SyntaxObject) EqualTo(v values.Value) bool {
	other, ok := v.(*SyntaxObject)
	if !ok {
		return false
	}
	return p == other
}
