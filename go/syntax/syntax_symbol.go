package syntax

import (
	"fmt"
	"wile/values"
)

var (
	_ values.Value = (*SyntaxSymbol)(nil)
	_ SyntaxValue  = (*SyntaxSymbol)(nil)
)

type SyntaxSymbol struct {
	Key           string
	sourceContext *SourceContext
}

func NewSyntaxSymbol(key string, sctx *SourceContext) *SyntaxSymbol {
	q := &SyntaxSymbol{
		Key:           key,
		sourceContext: sctx,
	}
	return q
}

// AddScope returns a new SyntaxSymbol with an additional scope.
//
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
		Key:           p.Key,
		sourceContext: p.sourceContext.WithScope(scope),
	}
}

// Scopes returns the scopes of this syntax symbol
func (p *SyntaxSymbol) Scopes() []*Scope {
	if p.sourceContext == nil {
		return nil
	}
	return p.sourceContext.Scopes
}

func (p *SyntaxSymbol) Datum() *values.Symbol {
	if p.IsVoid() {
		return nil
	}
	return values.NewSymbol(p.Key)
}

func (p *SyntaxSymbol) UnwrapAll() values.Value {
	return p.Unwrap()
}

func (p *SyntaxSymbol) Unwrap() values.Value {
	if p.IsVoid() {
		return values.Void
	}
	return values.NewSymbol(p.Key)
}

func (p *SyntaxSymbol) SourceContext() *SourceContext {
	return p.sourceContext
}

func (p *SyntaxSymbol) IsVoid() bool {
	return p == nil
}

func (p *SyntaxSymbol) SchemeString() string {
	if p.IsVoid() {
		return "#'<void>"
	}
	return fmt.Sprintf("#'%s", p.Key)
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
