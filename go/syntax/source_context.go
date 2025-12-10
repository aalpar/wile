package syntax

import (
	"fmt"
	"wile/values"
)

var (
	_ values.Value = (*SourceContext)(nil)
)

type SourceContext struct {
	Text   string
	File   string
	Start  SourceIndexes
	End    SourceIndexes
	Scopes []*Scope // Scopes associated with this source location
}

func NewSourceContext(text, file string, start, end SourceIndexes) *SourceContext {
	q := &SourceContext{
		Text:  text,
		File:  file,
		Start: start,
		End:   end,
	}
	return q
}

func NewZeroValueSourceContext() *SourceContext {
	q := &SourceContext{}
	return q
}

func (p *SourceContext) SchemeString() string {
	return fmt.Sprintf("<source-context %s:%d-%d>", p.File, p.Start, p.End)
}

func (p *SourceContext) IsVoid() bool {
	return p == nil
}

func (p *SourceContext) EqualTo(value values.Value) bool {
	v, ok := value.(*SourceContext)
	if !ok {
		return false
	}
	if p == v {
		return true
	}
	if p.Text != v.Text {
		return false
	}
	if p.File != v.File {
		return false
	}
	if !p.Start.EqualTo(v.Start) {
		return false
	}
	if !p.End.EqualTo(v.End) {
		return false
	}
	// Note: Not comparing scopes as they are for hygiene, not equality
	return true
}

// WithScope returns a new SourceContext with an additional scope.
//
// This is the primitive operation for adding hygiene scopes to syntax objects.
// In Flatt's "sets of scopes" model, each syntax object carries a set of scopes
// that identifies its binding context.
//
// Design Decision: Scopes are stored in SourceContext rather than on individual
// syntax types. This treats scopes as source-location metadata, keeping the
// syntax types simpler and the scope management centralized.
//
// The new scope is prepended to the list (most recent scope first). This
// doesn't affect the ScopesMatch algorithm, which uses set membership.
//
// Returns a NEW SourceContext (immutable design for syntax objects).
func (p *SourceContext) WithScope(scope *Scope) *SourceContext {
	if p == nil {
		return &SourceContext{
			Scopes: []*Scope{scope},
		}
	}
	newScopes := make([]*Scope, len(p.Scopes)+1)
	newScopes[0] = scope
	copy(newScopes[1:], p.Scopes)
	return &SourceContext{
		Text:   p.Text,
		File:   p.File,
		Start:  p.Start,
		End:    p.End,
		Scopes: newScopes,
	}
}

// WithScopes returns a new SourceContext with additional scopes
func (p *SourceContext) WithScopes(scopes []*Scope) *SourceContext {
	if p == nil {
		return &SourceContext{
			Scopes: scopes,
		}
	}
	if len(scopes) == 0 {
		return p
	}
	newScopes := make([]*Scope, len(scopes)+len(p.Scopes))
	copy(newScopes, scopes)
	copy(newScopes[len(scopes):], p.Scopes)
	return &SourceContext{
		Text:   p.Text,
		File:   p.File,
		Start:  p.Start,
		End:    p.End,
		Scopes: newScopes,
	}
}
