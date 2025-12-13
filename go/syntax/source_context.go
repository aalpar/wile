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
	_ values.Value = (*SourceContext)(nil)
)

// OriginInfo tracks macro expansion chains for better error reporting.
// Each OriginInfo represents one macro expansion in the chain.
type OriginInfo struct {
	Identifier string         // Macro name that caused expansion (e.g., "let", "my-macro")
	Location   *SourceContext // Where the macro was invoked (use-site)
	Parent     *OriginInfo    // Previous link in origin chain (for nested macros)
}

// Depth returns the length of the origin chain.
func (o *OriginInfo) Depth() int {
	depth := 0
	for curr := o; curr != nil; curr = curr.Parent {
		depth++
	}
	return depth
}

type SourceContext struct {
	Text   string
	File   string
	Start  SourceIndexes
	End    SourceIndexes
	Scopes []*Scope    // Scopes associated with this source location
	Origin *OriginInfo // Macro expansion origin chain (nil if not from macro)
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

// WithOrigin returns a new SourceContext with the given origin chain.
// Used to attach macro expansion tracking information to syntax objects.
func (p *SourceContext) WithOrigin(origin *OriginInfo) *SourceContext {
	if p == nil {
		return &SourceContext{Origin: origin}
	}
	return &SourceContext{
		Text:   p.Text,
		File:   p.File,
		Start:  p.Start,
		End:    p.End,
		Scopes: p.Scopes,
		Origin: origin,
	}
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
		Origin: p.Origin,
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
		Origin: p.Origin,
	}
}

// FormatOriginChain returns a formatted string showing the macro expansion chain.
// maxDepth limits how many expansions to show (0 = unlimited).
func FormatOriginChain(origin *OriginInfo, maxDepth int) string {
	if origin == nil {
		return ""
	}
	var result string
	depth := 0
	for o := origin; o != nil; o = o.Parent {
		depth++
		if maxDepth > 0 && depth > maxDepth {
			remaining := origin.Depth() - maxDepth
			result += fmt.Sprintf("\n  ... and %d more expansion(s)", remaining)
			break
		}
		result += fmt.Sprintf("\n  expanded from '%s'", o.Identifier)
		if o.Location != nil && o.Location.File != "" {
			result += fmt.Sprintf(" at %s:%d:%d",
				o.Location.File, o.Location.Start.Line(), o.Location.Start.Column())
		}
	}
	return result
}
