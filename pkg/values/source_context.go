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

package values

import (
	"fmt"
	"strings"
)

var _ Value = (*SourceContext)(nil)

// OriginInfo tracks macro expansion chains for debugging and error reporting.
// Each OriginInfo represents one macro expansion in the chain, enabling:
//   - Tracing generated code back to the macro that created it
//   - Locating where the macro was invoked
type OriginInfo struct {
	Identifier string         // Macro name that caused expansion (e.g., "let", "my-macro")
	Location   *SourceContext // Where the macro was invoked (use-site)
	Parent     *OriginInfo    // Previous link in origin chain (for nested macros)
}

// Depth returns the length of the origin chain.
func (p *OriginInfo) Depth() int {
	depth := 0
	for curr := p; curr != nil; curr = curr.Parent {
		depth++
	}
	return depth
}

// SourceContext holds source location and hygiene information for a syntax object.
type SourceContext struct {
	Text   string
	File   string
	Start  SourceIndexes
	End    SourceIndexes
	Scopes Scopes      // Scopes associated with this source location
	Origin *OriginInfo // Macro expansion origin chain (nil if not from macro)
}

// NewSourceContext creates a new source context with the given location info.
func NewSourceContext(text, file string, start, end SourceIndexes) *SourceContext {
	q := &SourceContext{
		Text:  text,
		File:  file,
		Start: start,
		End:   end,
	}
	return q
}

// NewZeroValueSourceContext creates an empty source context.
func NewZeroValueSourceContext() *SourceContext {
	q := &SourceContext{}
	return q
}

// Clone returns a shallow copy of the SourceContext.
// The Scopes slice and Origin pointer are shared with the original;
// callers that need to mutate those fields should assign new values
// after cloning (which is exactly what the With* methods do).
func (p *SourceContext) Clone() *SourceContext {
	if p == nil {
		return nil
	}
	c := *p
	return &c
}

// Location returns the source location formatted as "file:line:col".
// Returns empty string if the receiver is nil or carries no location at all.
//
// When File is empty (e.g. a nameless EvalMultiple program) but a position is
// present, the ":line:col" form is still returned so provenance is not lost.
// A truly position-less context (File=="" and Line==0) yields "", which is what
// lets machine.StackFrame.String fall through to the call site, or to the bare
// frame name, instead of printing ":0:0".
func (p *SourceContext) Location() string {
	if p == nil {
		return ""
	}
	if p.File != "" {
		return fmt.Sprintf("%s:%d:%d", p.File, p.Start.Line(), p.Start.Column())
	}
	if p.Start.Line() > 0 {
		return fmt.Sprintf(":%d:%d", p.Start.Line(), p.Start.Column())
	}
	return ""
}

// SchemeString returns the Scheme representation of the source context.
func (p *SourceContext) SchemeString() string {
	return fmt.Sprintf("<source-context %s:%s-%s>", p.File, p.Start, p.End)
}

// IsVoid returns true if the source context is nil.
func (p *SourceContext) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if this source context equals the given value.
func (p *SourceContext) EqualTo(value Value) bool {
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
	if p.Start != v.Start {
		return false
	}
	if p.End != v.End {
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
	c := p.Clone()
	c.Origin = origin
	return c
}

// WithoutScopes returns a new SourceContext with scopes cleared. Template
// expansion does not use it: applyHygieneToSymbol substitutes a template
// identifier's definition-site scope set instead of clearing it.
func (p *SourceContext) WithoutScopes() *SourceContext {
	if p == nil {
		return nil
	}
	c := p.Clone()
	c.Scopes = Scopes{}
	return c
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
// The set is canonically ordered by scope id, so "prepend" is no longer a
// property of this function: Scopes.Add places the scope by id and shares the
// rest of the chain. A freshly minted scope is the new maximum, which is the
// case every production add takes, so the common path is one 24-byte cell.
//
// This used to allocate len(p.Scopes)+1 and copy, which is the O(n) factor that
// made compiling n nested lexical forms O(n^3) in total.
//
// Returns a NEW SourceContext (immutable design for syntax objects).
func (p *SourceContext) WithScope(scope *Scope) *SourceContext {
	if p == nil {
		// Scopes{}.Add rather than ScopesFromSlice([]*Scope{scope}): the literal was
		// the last reducible raw slice operation in non-test code, and a one-member
		// set does not need a slice to say so.
		return &SourceContext{
			Scopes: Scopes{}.Add(scope),
		}
	}
	grown := p.Scopes.Add(scope)
	if grown == p.Scopes {
		return p
	}
	c := p.Clone()
	c.Scopes = grown
	return c
}

// WithoutScope returns a new SourceContext with scope removed, or the receiver
// unchanged when the scope is absent. The inverse of WithScope.
//
// Removal is a real operation in Flatt's model and not a repair: a use-site
// scope is added to a macro's input and then stripped again at binder positions
// whose binding is visible outside the expansion, which is what keeps a
// macro-generated define reachable from code that never saw the macro use.
// Racket calls the same operation remove-scopes and drives it from a registry of
// scopes it minted, never from a property of the scope itself.
func (p *SourceContext) WithoutScope(scope *Scope) *SourceContext {
	if p == nil {
		return nil
	}
	shrunk := p.Scopes.Remove(scope)
	if shrunk == p.Scopes {
		return p
	}
	c := p.Clone()
	c.Scopes = shrunk
	return c
}

// WithScopes returns a new SourceContext with additional scopes.
//
// It has zero non-test callers and is the only function that could ever build a
// duplicate-containing set, because it concatenated without dedup. Under Scopes
// it cannot: Add is idempotent per member. Kept for now so the flip changes one
// thing at a time; [I135-freefn] retires it.
func (p *SourceContext) WithScopes(scopes Scopes) *SourceContext {
	if p == nil {
		return &SourceContext{Scopes: scopes}
	}
	grown := p.Scopes
	scopes.ForEach(func(s *Scope) {
		grown = grown.Add(s)
	})
	if grown == p.Scopes {
		return p
	}
	c := p.Clone()
	c.Scopes = grown
	return c
}

// FormatOriginChain returns a formatted string showing the macro expansion chain.
// maxDepth limits how many expansions to show (0 = unlimited).
func FormatOriginChain(origin *OriginInfo, maxDepth int) string {
	if origin == nil {
		return ""
	}
	var result strings.Builder
	depth := 0
	for o := origin; o != nil; o = o.Parent {
		depth++
		if maxDepth > 0 && depth > maxDepth {
			remaining := origin.Depth() - maxDepth
			fmt.Fprintf(&result, "\n  ... and %d more expansion(s)", remaining)
			break
		}
		fmt.Fprintf(&result, "\n  expanded from '%s'", o.Identifier)
		loc := o.Location.Location()
		if loc != "" {
			fmt.Fprintf(&result, " at %s", loc)
		}
	}
	return result.String()
}
