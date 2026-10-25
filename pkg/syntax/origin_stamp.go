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
	"slices"

	"github.com/aalpar/wile/pkg/values"
)

// PassThrough is the set of nodes a transformer's output shares with its
// input: syntax the user wrote, handed back unchanged. FlipScopeStampOrigin
// leaves these out of the origin stamp, because an error on a form the user
// wrote must not name the macro that merely carried it.
//
// Every node is recorded by identity, symbols included. The introduction scope
// cannot stand in for the symbols: it marks input symbols, but a symbol a
// transformer builds in the keyword's context (datum->syntax on the keyword,
// a quoted literal in an ER macro) carries it too, and is the macro's. Keyed
// by node pointer rather than by
// SyntaxValue interface: hashing an interface key costs a type-word compare and
// a dynamic hash per node, and the set is built on every expansion.
type PassThrough struct {
	pairs nodeSet[*SyntaxPair]
	objs  nodeSet[*SyntaxObject]
	syms  nodeSet[*SyntaxSymbol]
	other nodeSet[SyntaxValue]
}

// smallSetMax is how many members a nodeSet scans linearly before it moves to a
// map. An expansion's input is a handful of nodes; a slice scan of that size is
// cheaper than hashing and allocates nothing beyond the slice itself.
const smallSetMax = 24

// nodeSet is a set of node pointers: a slice while small, a map once it grows.
type nodeSet[K comparable] struct {
	small []K
	big   map[K]struct{}
}

func (p *nodeSet[K]) has(k K) bool {
	if p.big != nil {
		_, ok := p.big[k]
		return ok
	}
	return slices.Contains(p.small, k)
}

// add inserts k and reports whether it was already a member.
func (p *nodeSet[K]) add(k K) bool {
	if p.has(k) {
		return true
	}
	if p.big != nil {
		p.big[k] = struct{}{}
		return false
	}
	if len(p.small) < smallSetMax {
		p.small = append(p.small, k)
		return false
	}
	p.big = make(map[K]struct{}, 2*smallSetMax)
	for _, m := range p.small {
		p.big[m] = struct{}{}
	}
	p.big[k] = struct{}{}
	p.small = nil
	return false
}

// NewPassThrough records every pair, symbol, literal, vector and box of input. A nil
// input yields an empty set.
func NewPassThrough(input SyntaxValue) *PassThrough {
	q := &PassThrough{}
	q.Add(input)
	return q
}

// Add records stx and everything beneath it. The recorded containers double as
// the visited set, so a shared or cyclic subgraph is entered once.
func (p *PassThrough) Add(stx SyntaxValue) {
	switch s := stx.(type) {
	case *SyntaxPair:
		if IsSyntaxEmptyList(s) {
			return
		}
		if p.pairs.add(s) {
			return
		}
		p.Add(s.Values[0])
		p.Add(s.Values[1])
	case *SyntaxObject:
		p.objs.add(s)
	case *SyntaxSymbol:
		p.syms.add(s)
	case *SyntaxVector:
		if p.other.add(s) {
			return
		}
		for _, elem := range s.Values {
			p.Add(elem)
		}
	case *SyntaxBox:
		if p.other.add(s) {
			return
		}
		p.Add(s.Value)
	}
}

// stampMemoMax bounds originStamp's context memo. A syntax-rules expansion
// reuses a few contexts (the use site's, mostly); a syntax-case template carries
// one per node, where a memo never hits, so it stops growing rather than turn a
// lookup per node into a scan of the whole template.
const stampMemoMax = 8

// originStamp is one FlipScopeStampOrigin walk. in and out memoize the stamped
// clone of each SourceContext seen: cloning one per node is most of what a
// naive stamp costs.
type originStamp struct {
	scope  *Scope
	origin *OriginInfo
	pass   *PassThrough
	in     [stampMemoMax]*SourceContext
	out    [stampMemoMax]*SourceContext
	n      int
}

func (p *originStamp) stamp(c *SourceContext) *SourceContext {
	for i := range p.n {
		if p.in[i] == c {
			return p.out[i]
		}
	}
	q := c.WithOrigin(p.origin)
	if p.n < stampMemoMax {
		p.in[p.n] = c
		p.out[p.n] = q
		p.n++
	}
	return q
}

// FlipScopeStampOrigin flips scope over stx, as FlipScope does, and records
// origin on every node the transformer constructed, so an error raised on any
// node of the expansion can name the macro that produced it (the error is
// stamped on the offending node and renders its Source.Origin). It is the
// expander's one pass over a transformer's output, and a separate walk from
// MapSyntaxTree on purpose: MapSyntaxTree never calls its function on a pair
// and returns unchanged nodes by identity, while the stamp has to reach the
// pair (the offending node for a malformed binding is the binding pair).
//
// A node of pass is the user's own syntax and is only flipped, never stamped.
// That includes a symbol: only the input's own symbol objects are the user's, so
// a symbol the transformer built, in the keyword's context or not, is blamed on
// the macro.
//
// WithOrigin replaces the context's Origin rather than appending, which is
// correct because the caller builds the chain: origin.Parent is the origin the
// use site already carried. A nil origin or scope degrades to FlipScope and to
// a bare stamp respectively; the empty list passes through.
func FlipScopeStampOrigin(stx SyntaxValue, scope *Scope, origin *OriginInfo, pass *PassThrough) SyntaxValue {
	if origin == nil {
		return FlipScope(stx, scope)
	}
	if stx == nil {
		return nil
	}
	if pass == nil {
		pass = NewPassThrough(nil)
	}
	p := &originStamp{scope: scope, origin: origin, pass: pass}
	return p.walk(stx)
}

// walk rebuilds stx.
func (p *originStamp) walk(stx SyntaxValue) SyntaxValue {
	switch s := stx.(type) {
	case *SyntaxPair:
		if IsSyntaxEmptyList(s) {
			return s
		}
		if p.pass.pairs.has(s) {
			return FlipScope(s, p.scope)
		}
		var car, cdr SyntaxValue
		if s.Values[0] != nil {
			car = p.walk(s.Values[0])
		}
		if s.Values[1] != nil {
			cdr = p.walk(s.Values[1])
		}
		return NewSyntaxCons(car, cdr, p.stamp(s.SourceContext()))

	case *SyntaxSymbol:
		sctx := s.SourceContext()
		c := sctx.Clone()
		if c == nil {
			c = &SourceContext{}
		}
		user := false
		// Only a symbol carrying the scope can be an input one, so the set is
		// consulted for those alone.
		if p.scope != nil && c.Scopes.Has(p.scope) && p.pass.syms.has(s) {
			user = true
		}
		if !user {
			c.Origin = p.origin
		}
		if p.scope != nil {
			c.Scopes = FlipScopeInSet(c.Scopes, p.scope)
		}
		return &SyntaxSymbol{
			Sym:             s.Sym,
			syntaxBase:      values.NewSyntaxBase(c),
			ResolvedBinding: s.ResolvedBinding,
		}

	case *SyntaxObject:
		if p.pass.objs.has(s) {
			return s
		}
		return &SyntaxObject{
			datum:      s.datum,
			syntaxBase: values.NewSyntaxBase(p.stamp(s.SourceContext())),
		}

	case *SyntaxVector:
		if p.pass.other.has(s) {
			return FlipScope(s, p.scope)
		}
		elems := make([]SyntaxValue, len(s.Values))
		for i, elem := range s.Values {
			if elem != nil {
				elems[i] = p.walk(elem)
			}
		}
		return NewSyntaxVector(p.stamp(s.SourceContext()), elems...)

	case *SyntaxBox:
		if p.pass.other.has(s) {
			return FlipScope(s, p.scope)
		}
		return NewSyntaxBox(p.walk(s.Value), p.stamp(s.SourceContext()))

	default:
		return stx
	}
}
