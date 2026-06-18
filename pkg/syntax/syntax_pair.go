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
	"context"
	"strings"

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

var (
	_ values.Value = (*SyntaxPair)(nil)
	_ values.Tuple = (*SyntaxPair)(nil)
	_ SyntaxTuple  = (*SyntaxPair)(nil)

	// SyntaxEmptyList is the empty list singleton at the syntax phase.
	// It is the same singleton as values.EmptyList — the empty list has no
	// symbols, scopes, or source-attachable hygiene content, so the
	// value-level singleton serves both phases (matching Chez's
	// `(equal? (syntax ()) '()) → #t`).
	SyntaxEmptyList SyntaxTuple = values.SyntaxEmptyList
)

// SyntaxPair wraps a Scheme pair (cons cell) with source context.
type SyntaxPair struct {
	Values [2]SyntaxValue
	syntaxBase
}

// AddScope recursively propagates a scope to all nested symbols.
//
// This implements scope propagation for Flatt's "sets of scopes" hygiene.
// When a macro expands, the intro scope must be added to all identifiers
// (symbols) in the expansion. This method walks the pair structure and
// calls AddScope on each element, ultimately reaching the symbols.
//
// Only symbols store scopes for hygiene resolution. Pairs just propagate.
func (p *SyntaxPair) AddScope(scope *Scope) SyntaxValue {
	// Use the generic mapSyntaxTree traversal to add scope to all nested nodes.
	// mapSyntaxTree handles pair recursion internally and calls the function only on leaf nodes (symbols, etc.).
	return mapSyntaxTree(p, func(node SyntaxValue) SyntaxValue {
		// Try to call AddScope on nodes that support it (symbols, etc.)
		adder, ok := node.(interface{ AddScope(*Scope) SyntaxValue })
		if ok {
			return adder.AddScope(scope)
		}
		// Nodes that don't support AddScope are returned unchanged
		return node
	})
}

// NewSyntaxCons creates a new syntax pair (cons cell).
func NewSyntaxCons(v0, v1 SyntaxValue, sctx *SourceContext) *SyntaxPair {
	q := &SyntaxPair{
		Values:     [2]SyntaxValue{v0, v1},
		syntaxBase: values.NewSyntaxBase(sctx),
	}
	return q
}

// SetSyntaxCar sets the car of the pair to a syntax value.
func (p *SyntaxPair) SetSyntaxCar(v SyntaxValue) {
	p.Values[0] = v
}

// SetSyntaxCdr sets the cdr of the pair to a syntax value.
func (p *SyntaxPair) SetSyntaxCdr(v SyntaxValue) {
	p.Values[1] = v
}

// SetCar sets the car of the pair.
func (p *SyntaxPair) SetCar(v values.Value) {
	p.Values[0] = v.(SyntaxValue)
}

// SetCdr sets the cdr of the pair.
func (p *SyntaxPair) SetCdr(v values.Value) {
	p.Values[1] = v.(SyntaxValue)
}

// SyntaxCar returns the car as a syntax value.
func (p *SyntaxPair) SyntaxCar() SyntaxValue {
	return p.Values[0]
}

// SyntaxCdr returns the cdr as a syntax value.
func (p *SyntaxPair) SyntaxCdr() SyntaxValue {
	return p.Values[1]
}

// Car returns the car of the pair.
func (p *SyntaxPair) Car() values.Value {
	return p.Values[0]
}

// Cdr returns the cdr of the pair.
func (p *SyntaxPair) Cdr() values.Value {
	return p.Values[1]
}

// UnwrapAll recursively unwraps the pair and returns a regular Scheme pair.
func (p *SyntaxPair) UnwrapAll() values.Value {
	return UnwrapAllShared(p, make(map[SyntaxValue]values.Value))
}

// Unwrap returns a regular Scheme pair without recursively unwrapping.
func (p *SyntaxPair) Unwrap() values.Value {
	if p.IsVoid() {
		return values.Void
	}
	return values.NewCons(p.Car(), p.Cdr())
}

// IsList returns true if the pair forms a proper list.
func (p *SyntaxPair) IsList() bool {
	pr := p
	if values.IsVoid(pr) {
		return false
	}
	v, _ := p.SyntaxForEach(context.Background(), func(_ context.Context, _ int, _ bool, _ SyntaxValue) error {
		return nil
	})
	return values.IsEmptyList(v)
}

// Append appends a value to the end of the list.
func (p *SyntaxPair) Append(vs values.Value) values.Value {
	if p.IsVoid() {
		panic(werr.WrapForeignErrorf(werr.ErrNotAList, "SyntaxPair.Append: receiver is void"))
	}
	if values.IsEmptyList(vs) {
		return p
	}
	q := p
	for !values.IsVoid(q) && !values.IsEmptyList(q.Cdr()) {
		ok := false
		q, ok = q.Cdr().(*SyntaxPair)
		if !ok {
			break
		}
	}
	if q.IsVoid() {
		panic(werr.WrapForeignErrorf(werr.ErrNotAList, "SyntaxPair.Append: traversal reached void"))
	}
	vs0, ok := vs.(SyntaxValue)
	if !ok {
		panic(werr.WrapForeignErrorf(werr.ErrNotASyntaxValue, "SyntaxPair.Append: value is not a SyntaxValue"))
	}
	q.SetCdr(vs0)
	return p
}

// SyntaxAppend appends a syntax value to the end of the list.
func (p *SyntaxPair) SyntaxAppend(vs SyntaxValue) SyntaxValue {
	if p.IsVoid() {
		panic(werr.WrapForeignErrorf(werr.ErrNotAList, "SyntaxPair.SyntaxAppend: receiver is void"))
	}
	if values.IsEmptyList(vs) {
		return p
	}
	q := p
	for !values.IsVoid(q) && !values.IsEmptyList(q.Cdr()) {
		ok := false
		q, ok = q.Cdr().(*SyntaxPair)
		if !ok {
			break
		}
	}
	if q.IsVoid() {
		panic(werr.WrapForeignErrorf(werr.ErrNotAList, "SyntaxPair.SyntaxAppend: traversal reached void"))
	}
	q.SetCdr(vs)
	return p
}

// Len returns the length of the list.
func (p *SyntaxPair) Length() int {
	q := 0
	r, _ := p.SyntaxForEach(context.Background(), func(_ context.Context, i int, _ bool, _ SyntaxValue) error {
		q = i + 1
		return nil
	})
	if !IsSyntaxEmptyList(r) {
		panic(werr.WrapForeignErrorf(werr.ErrNotAList, "SyntaxPair.Length: improper list"))
	}
	return q
}

// IsEmptyList returns false. A *SyntaxPair is never the empty list;
// SyntaxEmptyList (an alias for the values.EmptyList singleton) is
// the only representation of the empty list at the syntax phase.
func (*SyntaxPair) IsEmptyList() bool {
	return false
}

// IsVoid returns true if the pair is nil.
func (p *SyntaxPair) IsVoid() bool {
	return p == nil
}

// walkSyntaxSpine is the shared spine-walk consumed by ForEach and
// SyntaxForEach. It invokes fn for each cell's car as a SyntaxValue
// and returns the trailing cdr (values.EmptyList for proper lists,
// the improper tail otherwise) as a raw values.Value — SyntaxForEach
// casts that to SyntaxValue at the boundary.
//
// Stays open-coded rather than consuming an iter.Seq2-shaped iterator:
// the equivalent rewrite on values.Pair.ForEach regressed ~40-56% on
// BenchmarkPairForEach (values/pair_bench_test.go) because each yield
// goes through two function pointers. The macro-expansion path here
// is hot enough that the same overhead matters.
//
// The cdr.(SyntaxValue) assertion is deliberate: at the syntax phase
// every cdr is invariantly a SyntaxValue (enforced by NewSyntaxCons +
// SetSyntaxCdr), so this is a no-op cost on valid input. On misuse
// (e.g. NewSyntaxCons(nil, nil, nil)) the assertion panics with
// "interface conversion: interface is nil, not values.SyntaxValue" —
// the sharper diagnostic pinned by TestSyntaxPair_*_NilNilPair.
func (p *SyntaxPair) walkSyntaxSpine(ctx context.Context, fn SyntaxForEachFunc) (values.Value, error) {
	if p == nil {
		return values.EmptyList, nil
	}
	pr := p
	i := 0
	for pr != nil {
		cdr := pr.Cdr()
		hasNext := !values.IsEmptyList(cdr.(SyntaxValue))
		err := fn(ctx, i, hasNext, pr.SyntaxCar())
		if err != nil {
			return nil, err
		}
		pr0, ok := cdr.(*SyntaxPair)
		if !ok {
			return cdr, nil
		}
		pr = pr0
		i++
	}
	return values.EmptyList, nil
}

// ForEach iterates over the elements of the list.
func (p *SyntaxPair) ForEach(ctx context.Context, fn values.ForEachFunc) (values.Value, error) {
	return p.walkSyntaxSpine(ctx, func(ctx context.Context, i int, hasNext bool, v SyntaxValue) error {
		return fn(ctx, i, hasNext, v)
	})
}

// SyntaxForEach iterates over the syntax elements of the list.
func (p *SyntaxPair) SyntaxForEach(ctx context.Context, fn SyntaxForEachFunc) (SyntaxValue, error) {
	tail, err := p.walkSyntaxSpine(ctx, fn)
	if err != nil {
		return nil, err
	}
	return tail.(SyntaxValue), nil
}

// IsPair returns true; SyntaxPair is always a pair.
func (p *SyntaxPair) IsPair() bool {
	return true
}

// SchemeString returns a string representation of the syntax pair.
func (p *SyntaxPair) SchemeString() string {
	if p == nil {
		return "#<syntax-void>"
	}
	q := &strings.Builder{}
	q.WriteString("#'(")
	cdr, _ := p.SyntaxForEach(context.Background(), func(_ context.Context, i int, _ bool, v SyntaxValue) error {
		if i > 0 {
			q.WriteString(" ")
		}
		q.WriteString(v.SchemeString())
		return nil
	})
	if !IsSyntaxEmptyList(cdr) {
		q.WriteString(" . ")
		q.WriteString(cdr.SchemeString())
	}
	q.WriteString(")")
	return q.String()
}

// EqualTo performs pointer comparison only, matching Chez Scheme/Racket behavior.
// Two syntax objects are equal? only if they are the same object.
// For value comparison of syntax objects, use bound-identifier=? or free-identifier=?.
func (p *SyntaxPair) EqualTo(o values.Value) bool {
	v, ok := o.(*SyntaxPair)
	if !ok {
		return false
	}
	return p == v
}

// AsVector converts the SyntaxPair (assumed to be a proper list) into a Vector of unwrapped values.
func (p *SyntaxPair) AsVector() *values.Vector {
	if p.IsVoid() {
		return nil
	}
	vs := []values.Value{}
	cdr, err := p.SyntaxForEach(context.Background(), func(_ context.Context, _ int, _ bool, v SyntaxValue) error {
		v1 := v
		vs = append(vs, v1.UnwrapAll())
		return nil
	})
	if err != nil {
		panic(err)
	}
	if !values.IsEmptyList(cdr) {
		panic(werr.WrapForeignErrorf(werr.ErrNotAList, "SyntaxPair.AsVector: improper list"))
	}
	return values.NewVector(vs...)
}

// AsSyntaxVector converts the list to a syntax vector.
func (p *SyntaxPair) AsSyntaxVector() *SyntaxVector {
	if p.IsVoid() {
		return nil
	}
	vs := []SyntaxValue{}
	cdr, _ := p.SyntaxForEach(context.Background(), func(_ context.Context, _ int, _ bool, v SyntaxValue) error {
		vs = append(vs, v)
		return nil
	})
	if !IsSyntaxEmptyList(cdr) {
		panic(werr.WrapForeignErrorf(werr.ErrNotASyntaxList, "SyntaxPair.AsSyntaxVector: improper list"))
	}
	return NewSyntaxVector(p.SourceContext(), vs...)
}
