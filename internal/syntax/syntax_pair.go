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
	"context"
	"strings"

	"github.com/aalpar/wile/values"
)

var (
	_ values.Value = (*SyntaxPair)(nil)
	_ values.Tuple = (*SyntaxPair)(nil)
	_ SyntaxTuple  = (*SyntaxPair)(nil)

	// SyntaxEmptyList is the empty list sentinel value.
	SyntaxEmptyList = &SyntaxPair{Values: [2]SyntaxValue{}}
)

// SyntaxPair wraps a Scheme pair (cons cell) with source context.
type SyntaxPair struct {
	Values        [2]SyntaxValue
	sourceContext *SourceContext
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
	// Empty list has no symbols to propagate to
	if p.IsEmptyList() {
		return p
	}

	// Recursively add scope to car and cdr (scopes only matter on symbols)
	var newCar, newCdr SyntaxValue
	if p.Values[0] != nil {
		if adder, ok := p.Values[0].(interface{ AddScope(*Scope) SyntaxValue }); ok {
			newCar = adder.AddScope(scope)
		} else {
			newCar = p.Values[0]
		}
	}
	if p.Values[1] != nil {
		if adder, ok := p.Values[1].(interface{ AddScope(*Scope) SyntaxValue }); ok {
			newCdr = adder.AddScope(scope)
		} else {
			newCdr = p.Values[1]
		}
	}

	return &SyntaxPair{
		Values:        [2]SyntaxValue{newCar, newCdr},
		sourceContext: p.sourceContext, // scopes only matter on symbols, not pairs
	}
}

// NewSyntaxEmptyList creates a syntax empty list with the given source context.
func NewSyntaxEmptyList(sctx *SourceContext) *SyntaxPair {
	q := &SyntaxPair{
		Values:        [2]SyntaxValue{nil, nil},
		sourceContext: sctx,
	}
	return q
}

// NewSyntaxCons creates a new syntax pair (cons cell).
func NewSyntaxCons(v0, v1 SyntaxValue, sctx *SourceContext) *SyntaxPair {
	q := &SyntaxPair{
		Values:        [2]SyntaxValue{v0, v1},
		sourceContext: sctx,
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

// SourceContext returns the source context of the pair.
func (p *SyntaxPair) SourceContext() *SourceContext {
	return p.sourceContext
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
	if p.IsEmptyList() {
		return values.EmptyList
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
		panic(values.ErrNotAList)
	}
	if values.IsEmptyList(vs) {
		return p
	}
	if p.IsEmptyList() {
		return vs
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
		panic(values.ErrNotAList)
	}
	vs0, ok := vs.(SyntaxValue)
	if !ok {
		panic(values.ErrNotASyntaxValue)
	}
	q.SetCdr(vs0)
	return p
}

// SyntaxAppend appends a syntax value to the end of the list.
func (p *SyntaxPair) SyntaxAppend(vs SyntaxValue) SyntaxValue {
	if p.IsVoid() {
		panic(values.ErrNotAList)
	}
	if values.IsEmptyList(vs) {
		return p
	}
	if values.IsEmptyList(p) {
		return vs
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
		panic(values.ErrNotAList)
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
		panic(values.ErrNotAList)
	}
	return q
}

// IsEmptyList returns true if the pair represents an empty list.
func (p *SyntaxPair) IsEmptyList() bool {
	if p == nil {
		return false
	}
	return p.Values[0] == nil && p.Values[1] == nil
}

// IsVoid returns true if the pair is nil.
func (p *SyntaxPair) IsVoid() bool {
	return p == nil
}

// ForEach iterates over the elements of the list.
func (p *SyntaxPair) ForEach(ctx context.Context, fn values.ForEachFunc) (values.Value, error) {
	if p == nil {
		return values.Void, nil
	}
	pr := p
	i := 0
	for pr != nil && !pr.IsEmptyList() {
		hasNext := !values.IsEmptyList(pr.Cdr())
		err := fn(ctx, i, hasNext, pr.Car())
		if err != nil {
			return nil, err
		}
		pr0, ok := pr.Cdr().(*SyntaxPair)
		if !ok {
			return pr.Cdr(), nil
		}
		pr = pr0
		i++
	}
	return pr, nil
}

// SyntaxForEach iterates over the syntax elements of the list.
func (p *SyntaxPair) SyntaxForEach(ctx context.Context, fn SyntaxForEachFunc) (SyntaxValue, error) {
	if p == nil {
		return SyntaxVoid, nil
	}
	pr := p
	i := 0
	for pr != nil && !pr.IsEmptyList() {
		hasNext := !IsSyntaxEmptyList(pr.Cdr().(SyntaxValue))
		err := fn(ctx, i, hasNext, pr.Car().(SyntaxValue))
		if err != nil {
			return nil, err
		}
		pr0, ok := pr.Cdr().(*SyntaxPair)
		if !ok {
			return pr.Cdr().(SyntaxValue), nil
		}
		pr = pr0
		i++
	}
	return pr, nil
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
	if p.IsVoid() {
		return "#'<void>"
	}
	if p.IsEmptyList() {
		return "#'()"
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
	if p.IsEmptyList() {
		return values.NewVector()
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
		panic(values.ErrNotAList)
	}
	return values.NewVector(vs...)
}

// AsSyntaxVector converts the list to a syntax vector.
func (p *SyntaxPair) AsSyntaxVector() *SyntaxVector {
	if p.IsVoid() {
		return nil
	}
	if p.IsEmptyList() {
		return NewSyntaxVector(p.sourceContext)
	}
	vs := []SyntaxValue{}
	cdr, _ := p.SyntaxForEach(context.Background(), func(_ context.Context, _ int, _ bool, v SyntaxValue) error {
		vs = append(vs, v)
		return nil
	})
	if !IsSyntaxEmptyList(cdr) {
		panic(values.ErrNotASyntaxList)
	}
	return NewSyntaxVector(p.sourceContext, vs...)
}
