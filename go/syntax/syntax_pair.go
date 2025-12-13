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
	"wile/values"
	"strings"
)

var (
	_ values.Value = (*SyntaxPair)(nil)
	_ values.Tuple = (*SyntaxPair)(nil)
	_ SyntaxTuple  = (*SyntaxPair)(nil)

	// FIXME: consider using types for EmptyList and Void
	SyntaxEmptyList = &SyntaxPair{Values: [2]SyntaxValue{}}
)

type SyntaxPair struct {
	Values        [2]SyntaxValue
	sourceContext *SourceContext
}

// AddScope returns a new SyntaxPair with an additional scope.
//
// This implements recursive scope propagation for Flatt's "sets of scopes" hygiene.
// When a macro expands, the intro scope must be added to ALL identifiers in
// the expansion, including those nested inside pairs.
//
// The method:
//  1. Creates a new SyntaxPair (syntax objects are immutable)
//  2. Recursively calls AddScope on car and cdr elements
//  3. Adds the scope to this pair's SourceContext
//
// This ensures that (let ((tmp x)) body) in a macro expansion gets the intro
// scope on "let", "tmp", "x", and everything in "body", not just the outer pair.
//
// Returns SyntaxValue interface to support recursive scope propagation.
func (p *SyntaxPair) AddScope(scope *Scope) SyntaxValue {
	// Handle empty list case
	if p.IsEmptyList() {
		return &SyntaxPair{
			Values:        [2]SyntaxValue{nil, nil},
			sourceContext: p.sourceContext.WithScope(scope),
		}
	}

	// Recursively add scope to car and cdr
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
		sourceContext: p.sourceContext.WithScope(scope),
	}
}

// Scopes returns the scopes of this syntax pair
func (p *SyntaxPair) Scopes() []*Scope {
	if p.sourceContext == nil {
		return nil
	}
	return p.sourceContext.Scopes
}

func NewSyntaxEmptyList(sctx *SourceContext) *SyntaxPair {
	q := &SyntaxPair{
		Values:        [2]SyntaxValue{nil, nil},
		sourceContext: sctx,
	}
	return q
}

func NewSyntaxCons(v0, v1 SyntaxValue, sctx *SourceContext) *SyntaxPair {
	q := &SyntaxPair{
		Values:        [2]SyntaxValue{v0, v1},
		sourceContext: sctx,
	}
	return q
}

func (p *SyntaxPair) SetSyntaxCar(v SyntaxValue) {
	p.Values[0] = v
}

func (p *SyntaxPair) SetSyntaxCdr(v SyntaxValue) {
	p.Values[1] = v
}

func (p *SyntaxPair) SetCar(v values.Value) {
	p.Values[0] = v.(SyntaxValue)
}

func (p *SyntaxPair) SetCdr(v values.Value) {
	p.Values[1] = v.(SyntaxValue)
}

func (p *SyntaxPair) SyntaxCar() SyntaxValue {
	return p.Values[0]
}

func (p *SyntaxPair) SyntaxCdr() SyntaxValue {
	return p.Values[1]
}

func (p *SyntaxPair) Car() values.Value {
	return p.Values[0]
}

func (p *SyntaxPair) Cdr() values.Value {
	return p.Values[1]
}

func (p *SyntaxPair) SourceContext() *SourceContext {
	return p.sourceContext
}

func (p *SyntaxPair) UnwrapAll() values.Value {
	if p.IsVoid() {
		return values.Void
	}
	if p.IsEmptyList() {
		return values.EmptyList
	}
	// Handle nil car/cdr values defensively
	var car, cdr values.Value
	if p.Values[0] != nil {
		car = p.Values[0].UnwrapAll()
	}
	if p.Values[1] != nil {
		cdr = p.Values[1].UnwrapAll()
	} else {
		cdr = values.EmptyList
	}
	q := values.NewCons(car, cdr)
	return q
}

func (p *SyntaxPair) Unwrap() values.Value {
	if p.IsVoid() {
		return values.Void
	}
	if p.IsEmptyList() {
		return values.EmptyList
	}
	return values.NewCons(p.Car(), p.Cdr())
}

func (p *SyntaxPair) IsList() bool {
	pr := p
	if values.IsVoid(pr) {
		return false
	}
	v, _ := p.SyntaxForEach(context.Background(), func(ctx context.Context, i int, hasNext bool, v SyntaxValue) error {
		return nil
	})
	return values.IsEmptyList(v)
}

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

func (p *SyntaxPair) Length() int {
	q := 0
	r, _ := p.SyntaxForEach(context.Background(), func(ctx context.Context, i int, hasNext bool, v SyntaxValue) error {
		q = i + 1
		return nil
	})
	if !IsSyntaxEmptyList(r) {
		panic(values.ErrNotAList)
	}
	return q
}

func (p *SyntaxPair) IsEmptyList() bool {
	if p == nil {
		return false
	}
	return p.Values[0] == nil && p.Values[1] == nil
}

func (p *SyntaxPair) IsVoid() bool {
	return p == nil
}

func (p *SyntaxPair) ForEach(ctx context.Context, fn values.ForEachFunc) (values.Value, error) {
	if p == nil {
		return values.Void, nil
	}
	ok := false
	pr := p
	pr0 := p
	i := 0
	for pr != nil && !pr.IsEmptyList() {
		hasNext := !values.IsEmptyList(pr.Cdr())
		err := fn(ctx, i, hasNext, pr.Car())
		if err != nil {
			return nil, err
		}
		pr0, ok = pr.Cdr().(*SyntaxPair)
		if !ok {
			return pr.Cdr(), nil
		}
		pr = pr0
		i++
	}
	return pr, nil
}

func (p *SyntaxPair) SyntaxForEach(ctx context.Context, fn SyntaxForEachFunc) (SyntaxValue, error) {
	if p == nil {
		return SyntaxVoid, nil
	}
	ok := false
	pr := p
	pr0 := p
	i := 0
	for pr != nil && !pr.IsEmptyList() {
		hasNext := !IsSyntaxEmptyList(pr.Cdr().(SyntaxValue))
		err := fn(ctx, i, hasNext, pr.Car().(SyntaxValue))
		if err != nil {
			return nil, err
		}
		pr0, ok = pr.Cdr().(*SyntaxPair)
		if !ok {
			return pr.Cdr().(SyntaxValue), nil
		}
		pr = pr0
		i++
	}
	return pr, nil
}

func (p *SyntaxPair) IsPair() bool {
	return true
}

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
	cdr, _ := p.SyntaxForEach(context.Background(), func(ctx context.Context, i int, hasNext bool, v SyntaxValue) error {
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
	cdr, err := p.SyntaxForEach(context.Background(), func(ctx context.Context, i int, hasNext bool, v SyntaxValue) error {
		v1, ok := v.(SyntaxValue)
		if !ok {
			return values.ErrNotASyntaxValue
		}
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

func (p *SyntaxPair) AsSyntaxVector() *SyntaxVector {
	if p.IsVoid() {
		return nil
	}
	if p.IsEmptyList() {
		return NewSyntaxVector(p.sourceContext)
	}
	vs := []SyntaxValue{}
	cdr, _ := p.SyntaxForEach(context.Background(), func(ctx context.Context, i int, hasNext bool, v SyntaxValue) error {
		vs = append(vs, v)
		return nil
	})
	if !IsSyntaxEmptyList(cdr) {
		panic(values.ErrNotASyntaxList)
	}
	return NewSyntaxVector(p.sourceContext, vs...)
}
