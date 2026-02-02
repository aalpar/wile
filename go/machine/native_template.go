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

package machine

import (
	"math"
	"slices"

	"github.com/aalpar/wile/go/environment"
	"github.com/aalpar/wile/go/syntax"
	"github.com/aalpar/wile/go/values"
)

type LiteralIndex int

type NativeTemplate struct {
	parameterCount int
	valueCount     int
	isVariadic     bool
	literals       MultipleValues
	operations     Operations
	sourceMap      *SourceMap // PC → source location mapping
	name           string     // Function name (for stack traces)
}

func NewNativeTemplate(pcnt int, vcnt int, vd bool, operations ...Operation) *NativeTemplate {
	q := &NativeTemplate{
		parameterCount: pcnt,
		valueCount:     vcnt,
		isVariadic:     vd,
		operations:     operations,
		sourceMap:      NewSourceMap(),
	}
	return q
}

func (p *NativeTemplate) ParameterCount() int {
	return p.parameterCount
}

func (p *NativeTemplate) ValueCount() int {
	return p.valueCount
}

func (p *NativeTemplate) IsVariadic() bool {
	return p.isVariadic
}

func (p *NativeTemplate) Operations() Operations {
	return p.operations
}

func (p *NativeTemplate) SourceMap() *SourceMap {
	return p.sourceMap
}

func (p *NativeTemplate) SourceAt(pc int) *syntax.SourceContext {
	if p.sourceMap == nil {
		return nil
	}
	return p.sourceMap.Lookup(pc)
}

func (p *NativeTemplate) Name() string {
	return p.name
}

func (p *NativeTemplate) SetName(name string) {
	p.name = name
}

func (p *NativeTemplate) MaybeAppendLiteral(v values.Value) LiteralIndex {
	// Don't deduplicate environments - each closure needs its own instance
	// because environments are mutable and context-dependent. The parent
	// chain is set at runtime by OperationMakeClosure, so structural
	// equality doesn't capture lexical context.
	_, isEnv := v.(*environment.EnvironmentFrame)
	if !isEnv {
		for i, l := range p.literals {
			if literalIdentical(l, v) {
				return LiteralIndex(i)
			}
		}
	}
	l := len(p.literals)
	p.literals = append(p.literals, v)
	return LiteralIndex(l)
}

// literalIdentical returns true if two values are identical for literal
// deduplication purposes. This is stricter than EqualTo for floating point
// values: -0.0 and +0.0 are numerically equal but have different bit
// representations and must be kept as separate literals to preserve IEEE 754
// signed zero semantics in operations like atan2.
func literalIdentical(a, b values.Value) bool {
	// For floats, check both numeric equality and sign bit equality
	if af, ok := a.(*values.Float); ok {
		if bf, ok := b.(*values.Float); ok {
			return af.Value == bf.Value && math.Signbit(af.Value) == math.Signbit(bf.Value)
		}
		return false
	}
	// For all other types, use standard EqualTo
	return a.EqualTo(b)
}

func (p *NativeTemplate) findLiteral(v values.Value) values.Value {
	for _, l := range p.literals {
		if l.EqualTo(v) {
			return l
		}
	}
	return nil
}

func (p *NativeTemplate) deduplicateLiteral(v values.Value) values.Value {
	existing := p.findLiteral(v)
	if existing != nil {
		return existing
	}
	p.literals = append(p.literals, v)
	return v
}

// deduplicateVector deduplicates all elements in the given vector
// using the template's literal pool.
// Returns a new vector if any elements were changed, or the original vector otherwise.
// TODO: optimize for the common case where no elements change.  consider in place modification?
func (p *NativeTemplate) deduplicateVector(v *values.Vector) *values.Vector {
	if v == nil || len(*v) == 0 {
		return v
	}
	changed := false
	newElements := values.NewVectorWithLength(v.Length())
	for i, elem := range *v {
		deduped := p.DeduplicateLiteral(elem)
		(*newElements)[i] = deduped
		if deduped != elem {
			changed = true
		}
	}
	// No changes, return original vector
	// this avoids unnecessary pointer changes in the caller
	if !changed {
		return v
	}
	return newElements
}

// deduplicatePair deduplicates all elements in the given pair
// using the template's literal pool.
// Returns a new pair if any elements were changed, or the original pair otherwise.
// TODO: optimize for the common case where no elements change.  consider in place modification?
func (p *NativeTemplate) deduplicatePair(v *values.Pair) *values.Pair {
	if v == nil || v == values.EmptyList {
		return v
	}
	car := p.DeduplicateLiteral(v.Car())
	cdr := p.DeduplicateLiteral(v.Cdr())
	// No changes, return original pair
	// this avoids unnecessary pointer changes in the caller
	if car == v.Car() && cdr == v.Cdr() {
		return v
	}
	return values.NewCons(car, cdr)
}

// DeduplicateLiteral deduplicates the given value using the template's literal pool.
// For composite values (pairs and vectors), all elements are deduplicated recursively.
// Returns the deduplicated value.
func (p *NativeTemplate) DeduplicateLiteral(v values.Value) values.Value {
	switch val := v.(type) {
	case *values.Symbol, *values.Integer:
		return p.deduplicateLiteral(val)
	case *values.Pair:
		return p.deduplicatePair(val)
	case *values.Vector:
		return p.deduplicateVector(val)
	default:
		return v
	}
}

func (p *NativeTemplate) AppendOperations(ops ...Operation) {
	p.operations = append(p.operations, ops...)
}

func (p *NativeTemplate) SchemeString() string {
	return "#<native-template>"
}

func (p *NativeTemplate) IsVoid() bool {
	return p == nil
}

func (p *NativeTemplate) EqualTo(o values.Value) bool {
	v, ok := o.(*NativeTemplate)
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return p == v
	}
	if p.parameterCount != v.parameterCount {
		return false
	}
	if p.valueCount != v.valueCount {
		return false
	}
	if p.isVariadic != v.isVariadic {
		return false
	}
	if len(p.literals) != len(v.literals) {
		return false
	}
	for i, l := range p.literals {
		if !l.EqualTo(v.literals[i]) {
			return false
		}
	}
	if len(p.operations) != len(v.operations) {
		return false
	}
	for i := range p.operations {
		op0, ok0 := p.operations[i].(values.Value)
		op1, ok1 := v.operations[i].(values.Value)
		if !ok0 || !ok1 {
			return false
		}
		if !op0.EqualTo(op1) {
			return false
		}
	}
	return true
}

func (p *NativeTemplate) Copy() *NativeTemplate {
	if p == nil {
		return nil
	}
	q := &NativeTemplate{
		parameterCount: p.parameterCount,
		valueCount:     p.valueCount,
		isVariadic:     p.isVariadic,
		name:           p.name,
	}
	q.literals = slices.Clone(p.literals)
	q.operations = slices.Clone(p.operations)
	if p.sourceMap != nil {
		q.sourceMap = &SourceMap{entries: slices.Clone(p.sourceMap.entries)}
	}
	return q
}
