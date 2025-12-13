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
	"wile/syntax"
	"wile/values"
	"slices"
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
	for i, l := range p.literals {
		if l.EqualTo(v) {
			return LiteralIndex(i)
		}
	}
	l := len(p.literals)
	p.literals = append(p.literals, v)
	return LiteralIndex(l)
}

func (p *NativeTemplate) findLiteral(v values.Value) values.Value {
	for _, l := range p.literals {
		if l.EqualTo(v) {
			return l
		}
	}
	return nil
}

func (p *NativeTemplate) internLiteral(v values.Value) values.Value {
	if existing := p.findLiteral(v); existing != nil {
		return existing
	}
	p.literals = append(p.literals, v)
	return v
}

func (p *NativeTemplate) DeduplicateLiteral(v values.Value) values.Value {
	switch val := v.(type) {
	case *values.Symbol:
		return p.internLiteral(val)
	case *values.Integer:
		return p.internLiteral(val)
	case *values.Pair:
		if val == nil || val == values.EmptyList {
			return val
		}
		car := p.DeduplicateLiteral(val.Car())
		cdr := p.DeduplicateLiteral(val.Cdr())
		if car == val.Car() && cdr == val.Cdr() {
			return val
		}
		return values.NewCons(car, cdr)
	case *values.Vector:
		if val == nil || len(*val) == 0 {
			return val
		}
		changed := false
		newElements := make([]values.Value, len(*val))
		for i, elem := range *val {
			deduped := p.DeduplicateLiteral(elem)
			newElements[i] = deduped
			if deduped != elem {
				changed = true
			}
		}
		if !changed {
			return val
		}
		return values.NewVector(newElements...)
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
