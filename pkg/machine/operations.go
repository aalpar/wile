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

package machine

import (
	"github.com/aalpar/wile/pkg/values"
)

// Operations is the compiler's opcode list. It is deliberately NOT a
// values.Value.
//
// It carried SchemeString/IsVoid/EqualTo for container convenience, which made
// the naked []Operation slice a legal dynamic type inside a values.Value
// interface — and a slice is not Go-comparable, so any `==` or map-key hash of
// that interface faults. values.EqIdentity (eq?) is exactly such an `==`. The
// conformance was never used: nothing outside this package treated an Operations
// as a Scheme datum, and its SchemeString rendered the non-datum
// "#<machine-operations>". Meanwhile it cost the equality core real
// defensiveness, since values.Equal could not assume its operands were
// comparable.
//
// Having an equality method is not the same as being a Value. EqualTo below
// takes a concrete Operations, so callers still get structural comparison
// without the type entering the Value contract.
type Operations []Operation

func (p Operations) AsList() values.Tuple {
	if len(p) == 0 {
		return nil // or return EmptyList depending on your API
	}

	vs := make([]values.Value, len(p))
	for i, op := range p {
		vs[i] = op
	}
	return values.List(vs...)
}

func NewOperations(ops ...Operation) Operations {
	return ops
}

func (p Operations) Len() int {
	return len(p)
}

// EqualTo reports whether two operation lists are element-wise equal.
//
// It takes a concrete Operations, not a values.Value: this type is not a Scheme
// datum and does not implement the Value interface. See the type's doc comment.
func (p Operations) EqualTo(v Operations) bool {
	if len(p) != len(v) {
		return false
	}
	for i := range p {
		if !p[i].EqualTo(v[i]) {
			return false
		}
	}
	return true
}

func (p Operations) Copy() Operations {
	q := NewOperations(p...)
	return q
}
