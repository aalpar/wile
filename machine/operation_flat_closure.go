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
	"fmt"

	"github.com/aalpar/wile/values"
)

// OperationLoadFreeVar loads a value from the closure's flat freeVars array.
type OperationLoadFreeVar struct {
	OperationBase
	Index int32
}

// NewOperationLoadFreeVar creates a new free variable load operation.
func NewOperationLoadFreeVar(index int32) *OperationLoadFreeVar {
	return &OperationLoadFreeVar{
		OperationBase: NewOperationBase("machine-operation-load-free-var"),
		Index:         index,
	}
}

// SchemeString returns the Scheme representation of the operation.
func (p *OperationLoadFreeVar) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-load-free-var %d>", p.Index)
}

// EqualTo returns true if both operations have the same index.
func (p *OperationLoadFreeVar) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationLoadFreeVar)
	return fieldMatches(p, v, ok, func(op *OperationLoadFreeVar) int32 {
		return op.Index
	})
}

// OperationBox wraps the value register in a *values.Box.
type OperationBox struct {
	OperationBase
}

// NewOperationBox creates a new box operation.
func NewOperationBox() *OperationBox {
	return &OperationBox{
		OperationBase: NewOperationBase("machine-operation-box"),
	}
}

// EqualTo returns true if both operations are OperationBox.
func (p *OperationBox) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationBox)
	return sameType(p, v, ok)
}

// OperationUnbox unwraps the value register from a *values.Box.
type OperationUnbox struct {
	OperationBase
}

// NewOperationUnbox creates a new unbox operation.
func NewOperationUnbox() *OperationUnbox {
	return &OperationUnbox{
		OperationBase: NewOperationBase("machine-operation-unbox"),
	}
}

// EqualTo returns true if both operations are OperationUnbox.
func (p *OperationUnbox) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationUnbox)
	return sameType(p, v, ok)
}

// OperationSetBox sets the boxed value: value_reg.(*Box).Value = evals.Pop().
type OperationSetBox struct {
	OperationBase
}

// NewOperationSetBox creates a new set-box operation.
func NewOperationSetBox() *OperationSetBox {
	return &OperationSetBox{
		OperationBase: NewOperationBase("machine-operation-set-box"),
	}
}

// EqualTo returns true if both operations are OperationSetBox.
func (p *OperationSetBox) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationSetBox)
	return sameType(p, v, ok)
}

// OperationMakeFlatClosure creates a flat closure with a captured free-var array.
type OperationMakeFlatClosure struct {
	OperationBase
}

// NewOperationMakeFlatClosure creates a new make-flat-closure operation.
func NewOperationMakeFlatClosure() *OperationMakeFlatClosure {
	return &OperationMakeFlatClosure{
		OperationBase: NewOperationBase("machine-operation-make-flat-closure"),
	}
}

// EqualTo returns true if both operations are OperationMakeFlatClosure.
func (p *OperationMakeFlatClosure) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationMakeFlatClosure)
	return sameType(p, v, ok)
}
