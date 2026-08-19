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

// operations_free.go carries the read side of flat closures: indexing the
// executing closure's free vector.
//
// The three opcodes mirror the local family — load, fused push, fused call —
// but their operand is ONE dimension, a free-vector index, not a (slot, depth)
// pair. That is why they carry OperandRaw rather than OperandLocalIdx: the
// LocalIdx family is decomposed generically by instructionToOperation's default
// arm, and a free index run through DecodeLocalIndex would come back as a slot
// with a fabricated depth.
//
// OpPushFree and OpCallFree have no Operation type of their own: like every
// other fused/promoted opcode they are emitted by the peephole from an
// OperationLoadFree, and decompose back to one.

import (
	"fmt"

	"github.com/aalpar/wile/pkg/values"
)

// OperationLoadFree loads one entry of the executing closure's free vector into
// the value register.
//
// The entry holds the free variable's VALUE, copied when the closure was built
// — or, when the template's FreeBoxed marks this slot, the shared *values.Box
// that stands in for it. The emitter follows a boxed load with OpUnbox, exactly
// as it does for a boxed local.
type OperationLoadFree struct {
	OperationBase
	// Index is the free-vector slot, in the order compileClosureBody fixed.
	Index int
}

// NewOperationLoadFree creates a free-vector load for slot i.
func NewOperationLoadFree(i int) *OperationLoadFree {
	return &OperationLoadFree{
		OperationBase: NewOperationBase("machine-operation-load-free"),
		Index:         i,
	}
}

// SchemeString returns the Scheme representation of the operation.
func (p *OperationLoadFree) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-load-free %d>", p.Index)
}

// EqualTo compares the free-vector index.
func (p *OperationLoadFree) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationLoadFree)
	return FieldMatches(p, v, ok, func(op *OperationLoadFree) int {
		return op.Index
	})
}

// OperationStoreFree pops a value and writes it into the BOX held at one entry
// of the executing closure's free vector.
//
// It exists because a free variable that is written is captured-and-assigned,
// hence boxed, and the cell lives in the vector rather than in a frame slot —
// so neither OpStoreLocal nor OpStoreThroughBox can reach it. Writing the
// vector entry instead of the cell would leave every other holder, including
// the frame the variable is bound in, reading the old value.
type OperationStoreFree struct {
	OperationBase
	// Index is the free-vector slot whose box receives the value.
	Index int
}

// NewOperationStoreFree creates a write-through-the-free-vector's-box operation.
func NewOperationStoreFree(i int) *OperationStoreFree {
	return &OperationStoreFree{
		OperationBase: NewOperationBase("machine-operation-store-free"),
		Index:         i,
	}
}

// SchemeString returns the Scheme representation of the operation.
func (p *OperationStoreFree) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-store-free %d>", p.Index)
}

// EqualTo compares the free-vector index.
func (p *OperationStoreFree) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationStoreFree)
	return FieldMatches(p, v, ok, func(op *OperationStoreFree) int {
		return op.Index
	})
}
