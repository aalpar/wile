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

// operations_box.go carries the three operations of flat-closure boxing: a
// captured-and-assigned local slot holds a *values.Box, so that a closure which
// copies the slot's contents still shares the CELL.
//
// The three form one protocol and only make sense together. OpBoxSlot installs
// the cell at the binder, before any closure can observe the slot; OpUnbox
// follows every read of a boxed slot; OpStoreThroughBox replaces every write.
// A site that emits one without its partners produces a program that hands a
// #<box> to Scheme, or writes past the cell every sharer is reading.
//
// These are NOT the multiple-values Box/Unbox operations
// (OperationBoxValues / OperationUnboxValues) — those wrap a MultipleValues, and
// share nothing with this file but a verb.

import (
	"fmt"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"
)

// --- BoxSlot ---

// OperationBoxSlot replaces a local slot's contents with a fresh box holding
// them. Emitted once per boxed slot at the binder, after the slot's initial
// value is in place.
//
// One opcode rather than a load/wrap/store triple so the peephole cannot split
// the pair, and so re-entering the body — which OpSelfTailCall does, by jumping
// to pc=0 — allocates a FRESH cell per iteration rather than reusing the one
// the previous iteration's closures captured.
type OperationBoxSlot struct {
	OperationBase
	LocalIndex *environment.LocalIndex
}

// NewOperationBoxSlot creates a box-the-slot operation for li.
func NewOperationBoxSlot(li *environment.LocalIndex) *OperationBoxSlot {
	return &OperationBoxSlot{
		OperationBase: NewOperationBase("machine-operation-box-slot"),
		LocalIndex:    li,
	}
}

// SchemeString returns the Scheme representation of the operation.
func (p *OperationBoxSlot) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-box-slot %s>", p.LocalIndex)
}

// EqualTo compares the target slot.
func (p *OperationBoxSlot) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationBoxSlot)
	return fieldMethodMatches(p, v, ok,
		func(op *OperationBoxSlot) *environment.LocalIndex {
			return op.LocalIndex
		},
		func(a, b *environment.LocalIndex) bool {
			return a.EqualTo(b)
		})
}

// --- Unbox ---

// OperationUnbox replaces the value register's contents with the value inside
// the box it holds. Emitted after every load of a boxed slot.
//
// It operates on the value register rather than the eval stack, which is what
// keeps it out of the peephole's way: a boxed read compiles to
// LoadLocal · Unbox · Push, and the LoadLocal+Push fusion looks for those two
// ADJACENT, so it does not fire. A stack-top form would have had to be excluded
// from the call fusions by hand.
type OperationUnbox struct {
	OperationBase
}

// NewOperationUnbox creates an unbox-the-value-register operation.
func NewOperationUnbox() *OperationUnbox {
	return &OperationUnbox{
		OperationBase: NewOperationBase("machine-operation-unbox"),
	}
}

// SchemeString returns the Scheme representation of the operation.
func (*OperationUnbox) SchemeString() string {
	return "#<machine-operation-unbox>"
}

// EqualTo reports whether o is also an unbox operation. The operation carries no
// operand, so type identity is the whole comparison.
func (p *OperationUnbox) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationUnbox)
	return SameType(p, v, ok)
}

// --- StoreThroughBox ---

// OperationStoreThroughBox pops a value and writes it INTO the box a local slot
// holds, leaving the slot's box pointer alone. Emitted for every write to a
// boxed slot — a set!, an internal define's store, a let init's store.
//
// Writing the slot instead would install a new value where every sharer is
// still reading the old cell, which is exactly the aliasing the box exists to
// preserve.
type OperationStoreThroughBox struct {
	OperationBase
	LocalIndex *environment.LocalIndex
}

// NewOperationStoreThroughBox creates a store-through-the-box operation for li.
func NewOperationStoreThroughBox(li *environment.LocalIndex) *OperationStoreThroughBox {
	return &OperationStoreThroughBox{
		OperationBase: NewOperationBase("machine-operation-store-through-box"),
		LocalIndex:    li,
	}
}

// SchemeString returns the Scheme representation of the operation.
func (p *OperationStoreThroughBox) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-store-through-box %s>", p.LocalIndex)
}

// EqualTo compares the target slot.
func (p *OperationStoreThroughBox) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationStoreThroughBox)
	return fieldMethodMatches(p, v, ok,
		func(op *OperationStoreThroughBox) *environment.LocalIndex {
			return op.LocalIndex
		},
		func(a, b *environment.LocalIndex) bool {
			return a.EqualTo(b)
		})
}
