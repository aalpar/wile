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

package values

import (
	"fmt"
	"sync/atomic"
)

var (
	_ Value = (*AtomicBox)(nil)

	// atomicIDCounter assigns each AtomicBox a unique ID.
	atomicIDCounter atomic.Uint64
)

// atomicCell is the single concrete type ever stored in an AtomicBox's
// atomic.Value.
//
// sync/atomic.Value panics ("inconsistently typed value") when a Store, Swap, or
// CompareAndSwap sees a concrete Go type different from the one already stored.
// Storing an *Integer and later a *String into one box is ordinary in a
// dynamically typed language, so that panic was reachable from plain Scheme and
// escaped the VM as an uncatchable runtime error.
//
// Wrapping in a cell makes the stored concrete type invariant. It preserves CAS
// semantics exactly, which atomic.Pointer[Value] would not: a cell compares with
// == on its single interface field, so CompareAndSwap still matches on the
// identity of the stored Value (an equal-but-distinct object does not match, as
// TestAtomicCompareAndSwap requires), whereas atomic.Pointer would compare the
// address of a heap cell that changes on every Store.
type atomicCell struct {
	v Value
}

// AtomicBox provides atomic operations on a Value.
type AtomicBox struct {
	id    uint64
	value atomic.Value
}

// NewAtomicBox creates a new AtomicBox with the given initial value
func NewAtomicBox(initial Value) *AtomicBox {
	id := atomicIDCounter.Add(1)
	a := &AtomicBox{id: id}
	if initial != nil {
		a.value.Store(atomicCell{v: initial})
	}
	return a
}

// ID returns the AtomicBox's unique identifier
func (p *AtomicBox) ID() uint64 {
	return p.id
}

// Load atomically loads and returns the value
func (p *AtomicBox) Load() Value {
	v := p.value.Load()
	if v == nil {
		return nil
	}
	return v.(atomicCell).v
}

// Store atomically stores the value
func (p *AtomicBox) Store(v Value) {
	p.value.Store(atomicCell{v: v})
}

// Swap atomically stores new and returns the old value
func (p *AtomicBox) Swap(v Value) Value {
	old := p.value.Swap(atomicCell{v: v})
	if old == nil {
		return nil
	}
	return old.(atomicCell).v
}

// CompareAndSwap atomically compares and swaps if current equals old.
// Returns true if the swap was performed.
//
// Comparison is by identity of the stored Value, not by Scheme equal?: an
// equal-but-distinct object does not match. See atomicCell.
func (p *AtomicBox) CompareAndSwap(ol, nw Value) bool {
	return p.value.CompareAndSwap(atomicCell{v: ol}, atomicCell{v: nw})
}

// buf interface implementation

// IsVoid returns true if the atomic is nil.
func (p *AtomicBox) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if the atomics are the same object.
func (p *AtomicBox) EqualTo(v Value) bool {
	other, ok := v.(*AtomicBox)
	if !ok {
		return false
	}
	return p == other
}

// SchemeString returns the Scheme representation of the atomic.
func (p *AtomicBox) SchemeString() string {
	if p == nil {
		return "#<atomic:void>"
	}
	return p.schemeStringWithVisited(NewMapSet[Value](0), 1)
}

// schemeStringWithVisited renders the atomic under the same path-scoped cycle
// detection Pair, Vector, Hashtable and Box use. atomic-store! can point an
// atomic at a structure that reaches the atomic again, so the naive rendering
// this replaced recursed until the host Go stack overflowed — an unrecoverable
// runtime.throw, not a catchable panic.
//
// depth is this atomic's nesting level (root = 1); its content sits one level
// deeper, where schemeStringChild enforces the host-safety bound.
func (p *AtomicBox) schemeStringWithVisited(visited MapSet[Value], depth int) string {
	seen := visited.Get(p)
	if seen {
		return "..."
	}
	visited.Set(p)
	defer func() {
		visited.Unset(p)
	}()
	v := p.Load()
	if v == nil {
		return fmt.Sprintf("#<atomic id=%d value=#<void>>", p.id)
	}
	return fmt.Sprintf("#<atomic id=%d value=%s>", p.id, schemeStringChild(v, visited, depth+1))
}

// AtomicInt64 provides atomic operations on int64 values
// This is more efficient than AtomicBox for integer operations
type AtomicInt64 struct {
	id    uint64
	value atomic.Int64
}

var _ Value = (*AtomicInt64)(nil)

// NewAtomicInt64 creates a new AtomicInt64 with the given initial value
func NewAtomicInt64(initial int64) *AtomicInt64 {
	id := atomicIDCounter.Add(1)
	q := &AtomicInt64{id: id}
	q.value.Store(initial)
	return q
}

// ID returns the AtomicInt64's unique identifier
func (p *AtomicInt64) ID() uint64 {
	return p.id
}

// Load atomically loads and returns the value
func (p *AtomicInt64) Load() int64 {
	return p.value.Load()
}

// Store atomically stores the value
func (p *AtomicInt64) Store(v int64) {
	p.value.Store(v)
}

// Add atomically adds delta and returns the new value
func (p *AtomicInt64) Add(delta int64) int64 {
	return p.value.Add(delta)
}

// Swap atomically stores new and returns the old value
func (p *AtomicInt64) Swap(nw int64) int64 {
	return p.value.Swap(nw)
}

// CompareAndSwap atomically compares and swaps
// Returns true if the swap was performed
func (p *AtomicInt64) CompareAndSwap(ol, nw int64) bool {
	return p.value.CompareAndSwap(ol, nw)
}

// buf interface implementation

// IsVoid returns true if the atomic int64 is nil.
func (p *AtomicInt64) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if the atomics are the same object.
func (p *AtomicInt64) EqualTo(v Value) bool {
	other, ok := v.(*AtomicInt64)
	if !ok {
		return false
	}
	return p == other
}

// SchemeString returns the Scheme representation of the atomic int64.
func (p *AtomicInt64) SchemeString() string {
	if p == nil {
		return "#<atomic-int64:void>"
	}
	return fmt.Sprintf("#<atomic-int64 id=%d value=%d>", p.id, p.Load())
}
