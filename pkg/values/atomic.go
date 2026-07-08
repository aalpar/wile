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

// AtomicBox provides atomic operations on a Value
// This uses atomic.Value from the standard library
type AtomicBox struct {
	id    uint64
	value atomic.Value
}

// NewAtomicBox creates a new AtomicBox with the given initial value
func NewAtomicBox(initial Value) *AtomicBox {
	id := atomicIDCounter.Add(1)
	a := &AtomicBox{id: id}
	if initial != nil {
		a.value.Store(initial)
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
	return v.(Value)
}

// Store atomically stores the value
func (p *AtomicBox) Store(v Value) {
	p.value.Store(v)
}

// Swap atomically stores new and returns the old value
func (p *AtomicBox) Swap(v Value) Value {
	old := p.value.Swap(v)
	if old == nil {
		return nil
	}
	return old.(Value)
}

// CompareAndSwap atomically compares and swaps if current equals old
// Returns true if the swap was performed
func (p *AtomicBox) CompareAndSwap(ol, nw Value) bool {
	return p.value.CompareAndSwap(ol, nw)
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
	v := p.Load()
	if v == nil {
		return fmt.Sprintf("#<atomic id=%d value=#<void>>", p.id)
	}
	return fmt.Sprintf("#<atomic id=%d value=%s>", p.id, v.SchemeString())
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
