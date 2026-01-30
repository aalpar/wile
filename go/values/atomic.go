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

package values

import (
	"fmt"
	"sync/atomic"
)

var (
	_ Value = (*AtomicBox)(nil)

	// AtomicBox ID counter
	atomicIDCounter uint64
)

// AtomicBox provides atomic operations on a Value
// This uses atomic.Value from the standard library
type AtomicBox struct {
	id    uint64
	value atomic.Value
}

// NewAtomicBox creates a new AtomicBox with the given initial value
func NewAtomicBox(initial Value) *AtomicBox {
	id := atomic.AddUint64(&atomicIDCounter, 1)
	a := &AtomicBox{id: id}
	if initial != nil {
		a.value.Store(initial)
	}
	return a
}

// ID returns the AtomicBox's unique identifier
func (a *AtomicBox) ID() uint64 {
	return a.id
}

// Load atomically loads and returns the value
func (a *AtomicBox) Load() Value {
	v := a.value.Load()
	if v == nil {
		return nil
	}
	return v.(Value)
}

// Store atomically stores the value
func (a *AtomicBox) Store(v Value) {
	a.value.Store(v)
}

// Swap atomically stores new and returns the old value
func (a *AtomicBox) Swap(v Value) Value {
	old := a.value.Swap(v)
	if old == nil {
		return nil
	}
	return old.(Value)
}

// CompareAndSwap atomically compares and swaps if current equals old
// Returns true if the swap was performed
func (a *AtomicBox) CompareAndSwap(ol, nw Value) bool {
	return a.value.CompareAndSwap(ol, nw)
}

// buf interface implementation

// IsVoid returns true if the atomic is nil.
func (a *AtomicBox) IsVoid() bool {
	return a == nil
}

// EqualTo returns true if the atomics are the same object.
func (a *AtomicBox) EqualTo(v Value) bool {
	other, ok := v.(*AtomicBox)
	if !ok {
		return false
	}
	return a == other
}

// SchemeString returns the Scheme representation of the atomic.
func (a *AtomicBox) SchemeString() string {
	if a == nil {
		return "#<atomic:void>"
	}
	v := a.Load()
	if v == nil {
		return fmt.Sprintf("#<atomic id=%d value=#<void>>", a.id)
	}
	return fmt.Sprintf("#<atomic id=%d value=%s>", a.id, v.SchemeString())
}

// AtomicInt64 provides atomic operations on int64 values
// This is more efficient than AtomicBox for integer operations
type AtomicInt64 struct {
	id    uint64
	value int64
}

var _ Value = (*AtomicInt64)(nil)

// NewAtomicInt64 creates a new AtomicInt64 with the given initial value
func NewAtomicInt64(initial int64) *AtomicInt64 {
	id := atomic.AddUint64(&atomicIDCounter, 1)
	return &AtomicInt64{
		id:    id,
		value: initial,
	}
}

// ID returns the AtomicInt64's unique identifier
func (a *AtomicInt64) ID() uint64 {
	return a.id
}

// Load atomically loads and returns the value
func (a *AtomicInt64) Load() int64 {
	return atomic.LoadInt64(&a.value)
}

// Store atomically stores the value
func (a *AtomicInt64) Store(v int64) {
	atomic.StoreInt64(&a.value, v)
}

// Add atomically adds delta and returns the new value
func (a *AtomicInt64) Add(delta int64) int64 {
	return atomic.AddInt64(&a.value, delta)
}

// Swap atomically stores new and returns the old value
func (a *AtomicInt64) Swap(nw int64) int64 {
	return atomic.SwapInt64(&a.value, nw)
}

// CompareAndSwap atomically compares and swaps
// Returns true if the swap was performed
func (a *AtomicInt64) CompareAndSwap(ol, nw int64) bool {
	return atomic.CompareAndSwapInt64(&a.value, ol, nw)
}

// buf interface implementation

// IsVoid returns true if the atomic int64 is nil.
func (a *AtomicInt64) IsVoid() bool {
	return a == nil
}

// EqualTo returns true if the atomics are the same object.
func (a *AtomicInt64) EqualTo(v Value) bool {
	other, ok := v.(*AtomicInt64)
	if !ok {
		return false
	}
	return a == other
}

// SchemeString returns the Scheme representation of the atomic int64.
func (a *AtomicInt64) SchemeString() string {
	if a == nil {
		return "#<atomic-int64:void>"
	}
	return fmt.Sprintf("#<atomic-int64 id=%d value=%d>", a.id, a.Load())
}
