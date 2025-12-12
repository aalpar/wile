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
	_ Value = (*Atomic)(nil)

	// Atomic ID counter
	atomicIDCounter uint64
)

// Atomic provides atomic operations on a Value
// This uses atomic.Value from the standard library
type Atomic struct {
	id    uint64
	value atomic.Value
}

// NewAtomic creates a new Atomic with the given initial value
func NewAtomic(initial Value) *Atomic {
	id := atomic.AddUint64(&atomicIDCounter, 1)
	a := &Atomic{id: id}
	if initial != nil {
		a.value.Store(initial)
	}
	return a
}

// ID returns the Atomic's unique identifier
func (a *Atomic) ID() uint64 {
	return a.id
}

// Load atomically loads and returns the value
func (a *Atomic) Load() Value {
	v := a.value.Load()
	if v == nil {
		return nil
	}
	return v.(Value)
}

// Store atomically stores the value
func (a *Atomic) Store(v Value) {
	a.value.Store(v)
}

// Swap atomically stores new and returns the old value
func (a *Atomic) Swap(new Value) Value {
	old := a.value.Swap(new)
	if old == nil {
		return nil
	}
	return old.(Value)
}

// CompareAndSwap atomically compares and swaps if current equals old
// Returns true if the swap was performed
func (a *Atomic) CompareAndSwap(old, new Value) bool {
	return a.value.CompareAndSwap(old, new)
}

// Value interface implementation

func (a *Atomic) IsVoid() bool {
	return a == nil
}

func (a *Atomic) EqualTo(v Value) bool {
	other, ok := v.(*Atomic)
	if !ok {
		return false
	}
	return a == other
}

func (a *Atomic) SchemeString() string {
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
// This is more efficient than Atomic for integer operations
type AtomicInt64 struct {
	id    uint64
	value int64
}

var (
	_ Value = (*AtomicInt64)(nil)
)

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
func (a *AtomicInt64) Swap(new int64) int64 {
	return atomic.SwapInt64(&a.value, new)
}

// CompareAndSwap atomically compares and swaps
// Returns true if the swap was performed
func (a *AtomicInt64) CompareAndSwap(old, new int64) bool {
	return atomic.CompareAndSwapInt64(&a.value, old, new)
}

// Value interface implementation

func (a *AtomicInt64) IsVoid() bool {
	return a == nil
}

func (a *AtomicInt64) EqualTo(v Value) bool {
	other, ok := v.(*AtomicInt64)
	if !ok {
		return false
	}
	return a == other
}

func (a *AtomicInt64) SchemeString() string {
	if a == nil {
		return "#<atomic-int64:void>"
	}
	return fmt.Sprintf("#<atomic-int64 id=%d value=%d>", a.id, a.Load())
}
